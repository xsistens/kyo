package kyo

import scala.NamedTuple
import scala.deriving.Mirror
import scala.quoted.*

/** A props field no setter is made for: a host, a shared trait or a hand-written setter of another name writes it. */
final class noSetter extends scala.annotation.StaticAnnotation

/** The setters of a component `Self` over its props `P`, worked out at compile time by [[Setters.derive]]: `Fields` names each one
  * with its type, `set` copies one field by its name, `make` builds the component from new props.
  */
abstract class Setters[P, Self]:
    type Fields <: NamedTuple.AnyNamedTuple
    def set(p: P, name: String, value: Any): P
    def make(p: P): Self
end Setters

object Setters:
    /** Every field of `P` is a setter of its name, but a field `Self` declares a member of that name for (its own setter, written by
      * hand with its scaladoc) and a field marked `@noSetter`.
      */
    transparent inline def derive[P, Self]: Setters[P, Self] = ${ deriveImpl[P, Self] }

    private[kyo] def deriveImpl[P: Type, Self: Type](using Quotes): Expr[Setters[P, Self]] =
        import quotes.reflect.*
        val props                               = TypeRepr.of[P].typeSymbol
        val declared                            = TypeRepr.of[Self].typeSymbol.declarations.map(_.name).toSet
        val marked                              = TypeRepr.of[noSetter].typeSymbol
        val all                                 = props.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        val kept                                = all.filterNot(f => f.hasAnnotation(marked) || declared.contains(f.name))
        def tuple(ts: List[TypeRepr]): TypeRepr =
            ts.foldRight(TypeRepr.of[EmptyTuple])((t, rest) => TypeRepr.of[*:].appliedTo(List(t, rest)))
        val names  = tuple(kept.map(f => ConstantType(StringConstant(f.name))))
        val values = tuple(kept.map(f =>
            TypeRepr.of[Function1].appliedTo(List(TypeRepr.of[P].memberType(props.fieldMember(f.name)), TypeRepr.of[Self]))
        ))
        val labels = Expr(all.map(_.name).toArray)
        val mirror = Expr.summon[Mirror.ProductOf[P]].getOrElse(report.errorAndAbort(s"${Type.show[P]} is no case class"))
        def build(p: Expr[P]): Expr[Self] =
            New(Inferred(TypeRepr.of[Self])).select(TypeRepr.of[Self].typeSymbol.primaryConstructor).appliedTo(p.asTerm).asExprOf[Self]
        TypeRepr.of[NamedTuple.NamedTuple].appliedTo(List(names, values)).asType match
            case '[f] =>
                '{
                    new Setters[P, Self]:
                        type Fields = f & NamedTuple.AnyNamedTuple
                        private val names                          = $labels
                        def set(p: P, name: String, value: Any): P =
                            val fields = p.asInstanceOf[Product].productIterator.toArray
                            fields(names.indexOf(name)) = value
                            $mirror.fromProduct(Tuple.fromArray(fields))
                        end set
                        def make(p: P): Self = ${ build('p) }
                }
        end match
    end deriveImpl
end Setters
