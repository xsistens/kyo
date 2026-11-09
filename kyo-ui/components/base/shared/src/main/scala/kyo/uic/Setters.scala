package kyo.uic

import kyo.*
import scala.NamedTuple
import scala.deriving.Mirror
import scala.quoted.*

/** A props field no setter is made for: a host, a shared trait or a hand-written setter of another name writes it. */
final class noSetter extends scala.annotation.StaticAnnotation

/** The name of an ownable field's event setter, where it is not `on<Field>Change` (`@eventName("onChange") checked`). */
final class eventName(name: String) extends scala.annotation.StaticAnnotation

/** The setters of a component `Self` over its props `P`, worked out at compile time by [[Setters.apply]]: `Fields` names each one
  * with what it takes, `set` writes one by its name, `make` builds the component from new props.
  */
abstract class Setters[P, Self]:
    type Fields <: NamedTuple.AnyNamedTuple
    def set(p: P, name: String, value: Any)(using Frame): P
    def make(p: P): Self
end Setters

object Setters:
    /** Every field of `P` is a setter of its name, but a field marked `@noSetter`; a setter whose name `Self` declares a member for
      * is that member instead (written by hand, with its scaladoc).
      *
      *   - A field of a [[kyo.UI.Ownable]] type binds what the setter is handed (`checked(v: Prop[A])`), and has a second setter,
      *     its event, which hears every change: `on<Field>Change`, or the name `@eventName` gives.
      *   - A callback field (`Maybe[A => UI.Action]`, `Maybe[(A, B) => UI.Action]`) takes the function itself.
      *   - Any other field takes its own type.
      *
      * A function handed to an event or a callback runs under [[kyo.UI.attempt]], so its failure goes to the `onFailure` rules
      * where it ran and the component's own work after it still happens.
      */
    transparent inline def apply[P, Self]: Setters[P, Self] = ${ applyImpl[P, Self] }

    // How a setter writes its field.
    final private[kyo] val KindReplace = 0
    final private[kyo] val KindBind    = 1
    final private[kyo] val KindHear    = 2
    final private[kyo] val KindCall1   = 3
    final private[kyo] val KindCall2   = 4

    /** The step every generated setter runs: the field's new value, `value` as a setter of kind `kind` writes it over `old`. Public,
      * since the setters are generated where the component is.
      */
    def write(kind: Int, old: Any, value: Any)(using Frame): Any =
        kind match
            case KindBind => old.asInstanceOf[UI.Ownable[Any]].bind(value.asInstanceOf[UI.Prop[Any]])
            case KindHear =>
                val f = value.asInstanceOf[Any => UI.Action]
                old.asInstanceOf[UI.Ownable[Any]].withHearer(a => UI.attempt(f(a)))
            case KindCall1 =>
                val f = value.asInstanceOf[Any => UI.Action]
                Present((a: Any) => UI.attempt(f(a)))
            case KindCall2 =>
                val f = value.asInstanceOf[(Any, Any) => UI.Action]
                Present((a: Any, b: Any) => UI.attempt(f(a, b)))
            case _ => value
    end write

    private[kyo] def applyImpl[P: Type, Self: Type](using Quotes): Expr[Setters[P, Self]] =
        import quotes.reflect.*
        given CanEqual[Symbol, Symbol] = CanEqual.derived
        val props                      = TypeRepr.of[P].typeSymbol
        val declared                   = TypeRepr.of[Self].typeSymbol.declarations.map(_.name).toSet
        val marked                     = TypeRepr.of[noSetter].typeSymbol
        val named                      = TypeRepr.of[eventName].typeSymbol
        val ownable                    = TypeRepr.of[UI.Ownable[Any]].typeSymbol
        val maybe                      = TypeRepr.of[Maybe[Any]].typeSymbol
        val action                     = TypeRepr.of[UI.Action]
        val all                        = props.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)

        // One setter: its name, the field it writes, how, and what it takes.
        final case class One(name: String, at: Int, kind: Int, takes: TypeRepr)

        def function(args: List[TypeRepr]): TypeRepr = defn.FunctionClass(args.size).typeRef.appliedTo(args :+ action)

        def eventOf(f: Symbol): String =
            f.getAnnotation(named) match
                case Some(Apply(_, List(Literal(StringConstant(n))))) => n
                case Some(other) => report.errorAndAbort(s"@eventName on ${f.name} takes the event's name as a literal", other.pos)
                case None        => s"on${f.name.capitalize}Change"

        // The parameters of a callback field's function, for `Maybe[A => UI.Action]` and `Maybe[(A, B) => UI.Action]`.
        def callback(t: TypeRepr): Maybe[List[TypeRepr]] =
            t.dealias match
                case AppliedType(m, List(fn)) if m.typeSymbol == maybe =>
                    fn.dealias match
                        case AppliedType(_, args)
                            if fn.isFunctionType && (args.sizeIs == 2 || args.sizeIs == 3) && args.last =:= action =>
                            Present(args.init)
                        case _ => Absent
                case _ => Absent

        val made = all.zipWithIndex.filterNot((f, _) => f.hasAnnotation(marked)).flatMap { (f, i) =>
            val t = TypeRepr.of[P].memberType(props.fieldMember(f.name))
            t.baseType(ownable) match
                case AppliedType(_, List(a)) =>
                    List(One(f.name, i, KindBind, TypeRepr.of[UI.Prop].appliedTo(a)), One(eventOf(f), i, KindHear, function(List(a))))
                case _ =>
                    if f.hasAnnotation(named) then report.errorAndAbort(s"@eventName on ${f.name}, which is no UI.Ownable field")
                    callback(t) match
                        case Present(args) => List(One(f.name, i, if args.sizeIs == 1 then KindCall1 else KindCall2, function(args)))
                        case _             => List(One(f.name, i, KindReplace, t))
            end match
        }
        made.groupBy(_.name).collectFirst { case (n, ss) if ss.sizeIs > 1 => n }.foreach(n =>
            report.errorAndAbort(s"two setters of ${Type.show[P]} are named $n")
        )
        val kept = made.filterNot(s => declared.contains(s.name))

        def tuple(ts: List[TypeRepr]): TypeRepr =
            ts.foldRight(TypeRepr.of[EmptyTuple])((t, rest) => TypeRepr.of[*:].appliedTo(List(t, rest)))
        val names    = tuple(kept.map(s => ConstantType(StringConstant(s.name))))
        val values   = tuple(kept.map(s => TypeRepr.of[Function1].appliedTo(List(s.takes, TypeRepr.of[Self]))))
        val labels   = Expr(kept.map(_.name).toArray)
        val atList   = Expr(kept.map(_.at).toArray)
        val kindList = Expr(kept.map(_.kind).toArray)
        val mirror   = Expr.summon[Mirror.ProductOf[P]].getOrElse(report.errorAndAbort(s"${Type.show[P]} is no case class"))
        def build(p: Expr[P]): Expr[Self] =
            New(Inferred(TypeRepr.of[Self])).select(TypeRepr.of[Self].typeSymbol.primaryConstructor).appliedTo(p.asTerm).asExprOf[Self]
        TypeRepr.of[NamedTuple.NamedTuple].appliedTo(List(names, values)).asType match
            case '[f] =>
                '{
                    new Setters[P, Self]:
                        type Fields = f & NamedTuple.AnyNamedTuple
                        private val names                                       = $labels
                        private val ats                                         = $atList
                        private val kinds                                       = $kindList
                        def set(p: P, name: String, value: Any)(using Frame): P =
                            val k      = names.indexOf(name)
                            val fields = p.asInstanceOf[Product].productIterator.toArray
                            fields(ats(k)) = Setters.write(kinds(k), fields(ats(k)), value)
                            $mirror.fromProduct(Tuple.fromArray(fields))
                        end set
                        def make(p: P): Self = ${ build('p) }
                }
        end match
    end applyImpl
end Setters
