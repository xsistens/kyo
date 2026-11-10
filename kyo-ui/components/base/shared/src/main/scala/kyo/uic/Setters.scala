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
      *   - An action field (`Maybe[UI.Action]`) takes the action by name (`onClick(action: => UI.Action)`), so it is built
      *     when its event comes, each time, and not where the setter is called.
      *   - Any other field takes its own type.
      *
      * A function handed to an event or a callback, and an action, runs under [[kyo.UI.attempt]], so its failure goes to the
      * `onFailure` rules where it ran and the component's own work after it still happens.
      */
    transparent inline def apply[P, Self]: Setters[P, Self] = ${ applyImpl[P, Self] }

    // How a setter writes its field.
    final private[kyo] val KindReplace = 0
    final private[kyo] val KindBind    = 1
    final private[kyo] val KindHear    = 2
    final private[kyo] val KindCall1   = 3
    final private[kyo] val KindCall2   = 4
    final private[kyo] val KindAction  = 5

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
            case KindAction =>
                // A by-name argument reaches the setter as the thunk the compiler made of it.
                val thunk = value.asInstanceOf[() => UI.Action]
                Present(UI.attempt(thunk()))
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

        // What a member of the field's `UI.Ownable` takes, read from its declaration and never built: `Fields` then names `UI.Prop`
        // and `UI.Action` as the source does, where a type built with `appliedTo` or a quote is printed expanded (`Signal[A] | A`,
        // `Any < (Async & Abort[Any])`) in the IDE and in a compile error.
        def taken(owner: TypeRepr, method: String): TypeRepr =
            owner.memberType(ownable.methodMember(method).head) match
                case MethodType(_, List(p), _) => p
                case other                     => report.errorAndAbort(s"UI.Ownable.$method takes one value, not ${other.show}")

        def eventOf(f: Symbol): String =
            f.getAnnotation(named) match
                case Some(Apply(_, List(Literal(StringConstant(n))))) => n
                case Some(other) => report.errorAndAbort(s"@eventName on ${f.name} takes the event's name as a literal", other.pos)
                case None        => s"on${f.name.capitalize}Change"

        // A callback field's function as the field declares it, and how many values it takes, for `Maybe[A => UI.Action]` and
        // `Maybe[(A, B) => UI.Action]`.
        def callback(t: TypeRepr): Maybe[(Int, TypeRepr)] =
            t.dealias match
                case AppliedType(m, List(fn)) if m.typeSymbol == maybe =>
                    fn.dealias match
                        case AppliedType(_, args)
                            if fn.isFunctionType && (args.sizeIs == 2 || args.sizeIs == 3) && args.last =:= action =>
                            Present((args.size - 1, fn))
                        case _ => Absent
                case _ => Absent

        // An action field, `Maybe[UI.Action]`, and what its setter takes: `=> UI.Action` as `UI.attempt` declares it, read rather than
        // quoted for the reason `taken` gives (a quoted `=> UI.Action` prints as the computation type it stands for).
        def isAction(t: TypeRepr): Boolean =
            t.dealias match
                case AppliedType(m, List(a)) => m.typeSymbol == maybe && a =:= action
                case _                       => false
        val byNameAction =
            val ui = TypeRepr.of[UI.type]
            ui.memberType(ui.typeSymbol.methodMember("attempt").head) match
                case MethodType(_, List(p), _) => p
                case other                     => report.errorAndAbort(s"UI.attempt takes one action by name, not ${other.show}")
        end byNameAction

        val made = all.zipWithIndex.filterNot((f, _) => f.hasAnnotation(marked)).flatMap { (f, i) =>
            val t = TypeRepr.of[P].memberType(props.fieldMember(f.name))
            t.baseType(ownable) match
                case owner @ AppliedType(_, List(_)) =>
                    List(One(f.name, i, KindBind, taken(owner, "bind")), One(eventOf(f), i, KindHear, taken(owner, "withHearer")))
                case _ =>
                    if f.hasAnnotation(named) then report.errorAndAbort(s"@eventName on ${f.name}, which is no UI.Ownable field")
                    callback(t) match
                        case Present((arity, fn)) => List(One(f.name, i, if arity == 1 then KindCall1 else KindCall2, fn))
                        case _ if isAction(t)     => List(One(f.name, i, KindAction, byNameAction))
                        case _                    => List(One(f.name, i, KindReplace, t))
                    end match
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
