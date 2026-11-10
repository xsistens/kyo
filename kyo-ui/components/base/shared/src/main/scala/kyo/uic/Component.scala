package kyo.uic

import kyo.*
import scala.language.implicitConversions

/** An optional prop: unset, or set to a value. It is a `Maybe` to whoever reads it (`p.icon.map(...)`, `isDefined`), and its setter
  * takes the value itself: the conversion in this companion makes `Present` of it, so a caller never writes the wrapper.
  */
opaque type Opt[+A] <: Maybe[A] = Maybe[A]

object Opt:
    /** No value set, the default of an optional prop. */
    val unset: Opt[Nothing] = Absent

    /** A value handed to an optional prop's setter is that prop set. */
    implicit def set[A](a: A): Opt[A] = Present(a)
end Opt

/** A component over its props `P`: `final class Button(val p: Props) extends Component(Setters[Props, Button])`.
  *
  * Every field of `P` is a setter of its name, `uic.Button("Save").size(Size.Small)`, typed against the field and completed by the
  * IDE as a selectable field. A setter the component writes itself takes its field's place, so an author who wants the setter's
  * scaladoc in the IDE, or a setter that does more than store its value, writes it by hand. A `UI.Ownable` field is a binding setter
  * and its event (`on<Field>Change`, or `@eventName`), a callback field takes the function itself, and an action field its action
  * by name (see [[kyo.uic.Setters.apply]]).
  * Two components of one class are equal when their props are.
  */
abstract class Component[P, Self, S <: Setters[P, Self]](private[uic] val setters: S) extends Selectable:
    private[uic] def p: P

    type Fields = setters.Fields

    def selectDynamic(name: String)(using Frame): Any = (value: Any) => setters.make(setters.set(p, name, value))

    override def equals(other: Any): Boolean = other match
        case that: Component[?, ?, ?] => getClass.equals(that.getClass) && p.equals(that.p)
        case _                        => false

    override def hashCode: Int = p.hashCode

    override def toString: String = s"${getClass.getSimpleName}($p)"
end Component

object Component:
    /** The chain kyo-ui's `Element.when` gives an element, for a host that builds a component in one chain: a step taken only
      * where `cond` holds. An extension found through this companion, so it adds no member to any component and never shadows
      * kyo-ui's `UI.when` in a component's body.
      */
    extension [C <: Component[?, C, ?]](c: C)
        private[uic] def when(cond: Boolean)(f: C => C): C = if cond then f(c) else c

        /** A step taken with the value of `v` where it is present. */
        private[uic] def when[A](v: Maybe[A])(f: (C, A) => C): C = v.fold(c)(f(c, _))
    end extension
end Component

/** Evidence that a component `C` has a setter named `N` that takes an `A`, and the way to call it, so generic code (a form's `bind`)
  * can write through a setter without knowing whether `Setters` generated it or the class wrote it by hand.
  */
@scala.annotation.implicitNotFound("${C} has no setter ${N} that takes ${A}")
abstract class HasSetter[C, N <: String, A]:
    def apply(c: C, a: A)(using Frame): C

object HasSetter:
    /** A setter `Setters` generated: found in the component's `Fields` by its name, and checked against what it takes. A name
      * `Fields` does not hold leaves `Of` unreduced, so there is no instance.
      */
    given fromFields[F <: NamedTuple.AnyNamedTuple, C <: Component[?, ?, ?] { type Fields = F }, N <: String, A](using
        name: ValueOf[N],
        fits: Of[NamedTuple.Names[F], NamedTuple.DropNames[F], N] <:< (A => C)
    ): HasSetter[C, N, A] with
        def apply(c: C, a: A)(using Frame): C =
            fits(c.selectDynamic(name.value).asInstanceOf[Of[NamedTuple.Names[F], NamedTuple.DropNames[F], N]])(a)
    end fromFields

    /** What the setter named `N` takes and gives, looked up in the names `Ns` and setter types `Vs` of a `Fields`. */
    type Of[Ns <: Tuple, Vs <: Tuple, N] = (Ns, Vs) match
        case (N *: ?, v *: ?)   => v
        case (? *: ns, ? *: vs) => Of[ns, vs, N]
end HasSetter
