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

/** A component over its props `P`: `final class Button(val p: Props) extends Component(Setters.derive[Props, Button])`.
  *
  * Every field of `P` is a setter of its name, `uic.Button("Save").size(Size.Small)`, typed against the field and completed by the
  * IDE as a selectable field. A setter the component writes itself takes its field's place, so an author who wants the setter's
  * scaladoc in the IDE, or a setter that does more than store its value, writes it by hand. Two components of one class are equal
  * when their props are.
  */
abstract class Component[P, Self, S <: Setters[P, Self]](private[uic] val setters: S) extends Selectable:
    private[uic] def p: P

    type Fields = setters.Fields

    def selectDynamic(name: String): Any = (value: Any) => setters.make(setters.set(p, name, value))

    override def equals(other: Any): Boolean = other match
        case that: Component[?, ?, ?] => getClass.equals(that.getClass) && p.equals(that.p)
        case _                        => false

    override def hashCode: Int = p.hashCode

    override def toString: String = s"${getClass.getSimpleName}($p)"
end Component
