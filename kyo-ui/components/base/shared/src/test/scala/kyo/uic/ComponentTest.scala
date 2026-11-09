package kyo.uic

import kyo.*

/** A `Component` over its props: its setters come from `Setters`, an `Opt` field takes the value itself, two components of one class
  * compare by their props, and `HasSetter` finds a generated setter by its name and what it takes.
  */
class ComponentTest extends kyo.test.Test[Any]:

    "a field is a setter, and an Opt field's setter takes the value itself" in {
        val t = ComponentTest.Thing().size(2).label("x")
        assert(t.p == ComponentTest.Props(size = 2, label = Opt.set("x")), s"${t.p}")
    }

    "two components of one class are equal when their props are" in {
        val a = ComponentTest.Thing().size(2)
        val b = ComponentTest.Thing().size(2)
        val c = ComponentTest.Thing().size(3)
        assert(a.equals(b) && a.hashCode == b.hashCode && !a.equals(c), s"$a $b $c")
    }

    "HasSetter finds a generated setter by its name and type and writes through it" in {
        val size = summon[HasSetter[ComponentTest.Thing, "size", Int]]
        val pick = summon[HasSetter[ComponentTest.Thing, "onPick", Int => UI.Action]]
        val t    = pick(size(ComponentTest.Thing(), 4), _ => Kyo.unit)
        assert(t.p.size == 4 && t.p.onPick.isDefined, s"${t.p}")
    }

    "no setter of that name, or of another type, is no HasSetter" in {
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ComponentTest.Thing, "nope", Int]]""")
        typeCheckFailure("""summon[kyo.uic.HasSetter[kyo.uic.ComponentTest.Thing, "size", String]]""")
    }

end ComponentTest

object ComponentTest:
    final case class Props(size: Int = 1, label: Opt[String] = Opt.unset, onPick: Maybe[Int => UI.Action] = Absent)
        derives CanEqual

    final class Thing(private[uic] val p: Props) extends Component(Setters[Props, Thing])

    object Thing:
        def apply(): Thing = new Thing(Props())
end ComponentTest
