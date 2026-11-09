package kyo

/** The setters `Setters.derive` works out for a class over its props: every field but those the class writes itself and those
  * marked `@noSetter`.
  */
class SettersTest extends kyo.test.Test[Any]:

    "a field is a setter that copies it, and the class is built from the new props" in {
        val b = SettersTest.Thing().size(2).name("x")
        assert(b.p == SettersTest.Props(size = 2, name = "x"))
    }

    "a field the class declares a member for keeps that member" in {
        assert(SettersTest.Thing().tag("t").p.tag == "<t>")
    }

    "a field marked noSetter is no setter" in {
        typeCheckFailure("""kyo.SettersTest.Thing().hidden(1)""")
    }

    "a setter takes only its field's type" in {
        typeCheckFailure("""kyo.SettersTest.Thing().size("two")""")
    }

end SettersTest

object SettersTest:
    final case class Props(size: Int = 1, name: String = "", tag: String = "", @noSetter hidden: Int = 0) derives CanEqual

    final class Thing(val p: Props) extends Selectable:
        type Fields = Thing.setters.Fields
        def selectDynamic(name: String): Any = (v: Any) => Thing.setters.make(Thing.setters.set(p, name, v))
        def tag(v: String): Thing            = new Thing(p.copy(tag = s"<$v>"))
    end Thing

    object Thing:
        val setters        = Setters.derive[Props, Thing]
        def apply(): Thing = new Thing(Props())
end SettersTest
