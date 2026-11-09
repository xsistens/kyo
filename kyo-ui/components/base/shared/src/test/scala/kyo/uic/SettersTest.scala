package kyo.uic

import kyo.*

/** The setters `Setters[P, Self]` works out for a class over its props: every field but those the class writes itself and those
  * marked `@noSetter`, an ownable field as a binding setter and its event, and a callback field taking the function itself.
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
        typeCheckFailure("""kyo.uic.SettersTest.Thing().hidden(1)""")
    }

    "a setter takes only its field's type" in {
        typeCheckFailure("""kyo.uic.SettersTest.Thing().size("two")""")
    }

    "an ownable field is a setter that binds it, a constant or a signal" in {
        for
            sig <- Signal.initRef(7)
            a = SettersTest.Thing().count(3)
            b = SettersTest.Thing().count(sig)
        yield assert(a.p.count.bound.exists(_.equals(3)) && b.p.count.bound.exists(_.equals(sig)) && a.p.count.start == Present(0))
    }

    "an ownable field's event is on<Field>Change, and it hears the field" in {
        for
            heard <- AtomicRef.init(Chunk.empty[Int])
            t = SettersTest.Thing().onCountChange(v => heard.getAndUpdate(_.append(v)).unit)
            _  <- Abort.run[Any](t.p.count.hearer.get(4))
            hs <- heard.get
        yield assert(hs == Chunk(4) && t.p.count.bound.isEmpty, s"heard $hs, bound ${t.p.count.bound}")
    }

    "eventName names an ownable field's event in place of on<Field>Change" in {
        val t = SettersTest.Thing().onFlip(_ => Kyo.unit)
        typeCheckFailure("""kyo.uic.SettersTest.Thing().onFlippedChange(_ => kyo.Kyo.unit)""")
        assert(t.p.flipped.hearer.isDefined)
    }

    "an event the class declares a member for keeps that member" in {
        assert(SettersTest.Thing().onOtherChange(_ => Kyo.unit).p.other.hearer.isEmpty)
    }

    "a callback field takes the function itself, and its failure does not reach the caller" in {
        for
            heard <- AtomicRef.init(Chunk.empty[String])
            t = SettersTest.Thing()
                .onPick(v => heard.getAndUpdate(_.append(s"pick $v")).unit)
                .onMove((x, y) => heard.getAndUpdate(_.append(s"move $x $y")).andThen(Abort.fail("boom")))
            picked <- Abort.run[Any](t.p.onPick.get(1))
            moved  <- Abort.run[Any](t.p.onMove.get(2, 3))
            hs     <- heard.get
        yield assert(
            hs == Chunk("pick 1", "move 2 3") && picked.isSuccess && moved.isSuccess,
            s"heard $hs, pick $picked, move $moved"
        )
    }

    "a generated setter's type keeps the names Prop and UI.Action, as the IDE and a compile error show it" in {
        typeCheckFailure("""val s: Int = kyo.uic.SettersTest.Thing().count""")("Prop[Int] =>")
        typeCheckFailure("""val s: Int = kyo.uic.SettersTest.Thing().onCountChange""")("Int => kyo.UI.Action")
        typeCheckFailure("""val s: Int = kyo.uic.SettersTest.Thing().onPick""")("Int => kyo.UI.Action")
    }

    "an event setter takes only a function of its field's value" in {
        typeCheckFailure("""kyo.uic.SettersTest.Thing().onCountChange((s: String) => kyo.Kyo.unit)""")
    }

end SettersTest

object SettersTest:
    /** A minimal ownable prop: what the caller bound, where it starts, and who hears it. */
    final case class Held[A](bound: Maybe[UI.Prop[A]] = Absent, start: Maybe[A] = Absent, hearer: Maybe[A => UI.Action] = Absent)
        extends UI.Ownable[A]:
        def bind(v: UI.Prop[A]): Held[A]           = copy(bound = Present(v))
        def withHearer(f: A => UI.Action): Held[A] = copy(hearer = Present(f))
    end Held

    final case class Props(
        size: Int = 1,
        name: String = "",
        tag: String = "",
        @noSetter hidden: Int = 0,
        count: Held[Int] = Held(start = Present(0)),
        @eventName("onFlip") flipped: Held[Boolean] = Held(start = Present(false)),
        other: Held[Int] = Held(),
        onPick: Maybe[Int => UI.Action] = Absent,
        onMove: Maybe[(Int, Int) => UI.Action] = Absent
    ) derives CanEqual

    final class Thing(val p: Props) extends Selectable:
        type Fields = Thing.setters.Fields
        def selectDynamic(name: String)(using Frame): Any = (v: Any) => Thing.setters.make(Thing.setters.set(p, name, v))
        def tag(v: String): Thing                         = new Thing(p.copy(tag = s"<$v>"))
        def onOtherChange(f: Int => UI.Action): Thing     = this
    end Thing

    object Thing:
        val setters        = Setters[Props, Thing]
        def apply(): Thing = new Thing(Props())
end SettersTest
