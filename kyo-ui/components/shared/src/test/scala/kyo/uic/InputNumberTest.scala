package kyo.uic

import kyo.*

/** The `value` slot bound one way: the field shows what its owner holds, and a number its owner does not take is painted
  * away again, since the native number field already shows what the reader typed.
  */
class InputNumberTest extends UicTest:

    private def number(ui: UI)(using Frame): UI.Ast.NumberInput < Sync =
        elements(ui).map(_.collectFirst { case n: UI.Ast.NumberInput => n }.getOrElse(throw new AssertionError("no number field")))

    "a number its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(3.0)
            again <- repaintsAfter(uic.InputNumber().value(held.readOnly).onChange(_ => ()).render, held)(t =>
                number(t).map(n => handled(n.onChangeNumeric.getOrElse(throw new AssertionError("no change handler"))(7.0)))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a value bound one way that nothing hears says so; onChange, a ref or disabled keep it quiet" in {
        def errors(c: uic.InputNumber): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(3.0)
            deaf   <- errors(uic.InputNumber().value(held.readOnly))
            change <- errors(uic.InputNumber().value(held.readOnly).onChange(_ => ()))
            twoWay <- errors(uic.InputNumber().value(held))
            off    <- errors(uic.InputNumber().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(change.isEmpty && twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can change it")
        end for
    }

    "a constant nobody hears is fixed: read-only, and no card" in {
        for
            n    <- number(uic.InputNumber().value(3.0).render)
            card <- elementsWithClass(uic.InputNumber().value(3.0).render, "p-uic-key-error")
        yield
            assert(n.readOnly.contains(true) && n.onChangeNumeric.isEmpty, "a fixed field takes no number")
            assert(card.isEmpty, s"a fixed constant: ${card.map(textOf)}")
    }

    "an unbound field keeps what the spin buttons make of it" in {
        def holds(text: String)(t: UI): Boolean < Sync =
            elements(t).map(_.collectFirst { case n: UI.Ast.NumberInput => n.value }).map {
                case Some(Present(UI.Bound.Ref(r)))   => r.get.map(_ == text)
                case Some(Present(UI.Bound.Const(v))) => Kyo.lift(v == text)
                case _                                => Kyo.lift(false)
            }
        for
            after <- publishedAfterReady(
                uic.InputNumber().showButtons(true).render,
                t => elementsWithClass(t, "p-inputnumber-increment-button").map(_.exists(declaresClick)),
                until = holds("1")
            )(t => elementWithClass(t, "p-inputnumber-increment-button").map(click))
        yield assert(after.nonEmpty, "the field never showed what the spin button made of it")
    }

    private def textOf(n: UI.Ast.NumberInput)(using Frame): String < Sync =
        n.value match
            case Present(UI.Bound.Ref(r))   => r.get
            case Present(UI.Bound.Const(v)) => Kyo.lift(v)
            case _                          => Kyo.lift("")

    // The way the runtime dispatches an input event: the bound ref is written first, then onInput is told.
    private def typeText(n: UI.Ast.NumberInput, text: String)(using Frame): Any < Async =
        val write: UI.Action = n.value match
            case Present(UI.Bound.Ref(ref)) => ref.set(text)
            case _                          => ()
        n.onInput match
            case Present(f) => handled(write.andThen(f(text)))
            case Absent     => throw new AssertionError("the field hears no typing")
    end typeText

    "a constant text is the starting text, kept by the field" in {
        for
            t    <- mountedWhen(uic.InputNumber().text("4.").render)(t => number(t).map(_.value.isDefined))
            n    <- number(t)
            text <- textOf(n)
            card <- elementsWithClass(uic.InputNumber().text("4.").render, "p-uic-key-error")
        yield
            assert(text == "4.", s"the field starts from $text")
            assert(n.readOnly.isEmpty, "and the reader goes on typing from it")
            assert(card.isEmpty, s"a starting text is not a mistake: ${card.map(textOf)}")
    }

    "a text bound one way is shown, and typing is reported without being written" in {
        for
            held <- Signal.initRef("12")
            seen <- Signal.initRef(List.empty[String])
            t    <- mountedWhen(uic.InputNumber().text(held.readOnly).onTextChange(v => seen.updateAndGet(_ :+ v).unit).render)(t =>
                number(t).map(_.onInput.isDefined)
            )
            n     <- number(t)
            shown <- textOf(n)
            _     <- typeText(n, "123")
            h     <- held.get
            s     <- seen.get
            deaf  <- elementsWithClass(uic.InputNumber().text(held.readOnly).render, "p-uic-key-error")
        yield
            assert(shown == "12", s"the field shows the owner's text: $shown")
            assert(h == "12" && s == List("123"), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onTextChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a text ref is written before every keystroke is reported" in {
        for
            held <- Signal.initRef("")
            seen <- Signal.initRef(List.empty[(String, String)])
            n    <- number(uic.InputNumber().text(held).onTextChange(v => held.get.map(h => seen.updateAndGet(_ :+ (v, h)).unit)).render)
            _    <- typeText(n, "-")
            s    <- seen.get
        yield assert(s == List(("-", "-")), s"reported, held: $s")
    }

end InputNumberTest
