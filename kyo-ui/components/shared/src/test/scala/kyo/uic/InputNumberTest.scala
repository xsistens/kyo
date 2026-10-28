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

end InputNumberTest
