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

end InputNumberTest
