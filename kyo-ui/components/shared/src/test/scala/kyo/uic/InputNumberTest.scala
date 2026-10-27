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

end InputNumberTest
