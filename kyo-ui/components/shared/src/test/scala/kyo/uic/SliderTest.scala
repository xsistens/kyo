package kyo.uic

import kyo.*

/** The `value` slot bound one way: the slider shows what its owner holds, and a drag its owner does not take is painted
  * away again, since the native range input has already moved under the reader's hand.
  */
class SliderTest extends UicTest:

    private def range(ui: UI)(using Frame): UI.Ast.RangeInput < Sync =
        elements(ui).map(_.collectFirst { case r: UI.Ast.RangeInput => r }.getOrElse(throw new AssertionError("no range input")))

    "a drag its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(20.0)
            again <- repaintsAfter(uic.Slider().value(held.readOnly).onChange(_ => ()).render, held)(t =>
                range(t).map(r => handled(r.onChange.getOrElse(throw new AssertionError("no change handler"))(60.0)))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end SliderTest
