package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Inplace]]'s display side is a `div` carrying `role="button"` and a tab stop of its own, so
  * both activation keys are the component's to deliver.
  */
class InplaceTest extends UicTest:

    private def opened(key: UI.Keyboard)(using Frame): Boolean < Async =
        for
            active <- Signal.initRef(false)
            ui = uic.Inplace().display(span("View")).content(p("Full")).active(active).render
            el <- elementWithClass(ui, "p-inplace-display")
            _  <- press(el, key)
            v  <- active.get
        yield v

    "Enter opens the display side" in opened(UI.Keyboard.Enter).map(v => assert(v))

    "and so does Space, which a role of button owes the reader just as much" in
        opened(UI.Keyboard.Space).map(v => assert(v))

    "a key that is not an activation leaves it closed" in
        opened(UI.Keyboard.ArrowDown).map(v => assert(!v))

    "a disabled inplace answers neither key" in {
        for
            active <- Signal.initRef(false)
            ui = uic.Inplace().display(span("Locked")).content(p("never")).active(active).disabled(true).render
            el <- elementWithClass(ui, "p-inplace-display")
        yield assert(el.attrs.onKeyDown.isEmpty && el.attrs.onClick.isEmpty)
    }

end InplaceTest
