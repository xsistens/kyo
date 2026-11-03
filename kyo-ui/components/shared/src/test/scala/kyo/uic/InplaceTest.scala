package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Inplace]]'s display side is a `div` carrying `role="button"` and a tab stop of its own, so
  * both activation keys are the component's to deliver. Which side shows is held to rule A
  * ([[ReactiveValueContract]]): unbound it is the inplace's own, starting on the display.
  */
class InplaceTest extends ReactiveValueContract:

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

    private def inplace(using Frame) = uic.Inplace().display(span("View")).content(p("Full")).closable(true)

    holds(new SlotCase[Inplace, Boolean](
        name = "active",
        bare = Present(inplace),
        bind = inplace.active(_),
        hear = (i, f) => i.onActiveChange(f),
        event = "onActiveChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-inplace-display"),
        shows = elementsWithClass(_, "p-inplace-content").map(_.nonEmpty)
    ))

    "a fixed display keeps its tab stop and answers no key either" in {
        for d <- elementWithClass(inplace.active(false).render, "p-inplace-display")
        yield assert(d.attrs.onKeyDown.isEmpty && d.attrs.tabIndex.contains(0), "reachable, and no key opens it")
    }

    "the close button of an open content asks to close it, and is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            ui = inplace.active(held.readOnly).onActiveChange(v => seen.set(Present(v))).render
            close <- elementWithClass(ui, "p-button")
            _     <- click(close)
            h     <- held.get
            s     <- seen.get
        yield assert(h && s == Present(false), s"held $h, reported $s")
    }

end InplaceTest
