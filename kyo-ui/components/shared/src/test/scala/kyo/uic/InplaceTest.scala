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

    private def inplace(using Frame) = uic.Inplace().display(span("View")).content(p("Full")).closable(true)

    private def displays(t: UI)(using Frame) = elementsWithClass(t, "p-inplace-display")

    "an unbound inplace keeps the side itself, so activating the display opens the content" in {
        for
            after <- publishedAfterReady(
                inplace.render,
                t => displays(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-inplace-content").map(_.nonEmpty)
            )(t => displays(t).map(d => click(d.head)))
            card <- elementsWithClass(inplace.render, "p-uic-key-error")
        yield
            assert(after.nonEmpty, "the content never opened")
            assert(card.isEmpty, s"a side the inplace keeps itself: ${card.map(textOf)}")
    }

    "the page as first rendered does not call an unbound display fixed" in {
        for d <- elementWithClass(inplace.render, "p-inplace-display")
        yield assert(!d.attrs.ariaAttrs.contains("disabled"), "the display the reader will press is not disabled")
    }

    "a constant nobody hears is fixed: the display stays reachable, opens nothing, and says so" in {
        for
            d    <- elementWithClass(inplace.active(false).render, "p-inplace-display")
            card <- elementsWithClass(inplace.active(false).render, "p-uic-key-error")
        yield
            assert(d.attrs.onClick.isEmpty && d.attrs.onKeyDown.isEmpty, "nothing opens it")
            assert(d.attrs.tabIndex.contains(0) && d.attrs.ariaAttrs.get("disabled").contains("true"), "reachable, and says so")
            assert(card.isEmpty, s"a fixed side is not a mistake: ${card.map(textOf)}")
    }

    "a signal bound one way is followed, and the close is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            ui = inplace.active(held.readOnly).onActiveChange(v => seen.set(Present(v))).render
            close <- elementWithClass(ui, "p-button")
            _     <- click(close)
            h     <- held.get
            s     <- seen.get
            deaf  <- elementsWithClass(inplace.active(held.readOnly).render, "p-uic-key-error")
            heard <- elementsWithClass(inplace.active(held.readOnly).onActiveChange(_ => ()).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onActiveChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty, s"onActiveChange hears both ways: ${heard.map(textOf)}")
        end for
    }

    "a ref is written before onActiveChange hears it" in {
        for
            held  <- Signal.initRef(false)
            order <- Signal.initRef(List.empty[String])
            ui = inplace.active(held).onActiveChange(v => held.get.map(h => order.updateAndGet(_ :+ s"change $v held $h"))).render
            d <- elementWithClass(ui, "p-inplace-display")
            _ <- click(d)
            o <- order.get
        yield assert(o == List("change true held true"), s"order: $o")
    }

end InplaceTest
