package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Panel]]'s collapse state, under every way it can be bound: unbound it is the panel's own, a constant nobody hears
  * is fixed, a signal is followed and a press only reported, and a ref is written.
  */
class PanelTest extends UicTest:

    private def panel(using Frame) = uic.Panel().header("Details").toggleable(true)(p("body"))

    private def toggles(t: UI)(using Frame) = elementsWithClass(t, "p-panel-toggle-button")

    private def shut(t: UI)(using Frame) = elementsWithClass(t, "p-uic-collapsed").map(_.nonEmpty)

    "an unbound toggleable panel keeps the state itself, so a press collapses it" in {
        for
            after <- publishedAfterReady(panel.render, t => toggles(t).map(_.exists(declaresClick)), until = shut)(t =>
                toggles(t).map(b => click(b.head))
            )
            card <- elementsWithClass(panel.render, "p-uic-key-error")
        yield
            assert(after.nonEmpty, "the panel never collapsed")
            assert(card.isEmpty, s"state the panel keeps itself: ${card.map(textOf)}")
    }

    "the page as first rendered does not call an unbound toggle fixed" in {
        for b <- elementWithClass(panel.render, "p-panel-toggle-button")
        yield assert(!b.attrs.ariaAttrs.contains("disabled"), "the toggle the reader will press is not disabled")
    }

    "a constant nobody hears is fixed: the toggle stays reachable, changes nothing, and says so" in {
        val fixed = panel.collapsed(true)
        for
            b     <- elementWithClass(fixed.render, "p-panel-toggle-button")
            shown <- shut(fixed.render)
            card  <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(shown, "the panel shows the state it was given")
            assert(!declaresClick(b), "a press changes nothing")
            assert(b.attrs.ariaAttrs.get("disabled").contains("true"), "and the toggle says so")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
        end for
    }

    "a constant somebody hears reports a press with the state it asks for" in {
        for
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            b    <- elementWithClass(panel.collapsed(false).onCollapsedChange(v => seen.set(Present(v))).render, "p-panel-toggle-button")
            _    <- click(b)
            s    <- seen.get
        yield assert(s == Present(true), s"reported $s")
    }

    "a signal bound one way is followed, and a press is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            ui = panel.collapsed(held.readOnly).onCollapsedChange(v => seen.set(Present(v))).render
            shown <- shut(ui)
            b     <- elementWithClass(ui, "p-panel-toggle-button")
            _     <- click(b)
            h     <- held.get
            s     <- seen.get
            deaf  <- elementsWithClass(panel.collapsed(held.readOnly).render, "p-uic-key-error")
        yield
            assert(shown, "the panel follows the signal")
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onCollapsedChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the panel reports it" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef((false, false))
            b    <- elementWithClass(
                panel.collapsed(held).onCollapsedChange(v => held.get.map(h => seen.set((v, h)))).render,
                "p-panel-toggle-button"
            )
            _ <- click(b)
            s <- seen.get
        yield assert(s == (true, true), s"reported, held: $s")
    }

end PanelTest
