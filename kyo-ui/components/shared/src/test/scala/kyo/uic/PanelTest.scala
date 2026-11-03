package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Panel]]'s collapse state under rule A ([[ReactiveValueContract]]): a toggleable panel nobody binds keeps it itself,
  * starting expanded, and the toggle asks for the other state.
  */
class PanelTest extends ReactiveValueContract:

    private def panel(using Frame) = uic.Panel().header("Details").toggleable(true)(p("body"))

    holds(new SlotCase[Panel, Boolean](
        name = "collapsed",
        bare = Present(panel),
        bind = panel.collapsed(_),
        hear = (c, f) => c.onCollapsedChange(f),
        event = "onCollapsedChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-panel-toggle-button"),
        shows = elementsWithClass(_, "p-uic-collapsed").map(_.nonEmpty)
    ))

    // ---- the switch ----

    private def bareOf(using Frame) = uic.Panel().header("Details")

    private def togglesOf(t: UI)(using Frame) = elementsWithClass(t, "p-panel-toggle-button")

    private def collapsedHere(t: UI)(using Frame) =
        elementsWithClass(t, "p-panel-content-container").map(_.exists(_.attrs.cssClasses.contains("p-uic-collapsed")))

    "toggleable" - {
        "a binding alone turns the toggle on" in {
            Signal.initRef(false).map(held => togglesOf(bareOf.collapsed(held).render).map(ts => assert(ts.size == 1)))
        }
        "a binding alone follows it" in {
            Signal.initRef(true).map(held => collapsedHere(bareOf.collapsed(held).render).map(c => assert(c)))
        }
        "a hearer alone turns nothing on: no binding, no toggle" in {
            togglesOf(bareOf.onCollapsedChange(_ => ()).render).map(ts => assert(ts.isEmpty))
        }
        "false decides alone: over a binding the panel is plain, and nothing is said" in {
            for
                held <- Signal.initRef(true)
                ui = bareOf.collapsed(held.readOnly).toggleable(false).render
                ts    <- togglesOf(ui)
                c     <- collapsedHere(ui)
                cards <- elementsWithClass(ui, UicTest.cardClass)
            yield
                assert(ts.isEmpty, "no toggle")
                assert(!c, "the content shows")
                assert(cards.isEmpty, s"${cards.map(textOf)}")
            end for
        }
        "true unbound is its own, from the page as first rendered" in {
            togglesOf(bareOf.toggleable(true).render).map(ts => assert(ts.size == 1))
        }
        "true over a binding is the binding" in {
            Signal.initRef(true).map(held => collapsedHere(bareOf.toggleable(true).collapsed(held).render).map(c => assert(c)))
        }
        "a signal switch adds no mount over a binding, and one unbound, wherever it stands" in {
            for
                sw    <- Signal.initRef(false)
                held  <- Signal.initRef(false)
                bound <- mountsAbove(bareOf.collapsed(held).toggleable(sw).render, "p-panel")
                own   <- mountsAbove(bareOf.toggleable(sw).render, "p-panel")
            yield
                assert(bound == 0)
                assert(own == 1)
            end for
        }
        "a signal switch draws the toggle and the collapse while on, and neither while off" in {
            for
                sw   <- Signal.initRef(true)
                held <- Signal.initRef(true)
                ui = bareOf.collapsed(held).toggleable(sw).render
                on    <- togglesOf(ui)
                onC   <- collapsedHere(ui)
                _     <- sw.set(false)
                off   <- togglesOf(ui)
                offC  <- collapsedHere(ui)
                _     <- sw.set(true)
                back  <- togglesOf(ui)
                backC <- collapsedHere(ui)
            yield
                assert(on.size == 1 && onC)
                assert(off.isEmpty && !offC)
                assert(back.size == 1 && backC)
            end for
        }
        "a signal switch going off and on keeps what the reader collapsed" in {
            for
                sw    <- Signal.initRef(true)
                shown <- publishedAfterReady(
                    bareOf.toggleable(sw).render,
                    t => togglesOf(t).map(_.exists(declaresClick)),
                    until = collapsedHere(_)
                )(
                    t => togglesOf(t).map(ts => click(ts.find(declaresClick).get)),
                    _ => sw.set(false),
                    // The container is one region over the switch and the state: only once it has drawn the switch off is the
                    // next write one it has to draw, or the two would be one change that repaints nothing.
                    t =>
                        elementsWithClass(t, "p-panel-content-container").map(cs =>
                            if cs.headOption.exists(_.attrs.cssClasses.contains("p-uic-collapse")) then
                                throw new AssertionError("not drawn off yet")
                            else sw.set(true)
                        )
                )
            yield assert(shown.nonEmpty, "what the reader collapsed did not come back with the switch")
        }
        "a signal switch takes the card with it, and brings it back" in {
            for
                sw   <- Signal.initRef(true)
                held <- Signal.initRef(false)
                ui = bareOf.collapsed(held.readOnly).toggleable(sw).render
                on   <- elementsWithClass(ui, UicTest.cardClass)
                _    <- sw.set(false)
                off  <- elementsWithClass(ui, UicTest.cardClass)
                _    <- sw.set(true)
                back <- elementsWithClass(ui, UicTest.cardClass)
            yield
                assert(on.size == 1)
                assert(off.isEmpty, s"a stale card: ${off.map(textOf)}")
                assert(back.size == 1)
            end for
        }
    }

end PanelTest
