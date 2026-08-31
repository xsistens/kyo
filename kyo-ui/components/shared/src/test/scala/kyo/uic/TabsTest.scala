package kyo.uic

import kyo.*
import kyo.UI.*

/** A tablist is ONE tab stop: the roving tabindex takes every inactive header out of the Tab
  * order, so the arrows are the only way back to them. Before this the headers were simply
  * unreachable from the keyboard.
  *
  * The wiring lives in a mount, which a pure render shows only as its placeholder, so these drive
  * the `wired` seam with a recording focus function instead of a `UI.Commands`.
  */
class TabsTest extends UicTest:

    private val ids = List("t0", "t1", "t2")

    private def strip(using Frame) =
        uic.Tabs().tabs(
            uic.Tab("One", p("first"), "one"),
            uic.Tab("Two", p("second"), "two", disabled = true),
            uic.Tab("Three", p("third"), "three")
        )

    /** Presses `key` on the header at `at` and returns the id focus was moved to, if any. */
    private def move(at: Int, key: UI.Keyboard)(using Frame): Maybe[String] < Async =
        for
            moved <- Signal.initRef(Absent: Maybe[String])
            ui = strip.wired(ids, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-tab")
            _       <- press(headers(at), key)
            to      <- moved.get
        yield to

    "ArrowRight moves to the next header" in move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("t2")))

    "and steps over a disabled one on the way" in {
        // Position 1 is disabled, so ArrowRight from 0 lands on 2 rather than on it.
        move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("t2")))
    }

    "ArrowLeft moves back" in move(2, UI.Keyboard.ArrowLeft).map(to => assert(to == Present("t0")))

    "the ends wrap, as Prime's tablist does" in {
        for
            forward <- move(2, UI.Keyboard.ArrowRight)
            back    <- move(0, UI.Keyboard.ArrowLeft)
        yield assert(forward == Present("t0") && back == Present("t2"))
    }

    "Home and End reach the ends directly" in {
        for
            home <- move(2, UI.Keyboard.Home)
            end  <- move(0, UI.Keyboard.End)
        yield assert(home == Present("t0") && end == Present("t2"))
    }

    "the vertical arrows belong to the page, not to a horizontal tablist" in {
        for
            down <- move(0, UI.Keyboard.ArrowDown)
            up   <- move(0, UI.Keyboard.ArrowUp)
        yield assert(down == Absent && up == Absent)
    }

    "Enter and Space move nothing, since the header is a button the browser activates" in {
        for
            enter <- move(0, UI.Keyboard.Enter)
            space <- move(0, UI.Keyboard.Space)
        yield assert(enter == Absent && space == Absent)
    }

    "only the active header is in the Tab order, and a disabled one is in neither" in {
        for
            active  <- Signal.initRef("three")
            ui      <- Kyo.lift(strip.selected(active).wired(ids, _ => ()))
            headers <- elementsWithClass(ui, "p-tab")
        yield
            assert(headers(0).attrs.tabIndex.contains(-1), "an inactive header is out of the Tab order")
            assert(headers(2).attrs.tabIndex.contains(0), "the active one is the tab stop")
            assert(headers(1).attrs.tabIndex.isEmpty, "and a disabled one carries no tabindex at all")
            assert(headers(1).attrs.onKeyDown.isEmpty, "nor a key handler")
    }

end TabsTest
