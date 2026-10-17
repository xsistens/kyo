package kyo.uic

import kyo.*
import kyo.UI.*

/** The carousel's indicator dots rove their tabindex, so every inactive dot is out of the Tab
  * order and the arrows are the only way to reach it.
  *
  * The wiring lives in the mount that mints the strip id, so these drive the `wired` seam with a
  * recording focus function rather than a `UI.Commands`.
  */
class CarouselTest extends UicTest:

    private def strip(using Frame) =
        uic.Carousel[String]().items(Seq("a", "b", "c"))(s => p(s)).numVisible(1).numScroll(1)

    /** Presses `key` on the dot at `at` and returns the id focus was moved to, if any. */
    private def move(at: Int, key: UI.Keyboard, circular: Boolean = false)(using Frame): Maybe[String] < Async =
        for
            page  <- Signal.initRef(0)
            start <- Signal.initRef(0.0)
            moved <- Signal.initRef(Absent: Maybe[String])
            ui = (if circular then strip.circular(true) else strip)
                .wired(page, "car", start, id => moved.set(Present(id)))
            dots <- elementsWithClass(ui, "p-carousel-indicator-button")
            _    <- press(dots(at), key)
            to   <- moved.get
        yield to

    "ArrowRight moves to the next dot" in move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("car-d1")))

    "ArrowLeft moves back" in move(2, UI.Keyboard.ArrowLeft).map(to => assert(to == Present("car-d1")))

    "Home and End reach the ends" in {
        for
            home <- move(2, UI.Keyboard.Home)
            end  <- move(0, UI.Keyboard.End)
        yield assert(home == Present("car-d0") && end == Present("car-d2"))
    }

    "the vertical arrows belong to the page under a horizontal carousel" in {
        for
            down <- move(0, UI.Keyboard.ArrowDown)
            up   <- move(0, UI.Keyboard.ArrowUp)
        yield assert(down == Absent && up == Absent)
    }

    "Enter and Space move nothing, since a dot is a button the browser activates" in {
        for
            enter <- move(0, UI.Keyboard.Enter)
            space <- move(0, UI.Keyboard.Space)
        yield assert(enter == Absent && space == Absent)
    }

    "only the active dot is in the Tab order" in {
        for
            page  <- Signal.initRef(1)
            start <- Signal.initRef(0.0)
            ui    <- Kyo.lift(strip.wired(page, "car", start, _ => ()))
            dots  <- elementsWithClass(ui, "p-carousel-indicator-button")
        yield
            assert(dots(1).attrs.tabIndex.contains(0), "the active dot is the tab stop")
            assert(dots(0).attrs.tabIndex.contains(-1) && dots(2).attrs.tabIndex.contains(-1))
    }

    "a carousel that does not wrap stops at its ends, and a circular one comes round" in {
        // `wrap` follows `circular`, so the dots agree with what the next-page button would do.
        for
            stopped <- move(2, UI.Keyboard.ArrowRight)
            round   <- move(2, UI.Keyboard.ArrowRight, circular = true)
        yield
            assert(stopped == Absent, "the last dot has nowhere further to go")
            assert(round == Present("car-d0"), "unless the carousel is circular")
    }

end CarouselTest
