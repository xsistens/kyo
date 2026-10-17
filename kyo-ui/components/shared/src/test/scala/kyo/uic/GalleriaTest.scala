package kyo.uic

import kyo.*
import kyo.UI.*

/** Galleria carries the roving shape twice, on its indicator dots and on its thumbnail strip, so
  * before the arrows every inactive dot AND every inactive thumbnail was out of the Tab order with
  * nothing to reach it by.
  */
class GalleriaTest extends UicTest:

    private val pics = List(
        uic.GalleriaItem("/a.png", "A"),
        uic.GalleriaItem("/b.png", "B"),
        uic.GalleriaItem("/c.png", "C")
    )

    private def gallery(using Frame) = uic.Galleria().items(pics*).showIndicators(true).showThumbnails(true)

    /** Presses `key` on the element at `at` in the strip named by `cls`, returning where focus went. */
    private def move(cls: String, at: Int, key: UI.Keyboard)(using Frame): Maybe[String] < Async =
        for
            active <- Signal.initRef(0)
            moved  <- Signal.initRef(Absent: Maybe[String])
            ui = gallery.activeIndex(active).wired("g", id => moved.set(Present(id)))
            els <- elementsWithClass(ui, cls)
            _   <- press(els(at), key)
            to  <- moved.get
        yield to

    "ArrowRight moves along the indicator dots" in
        move("p-galleria-indicator-button", 0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("g-d1")))

    "ArrowLeft moves back along them" in
        move("p-galleria-indicator-button", 2, UI.Keyboard.ArrowLeft).map(to => assert(to == Present("g-d1")))

    "the thumbnail strip moves the same way, with ids of its own" in {
        for
            right <- move("p-galleria-thumbnail", 0, UI.Keyboard.ArrowRight)
            left  <- move("p-galleria-thumbnail", 2, UI.Keyboard.ArrowLeft)
        yield assert(right == Present("g-t1") && left == Present("g-t1"))
    }

    "Home and End reach the ends of a strip" in {
        for
            home <- move("p-galleria-indicator-button", 2, UI.Keyboard.Home)
            end  <- move("p-galleria-indicator-button", 0, UI.Keyboard.End)
        yield assert(home == Present("g-d0") && end == Present("g-d2"))
    }

    "a gallery that does not come round stops at its ends" in
        move("p-galleria-indicator-button", 2, UI.Keyboard.ArrowRight).map(to => assert(to == Absent))

    "Enter and Space move nothing, since both strips are made of buttons" in {
        for
            enter <- move("p-galleria-thumbnail", 0, UI.Keyboard.Enter)
            space <- move("p-galleria-thumbnail", 0, UI.Keyboard.Space)
        yield assert(enter == Absent && space == Absent)
    }

    "the placeholder render carries neither ids nor key handlers" in {
        // No mount has run there, so there is nothing to move focus with; the strips stay exactly
        // what they were, which is reachable by clicking.
        for
            active <- Signal.initRef(0)
            ui = gallery.activeIndex(active).render
            dots <- elementsWithClass(ui, "p-galleria-indicator-button")
        yield assert(dots.forall(_.attrs.onKeyDown.isEmpty) && dots.forall(_.attrs.identifier.isEmpty))
    }

end GalleriaTest
