package kyo.uic

import kyo.*
import kyo.UI.*

/** The Stepper's two layouts are two different ARIA shapes, and the keyboard follows each.
  *
  * Horizontal puts every header in one `p-steplist`, which is a real tablist: `role="tab"`, ONE
  * tab stop, horizontal arrows between them. Vertical interleaves each header with its own panel
  * inside a `p-stepitem`, which is an accordion's shape rather than a tablist's, so the headers
  * carry no `role="tab"` there, each stays its own tab stop, and the arrows are vertical.
  */
class StepperTest extends UicTest:

    private val ids = List("s0", "s1", "s2")

    private def steps(using Frame) =
        uic.Stepper()
            .step("One")(p("first"))
            .step("Two")(p("second"))
            .step("Three")(p("third"))

    private def move(vertical: Boolean, at: Int, key: UI.Keyboard, cur: Int = 0)(using Frame): Maybe[String] < Async =
        for
            active <- Signal.initRef(cur)
            moved  <- Signal.initRef(Absent: Maybe[String])
            base = if vertical then steps.vertical(true) else steps
            ui   = base.active(active).wired(ids, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-step-header")
            _       <- press(headers(at), key)
            to      <- moved.get
        yield to

    "the horizontal layout moves on the horizontal arrows" in {
        for
            right <- move(vertical = false, 0, UI.Keyboard.ArrowRight)
            left  <- move(vertical = false, 2, UI.Keyboard.ArrowLeft)
        yield assert(right == Present("s1") && left == Present("s1"))
    }

    "and leaves the vertical arrows to the page" in
        move(vertical = false, 0, UI.Keyboard.ArrowDown).map(to => assert(to == Absent))

    "the vertical layout moves on the vertical arrows" in {
        for
            down <- move(vertical = true, 0, UI.Keyboard.ArrowDown)
            up   <- move(vertical = true, 2, UI.Keyboard.ArrowUp)
        yield assert(down == Present("s1") && up == Present("s1"))
    }

    "and leaves the horizontal arrows to the page" in
        move(vertical = true, 0, UI.Keyboard.ArrowRight).map(to => assert(to == Absent))

    "only the horizontal layout is a tablist, so only it roves its tab stop" in {
        for
            active <- Signal.initRef(1)
            flat = steps.active(active).wired(ids, _ => ())
            tall = steps.vertical(true).active(active).wired(ids, _ => ())
            flatHeaders <- elementsWithClass(flat, "p-step-header")
            tallHeaders <- elementsWithClass(tall, "p-step-header")
            list        <- elementWithClass(flat, "p-steplist")
        yield
            assert(list.attrs.role.contains("tablist"), "the horizontal step list says what it is")
            assert(flatHeaders(0).attrs.role.contains("tab"))
            assert(flatHeaders(1).attrs.tabIndex.contains(0) && flatHeaders(0).attrs.tabIndex.contains(-1))
            assert(tallHeaders.forall(_.attrs.role.isEmpty), "a tab outside a tablist is not a tab")
            assert(tallHeaders.forall(_.attrs.tabIndex.isEmpty), "so every vertical header stays a tab stop")
    }

    "a step the reader cannot reach is stepped over" in {
        // Linear mode blocks everything past the current step, so from step 0 there is nowhere
        // forward to go and the arrow stays put.
        for
            active <- Signal.initRef(0)
            moved  <- Signal.initRef(Absent: Maybe[String])
            ui = steps.linear(true).active(active).wired(ids, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-step-header")
            _       <- press(headers(0), UI.Keyboard.ArrowRight)
            to      <- moved.get
        yield assert(to == Absent)
    }

end StepperTest
