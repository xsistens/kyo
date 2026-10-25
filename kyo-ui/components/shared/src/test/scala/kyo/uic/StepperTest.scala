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

    // The base the headers derive their ids from: `s"$base-$index"`, so "s-0", "s-1" and "s-2".
    private val base = "s"

    // Driven by position alone, so no type argument: see [[Stepper.apply]].
    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    private def steps(using Frame) =
        uic.Stepper()
            .step("One")(p("first"))
            .step("Two")(p("second"))
            .step("Three")(p("third"))

    private def move(vertical: Boolean, at: Int, key: UI.Keyboard, cur: Int = 0)(using Frame): Maybe[String] < Async =
        for
            active <- Signal.initRef(cur)
            moved  <- Signal.initRef(Absent: Maybe[String])
            shape = if vertical then steps.vertical(true) else steps
            ui    = shape.active(active).wired(base, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-step-header")
            _       <- press(headers(at), key)
            to      <- moved.get
        yield to

    "the horizontal layout moves on the horizontal arrows" in {
        for
            right <- move(vertical = false, 0, UI.Keyboard.ArrowRight)
            left  <- move(vertical = false, 2, UI.Keyboard.ArrowLeft)
        yield assert(right == Present("s-1") && left == Present("s-1"))
    }

    "and leaves the vertical arrows to the page" in
        move(vertical = false, 0, UI.Keyboard.ArrowDown).map(to => assert(to == Absent))

    "the vertical layout moves on the vertical arrows" in {
        for
            down <- move(vertical = true, 0, UI.Keyboard.ArrowDown)
            up   <- move(vertical = true, 2, UI.Keyboard.ArrowUp)
        yield assert(down == Present("s-1") && up == Present("s-1"))
    }

    "and leaves the horizontal arrows to the page" in
        move(vertical = true, 0, UI.Keyboard.ArrowRight).map(to => assert(to == Absent))

    "only the horizontal layout is a tablist, so only it roves its tab stop" in {
        for
            active <- Signal.initRef(1)
            flat = steps.active(active).wired(base, _ => ())
            tall = steps.vertical(true).active(active).wired(base, _ => ())
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
            ui = steps.linear(true).active(active).wired(base, id => moved.set(Present(id)))
            headers <- elementsWithClass(ui, "p-step-header")
            _       <- press(headers(0), UI.Keyboard.ArrowRight)
            to      <- moved.get
        yield assert(to == Absent)
    }

    "a stepper needs no type argument: K comes from the values, and is Nothing without any" in {
        val pre = "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\n"
        typeCheck(pre + "def x(r: SignalRef[Int])(using Frame): uic.Stepper[Nothing] = uic.Stepper().active(r).step(\"One\")(p(\"a\"))")
        typeCheck(
            pre +
                "def x(r: SignalRef[String])(using Frame): uic.Stepper[String] = uic.Stepper().value(r).step(\"One\", value = Present(\"one\"))(p(\"a\"))"
        )
        // Once settled at Nothing, a value has nowhere to go.
        typeCheckFailure(
            pre +
                "def x(r: SignalRef[Int])(using Frame) =\n  val s = uic.Stepper().active(r).step(\"One\")(p(\"a\"))\n  s.step(\"Two\", value = Present(\"two\"))(p(\"b\"))"
        )
        // Steps handed over as values settle K the same way; a `_` builder needs K settled before it, by `value`.
        typeCheck(pre + "def x(using Frame): uic.Stepper[Nothing] = uic.Stepper().steps(uic.Step(\"One\")(p(\"a\")))")
        typeCheck(
            pre +
                "def x(r: SignalRef[String])(using Frame): uic.Stepper[String] = uic.Stepper().value(r).steps(_.step(\"One\", value = Present(\"one\"))(p(\"a\")))"
        )
        // And a value of another type than the bound one does not compile either.
        typeCheckFailure(pre +
            "def x(r: SignalRef[String])(using Frame) = uic.Stepper().value(r).step(\"One\", value = Present(1))(p(\"a\"))")
    }

    "and a header click moves it by position" in {
        for
            active <- Signal.initRef(0)
            ui = steps.active(active).wired(base, _ => ())
            headers <- elementsWithClass(ui, "p-step-header")
            _       <- click(headers(2))
            now     <- active.get
        yield assert(now == 2)
    }

    "steps from a signal: a new step adds a header, and the bound value finds it" in {
        // A value no step carries shows the first step; once the signal brings the step that carries it, that one is
        // active, in the same render, with an id of its own.
        for
            live  <- Signal.initRef(Seq(uic.Step("Two", value = Present("two"))(p("second"))))
            value <- Signal.initRef("three")
            ui = uic.Stepper[String]().steps(_.step("One", value = Present("one"))(p("first")), live).value(value).wired(base, _ => ())
            before <- renderHtml(ui)
            _      <-
                live.set(Seq(uic.Step("Two", value = Present("two"))(p("second")), uic.Step("Three", value = Present("three"))(p("third"))))
            after   <- renderHtml(ui)
            headers <- elementsWithClass(ui, "p-step-header")
        yield
            assert(before.contains("first") && !before.contains("second"), "a value no step carries shows the first step")
            assert(after.contains("third") && !after.contains("first"), "the step the signal adds is the one the value names")
            assert(headers.map(_.attrs.identifier).toList == List(Present("s-0"), Present("s-1"), Present("s-2")))
        end for
    }

    "an index past the steps there are shows the last one, until the signal brings the step it names" in {
        for
            live   <- Signal.initRef(Seq.empty[uic.Step[Nothing]])
            active <- Signal.initRef(1)
            ui = uic.Stepper().steps(uic.Step("One")(p("first")), live).active(active).wired(base, _ => ())
            before <- renderHtml(ui)
            _      <- live.set(Seq(uic.Step("Two")(p("second"))))
            after  <- renderHtml(ui)
        yield
            assert(before.contains("first"), "clamped to the only step")
            assert(after.contains("second") && !after.contains("first"), "the step the index names, once it is there")
        end for
    }

end StepperTest
