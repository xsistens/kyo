package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.Rating`'s keyboard, which was undone by its own second handler.
  *
  * Selecting a radio from the keyboard is an ACTIVATION: the browser checks the radio, fires
  * `change`, and dispatches a `click` that bubbles out of the input. The stars carried a handler
  * for each of those. The `change` wrote the star the arrow had landed on, the region re-rendered
  * around it, and the `click` then arrived at an option whose handler had been rebuilt with the
  * new value in hand, read it as a second pick of the same star, and applied Prime's
  * cancel-on-same-value: every arrow press ended at zero.
  *
  * One activation, one handler. The click is the one that survives, because it is the one both
  * inputs produce: a mouse press on the visible star reaches only the option (the radio is
  * clipped away), and a key press reaches it through the input.
  */
class RatingTest extends UicTest:

    /** The five `p-rating-option` boxes, in star order. */
    private def options(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementsWithClass(ui, "p-rating-option")

    private def radios(ui: UI)(using Frame): Chunk[UI.Ast.Radio] < Sync =
        elements(ui).map(_.collect { case r: UI.Ast.Radio => r })

    /** What the browser does for one arrow key: check the radio, fire its `change`, then let the
      * activation click bubble to the option.
      *
      * The options are read AGAIN between the two, because that is the part that bites: the
      * `change` writes the ref, the region re-renders, and the click therefore lands on a handler
      * built from the value the change just wrote.
      */
    private def keyboardPick(ui: UI, star: Int)(using Frame): Any < Async =
        for
            rs <- radios(ui)
            change: (Any < Async) = rs(star - 1).onChange match
                case Present(f) => f(true)
                case Absent     => ()
            _  <- change
            os <- options(ui)
            r  <- click(os(star - 1))
        yield r

    "an arrow key picks the star it lands on" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            _   <- keyboardPick(ui, 4)
            got <- ref.get
        yield assert(got == 4, "the arrow moved to the fourth star, so the fourth star is the value")
    }

    "and holding it keeps counting up rather than falling back to nothing" in {
        for
            ref <- Signal.initRef(3)
            _   <- keyboardPick(uic.Rating().value(ref).wired("r"), 4)
            _   <- keyboardPick(uic.Rating().value(ref).wired("r"), 5)
            got <- ref.get
        yield assert(got == 5)
    }

    "one activation is wired once: the radios carry no second handler" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            rs <- radios(ui)
            os <- options(ui)
        yield
            assert(rs.size == 5 && rs.forall(_.onChange.isEmpty), "a change handler would fire beside the click")
            assert(os.forall(_.attrs.onClick.isDefined), "and the click is the handler both inputs produce")
    }

    "picking the star the value already sits on clears it, from either input" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            os  <- options(ui)
            _   <- click(os(2))
            got <- ref.get
        yield assert(got == 0, "Prime's cancel-on-same-value, and now reachable from the keyboard too")
    }

    "the radios share the minted group name, which is what gives the stars one tab stop" in {
        for
            ui <- Kyo.lift(uic.Rating().value(3).wired("stars-1"))
            rs <- radios(ui)
        yield assert(rs.forall(_.name.contains("stars-1")))
    }

end RatingTest
