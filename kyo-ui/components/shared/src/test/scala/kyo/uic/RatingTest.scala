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
  *
  * Both `value` slots hold to rule A ([[ReactiveValueContract]]) through a click on the fourth star, and say they are fixed
  * on the group, which is where a radio group's `aria-readonly` lives.
  */
class RatingTest extends ReactiveValueContract:

    /** The five `p-rating-option` boxes, in star order: those of the tree published last, where several are. */
    private def options(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementsWithClass(ui, "p-rating-option").map(_.take(5))

    private def radios(ui: UI)(using Frame): Chunk[UI.Ast.Radio] < Sync =
        elements(ui).map(_.collect { case r: UI.Ast.Radio => r }.take(5))

    /** The stars a render shows: the radio that is checked (0 where none is), which the stars drawn active must agree with. */
    private def stars(t: UI)(using Frame): Int < Sync =
        for
            rs <- radios(t)
            os <- options(t)
            on <- Kyo.foreach(rs)(_.checked match
                case Present(UI.Bound.Const(c))    => Kyo.lift(c)
                case Present(UI.Bound.Follow(sig)) => sig.current
                case _                             => Kyo.lift(false))
        yield
            val native = on.indexWhere(identity) + 1
            val drawn  = os.count(_.attrs.cssClasses.contains("p-rating-option-active"))
            if drawn != native then throw new AssertionError(s"drawn $drawn active stars over a checked star $native")
            native

    /** The group, which says the rating is fixed: a radio has no readonly state of its own. */
    private def group(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-rating")

    private def readonlyGroup(g: UI.Ast.Element): Boolean =
        g.attrs.ariaAttrs.get("readonly").contains("true") && g.attrs.cssClasses.contains("p-readonly") &&
            g.attrs.dataAttrs.get("kyo-inert").contains("1")

    /** The radios grouped by the name the mount mints, which is when the tree the reader clicks in is the one it meets. */
    private def named(t: UI)(using Frame): Boolean < Sync = radios(t).map(rs => rs.size == 5 && rs.forall(_.name.isDefined))

    holds(new Slot[uic.Rating, Int](
        name = "value",
        bare = Present(uic.Rating()),
        bind = uic.Rating().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0,
        next = 4,
        control = t => options(t).map(_(3)),
        shows = stars(_),
        ready = Present(named(_)),
        fixedOn = Present(group(_)),
        saysFixed = readonlyGroup
    ))

    // The form layer's `Double` binding reports through the same `onChange(Int)`: the slot is held with that report read
    // as the `Double` it is. Unbound there is one state, the `Int` slot's, so this slot has no unbound form of its own.
    holds(new Slot[uic.Rating, Double](
        name = "value as a Double",
        bare = Absent,
        bind = uic.Rating().value(_),
        hear = (c, f) => c.onChange((stars: Int) => f(stars.toDouble)),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 4.0,
        control = t => options(t).map(_(3)),
        shows = t => stars(t).map(_.toDouble),
        ready = Present(named(_)),
        fixedOn = Present(group(_)),
        saysFixed = readonlyGroup
    ))

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
                case Present(f) => handled(f(true))
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

    "picking the star the value already sits on clears it" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            os  <- options(ui)
            _   <- click(os(2))
            got <- ref.get
        yield assert(got == 0, "Prime's cancel-on-same-value")
    }

    "Space reaches that clear, since it is the one press the browser dispatches no click for" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            rs  <- radios(ui)
            _   <- press(rs(2), UI.Keyboard.Space)
            got <- ref.get
        yield assert(got == 0)
    }

    "and no other star answers a key, or the click that follows it would fire twice" in {
        for
            ref <- Signal.initRef(3)
            ui = uic.Rating().value(ref).wired("r")
            rs <- radios(ui)
        yield assert(rs.zipWithIndex.forall((r, i) => r.attrs.onKeyDown.isDefined == (i == 2)))
    }

    "the radios share the minted group name, which is what gives the stars one tab stop" in {
        for
            ui <- Kyo.lift(uic.Rating().value(3).wired("stars-1"))
            rs <- radios(ui)
        yield assert(rs.forall(_.name.contains("stars-1")))
    }

    "a star its owner does not take is put back on the native radio, which carries no handler" in {
        for
            held    <- Signal.initRef(2)
            patches <- assertsAfter(
                uic.Rating().value(held.readOnly).onChange(_ => ()).render
            )("""aria-label="4 stars"""")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield assert(patches == Chunk(("checked", "false")), s"patches: $patches")
        end for
    }

    "a constant with a listener puts back a star the owner does not take" in {
        for
            first   <- stars(uic.Rating().value(2).onChange(_ => ()).render)
            patches <-
                assertsAfter(uic.Rating().value(2).onChange(_ => ()).render)("""aria-label="4 stars"""")(kyo.internal.UIEvent.ChangeChecked(
                    _,
                    true
                ))
        yield
            assert(first == 2, s"the page as first rendered shows $first stars for the constant 2")
            assert(patches == Chunk(("checked", "false")), s"patches: $patches")
        end for
    }

    "and so does a constant bound as a Double" in {
        for
            first   <- stars(uic.Rating().value(2.0).onChange(_ => ()).render)
            patches <- assertsAfter(
                uic.Rating().value(2.0).onChange(_ => ()).render
            )("""aria-label="4 stars"""")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield
            assert(first == 2, s"the page as first rendered shows $first stars for the constant 2")
            assert(patches == Chunk(("checked", "false")), s"patches: $patches")
        end for
    }

    "a value bound one way says nothing where it reaches somebody or nobody can pick: a ref, disabled" in {
        def errors(c: uic.Rating): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(2)
            twoWay <- errors(uic.Rating().value(held))
            off    <- errors(uic.Rating().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can change it")
        end for
    }

    "a fixed constant is drawn with its stars, whichever slot holds it" in {
        for
            int    <- stars(uic.Rating().value(2).render)
            double <- stars(uic.Rating().value(2.0).render)
        yield assert(int == 2 && double == 2, s"shows $int and $double for the constant 2")
    }

end RatingTest
