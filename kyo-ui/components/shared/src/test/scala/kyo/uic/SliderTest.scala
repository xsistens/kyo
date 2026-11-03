package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) through the native range input: the browser delivers the
  * value the reader dragged to as a change, and fixed is what `readonly` is (the move declined in the client, and
  * `aria-readonly`). The cases after it are what only this field does: a drag its owner does not take is painted away
  * again, since the native range input has already moved under the reader's hand.
  */
class SliderTest extends ReactiveValueContract:

    private def range(ui: UI)(using Frame): UI.Ast.RangeInput < Sync =
        elements(ui).map(_.collectFirst { case r: UI.Ast.RangeInput => r }.getOrElse(throw new AssertionError("no range input")))

    /** Where the native range input stands, which the fill and the handle are drawn from. */
    private def at(ui: UI)(using Frame): Double < Sync =
        range(ui).map(_.value match
            case Present(UI.Bound.Const(v))    => Kyo.lift(v)
            case Present(UI.Bound.Follow(sig)) => sig.current
            case other                         => throw new AssertionError(s"the range input is bound as $other"))

    holds(new SlotCase[uic.Slider, Double](
        name = "value",
        bare = Present(uic.Slider()),
        bind = uic.Slider().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 60.0,
        control = range(_),
        shows = at(_),
        // The browser moves the thumb and delivers the value it stands at as the change.
        gesture = {
            case r: UI.Ast.RangeInput => handled(r.onChange.getOrElse(throw new AssertionError("no change handler"))(60.0))
            case other                => throw new AssertionError(s"not a range input: $other")
        },
        answers = {
            case r: UI.Ast.RangeInput => r.onChange.nonEmpty
            case _                    => false
        },
        saysFixed = {
            case r: UI.Ast.RangeInput =>
                r.attrs.ariaAttrs.get("readonly").contains("true") && r.attrs.dataAttrs.get("kyo-inert").contains("1") &&
                !r.disabled.contains(true)
            case _ => false
        }
    ))

    "a drag its owner does not take is put back" in {
        for
            held    <- Signal.initRef(20.0)
            patches <- assertsAfter(
                uic.Slider().value(held.readOnly).onChange(_ => ()).render
            )("p-uic-slider-native")(kyo.internal.UIEvent.ChangeNumeric(_, 60.0))
        yield assert(patches == Chunk(("value", "20")), s"patches: $patches")
        end for
    }

    "a constant with a listener puts back a drag the owner does not take" in {
        for
            first   <- at(uic.Slider().value(30.0).onChange(_ => ()).render)
            patches <-
                assertsAfter(uic.Slider().value(30.0).onChange(_ => ()).render)("p-uic-slider-native")(kyo.internal.UIEvent.ChangeNumeric(
                    _,
                    60.0
                ))
        yield
            assert(first == 30.0, s"the page as first rendered shows $first for the constant 30")
            assert(patches == Chunk(("value", "30")), s"patches: $patches")
        end for
    }

    "a value bound one way says nothing where it reaches somebody or nobody can move it: a ref, disabled" in {
        def errors(c: uic.Slider): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(20.0)
            twoWay <- errors(uic.Slider().value(held))
            off    <- errors(uic.Slider().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can change it")
        end for
    }

    "readonly keeps the value where it is: the native input is declined, says so, and reports nothing" in {
        for r <- range(uic.Slider().value(30.0).onChange(_ => ()).readonly(true).render)
        yield
            assert(r.attrs.dataAttrs.get("kyo-inert").contains("1"), "the browser's own move is declined")
            assert(r.attrs.ariaAttrs.get("readonly").contains("true"), "and it says it is read-only")
            assert(r.onChange.isEmpty, "with no handler behind it")
    }

end SliderTest
