package kyo.uic

import kyo.*

/** The `checked` slot holds to rule A ([[ReactiveValueContract]]). The button draws its state itself, so it is a click on
  * the button that asks for the other state, and `aria-disabled` (a `role="button"` has no readonly state) that says a
  * fixed one changes nothing.
  */
class ToggleButtonTest extends ReactiveValueContract:

    /** What the button shows: `aria-pressed`, which the checked class must agree with. */
    private def pressed(t: UI)(using Frame): Boolean < Sync =
        for
            el      <- elementWithClass(t, "p-togglebutton")
            checked <- carriesClass(el, "p-togglebutton-checked")
        yield
            val says = el.attrs.ariaAttrs.get("pressed").contains("true")
            if says != checked then throw new AssertionError(s"aria-pressed says $says, the checked class $checked")
            says

    holds(new SlotCase[uic.ToggleButton, Boolean](
        name = "checked",
        bare = Present(uic.ToggleButton()),
        bind = uic.ToggleButton().checked(_),
        hear = (b, f) => b.onChange(f),
        event = "onChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-togglebutton"),
        shows = pressed(_),
        saysFixed = {
            case b: UI.Ast.Button => b.attrs.ariaAttrs.get("disabled").contains("true") && !b.disabled.contains(true)
            case _                => false
        }
    ))

    "a signal nobody hears says nothing where it reaches somebody or nobody can toggle it: a ref, readonly, disabled" in {
        def errors(c: uic.ToggleButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.ToggleButton().checked(held))
            ro     <- errors(uic.ToggleButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleButton().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

    "a fixed constant of true is drawn pressed" in {
        for p <- pressed(uic.ToggleButton().checked(true).render)
        yield assert(p, "the constant is not shown")
    }

end ToggleButtonTest
