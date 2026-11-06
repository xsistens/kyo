package kyo.uic

import kyo.*
import kyo.UI.render

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
        def errors(c: uic.ToggleButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.ToggleButton().checked(held))
            ro     <- errors(uic.ToggleButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleButton().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

    "a host region that runs again keeps what the reader pressed and shows the run's props" in {
        def drawn(t: UI): Boolean < Sync = elementsWithClass(t, "p-togglebutton").map(_.nonEmpty)
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(uic.ToggleButton().onLabel(s"on $n"))))
            after <- publishedAfterReady(ui, _ => true, until = drawn)(
                t => elementWithClass(t, "p-togglebutton").map(click),
                _ => setSettled(tick, 1)
            )
            shown = after.getOrElse(ui)
            held  <- pressed(shown)
            label <- elementWithClass(shown, "p-togglebutton-label")
        yield assert(held && textOf(label) == "on 1", s"after the host's run: pressed $held, label ${textOf(label)}")
        end for
    }

    "a click after a host run writes the ref and reaches the onChange of that run" in {
        for
            tick  <- Signal.initRef(0)
            a     <- Signal.initRef(false)
            b     <- Signal.initRef(false)
            heard <- AtomicRef.init(Chunk.empty[String])
            ui = UI.div(tick.render(n =>
                UI.div(uic.ToggleButton().checked(if n == 0 then a else b).onChange(v => heard.getAndUpdate(_.append(s"run $n: $v")).unit))
            ))
            _  <- actedOn(ui, _ => true)(_ => setSettled(tick, 1), t => elementWithClass(t, "p-togglebutton").map(click))
            va <- a.get
            vb <- b.get
            hs <- heard.get
        yield assert((va, vb, hs) == (false, true, Chunk("run 1: true")), s"a $va, b $vb, heard $hs")
        end for
    }

    "a fixed constant of true is drawn pressed" in {
        for p <- pressed(uic.ToggleButton().checked(true).render)
        yield assert(p, "the constant is not shown")
    }

end ToggleButtonTest
