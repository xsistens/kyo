package kyo.uic

import kyo.*

/** The `checked` slot holds to rule A ([[ReactiveValueContract]]); the cases after it are what only a native switch does: it
  * paints a toggle its owner does not take away again, and it stays quiet wherever nobody can toggle it.
  */
class ToggleSwitchTest extends ReactiveValueContract:

    holds(nativeBoolean[uic.ToggleSwitch](
        name = "checked",
        bare = Present(uic.ToggleSwitch()),
        bind = uic.ToggleSwitch().checked(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        input = "p-toggleswitch-input",
        root = "p-toggleswitch",
        checkedClass = "p-toggleswitch-checked"
    ))

    private def box(ui: UI)(using Frame): UI.Ast.Checkbox < Sync =
        elements(ui).map(_.collectFirst { case c: UI.Ast.Checkbox => c }.getOrElse(throw new AssertionError("no switch input")))

    private def toggle(b: UI.Ast.Checkbox, to: Boolean)(using Frame): Any < Async =
        handled(b.onChange.getOrElse(throw new AssertionError("no toggle wired"))(to))

    "a signal nobody hears says nothing where it reaches somebody or nobody can toggle it: a ref, readonly, disabled" in {
        def errors(c: uic.ToggleSwitch): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.ToggleSwitch().checked(held))
            ro     <- errors(uic.ToggleSwitch().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleSwitch().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

    "a toggle its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(false)
            again <-
                repaintsAfter(uic.ToggleSwitch().checked(held.readOnly).onChange(_ => ()).render, held)(t => box(t).map(toggle(_, true)))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a constant with a listener paints itself again after a report, so a toggle the owner does not take is put back" in {
        for again <- repaintsAfter(uic.ToggleSwitch().checked(false).onChange(_ => ()).render)(t => box(t).map(toggle(_, true)))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a fixed constant of true is drawn on" in {
        for
            b       <- box(uic.ToggleSwitch().checked(true).render)
            root    <- elementWithClass(uic.ToggleSwitch().checked(true).render, "p-toggleswitch")
            checked <- carriesClass(root, "p-toggleswitch-checked")
        yield assert(b.checked.exists { case UI.Bound.Const(c) => c; case _ => false } && checked, s"shows the constant: ${b.checked}")
    }

    "a signal bound one way to true is shown on, and turning it off is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(List.empty[Boolean])
            b    <- box(uic.ToggleSwitch().checked(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render)
            _    <- toggle(b, false)
            h    <- held.get
            s    <- seen.get
        yield
            assert(b.checked.exists { case UI.Bound.Const(c) => c; case _ => false }, s"shows the signal: ${b.checked}")
            assert(s == List(false) && h, s"reported $s, held $h")
    }

end ToggleSwitchTest
