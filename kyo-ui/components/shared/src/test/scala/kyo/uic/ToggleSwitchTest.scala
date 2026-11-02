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

    "a toggle its owner does not take is put back" in {
        for
            held <- Signal.initRef(false)
            ui = uic.ToggleSwitch().checked(held.readOnly).onChange(_ => ()).render
            native  <- box(ui)
            patches <- assertsAfter(ui)("p-toggleswitch-input")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield
            assert(follows(native.checked), s"the native box follows the signal: ${native.checked}")
            assert(patches == Chunk(("checked", "false")), s"patches: $patches")
        end for
    }

    "a constant with a listener puts back a toggle the owner does not take" in {
        for patches <- assertsAfter(
                uic.ToggleSwitch().checked(false).onChange(_ => ()).render
            )("p-toggleswitch-input")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield assert(patches == Chunk(("checked", "false")), s"patches: $patches")
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
            assert(follows(b.checked), s"follows the signal: ${b.checked}")
            assert(s == List(false) && h, s"reported $s, held $h")
    }

end ToggleSwitchTest
