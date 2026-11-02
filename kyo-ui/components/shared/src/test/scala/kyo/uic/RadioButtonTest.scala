package kyo.uic

import kyo.*

/** The `checked` slot holds to rule A ([[ReactiveValueContract]]); the cases after it are what only a native radio does: it
  * paints a selection its owner does not take away again, and it stays quiet wherever nobody can select it.
  */
class RadioButtonTest extends ReactiveValueContract:

    holds(nativeBoolean[uic.RadioButton](
        name = "checked",
        bare = Present(uic.RadioButton()),
        bind = uic.RadioButton().checked(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        input = "p-radiobutton-input",
        root = "p-radiobutton",
        checkedClass = "p-radiobutton-checked",
        fixedAria = "disabled"
    ))

    private def radio(ui: UI)(using Frame): UI.Ast.Radio < Sync =
        elements(ui).map(_.collectFirst { case r: UI.Ast.Radio => r }.getOrElse(throw new AssertionError("no radio")))

    private def select(r: UI.Ast.Radio)(using Frame): Any < Async =
        handled(r.onChange.getOrElse(throw new AssertionError("no handler wired"))(true))

    "a signal nobody hears says nothing where it reaches somebody or nobody can select it: a ref, readonly, disabled" in {
        def errors(c: uic.RadioButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.RadioButton().checked(held))
            ro     <- errors(uic.RadioButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.RadioButton().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can select")
        end for
    }

    "a selection its owner does not take is put back" in {
        for
            held <- Signal.initRef(false)
            ui = uic.RadioButton().checked(held.readOnly).onChange(_ => ()).render
            native  <- radio(ui)
            patches <- assertsAfter(ui)("p-radiobutton-input")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield
            assert(follows(native.checked), s"the native box follows the signal: ${native.checked}")
            assert(patches == Chunk(("checked", "false")), s"patches: $patches")
        end for
    }

    "a constant with a listener puts back a selection the owner does not take" in {
        for patches <- assertsAfter(
                uic.RadioButton().checked(false).onChange(_ => ()).render
            )("p-radiobutton-input")(kyo.internal.UIEvent.ChangeChecked(_, true))
        yield assert(patches == Chunk(("checked", "false")), s"patches: $patches")
    }

    "a fixed constant of true is drawn selected" in {
        for
            r       <- radio(uic.RadioButton().checked(true).render)
            root    <- elementWithClass(uic.RadioButton().checked(true).render, "p-radiobutton")
            checked <- carriesClass(root, "p-radiobutton-checked")
        yield assert(r.checked.exists { case UI.Bound.Const(c) => c; case _ => false } && checked, s"shows the constant: ${r.checked}")
    }

end RadioButtonTest
