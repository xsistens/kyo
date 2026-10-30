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

    "a selection its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(false)
            again <- repaintsAfter(uic.RadioButton().checked(held.readOnly).onChange(_ => ()).render, held)(t => radio(t).map(select))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a constant with a listener paints itself again after a report, so a selection the owner does not take is put back" in {
        for again <- repaintsAfter(uic.RadioButton().checked(false).onChange(_ => ()).render)(t => radio(t).map(select))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a fixed constant of true is drawn selected" in {
        for
            r       <- radio(uic.RadioButton().checked(true).render)
            root    <- elementWithClass(uic.RadioButton().checked(true).render, "p-radiobutton")
            checked <- carriesClass(root, "p-radiobutton-checked")
        yield assert(r.checked.exists { case UI.Bound.Const(c) => c; case _ => false } && checked, s"shows the constant: ${r.checked}")
    }

end RadioButtonTest
