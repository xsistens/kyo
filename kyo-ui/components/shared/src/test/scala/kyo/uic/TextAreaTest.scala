package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: a ref is kyo-ui's own binding, and an edit its owner does
  * not take is painted away.
  */
class TextAreaTest extends ReactiveValueContract:

    private val valueSlot = nativeText[uic.TextArea](
        name = "value",
        bare = Present(uic.TextArea()),
        bind = uic.TextArea().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-textarea"
    )

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    heardBy(nativeText[uic.TextArea](
        name = "value heard by onChange",
        bare = Present(uic.TextArea()),
        bind = uic.TextArea().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-textarea",
        commit = true
    ))

    private def field(ui: UI)(using Frame): UI.Ast.Textarea < Sync =
        elements(ui).map(_.collectFirst { case t: UI.Ast.Textarea => t }.getOrElse(throw new AssertionError("no textarea")))

    // The browser half is held in kyo-ui (FilledClassTest, DomBackendFilledTest).
    "the field marks itself filled for the client, and the static render carries p-filled from a starting text" in {
        for
            started <- renderHtml(uic.TextArea().value("x").render)
            empty   <- renderHtml(uic.TextArea().value("").render)
            bare    <- renderHtml(uic.TextArea().render)
        yield
            assert(classInHtml(started, "p-filled") && started.contains("""data-kyo-filled="p-filled""""), started)
            Seq(empty, bare).foreach(h => assert(!classInHtml(h, "p-filled") && h.contains("""data-kyo-filled="p-filled""""), h))
        end for
    }

    "a value bound one way follows its owner" in {
        for
            held <- Signal.initRef("a")
            ui = uic.TextArea().value(held.readOnly).onInput(_ => ()).render
            _     <- held.set("xy")
            again <- field(ui).map(textIn)
        yield assert(again == "xy", s"follows the owner: $again")
    }

    "a ref stays kyo-ui's own two-way binding" in {
        for
            held <- Signal.initRef("a")
            f    <- field(uic.TextArea().value(held).render)
        yield assert(f.value.exists { case UI.Bound.Ref(r) => r eq held; case _ => false }, s"binds the ref: ${f.value}")
    }

    "a signal nobody hears says nothing where it reaches somebody or nobody can edit it: a ref, readonly, disabled" in {
        def errors(c: uic.TextArea): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.TextArea().value(held))
            ro     <- errors(uic.TextArea().value(held.readOnly).readonly(true))
            off    <- errors(uic.TextArea().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "an edit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("a")
            again <- repaintsAfter(uic.TextArea().value(held.readOnly).onInput(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end TextAreaTest
