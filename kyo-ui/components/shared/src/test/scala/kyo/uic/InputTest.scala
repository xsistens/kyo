package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: a ref is kyo-ui's own binding, and an edit its owner does
  * not take is painted away.
  */
class InputTest extends ReactiveValueContract:

    private val valueSlot = nativeText[uic.Input](
        name = "value",
        bare = Present(uic.Input()),
        bind = uic.Input().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-inputtext"
    )

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    heardBy(nativeText[uic.Input](
        name = "value heard by onChange",
        bare = Present(uic.Input()),
        bind = uic.Input().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-inputtext",
        commit = true
    ))

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst { case t: (UI.Ast.Element & UI.Ast.TextInput) => t }
            .getOrElse(throw new AssertionError("no text field")))

    // The client toggles p-filled from the text the field holds (kyo-ui's filledClass); the page as first rendered already
    // carries it where the starting text is not empty. The browser half is held in kyo-ui (FilledClassTest, DomBackendFilledTest).
    "the field marks itself filled for the client, and the static render carries p-filled from a starting text" in {
        for
            started <- renderHtml(uic.Input().value("x").render)
            empty   <- renderHtml(uic.Input().value("").render)
            bare    <- renderHtml(uic.Input().render)
        yield
            assert(classInHtml(started, "p-filled") && started.contains("""data-kyo-filled="p-filled""""), started)
            Seq(empty, bare).foreach(h => assert(!classInHtml(h, "p-filled") && h.contains("""data-kyo-filled="p-filled""""), h))
        end for
    }

    "a value bound one way follows its owner" in {
        for
            held <- Signal.initRef("a")
            ui = uic.Input().value(held.readOnly).onInput(_ => ()).render
            _     <- held.set("xy")
            again <- field(ui).map(textIn)
        yield assert(again == "xy", s"follows the owner: $again")
    }

    "a ref stays kyo-ui's own two-way binding" in {
        for
            held <- Signal.initRef("a")
            f    <- field(uic.Input().value(held).render)
        yield assert(f.value.exists { case UI.Bound.Ref(r) => r eq held; case _ => false }, s"binds the ref: ${f.value}")
    }

    "a signal nobody hears says nothing where it reaches somebody or nobody can edit it: a ref, readonly, disabled" in {
        def errors(c: uic.Input): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.Input().value(held))
            ro     <- errors(uic.Input().value(held.readOnly).readonly(true))
            off    <- errors(uic.Input().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "an edit its owner does not take is put back" in {
        for
            held    <- Signal.initRef("a")
            patches <-
                assertsAfter(uic.Input().value(held.readOnly).onInput(_ => ()).render)("p-inputtext")(kyo.internal.UIEvent.Input(_, "ab"))
        yield assert(patches == Chunk(("value", "a")), s"patches: $patches")
        end for
    }

end InputTest
