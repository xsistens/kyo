package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: a ref is kyo-ui's own binding, and an edit its owner does
  * not take is painted away.
  */
class InputTest extends ReactiveValueContract:

    holds(nativeText[uic.Input](
        name = "value",
        bare = Present(uic.Input()),
        bind = uic.Input().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-inputtext"
    ))

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

    "an edit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("a")
            again <- repaintsAfter(uic.Input().value(held.readOnly).onInput(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end InputTest
