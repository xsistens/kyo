package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: a ref is kyo-ui's own binding, and an edit its owner does
  * not take is painted away.
  */
class InputTest extends ReactiveValueContract:

    // Freed of every loss in its wave (T19): a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private val valueSlot = nativeText[uic.Input](
        name = "value",
        bare = Present(uic.Input()),
        bind = uic.Input().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-inputtext",
        // As an app places it, which keys an instance on the place.
        placed = Present(_.toUI)
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

    "a type from a signal patches the field's type between free-text types, and across to Number draws a number field" in {
        def typeChannel(e: UI.Ast.Element): Maybe[String] < Sync =
            e.attrs.reactiveAttrs.get("type") match
                case Some(sig) => sig.current.map(Present(_))
                case None      => Absent
        for
            kind <- Signal.initRef(uic.InputType.Text)
            ui = uic.Input().inputType(kind).render
            _      <- kind.set(uic.InputType.Email)
            email  <- field(ui)
            shown  <- typeChannel(email)
            _      <- kind.set(uic.InputType.Number)
            number <- field(ui)
        yield
            assert(shown == Present("email"), s"between free-text types the type is a channel of the field: $shown")
            assert(number.isInstanceOf[UI.Ast.NumberInput], s"across to Number the field is a number field: $number")
        end for
    }

    "a signal name, maxLength, inputFilter or inputMask that changes reaches the field" in {
        for
            name   <- Signal.initRef("a")
            cap    <- Signal.initRef(3)
            filter <- Signal.initRef("decimal")
            mask   <- Signal.initRef("99")
            ui = uic.Input().name(name).maxLength(cap).inputFilter(filter).inputMask(mask).render
            _ <- name.set("b")
            _ <- cap.set(5)
            _ <- filter.set("int")
            _ <- mask.set("999")
            f <- field(ui)
        yield
            val shown = f match
                case c: UI.Ast.ConstrainedInput =>
                    (f.attrs.jsProps.get("name"), f.attrs.jsProps.get("maxLength"), c.inputFilter, c.inputMask)
                case other => throw new AssertionError(s"not a free-text field: $other")
            assert(
                shown == (Some("b"), Some("5"), Present(UI.InputFilter.Digits), Present("999")),
                s"name, maxLength, filter and mask after the change: $shown"
            )
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
        def errors(c: uic.Input): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.Input().value(held))
            ro     <- errors(uic.Input().value(held.readOnly).readonly(true))
            off    <- errors(uic.Input().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "the card for a signal nobody hears shows only while a signal flag lets the reader edit" in {
        def cards(ui: UI): Int < Sync = elementsWithClass(ui, "p-uic-diagnostic").map(_.size)
        for
            held   <- Signal.initRef("a")
            off    <- Signal.initRef(true)
            locked <- Signal.initRef(true)
            disabled = uic.Input().value(held.readOnly).disabled(off).render
            readonly = uic.Input().value(held.readOnly).readonly(locked).render
            whileOff <- cards(disabled)
            whileRo  <- cards(readonly)
            _        <- off.set(false)
            _        <- locked.set(false)
            onAgain  <- cards(disabled)
            editable <- cards(readonly)
        yield assert(
            (whileOff, whileRo, onAgain, editable) == (0, 0, 1, 1),
            s"cards while disabled $whileOff, while readonly $whileRo, then $onAgain and $editable"
        )
        end for
    }

    "from a constant, a host region that runs again keeps the reader's text" in {
        def ran(t: UI): Boolean < Sync = elementsWithClass(t, UicTest.hostRunClass).map(_.exists(e => textOf(e) == "run 1"))
        for
            run <- Signal.initRef(0)
            ui = UI.div(run.map(n =>
                UI.div(UI.span.cssClass(UicTest.hostRunClass)(s"run $n"), uic.Input().value("start").onInput(_ => Kyo.unit).toUI)
            ))
            (every, _, _) <- paintedAround(ui, t => elementsWithClass(t, "p-inputtext").map(_.nonEmpty), ran)(
                t => field(t).map(f => typeInto(f, "typed")),
                _ => setSettled(run, 1)
            )
            shown <- field(every).map(textIn)
        yield assert(shown == "typed", s"after the host's run the field shows $shown, the reader had typed typed")
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
