package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: a ref is kyo-ui's own binding, and an edit its owner does
  * not take is painted away.
  */
class TextAreaTest extends ReactiveValueContract:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private val valueSlot = nativeText[uic.TextArea](
        name = "value",
        bare = Present(uic.TextArea()),
        bind = uic.TextArea().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-textarea",
        // As an app places it, which keys an instance on the place.
        placed = Present(_.toUI)
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

    /** The field the live page shows last among what it published: a host's tree holds a kept instance only as its place, which a
      * static walk would draw anew at its start, and the newest field is the one a restarted instance would have drawn.
      */
    private def shownLast(published: UI)(using Frame): String < Sync =
        UicTest.walkingLive.let(true)(elements(published)).map(_.collect { case t: UI.Ast.Textarea => t }.lastOption
            .getOrElse(throw new AssertionError("no textarea"))).map(textIn)

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

    "a size, a variant, the width, autoResize and the rows from signals patch the field in place, with no region" in {
        for
            size   <- Signal.initRef(uic.Size.Normal: uic.Size)
            filled <- Signal.initRef(uic.FieldVariant.Outlined: uic.FieldVariant)
            fluid  <- Signal.initRef(false)
            grows  <- Signal.initRef(false)
            rows   <- Signal.initRef(2)
            ui = uic.TextArea().size(size).variant(filled).fluid(fluid).autoResize(grows).rows(rows).render
            el <- field(ui)
            names = Seq("p-textarea-lg", "p-variant-filled", "p-textarea-fluid", "p-textarea-resizable", "p-uic-autoresize")
            before <- Kyo.foreach(names)(carriesClass(el, _))
            first  <- renderHtml(ui)
            after  <- size.set(uic.Size.Large).andThen(filled.set(uic.FieldVariant.Filled)).andThen(fluid.set(true))
                .andThen(grows.set(true)).andThen(Kyo.foreach(names)(carriesClass(el, _)))
            fewest <- rows.set(0).andThen(renderHtml(ui))
            n      <- regionsAbove(ui, "p-textarea")
        yield assert(
            before == Seq(false, false, false, false, false) && after == Seq(true, true, true, true, true) && n == 0 &&
                first.contains("""data-rows="2"""") && first.contains("min-height: 44px") &&
                fewest.contains("""data-rows="1"""") && fewest.contains("min-height: 22px"),
            s"before $before, after $after, $n regions: $first | $fewest"
        )
    }

    "disabled, readonly and required from signals toggle the native attributes and aria-required in place, with no region" in {
        for
            off      <- Signal.initRef(false)
            locked   <- Signal.initRef(false)
            required <- Signal.initRef(false)
            ui = uic.TextArea().disabled(off).readonly(locked).required(required).render
            free <- renderHtml(ui)
            set  <- off.set(true).andThen(locked.set(true)).andThen(required.set(true)).andThen(renderHtml(ui))
            n    <- regionsAbove(ui, "p-textarea")
        yield
            val attrs = Seq(" disabled", " readonly", " required", """aria-required="true"""")
            assert(
                attrs.forall(a => !free.contains(a)) && !free.contains("aria-required") && attrs.forall(set.contains) && n == 0,
                s"$n regions: $free | $set"
            )
        end for
    }

    "a signal name or maxLength that changes reaches the field" in {
        for
            name <- Signal.initRef("a")
            cap  <- Signal.initRef(3)
            ui = uic.TextArea().name(name).maxLength(cap).render
            _ <- name.set("b")
            _ <- cap.set(-2)
            f <- field(ui)
        yield
            val shown = (f.attrs.jsProps.get("name"), f.attrs.jsProps.get("maxLength"))
            assert(shown == (Some("b"), Some("0")), s"name and maxLength after the change: $shown")
        end for
    }

    "the card for a signal nobody hears shows only while a signal flag lets the reader edit" in {
        def cards(ui: UI): Int < Sync = elementsWithClass(ui, "p-uic-diagnostic").map(_.size)
        for
            held   <- Signal.initRef("a")
            off    <- Signal.initRef(true)
            locked <- Signal.initRef(true)
            disabled = uic.TextArea().value(held.readOnly).disabled(off).render
            readonly = uic.TextArea().value(held.readOnly).readonly(locked).render
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
                UI.div(UI.span.cssClass(UicTest.hostRunClass)(s"run $n"), uic.TextArea().value("start").onInput(_ => Kyo.unit).toUI)
            ))
            (every, _, _) <- paintedAround(ui, t => elementsWithClass(t, "p-textarea").map(_.nonEmpty), ran)(
                t => field(t).map(f => typeInto(f, "typed")),
                _ => setSettled(run, 1)
            )
            shown <- shownLast(every)
        yield assert(shown == "typed", s"after the host's run the field shows $shown, the reader had typed typed")
        end for
    }

    "inside a FloatLabel or an IconField, from a constant, a host region that runs again keeps the reader's text" in {
        def ran(t: UI): Boolean < Sync = elementsWithClass(t, UicTest.hostRunClass).map(_.exists(e => textOf(e) == "run 1"))
        def kept(wrap: uic.TextArea => UI.MountSite ?=> UI): String < Async =
            for
                run <- Signal.initRef(0)
                ui = UI.div(run.map(n =>
                    UI.div(UI.span.cssClass(UicTest.hostRunClass)(s"run $n"), wrap(uic.TextArea().value("start").onInput(_ => Kyo.unit)))
                ))
                (every, _, _) <- paintedAround(ui, t => elementsWithClass(t, "p-textarea").map(_.nonEmpty), ran)(
                    t => field(t).map(f => typeInto(f, "typed")),
                    _ => setSettled(run, 1)
                )
                shown <- shownLast(every)
            yield shown
        for
            floated <- kept(t => uic.FloatLabel(t, "Notes").toUI)
            iconed  <- kept(t => uic.IconField(t).iconStart(uic.Icons.search).toUI)
        yield assert(floated == "typed" && iconed == "typed", s"after the host's run: FloatLabel $floated, IconField $iconed")
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
        def errors(c: uic.TextArea): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.TextArea().value(held))
            ro     <- errors(uic.TextArea().value(held.readOnly).readonly(true))
            off    <- errors(uic.TextArea().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "an edit its owner does not take is put back" in {
        for
            held    <- Signal.initRef("a")
            patches <-
                assertsAfter(uic.TextArea().value(held.readOnly).onInput(_ => ()).render)("p-textarea")(kyo.internal.UIEvent.Input(_, "ab"))
        yield assert(patches == Chunk(("value", "a")), s"patches: $patches")
        end for
    }

end TextAreaTest
