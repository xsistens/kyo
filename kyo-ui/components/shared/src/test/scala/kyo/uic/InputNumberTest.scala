package kyo.uic

import kyo.*

/** The field's two slots, one storage, both held to rule A ([[ReactiveValueContract]]), each through the native field and
  * through the spin buttons. `value` is a number: a constant nobody hears is fixed (native `readonly`, the spin buttons
  * off), and it has no unbound form of its own, since a field nobody binds keeps text. `text` is what the reader typed: its
  * constant is the starting text, and unbound or from a constant the field keeps the text itself. The cases after them are
  * what only this field does: a number its owner does not take is painted away again, the wrapper's filled class, and
  * what a blur reports.
  */
class InputNumberTest extends ReactiveValueContract:

    private def number(ui: UI)(using Frame): UI.Ast.NumberInput < Sync =
        elements(ui).map(_.collectFirst { case n: UI.Ast.NumberInput => n }.getOrElse(throw new AssertionError("no number field")))

    /** The number the field shows, drawn from the bound number. */
    private def shownNumber(t: UI)(using Frame): Double < Sync =
        def parsed(v: String): Double = v.toDoubleOption.getOrElse(throw new AssertionError(s"the field shows no number: $v"))
        number(t).map(_.value match
            case Present(UI.Bound.Const(v))    => Kyo.lift(parsed(v))
            case Present(UI.Bound.Follow(sig)) => sig.current.map(parsed)
            case other                         => throw new AssertionError(s"the field is bound as $other"))
    end shownNumber

    private def increment(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-inputnumber-increment-button")

    private def readonlyField(e: UI.Ast.Element): Boolean = e match
        case n: UI.Ast.NumberInput => n.readOnly.contains(true)
        case _                     => false

    /** The field takes typing: the mount of a field that keeps its text has wired it (the page as first rendered has not). */
    private def typing(t: UI)(using Frame): Boolean < Sync = number(t).map(_.onInput.isDefined)

    holds(new SlotCase[uic.InputNumber, Double](
        name = "value",
        bare = Absent,
        bind = uic.InputNumber().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 7.0,
        control = number(_),
        shows = shownNumber(_),
        // The field commits the number the reader typed, delivered as a number.
        gesture = {
            case n: UI.Ast.NumberInput => handled(n.onChangeNumeric.getOrElse(throw new AssertionError("no change handler"))(7.0))
            case other                 => throw new AssertionError(s"not a number field: $other")
        },
        answers = {
            case n: UI.Ast.NumberInput => n.onChangeNumeric.nonEmpty
            case _                     => false
        },
        saysFixed = readonlyField
    ))

    holds(new SlotCase[uic.InputNumber, Double](
        name = "value spun",
        bare = Absent,
        bind = uic.InputNumber().showButtons(true).value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 1.0,
        control = increment(_),
        shows = shownNumber(_),
        fixedOn = Present(number(_)),
        saysFixed = readonlyField
    ))

    holds(new SlotCase[uic.InputNumber, String](
        name = "text",
        bare = Present(uic.InputNumber()),
        bind = uic.InputNumber().text(_),
        hear = (c, f) => c.onTextChange(f),
        event = "onTextChange",
        render = _.render,
        start = "",
        // A text no number can hold, which is what the text slot is for: it travels as typed.
        next = "-",
        control = number(_),
        shows = t => number(t).map(textIn),
        gesture = (c: UI.Ast.Element) => typed(c, "-", commit = false),
        answers = editable,
        saysFixed = readonlyField,
        ready = Present(typing(_)),
        startText = Present("4.")
    ))

    holds(new SlotCase[uic.InputNumber, String](
        name = "text spun",
        bare = Present(uic.InputNumber().showButtons(true)),
        bind = uic.InputNumber().showButtons(true).text(_),
        hear = (c, f) => c.onTextChange(f),
        event = "onTextChange",
        render = _.render,
        start = "",
        next = "1",
        control = increment(_),
        shows = t => number(t).map(textIn),
        // No `ready`: the spin buttons are wired on the page as first rendered, and a field bound to a text ref has no
        // mount, only a region around the field itself, which holds no button to act on.
        fixedOn = Present(number(_)),
        saysFixed = readonlyField,
        startText = Present("4.")
    ))

    "a number its owner does not take is put back" in {
        for
            held    <- Signal.initRef(3.0)
            patches <- assertsAfter(
                uic.InputNumber().value(held.readOnly).onChange(_ => ()).render
            )("p-inputnumber-input")(kyo.internal.UIEvent.ChangeNumeric(_, 7.0))
        yield assert(patches == Chunk(("value", "3")), s"patches: $patches")
        end for
    }

    "a constant with a listener puts back a number the owner does not take" in {
        for
            first   <- shownNumber(uic.InputNumber().value(3.0).onChange(_ => ()).render)
            patches <- assertsAfter(
                uic.InputNumber().value(3.0).onChange(_ => ()).render
            )("p-inputnumber-input")(kyo.internal.UIEvent.ChangeNumeric(_, 7.0))
        yield
            assert(first == 3.0, s"the page as first rendered shows $first for the constant 3")
            assert(patches == Chunk(("value", "3")), s"patches: $patches")
        end for
    }

    "a value bound one way says nothing where it reaches somebody or nobody can change it: a ref, disabled" in {
        def errors(c: uic.InputNumber): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef(3.0)
            twoWay <- errors(uic.InputNumber().value(held))
            off    <- errors(uic.InputNumber().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can change it")
        end for
    }

    "a constant text is still the text shown once the mount keeps it" in {
        for
            t    <- mountedWhen(uic.InputNumber().text("4.").render)(typing)
            n    <- number(t)
            text <- textIn(n)
            card <- elementsWithClass(uic.InputNumber().text("4.").render, "p-uic-diagnostic")
        yield
            assert(text == "4.", s"the field starts from $text")
            assert(n.readOnly.isEmpty, "and the reader goes on typing from it")
            assert(card.isEmpty, s"a starting text is not a mistake: ${card.map(textOf)}")
    }

    // The client toggles the wrapper's class from the text the number field holds (kyo-ui's filledClass with an ancestor
    // target), so a field bound to a constant the reader empties, or one keeping text, is not left with a stale class. The
    // wrapper's starting class comes from the starting text. The browser half is held in kyo-ui.
    "every path marks the field for the wrapper's filled class, and the page as first rendered stamps it from a starting text" in {
        def marked(h: String) = h.contains("""data-kyo-filled-up="p-inputwrapper-filled"""") &&
            h.contains("""data-kyo-filled-on=".p-inputnumber"""")
        for
            held    <- Signal.initRef("4.5")
            number  <- renderHtml(uic.InputNumber().value(5.0).onChange(_ => ()).render)
            text    <- renderHtml(uic.InputNumber().text("4.").render)
            textRef <- renderHtml(uic.InputNumber().text(held).render)
            bare    <- renderHtml(uic.InputNumber().render)
            textNo  <- renderHtml(uic.InputNumber().text("").render)
        yield
            Seq(number, text, textRef).foreach(h => assert(classInHtml(h, "p-inputwrapper-filled") && marked(h), h))
            Seq(bare, textNo).foreach(h => assert(!classInHtml(h, "p-inputwrapper-filled") && marked(h), h))
        end for
    }

    // The field keeps what the reader types as text (unbound, or bound to a text), so the number a blur reports is the
    // text the field holds as focus leaves it, read the way a spin reads it.
    "unbound, a blur reports the number the reader typed" in {
        for
            seen <- Signal.initRef(Chunk.empty[Double])
            t    <- mountedWhen(uic.InputNumber().onBlur(v => seen.updateAndGet(_.append(v)).unit).render)(t =>
                number(t).map(_.value.isDefined)
            )
            n   <- number(t)
            _   <- typeInto(n, "5")
            _   <- blurOut(n, "5")
            got <- seen.get
        yield assert(got == Chunk(5.0), s"reported $got")
    }

    "bound to a text, a blur reports the number the text holds, and a text that is no number as zero" in {
        for
            held <- Signal.initRef("")
            seen <- Signal.initRef(Chunk.empty[Double])
            n    <- number(uic.InputNumber().text(held).onBlur(v => seen.updateAndGet(_.append(v)).unit).render)
            _    <- typeInto(n, "4.5")
            _    <- blurOut(n, "4.5")
            _    <- typeInto(n, "-")
            _    <- blurOut(n, "-")
            got  <- seen.get
        yield assert(got == Chunk(4.5, 0.0), s"reported $got")
    }

    // A number field commits on change, clamped into its bounds, so what it holds as focus leaves is the committed
    // number, not the text the reader left in the field.
    "bound to a number, a blur reports the number committed, clamped, not the text typed" in {
        for
            held <- Signal.initRef(3.0)
            seen <- Signal.initRef(Chunk.empty[Double])
            ui = uic.InputNumber().value(held).max(10).onBlur(v => seen.updateAndGet(_.append(v)).unit).render
            n   <- number(ui)
            _   <- handled(n.onChangeNumeric.getOrElse(throw new AssertionError("no change handler"))(15.0))
            n2  <- number(ui)
            _   <- blurOut(n2, "15")
            got <- seen.get
        yield assert(got == Chunk(10.0), s"reported $got")
    }

    // With no ref a commit writes nowhere, so the field alone holds what the reader typed: the blur reports that, read
    // off the field the way a spin reads a text and clamped the way a commit is.
    "from a constant somebody hears, a blur reports the number the reader typed, not the constant" in {
        for
            seen <- Signal.initRef(Chunk.empty[Double])
            ui = uic.InputNumber().value(5.0).onChange(_ => ()).onBlur(v => seen.updateAndGet(_.append(v)).unit).render
            n   <- number(ui)
            _   <- handled(n.onChangeNumeric.getOrElse(throw new AssertionError("no change handler"))(7.0))
            n2  <- number(ui)
            _   <- blurOut(n2, "7")
            got <- seen.get
        yield assert(got == Chunk(7.0), s"reported $got")
    }

    "bound one way, a blur before the repaint reports the number the reader typed, not the owner's" in {
        for
            held <- Signal.initRef(5.0)
            seen <- Signal.initRef(Chunk.empty[Double])
            ui = uic.InputNumber().value(held.readOnly).onChange(_ => ()).onBlur(v => seen.updateAndGet(_.append(v)).unit).render
            n   <- number(ui)
            _   <- blurOut(n, "7")
            got <- seen.get
        yield assert(got == Chunk(7.0), s"reported $got")
    }

    "with no ref, a blur reports the typed number clamped into the bounds, and a field holding no number as zero, clamped" in {
        for
            seen <- Signal.initRef(Chunk.empty[Double])
            ui = uic.InputNumber().value(5.0).min(1).max(10).onChange(_ => ()).onBlur(v => seen.updateAndGet(_.append(v)).unit).render
            n   <- number(ui)
            _   <- blurOut(n, "15")
            _   <- blurOut(n, "")
            got <- seen.get
        yield assert(got == Chunk(10.0, 1.0), s"reported $got")
    }

end InputNumberTest
