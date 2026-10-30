package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: the strength meter judges the text it shows, and an edit
  * its owner does not take is painted away.
  */
class PasswordTest extends ReactiveValueContract:

    private val valueSlot = nativeText[uic.Password](
        name = "value",
        bare = Present(uic.Password()),
        bind = uic.Password().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-password-input"
    )

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    heardBy(nativeText[uic.Password](
        name = "value heard by onChange",
        bare = Present(uic.Password()),
        bind = uic.Password().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-password-input",
        commit = true
    ))

    /** What a field with the strength meter shows: its text, which the meter must judge. */
    private def judged(t: UI)(using Frame): String < Sync =
        for
            text  <- textField(t, "p-password-input").map(textIn)
            label <- elementWithClass(t, "p-password-meter-text").map(textOf)
        yield
            val expected = Password.strength(text) match
                case 0 => "Enter a password"
                case 1 => "Weak"
                case 2 => "Medium"
                case _ => "Strong"
            if label != expected then throw new AssertionError(s"the meter says $label over the text '$text'")
            text

    // With the meter on, the field draws the meter from its text, so the text is the field's to keep: unbound or from a
    // constant it keeps the text itself, and the meter judges what the reader typed.
    private val feedbackSlot = new Slot[uic.Password, String](
        name = "value with feedback",
        bare = Present(uic.Password().feedback(true)),
        bind = uic.Password().feedback(true).value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        start = "",
        next = "abcDEF12",
        control = textField(_, "p-password-input"),
        shows = judged(_),
        gesture = (c: UI.Ast.Element) => typed(c, "abcDEF12", commit = false),
        answers = editable,
        saysFixed = {
            case t: UI.Ast.TextInput => t.readOnly.contains(true)
            case _                   => false
        },
        // The page as first rendered draws a text the field keeps itself with nothing wired, the mount wires the typing.
        ready = Present(t => textField(t, "p-password-input").map { case f: UI.Ast.TextInput => f.onInput.isDefined; case _ => false }),
        startText = Present("abc")
    )

    holds(feedbackSlot)
    blurs(feedbackSlot, (c, f) => c.onBlur(f))

    // The eye's reveal draws the field again, so a constant drawn as the field's value would be put back over what the
    // reader typed; unbound, the field draws no value and the text stays the browser's.
    private val toggleSlot = nativeText[uic.Password](
        name = "value with the mask toggle",
        bare = Present(uic.Password().toggleMask(true)),
        bind = uic.Password().toggleMask(true).value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        field = "p-password-input",
        ready = Present(elementsWithClass(_, "p-password-toggle-mask-icon").map(_.nonEmpty))
    )

    holds(toggleSlot)
    blurs(toggleSlot, (c, f) => c.onBlur(f))

    "started from a constant, the meter judges what the reader types" in {
        for
            after <- publishedAfterReady(
                uic.Password().feedback(true).value("abc").onInput(_ => ()).render,
                textField(_, "p-password-input").map { case f: UI.Ast.TextInput => f.onInput.isDefined; case _ => false },
                until = t => elementsWithClass(t, "p-password-meter-strong").map(_.nonEmpty)
            )(t => textField(t, "p-password-input").map(typed(_, "abcDEF12", commit = false)))
        yield assert(after.nonEmpty, "the meter never judged the text typed")
    }

    // An unfocused field is given its value attribute on every render (`__kyoMorphAttrs` in kyo-ui's client), and the
    // reader has just clicked the eye.
    "a starting text the reader typed over survives a reveal" in {
        for
            after <- publishedAfterReady(
                uic.Password().toggleMask(true).value("abc").render,
                elementsWithClass(_, "p-password-toggle-mask-icon").map(_.nonEmpty),
                until = t => textField(t, "p-password-input").map(textIn).map(_ == "xyz")
            )(
                t => textField(t, "p-password-input").map(typed(_, "xyz", commit = false)),
                t => elementWithClass(t, "p-password-toggle-mask-icon").map(click)
            )
        yield assert(after.nonEmpty, "the reveal drew the starting text over what the reader typed")
    }

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst {
            case t: (UI.Ast.Element & UI.Ast.TextInput) if t.attrs.cssClasses.contains("p-password-input") => t
        }.getOrElse(throw new AssertionError("no password field")))

    "a value bound one way is shown with its strength, and follows its owner" in {
        for
            held <- Signal.initRef("abcDEF12")
            ui = uic.Password().feedback(true).value(held.readOnly).onInput(_ => ()).render
            first <- field(ui).map(textIn)
            meter <- elementsWithClass(ui, "p-password-meter-strong")
            _     <- held.set("xy")
            again <- field(ui).map(textIn)
        yield
            assert(first == "abcDEF12" && again == "xy", s"shows $first, then $again")
            assert(meter.size == 1, "and the meter judges what it shows")
    }

    "a signal nobody hears says nothing where it reaches somebody or nobody can edit it: a ref, readonly, disabled" in {
        def errors(c: uic.Password): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.Password().value(held))
            ro     <- errors(uic.Password().value(held.readOnly).readonly(true))
            off    <- errors(uic.Password().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "an edit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("a")
            again <- repaintsAfter(uic.Password().value(held.readOnly).onInput(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end PasswordTest
