package kyo.uic

import kyo.*
import kyo.UI.render

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. Typing reaches `onInput` and a commit `onChange`, so the slot is held through
  * each. The cases after them are what only this field does: the strength meter judges the text it shows, and an edit
  * its owner does not take is painted away.
  */
class PasswordTest extends ReactiveValueContract:

    // Freed of every loss in its wave (T19): a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private val valueSlot = nativeText[uic.Password](
        name = "value",
        bare = Present(uic.Password()),
        bind = uic.Password().value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        placed = Present(_.toUI),
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
        placed = Present(_.toUI),
        field = "p-password-input",
        commit = true
    ))

    /** What a field with the strength meter shows: its text, which the meter must judge. */
    private def judged(t: UI)(using Frame): String < Sync =
        for
            text <- textField(t, "p-password-input").map(textIn)
            // The label is a text channel of the instance's latest run: read at the value it shows now.
            label <- elementWithClass(t, "p-password-meter-text").flatMap(textNow)
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
    private val feedbackSlot = new SlotCase[uic.Password, String](
        name = "value with feedback",
        bare = Present(uic.Password().feedback(true)),
        bind = uic.Password().feedback(true).value(_),
        hear = (c, f) => c.onInput(f),
        event = "onInput or onChange",
        render = _.render,
        placed = Present(_.toUI),
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
        // The typing is wired once the field writes a ref (the instance's own text, or the caller's) or tells a hearer.
        ready = Present(t =>
            textField(t, "p-password-input").map {
                case f: UI.Ast.TextInput =>
                    f.onInput.isDefined ||
                    (f.value match
                        case Present(UI.Bound.Ref(_)) => true
                        case _                        => false)
                case _ => false
            }
        ),
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
        placed = Present(_.toUI),
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
        def errors(c: uic.Password): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(uic.Password().value(held))
            ro     <- errors(uic.Password().value(held.readOnly).readonly(true))
            off    <- errors(uic.Password().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "an edit its owner does not take is put back" in {
        for
            held    <- Signal.initRef("a")
            patches <-
                assertsAfter(uic.Password().value(held.readOnly).onInput(_ => ()).render)("p-password-input")(kyo.internal.UIEvent.Input(
                    _,
                    "ab"
                ))
        yield assert(patches == Chunk(("value", "a")), s"patches: $patches")
        end for
    }

    /** Whether the wrapper of `t` says it is filled. */
    private def filled(t: UI)(using Frame): Boolean < Sync = elementWithClass(t, "p-password").map(carriesClass(_, "p-inputwrapper-filled"))

    // Without the meter a ref is bound to the field directly and nothing renders per keystroke, so the wrapper follows the
    // ref through a class channel, as the meter's render follows it by drawing again.
    // The client toggles the wrapper's class from the field's text (kyo-ui's filledClass with an ancestor target); the
    // wrapper's starting class is the field's own to stamp. The browser half is held in kyo-ui (FilledClassTest,
    // DomBackendFilledTest).
    "every path marks the field for the wrapper's filled class, and the page as first rendered stamps it from a starting text" in {
        def marked(h: String) = h.contains("""data-kyo-filled-up="p-inputwrapper-filled"""") &&
            h.contains("""data-kyo-filled-on=".p-password"""")
        for
            plain    <- renderHtml(uic.Password().value("x").render)
            plainNo  <- renderHtml(uic.Password().render)
            meter    <- renderHtml(uic.Password().feedback(true).value("x").render)
            meterNo  <- renderHtml(uic.Password().feedback(true).render)
            toggle   <- renderHtml(uic.Password().toggleMask(true).value("x").render)
            toggleNo <- renderHtml(uic.Password().toggleMask(true).render)
        yield
            Seq(plain, meter, toggle).foreach(h => assert(classInHtml(h, "p-inputwrapper-filled") && marked(h), h))
            Seq(plainNo, meterNo, toggleNo).foreach(h => assert(!classInHtml(h, "p-inputwrapper-filled") && marked(h), h))
        end for
    }

    "bound to a ref with the meter off, the wrapper is filled while the ref holds text" in {
        for
            held <- Signal.initRef("abc")
            ui = uic.Password().value(held).render
            first <- filled(ui)
            _     <- held.set("")
            empty <- filled(ui)
            _     <- held.set("x")
            again <- filled(ui)
        yield assert((first, empty, again) == (true, false, true), s"filled over abc, empty, x: ${(first, empty, again)}")
    }

    "bound to a ref with the mask toggle and the meter off, the wrapper is filled while the ref holds text" in {
        for
            held <- Signal.initRef("abc")
            t    <- readyTree(uic.Password().toggleMask(true).value(held).render)(elementsWithClass(
                _,
                "p-password-toggle-mask-icon"
            ).map(_.nonEmpty))
            first <- filled(t)
            _     <- held.set("")
            empty <- filled(t)
        yield assert((first, empty) == (true, false), s"filled over abc, then empty: ${(first, empty)}")
    }

    "the field sits under no region, with the eye, the meter or both, so a reveal or a keystroke draws it nowhere again" in {
        for
            eye   <- regionsAbove(uic.Password().toggleMask(true).render, "p-password-input")
            meter <- regionsAbove(uic.Password().feedback(true).render, "p-password-input")
            both  <- regionsAbove(uic.Password().toggleMask(true).feedback(true).render, "p-password-input")
            held  <- Signal.initRef("abc")
            bound <- regionsAbove(uic.Password().toggleMask(true).feedback(true).value(held).render, "p-password-input")
        yield assert(
            (eye, meter, both, bound) == (0, 0, 0, 0),
            s"regions above the field (eye, meter, both, bound): ${(eye, meter, both, bound)}"
        )
    }

    /** The field's `type` now: the channel's value where one owns it, else the type its element class writes. */
    private def typeNow(e: UI.Ast.Element)(using Frame): String < Sync =
        e.attrs.reactiveAttrs.get("type") match
            case Some(sig) => sig.current
            case None      =>
                e match
                    case _: UI.Ast.PasswordInput => Kyo.lift("password")
                    case _                       => Kyo.lift("text")

    "the eye switches the field's type in place, on the field built once, and says so on itself" in {
        // One traversal: the field and the eye of one instance, so the click writes the reveal this field follows.
        for
            es <- elements(uic.Password().toggleMask(true).render)
            el  = es.collectFirst { case t: UI.Ast.Element if t.attrs.cssClasses.contains("p-password-input") => t }.get
            eye = es.collectFirst { case t: UI.Ast.Element if t.attrs.cssClasses.contains("p-password-toggle-mask-icon") => t }.get
            before <- typeNow(el)
            _      <- click(eye)
            after  <- typeNow(el)
            masks  <- carriesClass(eye, "p-password-mask-icon")
            label  <- ariaNow(eye, "label")
        yield assert(
            (before, after, masks, label) == ("password", "text", true, Present("Hide password")),
            s"type before $before, after $after; the eye offers to mask $masks, says $label"
        )
        end for
    }

    "a host region that runs again keeps the text the reader typed" in {
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(uic.Password().feedback(true).placeholder(s"run $n"))))
            after <- publishedAfterReady(ui, _ => true, until = t => elementsWithClass(t, "p-password-input").map(_.nonEmpty))(
                t => textField(t, "p-password-input").map(typed(_, "abcDEF12", commit = false)),
                _ => setSettled(tick, 1)
            )
            shown = after.getOrElse(ui)
            strong <- elementsWithClass(shown, "p-password-meter-strong")
            text   <- textField(shown, "p-password-input").map(textIn)
        yield assert(strong.nonEmpty && text == "abcDEF12", s"after the host's run: strong meter ${strong.nonEmpty}, text '$text'")
        end for
    }

end PasswordTest
