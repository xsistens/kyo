package kyo.uic

import kyo.*
import kyo.UI.render

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field that keeps its text does: a `defaultValue` is the
  * starting text, and unbound or from a default the instance's own ref holds what the reader types, with the meter, the
  * eye, both or neither. Typing reaches `onInput` and a commit `onChange`, so the slot is held through each. The cases after
  * them are what only this field does: the strength meter judges the text it shows, and an edit its owner does not take is
  * painted away.
  */
class PasswordTest extends ReactiveValueContract:

    // Freed of every loss in its wave (T19): a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    /** Whether the field of `t` is wired for typing: it writes a ref (the instance's own text, or the caller's) or tells a
      * hearer of a keystroke or of a commit.
      */
    private def writes(t: UI)(using Frame): Boolean < Sync =
        textField(t, "p-password-input").map {
            case f: UI.Ast.TextInput =>
                f.onInput.isDefined || f.onChange.isDefined ||
                (f.value match
                    case Present(UI.Bound.Ref(_)) => true
                    case _                        => false)
            case _ => false
        }

    /** A case of the value slot on `made`: the field keeps the text itself, unbound and from a default, so the reader's text
      * is the instance's and a host region that runs again keeps it.
      */
    private def ownText(
        name: String,
        made: Frame ?=> uic.Password,
        hear: (uic.Password, String => UI.Action) => Frame ?=> uic.Password = (c, f) => c.onInput(f),
        shows: UI => Frame ?=> String < Sync = t => textField(t, "p-password-input").map(textIn),
        commit: Boolean = false,
        ready: UI => Frame ?=> Boolean < Sync = t => writes(t)
    ): SlotCase[uic.Password, String] =
        new SlotCase[uic.Password, String](
            name = name,
            bare = Present(made),
            bind = v => made.value(v),
            hear = hear,
            event = "onInput or onChange",
            render = _.render,
            placed = Present(_.toUI),
            start = "",
            next = "abcDEF12",
            control = textField(_, "p-password-input"),
            shows = shows,
            gesture = (c: UI.Ast.Element) => typed(c, "abcDEF12", commit),
            answers = editable,
            saysFixed = {
                case t: UI.Ast.TextInput => t.readOnly.contains(true)
                case _                   => false
            },
            ready = Present(ready),
            startText = Present("abc"),
            defaults = Present((c, t) => c.defaultValue(t)),
            constantFixed = true
        )

    private val valueSlot = ownText("value", uic.Password())

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    heardBy(ownText("value heard by onChange", uic.Password(), hear = (c, f) => c.onChange(f), commit = true))

    "unbound or from a default, without the meter or the eye, the field binds the instance's own text" in {
        def own(c: uic.Password): Boolean < Sync = field(c.render).map(_.value match
            case Present(UI.Bound.Ref(_)) => true
            case _                        => false)
        for
            bare  <- own(uic.Password())
            start <- own(uic.Password().defaultValue("abc"))
        yield assert((bare, start) == (true, true), s"the field binds a ref of its own (unbound, from a default): ${(bare, start)}")
        end for
    }

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

    // With the meter on, the meter judges the text the field keeps, unbound and from a default alike.
    private val feedbackSlot = ownText("value with feedback", uic.Password().feedback(true), shows = judged(_))

    holds(feedbackSlot)
    blurs(feedbackSlot, (c, f) => c.onBlur(f))

    // With the eye, the field keeps the text through a reveal, which switches only its type.
    private val toggleSlot = ownText(
        "value with the mask toggle",
        uic.Password().toggleMask(true),
        ready = t => writes(t).map(w => elementsWithClass(t, "p-password-toggle-mask-icon").map(eye => w && eye.nonEmpty))
    )

    holds(toggleSlot)
    blurs(toggleSlot, (c, f) => c.onBlur(f))

    "started from a default, the meter judges what the reader types" in {
        for
            after <- publishedAfterReady(
                uic.Password().feedback(true).defaultValue("abc").onInput(_ => ()).render,
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
                uic.Password().toggleMask(true).defaultValue("abc").render,
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

    "the accessible name reference and the description reach the field, a signal description patching it in place" in {
        for
            said <- Signal.initRef("first")
            f    <- field(uic.Password().toggleMask(true).accessibleNameRef("lead").accessibleDescription(said).render)
            _    <- said.set("second")
            ref  <- ariaNow(f, "labelledby")
            desc <- ariaNow(f, "description")
        yield assert((ref, desc) == (Present("lead"), Present("second")), s"aria-labelledby, aria-description: ${(ref, desc)}")
    }

    "a signal disabled, readonly, required, size, variant or fluid patches the field, the eye and the wrapper in place" in {
        def flag(e: UI.Ast.Element, name: String): Boolean < Sync =
            e.attrs.reactiveBoolAttrs.get(name).fold(Kyo.lift(false))(_.current)
        for
            on      <- Signal.initRef(false)
            size    <- Signal.initRef[Size](Size.Normal)
            variant <- Signal.initRef[FieldVariant](FieldVariant.Outlined)
            ui = uic.Password().toggleMask(true).disabled(on).readonly(on).required(on).size(size).variant(variant).fluid(on).render
            regions <- regionsAbove(ui, "p-password-input")
            es      <- elements(ui)
            f    = es.collectFirst { case t: UI.Ast.Element if t.attrs.cssClasses.contains("p-password-input") => t }.get
            eye  = es.collectFirst { case t: UI.Ast.Element if t.attrs.cssClasses.contains("p-password-toggle-mask-icon") => t }.get
            root = es.collectFirst { case t: UI.Ast.Element if t.attrs.cssClasses.contains("p-password") => t }.get
            _      <- on.set(true)
            _      <- size.set(Size.Small)
            _      <- variant.set(FieldVariant.Filled)
            off    <- flag(f, "disabled")
            ro     <- flag(f, "readonly")
            req    <- flag(f, "required")
            small  <- carriesClass(f, "p-inputtext-sm")
            filled <- carriesClass(f, "p-variant-filled")
            fluid  <- carriesClass(root, "p-password-fluid")
            quiet  <- silentNow(eye)
        yield assert(
            (regions, off, ro, req, small, filled, fluid, quiet) == (0, true, true, true, true, true, true, true),
            s"regions above the field, then disabled, readonly, required, small, filled, fluid, a silent eye: " +
                s"${(regions, off, ro, req, small, filled, fluid, quiet)}"
        )
        end for
    }

    /** The field's `type` now: the channel's value where one owns it, else the `type` attribute the field renders. */
    private def typeNow(e: UI.Ast.Element)(using Frame): String < Sync =
        e.attrs.reactiveAttrs.get("type") match
            case Some(sig) => sig.current
            case None      =>
                kyo.internal.HtmlRenderer.render(e, Seq.empty).map(html =>
                    """ type="([^"]*)"""".r.findFirstMatchIn(html).map(_.group(1))
                        .getOrElse(throw new AssertionError(s"the field renders no type: $html"))
                )

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
