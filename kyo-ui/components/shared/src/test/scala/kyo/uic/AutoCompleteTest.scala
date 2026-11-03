package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.AutoComplete`, which lives on the FIELD rather than on the panel: the
  * panel never takes focus, so every key arrives at the text box.
  *
  * That is what limits it. The field is a text box, so Home, End, Space and every printable key
  * belong to the caret, and [[ListNav]] is read for the two vertical arrows alone. The gain over
  * the hand-written loop it replaces is that ArrowUp from nothing lands on the LAST suggestion,
  * the way it does everywhere else in the library, instead of on the first.
  */
class AutoCompleteTest extends ReactiveValueContract:

    private val cities = List("Amsterdam", "Berlin", "Cairo")

    private def field(using Frame) =
        uic.AutoComplete[String]().options(cities)(identity).optionKey(identity).id("ac")

    private def state(hiAt: Int, open: Boolean = true)(using
        Frame
    ): (SignalRef[Int], SignalRef[String], SignalRef[Boolean], UI) < Async =
        for
            openRef <- Signal.initRef(open)
            hi      <- Signal.initRef(hiAt)
            all     <- Signal.initRef(true)
            text    <- Signal.initRef("")
        yield (hi, text, openRef, field.value(text).wired(openRef, hi, all, Present("ac")))

    private def after(from: Int, key: UI.Keyboard)(using Frame): Int < Async =
        for
            (hi, _, _, ui) <- state(from)
            input          <- elementWithClass(ui, "p-autocomplete-input")
            _              <- press(input, key)
            at             <- hi.get
        yield at

    "ArrowDown walks the suggestions" in after(0, UI.Keyboard.ArrowDown).map(at => assert(at == 1))

    "ArrowUp from nothing lands on the last, which the hand-written loop got wrong" in
        after(-1, UI.Keyboard.ArrowUp).map(at => assert(at == 2))

    "the wired field is ONE reactive region over everything it reads, open or closed" in {
        for
            (_, _, open, ui) <- state(hiAt = 0)
            whenOpen         <- regionsAbove(ui, "p-autocomplete")
            panel            <- regionsAbove(ui, "p-autocomplete-list")
            _                <- open.set(false)
            whenClosed       <- regionsAbove(ui, "p-autocomplete")
        yield assert(
            whenOpen == 1 && panel == 1 && whenClosed == 1,
            s"a pick writes the text and closes the panel; a nested region can repaint the older state: $whenOpen / $panel / $whenClosed"
        )
    }

    "suggestions from a signal follow it after the reader has typed" in {
        // The order the demo's reader takes: type first, the list changes after.
        for
            live    <- Signal.initRef(Seq("Berlin", "Hamburg"))
            openRef <- Signal.initRef(true)
            hi      <- Signal.initRef(-1)
            all     <- Signal.initRef(false)
            text    <- Signal.initRef("B")
            ui = uic.AutoComplete[String]().options(live).value(text).wired(openRef, hi, all, Present("ac"))
            before <- elementsWithClass(ui, "p-autocomplete-option")
            _      <- live.set(Seq("Berlin", "Hamburg", "Bern", "Bremen"))
            after  <- elementsWithClass(ui, "p-autocomplete-option")
        yield assert(before.size == 1 && after.size == 3, s"${before.size} then ${after.size}")
        end for
    }

    "the ends hold" in {
        for
            top    <- after(0, UI.Keyboard.ArrowUp)
            bottom <- after(2, UI.Keyboard.ArrowDown)
        yield assert(top == 0 && bottom == 2)
    }

    "Home and End belong to the caret, not to the list" in {
        for
            home <- after(1, UI.Keyboard.Home)
            end  <- after(1, UI.Keyboard.End)
        yield assert(home == 1 && end == 1)
    }

    "Space belongs to the caret too, since the reader is typing into the field" in
        after(1, UI.Keyboard.Space).map(at => assert(at == 1))

    "a printable key types rather than jumping, for the same reason" in
        after(1, UI.Keyboard.Char('c')).map(at => assert(at == 1))

    "ArrowDown on a closed panel opens it ON an option, so the next key can pick" in {
        for
            (hi, _, openRef, ui) <- state(-1, open = false)
            input                <- elementWithClass(ui, "p-autocomplete-input")
            _                    <- press(input, UI.Keyboard.ArrowDown)
            opened               <- openRef.get
            at                   <- hi.get
        yield assert(opened && at == 0)
    }

    "Enter picks the highlighted suggestion" in {
        for
            (_, text, _, ui) <- state(2)
            input            <- elementWithClass(ui, "p-autocomplete-input")
            _                <- press(input, UI.Keyboard.Enter)
            got              <- text.get
        yield assert(got == "Cairo")
    }

    /** The announcement rides the INPUT, not the list.
      *
      * `aria-activedescendant` is read off whatever has DOM focus, and this panel is
      * `seedFocus(false)` precisely so focus never leaves the text box. On the list the attribute
      * named the right option and was hung off an element nothing was focused on.
      */
    "the field announces the highlighted suggestion, since the field is what holds focus" in {
        for
            (_, _, _, ui) <- state(1)
            input         <- elementWithClass(ui, "p-autocomplete-input")
            list          <- elementWithClass(ui, "p-autocomplete-list")
            rows          <- elementsWithClass(ui, "p-autocomplete-option")
        yield
            assert(input.attrs.role.contains("combobox"), "a text box that opens a list is a combobox")
            assert(input.attrs.ariaAttrs.get("activedescendant").contains("ac-option-1"))
            assert(input.attrs.ariaAttrs.get("controls").contains("ac-list"), "and it names the list it opens")
            assert(list.attrs.identifier.contains("ac-list"))
            assert(!list.attrs.ariaAttrs.contains("activedescendant"), "the unfocused list says nothing")
            assert(rows(1).attrs.identifier.contains("ac-option-1"))
    }

    "nothing highlighted announces nothing" in {
        for
            (_, _, _, ui) <- state(-1)
            input         <- elementWithClass(ui, "p-autocomplete-input")
        yield assert(!input.attrs.ariaAttrs.contains("activedescendant"))
    }

    /** The dropdown trigger is an affordance OF the combobox, not a widget beside it.
      *
      * It is its own tab stop and it opens the panel, but the keyboard of this component lives on
      * the input. A reader who opened the list from the button was left standing on the button:
      * no highlight to act on, no arrows, and no Escape, because none of those keys reach the
      * input from there.
      */
    private def withDropdown(selected: String)(using
        Frame
    )
        : (SignalRef[Int], SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        for
            openRef <- Signal.initRef(false)
            hi      <- Signal.initRef(-1)
            all     <- Signal.initRef(false)
            text    <- Signal.initRef(selected)
            moved   <- Signal.initRef(List.empty[String])
            ui = uic.AutoComplete[String]().options(cities)(identity).optionKey(identity).id("ac")
                .dropdown(true)
                .value(text)
                .wired(openRef, hi, all, Present("ac"), id => moved.updateAndGet(_ :+ id))
        yield (hi, openRef, moved, ui)

    "the dropdown trigger hands focus to the field, which is where the keyboard is" in {
        for
            (_, openRef, moved, ui) <- withDropdown("")
            dd                      <- elementWithClass(ui, "p-autocomplete-dropdown")
            _                       <- click(dd)
            opened                  <- openRef.get
            got                     <- moved.get
        yield assert(opened && got == List("ac"), "the field answers to the base id")
    }

    "and it opens on the first option when nothing is selected" in {
        for
            (hi, _, _, ui) <- withDropdown("")
            dd             <- elementWithClass(ui, "p-autocomplete-dropdown")
            _              <- click(dd)
            at             <- hi.get
        yield assert(at == 0)
    }

    "or on the selected one when there is one" in {
        for
            (hi, _, _, ui) <- withDropdown("Cairo")
            dd             <- elementWithClass(ui, "p-autocomplete-dropdown")
            _              <- click(dd)
            at             <- hi.get
        yield assert(at == 2)
    }

    "and once focus is there, Escape closes what the trigger opened" in {
        for
            openRef <- Signal.initRef(true)
            hi      <- Signal.initRef(1)
            all     <- Signal.initRef(true)
            text    <- Signal.initRef("")
            ui = field.value(text).wired(openRef, hi, all, Present("ac"))
            input <- elementWithClass(ui, "p-autocomplete-input")
            _     <- press(input, UI.Keyboard.Escape)
            still <- openRef.get
        yield assert(!still)
    }

    "and so does Tab, in both directions: the panel's keyboard is the field's" in {
        for
            (_, _, fwdOpen, fwdUi)   <- state(1)
            fwdInput                 <- elementWithClass(fwdUi, "p-autocomplete-input")
            _                        <- press(fwdInput, UI.Keyboard.Tab)
            fwd                      <- fwdOpen.get
            (_, _, backOpen, backUi) <- state(1)
            backInput                <- elementWithClass(backUi, "p-autocomplete-input")
            _                        <- press(backInput, UI.Keyboard.Tab, UI.Modifiers(shift = true))
            back                     <- backOpen.get
        yield assert(!fwd && !back, "a panel the reader has tabbed away from is one nothing answers")
    }

    "the field carries the base id, so the trigger has something to hand focus to" in {
        for
            (_, _, _, ui) <- withDropdown("")
            input         <- elementWithClass(ui, "p-autocomplete-input")
        yield assert(input.attrs.identifier.contains("ac"))
    }

    // ---- filter ----

    private val users = List(
        AutoCompleteTest.User("Ada Lovelace", "ada@engine.org"),
        AutoCompleteTest.User("Grace Hopper", "grace@navy.mil"),
        AutoCompleteTest.User("Alan Turing", "alan@bletchley.uk")
    )

    /** The suggestions `base` shows for the typed `text`, with the panel open on the filtered list. */
    private def suggestions(base: uic.AutoComplete[AutoCompleteTest.User], text: String)(using Frame): Chunk[String] < Async =
        for
            openRef <- Signal.initRef(true)
            hi      <- Signal.initRef(-1)
            all     <- Signal.initRef(false)
            typed   <- Signal.initRef(text)
            ui = base.value(typed).wired(openRef, hi, all, Present("ac"))
            rows <- elementsWithClass(ui, "p-autocomplete-option")
        yield rows.map(_.attrs.dataAttrs.getOrElse("uic-option-key", "-"))

    private def people(using Frame) = uic.AutoComplete[AutoCompleteTest.User]().options(users)(_.name)

    "a filter searches every text it is given, so a user is found by email" in
        suggestions(people.filter(uic.Filter.contains(_.name, _.email)), "NAVY").map(s =>
            assert(s == Chunk("Grace Hopper"), s"the email is searched too: $s")
        )

    "without a filter the start of any word of the label matches, and nothing else does" in {
        for
            word   <- suggestions(people, "HOP")
            inside <- suggestions(people, "opper")
            email  <- suggestions(people, "grace@")
        yield
            assert(word == Chunk("Grace Hopper"), s"the start of a later word: $word")
            assert(inside.isEmpty, s"not the inside of a word: $inside")
            assert(email.isEmpty, s"and the label alone: $email")
    }

    "startsWith matches only the start of the whole text" in {
        for
            start <- suggestions(people.filter(uic.Filter.startsWith(_.name)), "al")
            later <- suggestions(people.filter(uic.Filter.startsWith(_.name)), "turing")
        yield
            assert(start == Chunk("Alan Turing"), s"$start")
            assert(later.isEmpty, s"$later")
    }

    "Filter.all shows every option whatever was typed, for an app that filters them itself" in
        suggestions(people.filter(uic.Filter.all), "zzz").map(s =>
            assert(s == Chunk("Ada Lovelace", "Grace Hopper", "Alan Turing"), s"$s")
        )

    private def plain(using Frame) = uic.AutoComplete[String]().options(cities)(identity).optionKey(identity)

    /** The field the reader types into. */
    private def input(t: UI)(using Frame): UI.Ast.Element < Sync = textField(t, "p-autocomplete-input")

    /** Whether `t` is the field its mount wired: only that one carries the keyboard. */
    private def wiredField(t: UI)(using Frame): Boolean < Sync =
        Abort.run[AssertionError](Abort.catching[AssertionError](input(t))).map(_.toMaybe.exists(_.attrs.onKeyDown.isDefined))

    // Typing reaches onInput; a pick and a clear reach other events, which hear them here so that the card a one-way text
    // draws for every edit nobody hears says nothing about the edit this slot is not.
    private val valueSlot = new SlotCase[uic.AutoComplete[String], String](
        name = "value",
        bare = Present(plain),
        bind = plain.value(_),
        hear = (c, f) => c.onInput(f).onChange(_ => ()),
        event = "onInput",
        render = _.render,
        start = "",
        next = "Ber",
        control = input(_),
        shows = t => input(t).map(textIn),
        gesture = (c: UI.Ast.Element) => typed(c, "Ber", commit = false),
        answers = editable,
        saysFixed = {
            case t: UI.Ast.TextInput => t.readOnly.contains(true)
            case _                   => false
        },
        ready = Present(wiredField(_)),
        startText = Present("Ams")
    )

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    // The clear button, which a text drawn with nothing in it has no use for, so the slot starts from a text.
    heardBy(new SlotCase[uic.AutoComplete[String], String](
        name = "value cleared",
        bare = Present(plain.showClear(true)),
        bind = plain.showClear(true).value(_),
        hear = (c, f) => c.onChange(f).onInput(_ => ()),
        event = "onChange",
        render = _.render,
        start = "Ber",
        next = "",
        control = elementWithClass(_, "p-autocomplete-clear-icon"),
        shows = t => input(t).map(textIn),
        ready = Present(t =>
            wiredField(t).map(w =>
                if !w then Kyo.lift(false) else elementsWithClass(t, "p-autocomplete-clear-icon").map(_.nonEmpty)
            )
        )
    ))

    /** The suggestions the panel `t` shows, by label. */
    private def suggested(t: UI)(using Frame): Chunk[String] < Sync =
        elementsWithClass(t, "p-autocomplete-option").map(_.map(textOf))

    "unbound, the suggestions follow what the reader types" in {
        for
            after <- publishedAfterReady(plain.render, wiredField(_), until = t => suggested(t).map(_ == Chunk("Berlin")))(t =>
                input(t).map(typed(_, "Ber", commit = false))
            )
            shown <- after.fold(Kyo.lift(Chunk.empty[String]))(suggested)
        yield assert(shown == Chunk("Berlin"), s"suggests $shown for Ber")
    }

    "unbound, a pick puts the label in the field" in {
        for
            after <- publishedAfterReady(plain.render, wiredField(_), until = t => input(t).map(textIn).map(_ == "Berlin"))(
                t => input(t).map(typed(_, "Ber", commit = false)),
                // Each key acts on the field drawn after the one before it, whose keyboard knows what it shows: the panel
                // filtered by what was typed, then the highlight on its one row.
                t =>
                    suggested(t).map(s =>
                        if s != Chunk("Berlin") then throw new AssertionError(s"suggests $s")
                        else input(t).map(press(_, UI.Keyboard.ArrowDown))
                    ),
                t =>
                    input(t).map(f =>
                        if f.attrs.ariaAttrs.get("activedescendant").isEmpty then throw new AssertionError("nothing highlighted")
                        else press(f, UI.Keyboard.Enter)
                    )
            )
        yield assert(after.nonEmpty, "the field never showed the label picked")
    }

    // A field drawn with a value is put back to it by the next render (`__kyoMorphAttrs` in kyo-ui's client), and typing
    // opens the panel, which is a render.
    "a starting text the reader types over is not drawn back over what they typed" in {
        for
            after <- publishedAfterReady(plain.value("Ams").render, wiredField(_), until = t => input(t).map(textIn).map(_ == "Ber"))(t =>
                input(t).map(typed(_, "Ber", commit = false))
            )
        yield assert(after.nonEmpty, "the field was drawn with the starting text again")
    }

    "a text bound one way names each edit that reaches nobody: typing, a pick, a clear" in {
        def card(c: uic.AutoComplete[String]): String < Sync =
            elementsWithClass(c.render, "p-uic-key-error").map(_.map(textOf).mkString)
        for
            held      <- Signal.initRef("a")
            deaf      <- card(field.value(held.readOnly).showClear(true))
            typing    <- card(field.value(held.readOnly).onInput(_ => ()))
            clearing  <- card(field.value(held.readOnly).showClear(true).onInput(_ => ()).onSelect(_ => ()))
            committed <- card(field.value(held.readOnly).onChange(_ => ()))
            heard     <- card(field.value(held.readOnly).showClear(true).onInput(_ => ()).onChange(_ => ()))
            picked    <- card(field.value(held.readOnly).onInput(_ => ()).onSelect(_ => ()))
            constant  <- card(field.value("a"))
            twoWay    <- card(field.value(held))
            ro        <- card(field.value(held.readOnly).readonly(true))
        yield
            assert(!typing.contains("onInput") && typing.contains("a pick"), s"a pick reaches nobody: $typing")
            assert(deaf.contains("onInput") && deaf.contains("onChange or onSelect") && deaf.contains("a clear"), s"nothing: $deaf")
            assert(!clearing.contains("a pick") && clearing.contains("a clear"), s"a clear reaches nobody: $clearing")
            assert(committed.contains("typing") && !committed.contains("a pick"), s"typing reaches nobody: $committed")
            assert(
                heard.isEmpty && picked.isEmpty && constant.isEmpty && twoWay.isEmpty && ro.isEmpty,
                s"every edit is heard, the text stays, or nobody can edit: $heard $picked $constant $twoWay $ro"
            )
        end for
    }

    "typing its owner does not take is put back" in {
        for
            openRef <- Signal.initRef(true)
            hi      <- Signal.initRef(-1)
            all     <- Signal.initRef(false)
            held    <- Signal.initRef("zz")
            ui = field.value(held.readOnly).onInput(_ => ()).wired(openRef, hi, all, Present("ac"))
            patches <- assertsAfter(ui)("p-autocomplete-input")(kyo.internal.UIEvent.Input(_, "zzz"))
        yield assert(patches == Chunk(("value", "zz")), s"patches: $patches")
        end for
    }

end AutoCompleteTest

object AutoCompleteTest:
    final case class User(name: String, email: String) derives CanEqual
end AutoCompleteTest
