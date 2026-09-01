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
class AutoCompleteTest extends UicTest:

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

end AutoCompleteTest
