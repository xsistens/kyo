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

    "ArrowDown on a closed panel opens it rather than moving" in {
        for
            (hi, _, openRef, ui) <- state(-1, open = false)
            input                <- elementWithClass(ui, "p-autocomplete-input")
            _                    <- press(input, UI.Keyboard.ArrowDown)
            opened               <- openRef.get
            at                   <- hi.get
        yield assert(opened && at == -1, "a closed panel has no highlight to move")
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

end AutoCompleteTest
