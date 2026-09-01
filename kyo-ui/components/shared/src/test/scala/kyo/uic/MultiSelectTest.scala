package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.MultiSelect`'s open panel, which lives wherever focus does: on the
  * TRIGGER without a filter header, in the header's INPUT with one.
  *
  * The same [[ListNav]] the rest of the family reads, with the one difference the pattern asks
  * for: an activation toggles the highlighted option and leaves the panel open.
  */
class MultiSelectTest extends UicTest:

    final private case class Fruit(id: String, name: String, sold: Boolean) derives CanEqual

    private val fruit = List(
        Fruit("a", "Apple", false),
        Fruit("b", "Banana", true),
        Fruit("c", "Cherry", false),
        Fruit("d", "Date", false)
    )

    private def multi(using Frame) =
        uic.MultiSelect[Fruit]().options(fruit)(_.name).optionKey(_.id).optionDisabled(_.sold).id("ms")

    private def panelOf(hiAt: Int, filter: Boolean = false)(using
        Frame
    ): (SignalRef[Int], SignalRef[Set[String]], SignalRef[Boolean], UI) < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(hiAt)
            query <- Signal.initRef("")
            value <- Signal.initRef(Set.empty[String])
            base = if filter then multi.filterable(true) else multi
        yield (hi, value, open, base.value(value).wired(open, hi, query, Present("ms")))

    /** The element the panel's keyboard lives on: the filter header's input where there is one,
      * the trigger otherwise.
      */
    private def keyboardHost(ui: UI, filter: Boolean)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(ui, if filter then "p-multiselect-filter" else "p-multiselect")

    private def after(from: Int, key: UI.Keyboard, filter: Boolean = false)(using Frame): Int < Async =
        for
            (hi, _, _, ui) <- panelOf(from, filter)
            host           <- keyboardHost(ui, filter)
            _              <- press(host, key)
            at             <- hi.get
        yield at

    /** Presses `key` on the CLOSED trigger and reports whether the panel opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard, selected: Set[String])(using Frame): (Boolean, Int) < Async =
        for
            open  <- Signal.initRef(false)
            hi    <- Signal.initRef(-1)
            query <- Signal.initRef("")
            value <- Signal.initRef(selected)
            ui = multi.value(value).wired(open, hi, query, Present("ms"))
            trigger <- elementWithClass(ui, "p-multiselect")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- hi.get
        yield (isOpen, at)

    "an opening key lands on the first option already selected" in
        opening(UI.Keyboard.ArrowDown, Set("c")).map((open, at) => assert(open && at == 2))

    "and on the first option a highlight may sit on where none is" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown, Set.empty)
            space <- opening(UI.Keyboard.Space, Set.empty)
        yield assert(
            down == (true, 0) && space == (true, 0),
            "opening has to land ON an option, or the arrow that opened the panel moved nothing"
        )
    }

    "Tab closes the panel, in both directions and with a filter header too" in {
        val shift = UI.Modifiers(shift = true)
        for
            (_, _, fwdOpen, fwdUi)   <- panelOf(2)
            fwdHost                  <- keyboardHost(fwdUi, filter = false)
            _                        <- press(fwdHost, UI.Keyboard.Tab)
            fwd                      <- fwdOpen.get
            (_, _, backOpen, backUi) <- panelOf(2)
            backHost                 <- keyboardHost(backUi, filter = false)
            _                        <- press(backHost, UI.Keyboard.Tab, shift)
            back                     <- backOpen.get
            (_, _, filOpen, filUi)   <- panelOf(2, filter = true)
            filHost                  <- keyboardHost(filUi, filter = true)
            _                        <- press(filHost, UI.Keyboard.Tab)
            fil                      <- filOpen.get
        yield assert(!fwd && !back && !fil, "a panel the reader has tabbed away from is one nothing answers")
        end for
    }

    "Ctrl or Cmd with A works the select-all the header shows, since that box is no tab stop" in {
        val ctrl = UI.Modifiers(ctrl = true)
        val cmd  = UI.Modifiers(meta = true)
        for
            (_, value, _, ui)   <- panelOf(0)
            host                <- keyboardHost(ui, filter = false)
            _                   <- press(host, UI.Keyboard.Char('a'), ctrl)
            all                 <- value.get
            (_, value2, _, ui2) <- panelOf(0)
            _                   <- value2.set(Set("a", "c", "d"))
            host2               <- keyboardHost(ui2, filter = false)
            _                   <- press(host2, UI.Keyboard.Char('a'), cmd)
            cleared             <- value2.get
        yield assert(
            all == Set("a", "c", "d") && cleared.isEmpty,
            "every enabled option, and the second press clears them; the sold-out one is never selected"
        )
        end for
    }

    "the select-all box is out of the tab order, so its keys cannot be read twice" in {
        for
            (_, _, _, ui) <- panelOf(2)
            box           <- elementWithClass(ui, "p-checkbox-input")
        yield
            assert(box.attrs.tabIndex.contains(-1), "a key pressed on it would reach the trigger by bubbling")
            assert(box.attrs.identifier.contains("ms-all"), "and that is how the trigger knows one did")
    }

    "a key that came from the select-all box is the box's own, so the option list ignores it" in {
        for
            (_, value, open, ui) <- panelOf(2)
            trigger              <- elementWithClass(ui, "p-multiselect")
            bubbled: (Any < Async) = trigger.attrs.onKeyDown match
                case Present(f) => f(UI.KeyboardEvent(UI.Keyboard.Space, UI.Modifiers.none, Present("ms-all")))
                case Absent     => ()
            _      <- bubbled
            picked <- value.get
            still  <- open.get
        yield assert(picked.isEmpty && still, "the box toggles them all; the highlighted option is not a second answer")
    }

    "ArrowDown from nothing lands on the first option" in after(-1, UI.Keyboard.ArrowDown).map(at => assert(at == 0))

    "ArrowUp from nothing lands on the last" in after(-1, UI.Keyboard.ArrowUp).map(at => assert(at == 3))

    "a sold-out option is stepped over" in after(0, UI.Keyboard.ArrowDown).map(at => assert(at == 2))

    "Home and End reach the ends" in {
        for
            home <- after(3, UI.Keyboard.Home)
            end  <- after(0, UI.Keyboard.End)
        yield assert(home == 0 && end == 3)
    }

    "Enter toggles the highlighted option and leaves the panel open" in {
        for
            (_, value, open, ui) <- panelOf(2)
            host                 <- keyboardHost(ui, filter = false)
            _                    <- press(host, UI.Keyboard.Enter)
            picked               <- value.get
            still                <- open.get
        yield assert(picked == Set("c") && still, "a reader picking several things keeps the list")
    }

    "Space toggles it too, where the panel has no text field" in {
        for
            (_, value, _, ui) <- panelOf(2)
            host              <- keyboardHost(ui, filter = false)
            _                 <- press(host, UI.Keyboard.Space)
            picked            <- value.get
        yield assert(picked == Set("c"))
    }

    "with a filter header Space belongs to the caret" in {
        for
            (_, value, _, ui) <- panelOf(2, filter = true)
            host              <- keyboardHost(ui, filter = true)
            _                 <- press(host, UI.Keyboard.Space)
            picked            <- value.get
        yield assert(picked.isEmpty)
    }

    "Escape closes the panel, since the panel it would close never gets the key" in {
        for
            (_, _, open, ui) <- panelOf(2)
            host             <- keyboardHost(ui, filter = false)
            _                <- press(host, UI.Keyboard.Escape)
            still            <- open.get
        yield assert(!still)
    }

    "a printable key jumps to the option it starts" in
        after(-1, UI.Keyboard.Char('d')).map(at => assert(at == 3))

    "and types instead, once there is a filter header" in
        after(-1, UI.Keyboard.Char('d'), filter = true).map(at => assert(at == -1))

    "the trigger is the combobox, and announces the highlighted option" in {
        for
            (_, _, _, ui) <- panelOf(2)
            trigger       <- elementWithClass(ui, "p-multiselect")
            list          <- elementWithClass(ui, "p-multiselect-list")
            rows          <- elementsWithClass(ui, "p-multiselect-option")
        yield
            assert(trigger.attrs.role.contains("combobox"))
            assert(trigger.attrs.ariaAttrs.get("activedescendant").contains("ms-option-2"))
            assert(trigger.attrs.ariaAttrs.get("controls").contains("ms-list"))
            assert(list.attrs.identifier.contains("ms-list"))
            assert(!list.attrs.ariaAttrs.contains("activedescendant"), "the unfocused list says nothing")
            assert(rows(2).attrs.identifier.contains("ms-option-2"))
    }

    "with a filter header the header's input is the combobox instead" in {
        for
            (_, _, _, ui) <- panelOf(2, filter = true)
            trigger       <- elementWithClass(ui, "p-multiselect")
            filterEl      <- elementWithClass(ui, "p-multiselect-filter")
        yield
            assert(trigger.attrs.role.contains("button"))
            assert(!trigger.attrs.ariaAttrs.contains("activedescendant"))
            assert(filterEl.attrs.role.contains("combobox"))
            assert(filterEl.attrs.focusAuto.contains(true))
            assert(filterEl.attrs.ariaAttrs.get("activedescendant").contains("ms-option-2"))
    }

end MultiSelectTest
