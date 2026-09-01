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
