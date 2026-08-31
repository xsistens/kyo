package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.MultiSelect`'s open panel.
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

    private def after(from: Int, key: UI.Keyboard, filter: Boolean = false)(using Frame): Int < Async =
        for
            (hi, _, _, ui) <- panelOf(from, filter)
            panel          <- elementWithClass(ui, "p-uic-overlay-panel")
            _              <- press(panel, key)
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
            panel                <- elementWithClass(ui, "p-uic-overlay-panel")
            _                    <- press(panel, UI.Keyboard.Enter)
            picked               <- value.get
            still                <- open.get
        yield assert(picked == Set("c") && still, "a reader picking several things keeps the list")
    }

    "Space toggles it too, where the panel has no text field" in {
        for
            (_, value, _, ui) <- panelOf(2)
            panel             <- elementWithClass(ui, "p-uic-overlay-panel")
            _                 <- press(panel, UI.Keyboard.Space)
            picked            <- value.get
        yield assert(picked == Set("c"))
    }

    "with a filter header Space belongs to the caret" in {
        for
            (_, value, _, ui) <- panelOf(2, filter = true)
            panel             <- elementWithClass(ui, "p-uic-overlay-panel")
            _                 <- press(panel, UI.Keyboard.Space)
            picked            <- value.get
        yield assert(picked.isEmpty)
    }

    "a printable key jumps to the option it starts" in
        after(-1, UI.Keyboard.Char('d')).map(at => assert(at == 3))

    "and types instead, once there is a filter header" in
        after(-1, UI.Keyboard.Char('d'), filter = true).map(at => assert(at == -1))

    "the list announces the highlighted option" in {
        for
            (_, _, _, ui) <- panelOf(2)
            list          <- elementWithClass(ui, "p-multiselect-list")
            rows          <- elementsWithClass(ui, "p-multiselect-option")
        yield
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("ms-option-2"))
            assert(rows(2).attrs.identifier.contains("ms-option-2"))
    }

end MultiSelectTest
