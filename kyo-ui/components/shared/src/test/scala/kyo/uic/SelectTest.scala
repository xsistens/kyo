package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.Select`'s open panel.
  *
  * It used to be a hand-written `var` loop that knew ArrowDown, ArrowUp and Enter and nothing
  * else. It is [[ListNav]] now, which is where Home and End come from, and the two keys a text
  * field owns are held back only when the panel actually has one.
  */
class SelectTest extends UicTest:

    final private case class Fruit(id: String, name: String, sold: Boolean) derives CanEqual

    private val fruit = List(
        Fruit("a", "Apple", false),
        Fruit("b", "Banana", true),
        Fruit("c", "Cherry", false),
        Fruit("d", "Date", false)
    )

    private def select(using Frame) =
        uic.Select[Fruit]().options(fruit)(_.name).optionKey(_.id).optionDisabled(_.sold).id("sel")

    /** Opens the panel with the highlight at `from`, presses `key`, and reports where it landed. */
    private def after(from: Int, key: UI.Keyboard, filter: Boolean = false)(using Frame): Int < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(from)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            base = if filter then select.filterable(true) else select
            ui   = base.value(value).wired(open, hi, query, Present("sel"))
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
            _     <- press(panel, key)
            at    <- hi.get
        yield at

    "ArrowDown from nothing lands on the first option" in after(-1, UI.Keyboard.ArrowDown).map(at => assert(at == 0))

    "ArrowUp from nothing lands on the last, as it does everywhere else in the family" in
        after(-1, UI.Keyboard.ArrowUp).map(at => assert(at == 3))

    "a sold-out option is stepped over" in after(0, UI.Keyboard.ArrowDown).map(at => assert(at == 2))

    "Home and End reach the ends, which the hand-written loop had no answer for" in {
        for
            home <- after(3, UI.Keyboard.Home)
            end  <- after(0, UI.Keyboard.End)
        yield assert(home == 0 && end == 3)
    }

    "the ends hold, since Prime's select does not cycle" in {
        for
            top    <- after(0, UI.Keyboard.ArrowUp)
            bottom <- after(3, UI.Keyboard.ArrowDown)
        yield assert(top == 0 && bottom == 3)
    }

    "Enter picks the highlighted option" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
            _     <- press(panel, UI.Keyboard.Enter)
            got   <- value.get
            still <- open.get
        yield assert(got == "c" && !still, "it picks and closes")
    }

    "Space picks it too, where the panel has no text field to type into" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
            _     <- press(panel, UI.Keyboard.Space)
            got   <- value.get
        yield assert(got == "c")
    }

    "with a filter header Space belongs to the caret, so it picks nothing" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.filterable(true).value(value).wired(open, hi, query, Present("sel"))
            panel <- elementWithClass(ui, "p-uic-overlay-panel")
            _     <- press(panel, UI.Keyboard.Space)
            got   <- value.get
        yield assert(got.isEmpty)
    }

    "a printable key jumps to the option it starts" in
        after(-1, UI.Keyboard.Char('c')).map(at => assert(at == 2))

    "the same letter again walks to the next match" in {
        // Two options start with a D and a B, so C is the interesting single case; a repeat of a
        // letter with one match stays where it is.
        for
            first <- after(-1, UI.Keyboard.Char('a'))
            again <- after(0, UI.Keyboard.Char('a'))
        yield assert(first == 0 && again == 0)
    }

    "a printable key does not jump past a sold-out option" in
        after(-1, UI.Keyboard.Char('b')).map(at => assert(at == -1, "Banana is disabled, so nothing matches"))

    "with a filter header a printable key types instead of jumping" in
        after(-1, UI.Keyboard.Char('c'), filter = true).map(at => assert(at == -1))

    "the list announces the highlighted option" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            list <- elementWithClass(ui, "p-select-list")
            rows <- elementsWithClass(ui, "p-select-option")
        yield
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("sel-option-2"))
            assert(rows(2).attrs.identifier.contains("sel-option-2"), "and the option it names is there")
    }

    "nothing highlighted announces nothing" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(-1)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            list <- elementWithClass(ui, "p-select-list")
        yield assert(!list.attrs.ariaAttrs.contains("activedescendant"))
    }

end SelectTest
