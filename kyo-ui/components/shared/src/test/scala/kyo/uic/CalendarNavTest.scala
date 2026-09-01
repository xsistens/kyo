package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers

/** The calendar's pure key machine, which is deliberately not [[GridNav]].
  *
  * Every step is in the view's own unit, so the same machine drives the day, month and year
  * grids and every piece of date arithmetic stays in the host that already owns it.
  */
class CalendarNavTest extends UicTest:

    private val none                                                      = Modifiers.none
    private def key(k: Keyboard, mods: Modifiers = none, perRow: Int = 7) = CalendarNav.onKey(k, mods, perRow)

    "the horizontal arrows step one cell" in {
        assert(key(Keyboard.ArrowLeft) == Present(CalendarNav.Step.Move(-1)))
        assert(key(Keyboard.ArrowRight) == Present(CalendarNav.Step.Move(1)))
    }

    "the vertical arrows step one row, which is the grid's width" in {
        assert(key(Keyboard.ArrowUp) == Present(CalendarNav.Step.Move(-7)))
        assert(key(Keyboard.ArrowDown) == Present(CalendarNav.Step.Move(7)))
        assert(key(Keyboard.ArrowDown, perRow = 3) == Present(CalendarNav.Step.Move(3)), "the month grid is three wide")
    }

    "a grid with no width leaves the vertical arrows to the page" in {
        assert(key(Keyboard.ArrowUp, perRow = 0) == Absent)
        assert(key(Keyboard.ArrowDown, perRow = 0) == Absent)
        assert(key(Keyboard.ArrowLeft, perRow = 0) == Present(CalendarNav.Step.Move(-1)), "sideways still means one")
    }

    "Home and End are the ends of the row, not of the grid" in {
        assert(key(Keyboard.Home) == Present(CalendarNav.Step.RowStart))
        assert(key(Keyboard.End) == Present(CalendarNav.Step.RowEnd))
    }

    "the page keys turn one page, and Shift turns the unit above it" in {
        assert(key(Keyboard.PageUp) == Present(CalendarNav.Step.Page(-1, large = false)))
        assert(key(Keyboard.PageDown) == Present(CalendarNav.Step.Page(1, large = false)))
        assert(key(Keyboard.PageDown, Modifiers.none.copy(shift = true)) == Present(CalendarNav.Step.Page(1, large = true)))
    }

    "Enter and Space both pick, unlike a table's grid where they part ways" in {
        assert(key(Keyboard.Enter) == Present(CalendarNav.Step.Activate))
        assert(key(Keyboard.Space) == Present(CalendarNav.Step.Activate))
    }

    "Escape closes" in assert(key(Keyboard.Escape) == Present(CalendarNav.Step.Dismiss))

    "everything else belongs to the browser" in {
        assert(key(Keyboard.Tab) == Absent, "Tab leaves the grid, which is what a non-modal panel wants")
        assert(key(Keyboard.Char('x')) == Absent)
        assert(key(Keyboard.Backspace) == Absent)
    }

end CalendarNavTest
