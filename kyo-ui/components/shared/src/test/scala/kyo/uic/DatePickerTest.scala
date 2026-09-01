package kyo.uic

import kyo.*
import kyo.UI.*

/** The calendar's keyboard, which is one tab stop with a roving highlight inside it.
  *
  * A day is a `<span>`, not a control of its own, so this is the announced-highlight form the
  * list family uses rather than a roving tabindex: the grid holds the focus, `.p-focus` says
  * which day, and `aria-activedescendant` says it out loud. That is also why nothing here moves
  * DOM focus, and therefore why nothing here can race the render that draws the month it moved
  * into.
  */
class DatePickerTest extends UicTest:

    /** July 2026 starts on a Wednesday and has 31 days, so it spans five weeks. */
    private val july = "2026-07-15"

    private def wired(value: String, month: String = "", cursor: String = "")(using
        Frame
    ): (SignalRef[String], SignalRef[String], SignalRef[String], SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        for
            vref  <- Signal.initRef(value)
            mref  <- Signal.initRef(month)
            cur   <- Signal.initRef(cursor)
            open  <- Signal.initRef(true)
            moved <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker().value(vref).wired(open, mref, cur, "dp", id => moved.updateAndGet(_ :+ id))
        yield (vref, mref, cur, open, moved, ui)

    /** Presses `key` on the day grid and reports where the cursor landed. */
    private def after(key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none, from: String = july)(using
        Frame
    ): (String, String) < Async =
        for
            (_, mref, cur, _, _, ui) <- wired(july, cursor = from)
            grid                     <- elementWithClass(ui, "p-datepicker-day-view")
            _                        <- press(grid, key, mods)
            at                       <- cur.get
            month                    <- mref.get
        yield (at, month)

    "the grid is ONE tab stop, and it says which day it is on" in {
        for
            (_, _, _, _, _, ui) <- wired(july)
            grid                <- elementWithClass(ui, "p-datepicker-day-view")
            days                <- elementsWithClass(ui, "p-datepicker-day")
            cells               <- elementsWithClass(ui, "p-datepicker-day-cell")
        yield
            assert(grid.attrs.role.contains("grid"))
            assert(grid.attrs.tabIndex.contains(0), "the grid takes the Tab, not the thirty-one days in it")
            assert(days.forall(_.attrs.tabIndex.isEmpty), "a day is not its own tab stop")
            assert(cells.forall(_.attrs.role.contains("gridcell")), "every cell says what it is")
            // July 2026 starts on a Wednesday, so the 15th sits at cell 3 + 15 - 1.
            assert(grid.attrs.ariaAttrs.get("activedescendant").contains("dp-c17"))
            assert(cells.exists(_.attrs.identifier.contains("dp-c17")), "and the cell it names is there")
            assert(days.exists(d => d.attrs.cssClasses.contains("p-focus")), "the class is on the day the sheet paints")
    }

    "with no cursor yet the highlight opens on the selected day" in {
        for
            (_, _, _, _, _, ui) <- wired(july)
            grid                <- elementWithClass(ui, "p-datepicker-day-view")
        yield assert(grid.attrs.ariaAttrs.get("activedescendant").contains("dp-c17"))
    }

    "the horizontal arrows step a day, crossing the week rather than stopping at its edge" in {
        for
            // The 18th of July 2026 is a Saturday, so a step right leaves the row.
            (right, _) <- after(UI.Keyboard.ArrowRight, from = "2026-07-18")
            (left, _)  <- after(UI.Keyboard.ArrowLeft, from = "2026-07-19")
        yield assert(right == "2026-07-19" && left == "2026-07-18")
    }

    "the vertical arrows step a week" in {
        for
            (down, _) <- after(UI.Keyboard.ArrowDown)
            (up, _)   <- after(UI.Keyboard.ArrowUp)
        yield assert(down == "2026-07-22" && up == "2026-07-08")
    }

    "a step off the month carries the displayed month with it" in {
        for
            (at, month) <- after(UI.Keyboard.ArrowDown, from = "2026-07-29")
        yield assert(at == "2026-08-05" && month == "2026-08", "the grid follows the cursor out of July")
    }

    "Home and End are the ends of the WEEK" in {
        for
            (home, _) <- after(UI.Keyboard.Home)
            (end, _)  <- after(UI.Keyboard.End)
        yield assert(home == "2026-07-12" && end == "2026-07-18", "the 15th is a Wednesday")
    }

    "the page keys turn a month, and Shift turns a year" in {
        for
            (up, upM)     <- after(UI.Keyboard.PageUp)
            (down, downM) <- after(UI.Keyboard.PageDown)
            (year, yearM) <- after(UI.Keyboard.PageDown, UI.Modifiers.none.copy(shift = true))
        yield
            assert(up == "2026-06-15" && upM == "2026-06")
            assert(down == "2026-08-15" && downM == "2026-08")
            assert(year == "2027-07-15" && yearM == "2027-07")
    }

    "a page into a shorter month clamps the day instead of jumping to its first" in
        after(UI.Keyboard.PageUp, from = "2026-07-31").map((at, _) => assert(at == "2026-06-30"))

    "Enter picks the highlighted day and closes, and so does Space" in {
        for
            (v1, _, _, o1, _, ui1) <- wired("", month = "2026-07", cursor = "2026-07-09")
            grid1                  <- elementWithClass(ui1, "p-datepicker-day-view")
            _                      <- press(grid1, UI.Keyboard.Enter)
            picked                 <- v1.get
            open                   <- o1.get
            (v2, _, _, _, _, ui2)  <- wired("", month = "2026-07", cursor = "2026-07-09")
            grid2                  <- elementWithClass(ui2, "p-datepicker-day-view")
            _                      <- press(grid2, UI.Keyboard.Space)
            spaced                 <- v2.get
        yield assert(picked == "2026-07-09" && !open && spaced == "2026-07-09")
    }

    "Escape closes and hands focus back to the field" in {
        for
            (_, _, _, open, moved, ui) <- wired(july)
            grid                       <- elementWithClass(ui, "p-datepicker-day-view")
            _                          <- press(grid, UI.Keyboard.Escape)
            still                      <- open.get
            got                        <- moved.get
        yield assert(!still && got == List("dp-field"))
    }

    "the field opens on ArrowDown, and steps into the grid once it is open" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            moved     <- Signal.initRef(List.empty[String])
            vref      <- Signal.initRef(july)
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, "dp", id => moved.updateAndGet(_ :+ id))
            field  <- elementWithClass(ui, "p-datepicker-input")
            _      <- press(field, UI.Keyboard.ArrowDown)
            opened <- closedRef.get
            first  <- moved.get
            _      <- press(field, UI.Keyboard.ArrowDown)
            second <- moved.get
        yield
            assert(opened, "the first press opens")
            assert(first.isEmpty, "and moves no focus, since the grid it would move to does not exist yet")
            assert(second == List("dp-grid"), "the second steps in")
    }

    "the field keeps Space and Enter, because it is a text box" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            vref      <- Signal.initRef("")
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, "dp", _ => ())
            field <- elementWithClass(ui, "p-datepicker-input")
            _     <- press(field, UI.Keyboard.Space)
            _     <- press(field, UI.Keyboard.Enter)
            still <- closedRef.get
        yield assert(!still, "neither opens the panel: one types a space, the other belongs to the form")
    }

    "the header buttons move the month without a caller binding one" in {
        for
            (_, mref, _, _, _, ui) <- wired(july)
            prev                   <- elementWithClass(ui, "p-datepicker-prev-button")
            _                      <- click(prev)
            at                     <- mref.get
        yield assert(at == "2026-06", "the mount mints the ref that used to leave these disabled")
    }

end DatePickerTest
