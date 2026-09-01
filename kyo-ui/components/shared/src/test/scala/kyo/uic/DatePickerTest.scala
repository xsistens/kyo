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
            view  <- Signal.initRef(DatePickerView.Date)
            seed  <- Signal.initRef(false)
            moved <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker().value(vref).wired(open, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
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

    /** A picker built the way its mount builds it, with the focus moves it asks for recorded. */
    private def recording(dp: uic.DatePicker, value: String, month: String = "", cursor: String = "", open: Boolean = true)(
        using Frame
    ): (SignalRef[String], SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        for
            vref <- Signal.initRef(value)
            mref <- Signal.initRef(month)
            cur  <- Signal.initRef(cursor)
            oref <- Signal.initRef(open)
            // The mount starts its minted ref at the static view, and so does this.
            view  <- Signal.initRef(dp.viewV)
            seed  <- Signal.initRef(false)
            moved <- Signal.initRef(List.empty[String])
            ui = dp.value(vref).wired(oref, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
        yield (vref, oref, moved, ui)

    "a pick hands the focus back too, since the pick takes the grid with it" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker(), "", month = "2026-07", cursor = "2026-07-09")
            grid                 <- elementWithClass(ui, "p-datepicker-day-view")
            _                    <- press(grid, UI.Keyboard.Enter)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"), "or the next Tab starts at the top of the document")
    }

    "the button bar closes on the same terms, and its button goes with the panel" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker().showButtonBar(true), july)
            clear                <- elementWithClass(ui, "p-datepicker-clear-button")
            _                    <- click(clear)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"))
    }

    "and so does a month pick, which closes the panel under view(Month)" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker().view(DatePickerView.Month), "2026-07")
            grid                 <- elementWithClass(ui, "p-datepicker-month-view")
            _                    <- press(grid, UI.Keyboard.Enter)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"))
    }

    "a completed range closes and returns the focus; the first half of it does neither" in {
        for
            start <- Signal.initRef("")
            end   <- Signal.initRef("")
            mref  <- Signal.initRef("2026-07")
            cur   <- Signal.initRef("2026-07-09")
            oref  <- Signal.initRef(true)
            view  <- Signal.initRef(DatePickerView.Date)
            seed  <- Signal.initRef(false)
            moved <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker().range(start, end).wired(oref, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            opened <- oref.get
            half   <- moved.get
            _      <- cur.set("2026-07-14")
            later  <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(later, UI.Keyboard.Enter)
            from   <- start.get
            to     <- end.get
            closed <- oref.get
            got    <- moved.get
        yield
            assert(opened && half.isEmpty, "a range half-picked is still being picked, so the panel stays")
            assert(from == "2026-07-09" && to == "2026-07-14")
            assert(!closed && got == List("dp-field"))
    }

    "a pick in multiple mode leaves the panel standing, and the focus on the grid" in {
        for
            set   <- Signal.initRef(Set("2026-07-02"))
            mref  <- Signal.initRef("2026-07")
            cur   <- Signal.initRef("2026-07-09")
            oref  <- Signal.initRef(true)
            view  <- Signal.initRef(DatePickerView.Date)
            seed  <- Signal.initRef(false)
            moved <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker().values(set).wired(oref, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            picked <- set.get
            still  <- oref.get
            got    <- moved.get
        yield assert(picked == Set("2026-07-02", "2026-07-09") && still && got.isEmpty, "there is a second date to pick")
    }

    "Escape in the month grid closes the panel, the way it does in the day grid" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker().view(DatePickerView.Month), "2026-07")
            grid                 <- elementWithClass(ui, "p-datepicker-month-view")
            _                    <- press(grid, UI.Keyboard.Escape)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"), "it used to move the focus and leave the panel standing")
    }

    "an inline calendar stays on the page when a day is picked, and keeps the focus with it" in {
        for
            (value, open, moved, ui) <- recording(uic.DatePicker().inline(true), "", month = "2026-07", cursor = "2026-07-09")
            grid                     <- elementWithClass(ui, "p-datepicker-day-view")
            _                        <- press(grid, UI.Keyboard.Enter)
            picked                   <- value.get
            still                    <- open.get
            got                      <- moved.get
        yield
            assert(picked == "2026-07-09")
            assert(still, "an in-flow calendar is the page's furniture: a pick is not a reason to take it away")
            assert(got.isEmpty, "and the reader is standing on the grid that stays")
    }

    "nor does Escape take it away, there being no panel to dismiss" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker().inline(true), july)
            grid                 <- elementWithClass(ui, "p-datepicker-day-view")
            _                    <- press(grid, UI.Keyboard.Escape)
            still                <- open.get
            got                  <- moved.get
        yield assert(still && got.isEmpty)
    }

    "drilling down leaves the keyboard on a DAY, not on the month it drilled through" in {
        for
            vref <- Signal.initRef(july)
            view <- Signal.initRef(DatePickerView.Month)
            seed <- Signal.initRef(false)
            mref <- Signal.initRef("2026-07")
            cur  <- Signal.initRef("")
            oref <- Signal.initRef(true)
            ui = uic.DatePicker().value(vref).wired(oref, mref, cur, view, seed, "dp", _ => ())
            months <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(months, UI.Keyboard.ArrowRight)
            // Re-read: the handler on the element above closed over the cursor it rendered with,
            // and the browser would be pressing Enter on the grid the move re-rendered.
            moved  <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(moved, UI.Keyboard.Enter)
            landed <- view.get
            days   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(days, UI.Keyboard.ArrowRight)
            at     <- cur.get
        yield
            assert(landed == DatePickerView.Date, "Enter on a month drills into it")
            assert(at.length == 10 && at.startsWith("2026-08"), s"the cursor is a day of the month drilled into, and was $at")
    }

    "a click seeds the highlight, so the next arrow carries on from the day the pointer named" in {
        for
            set  <- Signal.initRef(Set.empty[String])
            mref <- Signal.initRef("2026-07")
            cur  <- Signal.initRef("2026-07-15")
            oref <- Signal.initRef(true)
            view <- Signal.initRef(DatePickerView.Date)
            seed <- Signal.initRef(false)
            ui = uic.DatePicker().values(set).wired(oref, mref, cur, view, seed, "dp", _ => ())
            days  <- elementsWithClass(ui, "p-datepicker-day")
            _     <- click(days.find(_.children.exists(_ == UI.Ast.Text("4"))).get)
            after <- cur.get
        yield assert(after == "2026-07-04", "the pointer seeds and the keyboard carries on; it does not start over")
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

    "the field opens on ArrowDown, and a press on the open field steps in by command" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            moved     <- Signal.initRef(List.empty[String])
            vref      <- Signal.initRef(july)
            view      <- Signal.initRef(DatePickerView.Date)
            seed      <- Signal.initRef(false)
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
            field  <- elementWithClass(ui, "p-datepicker-input")
            _      <- press(field, UI.Keyboard.ArrowDown)
            opened <- closedRef.get
            first  <- moved.get
            _      <- press(field, UI.Keyboard.ArrowDown)
            second <- moved.get
        yield
            assert(opened, "the first press opens")
            assert(first.isEmpty, "and sends no focus command, since the grid it would name does not exist yet")
            assert(second == List("dp-grid"), "with the panel open the grid is there to be named")
    }

    "a keyboard open marks the grid to take the focus in the patch that inserts it" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            view      <- Signal.initRef(DatePickerView.Date)
            seed      <- Signal.initRef(false)
            vref      <- Signal.initRef(july)
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, view, seed, "dp", _ => ())
            field  <- elementWithClass(ui, "p-datepicker-input")
            _      <- press(field, UI.Keyboard.ArrowDown)
            opened <- closedRef.get
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
        yield
            assert(opened)
            assert(grid.attrs.focusAuto.contains(true), "a focus command would race the insert; this rides with it")
    }

    "and a pointer open does not, since the reader clicked a text field" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            view      <- Signal.initRef(DatePickerView.Date)
            seed      <- Signal.initRef(true)
            vref      <- Signal.initRef(july)
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, view, seed, "dp", _ => ())
            field <- elementWithClass(ui, "p-datepicker-input")
            _     <- click(field)
            grid  <- elementWithClass(ui, "p-datepicker-day-view")
        yield assert(!grid.attrs.focusAuto.contains(true), "and it lowers a flag an earlier keyboard open left up")
    }

    "the field keeps Space and Enter, because it is a text box" in {
        for
            closedRef <- Signal.initRef(false)
            mref      <- Signal.initRef("")
            cur       <- Signal.initRef("")
            vref      <- Signal.initRef("")
            view      <- Signal.initRef(DatePickerView.Date)
            seed      <- Signal.initRef(false)
            ui = uic.DatePicker().value(vref).wired(closedRef, mref, cur, view, seed, "dp", _ => ())
            field <- elementWithClass(ui, "p-datepicker-input")
            _     <- press(field, UI.Keyboard.Space)
            _     <- press(field, UI.Keyboard.Enter)
            still <- closedRef.get
        yield assert(!still, "neither opens the panel: one types a space, the other belongs to the form")
    }

    "the granular grids are grids: every cell sits in a row" in {
        for
            (_, _, _, ui) <- recording(uic.DatePicker().view(DatePickerView.Month), "2026-07")
            grid          <- elementWithClass(ui, "p-datepicker-month-view")
            all           <- elements(ui)
            rows  = all.filter(_.attrs.role.contains("row"))
            cells = all.filter(_.attrs.role.contains("gridcell"))
        yield
            assert(grid.attrs.role.contains("grid"))
            assert(cells.size == 12, "twelve months")
            assert(rows.size == 4, "three to a row, which is the width the vertical arrows step by")
            assert(
                rows.forall(r => r.children.count { case e: UI.Ast.Element => e.attrs.role.contains("gridcell"); case _ => false } == 3),
                "a gridcell with no row over it is not in the grid as far as the ARIA tree is concerned"
            )
    }

    "a view switch hands the focus to the grid it switched to" in {
        for
            (_, _, moved, ui) <- recording(uic.DatePicker(), july)
            month <- elements(ui).map(_.collectFirst {
                case b: UI.Ast.Button if b.attrs.cssClasses.contains("p-datepicker-select-month") => b
            }.get)
            _   <- click(month)
            got <- moved.get
        yield assert(got == List("dp-grid"), "the button it was pressed on is not in the header the switch draws")
    }

    "and a drill hands it to the day grid it drilled into" in {
        for
            vref  <- Signal.initRef(july)
            view  <- Signal.initRef(DatePickerView.Month)
            seed  <- Signal.initRef(false)
            mref  <- Signal.initRef("2026-07")
            cur   <- Signal.initRef("")
            oref  <- Signal.initRef(true)
            moved <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker().value(vref).wired(oref, mref, cur, view, seed, "dp", id => moved.updateAndGet(_ :+ id))
            months <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(months, UI.Keyboard.Enter)
            landed <- view.get
            got    <- moved.get
        yield assert(landed == DatePickerView.Date && got == List("dp-grid"))
    }

    "the title buttons switch the grid without a caller binding a view ref either" in {
        for
            (_, _, _, _, _, ui) <- wired(july)
            month <- elements(ui).map(_.collectFirst {
                case b: UI.Ast.Button if b.attrs.cssClasses.contains("p-datepicker-select-month") => b
            }.get)
            _    <- click(month)
            grid <- elementWithClass(ui, "p-datepicker-month-view")
        yield
            assert(!month.disabled.contains(true), "they used to render disabled, which left the two granular grids unreachable")
            assert(grid.attrs.role.contains("grid"), "and the grid they switch to is the one with the keyboard")
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
