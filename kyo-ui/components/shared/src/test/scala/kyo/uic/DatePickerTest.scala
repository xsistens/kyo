package kyo.uic

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Year
import java.time.YearMonth
import java.time.ZonedDateTime
import java.time.ZoneId
import kyo.*
import kyo.UI.*
import kyo.uic.DateKind.given

/** The calendar's keyboard, which is one tab stop with a roving highlight inside it, and the typed value behind it.
  *
  * A day is a `<span>`, not a control of its own, so this is the announced-highlight form the list family uses rather than
  * a roving tabindex: the grid holds the focus, `.p-focus` says which day, and `aria-activedescendant` says it out loud.
  * That is also why nothing here moves DOM focus, and therefore why nothing here can race the render that draws the month it
  * moved into.
  *
  * The value type decides what the picker is, so each kind has its own section: what it shows, what a pick writes, and that
  * a write naming the value already held leaves the app's own instance alone.
  */
class DatePickerTest extends UicTest:

    /** July 2026 starts on a Wednesday and has 31 days, so it spans five weeks. */
    private val july = "2026-07-15"

    private def day(iso: String): LocalDate = LocalDate.parse(iso)

    /** The refs a mount hands the wired host, minted the way the mount mints them, plus the month the test reads back. */
    private def refs(open: Boolean = true, month: String = "", cursor: String = "", view: DatePickerView = DatePickerView.Date)(
        using Frame
    ): (DatePicker.Refs, SignalRef[String]) < Async =
        for
            oref    <- Signal.initRef(open)
            mref    <- Signal.initRef(month)
            cur     <- Signal.initRef(cursor)
            vref    <- Signal.initRef(view)
            seed    <- Signal.initRef(false)
            restore <- Signal.initRef(Maybe.empty[DatePicker.Restore])
            draft   <- Signal.initRef(Maybe.empty[String])
        yield (DatePicker.Refs(oref, DatePicker.ShownMonth.Own(mref), cur, vref, seed, restore, draft), mref)

    private def wired(value: String, month: String = "", cursor: String = "")(using
        Frame
    ): (SignalRef[Maybe[LocalDate]], SignalRef[String], SignalRef[String], SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        for
            vref       <- Signal.initRef(if value.isEmpty then Maybe.empty[LocalDate] else Present(day(value)))
            (rs, mref) <- refs(month = month, cursor = cursor)
            moved      <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
        yield (vref, mref, rs.cursor, rs.open, moved, ui)

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

    /** Commits `text` into the field, as the browser's `change` would. */
    private def typeInto(ui: UI, text: String)(using Frame): Any < Async =
        elementWithClass(ui, "p-datepicker-input").map {
            case i: UI.Ast.Input =>
                i.onChange match
                    case Present(f) => handled(f(text))
                    case Absent     => throw new AssertionError("the field reads no typed text")
            case other => throw new AssertionError(s"the field is not a text input: $other")
        }

    /** Moves the focus off the field. */
    private def blur(ui: UI)(using Frame): Any < Async =
        elementWithClass(ui, "p-datepicker-input").map(_.attrs.onBlur match
            case Present(eff) => handled(eff)
            case Absent       => throw new AssertionError("the field declares no blur handler"))

    /** The text the field shows. */
    private def fieldText(ui: UI)(using Frame): String < Async =
        elementWithClass(ui, "p-datepicker-input").map {
            case i: UI.Ast.Input =>
                i.value match
                    case Present(UI.Bound.Const(v)) => v
                    case other                      => throw new AssertionError(s"the field holds no constant text: $other")
            case other => throw new AssertionError(s"the field is not a text input: $other")
        }

    /** The first button carrying `cls`. */
    private def buttonWith(ui: UI, cls: String)(using Frame): UI.Ast.Button < Sync =
        elements(ui).map(_.collectFirst { case b: UI.Ast.Button if b.attrs.cssClasses.contains(cls) => b }.get)

    private def dayCell(ui: UI, n: Int)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-datepicker-day").map(
            _.find(d => !d.attrs.cssClasses.contains("p-disabled") && d.children.exists(_ == UI.Ast.Text(n.toString))).get
        )

    // === the keyboard ===================================================================================

    "the open calendar is ONE reactive region over everything it reads, the open gate included" in {
        for
            (_, _, _, _, _, ui) <- wired(july)
            regions             <- regionsAbove(ui, "p-datepicker-panel")
        yield assert(
            regions == 1,
            s"a pick writes the value, the month and the cursor and closes the panel; nested regions can repaint the older state: $regions"
        )
    }

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

    "a carried horizontal arrow turns the month, and Shift turns four" in {
        for
            (fwd, fwdM)   <- after(UI.Keyboard.ArrowRight, UI.Modifiers.none.copy(meta = true))
            (back, backM) <- after(UI.Keyboard.ArrowLeft, UI.Modifiers.none.copy(ctrl = true))
            (four, fourM) <- after(UI.Keyboard.ArrowRight, UI.Modifiers.none.copy(meta = true, shift = true))
        yield
            assert(fwd == "2026-08-15" && fwdM == "2026-08")
            assert(back == "2026-06-15" && backM == "2026-06")
            assert(four == "2026-11-15" && fourM == "2026-11")
    }

    "a carried vertical arrow turns the year, and Shift turns ten" in {
        for
            (up, upM)     <- after(UI.Keyboard.ArrowUp, UI.Modifiers.none.copy(meta = true))
            (down, downM) <- after(UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(meta = true))
            (ten, tenM)   <- after(UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(meta = true, shift = true))
        yield
            assert(up == "2027-07-15" && upM == "2027-07", "up is forward for the carried pair")
            assert(down == "2025-07-15" && downM == "2025-07")
            assert(ten == "2016-07-15" && tenM == "2016-07")
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
        yield assert(picked == Present(day("2026-07-09")) && !open && spaced == Present(day("2026-07-09")))
    }

    /** A picker built the way its mount builds it, with the focus moves it asks for recorded. */
    private def recording[A](dp: uic.DatePicker[A], value: Maybe[A], month: String = "", cursor: String = "", open: Boolean = true)(
        using Frame
    ): (SignalRef[Maybe[A]], SignalRef[Boolean], SignalRef[List[String]], UI) < Async =
        given CanEqual[A, A] = CanEqual.derived
        for
            vref <- Signal.initRef(value)
            // The mount starts its minted view ref at the grid the kind picks in, and so does this.
            (rs, _) <- refs(open = open, month = month, cursor = cursor, view = dp.kind.view)
            moved   <- Signal.initRef(List.empty[String])
            ui = dp.value(vref).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
        yield (vref, rs.open, moved, ui)
        end for
    end recording

    "a pick hands the focus back too, since the pick takes the grid with it" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker[LocalDate](), Absent, month = "2026-07", cursor = "2026-07-09")
            grid                 <- elementWithClass(ui, "p-datepicker-day-view")
            _                    <- press(grid, UI.Keyboard.Enter)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"), "or the next Tab starts at the top of the document")
    }

    "the button bar closes on the same terms, and its button goes with the panel" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker[LocalDate]().showButtonBar(true), Present(day(july)))
            clear                <- elementWithClass(ui, "p-datepicker-clear-button")
            _                    <- click(clear)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"))
    }

    "a completed range closes and returns the focus; the first half of it does neither" in {
        for
            start   <- Signal.initRef(Maybe.empty[LocalDate])
            end     <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            moved   <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker[LocalDate]().range(start, end).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            opened <- rs.open.get
            half   <- moved.get
            _      <- rs.cursor.set("2026-07-14")
            later  <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(later, UI.Keyboard.Enter)
            from   <- start.get
            to     <- end.get
            closed <- rs.open.get
            got    <- moved.get
        yield
            assert(opened && half.isEmpty, "a range half-picked is still being picked, so the panel stays")
            assert(from == Present(day("2026-07-09")) && to == Present(day("2026-07-14")))
            assert(!closed && got == List("dp-field"))
    }

    "a pick in multiple mode leaves the panel standing, and the focus on the grid" in {
        for
            set     <- Signal.initRef(Set(day("2026-07-02")))
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            moved   <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker[LocalDate]().values(set).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            picked <- set.get
            still  <- rs.open.get
            got    <- moved.get
        yield assert(picked == Set(day("2026-07-02"), day("2026-07-09")) && still && got.isEmpty, "there is a second date to pick")
    }

    "Escape in the month grid closes the panel, the way it does in the day grid" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker[YearMonth](), Present(YearMonth.of(2026, 7)))
            grid                 <- elementWithClass(ui, "p-datepicker-month-view")
            _                    <- press(grid, UI.Keyboard.Escape)
            still                <- open.get
            got                  <- moved.get
        yield assert(!still && got == List("dp-field"), "it used to move the focus and leave the panel standing")
    }

    "an inline calendar stays on the page when a day is picked, and keeps the focus with it" in {
        for
            (value, open, moved, ui) <-
                recording(uic.DatePicker[LocalDate]().inline(true), Absent, month = "2026-07", cursor = "2026-07-09")
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            picked <- value.get
            still  <- open.get
            got    <- moved.get
        yield
            assert(picked == Present(day("2026-07-09")))
            assert(still, "an in-flow calendar is the page's furniture: a pick is not a reason to take it away")
            assert(got.isEmpty, "and the reader is standing on the grid that stays")
    }

    "nor does Escape take it away, there being no panel to dismiss" in {
        for
            (_, open, moved, ui) <- recording(uic.DatePicker[LocalDate]().inline(true), Present(day(july)))
            grid                 <- elementWithClass(ui, "p-datepicker-day-view")
            _                    <- press(grid, UI.Keyboard.Escape)
            still                <- open.get
            got                  <- moved.get
        yield assert(still && got.isEmpty)
    }

    "drilling down leaves the keyboard on a DAY, not on the month it drilled through" in {
        for
            vref    <- Signal.initRef(Maybe(day(july)))
            (rs, _) <- refs(month = "2026-07", view = DatePickerView.Month)
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", _ => ())
            months <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(months, UI.Keyboard.ArrowRight)
            // Re-read: the handler on the element above closed over the cursor it rendered with,
            // and the browser would be pressing Enter on the grid the move re-rendered.
            moved  <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(moved, UI.Keyboard.Enter)
            landed <- rs.view.get
            days   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(days, UI.Keyboard.ArrowRight)
            at     <- rs.cursor.get
            kept   <- vref.get
        yield
            assert(landed == DatePickerView.Date, "Enter on a month drills into it")
            assert(at.length == 10 && at.startsWith("2026-08"), s"the cursor is a day of the month drilled into, and was $at")
            assert(kept == Present(day(july)), "a drill pages the calendar; it picks nothing")
    }

    "a click seeds the highlight, so the next arrow carries on from the day the pointer named" in {
        for
            set     <- Signal.initRef(Set.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-15")
            ui = uic.DatePicker[LocalDate]().values(set).wired(rs, "dp", _ => ())
            four  <- dayCell(ui, 4)
            _     <- click(four)
            after <- rs.cursor.get
        yield assert(after == "2026-07-04", "the pointer seeds and the keyboard carries on; it does not start over")
    }

    "Escape is a cancel: it puts back what the panel held before the reader touched it" in {
        for
            set     <- Signal.initRef(Set(day("2026-07-02")))
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            fired   <- Signal.initRef(0)
            ui = uic.DatePicker[LocalDate]().values(set).onChange(_ => fired.updateAndGet(_ + 1).unit).wired(rs, "dp", _ => ())
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            picked <- set.get
            after  <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(after, UI.Keyboard.Escape)
            back   <- set.get
            open   <- rs.open.get
            count  <- fired.get
        yield
            assert(picked == Set(day("2026-07-02"), day("2026-07-09")), "the pick lands while the panel is up")
            assert(back == Set(day("2026-07-02")), "and Escape takes it away again")
            assert(!open)
            assert(count == 1, "the pick reported itself; the undo of it did not")
    }

    "a half-picked range is cancelled whole" in {
        for
            start   <- Signal.initRef(Maybe.empty[LocalDate])
            end     <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            ui = uic.DatePicker[LocalDate]().range(start, end).wired(rs, "dp", _ => ())
            grid  <- elementWithClass(ui, "p-datepicker-day-view")
            _     <- press(grid, UI.Keyboard.Enter)
            half  <- start.get
            after <- elementWithClass(ui, "p-datepicker-day-view")
            _     <- press(after, UI.Keyboard.Escape)
            from  <- start.get
            to    <- end.get
        yield assert(half == Present(day("2026-07-09")) && from.isEmpty && to.isEmpty)
    }

    "the time picker is cancelled with everything else, since it writes the same value" in {
        for
            vref    <- Signal.initRef(Maybe(LocalDateTime.of(2026, 7, 15, 9, 30)))
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-15")
            ui = uic.DatePicker[LocalDateTime]().value(vref).wired(rs, "dp", _ => ())
            up     <- buttonWith(ui, "p-datepicker-increment-button")
            _      <- click(up)
            bumped <- vref.get
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Escape)
            back   <- vref.get
        yield assert(bumped == Present(LocalDateTime.of(2026, 7, 15, 10, 30)) && back == Present(LocalDateTime.of(2026, 7, 15, 9, 30)))
    }

    "a close that commits forgets, so the next Escape has nothing old to put back" in {
        for
            vref    <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", _ => ())
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            picked <- vref.get
            _      <- rs.open.set(true)
            again  <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(again, UI.Keyboard.Escape)
            kept   <- vref.get
        yield assert(picked == Present(day("2026-07-09")) && kept == Present(day("2026-07-09")), "a pick that closed the panel is a commit")
    }

    "an in-flow calendar has nothing to cancel" in {
        for
            set     <- Signal.initRef(Set.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-09")
            ui = uic.DatePicker[LocalDate]().inline(true).values(set).wired(rs, "dp", _ => ())
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(grid, UI.Keyboard.Enter)
            after  <- elementWithClass(ui, "p-datepicker-day-view")
            _      <- press(after, UI.Keyboard.Escape)
            picked <- set.get
            still  <- rs.open.get
        yield assert(picked == Set(day("2026-07-09")) && still, "what is picked in the page is picked")
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
            (rs, _) <- refs(open = false)
            moved   <- Signal.initRef(List.empty[String])
            vref    <- Signal.initRef(Maybe(day(july)))
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
            field  <- elementWithClass(ui, "p-datepicker-input")
            _      <- press(field, UI.Keyboard.ArrowDown)
            opened <- rs.open.get
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
            (rs, _) <- refs(open = false)
            vref    <- Signal.initRef(Maybe(day(july)))
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", _ => ())
            field  <- elementWithClass(ui, "p-datepicker-input")
            _      <- press(field, UI.Keyboard.ArrowDown)
            opened <- rs.open.get
            grid   <- elementWithClass(ui, "p-datepicker-day-view")
        yield
            assert(opened)
            assert(grid.attrs.focusAuto.contains(true), "a focus command would race the insert; this rides with it")
    }

    "and a pointer open marks it too, since the keys are in the grid whoever opened it" in {
        for
            (rs, _) <- refs(open = false)
            vref    <- Signal.initRef(Maybe(day(july)))
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", _ => ())
            field <- elementWithClass(ui, "p-datepicker-input")
            _     <- click(field)
            grid  <- elementWithClass(ui, "p-datepicker-day-view")
        yield assert(grid.attrs.focusAuto.contains(true))
    }

    "nothing is marked before a reader asks for it, or every calendar on a page would fight for the focus" in {
        for
            (rs, _) <- refs()
            vref    <- Signal.initRef(Maybe(day(july)))
            ui = uic.DatePicker[LocalDate]().inline(true).value(vref).wired(rs, "dp", _ => ())
            grid <- elementWithClass(ui, "p-datepicker-day-view")
        yield assert(!grid.attrs.focusAuto.contains(true), "an in-flow calendar is on screen from the first paint")
    }

    "the field keeps Space and Enter, because it is a text box" in {
        for
            (rs, _) <- refs(open = false)
            vref    <- Signal.initRef(Maybe.empty[LocalDate])
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", _ => ())
            field <- elementWithClass(ui, "p-datepicker-input")
            _     <- press(field, UI.Keyboard.Space)
            _     <- press(field, UI.Keyboard.Enter)
            still <- rs.open.get
        yield assert(!still, "neither opens the panel: one types a space, the other belongs to the form")
    }

    "the granular grids are grids: every cell sits in a row" in {
        for
            (_, _, _, ui) <- recording(uic.DatePicker[YearMonth](), Present(YearMonth.of(2026, 7)))
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
            (_, _, moved, ui) <- recording(uic.DatePicker[LocalDate](), Present(day(july)))
            month             <- buttonWith(ui, "p-datepicker-select-month")
            _                 <- click(month)
            got               <- moved.get
        yield assert(got == List("dp-grid"), "the button it was pressed on is not in the header the switch draws")
    }

    "and a drill hands it to the day grid it drilled into" in {
        for
            vref    <- Signal.initRef(Maybe(day(july)))
            (rs, _) <- refs(month = "2026-07", view = DatePickerView.Month)
            moved   <- Signal.initRef(List.empty[String])
            ui = uic.DatePicker[LocalDate]().value(vref).wired(rs, "dp", id => moved.updateAndGet(_ :+ id))
            months <- elementWithClass(ui, "p-datepicker-month-view")
            _      <- press(months, UI.Keyboard.Enter)
            landed <- rs.view.get
            got    <- moved.get
        yield assert(landed == DatePickerView.Date && got == List("dp-grid"))
    }

    "the title buttons switch the grid without a caller binding a view ref either" in {
        for
            (_, _, _, _, _, ui) <- wired(july)
            month               <- buttonWith(ui, "p-datepicker-select-month")
            _                   <- click(month)
            grid                <- elementWithClass(ui, "p-datepicker-month-view")
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

    "a caller's own month ref is typed, and the header writes it as a YearMonth" in {
        for
            shown   <- Signal.initRef(YearMonth.of(2026, 7))
            vref    <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, _) <- refs()
            ui = uic.DatePicker[LocalDate]().value(vref).month(shown).wired(
                rs.copy(month = DatePicker.ShownMonth.Bound(shown)),
                "dp",
                _ => ()
            )
            title <- buttonWith(ui, "p-datepicker-select-month")
            next  <- elementWithClass(ui, "p-datepicker-next-button")
            _     <- click(next)
            moved <- shown.get
        yield
            assert(title.children.exists(_ == UI.Ast.Text("July")), "the bound month decides the page")
            assert(moved == YearMonth.of(2026, 8))
    }

    // === what a pick writes, kind by kind ===============================================================

    "a LocalDate picker writes the day picked, and reports it once" in {
        for
            vref    <- Signal.initRef(Maybe(day(july)))
            seen    <- Signal.initRef(List.empty[Maybe[LocalDate]])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]().value(vref).onChange(v => seen.updateAndGet(_ :+ v).unit).wired(rs, "dp", _ => ())
            fifteen <- dayCell(ui, 15)
            _       <- click(fifteen)
            same    <- seen.get
            _       <- rs.open.set(true)
            twenty  <- dayCell(ui, 20)
            _       <- click(twenty)
            now     <- vref.get
            all     <- seen.get
        yield
            assert(same.isEmpty, "a pick of the day already held is no change, so nothing is reported")
            assert(now == Present(day("2026-07-20")))
            assert(all == List(Present(day("2026-07-20"))))
    }

    "a LocalDateTime picker shows the time picker and keeps the time of day through a day pick" in {
        for
            vref    <- Signal.initRef(Maybe(LocalDateTime.of(2026, 7, 15, 9, 30)))
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDateTime]().value(vref).wired(rs, "dp", _ => ())
            pickers <- elementsWithClass(ui, "p-datepicker-time-picker")
            grids   <- elementsWithClass(ui, "p-datepicker-day-view")
            text    <- fieldText(ui)
            twenty  <- dayCell(ui, 20)
            _       <- click(twenty)
            now     <- vref.get
            open    <- rs.open.get
        yield
            assert(pickers.size == 1 && grids.size == 1, "the day grid and the time picker")
            assert(text == "2026-07-15T09:30")
            assert(now == Present(LocalDateTime.of(2026, 7, 20, 9, 30)))
            assert(open, "the panel stays for the time")
    }

    "a LocalDateTime bound with a time of day clamps the pick on its own day, and the time picker stops at it" in {
        for
            vref    <- Signal.initRef(Maybe(LocalDateTime.of(2026, 7, 20, 8, 0)))
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDateTime]()
                .value(vref)
                .minDate(LocalDateTime.of(2026, 7, 15, 9, 0))
                .wired(rs, "dp", _ => ())
            fourteen <- elementsWithClass(ui, "p-datepicker-day").map(_.find(_.children.exists(_ == UI.Ast.Text("14"))).get)
            fifteen  <- dayCell(ui, 15)
            _        <- click(fifteen)
            clamped  <- vref.get
            down     <- buttonWith(ui, "p-datepicker-decrement-button")
            up       <- buttonWith(ui, "p-datepicker-increment-button")
        yield
            assert(fourteen.attrs.cssClasses.contains("p-disabled"), "the day before the bound is out")
            assert(
                clamped == Present(LocalDateTime.of(2026, 7, 15, 9, 0)),
                "08:00 on the bound's day is before it, so the pick takes the bound"
            )
            assert(down.disabled.contains(true), "and from the bound the hour cannot step back past it")
            assert(!up.disabled.contains(true), "while it can still step forward")
    }

    "a LocalTime picker is the time picker alone, and spins its value" in {
        for
            vref    <- Signal.initRef(Maybe(LocalTime.of(9, 30)))
            seen    <- Signal.initRef(List.empty[Maybe[LocalTime]])
            (rs, _) <- refs()
            ui = uic.DatePicker[LocalTime]().value(vref).onChange(v => seen.updateAndGet(_ :+ v).unit).wired(rs, "dp", _ => ())
            panel <- elementWithClass(ui, "p-datepicker-panel")
            cals  <- elementsWithClass(ui, "p-datepicker-calendar-container")
            minus <- elements(ui).map(_.collect {
                case b: UI.Ast.Button if b.attrs.cssClasses.contains("p-datepicker-decrement-button") => b
            })
            _    <- click(minus(1))
            now  <- vref.get
            all  <- seen.get
            text <- fieldText(ui)
        yield
            assert(panel.attrs.cssClasses.contains("p-datepicker-timeonly") && cals.isEmpty, "no calendar")
            assert(now == Present(LocalTime.of(9, 29)), "the second column is the minute")
            assert(all == List(Present(LocalTime.of(9, 29))))
            assert(text == "09:29")
    }

    "an empty LocalTime picker starts at the earliest time its bounds allow" in {
        for
            vref    <- Signal.initRef(Maybe.empty[LocalTime])
            (rs, _) <- refs()
            ui = uic.DatePicker[LocalTime]().value(vref).minDate(LocalTime.of(8, 0)).maxDate(LocalTime.of(17, 0)).wired(rs, "dp", _ => ())
            hours <- elementWithClass(ui, "p-datepicker-hour-picker")
            ups   <- elements(ui).map(_.collect {
                case b: UI.Ast.Button if b.attrs.cssClasses.contains("p-datepicker-increment-button") => b
            })
            downs <- elements(ui).map(_.collect {
                case b: UI.Ast.Button if b.attrs.cssClasses.contains("p-datepicker-decrement-button") => b
            })
            _   <- click(ups.head)
            now <- vref.get
        yield
            assert(hours.children.exists { case s: UI.Ast.Element => s.children.exists(_ == UI.Ast.Text("08")); case _ => false })
            assert(downs.head.disabled.contains(true), "08:00 is the floor, so the hour cannot step back")
            assert(now == Present(LocalTime.of(9, 0)))
    }

    "a YearMonth picker opens on the month grid and writes the month picked" in {
        for
            (vref, open, _, ui) <- recording(uic.DatePicker[YearMonth](), Present(YearMonth.of(2026, 7)))
            days                <- elementsWithClass(ui, "p-datepicker-day-view")
            months              <- elementsWithClass(ui, "p-datepicker-month")
            selected = months.filter(_.attrs.cssClasses.contains("p-datepicker-month-selected"))
            _    <- click(months(2))
            now  <- vref.get
            shut <- open.get
        yield
            assert(days.isEmpty && months.size == 12)
            assert(selected.size == 1 && selected.head.children.exists(_ == UI.Ast.Text("Jul")))
            assert(now == Present(YearMonth.of(2026, 3)) && !shut)
    }

    "a Year picker opens on the year grid and writes the year picked" in {
        for
            (vref, _, _, ui) <- recording(uic.DatePicker[Year](), Present(Year.of(2026)))
            years            <- elementsWithClass(ui, "p-datepicker-year")
            title            <- elementsWithClass(ui, "p-datepicker-select-month")
            _                <- click(years.find(_.children.exists(_ == UI.Ast.Text("2028"))).get)
            now              <- vref.get
        yield
            assert(years.size == 10 && title.isEmpty, "the decade 2020 to 2029, and no month to choose")
            assert(now == Present(Year.of(2028)))
    }

    "a zoned picker shows the zone's day, and a pick of it keeps the instant" in {
        val berlin = ZoneId.of("Europe/Berlin")
        // 23:30 UTC on the 14th is already the 15th in Berlin.
        val held = ZonedDateTime.ofInstant(Instant.parse("2026-07-14T23:30:00Z"), ZoneId.of("UTC"))
        for
            vref    <- Signal.initRef(Maybe(held))
            seen    <- Signal.initRef(0)
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker(DateKind.zoned(berlin, LocalTime.NOON))
                .value(vref)
                .onChange(_ => seen.updateAndGet(_ + 1).unit)
                .wired(rs, "dp", _ => ())
            text    <- fieldText(ui)
            fifteen <- dayCell(ui, 15)
            _       <- click(fifteen)
            kept    <- vref.get
            fired   <- seen.get
            _       <- rs.open.set(true)
            sixteen <- dayCell(ui, 16)
            _       <- click(sixteen)
            moved   <- vref.get
        yield
            assert(text == "2026-07-15")
            assert(kept == Present(held) && fired == 0, "the same day is no change, so the app's own instant stays")
            assert(moved == Present(ZonedDateTime.of(2026, 7, 16, 12, 0, 0, 0, berlin)), "another day lands at the kind's time in its zone")
        end for
    }

    "values makes a MultiDatePicker: a typed set, and one onChange reporting the set after each real change" in {
        for
            set     <- Signal.initRef(Set(day("2026-07-02")))
            seen    <- Signal.initRef(List.empty[Set[LocalDate]])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]().values(set).onChange(s => seen.updateAndGet(_ :+ s).unit).wired(rs, "dp", _ => ())
            nine <- dayCell(ui, 9)
            _    <- click(nine)
            two  <- dayCell(ui, 2)
            _    <- click(two)
            now  <- set.get
            all  <- seen.get
            text <- fieldText(ui)
        yield
            assert(now == Set(day("2026-07-09")))
            assert(all == List(Set(day("2026-07-02"), day("2026-07-09")), Set(day("2026-07-09"))))
            assert(text == "2026-07-09")
    }

    "range makes a RangeDatePicker: a typed start and end, and one onChange reporting both after a real change" in {
        for
            start   <- Signal.initRef(Maybe.empty[LocalDate])
            end     <- Signal.initRef(Maybe.empty[LocalDate])
            seen    <- Signal.initRef(List.empty[(Maybe[LocalDate], Maybe[LocalDate])])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]()
                .range(start, end)
                .onChange((s, e) => seen.updateAndGet(_ :+ ((s, e))).unit)
                .wired(rs, "dp", _ => ())
            three <- dayCell(ui, 3)
            _     <- click(three)
            eight <- dayCell(ui, 8)
            _     <- click(eight)
            text  <- fieldText(ui)
            shut  <- rs.open.get
            _     <- rs.open.set(true)
            mid   <- elementsWithClass(ui, "p-datepicker-day-selected-range")
            all   <- seen.get
        yield
            assert(all == List[(Maybe[LocalDate], Maybe[LocalDate])](
                (Present(day("2026-07-03")), Absent),
                (Present(day("2026-07-03")), Present(day("2026-07-08")))
            ))
            assert(text == "2026-07-03 - 2026-07-08")
            assert(!shut, "the completing pick closes the panel")
            assert(mid.size == 4, "the 4th to the 7th")
    }

    "a MultiDatePicker reports nothing for a pick that changes nothing, and its blur reports the set" in {
        for
            set     <- Signal.initRef(Set(day("2026-07-02")))
            seen    <- Signal.initRef(0)
            blurs   <- Signal.initRef(List.empty[Set[LocalDate]])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]()
                .values(set)
                .minDate(day("2026-07-05"))
                .showButtonBar(true)
                .onChange(_ => seen.updateAndGet(_ + 1).unit)
                .onBlur(s => blurs.updateAndGet(_ :+ s).unit)
                .wired(rs, "dp", _ => ())
            two   <- elementsWithClass(ui, "p-datepicker-day").map(_.find(_.children.exists(_ == UI.Ast.Text("2"))).get)
            clear <- elementWithClass(ui, "p-datepicker-clear-button")
            _     <- click(clear)
            _     <- rs.open.set(true)
            again <- elementWithClass(ui, "p-datepicker-clear-button")
            _     <- click(again)
            count <- seen.get
            _     <- blur(ui)
            all   <- blurs.get
        yield
            assert(two.attrs.cssClasses.contains("p-disabled"), "the shared bounds reach the set picker")
            assert(count == 1, "the first Clear emptied the set; the second found nothing to clear")
            assert(all == List(Set.empty[LocalDate]))
    }

    "a RangeDatePicker reports nothing for a restart onto the start already held" in {
        for
            start   <- Signal.initRef(Maybe(day("2026-07-03")))
            end     <- Signal.initRef(Maybe.empty[LocalDate])
            seen    <- Signal.initRef(0)
            blurs   <- Signal.initRef(List.empty[(Maybe[LocalDate], Maybe[LocalDate])])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]()
                .range(start, end)
                .onChange((_, _) => seen.updateAndGet(_ + 1).unit)
                .onBlur((s, e) => blurs.updateAndGet(_ :+ ((s, e))).unit)
                .wired(rs, "dp", _ => ())
            one   <- dayCell(ui, 1)
            _     <- click(one)
            moved <- start.get
            count <- seen.get
            _     <- blur(ui)
            all   <- blurs.get
        yield
            assert(moved == Present(day("2026-07-01")) && count == 1, "a pick before the start restarts the range, a real change")
            assert(all == List[(Maybe[LocalDate], Maybe[LocalDate])]((Present(day("2026-07-01")), Absent)))
    }

    "turning a picker into a set or range picker drops the single-value callbacks, which could never fire" in {
        for
            set     <- Signal.initRef(Set.empty[LocalDate])
            single  <- Signal.initRef(0)
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]()
                .onChange(_ => single.updateAndGet(_ + 1).unit)
                .values(set)
                .wired(rs, "dp", _ => ())
            nine  <- dayCell(ui, 9)
            _     <- click(nine)
            now   <- set.get
            count <- single.get
        yield
            assert(now == Set(day("2026-07-09")))
            assert(count == 0, "the set picker has its own onChange; the single one did not come along")
            typeCheckFailure("""
                import kyo.*
                import kyo.uic
                import java.time.LocalDate
                def x(s: SignalRef[Set[LocalDate]]) = uic.DatePicker[LocalDate]().values(s).onChange((v: Maybe[LocalDate]) => ())
            """)
            typeCheckFailure("""
                import kyo.*
                import kyo.uic
                import java.time.LocalDate
                def x(s: SignalRef[Maybe[LocalDate]]) = uic.DatePicker[LocalDate]().range(s, s).value(s)
            """)
    }

    "Clear reports a change only when there was something to clear" in {
        for
            vref    <- Signal.initRef(Maybe.empty[LocalDate])
            seen    <- Signal.initRef(List.empty[Maybe[LocalDate]])
            (rs, _) <- refs(month = "2026-07")
            ui = uic.DatePicker[LocalDate]().value(vref).showButtonBar(true).onChange(v => seen.updateAndGet(_ :+ v).unit).wired(
                rs,
                "dp",
                _ => ()
            )
            clear <- elementWithClass(ui, "p-datepicker-clear-button")
            _     <- click(clear)
            idle  <- seen.get
            _     <- vref.set(Present(day(july)))
            _     <- rs.open.set(true)
            again <- elementWithClass(ui, "p-datepicker-clear-button")
            _     <- click(again)
            all   <- seen.get
            now   <- vref.get
        yield
            assert(idle.isEmpty)
            assert(all == List(Maybe.empty[LocalDate]) && now.isEmpty)
    }

    "Today shows its page and picks the typed value" in {
        for
            vref       <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, mref) <- refs(month = "2026-01")
            ui = uic.DatePicker[LocalDate]().value(vref).showButtonBar(true).today(day("2026-07-16")).wired(rs, "dp", _ => ())
            today <- elementWithClass(ui, "p-datepicker-today-button")
            _     <- click(today)
            now   <- vref.get
            page  <- mref.get
        yield assert(now == Present(day("2026-07-16")) && page == "2026-07")
    }

    "a day the bounds rule out is disabled, and Enter on it picks nothing" in {
        for
            vref    <- Signal.initRef(Maybe.empty[LocalDate])
            (rs, _) <- refs(month = "2026-07", cursor = "2026-07-25")
            ui = uic.DatePicker[LocalDate]().value(vref).minDate(day("2026-07-05")).maxDate(day("2026-07-20")).wired(rs, "dp", _ => ())
            days <- elementsWithClass(ui, "p-datepicker-day")
            grid <- elementWithClass(ui, "p-datepicker-day-view")
            _    <- press(grid, UI.Keyboard.Enter)
            now  <- vref.get
        yield
            // The adjacent months' days are disabled too, but say nothing to a screen reader: they are not choices at all.
            val ruledOut = days.filter(_.attrs.ariaAttrs.get("disabled").contains("true"))
                .map(_.children.collect { case UI.Ast.Text(t) => t }.mkString)
            assert(ruledOut == Chunk(1, 2, 3, 4, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31).map(_.toString))
            assert(now.isEmpty, "the cursor stood on the 25th, past the bound")
    }

    // === the typed text =================================================================================

    "a typed text that reads as a value is written, typed" in {
        for
            (vref, _, _, _, _, ui) <- wired("")
            _                      <- typeInto(ui, " 2026-08-03 ")
            now                    <- vref.get
            text                   <- fieldText(ui)
        yield assert(now == Present(day("2026-08-03")) && text == "2026-08-03")
    }

    "an unreadable text leaves the ref alone, and goes back to the held value on blur" in {
        for
            (vref, _, _, _, _, ui) <- wired(july)
            _                      <- typeInto(ui, "next tuesday")
            kept                   <- vref.get
            shown                  <- fieldText(ui)
            _                      <- blur(ui)
            back                   <- fieldText(ui)
        yield
            assert(kept == Present(day(july)), "nothing that is not a date reaches the typed ref")
            assert(shown == "next tuesday", "the reader's text stays while they are on it")
            assert(back == july, "and the field shows the held value again once they leave")
    }

    "a typed text outside the bounds is not taken either" in {
        for
            vref    <- Signal.initRef(Maybe(day(july)))
            (rs, _) <- refs()
            ui = uic.DatePicker[LocalDate]().value(vref).maxDate(day("2026-12-31")).wired(rs, "dp", _ => ())
            _   <- typeInto(ui, "2027-01-01")
            now <- vref.get
        yield assert(now == Present(day(july)))
    }

    "an emptied field clears the value" in {
        for
            (vref, _, _, _, _, ui) <- wired(july)
            _                      <- typeInto(ui, "")
            now                    <- vref.get
        yield assert(now.isEmpty)
    }

    "a typed text naming the held value writes nothing, so a zoned instant survives it" in {
        val berlin = ZoneId.of("Europe/Berlin")
        val held   = ZonedDateTime.of(2026, 7, 15, 14, 30, 0, 0, berlin)
        for
            vref    <- Signal.initRef(Maybe(held))
            (rs, _) <- refs()
            ui = uic.DatePicker(DateKind.zoned(berlin)).value(vref).wired(rs, "dp", _ => ())
            _   <- typeInto(ui, "2026-07-15")
            now <- vref.get
        yield assert(now == Present(held))
        end for
    }

    "blur reports the typed value held" in {
        for
            vref    <- Signal.initRef(Maybe(YearMonth.of(2026, 7)))
            seen    <- Signal.initRef(List.empty[Maybe[YearMonth]])
            (rs, _) <- refs(view = DatePickerView.Month)
            ui = uic.DatePicker[YearMonth]().value(vref).onBlur(v => seen.updateAndGet(_ :+ v).unit).wired(rs, "dp", _ => ())
            _   <- blur(ui)
            all <- seen.get
        yield assert(all == List(Present(YearMonth.of(2026, 7))))
    }

    // === the kind decides which setters exist ===========================================================

    "a setter a kind cannot honour does not compile" in {
        val pre = "import kyo.*\nimport kyo.uic\nimport java.time.*\n"
        typeCheck(pre + "def x(s: SignalRef[Set[LocalDate]]) = uic.DatePicker[LocalDate]().values(s)")
        typeCheckFailure(pre + "def x(s: SignalRef[Set[LocalTime]]) = uic.DatePicker[LocalTime]().values(s)")
        typeCheckFailure(pre + "def x(s: SignalRef[Set[LocalDateTime]]) = uic.DatePicker[LocalDateTime]().values(s)")
        typeCheckFailure(pre + "def x(s: SignalRef[Maybe[YearMonth]]) = uic.DatePicker[YearMonth]().range(s, s)")
        typeCheckFailure(pre + "def x = uic.DatePicker[LocalDate]().hourFormat(uic.HourFormat.H12)")
        typeCheck(pre + "def x = uic.DatePicker[LocalTime]().hourFormat(uic.HourFormat.H12)")
        typeCheckFailure(pre + "def x(m: SignalRef[YearMonth]) = uic.DatePicker[LocalTime]().month(m)")
        typeCheckFailure(pre + "def x = uic.DatePicker[LocalTime]().referenceDate(LocalTime.NOON)")
        typeCheckFailure(pre + "def x(v: SignalRef[uic.DatePickerView]) = uic.DatePicker[Year]().currentView(v)")
        typeCheckFailure(pre + "def x = uic.DatePicker[YearMonth]().showWeek(true)")
        typeCheckFailure(pre + "def x = uic.DatePicker[String]()")
    }

end DatePickerTest
