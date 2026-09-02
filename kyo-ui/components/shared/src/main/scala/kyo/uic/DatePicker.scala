package kyo.uic

import kyo.*
import kyo.UI.*

/** DatePicker — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DatePicker anatomy: `div.p-datepicker.p-component` >
  * `input.p-datepicker-input.p-inputtext` + `button.p-datepicker-dropdown` with
  * the calendar glyph, over the floating `div.p-datepicker-panel` — or the
  * `.p-datepicker-panel-inline` variant under `inline(true)` — >
  * `div.p-datepicker-calendar-container` > `div.p-datepicker-calendar` with
  * Prime's header (prev / month+year title buttons / next) and the
  * `table.p-datepicker-day-view` day grid — or the `.p-datepicker-month-view` /
  * `.p-datepicker-year-view` grids, the `.p-datepicker-time-picker`, and the
  * `.p-datepicker-buttonbar` as configured), so the extracted `@primeuix`
  * datepicker CSS applies verbatim.
  *
  * The calendar is the REAL floating panel, built on the [[Overlay]] primitive
  * with `seedFocus(false)` — focus stays on the FIELD (the panel's buttons stay
  * reachable by Tab/click). Clicking the field or the calendar dropdown button
  * opens it (Prime's `showOnFocus` + `showIcon` patterns); a completed pick
  * closes it in single mode (multiple/range keep it open per Prime), and
  * Escape (on the field or inside the panel) or an outside click closes.
  *
  * Keyboard (the ARIA date-grid pattern, over the pure [[CalendarNav]] machine):
  * ArrowDown on the field opens the calendar, and a second one steps into the
  * grid. The grid is ONE tab stop with a roving `.p-focus` highlight inside it,
  * announced through `aria-activedescendant` — a day is a `<span>` rather than a
  * control of its own, which is the same reason [[Listbox]] and [[Tree]] rove a
  * highlight instead of a tabindex. The arrows step a day and a week, crossing
  * whatever boundary that implies and carrying the displayed month with them;
  * Home and End reach the ends of the WEEK; PageUp and PageDown turn a month and
  * with Shift a year; Enter or Space picks the highlighted day; Escape closes and
  * hands focus back to the field. The month and year grids answer the same keys in
  * their own unit. Space and Enter are deliberately NOT opening keys on the field:
  * it is a text box, so one types a space and the other belongs to the form.
  * `open(ref)` optionally binds the visibility (self-managed via an effectful
  * `UI.mounted` region otherwise — static projections render the closed anatomy
  * inert). `inline(true)` renders the panel in-flow below the field instead
  * (Prime's `inline` prop): always visible without an `open` ref, toggled by
  * the dropdown button with one.
  *
  * The value model is a deliberate deviation from Prime's `Date` objects and
  * format strings: everything binds ISO-prefix strings two-way, keeping the
  * shared JVM+JS render pure — it never reads `Date.now()`. Selection modes are
  * carried by the binding overloads: `value` (single, `YYYY-MM-DD`, or with
  * [[showTime]] `YYYY-MM-DDTHH:MM`, with [[timeOnly]] `HH:MM`, with
  * `view(Month)` `YYYY-MM`, with `view(Year)` `YYYY`), `values`
  * (multiple, a `Set` of ISO dates), and `range` (a start/end pair of ISO
  * dates). The displayed month derives from an optional `month` ref
  * (`"YYYY-MM"`, shifted by the prev/next header buttons), falling back to the
  * bound value, then the caller-supplied `referenceDate`, then a fixed epoch.
  * The month/year drill-down (clicking the title buttons) needs somewhere
  * server-honest to keep the CURRENT VIEW — bind it with `currentView(ref)`
  * (the `month`-ref pattern); without it the title buttons render disabled.
  * Because the render is pure there is also no "today" highlight, and the
  * button bar's Today button renders only when the app supplies `today(iso)`
  * explicitly. All date math is hand-rolled integer arithmetic (no `java.time`,
  * which Scala.js lacks by default).
  */
final case class DatePicker private (
    valueBinding: Maybe[Input.Value] = Absent,
    multiRef: Maybe[SignalRef[Set[String]]] = Absent,
    rangeRefsV: Maybe[(SignalRef[String], SignalRef[String])] = Absent,
    monthRef: Maybe[SignalRef[String]] = Absent,
    currentViewRefV: Maybe[SignalRef[DatePickerView]] = Absent,
    viewV: DatePickerView = DatePickerView.Date,
    inlineFlag: Boolean = false,
    showTimeFlag: Boolean = false,
    timeOnlyFlag: Boolean = false,
    hourFormatV: HourFormat = HourFormat.H24,
    showButtonBarFlag: Boolean = false,
    todayV: Maybe[String] = Absent,
    placeholderText: Maybe[TextValue] = Absent,
    disabledFlag: Boolean = false,
    readonlyFlag: Boolean = false,
    requiredFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    minDateV: Maybe[String] = Absent,
    maxDateV: Maybe[String] = Absent,
    showWeekFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    openRef: Maybe[SignalRef[Boolean]] = Absent,
    referenceDateV: Maybe[String] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onOpenF: Maybe[() => Any < Async] = Absent,
    onCloseF: Maybe[() => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl, HasPlaceholder, HasAccessibleNameRef:
    type Self = DatePicker

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): DatePicker = copy(idV = v)

    /** Sets a constant ISO value (`YYYY-MM-DD`; `YYYY-MM` / `YYYY` under
      * `view(Month|Year)`; datetime/time under `showTime`/`timeOnly`).
      */
    def value(v: String): DatePicker = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref` (single selection): picking writes back the ISO string. */
    def value(ref: SignalRef[String]): DatePicker = copy(valueBinding = Present(Input.Value.Ref(ref)))

    /** Multiple selection (Prime's `selectionMode="multiple"`): binds the picked
      * ISO dates two-way as a Set — a day click toggles membership, the panel
      * stays open (the `Listbox.selected` / `SelectButton.values` collection
      * pattern). Wins over [[value]].
      */
    def values(ref: SignalRef[Set[String]]): DatePicker = copy(multiRef = Present(ref))

    /** Range selection (Prime's `selectionMode="range"`): binds the start/end ISO
      * dates two-way. The first pick writes `start` (clearing `end`), a pick on
      * or after it completes the range and closes the panel, an earlier pick
      * restarts. In-range days render Prime's `.p-datepicker-day-selected-range`.
      * Wins over [[value]] and [[values]].
      */
    def range(start: SignalRef[String], end: SignalRef[String]): DatePicker =
        copy(rangeRefsV = Present((start, end)))

    /** Binds the DISPLAYED month two-way to `ref` (`"YYYY-MM"`): the header's
      * prev/next buttons shift it by pure integer math (by month, year, or decade
      * depending on the current view), and so do the page keys and any arrow that
      * steps out of the month on the screen. Bind it only to READ or drive the
      * month from outside; the mount mints one when you do not, so navigating the
      * calendar is not something a caller has to opt into. An empty ref value
      * falls back to the value-derived month.
      */
    def month(ref: SignalRef[String]): DatePicker = copy(monthRef = Present(ref))

    /** Renders the calendar panel IN-FLOW below the field instead of floating
      * (Prime's `inline` prop, `.p-datepicker-panel-inline`): always visible
      * without an [[open]] ref, toggled by the dropdown button with one.
      */
    def inline(v: Boolean): DatePicker = copy(inlineFlag = v)

    /** Picking granularity (Prime's `view`): `Month` starts in the month grid and
      * picks `YYYY-MM` values, `Year` starts in the year grid and picks `YYYY`.
      * Default `Date`.
      */
    def view(v: DatePickerView): DatePicker = copy(viewV = v)

    /** Binds the CURRENT VIEW two-way to `ref` — kyo's server-honest replacement
      * for Prime's internal `currentView` state (the `month`-ref pattern):
      * clicking the title's month/year buttons drills into the month/year grids,
      * picking there writes the `month` ref and drills back. Initialize the ref
      * with the [[view]] granularity; without this binding the title buttons
      * render disabled.
      */
    def currentView(ref: SignalRef[DatePickerView]): DatePicker = copy(currentViewRefV = Present(ref))

    /** Adds Prime's time picker (`.p-datepicker-time-picker`) below the calendar;
      * the single-selection value model extends to `YYYY-MM-DDTHH:MM` and a day
      * pick no longer closes the panel (Prime keeps it open for the time). The
      * hour/minute buttons render disabled while no date is picked (the pure
      * render has no clock to default to).
      */
    def showTime(v: Boolean): DatePicker = copy(showTimeFlag = v)

    /** Time-only mode (Prime's `timeOnly`): no calendar, just the time picker;
      * the value model is `HH:MM` (an empty value edits from 00:00).
      */
    def timeOnly(v: Boolean): DatePicker = copy(timeOnlyFlag = v)

    /** Hour display format: `H12` shows 12-hour hours plus Prime's AM/PM picker
      * (the value model stays 24-hour ISO). Default `H24`.
      */
    def hourFormat(v: HourFormat): DatePicker = copy(hourFormatV = v)

    /** Renders Prime's button bar (`.p-datepicker-buttonbar`) with Today + Clear.
      * Clear empties the bound selection and closes the panel. Today needs a date
      * and the render is pure (no clock), so the Today button renders only when
      * the app supplies [[today]] explicitly.
      */
    def showButtonBar(v: Boolean): DatePicker = copy(showButtonBarFlag = v)

    /** The app-supplied "today" (`YYYY-MM-DD`) for the button bar's Today button —
      * clicking it navigates the displayed month there and picks it.
      */
    def today(iso: String): DatePicker = copy(todayV = Present(iso))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): DatePicker = copy(placeholderText = v)

    def disabled(v: Boolean): DatePicker = copy(disabledFlag = v)

    /** Native `readonly` on the field; the calendar cannot be opened via the
      * dropdown button either.
      */
    def readonly(v: Boolean): DatePicker = copy(readonlyFlag = v)

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): DatePicker = copy(requiredFlag = v)

    /** Native `name` for HTML form participation. */
    def name(v: String): DatePicker = copy(nameV = Present(v))

    /** Earliest selectable date (ISO `YYYY-MM-DD`) — earlier day/month/year cells
      * render disabled.
      */
    def minDate(iso: String): DatePicker = copy(minDateV = Present(iso))

    /** Latest selectable date (ISO `YYYY-MM-DD`) — later day/month/year cells
      * render disabled.
      */
    def maxDate(iso: String): DatePicker = copy(maxDateV = Present(iso))

    /** Shows the ISO 8601 week-number column (`td.p-datepicker-weeknumber`; hidden
      * by default, matching Prime's `showWeek=false`).
      */
    def showWeek(v: Boolean): DatePicker = copy(showWeekFlag = v)

    /** Size of the field: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): DatePicker = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): DatePicker = copy(variantV = v)

    /** Spans the full width of its container (`.p-datepicker-fluid`). */
    def fluid(v: Boolean): DatePicker = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): DatePicker                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): DatePicker                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): DatePicker = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): DatePicker = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): DatePicker = copy(accNameRefV = v)

    /** Binds the calendar's visibility two-way to `ref` (as [[Dialog.open]] does). */
    def open(ref: SignalRef[Boolean]): DatePicker = copy(openRef = Present(ref))

    /** The month to display when neither a `month` ref nor a value is bound (ISO
      * `YYYY-MM-DD`) — keeps the render pure.
      */
    def referenceDate(iso: String): DatePicker = copy(referenceDateV = Present(iso))

    /** Fired with the picked/toggled ISO string after every pick's ref write. */
    def onChange(f: String => Any < Async): DatePicker = copy(onChangeF = Present(f))

    /** Fired when the calendar transitions closed → open (internal path: the
      * dropdown button). An external `openRef.set` from app code is not intercepted.
      */
    def onOpen(f: => Any < Async): DatePicker = copy(onOpenF = Present(() => f))

    /** Fired when the calendar transitions open → closed (internal paths: the
      * dropdown button, a completing pick, the button bar). External `openRef.set`
      * writes are not intercepted.
      */
    def onClose(f: => Any < Async): DatePicker = copy(onCloseF = Present(() => f))

    /** Fires on focus loss (native `blur`) with the field's current date string —
      * unlike `onChange` it fires even when the value was not edited. The
      * form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): DatePicker = copy(onBlurF = Present(f))

    // === render =============================================================

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

    /** The keyboard state the mount allocates, and the ids it addresses cells by.
      *
      * `monthOv` is the displayed month. A caller's own `month(ref)` wins; without one a minted
      * ref stands in, holding `""` until something moves the month. That is what lets the header
      * buttons and the arrows move it at all: before, an unbound `month` left them disabled and
      * the calendar frozen on one month.
      *
      * `cursor` is the cell the keyboard is on, as an ISO string at the view's own precision.
      * Cell ids are POSITIONAL (`base-c<n>`), not date-keyed, so a month change patches the cells
      * that are already in the document instead of inserting new ones, and moving focus into the
      * new month cannot race the render that draws it.
      */
    private[uic] def render(using Frame): UI =
        // The static projection (SSG, the SSR page GET) is the same anatomy, inert: an inline
        // calendar still draws its month, a floating one still draws its closed field.
        val stat: UI =
            if inlineFlag then
                val panel = openRef match
                    case Present(o) => when(o)(calendarReactive(Absent, Absent, inline = true))
                    case Absent     => calendarReactive(Absent, Absent, inline = true)
                assemble(openRef, Absent, floating = false, panel)
            else assemble(Absent, Absent, floating = true, UI.empty)
        if !interactive then stat
        else
            UI.mounted {
                for
                    cmds <- UI.commands
                    base <- cmds.freshId
                    oref <- openRef match
                        case Present(r) => Kyo.lift(r)
                        case Absent     => Signal.initRef(inlineFlag)
                    mref <- monthRef match
                        case Present(r) => Kyo.lift(r)
                        case Absent     => Signal.initRef("")
                    vref <- currentViewRefV match
                        case Present(r) => Kyo.lift(r)
                        case Absent     => Signal.initRef(viewV)
                    cur     <- Signal.initRef("")
                    seed    <- Signal.initRef(false)
                    restore <- Signal.initRef(Maybe.empty[DatePicker.Restore])
                    refs = DatePicker.Refs(oref, mref, cur, vref, seed, restore)
                yield wired(refs, base, id => cmds.focusId(id))
            }.placeholder(stat)
        end if
    end render

    /** The host the mount publishes — the seam golden tests render directly (a full top-down
      * re-render shows mounted regions as placeholders).
      *
      * The FIELD is built once, outside the open ref's subscription: only the panel and the
      * root's anchor class follow it. That is what makes the focus return on Escape safe, since
      * the element focus goes back to is never the one a close replaces.
      */
    private[uic] def wired(refs: DatePicker.Refs, base: String, focus: String => Any < Async)(using Frame): UI =
        // The mount mints a view ref when the caller binds none, the way it mints the month one.
        // Without it the month and year buttons rendered disabled and a drill-down had nothing to
        // write, which left the two granular grids unreachable in the default configuration.
        copy(currentViewRefV = Present(refs.view)).wiredWith(refs, base, focus)

    private def wiredWith(refs: DatePicker.Refs, base: String, focus: String => Any < Async)(using Frame): UI =
        val oref   = refs.open
        val mref   = refs.month
        val cursor = refs.cursor
        val seed   = refs.seed
        def nav(mv: String, cv: String, sd: Boolean): DatePicker.Nav =
            DatePicker.Nav(mref, mv, cursor, cv, seed, sd, refs.restore, base, focus)
        // The cursor and the seed flag are read together rather than through a second nested
        // render: two regions over the same grid would leave the inner one drawing against the
        // cursor it closed over.
        val body: UI =
            mref.render(mv =>
                cursor.combineLatest(seed).render { pair =>
                    val (cv, sd) = pair
                    calendarReactive(Present(oref), Present(nav(mv, cv, sd)), inline = inlineFlag)
                }
            )
        val panel: UI =
            if inlineFlag then when(oref)(body)
            else Overlay(oref).seedFocus(false)(body).render
        assemble(Present(oref), Present(nav("", "", false)), floating = !inlineFlag, panel, openSignal = Present(oref))
    end wiredWith

    /** Field + dropdown button + panel host inside the `.p-datepicker` root. */
    private def assemble(
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav],
        floating: Boolean,
        panel: UI,
        openSignal: Maybe[SignalRef[Boolean]] = Absent
    )(using Frame): UI =
        // Reactive-invalid gate (BELOW every UI.mounted node — assemble runs inside
        // the open ref's subscription / at the static projection, never around the
        // mount): with a reactive slot set, re-render field + root + message through
        // the shared helper; otherwise the untouched static form.
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => assembleStatic(oref, nav, floating, panel, openSignal)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                        .assembleStatic(oref, nav, floating, panel, openSignal)
                )

    private def assembleStatic(
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav],
        floating: Boolean,
        panel: UI,
        openSignal: Maybe[SignalRef[Boolean]]
    )(using Frame): UI =
        val field: UI = rangeRefsV match
            case Present((s, e)) =>
                s.render(sv => e.render(ev => fieldUI(Present(rangeText(sv, ev)), oref, nav, floating)))
            case Absent =>
                multiRef match
                    case Present(mref) =>
                        mref.render(set => fieldUI(Present(set.toList.sorted.mkString(", ")), oref, nav, floating))
                    case Absent => fieldUI(Absent, oref, nav, floating)

        // The dropdown button toggles the calendar, firing onOpen/onClose on
        // actual transitions. Inline without an open ref keeps it disabled (the
        // always-on panel has nothing to toggle); the floating static projection
        // renders it enabled-looking but inert until the mount wires it.
        var dd = button
            .cssClass("p-datepicker-dropdown")
            .jsProp("type", "button")
            .aria("label", "Choose date")
        if !interactive || (!floating && oref.isEmpty) then dd = dd.disabled(true)
        else oref.foreach(r => dd = dd.onClick(seedGrid(nav).andThen(togglePanel(r))))
        val dropdown: UI = dd(toChild(GlyphSvg(Icons.calendar, "p-icon")))

        var el = div.cssClass("p-datepicker").cssClass("p-component")
        if invalidV.constTrue then el = el.cssClass("p-invalid")
        if disabledFlag then el = el.cssClass("p-disabled")
        if fluidFlag then el = el.cssClass("p-datepicker-fluid")
        // Reactive rather than a render-time branch: the anchor class was the only thing the
        // field's own subtree needed the open state for, and re-rendering the field on every open
        // is what made the focus return on Escape a race.
        openSignal match
            case Present(sig) if floating => el = el.cssClass("p-uic-overlay-anchor", sig)
            case _                        => ()
        val root = el(toChild(field), toChild(dropdown), toChild(panel))
        FieldInvalid.withMessage(root, invalidV.constTrue, invalidMsgV)
    end assembleStatic

    /** The text field. Single selection binds two-way; multiple/range supply a
      * derived DISPLAY text (`displayOverride`) — the field is not parsed back.
      * In the floating host a click on the field opens the panel (Prime's
      * `showOnFocus` default) and Escape closes it.
      */
    private def fieldUI(
        displayOverride: Maybe[String],
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav],
        floating: Boolean
    )(using Frame): UI =
        var i = input.cssClass("p-datepicker-input").cssClass("p-inputtext").cssClass("p-component")
        i = idV.map(v => i.id(v)).getOrElse(i)
        sizeV match
            case Size.Small  => i = i.cssClass("p-inputtext-sm")
            case Size.Large  => i = i.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then i = i.cssClass("p-variant-filled")
        if invalidV.constTrue then i = i.aria("invalid", "true")
        displayOverride match
            case Present(text) => i = i.value(text)
            case Absent =>
                valueBinding match
                    case Present(Input.Value.Const(v)) => i = i.value(v)
                    case Present(Input.Value.Ref(r))   => i = i.value(r)
                    case Absent                        => ()
        end match
        i = placeholderText match
            case Present(TextValue.Const(v)) => i.placeholder(v)
            case Present(TextValue.Dyn(sig)) => i.placeholder(sig)
            case Absent                      => i
        if disabledFlag then i = i.disabled(true)
        if readonlyFlag || displayOverride.isDefined then i = i.readOnly(true)
        if requiredFlag then i = i.aria("required", "true").jsProp("required", "true")
        i = nameV.map(v => i.jsProp("name", v)).getOrElse(i)
        i = accNameV match
            case Present(TextValue.Const(v)) => i.aria("label", v)
            case Present(TextValue.Dyn(s))   => i.aria("label", s)
            case Absent                      => i
        i = accNameRefV.map(v => i.aria("labelledby", v)).getOrElse(i)
        if displayOverride.isEmpty then i = onChangeF.map(f => i.onChange(f)).getOrElse(i)
        onBlurF.foreach { f =>
            // onBlur carries no payload, so the current value is read from the bound ref
            // (the derived display text for the multiple/range modes, which aren't parsed back).
            val eff = displayOverride match
                case Present(text) => f(text)
                case Absent =>
                    valueBinding match
                        case Present(Input.Value.Ref(r))   => r.use(f)
                        case Present(Input.Value.Const(v)) => f(v)
                        case Absent                        => f("")
            i = i.onBlur(eff)
        }
        nav.foreach(n => i = i.id(n.fieldId))
        if floating && interactive then
            oref.foreach { r =>
                // Opening puts the reader in the grid, however the panel was opened: that is where
                // the calendar's keys are, and a panel nobody is standing in answers none of them.
                // The cost is the text field: while the panel is open the caret is not in it, so
                // typing a date by hand means Escape (or Shift+Tab) first.
                i = i.onClick(seedGrid(nav).andThen(openPanel(r)))
                i = i.onKeyDown { e =>
                    // ArrowDown opens, and nothing else does. This is a TEXT BOX, not one of the
                    // library's four div-shaped select triggers: Space types a space and Enter
                    // belongs to the form, so taking either of them here would cost the reader a
                    // key they need for what the field is actually for. Once open, ArrowDown a
                    // second time steps into the grid, which is where the calendar keyboard is.
                    val eff: Any < Async = e.key match
                        case Keyboard.Escape    => cancelVia(Present(r), nav)
                        case Keyboard.ArrowDown => enterGrid(r, nav)
                        case _                  => ()
                    eff
                }
            }
        end if
        i
    end fieldUI

    /** ArrowDown on the field: opens the calendar and puts the reader in the grid.
      *
      * A focus COMMAND cannot do the opening half of that, since the grid does not exist until the
      * panel is rendered and the command would race the insert. So the press raises the seed flag
      * instead, the grid renders `focusAuto` while it is up, and the client focuses the grid in
      * the same patch that inserts it. The flag is declarative for exactly that reason, and it is
      * lowered by every pointer open, where the reader wants the text field they clicked.
      */
    private def enterGrid(oref: SignalRef[Boolean], nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        for
            _      <- seedGrid(nav)
            isOpen <- oref.get
            _ <-
                if !isOpen then openPanel(oref)
                else
                    nav match
                        case Present(n) => n.focus(n.gridId)
                        case Absent     => (): Any < Async
        yield ()

    /** Raises the flag that says the next grid to appear takes the focus.
      *
      * It starts down and is never lowered: what it keeps out is the page's own first paint,
      * where an in-flow calendar would otherwise take the focus off the document the moment it
      * loaded. Everything after that is a reader asking to be in the calendar.
      */
    private def seedGrid(nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        nav match
            case Present(n) => n.seed.set(true)
            case Absent     => ()

    private def rangeText(s: String, e: String): String =
        if s.isEmpty then "" else if e.isEmpty then s else s"$s - $e"

    /** Toggles the panel via the bound ref, firing onOpen/onClose on the transition. */
    private def togglePanel(oref: SignalRef[Boolean])(using Frame): Any < Async =
        for
            wasOpen <- oref.getAndUpdate(!_)
            _       <- if wasOpen then fire(onCloseF) else fire(onOpenF)
        yield ()

    /** Opens the panel (firing `onOpen` on an actual transition). */
    private def openPanel(oref: SignalRef[Boolean])(using Frame): Any < Async =
        for
            wasOpen <- oref.getAndSet(true)
            r       <- if wasOpen then (): Any < Async else fire(onOpenF)
        yield r

    private def fire(f: Maybe[() => Any < Async])(using Frame): Any < Async =
        f match
            case Present(g) => g()
            case Absent     => ()

    private def fireChange(iso: String)(using Frame): Any < Async =
        onChangeF match
            case Present(f) => f(iso)
            case Absent     => ()

    /** Closes the panel through the EFFECTIVE open ref (bound or mount-allocated),
      * firing `onClose` on an actual transition.
      */
    private def closeVia(oref: Maybe[SignalRef[Boolean]])(using Frame): Any < Async =
        oref match
            case Present(r) =>
                for
                    wasOpen <- r.getAndSet(false)
                    res     <- if wasOpen then fire(onCloseF) else (): Any < Async
                yield res
            case Absent => ()

    /** Closes the panel and hands the focus back to the field.
      *
      * A pick destroys what the reader is standing on: the grid they arrowed through, or the
      * button bar's button they pressed. Without the return the focus falls to the document, and
      * the next Tab starts at the top of the page instead of after the field. Plain [[closeVia]]
      * is for the closes whose focus is already outside the panel (the field's own Escape) or
      * belongs where the pointer left it.
      *
      * An in-flow calendar is left alone entirely. It is the page's own furniture rather than a
      * panel over it: a pick is not a reason to take it away, an Escape has no panel to dismiss,
      * and the grid the reader is standing on stays, so there is nothing to hand back either.
      * The `open` ref an inline caller binds is their toggle, not the pick's.
      */
    private def closeAndReturn(oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        if inlineFlag then ()
        else
            for
                _ <- closeVia(oref)
                _ <- forget(nav)
                r <- nav match
                    case Present(n) => n.focus(n.fieldId)
                    case Absent     => (): Any < Async
            yield r

    /** Records the values the panel holds, unless this open already recorded them.
      *
      * Every write inside the panel goes through here first, which is what makes Escape a cancel:
      * the memory is taken before the first change rather than at the open, so it does not matter
      * whether the reader opened the panel, or a caller wrote the open ref they bound.
      */
    private def remember(nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        nav match
            case Absent => ()
            case Present(n) =>
                n.restore.use {
                    case Present(_) => (): Any < Async
                    case Absent =>
                        for
                            single <- valueBinding match
                                case Present(Input.Value.Ref(r))   => r.get
                                case Present(Input.Value.Const(v)) => Kyo.lift(v)
                                case Absent                        => Kyo.lift("")
                            multi <- multiRef match
                                case Present(r) => r.get
                                case Absent     => Kyo.lift(Set.empty[String])
                            start <- rangeRefsV match
                                case Present((r, _)) => r.get
                                case Absent          => Kyo.lift("")
                            end <- rangeRefsV match
                                case Present((_, r)) => r.get
                                case Absent          => Kyo.lift("")
                            r <- n.restore.set(Present(DatePicker.Restore(single, multi, start, end)))
                        yield r
                }

    /** Drops the memory, which a close that commits has no more use for. */
    private def forget(nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        nav match
            case Present(n) => n.restore.set(Absent)
            case Absent     => ()

    /** Escape: put back what the panel held before the reader changed anything in it, then close
      * and hand the focus back. The refs carry the restored values, and `onChange` stays silent,
      * because over the whole open nothing changed.
      *
      * An in-flow calendar has nothing to cancel: it does not close, and what is picked in it is
      * picked in the page rather than in a panel over it.
      */
    private def cancelVia(oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        if inlineFlag then ()
        else
            val undo: Any < Async = nav match
                case Absent => ()
                case Present(n) =>
                    n.restore.use {
                        case Present(rs) => writeBack(rs)
                        case Absent      => (): Any < Async
                    }
            undo.andThen(closeAndReturn(oref, nav))

    /** Puts a [[DatePicker.Restore]] back into whichever refs this picker binds. */
    private def writeBack(rs: DatePicker.Restore)(using Frame): Any < Async =
        val single: Any < Async = valueBinding match
            case Present(Input.Value.Ref(ref)) => ref.set(rs.single)
            case _                             => ()
        val multi: Any < Async = multiRef match
            case Present(ref) => ref.set(rs.multi)
            case Absent       => ()
        val range: Any < Async = rangeRefsV match
            case Present((s, e)) => s.set(rs.start).andThen(e.set(rs.end))
            case Absent          => ()
        single.andThen(multi).andThen(range)
    end writeBack

    // === reactive panel =====================================================

    /** The resolved selection at render time. */
    private enum Snapshot:
        case Single(value: String)
        case Multi(values: Set[String])
        case Range(start: String, end: String)
    end Snapshot

    /** The calendar, reacting to the bound selection, month ref, and view ref.
      * `oref` is the EFFECTIVE open ref picks close through; `inline` picks the
      * panel variant class.
      */
    private def calendarReactive(oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav], inline: Boolean)(using Frame): UI =
        withView { view =>
            withMonthOverride(nav) { mOv =>
                withSelection(snap => panelUI(view, mOv, snap, oref, nav, inline))
            }
        }

    private def withView(f: DatePickerView => UI)(using Frame): UI =
        currentViewRefV match
            case Present(ref) => ref.render(f)
            case Absent       => f(viewV)

    /** The displayed month. The mount's own ref is already resolved into [[Nav]] (and rendered
      * one level up), so this only has to turn its empty state back into "no override"; without a
      * mount a caller's own `month(ref)` is the only source there is.
      */
    private def withMonthOverride(nav: Maybe[DatePicker.Nav])(f: Maybe[String] => UI)(using Frame): UI =
        nav match
            case Present(n) => f(if n.monthOvV.isEmpty then Absent else Present(n.monthOvV))
            case Absent =>
                monthRef match
                    case Present(ref) => ref.render(m => f(Present(m)))
                    case Absent       => f(Absent)

    private def withSelection(f: Snapshot => UI)(using Frame): UI =
        rangeRefsV match
            case Present((s, e)) => s.render(sv => e.render(ev => f(Snapshot.Range(sv, ev))))
            case Absent =>
                multiRef match
                    case Present(mref) => mref.render(set => f(Snapshot.Multi(set)))
                    case Absent =>
                        valueBinding match
                            case Present(Input.Value.Ref(ref)) => ref.render(v => f(Snapshot.Single(v)))
                            case Present(Input.Value.Const(v)) => f(Snapshot.Single(v))
                            case Absent                        => f(Snapshot.Single(""))

    /** The value the displayed month derives from (before override/referenceDate). */
    private def anchorValue(snap: Snapshot): String = snap match
        case Snapshot.Single(v)   => v
        case Snapshot.Multi(set)  => set.maxOption.getOrElse("")
        case Snapshot.Range(s, _) => s

    /** Builds the panel for the derived month and current view — the SAME render
      * for both hosts; only the variant class differs (`-inline` in-flow vs the
      * bare floating panel inside the Overlay).
      */
    private def panelUI(
        view: DatePickerView,
        monthOverride: Maybe[String],
        snap: Snapshot,
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav],
        inline: Boolean
    )(using Frame): UI =
        val (year, month) = deriveMonth(anchorValue(snap), monthOverride)

        val calendarChildren: List[UI] =
            if timeOnlyFlag then Nil
            else
                val grid: List[UI] = view match
                    case DatePickerView.Date => List(dayView(year, month, snap, oref, nav))
                    case _                   => Nil
                val calendar: UI =
                    div.cssClass("p-datepicker-calendar-container")(
                        toChild(
                            div.cssClass("p-datepicker-calendar")(
                                ((headerUI(view, year, month, nav) :: grid)).map(toChild)*
                            )
                        )
                    )
                val monthGrid: List[UI] =
                    if view == DatePickerView.Month then List(monthView(year, snap, oref, nav)) else Nil
                val yearGrid: List[UI] =
                    if view == DatePickerView.Year then List(yearView(year, month, snap, oref, nav)) else Nil
                calendar :: (monthGrid ++ yearGrid)

        val timeChildren: List[UI] =
            if timeOnlyFlag || (showTimeFlag && view == DatePickerView.Date) then List(timePickerUI(snap, nav))
            else Nil

        val barChildren: List[UI] = if showButtonBarFlag then List(buttonBarUI(snap, oref, nav)) else Nil

        var panel = div
            .cssClass("p-datepicker-panel")
            .cssClass("p-component")
        if inline then panel = panel.cssClass("p-datepicker-panel-inline")
        if timeOnlyFlag then panel = panel.cssClass("p-datepicker-timeonly")
        panel((calendarChildren ++ timeChildren ++ barChildren).map(toChild)*)
    end panelUI

    /** Prime's calendar header: prev button, the month/year title BUTTONS (they drill into the
      * month and year grids through the view ref), next button. The nav buttons shift the month
      * ref by month, year or decade depending on the view.
      *
      * The mount mints both refs when the caller binds none, so every button here is live in a
      * mounted picker. They render disabled only in the static projection, which is inert
      * throughout: a title button that cannot be pressed would leave the two granular grids, and
      * the keyboard in them, unreachable.
      */
    private def headerUI(view: DatePickerView, year: Int, month: Int, nav: Maybe[DatePicker.Nav])(using Frame): UI =
        def navButton(cls: String, glyph: IconGlyph, target: (Int, Int), label: String): UI =
            var b = button
                .cssClass(cls)
                .cssClass("p-button")
                .cssClass("p-component")
                .cssClass("p-button-icon-only")
                .cssClass("p-button-secondary")
                .cssClass("p-button-rounded")
                .cssClass("p-button-text")
                .jsProp("type", "button")
                .aria("label", label)
            // The mount mints a month ref when the caller binds none, so these are live by
            // default. They were disabled without one, which left the calendar frozen on the
            // month it derived and gave the arrows nowhere to page to.
            val mv: Maybe[SignalRef[String]] = nav.map(_.monthOv).orElse(monthRef)
            mv match
                case Present(mref) =>
                    val iso = DatePicker.pad(target._1, 4) + "-" + DatePicker.pad(target._2, 2)
                    b = b.onClick(mref.set(iso))
                case Absent => b = b.disabled(true)
            end match
            b(toChild(GlyphSvg(glyph, "p-icon")))
        end navButton

        val (prevTarget, nextTarget) = view match
            case DatePickerView.Date =>
                (
                    if month == 1 then (year - 1, 12) else (year, month - 1),
                    if month == 12 then (year + 1, 1) else (year, month + 1)
                )
            case DatePickerView.Month => ((year - 1, month), (year + 1, month))
            case DatePickerView.Year  => ((year - 10, month), (year + 10, month))

        def viewButton(cls: String, text: String, target: DatePickerView, label: String): UI =
            var b = button.cssClass(cls).jsProp("type", "button").aria("label", label)
            currentViewRefV match
                // The switch redraws the header without the button that was pressed (the year view
                // titles a decade, and neither granular view keeps its own button), so the focus
                // goes on to the grid that appeared rather than falling to the document.
                case Present(vref) =>
                    b = b.onClick(seedGrid(nav).andThen(vref.set(target)).andThen(focusGrid(nav)))
                case Absent => b = b.disabled(true)
            end match
            b(text)
        end viewButton

        val titleChildren: List[UI] = view match
            case DatePickerView.Date =>
                List(
                    viewButton("p-datepicker-select-month", DatePicker.monthNames(month - 1), DatePickerView.Month, "Choose Month"),
                    viewButton("p-datepicker-select-year", year.toString, DatePickerView.Year, "Choose Year")
                )
            case DatePickerView.Month =>
                List(viewButton("p-datepicker-select-year", year.toString, DatePickerView.Year, "Choose Year"))
            case DatePickerView.Year =>
                val base = year - Math.floorMod(year, 10)
                List(span.cssClass("p-datepicker-decade")(s"$base - ${base + 9}"))

        val title: UI = div.cssClass("p-datepicker-title")(titleChildren.map(toChild)*)

        div.cssClass("p-datepicker-header")(
            toChild(navButton("p-datepicker-prev-button", Icons.chevronLeft, prevTarget, "Previous Month")),
            toChild(title),
            toChild(navButton("p-datepicker-next-button", Icons.chevronRight, nextTarget, "Next Month"))
        )
    end headerUI

    // === day view ===========================================================

    /** Prime's `table.p-datepicker-day-view`: a weekday header row over full weeks
      * of day cells; leading/trailing cells show the adjacent months' days
      * (`.p-datepicker-other-month`, non-interactive).
      */
    private def dayView(year: Int, month: Int, snap: Snapshot, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI =
        val dim      = DatePicker.daysInMonth(year, month)
        val firstDow = DatePicker.dayOfWeek(year, month, 1) // 0 = Sunday
        val (py, pm) = if month == 1 then (year - 1, 12) else (year, month - 1)
        val pdim     = DatePicker.daysInMonth(py, pm)

        val headWeek: List[UI] =
            if showWeekFlag then
                List(
                    th.cssClass("p-datepicker-weekheader").cssClass("p-disabled")(
                        span.cssClass("p-datepicker-weeklabel-container")("Wk")
                    )
                )
            else Nil
        val headDays: List[UI] = DatePicker.weekdayNames.toList.map { wd =>
            th.cssClass("p-datepicker-weekday-cell")(
                span.cssClass("p-datepicker-weekday")(wd)
            )
        }
        val headRow: UI = tr((headWeek ++ headDays).map(toChild)*)

        // Cells: previous-month lead-in, the month itself, next-month fill to a full week.
        val leading: List[UI] = (0 until firstDow).toList.map { i =>
            otherMonthCell(pdim - firstDow + 1 + i)
        }
        // Where the keyboard is. A cursor outside the displayed month is not this grid's, so it
        // falls back to the selected day and then to the first, which is what a reader who has
        // just paged into a month means by "here".
        val cursorIso = dayCursor(year, month, snap, nav)
        val days: List[UI] = (1 to dim).toList.map { d =>
            val iso = DatePicker.isoDate(year, month, d)
            // ISO strings compare lexicographically, so plain string order is date order.
            val outOfRange =
                minDateV.exists(min => iso < min) || maxDateV.exists(max => iso > max)
            var daySpan = span.cssClass("p-datepicker-day")
            if outOfRange then daySpan = daySpan.cssClass("p-disabled").aria("disabled", "true")
            else
                daySpan = daySpan.onClick(seedCursor(iso, nav).andThen(selectDay(iso, snap, oref, nav)))
                dayStateClass(iso, snap).foreach(c => daySpan = daySpan.cssClass(c))
            end if
            if iso == cursorIso then daySpan = daySpan.cssClass("p-focus").scrollAuto(true)
            // Every cell answers to an id, not only the highlighted one, so the attribute that
            // names it points at something already in the document whichever day it moves to. The
            // id is on the CELL, which is what carries the role a grid's highlight may name; the
            // class is on the day inside it, which is what the sheet paints.
            var cell = td.cssClass("p-datepicker-day-cell").role("gridcell")
            nav.foreach(n => cell = cell.id(n.cellId(firstDow + d - 1)))
            cell(daySpan(d.toString))
        }
        val used     = firstDow + dim
        val trailing = (1 to (7 - used % 7) % 7).toList.map(otherMonthCell)
        val cells    = leading ++ days ++ trailing

        val bodyRows: List[UI] = cells.grouped(7).toList.zipWithIndex.map { (week, r) =>
            val weekCell: List[UI] =
                if showWeekFlag then
                    val firstDayInRow = math.max(1, r * 7 - firstDow + 1)
                    val weekNo        = DatePicker.isoWeekNumber(year, month, firstDayInRow)
                    List(td.cssClass("p-datepicker-weeknumber")(span.cssClass("p-disabled")(weekNo.toString)))
                else Nil
            tr.role("row")((weekCell ++ week).map(toChild)*)
        }

        // ONE tab stop with the arrows inside it, and the highlight announced rather than focused:
        // a day is a `<span>`, not a control of its own, so this is the roving-highlight form the
        // list family already uses. Nothing here moves DOM focus, which is also why nothing here
        // can race the render that draws the month it moved into.
        var grid = table.cssClass("p-datepicker-day-view").role("grid")
        nav.foreach { n =>
            grid = grid
                .id(n.gridId)
                .tabIndex(0)
                .preventScrollKeys
                .onKeyDown(dayKey(year, month, cursorIso, snap, oref, n))
            if n.seedV then grid = grid.focusAuto(true)
            if cursorIso.nonEmpty then
                grid = grid.aria("activedescendant", n.cellId(firstDow + DatePicker.dayOf(cursorIso) - 1))
        }
        grid(((headRow :: bodyRows)).map(toChild)*)
    end dayView

    /** The day the keyboard is on, clamped into the month on the screen. */
    private def dayCursor(year: Int, month: Int, snap: Snapshot, nav: Maybe[DatePicker.Nav]): String =
        val prefix = DatePicker.pad(year, 4) + "-" + DatePicker.pad(month, 2)
        // Length as well as prefix: the cursor is shared with the month and year grids, so after
        // a drill it can still hold a `YYYY-MM`, which starts with the prefix and is no day.
        val fromCursor = nav.map(_.cursorV).filter(c => c.length == 10 && c.startsWith(prefix))
        if fromCursor.isDefined then fromCursor.getOrElse("")
        else
            val selected = DatePicker.datePart(anchorValue(snap))
            if selected.startsWith(prefix) then selected
            else if nav.isDefined then prefix + "-01"
            else ""
        end if
    end dayCursor

    /** One key over the day grid, through [[CalendarNav]].
      *
      * Every move is expressed in days and applied with the same arithmetic the grid is drawn
      * from, so crossing a week boundary lands on the next day and crossing a month boundary
      * carries the displayed month with it. `Page` is a month, and its Shift form a year, which
      * is what the ARIA date-grid pattern asks for and what [[GridNav]]'s pages could not mean.
      */
    private def dayKey(
        year: Int,
        month: Int,
        cursorIso: String,
        snap: Snapshot,
        oref: Maybe[SignalRef[Boolean]],
        nav: DatePicker.Nav
    )(e: KeyboardEvent)(using Frame): Any < Async =
        CalendarNav.onKey(e.key, e.modifiers, perRow = 7) match
            case Absent => ()
            case Present(step) =>
                DatePicker.parseDate(cursorIso) match
                    case Absent => ()
                    case Present((cy, cm, cd)) =>
                        import CalendarNav.Step.*
                        val target: Maybe[(Int, Int, Int)] = step match
                            case Move(cells)        => Present(DatePicker.addDays(cy, cm, cd, cells))
                            case RowStart           => Present(DatePicker.addDays(cy, cm, cd, -DatePicker.dayOfWeek(cy, cm, cd)))
                            case RowEnd             => Present(DatePicker.addDays(cy, cm, cd, 6 - DatePicker.dayOfWeek(cy, cm, cd)))
                            case Page(delta, big)   => Present(DatePicker.addMonths(cy, cm, cd, if big then delta * 12 else delta))
                            case Activate | Dismiss => Absent
                        val moved: Any < Async = target match
                            case Present((ny, nm, nd)) => moveDayCursor(ny, nm, nd, nav)
                            case Absent                => ()
                        val acted: Any < Async = step match
                            case Activate => selectDay(cursorIso, snap, oref, Present(nav))
                            case Dismiss  => cancelVia(oref, Present(nav))
                            case _        => ()
                        moved.andThen(acted)
    end dayKey

    /** A click on a cell seeds the highlight, so the next arrow carries on from the cell the
      * pointer named rather than from wherever the grid last derived one. The ring stays the
      * keyboard's: this writes the highlight, not the focus.
      */
    /** Hands the focus to the grid on screen, which is where a view switch leaves the reader. */
    private def focusGrid(nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        nav match
            case Present(n) => n.focus(n.gridId)
            case Absent     => ()

    private def seedCursor(iso: String, nav: Maybe[DatePicker.Nav])(using Frame): Any < Async =
        nav match
            case Present(n) => n.cursor.set(iso)
            case Absent     => ()

    /** Writes the cursor and, when the move left the displayed month, the month with it. */
    private def moveDayCursor(y: Int, m: Int, d: Int, nav: DatePicker.Nav)(using Frame): Any < Async =
        val iso  = DatePicker.isoDate(y, m, d)
        val ym   = DatePicker.pad(y, 4) + "-" + DatePicker.pad(m, 2)
        val page = if nav.monthOvV == ym then (): Any < Async else nav.monthOv.set(ym)
        nav.cursor.set(iso).andThen(page)
    end moveDayCursor

    /** Prime's day state classes: endpoints/picked days get `-day-selected`,
      * strictly-in-range days of a complete range get `-day-selected-range`.
      */
    private def dayStateClass(iso: String, snap: Snapshot): Maybe[String] = snap match
        case Snapshot.Single(v) =>
            if v.nonEmpty && DatePicker.datePart(v) == iso then Present("p-datepicker-day-selected") else Absent
        case Snapshot.Multi(set) =>
            if set.contains(iso) then Present("p-datepicker-day-selected") else Absent
        case Snapshot.Range(s, e) =>
            if iso == s || (e.nonEmpty && iso == e) then Present("p-datepicker-day-selected")
            else if s.nonEmpty && e.nonEmpty && s < iso && iso < e then Present("p-datepicker-day-selected-range")
            else Absent

    private def otherMonthCell(day: Int)(using Frame): UI =
        td.cssClass("p-datepicker-day-cell").cssClass("p-datepicker-other-month").role("gridcell")(
            span.cssClass("p-datepicker-day").cssClass("p-disabled")(day.toString)
        )

    /** A day pick, per selection mode: single writes the ISO string (keeping the
      * time part under `showTime`) and closes unless the time picker needs the
      * panel; multiple toggles set membership and stays open; range fills
      * start/end and closes on completion. Every write fires `onChange` with the
      * picked day.
      */
    private def selectDay(iso: String, snap: Snapshot, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): Any < Async =
        remember(nav).andThen(selectDayNow(iso, snap, oref, nav))

    private def selectDayNow(iso: String, snap: Snapshot, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): Any < Async =
        snap match
            case Snapshot.Single(cur) =>
                val newValue = if showTimeFlag then iso + "T" + DatePicker.timeText(cur) else iso
                for
                    _ <- writeSingle(newValue)
                    _ <- fireChange(newValue)
                    r <- if showTimeFlag then (): Any < Async else closeAndReturn(oref, nav)
                yield r
                end for
            case Snapshot.Multi(set) =>
                val toggled = if set.contains(iso) then set - iso else set + iso
                val write: Any < Async = multiRef match
                    case Present(ref) => ref.set(toggled)
                    case Absent       => ()
                for
                    _ <- write
                    r <- fireChange(iso)
                yield r
                end for
            case Snapshot.Range(s, e) =>
                rangeRefsV match
                    case Present((startRef, endRef)) =>
                        if s.isEmpty || e.nonEmpty || iso < s then
                            // (Re)start the range: write start, clear a stale end.
                            for
                                _ <- startRef.set(iso)
                                _ <- endRef.set("")
                                r <- fireChange(iso)
                            yield r
                        else
                            for
                                _ <- endRef.set(iso)
                                _ <- fireChange(iso)
                                r <- closeAndReturn(oref, nav)
                            yield r
                    case Absent => fireChange(iso)
    end selectDayNow

    private def writeSingle(newValue: String)(using Frame): Any < Async =
        valueBinding match
            case Present(Input.Value.Ref(ref)) => ref.set(newValue)
            case _                             => ()

    // === month / year views =================================================

    /** Prime's `.p-datepicker-month-view`: 12 month cells. Under `view(Month)`
      * a pick writes the `YYYY-MM` value and closes; while drilling from the day
      * view it writes the displayed `month` ref and drills back.
      */
    private def monthView(year: Int, snap: Snapshot, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI =
        val current = DatePicker.datePart(anchorValue(snap)).take(7)
        val cursor  = granularCursor(DatePicker.pad(year, 4) + "-", current, 7, nav)
        val cells: List[UI] = (1 to 12).toList.map { m =>
            val iso7 = DatePicker.pad(year, 4) + "-" + DatePicker.pad(m, 2)
            val outOfRange =
                minDateV.exists(min => iso7 < min.take(7)) || maxDateV.exists(max => iso7 > max.take(7))
            var cell = span.cssClass("p-datepicker-month")
            if current == iso7 then cell = cell.cssClass("p-datepicker-month-selected")
            val action: Maybe[Any < Async] =
                if outOfRange then Absent
                else if viewV == DatePickerView.Month then Present(pickGranular(iso7, oref, nav))
                else drillTo(iso7, DatePickerView.Date, nav)
            action match
                case Present(a) => cell = cell.onClick(seedCursor(iso7, nav).andThen(a))
                case Absent     => cell = cell.cssClass("p-disabled").aria("disabled", "true")
            nav.foreach(n => cell = cell.id(n.cellId(m - 1)).role("gridcell"))
            if iso7 == cursor then cell = cell.cssClass("p-focus").scrollAuto(true)
            cell(DatePicker.monthNamesShort(m - 1))
        }
        // Prime lays the twelve months out three to a row, which is the width the vertical
        // arrows step by.
        granularGrid("p-datepicker-month-view", cells, cursor, iso => DatePicker.monthOf(iso) - 1, 3, oref, nav) { (at, step) =>
            import CalendarNav.Step.*
            step match
                case Move(cells) => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(wrapIndex(at + cells, 12) + 1, 2))
                case RowStart    => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(at - at % 3 + 1, 2))
                case RowEnd      => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(at - at % 3 + 3, 2))
                case Page(delta, big) =>
                    Present(DatePicker.pad(year + (if big then delta * 10 else delta), 4) + "-" + DatePicker.pad(at + 1, 2))
                case Activate | Dismiss => Absent
            end match
        } { iso7 =>
            if viewV == DatePickerView.Month then pickGranular(iso7, oref, nav)
            else
                drillTo(iso7, DatePickerView.Date, nav) match
                    case Present(eff) => eff
                    case Absent       => (): Any < Async
        }
    end monthView

    /** Prime's `.p-datepicker-year-view`: the displayed decade's 10 year cells.
      * Under `view(Year)` a pick writes the `YYYY` value and closes; while
      * drilling it writes the `month` ref's year and drills to the month grid.
      */
    private def yearView(
        year: Int,
        month: Int,
        snap: Snapshot,
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(using Frame): UI =
        val current = DatePicker.datePart(anchorValue(snap)).take(4)
        val base    = year - Math.floorMod(year, 10)
        val cursor  = granularCursor("", current, 4, nav, fallback = DatePicker.pad(base, 4))
        val cells: List[UI] = (base to base + 9).toList.map { y =>
            val iso4 = DatePicker.pad(y, 4)
            val outOfRange =
                minDateV.exists(min => iso4 < min.take(4)) || maxDateV.exists(max => iso4 > max.take(4))
            var cell = span.cssClass("p-datepicker-year")
            if current == iso4 then cell = cell.cssClass("p-datepicker-year-selected")
            val action: Maybe[Any < Async] =
                if outOfRange then Absent
                else if viewV == DatePickerView.Year then Present(pickGranular(iso4, oref, nav))
                else drillTo(iso4 + "-" + DatePicker.pad(month, 2), DatePickerView.Month, nav)
            action match
                case Present(a) => cell = cell.onClick(seedCursor(iso4, nav).andThen(a))
                case Absent     => cell = cell.cssClass("p-disabled").aria("disabled", "true")
            nav.foreach(n => cell = cell.id(n.cellId(y - base)).role("gridcell"))
            if iso4 == cursor then cell = cell.cssClass("p-focus").scrollAuto(true)
            cell(y.toString)
        }
        // Prime lays the decade out two to a row.
        granularGrid("p-datepicker-year-view", cells, cursor, iso => iso.toIntOption.getOrElse(base) - base, 2, oref, nav) {
            (at, step) =>
                import CalendarNav.Step.*
                step match
                    case Move(cells)        => Present(DatePicker.pad(base + wrapIndex(at + cells, 10), 4))
                    case RowStart           => Present(DatePicker.pad(base + at - at % 2, 4))
                    case RowEnd             => Present(DatePicker.pad(base + at - at % 2 + 1, 4))
                    case Page(delta, big)   => Present(DatePicker.pad(base + at + (if big then delta * 100 else delta * 10), 4))
                    case Activate | Dismiss => Absent
                end match
        } { iso4 =>
            if viewV == DatePickerView.Year then pickGranular(iso4, oref, nav)
            else
                drillTo(iso4 + "-" + DatePicker.pad(month, 2), DatePickerView.Month, nav) match
                    case Present(eff) => eff
                    case Absent       => (): Any < Async
        }
    end yearView

    /** The cell the keyboard is on in a granular grid, clamped into the page on the screen. */
    private def granularCursor(
        prefix: String,
        selected: String,
        width: Int,
        nav: Maybe[DatePicker.Nav],
        fallback: String = ""
    ): String =
        val fromCursor = nav.map(_.cursorV).filter(c => c.length == width && c.startsWith(prefix))
        if fromCursor.isDefined then fromCursor.getOrElse("")
        else if selected.length == width && selected.startsWith(prefix) then selected
        else if nav.isEmpty then ""
        else if fallback.nonEmpty then fallback
        else prefix + "01"
        end if
    end granularCursor

    /** An index that comes round rather than stopping, since a granular grid is a closed ring of
      * twelve months or ten years and the page keys are what leave it.
      */
    private def wrapIndex(i: Int, size: Int): Int = Math.floorMod(i, size)

    /** The shared wiring of the month and year grids: one tab stop, the highlight announced, and
      * [[CalendarNav]] read against the grid's own width. `move` maps a step onto the next cell's
      * ISO string, `activate` picks it.
      */
    private def granularGrid(
        cls: String,
        cells: List[UI],
        cursor: String,
        indexOf: String => Int,
        perRow: Int,
        oref: Maybe[SignalRef[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(move: (Int, CalendarNav.Step) => Maybe[String])(activate: String => Any < Async)(using Frame): UI =
        var grid = div.cssClass(cls)
        nav.foreach { n =>
            grid = grid.id(n.gridId).tabIndex(0).preventScrollKeys.role("grid")
            if n.seedV then grid = grid.focusAuto(true)
            if cursor.nonEmpty then grid = grid.aria("activedescendant", n.cellId(indexOf(cursor)))
            grid = grid.onKeyDown { e =>
                val eff: Any < Async =
                    if cursor.isEmpty then ()
                    else
                        CalendarNav.onKey(e.key, e.modifiers, perRow) match
                            case Absent => ()
                            case Present(step) =>
                                val moved: Any < Async = move(indexOf(cursor), step) match
                                    case Present(next) => n.cursor.set(next)
                                    case Absent        => ()
                                val acted: Any < Async = step match
                                    case CalendarNav.Step.Activate => activate(cursor)
                                    case CalendarNav.Step.Dismiss  => cancelVia(oref, nav)
                                    case _                         => ()
                                moved.andThen(acted)
                eff
            }
        }
        // A `gridcell` with no `row` over it is outside the grid as far as the ARIA tree is
        // concerned, and Prime's own markup lays the cells straight into a wrapping flex box. The
        // rows carry the structure and `display: contents` in the extra sheet keeps the layout,
        // so the twelve months still wrap the way Prime draws them.
        val rows: List[UI] =
            cells.grouped(perRow).toList.map(row => div.role("row")(row.map(toChild)*))
        grid(rows.map(toChild)*)
    end granularGrid

    /** A granularity pick (`view(Month|Year)`): write the ISO prefix, close. */
    private def pickGranular(iso: String, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): Any < Async =
        for
            _ <- remember(nav)
            _ <- writeSingle(iso)
            _ <- fireChange(iso)
            r <- closeAndReturn(oref, nav)
        yield r

    /** A drill-down pick: write the displayed month and switch the view back.
      * Needs BOTH the `month` and `currentView` refs; `Absent` renders the cell
      * disabled instead.
      */
    private def drillTo(yearMonth: String, target: DatePickerView, nav: Maybe[DatePicker.Nav])(using
        Frame
    ): Maybe[Any < Async] =
        (nav.map(_.monthOv).orElse(monthRef), currentViewRefV) match
            case (Present(mref), Present(vref)) =>
                Present(
                    for
                        _ <- seedGrid(nav)
                        _ <- mref.set(yearMonth)
                        _ <- vref.set(target)
                        r <- focusGrid(nav)
                    yield r
                )
            case _ => Absent

    // === time picker ========================================================

    /** Prime's `.p-datepicker-time-picker`: hour/minute columns of increment /
      * display / decrement, plus the AM/PM column under `hourFormat(H12)`. Only
      * the single-selection value model carries a time.
      */
    private def timePickerUI(snap: Snapshot, nav: Maybe[DatePicker.Nav])(using Frame): UI =
        val cur = snap match
            case Snapshot.Single(v) => v
            case _                  => ""
        // timeOnly edits from 00:00; date+time needs a picked date first (the pure
        // render has no clock to substitute), so the buttons render disabled.
        val editable       = timeOnlyFlag || cur.nonEmpty
        val (hour, minute) = DatePicker.parseTime(DatePicker.timeText(cur))

        def write(h: Int, m: Int): Any < Async =
            val t        = DatePicker.pad(h, 2) + ":" + DatePicker.pad(m, 2)
            val newValue = if timeOnlyFlag then t else DatePicker.datePart(cur) + "T" + t
            for
                _ <- remember(nav)
                _ <- writeSingle(newValue)
                r <- fireChange(newValue)
            yield r
            end for
        end write

        def spinButton(cls: String, glyph: IconGlyph, label: String, action: Any < Async): UI =
            var b = button
                .cssClass(cls)
                .cssClass("p-button")
                .cssClass("p-component")
                .cssClass("p-button-icon-only")
                .cssClass("p-button-secondary")
                .cssClass("p-button-rounded")
                .cssClass("p-button-text")
                .jsProp("type", "button")
                .aria("label", label)
            if editable then b = b.onClick(action) else b = b.disabled(true)
            b(toChild(GlyphSvg(glyph, "p-icon")))
        end spinButton

        def column(cls: String, inc: Any < Async, dec: Any < Async, text: String, incLabel: String, decLabel: String): UI =
            div.cssClass(cls)(
                toChild(spinButton("p-datepicker-increment-button", Icons.chevronUp, incLabel, inc)),
                toChild(span(text)),
                toChild(spinButton("p-datepicker-decrement-button", Icons.chevronDown, decLabel, dec))
            )

        val hourText = hourFormatV match
            case HourFormat.H24 => DatePicker.pad(hour, 2)
            case HourFormat.H12 => DatePicker.pad(Math.floorMod(hour + 11, 12) + 1, 2)

        val columns: List[UI] =
            List(
                column(
                    "p-datepicker-hour-picker",
                    write(Math.floorMod(hour + 1, 24), minute),
                    write(Math.floorMod(hour - 1, 24), minute),
                    hourText,
                    "Next Hour",
                    "Previous Hour"
                ),
                div(toChild(span(":"))),
                column(
                    "p-datepicker-minute-picker",
                    write(hour, Math.floorMod(minute + 1, 60)),
                    write(hour, Math.floorMod(minute - 1, 60)),
                    DatePicker.pad(minute, 2),
                    "Next Minute",
                    "Previous Minute"
                )
            ) ++ (hourFormatV match
                case HourFormat.H12 =>
                    val toggle = write(Math.floorMod(hour + 12, 24), minute)
                    List(
                        div.cssClass("p-datepicker-ampm-picker")(
                            toChild(spinButton("p-datepicker-increment-button", Icons.chevronUp, "AM/PM", toggle)),
                            toChild(span(if hour < 12 then "AM" else "PM")),
                            toChild(spinButton("p-datepicker-decrement-button", Icons.chevronDown, "AM/PM", toggle))
                        )
                    )
                case HourFormat.H24 => Nil)

        div.cssClass("p-datepicker-time-picker")(columns.map(toChild)*)
    end timePickerUI

    // === button bar =========================================================

    /** Prime's `.p-datepicker-buttonbar`: Today (only with an explicit [[today]]
      * date — the render is pure) + Clear, both in Prime's small secondary text
      * Button skin.
      */
    private def buttonBarUI(snap: Snapshot, oref: Maybe[SignalRef[Boolean]], nav: Maybe[DatePicker.Nav])(using Frame): UI =
        def barButton(cls: String, label: String, action: Any < Async): UI =
            button
                .cssClass(cls)
                .cssClass("p-button")
                .cssClass("p-component")
                .cssClass("p-button-secondary")
                .cssClass("p-button-text")
                .cssClass("p-button-sm")
                .jsProp("type", "button")
                .onClick(action)(toChild(span.cssClass("p-button-label")(label)))

        val todayButton: List[UI] = todayV.toList.map { iso =>
            val navigate: Any < Async = monthRef match
                case Present(mref) => mref.set(iso.take(7))
                case Absent        => ()
            val action =
                for
                    _ <- navigate
                    r <- selectDay(DatePicker.datePart(iso), snap, oref, nav)
                yield r
            barButton("p-datepicker-today-button", "Today", action)
        }

        val clearAction: Any < Async =
            val wipeSingle: Any < Async = writeSingle("")
            val wipeMulti: Any < Async = multiRef match
                case Present(ref) => ref.set(Set.empty)
                case Absent       => ()
            val wipeRange: Any < Async = rangeRefsV match
                case Present((s, e)) =>
                    for
                        _ <- s.set("")
                        r <- e.set("")
                    yield r
                case Absent => ()
            for
                _ <- remember(nav)
                _ <- wipeSingle
                _ <- wipeMulti
                _ <- wipeRange
                _ <- fireChange("")
                r <- closeAndReturn(oref, nav)
            yield r
            end for
        end clearAction

        val clearButton: UI = barButton("p-datepicker-clear-button", "Clear", clearAction)

        div.cssClass("p-datepicker-buttonbar")((todayButton :+ clearButton).map(toChild)*)
    end buttonBarUI

    /** Derives `(year, month)` from the month override, else the bound value, else
      * the reference date, else a fixed epoch.
      */
    private def deriveMonth(current: String, monthOverride: Maybe[String]): (Int, Int) =
        val fromOverride = monthOverride.flatMap(DatePicker.parseYearMonth)
        fromOverride match
            case Present(ym) => ym
            case Absent =>
                DatePicker.parseYearMonth(current) match
                    case Present(ym) => ym
                    case Absent =>
                        referenceDateV match
                            case Present(rd) => DatePicker.parseYearMonth(rd).getOrElse((2000, 1))
                            case Absent      => (2000, 1)
        end match
    end deriveMonth
end DatePicker

object DatePicker:
    def apply(): DatePicker = new DatePicker()

    /** The keyboard state the mount allocates, and the ids it addresses the calendar by.
      *
      * On the companion rather than inside the class, because the reactive-invalid path renders
      * through a `copy` of the field and a state typed against the original instance would not
      * fit it (the [[CascadeSelect]] precedent).
      *
      * `monthOv` is the displayed month. A caller's own `month(ref)` wins; without one a minted
      * ref stands in, holding `""` until something moves the month. That is what lets the header
      * buttons and the arrows move it at all: before, an unbound `month` left them disabled and
      * the calendar frozen on whatever month it derived.
      *
      * `cursor` is the cell the keyboard is on, as an ISO string at the view's own precision.
      */
    /** The values the panel held before the reader changed anything in it, which is what Escape
      * puts back. Recorded once per open, at the first write rather than at the open itself, so
      * an open a caller drove through its own ref is remembered too.
      */
    final private[uic] case class Restore(single: String, multi: Set[String], start: String, end: String) derives CanEqual

    /** The refs a mount hands the wired host: the ones a caller may bind (open, month, view) and
      * the ones the component keeps for itself (the keyboard cursor, the flag that says the
      * keyboard is coming into the grid, and what Escape puts back).
      */
    final private[uic] case class Refs(
        open: SignalRef[Boolean],
        month: SignalRef[String],
        cursor: SignalRef[String],
        view: SignalRef[DatePickerView],
        seed: SignalRef[Boolean],
        restore: SignalRef[Maybe[Restore]]
    )

    final private[uic] case class Nav(
        monthOv: SignalRef[String],
        monthOvV: String,
        cursor: SignalRef[String],
        cursorV: String,
        seed: SignalRef[Boolean],
        seedV: Boolean,
        restore: SignalRef[Maybe[Restore]],
        base: String,
        focus: String => Any < Async
    ):
        def cellId(index: Int): String = s"$base-c$index"
        def gridId: String             = s"$base-grid"
        def fieldId: String            = s"$base-field"
    end Nav

    private val monthNames: Array[String] =
        Array(
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
        )

    private val monthNamesShort: Array[String] =
        Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    private val weekdayNames: Array[String] = Array("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")

    private def isLeap(y: Int): Boolean = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)

    private def daysInMonth(y: Int, m: Int): Int =
        m match
            case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
            case 4 | 6 | 9 | 11              => 30
            case 2                           => if isLeap(y) then 29 else 28
            case _                           => 30

    // Sakamoto's algorithm — returns 0 = Sunday .. 6 = Saturday.
    private val sakamoto = Array(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    private def dayOfWeek(y: Int, m: Int, d: Int): Int =
        val yy = if m < 3 then y - 1 else y
        Math.floorMod(yy + yy / 4 - yy / 100 + yy / 400 + sakamoto(m - 1) + d, 7)

    private def dayOfYear(y: Int, m: Int, d: Int): Int =
        (1 until m).foldLeft(d)((acc, mm) => acc + daysInMonth(y, mm))

    /** Number of ISO 8601 weeks in `y` (52 or 53) — 53 iff the year starts or ends on
      * a Thursday, expressed via the standard `p(y)` weekday-of-Dec-31 formula.
      */
    private def isoWeeksInYear(y: Int): Int =
        def p(yy: Int): Int = Math.floorMod(yy + yy / 4 - yy / 100 + yy / 400, 7)
        if p(y) == 4 || p(y - 1) == 3 then 53 else 52

    /** ISO 8601 week number (weeks start Monday; week 1 contains the year's first
      * Thursday). Pure integer math — same no-`java.time` constraint as [[dayOfWeek]].
      */
    private def isoWeekNumber(y: Int, m: Int, d: Int): Int =
        val dowRaw = dayOfWeek(y, m, d)
        val isoDow = if dowRaw == 0 then 7 else dowRaw // 1 = Monday .. 7 = Sunday
        val week   = (dayOfYear(y, m, d) - isoDow + 10) / 7
        if week < 1 then isoWeeksInYear(y - 1)
        else if week > isoWeeksInYear(y) then 1
        else week
    end isoWeekNumber

    private def pad(n: Int, width: Int): String =
        val s = n.toString
        if s.length >= width then s else "0" * (width - s.length) + s

    private def isoDate(y: Int, m: Int, d: Int): String = pad(y, 4) + "-" + pad(m, 2) + "-" + pad(d, 2)

    /** The day-of-month of an ISO date, or 1 for anything that does not parse. */
    private def dayOf(iso: String): Int = parseDate(iso).map(_._3).getOrElse(1)

    /** The month of an ISO `YYYY-MM[-DD]`, or 1 for anything that does not parse. */
    private def monthOf(iso: String): Int = parseYearMonth(iso).map(_._2).getOrElse(1)

    /** The date part of an ISO value (`"…T…"` → before the `T`; else unchanged). */
    private def datePart(v: String): String =
        val t = v.indexOf('T')
        if t >= 0 then v.take(t) else v

    /** The time part of an ISO value: after the `T`, or the whole value when it IS
      * a time (`HH:MM`), else `"00:00"`.
      */
    private def timeText(v: String): String =
        val t = v.indexOf('T')
        if t >= 0 then v.drop(t + 1)
        else if v.length >= 4 && v.contains(':') then v
        else "00:00"
    end timeText

    /** Parses `HH:MM` into clamped `(hour, minute)`; anything malformed → 0. */
    private def parseTime(t: String): (Int, Int) =
        val parts = t.split(":")
        val h     = if parts.length >= 1 then parts(0).toIntOption.getOrElse(0) else 0
        val m     = if parts.length >= 2 then parts(1).toIntOption.getOrElse(0) else 0
        (Math.floorMod(h, 24), Math.floorMod(m, 60))
    end parseTime

    /** Parses `YYYY-MM-DD` out of an ISO string; anything shorter or malformed is `Absent`. */
    private def parseDate(s: String): Maybe[(Int, Int, Int)] =
        val parts = datePart(s).split("-")
        if parts.length < 3 then Absent
        else
            (parts(0).toIntOption, parts(1).toIntOption, parts(2).toIntOption) match
                case (Some(y), Some(m), Some(d)) if m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth(y, m) =>
                    Present((y, m, d))
                case _ => Absent
        end if
    end parseDate

    /** `n` days from `(y, m, d)`, crossing month and year boundaries.
      *
      * A day-at-a-time walk rather than an epoch conversion, because the only shifts the
      * calendar asks for are one day and one week: the loop is over in seven steps and is
      * obviously right, where a day-number round trip is neither.
      */
    private def addDays(y: Int, m: Int, d: Int, n: Int): (Int, Int, Int) =
        @annotation.tailrec
        def loop(y: Int, m: Int, d: Int, k: Int): (Int, Int, Int) =
            if k == 0 then (y, m, d)
            else if k > 0 then
                if d < daysInMonth(y, m) then loop(y, m, d + 1, k - 1)
                else if m == 12 then loop(y + 1, 1, 1, k - 1)
                else loop(y, m + 1, 1, k - 1)
            else if d > 1 then loop(y, m, d - 1, k + 1)
            else if m == 1 then loop(y - 1, 12, 31, k + 1)
            else loop(y, m - 1, daysInMonth(y, m - 1), k + 1)
        loop(y, m, d, n)
    end addDays

    /** `n` months from `(y, m)`, with the day clamped into the month it lands in: a 31st
      * paged into a 30-day month is that month's 30th, not its 1st.
      */
    private def addMonths(y: Int, m: Int, d: Int, n: Int): (Int, Int, Int) =
        val total = (y * 12 + (m - 1)) + n
        val ny    = Math.floorDiv(total, 12)
        val nm    = Math.floorMod(total, 12) + 1
        (ny, nm, math.min(d, daysInMonth(ny, nm)))
    end addMonths

    /** Parses the leading `YYYY[-MM]` out of an ISO string (a bare 4-digit year
      * reads as January — the year-granularity value model).
      */
    private def parseYearMonth(s: String): Maybe[(Int, Int)] =
        val parts = datePart(s).split("-")
        if parts.length >= 2 then
            (parts(0).toIntOption, parts(1).toIntOption) match
                case (Some(y), Some(m)) if m >= 1 && m <= 12 => Present((y, m))
                case _                                       => Absent
        else if parts.length == 1 && parts(0).length == 4 then
            parts(0).toIntOption match
                case Some(y) => Present((y, 1))
                case None    => Absent
        else Absent
        end if
    end parseYearMonth
end DatePicker
