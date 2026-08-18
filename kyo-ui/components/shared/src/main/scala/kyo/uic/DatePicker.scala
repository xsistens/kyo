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
) extends Node, TextFormControl:
    type Self = DatePicker

    /** Native `id` on the picker's text input — pair with `Label.forId`; the form layer
      * stamps the bound field's id here so focus-first-invalid can target the input.
      */
    def id(v: String): DatePicker = copy(idV = Present(v))

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
      * depending on the current view). Without it the displayed month stays
      * derived from the value/referenceDate and the buttons render disabled. An
      * empty ref value falls back to the value-derived month.
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

    def placeholder(v: String): DatePicker = copy(placeholderText = Present(TextValue.Const(v)))

    /** Reactive placeholder that tracks `sig` — patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render/re-mount of the field) on each emission.
      */
    def placeholder(sig: Signal[String]): DatePicker = copy(placeholderText = Present(TextValue.Dyn(sig)))
    def disabled(v: Boolean): DatePicker             = copy(disabledFlag = v)

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

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): DatePicker = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): DatePicker = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): DatePicker = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): DatePicker = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): DatePicker = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): DatePicker = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): DatePicker = copy(accNameRefV = Present(v))

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

    private[uic] def render(using Frame): UI =
        if inlineFlag then
            // In-flow panel below the field (Prime's inline prop): always visible
            // without an open ref, toggled by the dropdown button with one.
            val panel: UI = openRef match
                case Present(oref) => when(oref)(calendarReactive(openRef, inline = true))
                case Absent        => calendarReactive(Absent, inline = true)
            assemble(openRef, isOpen = false, floating = false, panel)
        else
            openRef match
                case Present(oref) => wired(oref)
                case Absent        =>
                    // Self-managed visibility: the open signal lives in this effectful
                    // mount; static projections (SSG, the SSR page GET) render the same
                    // closed anatomy inert until the transport attaches.
                    val stat: UI = assemble(Absent, isOpen = false, floating = true, UI.empty)
                    if !interactive then stat
                    else UI.mounted(Signal.initRef(false).map(wired)).placeholder(stat)

    /** The floating host the mount (or a bound `open` ref) publishes — the seam
      * golden tests render directly (a full top-down re-render shows mounted
      * regions as placeholders).
      */
    private[uic] def wired(oref: SignalRef[Boolean])(using Frame): UI =
        oref.render { isOpen =>
            val panel: UI =
                Overlay(oref)
                    .seedFocus(false)(calendarReactive(Present(oref), inline = false))
                    .render
            assemble(Present(oref), isOpen, floating = true, panel)
        }

    /** Field + dropdown button + panel host inside the `.p-datepicker` root. */
    private def assemble(
        oref: Maybe[SignalRef[Boolean]],
        isOpen: Boolean,
        floating: Boolean,
        panel: UI
    )(using Frame): UI =
        // Reactive-invalid gate (BELOW every UI.mounted node — assemble runs inside
        // the open ref's subscription / at the static projection, never around the
        // mount): with a reactive slot set, re-render field + root + message through
        // the shared helper; otherwise the untouched static form.
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => assembleStatic(oref, isOpen, floating, panel)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                        .assembleStatic(oref, isOpen, floating, panel)
                )

    private def assembleStatic(
        oref: Maybe[SignalRef[Boolean]],
        isOpen: Boolean,
        floating: Boolean,
        panel: UI
    )(using Frame): UI =
        val field: UI = rangeRefsV match
            case Present((s, e)) =>
                s.render(sv => e.render(ev => fieldUI(Present(rangeText(sv, ev)), oref, floating)))
            case Absent =>
                multiRef match
                    case Present(mref) => mref.render(set => fieldUI(Present(set.toList.sorted.mkString(", ")), oref, floating))
                    case Absent        => fieldUI(Absent, oref, floating)

        // The dropdown button toggles the calendar, firing onOpen/onClose on
        // actual transitions. Inline without an open ref keeps it disabled (the
        // always-on panel has nothing to toggle); the floating static projection
        // renders it enabled-looking but inert until the mount wires it.
        var dd = button
            .cssClass("p-datepicker-dropdown")
            .jsProp("type", "button")
            .aria("label", "Choose date")
        if !interactive || (!floating && oref.isEmpty) then dd = dd.disabled(true)
        else oref.foreach(r => dd = dd.onClick(togglePanel(r)))
        val dropdown: UI = dd(toChild(GlyphSvg(Icons.calendar, "p-icon")))

        var el = div.cssClass("p-datepicker").cssClass("p-component")
        if invalidV.constTrue then el = el.cssClass("p-invalid")
        if disabledFlag then el = el.cssClass("p-disabled")
        if fluidFlag then el = el.cssClass("p-datepicker-fluid")
        if floating && isOpen then el = el.cssClass("p-uic-overlay-anchor")
        val root = el(toChild(field), toChild(dropdown), toChild(panel))
        FieldInvalid.withMessage(root, invalidV.constTrue, invalidMsgV)
    end assembleStatic

    /** The text field. Single selection binds two-way; multiple/range supply a
      * derived DISPLAY text (`displayOverride`) — the field is not parsed back.
      * In the floating host a click on the field opens the panel (Prime's
      * `showOnFocus` default) and Escape closes it.
      */
    private def fieldUI(displayOverride: Maybe[String], oref: Maybe[SignalRef[Boolean]], floating: Boolean)(using
        Frame
    ): UI =
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
        if floating && interactive then
            oref.foreach { r =>
                i = i.onClick(openPanel(r))
                i = i.onKeyDown { e =>
                    e.key match
                        case Keyboard.Escape => closeVia(Present(r))
                        case _               => ()
                }
            }
        end if
        i
    end fieldUI

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
    private def calendarReactive(oref: Maybe[SignalRef[Boolean]], inline: Boolean)(using Frame): UI =
        withView { view =>
            withMonthOverride { mOv =>
                withSelection(snap => panelUI(view, mOv, snap, oref, inline))
            }
        }

    private def withView(f: DatePickerView => UI)(using Frame): UI =
        currentViewRefV match
            case Present(ref) => ref.render(f)
            case Absent       => f(viewV)

    private def withMonthOverride(f: Maybe[String] => UI)(using Frame): UI =
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
        inline: Boolean
    )(using Frame): UI =
        val (year, month) = deriveMonth(anchorValue(snap), monthOverride)

        val calendarChildren: List[UI] =
            if timeOnlyFlag then Nil
            else
                val grid: List[UI] = view match
                    case DatePickerView.Date => List(dayView(year, month, snap, oref))
                    case _                   => Nil
                val calendar: UI =
                    div.cssClass("p-datepicker-calendar-container")(
                        toChild(
                            div.cssClass("p-datepicker-calendar")(
                                ((headerUI(view, year, month) :: grid)).map(toChild)*
                            )
                        )
                    )
                val monthGrid: List[UI] = if view == DatePickerView.Month then List(monthView(year, snap, oref)) else Nil
                val yearGrid: List[UI]  = if view == DatePickerView.Year then List(yearView(year, month, snap, oref)) else Nil
                calendar :: (monthGrid ++ yearGrid)

        val timeChildren: List[UI] =
            if timeOnlyFlag || (showTimeFlag && view == DatePickerView.Date) then List(timePickerUI(snap))
            else Nil

        val barChildren: List[UI] = if showButtonBarFlag then List(buttonBarUI(snap, oref)) else Nil

        var panel = div
            .cssClass("p-datepicker-panel")
            .cssClass("p-component")
        if inline then panel = panel.cssClass("p-datepicker-panel-inline")
        if timeOnlyFlag then panel = panel.cssClass("p-datepicker-timeonly")
        panel((calendarChildren ++ timeChildren ++ barChildren).map(toChild)*)
    end panelUI

    /** Prime's calendar header: prev button, the month/year title BUTTONS (they
      * drill into the month/year grids via the bound `currentView` ref; disabled
      * without one — Prime's `switchViewButtonDisabled`), next button. The nav
      * buttons shift the bound `month` ref by month/year/decade depending on the
      * view; without one they render disabled.
      */
    private def headerUI(view: DatePickerView, year: Int, month: Int)(using Frame): UI =
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
            monthRef match
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
                case Present(vref) => b = b.onClick(vref.set(target))
                case Absent        => b = b.disabled(true)
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
    private def dayView(year: Int, month: Int, snap: Snapshot, oref: Maybe[SignalRef[Boolean]])(using Frame): UI =
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
        val days: List[UI] = (1 to dim).toList.map { d =>
            val iso = DatePicker.isoDate(year, month, d)
            // ISO strings compare lexicographically, so plain string order is date order.
            val outOfRange =
                minDateV.exists(min => iso < min) || maxDateV.exists(max => iso > max)
            var daySpan = span.cssClass("p-datepicker-day")
            if outOfRange then daySpan = daySpan.cssClass("p-disabled").aria("disabled", "true")
            else
                daySpan = daySpan.onClick(selectDay(iso, snap, oref))
                dayStateClass(iso, snap).foreach(c => daySpan = daySpan.cssClass(c))
            end if
            td.cssClass("p-datepicker-day-cell")(daySpan(d.toString))
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
            tr((weekCell ++ week).map(toChild)*)
        }

        table.cssClass("p-datepicker-day-view").role("grid")(
            ((headRow :: bodyRows)).map(toChild)*
        )
    end dayView

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
        td.cssClass("p-datepicker-day-cell").cssClass("p-datepicker-other-month")(
            span.cssClass("p-datepicker-day").cssClass("p-disabled")(day.toString)
        )

    /** A day pick, per selection mode: single writes the ISO string (keeping the
      * time part under `showTime`) and closes unless the time picker needs the
      * panel; multiple toggles set membership and stays open; range fills
      * start/end and closes on completion. Every write fires `onChange` with the
      * picked day.
      */
    private def selectDay(iso: String, snap: Snapshot, oref: Maybe[SignalRef[Boolean]])(using Frame): Any < Async =
        snap match
            case Snapshot.Single(cur) =>
                val newValue = if showTimeFlag then iso + "T" + DatePicker.timeText(cur) else iso
                for
                    _ <- writeSingle(newValue)
                    _ <- fireChange(newValue)
                    r <- if showTimeFlag then (): Any < Async else closeVia(oref)
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
                                r <- closeVia(oref)
                            yield r
                    case Absent => fireChange(iso)
    end selectDay

    private def writeSingle(newValue: String)(using Frame): Any < Async =
        valueBinding match
            case Present(Input.Value.Ref(ref)) => ref.set(newValue)
            case _                             => ()

    // === month / year views =================================================

    /** Prime's `.p-datepicker-month-view`: 12 month cells. Under `view(Month)`
      * a pick writes the `YYYY-MM` value and closes; while drilling from the day
      * view it writes the displayed `month` ref and drills back.
      */
    private def monthView(year: Int, snap: Snapshot, oref: Maybe[SignalRef[Boolean]])(using Frame): UI =
        val current = DatePicker.datePart(anchorValue(snap)).take(7)
        val cells: List[UI] = (1 to 12).toList.map { m =>
            val iso7 = DatePicker.pad(year, 4) + "-" + DatePicker.pad(m, 2)
            val outOfRange =
                minDateV.exists(min => iso7 < min.take(7)) || maxDateV.exists(max => iso7 > max.take(7))
            var cell = span.cssClass("p-datepicker-month")
            if current == iso7 then cell = cell.cssClass("p-datepicker-month-selected")
            val action: Maybe[Any < Async] =
                if outOfRange then Absent
                else if viewV == DatePickerView.Month then Present(pickGranular(iso7, oref))
                else drillTo(iso7, DatePickerView.Date)
            action match
                case Present(a) => cell = cell.onClick(a)
                case Absent     => cell = cell.cssClass("p-disabled").aria("disabled", "true")
            cell(DatePicker.monthNamesShort(m - 1))
        }
        div.cssClass("p-datepicker-month-view")(cells.map(toChild)*)
    end monthView

    /** Prime's `.p-datepicker-year-view`: the displayed decade's 10 year cells.
      * Under `view(Year)` a pick writes the `YYYY` value and closes; while
      * drilling it writes the `month` ref's year and drills to the month grid.
      */
    private def yearView(year: Int, month: Int, snap: Snapshot, oref: Maybe[SignalRef[Boolean]])(using Frame): UI =
        val current = DatePicker.datePart(anchorValue(snap)).take(4)
        val base    = year - Math.floorMod(year, 10)
        val cells: List[UI] = (base to base + 9).toList.map { y =>
            val iso4 = DatePicker.pad(y, 4)
            val outOfRange =
                minDateV.exists(min => iso4 < min.take(4)) || maxDateV.exists(max => iso4 > max.take(4))
            var cell = span.cssClass("p-datepicker-year")
            if current == iso4 then cell = cell.cssClass("p-datepicker-year-selected")
            val action: Maybe[Any < Async] =
                if outOfRange then Absent
                else if viewV == DatePickerView.Year then Present(pickGranular(iso4, oref))
                else drillTo(iso4 + "-" + DatePicker.pad(month, 2), DatePickerView.Month)
            action match
                case Present(a) => cell = cell.onClick(a)
                case Absent     => cell = cell.cssClass("p-disabled").aria("disabled", "true")
            cell(y.toString)
        }
        div.cssClass("p-datepicker-year-view")(cells.map(toChild)*)
    end yearView

    /** A granularity pick (`view(Month|Year)`): write the ISO prefix, close. */
    private def pickGranular(iso: String, oref: Maybe[SignalRef[Boolean]])(using Frame): Any < Async =
        for
            _ <- writeSingle(iso)
            _ <- fireChange(iso)
            r <- closeVia(oref)
        yield r

    /** A drill-down pick: write the displayed month and switch the view back.
      * Needs BOTH the `month` and `currentView` refs; `Absent` renders the cell
      * disabled instead.
      */
    private def drillTo(yearMonth: String, target: DatePickerView)(using Frame): Maybe[Any < Async] =
        (monthRef, currentViewRefV) match
            case (Present(mref), Present(vref)) =>
                Present(
                    for
                        _ <- mref.set(yearMonth)
                        r <- vref.set(target)
                    yield r
                )
            case _ => Absent

    // === time picker ========================================================

    /** Prime's `.p-datepicker-time-picker`: hour/minute columns of increment /
      * display / decrement, plus the AM/PM column under `hourFormat(H12)`. Only
      * the single-selection value model carries a time.
      */
    private def timePickerUI(snap: Snapshot)(using Frame): UI =
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
    private def buttonBarUI(snap: Snapshot, oref: Maybe[SignalRef[Boolean]])(using Frame): UI =
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
                    r <- selectDay(DatePicker.datePart(iso), snap, oref)
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
                _ <- wipeSingle
                _ <- wipeMulti
                _ <- wipeRange
                _ <- fireChange("")
                r <- closeVia(oref)
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
