package kyo.uic

import java.time.YearMonth
import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** DatePicker, native kyo-ui in the PrimeOne design. It mirrors Prime's anatomy (`div.p-datepicker.p-component` over
  * `input.p-datepicker-input.p-inputtext` and `button.p-datepicker-dropdown`, the floating `div.p-datepicker-panel` or its
  * `.p-datepicker-panel-inline` variant, the header, the `table.p-datepicker-day-view` day grid, the month and year grids, the
  * `.p-datepicker-time-picker` and the `.p-datepicker-buttonbar`), so the extracted `@primeuix` sheet applies verbatim.
  *
  * The value type decides what the picker is: `DatePicker[LocalDate]` picks a day, `[LocalDateTime]` a day and a time,
  * `[LocalTime]` a time alone, `[YearMonth]` a month and `[Year]` a year, each through its [[DateKind]] given, and
  * `DatePicker(DateKind.zoned(zone))` a day held as a `ZonedDateTime`. What it holds binds two-way as `Maybe[A]` through
  * `value`; for the day kinds, `values` turns it into a [[MultiDatePicker]] over `Set[A]` and `range` into a
  * [[RangeDatePicker]] over a start and an end, each with the one `onChange` and `onBlur` its value calls for. Every event
  * reports the typed value and fires only when a write changed what is held. A setter only some kinds can honour asks for
  * one of the [[DateKind]] capability markers, so it does not compile where it would mean nothing.
  *
  * The calendar is the REAL floating panel, built on [[Overlay]] with `seedFocus(false)`. Clicking the field or the dropdown
  * button opens it, a completed pick closes it in single mode, and Escape or an outside click closes it. Keyboard (the ARIA
  * date-grid pattern, over the pure [[CalendarNav]] machine): ArrowDown on the field opens the calendar and a second one steps
  * into the grid, which is ONE tab stop with a roving `.p-focus` highlight announced through `aria-activedescendant`. The
  * arrows step a day and a week, Home and End reach the ends of the week, PageUp and PageDown turn a month (a year with
  * Shift), Enter or Space picks, and Escape is a cancel that puts back what the panel held before the reader touched it.
  *
  * The field shows the held value as its ISO text and reads what a reader types back through the kind: text naming a value
  * within the bounds is written, an empty field clears, and anything else writes nothing and goes back to the held value
  * when the field loses focus.
  *
  * The render is pure: it never reads a clock, so the Today button renders only when the app supplies [[today]]. The month
  * shown comes from the `month` ref, else the held value, else [[referenceDate]], else January 2000; the mount mints the
  * month, view, open and cursor state a caller does not bind.
  *
  * `id` lands on the text field, and the live picker derives the day grid's `$id-grid` and its cells' `$id-c<n>`
  * from it (a minted base stands in where none is given).
  */
final case class DatePicker[A] private (
    kind: DateKind[A],
    bindingV: Maybe[DatePicker.Binding[A]] = Absent,
    monthSlot: Maybe[ReactiveValue[YearMonth]] = Absent,
    onMonthF: Maybe[YearMonth => UI.Action] = Absent,
    currentViewSlot: Maybe[ReactiveValue[DatePickerView]] = Absent,
    onCurrentViewF: Maybe[DatePickerView => UI.Action] = Absent,
    inlineFlag: Boolean = false,
    hourFormatV: HourFormat = HourFormat.H24,
    showButtonBarFlag: Boolean = false,
    todayV: Maybe[A] = Absent,
    placeholderText: Maybe[TextValue] = Absent,
    disabledFlag: Boolean = false,
    readonlyFlag: Boolean = false,
    requiredFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    minDateV: Maybe[A] = Absent,
    maxDateV: Maybe[A] = Absent,
    showWeekFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    openSlot: Maybe[ReactiveValue[Boolean]] = Absent,
    referenceDateV: Maybe[A] = Absent,
    onChangeF: Maybe[Maybe[A] => UI.Action] = Absent,
    onValuesChangeF: Maybe[Set[A] => UI.Action] = Absent,
    onRangeChangeF: Maybe[(Maybe[A], Maybe[A]) => UI.Action] = Absent,
    onOpenF: Maybe[Boolean => UI.Action] = Absent,
    onBlurF: Maybe[Maybe[A] => UI.Action] = Absent,
    onValuesBlurF: Maybe[Set[A] => UI.Action] = Absent,
    onRangeBlurF: Maybe[(Maybe[A], Maybe[A]) => UI.Action] = Absent,
    idV: Maybe[String] = Absent
) extends Node, OptionalFormControl[A], HasPlaceholder, HasAccessibleNameRef:
    type Self = DatePicker[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): DatePicker[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Binds the held value two-way to `ref`, `Absent` while nothing is picked: a pick, the time picker, the button bar and a
      * typed text write it back.
      */
    def value(v: Maybe[A] | Signal[Maybe[A]]): DatePicker[A] =
        copy(bindingV = Present(DatePicker.Binding.Single(ReactiveValue[Maybe[A]](v))))

    /** The same picker with several days picked at once (Prime's `selectionMode="multiple"`): a [[MultiDatePicker]] bound
      * two-way to the set. A day click toggles membership and the panel stays open. Everything set so far carries over except
      * the single-value binding and its `onChange` and `onBlur`, which hold one value and would never fire on a set.
      */
    def values(v: Set[A] | Signal[Set[A]])(using DateKind.Days[A]): MultiDatePicker[A] =
        MultiDatePicker(single.copy(bindingV = Present(DatePicker.Binding.Multi(ReactiveValue[Set[A]](v)))))

    /** The same picker picking a range (Prime's `selectionMode="range"`): a [[RangeDatePicker]] bound two-way to the start
      * and end days. The first pick writes `start` (clearing `end`), a pick on or after it completes the range and closes the
      * panel, an earlier pick restarts. Everything set so far carries over except the single-value binding and its events.
      */
    def range(start: Maybe[A] | Signal[Maybe[A]], end: Maybe[A] | Signal[Maybe[A]])(using DateKind.Days[A]): RangeDatePicker[A] =
        RangeDatePicker(single.copy(bindingV =
            Present(DatePicker.Binding.Range(ReactiveValue[Maybe[A]](start), ReactiveValue[Maybe[A]](end)))
        ))

    /** This picker without its single-value binding and events, which is what a set or range picker is built from. */
    private def single: DatePicker[A] = copy(bindingV = Absent, onChangeF = Absent, onBlurF = Absent)

    /** Binds the DISPLAYED month: the header's prev and next buttons move it (by a month, a year or a decade, as the grid on
      * screen counts), and so do the page keys and any arrow that steps out of the page. In the month grid its year is the year
      * shown, in the year grid its decade. A `SignalRef` binds two way and [[onMonthChange]] hears every page; a plain
      * `Signal` binds one way and the picker only reports; a constant nobody hears is fixed, the page buttons inert. Bind it
      * only to read or drive the page from outside: unbound, the mount keeps the page itself, so navigating is not something
      * a caller has to opt into. A signal nothing hears is reported in a card.
      */
    def month(v: YearMonth | Signal[YearMonth])(using DateKind.Calendar[A]): DatePicker[A] = copy(monthSlot = Present(ReactiveValue(v)))

    /** Fired with the month the reader pages to, after a two-way [[month]] is written. */
    def onMonthChange(f: YearMonth => UI.Action)(using Frame, DateKind.Calendar[A]): DatePicker[A] =
        copy(onMonthF = Present(Callback.of(f)))

    /** Renders the calendar panel IN-FLOW below the field instead of floating (Prime's `inline` prop,
      * `.p-datepicker-panel-inline`): always visible without an [[open]] ref, toggled by the dropdown button with one.
      */
    def inline(v: Boolean): DatePicker[A] = copy(inlineFlag = v)

    /** Binds the grid on screen, kyo's server-honest stand-in for Prime's internal `currentView` state: the title's month
      * and year buttons drill into the coarser grids, and a pick there drills back. A `SignalRef` binds two way and
      * [[onCurrentViewChange]] hears every drill; a plain `Signal` binds one way and the picker only reports; a constant
      * nobody hears is fixed, the title buttons inert. Unbound, the mount keeps the state itself, starting at the grid the
      * kind picks in (the day grid, or the month grid for `YearMonth`). A signal nothing hears is reported in a card.
      */
    def currentView(v: DatePickerView | Signal[DatePickerView])(using DateKind.Drill[A]): DatePicker[A] =
        copy(currentViewSlot = Present(ReactiveValue(v)))

    /** Fired with the grid the reader drills to, after a two-way [[currentView]] is written. */
    def onCurrentViewChange(f: DatePickerView => UI.Action)(using Frame, DateKind.Drill[A]): DatePicker[A] =
        copy(onCurrentViewF = Present(Callback.of(f)))

    /** Hour display format: `H12` shows 12-hour hours plus Prime's AM/PM picker (the held value is the same either way).
      * Default `H24`.
      */
    def hourFormat(v: HourFormat)(using DateKind.Clock[A]): DatePicker[A] = copy(hourFormatV = v)

    /** Renders Prime's button bar (`.p-datepicker-buttonbar`) with Today and Clear. Clear empties the bound selection and
      * closes the panel. Today needs a date and the render is pure (no clock), so the Today button renders only when the app
      * supplies [[today]].
      */
    def showButtonBar(v: Boolean): DatePicker[A] = copy(showButtonBarFlag = v)

    /** The app-supplied "now" for the button bar's Today button, which shows its page and picks it: the whole value in single
      * mode (a time kind takes its time too), the day in multiple and range mode. Outside the bounds it renders disabled.
      */
    def today(v: A): DatePicker[A] = copy(todayV = Present(v))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): DatePicker[A] = copy(placeholderText = v)

    def disabled(v: Boolean): DatePicker[A] = copy(disabledFlag = v)

    /** Native `readonly` on the field; the calendar cannot be opened through the dropdown button either. */
    def readonly(v: Boolean): DatePicker[A] = copy(readonlyFlag = v)

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): DatePicker[A] = copy(requiredFlag = v)

    /** Native `name` for HTML form participation. */
    def name(v: String): DatePicker[A] = copy(nameV = Present(v))

    /** Earliest value the picker writes. Earlier day, month and year cells render disabled; a time picker cannot step below
      * it (for a `LocalDateTime`, on its day); typed text before it is not taken.
      */
    def minDate(v: A): DatePicker[A] = copy(minDateV = Present(v))

    /** Latest value the picker writes, the mirror of [[minDate]]. */
    def maxDate(v: A): DatePicker[A] = copy(maxDateV = Present(v))

    /** Shows the ISO 8601 week-number column (`td.p-datepicker-weeknumber`; hidden by default, as Prime's `showWeek=false`). */
    def showWeek(v: Boolean)(using DateKind.Weeks[A]): DatePicker[A] = copy(showWeekFlag = v)

    /** Size of the field: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): DatePicker[A] = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): DatePicker[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-datepicker-fluid`). */
    def fluid(v: Boolean): DatePicker[A] = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): DatePicker[A]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): DatePicker[A]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): DatePicker[A] = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): DatePicker[A] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): DatePicker[A] = copy(accNameRefV = v)

    /** Binds the calendar's visibility. A `SignalRef` binds two way: the field, the dropdown button, a completing pick,
      * the button bar, Escape and an outside click write it, then [[onOpenChange]] hears the new state. A plain `Signal`
      * binds one way: the calendar follows it and the picker only reports. A constant nobody hears is fixed: `false`
      * draws the picker read-only, `true` keeps the calendar open whatever the reader does. Unbound, the picker keeps the
      * state itself, closed (an [[inline]] calendar open). A signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): DatePicker[A] = copy(openSlot = Present(ReactiveValue(v)))

    /** The page to show while neither a `month` ref nor a held value says which. Keeps the render pure. */
    def referenceDate(v: A)(using DateKind.Calendar[A]): DatePicker[A] = copy(referenceDateV = Present(v))

    /** Fired with the value now held after a write that changed it (`Absent` once cleared). A pick of the value already held
      * fires nothing. The undo of an Escape writes a two-way binding back without firing; a one-way binding took every pick
      * from this event, so it hears the value to put back through it too.
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): DatePicker[A] = copy(onChangeF = Present(Callback.of(f)))

    /** Package-internal: the events [[MultiDatePicker]] and [[RangeDatePicker]] set. */
    private[uic] def withChangeSet(f: Set[A] => UI.Action)(using Frame): DatePicker[A] = copy(onValuesChangeF = Present(Callback.of(f)))
    private[uic] def withBlurSet(f: Set[A] => UI.Action)(using Frame): DatePicker[A]   = copy(onValuesBlurF = Present(Callback.of(f)))
    private[uic] def withChangeRange(f: (Maybe[A], Maybe[A]) => UI.Action)(using Frame): DatePicker[A] =
        copy(onRangeChangeF = Present(Callback.of2(f)))
    private[uic] def withBlurRange(f: (Maybe[A], Maybe[A]) => UI.Action)(using Frame): DatePicker[A] =
        copy(onRangeBlurF = Present(Callback.of2(f)))

    /** Fired with the visibility the reader asks for, true when the field or the dropdown button opens the calendar and
      * false when the button, a completing pick, the button bar, Escape or an outside click closes it, after a two-way
      * [[open]] is written.
      * One event for both, since an open and a close are never both. External writes fire nothing.
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): DatePicker[A] = copy(onOpenF = Present(Callback.of(f)))

    /** Fires on focus loss with the value held; unlike `onChange` it fires whether or not anything was edited. The
      * form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): DatePicker[A] = copy(onBlurF = Present(Callback.of(f)))

    // === render =============================================================

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

    private val minIso: Maybe[String] = minDateV.map(kind.toIso)
    private val maxIso: Maybe[String] = maxDateV.map(kind.toIso)

    /** Whether `iso` (at the kind's own precision) lies within the bounds. ISO strings of one kind compare as their values. */
    private def inBounds(iso: String): Boolean = minIso.forall(iso >= _) && maxIso.forall(iso <= _)

    /** `iso` moved into the bounds. */
    private def clamp(iso: String): String =
        minIso.filter(iso < _).orElse(maxIso.filter(iso > _)).getOrElse(iso)

    /** The keyboard state the mount allocates, and the ids it addresses cells by.
      *
      * A caller's own `month`, `currentView` and `open` refs win; the mount mints what the caller did not bind, including the
      * held value itself, so a picker read only through `onChange` still has somewhere to keep its pick. The cursor is the cell
      * the keyboard is on, as an ISO string at the grid's own precision. Cell ids are POSITIONAL (`base-c<n>`), so a page
      * change patches the cells already in the document rather than inserting new ones.
      */
    private[uic] def render(using Frame): UI =
        // A calendar bound shut by a constant nobody hears: nothing the reader does may open it, so the picker is read-only.
        val openFixedShut = ReactiveValue.fixed(openSlot, onOpenF) && openSlot.flatMap(_.const).contains(false)
        // A value held by a constant nobody hears is fixed too: drawn read-only, reachable and readable.
        val valueFixed = bindingV.exists(_.fixed(valueHeard))
        if (openFixedShut || valueFixed) && !readonlyFlag then copy(readonlyFlag = true).render
        // A calendar bound one way that nothing hears: every open and close the reader asks for is painted away again.
        else
            // A slot bound one way that nothing hears: every change the reader asks for there is painted away again.
            val unheard =
                (if bindingV.exists(_.deaf(valueHeard)) then List("the value is bound one way and nothing hears a pick; add onChange")
                 else Nil) ++
                    (if ReactiveValue.deaf(openSlot, onOpenF) then
                         List("the calendar is bound one way and nothing hears an open or a close; add onOpenChange")
                     else Nil) ++
                    (if ReactiveValue.deaf(monthSlot, onMonthF) then
                         List("the month is bound one way and nothing hears a page; add onMonthChange")
                     else Nil) ++
                    (if ReactiveValue.deaf(currentViewSlot, onCurrentViewF) then
                         List("the view is bound one way and nothing hears a drill; add onCurrentViewChange")
                     else Nil)
            if unheard.isEmpty then renderLive
            else
                fragment(
                    (unheard.map(advice => KeyDiagnostics.card("DatePicker", advice + ", or bind a SignalRef", Nil)) :+ renderLive)*
                )
            end if
        end if
    end render

    private def renderLive(using Frame): UI =
        // The static projection (SSG, the SSR page GET) is the same anatomy, inert: an inline
        // calendar still draws its month, a floating one still draws its closed field.
        val stat: UI =
            if inlineFlag then
                val panel = openSlot match
                    case Present(o) =>
                        o.const match
                            case Present(v) => if v then calendarStatic else UI.empty
                            case Absent     => when(o.signal)(calendarStatic)
                    case Absent => calendarStatic
                assemble(openSlot, Absent, floating = false, panel)
            else assemble(Absent, Absent, floating = true, UI.empty)
        if !interactive then stat
        else
            UI.mounted {
                // The held value of an unbound picker is the mount's own; its equality is the value type's `equals`, which
                // is what every kind's value type defines.
                given CanEqual[A, A] = CanEqual.derived
                for
                    cmds <- UI.commands
                    // A caller's own id is the base the grid and its cells derive from; a minted one stands in.
                    base <- idV.map(v => Kyo.lift(v)).getOrElse(cmds.freshId)
                    held <- bindingV match
                        case Present(b) => Kyo.lift(b)
                        case Absent     =>
                            Signal.initRef(Maybe.empty[A]).map(r =>
                                DatePicker.Binding.Single(ReactiveValue[Maybe[A]](r)): DatePicker.Binding[A]
                            )
                    // Unbound, the picker keeps the calendar's visibility itself: closed, or open when it is inline.
                    oref <- openSlot match
                        case Present(o) => Kyo.lift(o)
                        case Absent     => Signal.initRef(inlineFlag).map(r => ReactiveValue[Boolean](r))
                    mref <- monthSlot match
                        case Present(m) => Kyo.lift(DatePicker.ShownMonth.Bound(m, onMonthF))
                        case Absent     => Signal.initRef("").map(DatePicker.ShownMonth.Own(_))
                    vref <- currentViewSlot match
                        case Present(v) => Kyo.lift(v)
                        case Absent     => Signal.initRef(kind.view).map(r => ReactiveValue[DatePickerView](r))
                    cur     <- Signal.initRef("")
                    seed    <- Signal.initRef(false)
                    restore <- Signal.initRef(Maybe.empty[DatePicker.Restore])
                    draft   <- Signal.initRef(Maybe.empty[String])
                    refs = DatePicker.Refs(oref, mref, cur, vref, seed, restore, draft)
                yield copy(bindingV = Present(held)).wired(refs, base, id => cmds.focusId(id))
                end for
            }.placeholder(stat)
        end if
    end renderLive

    /** The host the mount publishes, and the seam golden tests render directly (a full top-down re-render shows mounted
      * regions as placeholders).
      *
      * The FIELD is built outside the open ref's subscription: only the panel and the root's anchor class follow it. That is
      * what makes the focus return on Escape safe, since the element focus goes back to is never one a close replaces.
      */
    private[uic] def wired(refs: DatePicker.Refs, base: String, focus: String => UI.Action)(using Frame): UI =
        // The mount mints a view ref when the caller binds none, the way it mints the month one. Without it the month and year
        // buttons rendered disabled and a drill-down had nothing to write.
        copy(currentViewSlot = Present(refs.view)).wiredWith(refs, base, focus)

    private def wiredWith(refs: DatePicker.Refs, base: String, focus: String => UI.Action)(using Frame): UI =
        val oref                                                     = refs.open
        val mref                                                     = refs.month
        val cursor                                                   = refs.cursor
        val seed                                                     = refs.seed
        def nav(mv: String, cv: String, sd: Boolean): DatePicker.Nav =
            DatePicker.Nav(mref, mv, cursor, cv, seed, sd, refs.restore, refs.draft, base, focus, idV)
        // Whether it is open, the month, the cursor, the seed flag, the view and the selection are read as ONE region (see
        // [[Reading]]): a pick writes several of them and closes, and a nested region would draw the grid against the values
        // it closed over. The panel's presence is decided here too, so the Overlay is host-gated (`renderShown`).
        val calendar =
            Reading.of(oref).and(Reading.Live(mref.signal)).and(Reading.Live(cursor)).and(Reading.Live(seed)).and(calendarState)
        val panel: UI =
            calendar.render { case ((((open, mv), cv), sd), (view, snap)) =>
                if !open then UI.empty
                else
                    val n    = nav(mv, cv, sd)
                    val grid =
                        panelUI(view, if mv.isEmpty then Absent else Present(mv), snap, Present(oref), Present(n), inline = inlineFlag)
                    if inlineFlag then grid else Overlay.hosted(oref, onOpenF).seedFocus(false)(grid).renderShown(Present(s"$base-panel"))
                end if
            }
        assemble(Present(oref), Present(nav("", "", false)), floating = !inlineFlag, panel, openSignal = Present(oref))
    end wiredWith

    /** Field + dropdown button + panel host inside the `.p-datepicker` root. */
    private def assemble(
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav],
        floating: Boolean,
        panel: UI,
        openSignal: Maybe[ReactiveValue[Boolean]] = Absent
    )(using Frame): UI =
        val validity  = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val field: UI = bindingV match
            case Present(single @ DatePicker.Binding.Single(_)) =>
                // The typed text and a rejected draft are read together, as one region: the draft is what puts a rejected
                // text back once the field loses focus.
                val held = single.snapshot(kind).map(DatePicker.anchorOf)
                nav match
                    case Present(n) =>
                        held.combineLatest(n.draft).render { pair =>
                            val (text, draft) = pair
                            fieldUI(draft.getOrElse(text), editable = true, oref, nav, floating, validity)
                        }
                    case Absent => held.render(text => fieldUI(text, editable = true, oref, nav, floating, validity))
                end match
            case Present(b) =>
                b.snapshot(kind).render(snap => fieldUI(DatePicker.displayText(snap), editable = false, oref, nav, floating, validity))
            case Absent => fieldUI("", editable = true, oref, nav, floating, validity)

        // The dropdown button toggles the calendar, firing onOpen/onClose on actual transitions. Inline without an open ref
        // keeps it disabled (the always-on panel has nothing to toggle); the floating static projection renders it
        // enabled-looking but inert until the mount wires it. A read-only picker (readonly, or fixed by a constant nobody
        // hears) keeps it a tab stop that says `aria-disabled` and takes no click: readonly is not disabled, which stays
        // native.
        var dd = button
            .cssClass("p-datepicker-dropdown")
            .jsProp("type", "button")
            .aria("label", "Choose date")
        if disabledFlag then dd = dd.disabled(true)
        else if readonlyFlag then dd = dd.aria("disabled", "true")
        else if !floating && oref.isEmpty then dd = dd.disabled(true)
        else oref.foreach(r => dd = dd.onClick(seedGrid(nav).andThen(togglePanel(r))))
        end if
        val dropdown: UI = dd(toChild(GlyphSvg(Icons.calendar, "p-icon")))

        var el = div.cssClass("p-datepicker").cssClass("p-component")
        el = validity.markClass(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        if fluidFlag then el = el.cssClass("p-datepicker-fluid")
        // Reactive rather than a render-time branch: the anchor class was the only thing the field's own subtree needed the
        // open state for, and re-rendering the field on every open made the focus return on Escape a race.
        openSignal match
            case Present(sig) if floating => el = el.cssClass("p-uic-overlay-anchor", sig.bound)
            case _                        => ()
        validity.follow(el(toChild(field), toChild(dropdown), toChild(panel)))
    end assemble

    /** The text field. Under a [[value]] binding it shows the held value and reads a typed text back (`editable`); under
      * [[values]] and [[range]] it shows a derived text and is read-only. In the floating host a click on the field opens the
      * panel (Prime's `showOnFocus` default) and Escape closes it.
      */
    private def fieldUI(
        text: String,
        editable: Boolean,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav],
        floating: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        var i = input.cssClass("p-datepicker-input").cssClass("p-inputtext").cssClass("p-component")
        // The live render stamps `fieldId` below, which is this same id where the caller gave one.
        i = idV.map(v => i.id(v)).getOrElse(i)
        sizeV match
            case Size.Small  => i = i.cssClass("p-inputtext-sm")
            case Size.Large  => i = i.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then i = i.cssClass("p-variant-filled")
        i = validity.markAria(i)
        i = i.value(text)
        i = placeholderText match
            case Present(TextValue.Const(v)) => i.placeholder(v)
            case Present(TextValue.Dyn(sig)) => i.placeholder(sig)
            case Absent                      => i
        if disabledFlag then i = i.disabled(true)
        if readonlyFlag || !editable then i = i.readOnly(true)
        if requiredFlag then i = i.aria("required", "true").jsProp("required", "true")
        i = nameV.map(v => i.jsProp("name", v)).getOrElse(i)
        i = accNameV match
            case Present(TextValue.Const(v)) => i.aria("label", v)
            case Present(TextValue.Dyn(s))   => i.aria("label", s)
            case Absent                      => i
        i = accNameRefV.map(v => i.aria("labelledby", v)).getOrElse(i)
        nav.foreach { n =>
            if editable && interactive then i = i.onChange(typed => commitText(typed, n))
            // The blur reports what is held, read from the bound refs, not the text in the field (which a blur could carry):
            // typed text becomes a value only through a change that parses. A rejected text goes back to the held value
            // here, which is the moment the reader is done with it.
            val blur: UI.Action = n.draft.set(Absent).andThen(blurReport)
            i = i.onBlur(blur)
            i = i.id(n.fieldId)
        }
        if floating && interactive then
            oref.foreach { r =>
                // Opening puts the reader in the grid, however the panel was opened: that is where the calendar's keys are.
                // The cost is the text field: while the panel is open the caret is not in it, so typing a value by hand
                // means Escape (or Shift+Tab) first.
                i = i.onClick(seedGrid(nav).andThen(openPanel(r)))
                i = i.onKeyDown { e =>
                    // ArrowDown opens, and nothing else does. This is a TEXT BOX: Space types a space and Enter belongs to
                    // the form. Once open, ArrowDown a second time steps into the grid.
                    val eff: UI.Action = e.key match
                        case Keyboard.Escape    => cancelVia(Present(r), nav)
                        case Keyboard.ArrowDown => enterGrid(r, nav)
                        case _                  => ()
                    eff
                }
            }
        end if
        i
    end fieldUI

    /** What `onBlur` (or its set or range form) reports: what the binding holds at the moment focus leaves. */
    private def blurReport(using Frame): UI.Action =
        bindingV match
            case Present(DatePicker.Binding.Single(r)) =>
                onBlurF match
                    case Present(f) => r.current.map(f)
                    case Absent     => ()
            case Present(DatePicker.Binding.Multi(r)) =>
                onValuesBlurF match
                    case Present(f) => r.current.map(f)
                    case Absent     => ()
            case Present(DatePicker.Binding.Range(s, e)) =>
                onRangeBlurF match
                    case Present(f) => s.current.map(sv => e.current.map(ev => f(sv, ev)))
                    case Absent     => ()
            case Absent =>
                onBlurF match
                    case Present(f) => f(Absent)
                    case Absent     => ()

    /** A typed text, committed by the field's `change`: an empty field clears, a text the kind reads as a value within the
      * bounds is written, and anything else writes nothing and is kept as a draft until the field loses focus.
      */
    private def commitText(typed: String, nav: DatePicker.Nav)(using Frame): UI.Action =
        val trimmed            = typed.trim
        val iso: Maybe[String] =
            if trimmed.isEmpty then Present("")
            else kind.fromIso(trimmed).map(kind.toIso).filter(inBounds)
        iso match
            case Present(v) => nav.draft.set(Absent).andThen(putValue(v))
            case Absent     => nav.draft.set(Present(typed))
    end commitText

    /** ArrowDown on the field: opens the calendar and puts the reader in the grid.
      *
      * A focus COMMAND cannot do the opening half of that, since the grid does not exist until the panel is rendered and the
      * command would race the insert. So the press raises the seed flag instead, the grid renders `focusAuto` while it is up,
      * and the client focuses the grid in the same patch that inserts it.
      */
    private def enterGrid(oref: ReactiveValue[Boolean], nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        for
            _      <- seedGrid(nav)
            isOpen <- oref.current
            _      <-
                if !isOpen then openPanel(oref)
                else
                    nav match
                        case Present(n) => n.focus(n.gridId)
                        case Absent     => (): UI.Action
        yield ()

    /** Raises the flag that says the next grid to appear takes the focus.
      *
      * It starts down and is never lowered: what it keeps out is the page's own first paint, where an in-flow calendar would
      * otherwise take the focus off the document the moment it loaded.
      */
    private def seedGrid(nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        nav match
            case Present(n) => n.seed.set(true)
            case Absent     => ()

    /** Asks for the other visibility: written into a two-way binding, then reported to `onOpenChange`. */
    private def togglePanel(oref: ReactiveValue[Boolean])(using Frame): UI.Action =
        ReactiveValue.update(Present(oref), !_, onOpenF)

    /** Asks for the calendar open, where it is not already. */
    private def openPanel(oref: ReactiveValue[Boolean])(using Frame): UI.Action =
        oref.current.map(isOpen => if isOpen then (): UI.Action else ReactiveValue.change(Present(oref), true, onOpenF))

    /** Whether the binding's own event is set: `onChange`, or its set or range form. */
    private def valueHeard: Boolean =
        bindingV match
            case Present(DatePicker.Binding.Single(_))   => onChangeF.isDefined
            case Present(DatePicker.Binding.Multi(_))    => onValuesChangeF.isDefined
            case Present(DatePicker.Binding.Range(_, _)) => onRangeChangeF.isDefined
            case Absent                                  => false

    // === writes =============================================================

    /** Writes the value `iso` names into a [[value]] binding (`""` clears) and fires `onChange` when that changed it. */
    private def putValue(iso: String)(using Frame): UI.Action =
        bindingV match
            case Present(DatePicker.Binding.Single(r)) =>
                DatePicker.Binding.put(r, kind, iso).map {
                    case Present(now) => fireValue(now)
                    case Absent       => (): UI.Action
                }
            case _ => ()

    private def fireValue(now: Maybe[A])(using Frame): UI.Action =
        onChangeF match
            case Present(f) => f(now)
            case Absent     => ()

    private def fireValues(now: Set[A])(using Frame): UI.Action =
        onValuesChangeF match
            case Present(f) => f(now)
            case Absent     => ()

    private def fireRange(now: (Maybe[A], Maybe[A]))(using Frame): UI.Action =
        onRangeChangeF match
            case Present(f) => f(now._1, now._2)
            case Absent     => ()

    /** Asks for the calendar closed through the EFFECTIVE open binding (bound or mount-allocated), where it is open. */
    private def closeVia(oref: Maybe[ReactiveValue[Boolean]])(using Frame): UI.Action =
        oref match
            case Present(r) =>
                r.current.map(isOpen => if isOpen then ReactiveValue.change(Present(r), false, onOpenF) else (): UI.Action)
            case Absent => ()

    /** Closes the panel and hands the focus back to the field.
      *
      * A pick destroys what the reader is standing on: the grid they arrowed through, or the button bar's button they
      * pressed. Without the return the focus falls to the document, and the next Tab starts at the top of the page.
      *
      * An in-flow calendar is left alone entirely: it is the page's own furniture rather than a panel over it, so a pick is
      * not a reason to take it away and there is nothing to hand back.
      */
    private def closeAndReturn(oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        if inlineFlag then ()
        else
            for
                _ <- closeVia(oref)
                _ <- forget(nav)
                r <- nav match
                    case Present(n) => n.focus(n.fieldId)
                    case Absent     => (): UI.Action
            yield r

    /** Records what the binding holds, unless this open already recorded it.
      *
      * Every write inside the panel goes through here first, which is what makes Escape a cancel: the memory is taken before
      * the first change rather than at the open, so it does not matter who opened the panel.
      */
    private def remember(nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        (nav, bindingV) match
            case (Present(n), Present(b)) =>
                n.restore.use {
                    case Present(_) => (): UI.Action
                    case Absent     =>
                        b.capture(kind, fireValue, fireValues, (a, b) => fireRange((a, b)))
                            .map(rs => n.restore.set(Present(rs)))
                }
            case _ => ()

    /** Drops the memory, which a close that commits has no more use for. */
    private def forget(nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        nav match
            case Present(n) => n.restore.set(Absent)
            case Absent     => ()

    /** Escape: put back what the panel held before the reader changed anything in it, then close and hand the focus back.
      * The refs carry the restored values, and no change callback fires, because over the whole open nothing changed.
      */
    private def cancelVia(oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        if inlineFlag then ()
        else
            val undo: UI.Action = nav match
                case Absent     => ()
                case Present(n) =>
                    n.restore.use {
                        case Present(rs) => rs.put
                        case Absent      => (): UI.Action
                    }
            undo.andThen(closeAndReturn(oref, nav))

    // === reactive panel =====================================================

    /** The inline calendar of the static projection, inert. Without a mount a caller's own `month(ref)` is the only month
      * there is, read with the view and the selection as ONE region.
      */
    private def calendarStatic(using Frame): UI =
        val month: Reading[Maybe[String]] =
            monthSlot.fold(Reading.Fixed(Maybe.empty[String])) { m =>
                Reading.Live(DatePicker.ShownMonth.Bound(m, onMonthF).signal.map(iso => Present(iso)))
            }
        calendarState.and(month).render { case ((view, snap), mOv) => panelUI(view, mOv, snap, Absent, Absent, inline = true) }
    end calendarStatic

    /** The view and the selection the calendar reads, as one [[Reading]] the host reads beside its month. */
    private def calendarState(using Frame): Reading[(DatePickerView, DatePicker.Snapshot)] =
        val view = currentViewSlot.fold(Reading.Fixed(kind.view))(Reading.of(_))
        val snap = bindingV.fold(Reading.Fixed[DatePicker.Snapshot](DatePicker.Snapshot.Single("")))(b => Reading.Live(b.snapshot(kind)))
        view.and(snap)
    end calendarState

    /** Builds the panel for the derived month and current view, the SAME render for both hosts; only the variant class
      * differs (`-inline` in-flow vs the bare floating panel inside the Overlay).
      */
    private def panelUI(
        view: DatePickerView,
        monthOverride: Maybe[String],
        snap: DatePicker.Snapshot,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav],
        inline: Boolean
    )(using Frame): UI =
        val (year, month) = deriveMonth(DatePicker.anchorOf(snap), monthOverride)

        val calendarChildren: List[UI] =
            if kind.timeOnly then Nil
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
            if kind.timeOnly || (kind.withTime && view == DatePickerView.Date) then List(timePickerUI(snap, nav))
            else Nil

        val barChildren: List[UI] = if showButtonBarFlag then List(buttonBarUI(snap, oref, nav)) else Nil

        var panel = div
            .cssClass("p-datepicker-panel")
            .cssClass("p-component")
        if inline then panel = panel.cssClass("p-datepicker-panel-inline")
        if kind.timeOnly then panel = panel.cssClass("p-datepicker-timeonly")
        panel((calendarChildren ++ timeChildren ++ barChildren).map(toChild)*)
    end panelUI

    /** The page the month override and the header buttons write: the minted ref in a mounted picker, else the caller's own. */
    private def shownMonth(nav: Maybe[DatePicker.Nav]): Maybe[DatePicker.ShownMonth] =
        nav.map(_.monthOv).orElse(monthSlot.map(DatePicker.ShownMonth.Bound(_, onMonthF)))

    /** Prime's calendar header: prev button, the month/year title BUTTONS (they drill into the month and year grids through
      * the view ref), next button. The nav buttons shift the month by a month, a year or a decade depending on the view.
      *
      * The mount mints both refs when the caller binds none, so every button here is live in a mounted picker. They render
      * disabled only in the static projection, which is inert throughout.
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
            shownMonth(nav) match
                // A page fixed by a constant nobody hears: the button stays a tab stop and says it changes nothing.
                case Present(_) if ReactiveValue.fixed(monthSlot, onMonthF) => b = b.aria("disabled", "true")
                case Present(mref)                                          =>
                    b = b.onClick(mref.set(DatePicker.pad(target._1, 4) + "-" + DatePicker.pad(target._2, 2)))
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
            currentViewSlot match
                // A view fixed by a constant nobody hears: the button stays a tab stop and says it changes nothing.
                case Present(_) if ReactiveValue.fixed(currentViewSlot, onCurrentViewF) => b = b.aria("disabled", "true")
                // The switch redraws the header without the button that was pressed, so the focus goes on to the grid that
                // appeared rather than falling to the document.
                case Present(_) =>
                    b = b.onClick(seedGrid(nav).andThen(askView(target)).andThen(focusGrid(nav)))
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

    /** Prime's `table.p-datepicker-day-view`: a weekday header row over full weeks of day cells; leading and trailing cells
      * show the adjacent months' days (`.p-datepicker-other-month`, non-interactive).
      */
    private def dayView(
        year: Int,
        month: Int,
        snap: DatePicker.Snapshot,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(using Frame): UI =
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
        // Where the keyboard is. A cursor outside the displayed month is not this grid's, so it falls back to the selected
        // day and then to the first, which is what a reader who has just paged into a month means by "here".
        val cursorIso      = dayCursor(year, month, snap, nav)
        val days: List[UI] = (1 to dim).toList.map { d =>
            val iso = DatePicker.isoDate(year, month, d)
            // A day is out of bounds when all of it is: a bound with a time of day still leaves its own day pickable, and the
            // pick is clamped to the bound.
            val outOfRange =
                minIso.exists(min => iso < DatePicker.datePart(min)) || maxIso.exists(max => iso > DatePicker.datePart(max))
            var daySpan = span.cssClass("p-datepicker-day")
            if outOfRange then daySpan = daySpan.cssClass("p-disabled").aria("disabled", "true")
            else
                daySpan = daySpan.onClick(seedCursor(iso, nav).andThen(selectDay(iso, snap, oref, nav)))
                dayStateClass(iso, snap).foreach(c => daySpan = daySpan.cssClass(c))
            end if
            if iso == cursorIso then daySpan = daySpan.cssClass("p-focus").scrollAuto(true)
            // Every cell answers to an id, so the attribute that names the highlight points at something already in the
            // document whichever day it moves to. The id is on the CELL, which carries the role; the class is on the day.
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

        // ONE tab stop with the arrows inside it, and the highlight announced rather than focused: a day is a `<span>`, not a
        // control of its own. Nothing here moves DOM focus, which is also why nothing here can race the render that draws the
        // month it moved into.
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
    private def dayCursor(year: Int, month: Int, snap: DatePicker.Snapshot, nav: Maybe[DatePicker.Nav]): String =
        val prefix = DatePicker.pad(year, 4) + "-" + DatePicker.pad(month, 2)
        // Length as well as prefix: the cursor is shared with the month and year grids, so after a drill it can still hold a
        // `YYYY-MM`, which starts with the prefix and is no day.
        val fromCursor = nav.map(_.cursorV).filter(c => c.length == 10 && c.startsWith(prefix))
        if fromCursor.isDefined then fromCursor.getOrElse("")
        else
            val selected = DatePicker.datePart(DatePicker.anchorOf(snap))
            if selected.startsWith(prefix) then selected
            else if nav.isDefined then prefix + "-01"
            else ""
        end if
    end dayCursor

    /** One key over the day grid, through [[CalendarNav]].
      *
      * Every move is expressed in days and applied with the same arithmetic the grid is drawn from, so crossing a week
      * boundary lands on the next day and crossing a month boundary carries the displayed month with it. `Page` is a month,
      * and its Shift form a year.
      */
    private def dayKey(
        year: Int,
        month: Int,
        cursorIso: String,
        snap: DatePicker.Snapshot,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: DatePicker.Nav
    )(e: KeyboardEvent)(using Frame): UI.Action =
        CalendarNav.onKey(e.key, e.modifiers, perRow = 7) match
            case Absent        => ()
            case Present(step) =>
                DatePicker.parseDate(cursorIso) match
                    case Absent                => ()
                    case Present((cy, cm, cd)) =>
                        import CalendarNav.Step.*
                        val target: Maybe[(Int, Int, Int)] = step match
                            case Move(cells)        => Present(DatePicker.addDays(cy, cm, cd, cells))
                            case RowStart           => Present(DatePicker.addDays(cy, cm, cd, -DatePicker.dayOfWeek(cy, cm, cd)))
                            case RowEnd             => Present(DatePicker.addDays(cy, cm, cd, 6 - DatePicker.dayOfWeek(cy, cm, cd)))
                            case Page(delta, big)   => Present(DatePicker.addMonths(cy, cm, cd, if big then delta * 12 else delta))
                            case Activate | Dismiss => Absent
                        val moved: UI.Action = target match
                            case Present((ny, nm, nd)) => moveDayCursor(ny, nm, nd, nav)
                            case Absent                => ()
                        val acted: UI.Action = step match
                            case Activate => selectDayIfInBounds(cursorIso, snap, oref, Present(nav))
                            case Dismiss  => cancelVia(oref, Present(nav))
                            case _        => ()
                        moved.andThen(acted)
    end dayKey

    /** Enter on a day the bounds rule out does what a click on its disabled cell does: nothing. */
    private def selectDayIfInBounds(
        iso: String,
        snap: DatePicker.Snapshot,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(using Frame): UI.Action =
        val outOfRange =
            minIso.exists(min => iso < DatePicker.datePart(min)) || maxIso.exists(max => iso > DatePicker.datePart(max))
        if outOfRange then () else selectDay(iso, snap, oref, nav)
    end selectDayIfInBounds

    /** Hands the focus to the grid on screen, which is where a view switch leaves the reader. */
    private def focusGrid(nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        nav match
            case Present(n) => n.focus(n.gridId)
            case Absent     => ()

    /** A click on a cell seeds the highlight, so the next arrow carries on from the cell the pointer named. */
    private def seedCursor(iso: String, nav: Maybe[DatePicker.Nav])(using Frame): UI.Action =
        nav match
            case Present(n) => n.cursor.set(iso)
            case Absent     => ()

    /** Writes the cursor and, when the move left the displayed month, the month with it. */
    private def moveDayCursor(y: Int, m: Int, d: Int, nav: DatePicker.Nav)(using Frame): UI.Action =
        val iso  = DatePicker.isoDate(y, m, d)
        val ym   = DatePicker.pad(y, 4) + "-" + DatePicker.pad(m, 2)
        val page = if nav.monthOvV == ym then (): UI.Action else nav.monthOv.set(ym)
        nav.cursor.set(iso).andThen(page)
    end moveDayCursor

    /** Prime's day state classes: endpoints and picked days get `-day-selected`, strictly-in-range days of a complete range
      * get `-day-selected-range`.
      */
    private def dayStateClass(iso: String, snap: DatePicker.Snapshot): Maybe[String] = snap match
        case DatePicker.Snapshot.Single(v) =>
            if v.nonEmpty && DatePicker.datePart(v) == iso then Present("p-datepicker-day-selected") else Absent
        case DatePicker.Snapshot.Multi(set) =>
            if set.contains(iso) then Present("p-datepicker-day-selected") else Absent
        case DatePicker.Snapshot.Range(s, e) =>
            if iso == s || (e.nonEmpty && iso == e) then Present("p-datepicker-day-selected")
            else if s.nonEmpty && e.nonEmpty && s < iso && iso < e then Present("p-datepicker-day-selected-range")
            else Absent

    private def otherMonthCell(day: Int)(using Frame): UI =
        td.cssClass("p-datepicker-day-cell").cssClass("p-datepicker-other-month").role("gridcell")(
            span.cssClass("p-datepicker-day").cssClass("p-disabled")(day.toString)
        )

    /** A day pick, per binding: a single value takes the day (keeping its time of day for a time kind, clamped into the
      * bounds) and closes unless the time picker needs the panel; a set toggles the day and stays open; a range fills start
      * and end and closes on completion.
      */
    private def selectDay(iso: String, snap: DatePicker.Snapshot, oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI.Action =
        remember(nav).andThen(selectDayNow(iso, snap, oref, nav))

    private def selectDayNow(iso: String, snap: DatePicker.Snapshot, oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(
        using Frame
    ): UI.Action =
        bindingV match
            case Present(DatePicker.Binding.Multi(ref)) =>
                DatePicker.Binding.toggle(ref, kind, iso).map {
                    case Present(now) => fireValues(now)
                    case Absent       => (): UI.Action
                }
            case Present(DatePicker.Binding.Range(start, end)) =>
                DatePicker.Binding.pickRange(start, end, kind, iso).map { (change, complete) =>
                    val report: UI.Action = change match
                        case Present(now) => fireRange(now)
                        case Absent       => ()
                    report.andThen(if complete then closeAndReturn(oref, nav) else (): UI.Action)
                }
            case _ =>
                val cur = snap match
                    case DatePicker.Snapshot.Single(v) => v
                    case _                             => ""
                val newValue = if kind.withTime then clamp(iso + "T" + DatePicker.timeText(cur)) else iso
                putValue(newValue).andThen(if kind.withTime then (): UI.Action else closeAndReturn(oref, nav))
    end selectDayNow

    // === month / year views =================================================

    /** Prime's `.p-datepicker-month-view`: 12 month cells. For a `YearMonth` a pick writes the value and closes; while
      * drilling from the day view it writes the displayed month and drills back.
      */
    private def monthView(year: Int, snap: DatePicker.Snapshot, oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI =
        val current         = DatePicker.datePart(DatePicker.anchorOf(snap)).take(7)
        val cursor          = granularCursor(DatePicker.pad(year, 4) + "-", current, 7, nav)
        val picks           = kind.view == DatePickerView.Month
        val cells: List[UI] = (1 to 12).toList.map { m =>
            val iso7       = DatePicker.pad(year, 4) + "-" + DatePicker.pad(m, 2)
            val outOfRange =
                minIso.exists(min => iso7 < min.take(7)) || maxIso.exists(max => iso7 > max.take(7))
            var cell = span.cssClass("p-datepicker-month")
            if current == iso7 then cell = cell.cssClass("p-datepicker-month-selected")
            val action: Maybe[UI.Action] =
                if outOfRange then Absent
                else if picks then Present(pickGranular(iso7, oref, nav))
                else drillTo(iso7, DatePickerView.Date, nav)
            action match
                case Present(a) => cell = cell.onClick(seedCursor(iso7, nav).andThen(a))
                case Absent     => cell = cell.cssClass("p-disabled").aria("disabled", "true")
            nav.foreach(n => cell = cell.id(n.cellId(m - 1)).role("gridcell"))
            if iso7 == cursor then cell = cell.cssClass("p-focus").scrollAuto(true)
            cell(DatePicker.monthNamesShort(m - 1))
        }
        // Prime lays the twelve months out three to a row, which is the width the vertical arrows step by.
        granularGrid("p-datepicker-month-view", cells, cursor, iso => DatePicker.monthOf(iso) - 1, 3, oref, nav) { (at, step) =>
            import CalendarNav.Step.*
            step match
                case Move(cells)      => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(wrapIndex(at + cells, 12) + 1, 2))
                case RowStart         => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(at - at % 3 + 1, 2))
                case RowEnd           => Present(DatePicker.pad(year, 4) + "-" + DatePicker.pad(at - at % 3 + 3, 2))
                case Page(delta, big) =>
                    Present(DatePicker.pad(year + (if big then delta * 10 else delta), 4) + "-" + DatePicker.pad(at + 1, 2))
                case Activate | Dismiss => Absent
            end match
        } { iso7 =>
            val outOfRange =
                minIso.exists(min => iso7 < min.take(7)) || maxIso.exists(max => iso7 > max.take(7))
            if outOfRange then ()
            else if picks then pickGranular(iso7, oref, nav)
            else
                drillTo(iso7, DatePickerView.Date, nav) match
                    case Present(eff) => eff
                    case Absent       => (): UI.Action
            end if
        }
    end monthView

    /** Prime's `.p-datepicker-year-view`: the displayed decade's 10 year cells. For a `Year` a pick writes the value and
      * closes; while drilling it writes the displayed month's year and drills to the month grid.
      */
    private def yearView(
        year: Int,
        month: Int,
        snap: DatePicker.Snapshot,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(using Frame): UI =
        val current         = DatePicker.datePart(DatePicker.anchorOf(snap)).take(4)
        val base            = year - Math.floorMod(year, 10)
        val cursor          = granularCursor("", current, 4, nav, fallback = DatePicker.pad(base, 4))
        val picks           = kind.view == DatePickerView.Year
        val cells: List[UI] = (base to base + 9).toList.map { y =>
            val iso4       = DatePicker.pad(y, 4)
            val outOfRange =
                minIso.exists(min => iso4 < min.take(4)) || maxIso.exists(max => iso4 > max.take(4))
            var cell = span.cssClass("p-datepicker-year")
            if current == iso4 then cell = cell.cssClass("p-datepicker-year-selected")
            val action: Maybe[UI.Action] =
                if outOfRange then Absent
                else if picks then Present(pickGranular(iso4, oref, nav))
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
            val outOfRange =
                minIso.exists(min => iso4 < min.take(4)) || maxIso.exists(max => iso4 > max.take(4))
            if outOfRange then ()
            else if picks then pickGranular(iso4, oref, nav)
            else
                drillTo(iso4 + "-" + DatePicker.pad(month, 2), DatePickerView.Month, nav) match
                    case Present(eff) => eff
                    case Absent       => (): UI.Action
            end if
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

    /** An index that comes round rather than stopping, since a granular grid is a closed ring of twelve months or ten years
      * and the page keys are what leave it.
      */
    private def wrapIndex(i: Int, size: Int): Int = Math.floorMod(i, size)

    /** The shared wiring of the month and year grids: one tab stop, the highlight announced, and [[CalendarNav]] read against
      * the grid's own width. `move` maps a step onto the next cell's ISO string, `activate` picks it.
      */
    private def granularGrid(
        cls: String,
        cells: List[UI],
        cursor: String,
        indexOf: String => Int,
        perRow: Int,
        oref: Maybe[ReactiveValue[Boolean]],
        nav: Maybe[DatePicker.Nav]
    )(move: (Int, CalendarNav.Step) => Maybe[String])(activate: String => UI.Action)(using Frame): UI =
        var grid = div.cssClass(cls)
        nav.foreach { n =>
            grid = grid.id(n.gridId).tabIndex(0).preventScrollKeys.role("grid")
            if n.seedV then grid = grid.focusAuto(true)
            if cursor.nonEmpty then grid = grid.aria("activedescendant", n.cellId(indexOf(cursor)))
            grid = grid.onKeyDown { e =>
                val eff: UI.Action =
                    if cursor.isEmpty then ()
                    else
                        CalendarNav.onKey(e.key, e.modifiers, perRow) match
                            case Absent        => ()
                            case Present(step) =>
                                val moved: UI.Action = move(indexOf(cursor), step) match
                                    case Present(next) => n.cursor.set(next)
                                    case Absent        => ()
                                val acted: UI.Action = step match
                                    case CalendarNav.Step.Activate => activate(cursor)
                                    case CalendarNav.Step.Dismiss  => cancelVia(oref, nav)
                                    case _                         => ()
                                moved.andThen(acted)
                eff
            }
        }
        // A `gridcell` with no `row` over it is outside the grid as far as the ARIA tree is concerned, and Prime's own markup
        // lays the cells straight into a wrapping flex box. The rows carry the structure and `display: contents` in the extra
        // sheet keeps the layout.
        val rows: List[UI] =
            cells.grouped(perRow).toList.map(row => div.role("row")(row.map(toChild)*))
        grid(rows.map(toChild)*)
    end granularGrid

    /** A pick in the grid the kind picks in (`YearMonth`, `Year`): write the value, close. */
    private def pickGranular(iso: String, oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI.Action =
        for
            _ <- remember(nav)
            _ <- putValue(iso)
            r <- closeAndReturn(oref, nav)
        yield r

    /** A drill-down pick: write the displayed month and switch the view. Needs BOTH the month and the view refs; `Absent`
      * renders the cell disabled instead.
      */
    private def drillTo(yearMonth: String, target: DatePickerView, nav: Maybe[DatePicker.Nav])(using
        Frame
    ): Maybe[UI.Action] =
        (shownMonth(nav), currentViewSlot) match
            case (Present(mref), Present(_)) =>
                Present(
                    for
                        _ <- seedGrid(nav)
                        _ <- mref.set(yearMonth)
                        _ <- askView(target)
                        r <- focusGrid(nav)
                    yield r
                )
            case _ => Absent

    /** Asks for grid `target`: written into a two-way [[currentView]], then reported. */
    private def askView(target: DatePickerView)(using Frame): UI.Action =
        ReactiveValue.change(currentViewSlot, target, onCurrentViewF)

    // === time picker ========================================================

    /** Prime's `.p-datepicker-time-picker`: hour and minute columns of increment, display and decrement, plus the AM/PM
      * column under `hourFormat(H12)`. A step that would leave the bounds renders its button disabled.
      */
    private def timePickerUI(snap: DatePicker.Snapshot, nav: Maybe[DatePicker.Nav])(using Frame): UI =
        val cur = snap match
            case DatePicker.Snapshot.Single(v) => v
            case _                             => ""
        // A time alone edits from midnight (or the earliest time the bounds allow); a day and a time needs a picked day first
        // (the pure render has no clock to substitute), so its buttons render disabled until then.
        val start          = if cur.nonEmpty then cur else if kind.timeOnly then clamp("00:00") else ""
        val editable       = start.nonEmpty
        val (hour, minute) = DatePicker.parseTime(DatePicker.timeText(start))

        def candidate(h: Int, m: Int): String =
            val t = DatePicker.pad(h, 2) + ":" + DatePicker.pad(m, 2)
            if kind.timeOnly then t else DatePicker.datePart(start) + "T" + t

        def write(iso: String): UI.Action = remember(nav).andThen(putValue(iso))

        def spinButton(cls: String, glyph: IconGlyph, label: String, iso: String): UI =
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
            if editable && inBounds(iso) then b = b.onClick(write(iso)) else b = b.disabled(true)
            b(toChild(GlyphSvg(glyph, "p-icon")))
        end spinButton

        def column(cls: String, inc: String, dec: String, text: String, incLabel: String, decLabel: String): UI =
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
                    candidate(Math.floorMod(hour + 1, 24), minute),
                    candidate(Math.floorMod(hour - 1, 24), minute),
                    hourText,
                    "Next Hour",
                    "Previous Hour"
                ),
                div(toChild(span(":"))),
                column(
                    "p-datepicker-minute-picker",
                    candidate(hour, Math.floorMod(minute + 1, 60)),
                    candidate(hour, Math.floorMod(minute - 1, 60)),
                    DatePicker.pad(minute, 2),
                    "Next Minute",
                    "Previous Minute"
                )
            ) ++
                (hourFormatV match
                    case HourFormat.H12 =>
                        val toggle = candidate(Math.floorMod(hour + 12, 24), minute)
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

    /** Prime's `.p-datepicker-buttonbar`: Today (only with an explicit [[today]], the render being pure) and Clear, both in
      * Prime's small secondary text Button skin.
      */
    private def buttonBarUI(snap: DatePicker.Snapshot, oref: Maybe[ReactiveValue[Boolean]], nav: Maybe[DatePicker.Nav])(using
        Frame
    ): UI =
        def barButton(cls: String, label: String, action: Maybe[UI.Action]): UI =
            var b = button
                .cssClass(cls)
                .cssClass("p-button")
                .cssClass("p-component")
                .cssClass("p-button-secondary")
                .cssClass("p-button-text")
                .cssClass("p-button-sm")
                .jsProp("type", "button")
            b = action match
                case Present(a) => b.onClick(a)
                case Absent     => b.disabled(true)
            b(toChild(span.cssClass("p-button-label")(label)))
        end barButton

        val todayButton: List[UI] = todayV.toList.map { t =>
            val iso                 = kind.toIso(t)
            val navigate: UI.Action =
                (shownMonth(nav), DatePicker.parseYearMonth(iso)) match
                    case (Present(mref), Present((y, m))) if !kind.timeOnly =>
                        mref.set(DatePicker.pad(y, 4) + "-" + DatePicker.pad(m, 2))
                    case _ => ()
            val pick: UI.Action = bindingV match
                case Present(DatePicker.Binding.Multi(_)) | Present(DatePicker.Binding.Range(_, _)) =>
                    selectDay(DatePicker.datePart(iso), snap, oref, nav)
                case _ =>
                    remember(nav).andThen(putValue(iso)).andThen(closeAndReturn(oref, nav))
            barButton("p-datepicker-today-button", "Today", if inBounds(iso) then Present(navigate.andThen(pick)) else Absent)
        }

        val clearAction: UI.Action =
            val wipe: UI.Action = bindingV match
                case Present(DatePicker.Binding.Single(_)) => putValue("")
                case Present(DatePicker.Binding.Multi(r))  =>
                    r.current.map(prev =>
                        if prev.isEmpty then (): UI.Action
                        else DatePicker.Binding.write(r, Set.empty[A]).andThen(fireValues(Set.empty))
                    )
                case Present(DatePicker.Binding.Range(s, e)) =>
                    for
                        sv <- s.current
                        ev <- e.current
                        r  <-
                            if sv.isEmpty && ev.isEmpty then (): UI.Action
                            else
                                DatePicker.Binding.write(s, Maybe.empty[A])
                                    .andThen(DatePicker.Binding.write(e, Maybe.empty[A]))
                                    .andThen(fireRange((Absent, Absent)))
                    yield r
                case Absent => ()
            for
                _ <- remember(nav)
                _ <- wipe
                r <- closeAndReturn(oref, nav)
            yield r
            end for
        end clearAction

        val clearButton: UI = barButton("p-datepicker-clear-button", "Clear", Present(clearAction))

        div.cssClass("p-datepicker-buttonbar")((todayButton :+ clearButton).map(toChild)*)
    end buttonBarUI

    /** Derives `(year, month)` from the month override, else the held value, else the reference value, else January 2000. */
    private def deriveMonth(current: String, monthOverride: Maybe[String]): (Int, Int) =
        monthOverride.flatMap(DatePicker.parseYearMonth)
            .orElse(DatePicker.parseYearMonth(current))
            .orElse(referenceDateV.flatMap(r => DatePicker.parseYearMonth(kind.toIso(r))))
            .getOrElse((2000, 1))
end DatePicker

object DatePicker:

    /** A picker over the value type `A`, whose [[DateKind]] given decides what it shows: `uic.DatePicker[LocalDate]()`. */
    def apply[A]()(using kind: DateKind[A]): DatePicker[A] = new DatePicker[A](kind)

    /** A picker over an explicitly chosen kind, which is how a `ZonedDateTime` picker is made:
      * `uic.DatePicker(uic.DateKind.zoned(zone))`.
      */
    @targetName("applyKind")
    def apply[A](kind: DateKind[A]): DatePicker[A] = new DatePicker[A](kind)

    /** The selection the calendar renders, as ISO strings at the kind's precision (`""` for none). Every comparison the grids
      * make is between such strings, which is what keeps the date math integer arithmetic over them.
      */
    private[uic] enum Snapshot derives CanEqual:
        case Single(value: String)
        case Multi(values: Set[String])
        case Range(start: String, end: String)
    end Snapshot

    /** The value the displayed month derives from (before the month override and the reference value). */
    private def anchorOf(snap: Snapshot): String = snap match
        case Snapshot.Single(v)   => v
        case Snapshot.Multi(set)  => set.maxOption.getOrElse("")
        case Snapshot.Range(s, _) => s

    /** The field's text under a set or range binding, which is shown and not read back. */
    private def displayText(snap: Snapshot): String = snap match
        case Snapshot.Single(v)   => v
        case Snapshot.Multi(set)  => set.toList.sorted.mkString(", ")
        case Snapshot.Range(s, e) => if s.isEmpty then "" else if e.isEmpty then s else s"$s - $e"

    /** What a [[DatePicker]] holds its selection in: a value that may stay empty, a set of days, or a range of them, each in
      * the app's own type.
      *
      * The calendar reads them as ISO strings through the picker's [[DateKind]] and writes back values parsed from such
      * strings. Two values the kind writes as one string are one pick, so a write naming the value already held leaves the
      * app's instance in place and reports no change, which is what keeps the change callbacks from announcing a write that
      * did not happen.
      */
    private[uic] enum Binding[A]:
        // A constant, a one-way signal or a two-way ref, as every value slot takes them.
        case Single(value: ReactiveValue[Maybe[A]])
        case Multi(values: ReactiveValue[Set[A]])
        case Range(start: ReactiveValue[Maybe[A]], end: ReactiveValue[Maybe[A]])

        /** The held selection as ISO strings. */
        def snapshot(kind: DateKind[A])(using Frame): Signal[Snapshot] =
            // Compared by the app's own equality, as the value type defines it.
            given CanEqual[Maybe[A], Maybe[A]] = CanEqual.derived
            given CanEqual[Set[A], Set[A]]     = CanEqual.derived
            this match
                case Single(r)   => r.signal.map(m => Snapshot.Single(Binding.isoOf(kind, m)))
                case Multi(r)    => r.signal.map(as => Snapshot.Multi(as.map(kind.toIso)))
                case Range(s, e) =>
                    s.signal.map(Binding.isoOf(kind, _)).combineLatest(e.signal.map(Binding.isoOf(kind, _))).map((a, b) =>
                        Snapshot.Range(a, b)
                    )
            end match
        end snapshot

        /** What the binding holds now, as what would put it back: Escape's memory. A two-way binding is written back and
          * nothing is reported, since over the whole open nothing changed. A one-way binding was never written: its owner
          * took each pick from the event, so the undo asks the owner through the same event for what was held, where that
          * differs from what the owner holds now.
          */
        def capture(
            kind: DateKind[A],
            one: Maybe[A] => UI.Action,
            set: Set[A] => UI.Action,
            range: (Maybe[A], Maybe[A]) => UI.Action
        )(using Frame): Restore < Sync =
            def isoSet(as: Set[A]): Set[String] = as.map(kind.toIso)
            this match
                case Single(r) =>
                    r.current.map(v =>
                        Restore(r.writable match
                            case Present(ref) => ref.set(v)
                            case Absent       =>
                                r.current.map(now => if Binding.isoOf(kind, now) == Binding.isoOf(kind, v) then (): UI.Action else one(v)))
                    )
                case Multi(r) =>
                    r.current.map(v =>
                        Restore(r.writable match
                            case Present(ref) => ref.set(v)
                            case Absent       => r.current.map(now => if isoSet(now) == isoSet(v) then (): UI.Action else set(v)))
                    )
                case Range(s, e) =>
                    for
                        sv <- s.current
                        ev <- e.current
                    yield Restore(
                        if s.writable.isDefined || e.writable.isDefined then Binding.write(s, sv).andThen(Binding.write(e, ev))
                        else
                            s.current.map(sn =>
                                e.current.map(en =>
                                    val same = Binding.isoOf(kind, sn) == Binding.isoOf(kind, sv) &&
                                        Binding.isoOf(kind, en) == Binding.isoOf(kind, ev)
                                    if same then (): UI.Action else range(sv, ev)
                                )
                            )
                    )
            end match
        end capture

        /** A constant nobody hears ([[ReactiveValue.fixed]] for the binding's own event): the picker is drawn read-only. */
        def fixed(heard: Boolean): Boolean =
            !heard &&
                (this match
                    case Single(r)   => r.const.isDefined
                    case Multi(r)    => r.const.isDefined
                    case Range(s, e) => s.const.isDefined && e.const.isDefined)

        /** A signal nobody hears: every pick is painted away again. */
        def deaf(heard: Boolean): Boolean =
            def oneWay[B](v: ReactiveValue[B]): Boolean = v.dyn.isDefined && v.writable.isEmpty
            !heard &&
            (this match
                case Single(r)   => oneWay(r)
                case Multi(r)    => oneWay(r)
                case Range(s, e) => oneWay(s) || oneWay(e))
        end deaf
    end Binding

    private[uic] object Binding:

        private[uic] def isoOf[A](kind: DateKind[A], v: Maybe[A]): String = v.map(kind.toIso).getOrElse("")

        /** Writes `next` where the binding is two way. A one-way binding is only reported, which the caller does. */
        private[uic] def write[B](v: ReactiveValue[B], next: B)(using Frame): Unit < Sync =
            v.writable.fold(Kyo.unit)(_.set(next))

        /** The value `iso` names, keeping `prev` where it names the same one. */
        private def keep[A](kind: DateKind[A], prev: Maybe[A], iso: String): Maybe[A] =
            if isoOf(kind, prev) == iso then prev
            else if iso.isEmpty then Absent
            else kind.fromIso(iso)

        /** Writes the value `iso` names (`""` for none) unless it is the one held, and answers the value now held where the
          * write changed it. A string the kind cannot read writes nothing.
          */
        def put[A](v: ReactiveValue[Maybe[A]], kind: DateKind[A], iso: String)(using Frame): Maybe[Maybe[A]] < Sync =
            if iso.nonEmpty && kind.fromIso(iso).isEmpty then Kyo.lift(Maybe.empty[Maybe[A]])
            else
                def answer(prev: Maybe[A]): Maybe[Maybe[A]] = if isoOf(kind, prev) == iso then Absent else Present(keep(kind, prev, iso))
                v.writable match
                    // Two way: one atomic step, so two writes cannot both read the value before either.
                    case Present(ref) => ref.getAndUpdate(prev => keep(kind, prev, iso)).map(answer)
                    case Absent       => v.current.map(answer)
                end match

        /** Toggles the day `iso` names in or out of the set, and answers the set now held. A member the set already holds
          * stays as the app wrote it.
          */
        def toggle[A](v: ReactiveValue[Set[A]], kind: DateKind[A], iso: String)(using Frame): Maybe[Set[A]] < Sync =
            def step(prev: Set[A]): Set[A] =
                if prev.exists(a => kind.toIso(a) == iso) then prev.filterNot(a => kind.toIso(a) == iso)
                else kind.fromIso(iso).map(prev + _).getOrElse(prev)
            def answer(prev: Set[A]): Maybe[Set[A]] =
                val now = step(prev)
                if now.map(kind.toIso) == prev.map(kind.toIso) then Absent else Present(now)
            v.writable match
                case Present(ref) => ref.getAndUpdate(step).map(answer)
                case Absent       => v.current.map(answer)
        end toggle

        /** One pick of a range: it restarts the range (start written, end cleared) when there is no start yet, when the range
          * is already complete, or when the day is before the start; otherwise it completes the range. Answers the start and
          * end now held where either changed, and whether the pick completed the range.
          */
        def pickRange[A](start: ReactiveValue[Maybe[A]], end: ReactiveValue[Maybe[A]], kind: DateKind[A], iso: String)(using
            Frame
        ): (Maybe[(Maybe[A], Maybe[A])], Boolean) < Sync =
            for
                sv <- start.current
                ev <- end.current
                sIso           = isoOf(kind, sv)
                eIso           = isoOf(kind, ev)
                restart        = sIso.isEmpty || eIso.nonEmpty || iso < sIso
                (sNext, eNext) = if restart then (keep(kind, sv, iso), Maybe.empty[A]) else (sv, keep(kind, ev, iso))
                _ <- write(start, sNext)
                _ <- write(end, eNext)
            yield
                val changed = isoOf(kind, sNext) != sIso || isoOf(kind, eNext) != eIso
                (if changed then Present((sNext, eNext)) else Absent, !restart)
    end Binding

    /** What Escape puts back: the write that restores the values the panel held before the reader changed anything in it.
      * Recorded once per open, at the first write rather than at the open itself, so an open a caller drove through its own
      * ref is remembered too. It holds the app's own instances, so an undo gives back exactly what was there.
      */
    final private[uic] case class Restore(put: UI.Action)

    private[uic] object Restore:
        given CanEqual[Restore, Restore] = CanEqual.derived

    /** The displayed month, as the `YYYY-MM` the header and the keyboard write: a caller's own `SignalRef[YearMonth]`, or the
      * mount's own ref, whose `""` means "derive it from the held value".
      */
    private[uic] enum ShownMonth:
        // A caller's binding, and what hears a page the reader asks for.
        case Bound(month: ReactiveValue[YearMonth], report: Maybe[YearMonth => UI.Action])
        case Own(ref: SignalRef[String])

        def signal(using Frame): Signal[String] =
            import DateKind.given_CanEqual_YearMonth_YearMonth
            this match
                case Bound(m, _) => m.signal.map(ym => pad(ym.getYear, 4) + "-" + pad(ym.getMonthValue, 2))
                case Own(r)      => r
        end signal

        /** Asks for page `iso`: written into a two-way binding, then reported; the page the picker keeps is just written. */
        def set(iso: String)(using Frame): UI.Action =
            this match
                case Bound(m, report) =>
                    parseYearMonth(iso) match
                        case Present((y, mo)) => ReactiveValue.change(Present(m), YearMonth.of(y, mo), report)
                        case Absent           => ()
                case Own(r) => r.set(iso)
    end ShownMonth

    /** The refs a mount hands the wired host: the ones a caller may bind (open, month, view) and the ones the component keeps
      * for itself (the keyboard cursor, the flag that says the keyboard is coming into the grid, what Escape puts back, and a
      * typed text the field did not take).
      */
    final private[uic] case class Refs(
        open: ReactiveValue[Boolean],
        month: ShownMonth,
        cursor: SignalRef[String],
        view: ReactiveValue[DatePickerView],
        seed: SignalRef[Boolean],
        restore: SignalRef[Maybe[Restore]],
        draft: SignalRef[Maybe[String]]
    )

    private[uic] object Refs:
        /** The refs over a two-way open ref, the shape a test or a golden hands the wired host. */
        def apply(
            open: SignalRef[Boolean],
            month: ShownMonth,
            cursor: SignalRef[String],
            view: SignalRef[DatePickerView],
            seed: SignalRef[Boolean],
            restore: SignalRef[Maybe[Restore]],
            draft: SignalRef[Maybe[String]]
        ): Refs = new Refs(ReactiveValue[Boolean](open), month, cursor, ReactiveValue[DatePickerView](view), seed, restore, draft)
    end Refs

    final private[uic] case class Nav(
        monthOv: ShownMonth,
        monthOvV: String,
        cursor: SignalRef[String],
        cursorV: String,
        seed: SignalRef[Boolean],
        seedV: Boolean,
        restore: SignalRef[Maybe[Restore]],
        draft: SignalRef[Maybe[String]],
        base: String,
        focus: String => UI.Action,
        callerId: Maybe[String]
    ):
        def cellId(index: Int): String = s"$base-c$index"
        def gridId: String             = s"$base-grid"

        /** The text field's id: the caller's own where there is one, since the field is the control's focusable element and
          * the id is what `Label.forId` and the form's focus-first-invalid address; a derived one otherwise, which Escape
          * hands focus back to.
          */
        def fieldId: String = callerId.getOrElse(s"$base-field")
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

    // Sakamoto's algorithm: 0 = Sunday .. 6 = Saturday.
    private val sakamoto                               = Array(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    private def dayOfWeek(y: Int, m: Int, d: Int): Int =
        val yy = if m < 3 then y - 1 else y
        Math.floorMod(yy + yy / 4 - yy / 100 + yy / 400 + sakamoto(m - 1) + d, 7)

    private def dayOfYear(y: Int, m: Int, d: Int): Int = (1 until m).foldLeft(d)((acc, mm) => acc + daysInMonth(y, mm))

    /** Number of ISO 8601 weeks in `y` (52 or 53): 53 iff the year starts or ends on a Thursday, expressed via the standard
      * `p(y)` weekday-of-Dec-31 formula.
      */
    private def isoWeeksInYear(y: Int): Int =
        def p(yy: Int): Int = Math.floorMod(yy + yy / 4 - yy / 100 + yy / 400, 7)
        if p(y) == 4 || p(y - 1) == 3 then 53 else 52

    /** ISO 8601 week number (weeks start Monday; week 1 contains the year's first Thursday), in integer math like the rest of
      * the grid.
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

    /** The date part of an ISO value: what comes before the `T`, else the whole value. */
    private def datePart(v: String): String =
        val t = v.indexOf('T')
        if t >= 0 then v.take(t) else v

    /** The time part of an ISO value: after the `T`, or the whole value when it IS a time (`HH:MM`), else `"00:00"`. */
    private def timeText(v: String): String =
        val t = v.indexOf('T')
        if t >= 0 then v.drop(t + 1)
        else if v.length >= 4 && v.contains(':') then v
        else "00:00"
    end timeText

    /** Parses `HH:MM` into clamped `(hour, minute)`; anything malformed reads as 0. */
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
      * A day-at-a-time walk rather than an epoch conversion, because the only shifts the calendar asks for are one day and one
      * week: the loop is over in seven steps and is obviously right.
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

    /** `n` months from `(y, m)`, with the day clamped into the month it lands in: a 31st paged into a 30-day month is that
      * month's 30th, not its 1st.
      */
    private def addMonths(y: Int, m: Int, d: Int, n: Int): (Int, Int, Int) =
        val total = (y * 12 + (m - 1)) + n
        val ny    = Math.floorDiv(total, 12)
        val nm    = Math.floorMod(total, 12) + 1
        (ny, nm, math.min(d, daysInMonth(ny, nm)))
    end addMonths

    /** Parses the leading `YYYY[-MM]` out of an ISO string (a bare 4-digit year reads as January, which is how the year grid's
      * values page).
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

/** A [[DatePicker]] picking several days at once (Prime's `selectionMode="multiple"`), bound two-way to the `Set[A]` of the
  * days picked. Made by [[DatePicker.values]], and a type of its own because its value is a set: it has one `onChange` and
  * one `onBlur`, both over `Set[A]`, so a callback for a single value cannot be set on it and left never to fire.
  *
  * A day click toggles membership and the panel stays open; Clear empties the set. A member the set already holds stays as
  * the app wrote it. It renders through the picker it was made from, so the two share every display setter, the bounds, the
  * keyboard and the anatomy; the field shows the picked days and is read-only.
  *
  * It carries the validation vocabulary ([[FormControl]]) but is not a form-bindable control: the form layer's set-valued
  * trait binds through `selection`, and a second spelling of the binding `values` already made would be a slot with two names.
  */
final case class MultiDatePicker[A] private[uic] (private[uic] val picker: DatePicker[A])
    extends Node, FormControl, HasPlaceholder, HasAccessibleNameRef:
    type Self = MultiDatePicker[A]

    private def on(p: DatePicker[A]): MultiDatePicker[A] = copy(picker = p)

    /** Fired with the set now held after a toggle or a clear changed it; the undo of an Escape fires nothing. */
    def onChange(f: Set[A] => UI.Action)(using Frame): MultiDatePicker[A] = on(picker.withChangeSet(f))

    /** Fires on focus loss with the set held, whether or not anything was edited. */
    def onBlur(f: Set[A] => UI.Action)(using Frame): MultiDatePicker[A] = on(picker.withBlurSet(f))

    /** Earliest day the picker writes, as [[DatePicker.minDate]]. */
    def minDate(v: A): Self = on(picker.minDate(v))

    /** Latest day the picker writes, as [[DatePicker.maxDate]]. */
    def maxDate(v: A): Self = on(picker.maxDate(v))

    /** The app-supplied "now" for the Today button, which shows its page and picks its day. */
    def today(v: A): Self = on(picker.today(v))

    /** Renders the calendar in-flow below the field, as [[DatePicker.inline]]. */
    def inline(v: Boolean): Self = on(picker.inline(v))

    /** Renders Prime's button bar with Today and Clear, as [[DatePicker.showButtonBar]]. */
    def showButtonBar(v: Boolean): Self = on(picker.showButtonBar(v))

    /** Shows the ISO 8601 week-number column. */
    def showWeek(v: Boolean)(using DateKind.Weeks[A]): Self = on(picker.showWeek(v))

    /** Binds the displayed month two-way, as [[DatePicker.month]]. */
    def month(v: YearMonth | Signal[YearMonth])(using DateKind.Calendar[A]): Self = on(picker.month(v))

    /** Fired with the month the reader pages to, as [[DatePicker.onMonthChange]]. */
    def onMonthChange(f: YearMonth => UI.Action)(using Frame, DateKind.Calendar[A]): Self = on(picker.onMonthChange(f))

    /** The page to show while neither a month ref nor a held day says which. */
    def referenceDate(v: A)(using DateKind.Calendar[A]): Self = on(picker.referenceDate(v))

    /** Binds the grid on screen two-way, as [[DatePicker.currentView]]. */
    def currentView(v: DatePickerView | Signal[DatePickerView])(using DateKind.Drill[A]): Self = on(picker.currentView(v))

    /** Fired with the grid the reader drills to, as [[DatePicker.onCurrentViewChange]]. */
    def onCurrentViewChange(f: DatePickerView => UI.Action)(using Frame, DateKind.Drill[A]): Self = on(picker.onCurrentViewChange(f))

    /** Binds the calendar's visibility, as [[DatePicker.open]]. */
    def open(v: Boolean | Signal[Boolean]): Self = on(picker.open(v))

    /** Fired with the visibility the reader asks for, as [[DatePicker.onOpenChange]]. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Self = on(picker.onOpenChange(f))

    def disabled(v: Boolean): Self = on(picker.disabled(v))

    /** Native `readonly` on the field; the calendar cannot be opened either. */
    def readonly(v: Boolean): Self = on(picker.readonly(v))

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): Self = on(picker.required(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): Self = on(picker.name(v))

    /** Size of the field. */
    def size(v: Size): Self = on(picker.size(v))

    /** Fill variant. */
    def variant(v: FieldVariant): Self = on(picker.variant(v))

    /** Spans the full width of its container. */
    def fluid(v: Boolean): Self = on(picker.fluid(v))

    private[uic] def withElementId(v: Maybe[String]): Self = on(picker.withElementId(v))
    private[uic] def elementId: Maybe[String]              = picker.elementId

    private[uic] def withPlaceholder(v: Maybe[TextValue]): Self = on(picker.withPlaceholder(v))

    private[uic] def withInvalid(v: Maybe[BoolValue]): Self                       = on(picker.withInvalid(v))
    private[uic] def withInvalidMessage(v: Maybe[String]): Self                   = on(picker.withInvalidMessage(v))
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Self = on(picker.withInvalidMessageDyn(v))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Self = on(picker.withAccessibleName(v))
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Self = on(picker.withAccessibleNameRef(v))

    /** The seam golden tests render, as [[DatePicker.wired]]. */
    private[uic] def wired(refs: DatePicker.Refs, base: String, focus: String => UI.Action)(using Frame): UI =
        picker.wired(refs, base, focus)

    private[uic] def render(using Frame): UI = picker.render
end MultiDatePicker

/** A [[DatePicker]] picking a range of days (Prime's `selectionMode="range"`), bound two-way to a start and an end, each
  * `Absent` until picked. Made by [[DatePicker.range]], and a type of its own because its value is a pair: it has one
  * `onChange` and one `onBlur`, both over the start and the end, so a callback for a single value cannot be set on it and
  * left never to fire.
  *
  * The first pick writes the start (clearing the end), a pick on or after it completes the range and closes the panel, an
  * earlier pick restarts; in-range days render Prime's `.p-datepicker-day-selected-range`. It renders through the picker it
  * was made from, so the two share every display setter, the bounds, the keyboard and the anatomy; the field shows the
  * range and is read-only.
  *
  * It carries the validation vocabulary ([[FormControl]]) but no form binding, since no form field holds two refs.
  */
final case class RangeDatePicker[A] private[uic] (private[uic] val picker: DatePicker[A])
    extends Node, FormControl, HasPlaceholder, HasAccessibleNameRef:
    type Self = RangeDatePicker[A]

    private def on(p: DatePicker[A]): RangeDatePicker[A] = copy(picker = p)

    /** Fired with the start and end now held after a pick or a clear changed either; the undo of an Escape fires nothing. */
    def onChange(f: (Maybe[A], Maybe[A]) => UI.Action)(using Frame): RangeDatePicker[A] = on(picker.withChangeRange(f))

    /** Fires on focus loss with the start and end held, whether or not anything was edited. */
    def onBlur(f: (Maybe[A], Maybe[A]) => UI.Action)(using Frame): RangeDatePicker[A] = on(picker.withBlurRange(f))

    /** Earliest day the picker writes, as [[DatePicker.minDate]]. */
    def minDate(v: A): Self = on(picker.minDate(v))

    /** Latest day the picker writes, as [[DatePicker.maxDate]]. */
    def maxDate(v: A): Self = on(picker.maxDate(v))

    /** The app-supplied "now" for the Today button, which shows its page and picks its day. */
    def today(v: A): Self = on(picker.today(v))

    /** Renders the calendar in-flow below the field, as [[DatePicker.inline]]. */
    def inline(v: Boolean): Self = on(picker.inline(v))

    /** Renders Prime's button bar with Today and Clear, as [[DatePicker.showButtonBar]]. */
    def showButtonBar(v: Boolean): Self = on(picker.showButtonBar(v))

    /** Shows the ISO 8601 week-number column. */
    def showWeek(v: Boolean)(using DateKind.Weeks[A]): Self = on(picker.showWeek(v))

    /** Binds the displayed month two-way, as [[DatePicker.month]]. */
    def month(v: YearMonth | Signal[YearMonth])(using DateKind.Calendar[A]): Self = on(picker.month(v))

    /** Fired with the month the reader pages to, as [[DatePicker.onMonthChange]]. */
    def onMonthChange(f: YearMonth => UI.Action)(using Frame, DateKind.Calendar[A]): Self = on(picker.onMonthChange(f))

    /** The page to show while neither a month ref nor a held day says which. */
    def referenceDate(v: A)(using DateKind.Calendar[A]): Self = on(picker.referenceDate(v))

    /** Binds the grid on screen two-way, as [[DatePicker.currentView]]. */
    def currentView(v: DatePickerView | Signal[DatePickerView])(using DateKind.Drill[A]): Self = on(picker.currentView(v))

    /** Fired with the grid the reader drills to, as [[DatePicker.onCurrentViewChange]]. */
    def onCurrentViewChange(f: DatePickerView => UI.Action)(using Frame, DateKind.Drill[A]): Self = on(picker.onCurrentViewChange(f))

    /** Binds the calendar's visibility, as [[DatePicker.open]]. */
    def open(v: Boolean | Signal[Boolean]): Self = on(picker.open(v))

    /** Fired with the visibility the reader asks for, as [[DatePicker.onOpenChange]]. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Self = on(picker.onOpenChange(f))

    def disabled(v: Boolean): Self = on(picker.disabled(v))

    /** Native `readonly` on the field; the calendar cannot be opened either. */
    def readonly(v: Boolean): Self = on(picker.readonly(v))

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): Self = on(picker.required(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): Self = on(picker.name(v))

    /** Size of the field. */
    def size(v: Size): Self = on(picker.size(v))

    /** Fill variant. */
    def variant(v: FieldVariant): Self = on(picker.variant(v))

    /** Spans the full width of its container. */
    def fluid(v: Boolean): Self = on(picker.fluid(v))

    private[uic] def withElementId(v: Maybe[String]): Self = on(picker.withElementId(v))
    private[uic] def elementId: Maybe[String]              = picker.elementId

    private[uic] def withPlaceholder(v: Maybe[TextValue]): Self = on(picker.withPlaceholder(v))

    private[uic] def withInvalid(v: Maybe[BoolValue]): Self                       = on(picker.withInvalid(v))
    private[uic] def withInvalidMessage(v: Maybe[String]): Self                   = on(picker.withInvalidMessage(v))
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Self = on(picker.withInvalidMessageDyn(v))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Self = on(picker.withAccessibleName(v))
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Self = on(picker.withAccessibleNameRef(v))

    /** The seam golden tests render, as [[DatePicker.wired]]. */
    private[uic] def wired(refs: DatePicker.Refs, base: String, focus: String => UI.Action)(using Frame): UI =
        picker.wired(refs, base, focus)

    private[uic] def render(using Frame): UI = picker.render
end RangeDatePicker
