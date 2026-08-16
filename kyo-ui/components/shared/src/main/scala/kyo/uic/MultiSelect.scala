package kyo.uic

import kyo.*
import kyo.UI.*

/** How a [[MultiSelect]] trigger renders its selection: comma-joined label text
  * (Prime's `display="comma"`, the default) or one removable [[Chip]] per
  * selected option (`display="chip"`, root modifier `.p-multiselect-display-chip`).
  */
enum MultiSelectDisplay derives CanEqual:
    case Comma, Chip

/** MultiSelect — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * MultiSelect anatomy: `div.p-multiselect.p-component.p-inputwrapper
  * [.p-multiselect-display-chip][.p-invalid][.p-disabled][.p-multiselect-sm|-lg]
  * [.p-variant-filled][.p-multiselect-fluid][.p-multiselect-open]` >
  * `div.p-multiselect-label-container` > `div.p-multiselect-label` + optional
  * `button.p-multiselect-clear-icon` + `div.p-multiselect-dropdown` with the
  * chevron, floating `div.p-multiselect-overlay` panel > optional
  * `div.p-multiselect-header` (select-all checkbox + filter) >
  * `div.p-multiselect-list-container` > `ul.p-multiselect-list[role=listbox]`
  * of `li.p-multiselect-option` rows, each a Prime checkbox + label), so the
  * extracted `@primeuix` multiselect CSS applies verbatim.
  *
  * [[Select]]'s multi-value sibling: the same [[Overlay]]-based floating panel
  * (outside click / Escape close it, the panel seeds focus on open), but the
  * value binds to a `SignalRef[Set[String]]` of option keys and PICKING DOES NOT
  * CLOSE — clicking a row (or Enter on the `.p-focus` highlight) toggles the key
  * and the panel stays open, exactly Prime's multiselect semantics.
  *
  * The header select-all checkbox follows PrimeVue: checked while every visible
  * non-disabled option is selected (with `filter(true)` "visible" means the
  * FILTERED options), unchecked otherwise — Prime renders no indeterminate
  * state here; checking selects exactly the visible enabled options (replacing
  * the whole value, like Prime), unchecking clears the selection entirely.
  *
  * Trigger label: comma-joined labels of the selected options in OPTIONS order
  * (a `Set` carries no pick order); `maxSelectedLabels(n)` switches to the
  * `selectedItemsLabel` summary ("{0} items selected", `{0}` = count) beyond
  * `n` selections; `display(MultiSelectDisplay.Chip)` renders removable
  * [[Chip]]s instead. DEVIATION: in chip mode the trigger opens via
  * `onClickSelf` on the label area, so clicking a chip's body does not toggle
  * the panel (kyo has no event-consumption primitive to exempt the chip remove
  * button from a container-level handler).
  *
  * Honest deferrals: option groups, virtual scrolling, per-option templates,
  * and `selectionLimit` are not implemented. Arrow keys on the open panel no
  * longer scroll the page underneath — the panel declares `preventScrollKeys`.
  */
final case class MultiSelect[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    valueRef: Maybe[SignalRef[Set[String]]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    placeholderV: Maybe[TextValue] = Absent,
    displayV: MultiSelectDisplay = MultiSelectDisplay.Comma,
    maxSelectedLabelsV: Maybe[Int] = Absent,
    selectedItemsLabelV: Maybe[TextValue] = Absent,
    filterFlag: Boolean = false,
    showToggleAllFlag: Boolean = true,
    highlightOnSelectFlag: Boolean = false,
    showClearFlag: Boolean = false,
    emptyMessageV: Maybe[TextValue] = Absent,
    disabledFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    tooltipV: Maybe[TextValue] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Set[String] => Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[String] => Any < Async] = Absent
) extends Node, MultiSelectFormControl:
    type Self = MultiSelect[A]

    /** Native `id` on the trigger — pair with `Label.forId` / `FloatLabel.forId`. */
    def id(v: String): MultiSelect[A] = copy(idV = Present(v))

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): MultiSelect[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): MultiSelect[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key — the value written into the bound set (defaults to
      * the label projection).
      */
    def optionKey(f: A => String): MultiSelect[A] = copy(keyF = Present(f))

    /** Renders matching options disabled (`.p-disabled` rows — shown in the panel
      * but not pickable; the keyboard highlight and select-all skip them).
      */
    def optionDisabled(f: A => Boolean): MultiSelect[A] = copy(optionDisabledF = Present(f))

    /** Binds the selection two-way to `ref`, keyed by [[optionKey]]: toggles write
      * the updated key set back, ref changes reselect the matching options.
      */
    def value(ref: SignalRef[Set[String]]): MultiSelect[A] = copy(valueRef = Present(ref))

    /** Binds the panel visibility two-way to `ref` (optional — the panel is
      * self-managed otherwise).
      */
    def open(ref: SignalRef[Boolean]): MultiSelect[A] = copy(openRefV = Present(ref))

    /** Text shown on the closed trigger while the bound set is empty (Prime's
      * `p-placeholder` label skin).
      */
    def placeholder(v: String): MultiSelect[A] = copy(placeholderV = Present(TextValue.Const(v)))

    /** Reactive placeholder — re-renders the label in place on signal emission (resolved INSIDE the
      * mount subscription, so open/highlight/filter state survives). For locale-driven text.
      */
    def placeholder(sig: Signal[String]): MultiSelect[A] = copy(placeholderV = Present(TextValue.Dyn(sig)))

    /** Trigger selection rendering: comma-joined text (default) or removable
      * chips (`.p-multiselect-display-chip`).
      */
    def display(v: MultiSelectDisplay): MultiSelect[A] = copy(displayV = v)

    /** Beyond `n` selected options the trigger shows the [[selectedItemsLabel]]
      * summary instead of the joined labels/chips (Prime's `maxSelectedLabels`).
      */
    def maxSelectedLabels(n: Int): MultiSelect[A] = copy(maxSelectedLabelsV = Present(n))

    /** Summary template used beyond [[maxSelectedLabels]] — `{0}` is replaced by
      * the selection count (default "{0} items selected", Prime's locale default).
      */
    def selectedItemsLabel(v: String): MultiSelect[A] = copy(selectedItemsLabelV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the summary in place on signal emission (the
      * emitted string is still a `{0}` template interpolated with the count).
      */
    def selectedItemsLabel(sig: Signal[String]): MultiSelect[A] = copy(selectedItemsLabelV = Present(TextValue.Dyn(sig)))

    /** Renders Prime's header filter (an IconField `.p-multiselect-filter-container`
      * with the `input.p-multiselect-filter`) filtering the options by a
      * case-insensitive contains on the label projection; the query resets on
      * every open. Select-all then applies to the FILTERED options (Prime).
      */
    def filter(v: Boolean): MultiSelect[A] = copy(filterFlag = v)

    /** Whether the header renders the select-all checkbox (default true —
      * Prime's `showToggleAll`).
      */
    def showToggleAll(v: Boolean): MultiSelect[A] = copy(showToggleAllFlag = v)

    /** Whether selected rows ALSO get Prime's highlighted row skin
      * (`.p-multiselect-option-selected`). Default false — PrimeVue's default:
      * the checked box alone marks a selected row.
      */
    def highlightOnSelect(v: Boolean): MultiSelect[A] = copy(highlightOnSelectFlag = v)

    /** Renders the clear affordance on the trigger while the selection is
      * non-empty — clears the bound set and fires `onChange(Set.empty)`.
      */
    def showClear(v: Boolean): MultiSelect[A] = copy(showClearFlag = v)

    /** Text of the `li.p-multiselect-empty-message` row when no options render
      * (default "No results found" — Prime's default).
      */
    def emptyMessage(v: String): MultiSelect[A] = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): MultiSelect[A] = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    def disabled(v: Boolean): MultiSelect[A] = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected keys (the div trigger has no native form value).
      */
    def name(v: String): MultiSelect[A] = copy(nameV = Present(v))

    /** Native tooltip (`title`). */
    def tooltip(v: String): MultiSelect[A] = copy(tooltipV = Present(TextValue.Const(v)))

    /** Reactive tooltip — native `title` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def tooltip(sig: Signal[String]): MultiSelect[A] = copy(tooltipV = Present(TextValue.Dyn(sig)))

    /** Size: `.p-multiselect-sm` / default / `.p-multiselect-lg`. */
    def size(v: Size): MultiSelect[A] = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): MultiSelect[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-multiselect-fluid`). */
    def fluid(v: Boolean): MultiSelect[A] = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): MultiSelect[A] = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): MultiSelect[A] = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): MultiSelect[A] = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): MultiSelect[A] = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): MultiSelect[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): MultiSelect[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): MultiSelect[A] = copy(accNameRefV = Present(v))

    /** Fired with the FULL updated key set after every selection change (toggle,
      * select-all, chip remove, clear).
      */
    def onChange(f: Set[String] => Any < Async): MultiSelect[A] = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the current selection — unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    def onBlur(f: Set[String] => Any < Async): MultiSelect[A] = copy(onBlurF = Present(f))

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    private def isOptionDisabled(a: A): Boolean = optionDisabledF.exists(_(a))

    /** The wired interaction state, allocated per mount. */
    final private case class State(
        open: SignalRef[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        q: SignalRef[String],
        qV: String
    )

    private[uic] def render(using Frame): UI =
        // The interaction state (open/highlight/filter) lives in signals allocated
        // by this effectful mount; static projections render the same closed
        // anatomy inert (the Select precedent).
        val stat: UI = withValue(cur => body(cur, Absent))
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                hi <- Signal.initRef(-1)
                q  <- Signal.initRef("")
            yield wired(open, hi, q)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String])(using Frame): UI =
        open.render { o =>
            hi.render { h =>
                q.render { qq =>
                    withValue(cur => body(cur, Present(State(open, o, hi, h, q, qq))))
                }
            }
        }

    private def withValue(f: Set[String] => UI)(using Frame): UI =
        valueRef match
            case Present(ref) => ref.render(f)
            case Absent       => f(Set.empty)

    /** Writes `next` into the bound set and fires `onChange` with it. */
    private def write(next: Set[String])(using Frame): Any < Async =
        for
            _ <- valueRef match
                case Present(r) => r.set(next)
                case Absent     => (): Any < Async
            _ <- onChangeF match
                case Present(g) => g(next)
                case Absent     => (): Any < Async
        yield ()

    private def body(current: Set[String], st: Maybe[State])(using Frame): UI =
        // Reactive-placeholder + -invalid gates (INSIDE the mount subscription — never around the
        // UI.mounted node). `State` is a path-dependent inner type, so both reactive re-renders thread
        // their resolved values (placeholder text; red flag + message) into `bodyStatic` directly rather
        // than through `copy(...)` (a copied instance's `State` would not match).
        TextValue.reactive(placeholderV): ph =>
            (invalidV.dynSig, invalidMsgDynV) match
                case (Absent, Absent) => bodyStatic(current, st, ph, invalidV.constTrue, invalidMsgV)
                case _ =>
                    FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) => bodyStatic(current, st, ph, red, msg))

    private def bodyStatic(
        current: Set[String],
        st: Maybe[State],
        placeholder: Maybe[String],
        redFlag: Boolean,
        redMsg: Maybe[String]
    )(using Frame): UI =
        val isOpen   = st.exists(_.isOpen)
        val selected = items.filter(a => current.contains(key(a)))

        // Toggles one option key — the panel STAYS OPEN (Prime's multiselect pick).
        def toggleOption(a: A): Any < Async =
            val k = key(a)
            write(if current.contains(k) then current - k else current + k)

        // Opens with a fresh query and no highlight.
        def openPanel(s: State): Any < Async =
            for
                _ <- s.q.set("")
                _ <- s.hi.set(-1)
                _ <- s.open.set(true)
            yield ()

        def toggle(s: State): Any < Async =
            if s.isOpen then s.open.set(false) else openPanel(s)

        // === closed trigger ======================================================
        val overMax         = maxSelectedLabelsV.exists(m => selected.size > m)
        val showPlaceholder = selected.isEmpty && placeholder.isDefined
        val commaText       = selected.map(labelF).mkString(", ")

        val labelChildren: List[UI] =
            displayV match
                case MultiSelectDisplay.Chip if selected.nonEmpty && !overMax =>
                    selected.map { a =>
                        span.cssClass("p-multiselect-chip-item")(
                            toChild(
                                Chip(labelF(a))
                                    .extraClass("p-multiselect-chip")
                                    .removable(true)
                                    .onRemove(write(current - key(a)))
                                    .render
                            )
                        )
                    }
                case _ if overMax && selected.nonEmpty =>
                    // NBSP keeps the empty label's line box (Prime renders 'empty').
                    selectedItemsLabelV.getOrElse(TextValue.Const("{0} items selected")) match
                        case TextValue.Const(t) =>
                            val text = t.replace("{0}", selected.size.toString)
                            List(span(if text.isEmpty then " " else text))
                        case TextValue.Dyn(s) =>
                            List(s.render { raw =>
                                val text = raw.replace("{0}", selected.size.toString)
                                span(if text.isEmpty then " " else text)
                            })
                case _ =>
                    val text =
                        if selected.isEmpty then placeholder.getOrElse("")
                        else commaText
                    // NBSP keeps the empty label's line box (Prime renders 'empty').
                    List(span(if text.isEmpty then " " else text))

        var lbl = div.cssClass("p-multiselect-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if selected.isEmpty && placeholder.isEmpty then lbl = lbl.cssClass("p-multiselect-label-empty")
        // onClickSelf, not onClick: in chip mode the chips (and their remove
        // buttons) live INSIDE the label — a bubbling handler would also toggle
        // the panel on every chip remove.
        st.foreach(s => if !disabledFlag then lbl = lbl.onClickSelf(toggle(s)))
        val labelUI: UI = lbl(labelChildren.map(toChild)*)

        var lblContainer = div.cssClass("p-multiselect-label-container")
        idV.foreach(v => lblContainer = lblContainer.id(v))
        st.foreach(s => if !disabledFlag then lblContainer = lblContainer.onClickSelf(toggle(s)))
        val labelContainerUI: UI = lblContainer(toChild(labelUI))

        // Explicitly typed effect (the untyped-match Any-inference trap).
        val clearEffect: Any < Async =
            st match
                case Present(_) => write(Set.empty)
                case Absent     => ()

        val clearUI: List[UI] =
            if showClearFlag && !disabledFlag && selected.nonEmpty then
                List(
                    button
                        .cssClass("p-multiselect-clear-icon")
                        .jsProp("type", "button")
                        .aria("label", "Clear")
                        .onClick(clearEffect)(toChild(GlyphSvg(Icons.times, "p-icon")))
                )
            else Nil

        var dd = div.cssClass("p-multiselect-dropdown")
        st.foreach(s => if !disabledFlag then dd = dd.onClick(toggle(s)))
        val dropdownUI: UI = dd(toChild(GlyphSvg(Icons.chevronDown, "p-multiselect-dropdown-icon", "p-icon")))

        // Hidden form carrier: comma-joined selected keys in options order.
        val hiddenCarrier: List[UI] =
            nameV.toList.map { n =>
                hiddenInput.jsProp("name", n).value(items.filter(a => current.contains(key(a))).map(key).mkString(","))
            }

        // === floating panel ======================================================
        val panelUI: List[UI] = st.toList.map(s => overlayPanel(current, s, toggleOption))

        // === field root ==========================================================
        var el = div.cssClass("p-multiselect").cssClass("p-component").cssClass("p-inputwrapper")
        if displayV == MultiSelectDisplay.Chip then el = el.cssClass("p-multiselect-display-chip")
        if selected.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if isOpen then el = el.cssClass("p-multiselect-open").cssClass("p-uic-overlay-anchor")
        if redFlag then el = el.cssClass("p-invalid").aria("invalid", "true")
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small               => el = el.cssClass("p-multiselect-sm")
            case Size.Large | Size.XLarge => el = el.cssClass("p-multiselect-lg")
            case Size.Normal              => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-multiselect-fluid")
        tooltipV match
            case Present(TextValue.Const(v)) => el = el.jsProp("title", v)
            case Present(TextValue.Dyn(s))   => el = el.title(s)
            case Absent                      => ()
        end match
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => el = el.aria("labelledby", v))
        el = el.aria("haspopup", "listbox").aria("expanded", isOpen.toString)
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            st.foreach { s =>
                el = el.onKeyDown { e =>
                    e.key match
                        case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space if !s.isOpen => openPanel(s)
                        case _                                                                 => ()
                }
            }
            // Focus-loss on the trigger reports the current selection — the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueRef match
                    case Present(r) => r.use(f)
                    case Absent     => f(current))
            }
        end if

        FieldInvalid.withMessage(
            el(((labelContainerUI :: clearUI) ++ (dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*),
            redFlag,
            redMsg
        )
    end bodyStatic

    /** The floating option panel: Prime's `.p-multiselect-overlay` skin on the
      * [[Overlay]] primitive — header (select-all + filter), then the checkbox
      * option rows.
      */
    private def overlayPanel(current: Set[String], s: State, toggleOption: A => Any < Async)(using Frame): UI =
        val shown =
            if filterFlag && s.qV.nonEmpty then
                items.filter(a => labelF(a).toLowerCase.contains(s.qV.toLowerCase))
            else items
        val hiEff = if shown.isEmpty then -1 else math.min(s.hiV, shown.size - 1)

        /** Next enabled index from `from` in direction `step` (no wrap — Prime). */
        def move(from: Int, step: Int): Int =
            var i = from + step
            while i >= 0 && i < shown.size && isOptionDisabled(shown(i)) do i += step
            if i >= 0 && i < shown.size then i else from
        end move

        // Prime's select-all semantics: checked while every visible enabled option
        // is selected; checking replaces the WHOLE value with the visible enabled
        // keys, unchecking clears the selection entirely.
        val enabledShown = shown.filterNot(isOptionDisabled)
        val allSelected  = enabledShown.nonEmpty && enabledShown.forall(a => current.contains(key(a)))
        val toggleAll: Any < Async =
            if allSelected then write(Set.empty)
            else write(enabledShown.map(key).toSet)

        // The inert Prime checkbox anatomy on a row (the Tree precedent: no native
        // input — the row's own click handler toggles, so an input would double-fire).
        def rowCheckbox(isSel: Boolean): UI =
            var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
            if isSel then cb = cb.cssClass("p-checkbox-checked")
            val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
            cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*)))
        end rowCheckbox

        val rows: List[UI] = shown.zipWithIndex.map { (a, i) =>
            val isSel = current.contains(key(a))
            val isDis = isOptionDisabled(a)
            var row   = li.cssClass("p-multiselect-option").role("option").aria("selected", isSel.toString)
            // Prime gates the highlighted row skin behind highlightOnSelect (default
            // OFF): by default the checked box alone marks a selected row.
            if isSel && highlightOnSelectFlag then row = row.cssClass("p-multiselect-option-selected")
            if isDis then row = row.cssClass("p-disabled").aria("disabled", "true")
            if i == hiEff then row = row.cssClass("p-focus")
            if !isDis then row = row.onClick(toggleOption(a))
            row(List[UI](rowCheckbox(isSel), span(labelF(a))).map(toChild)*)
        }
        val emptyRow: List[UI] =
            if shown.isEmpty then
                List(emptyMessageV.getOrElse(TextValue.Const("No results found")) match
                    case TextValue.Const(t) => li.cssClass("p-multiselect-empty-message").role("option")(t): UI
                    case TextValue.Dyn(s)   => s.render(t => li.cssClass("p-multiselect-empty-message").role("option")(t)))
            else Nil

        val listUI: UI =
            div.cssClass("p-multiselect-list-container")(
                toChild(
                    ul.cssClass("p-multiselect-list").role("listbox").aria("multiselectable", "true")(
                        (rows ++ emptyRow).map(toChild)*
                    )
                )
            )

        // Header: the REAL uic.CheckBox (interactive select-all — const-checked, so
        // no nested subscription) plus Prime's IconField filter container.
        val headerKids: List[UI] =
            (if showToggleAllFlag then
                 List(
                     CheckBox()
                         .checked(allSelected)
                         .accessibleName("Toggle All")
                         .onChange(_ => toggleAll)
                         .render
                 )
             else Nil) ++
                (if filterFlag then
                     List(
                         div.cssClass("p-iconfield").cssClass("p-multiselect-filter-container")(
                             toChild(
                                 input
                                     .cssClass("p-multiselect-filter")
                                     .cssClass("p-inputtext")
                                     .cssClass("p-component")
                                     .cssClass("p-uic-iconfield-end")
                                     .role("searchbox")
                                     .aria("label", "Filter")
                                     .value(s.q)
                                     .onInput(_ => s.hi.set(0))
                             ),
                             toChild(
                                 span.cssClass("p-inputicon")(
                                     toChild(GlyphSvg(Icons.search, "p-icon"))
                                 )
                             )
                         )
                     )
                 else Nil)

        val header: List[UI] =
            if headerKids.nonEmpty then List(div.cssClass("p-multiselect-header")(headerKids.map(toChild)*))
            else Nil

        Overlay(s.open)
            .panelClass("p-multiselect-overlay")
            .panelClass("p-component")
            .onPanelKeyDown { e =>
                e.key match
                    case Keyboard.ArrowDown                                                                    => s.hi.set(move(hiEff, 1))
                    case Keyboard.ArrowUp                                                                      => s.hi.set(move(hiEff, -1))
                    case Keyboard.Enter if hiEff >= 0 && hiEff < shown.size && !isOptionDisabled(shown(hiEff)) =>
                        // Toggles the highlighted option; the panel STAYS OPEN (Prime).
                        toggleOption(shown(hiEff))
                    case _ => ()
            }((header :+ listUI)*)
            .render
    end overlayPanel
end MultiSelect

object MultiSelect:
    def apply[A](): MultiSelect[A] = new MultiSelect[A](Nil, _.toString)
