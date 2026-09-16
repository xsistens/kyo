package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

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
  * (outside click / Escape close it, and the panel takes no focus: the trigger
  * keeps it, or a filter header's input does), but the
  * value binds to a `SignalRef[Set[String]]` of option keys and PICKING DOES NOT
  * CLOSE — clicking a row (or Enter on the `.p-focus` highlight) toggles the key
  * and the panel stays open, exactly Prime's multiselect semantics.
  *
  * The header select-all checkbox follows PrimeVue: checked while every visible
  * non-disabled option is selected (with `filterable(true)` "visible" means the
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
  * Honest deferrals: virtual scrolling, per-option templates, and
  * `selectionLimit` are not implemented. Arrow keys on the open panel no longer
  * scroll the page underneath: the panel declares `preventScrollKeys`.
  */
final case class MultiSelect[A] private (
    optionsV: List[OptionItem[A]],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    valueRef: Maybe[SignalRef[Set[String]]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    placeholderV: Maybe[TextValue] = Absent,
    displayV: MultiSelectDisplay = MultiSelectDisplay.Comma,
    maxSelectedLabelsV: Maybe[Int] = Absent,
    selectedItemsLabelV: Maybe[TextValue] = Absent,
    filterableFlag: Boolean = false,
    filterQueryRefV: Maybe[SignalRef[String]] = Absent,
    showToggleAllFlag: Boolean = true,
    highlightOnSelectFlag: Boolean = false,
    showClearFlag: Boolean = false,
    emptyContentV: Maybe[EmptyContent] = Absent,
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
) extends Node, MultiSelectFormControl, HasEmptyContent, HasTooltip, HasPlaceholder, HasAccessibleNameRef:
    type Self = MultiSelect[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): MultiSelect[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): MultiSelect[A] =
        copy(optionsV = optionsV ++ is.map(OptionItem.Item(_)), labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): MultiSelect[A] =
        copy(optionsV = optionsV ++ is.map(a => OptionItem.Item(ev(a))), labelF = a => ev.flip(a))

    /** Appends grouped options: [[OptionItem.group]] rows render their options
      * under a labelled header, [[OptionItem.item]] rows beside them.
      *
      * A separate name rather than an `options` overload, because both would
      * erase to the same signature. Grouping changes the panel only. Select-all
      * keeps counting the visible enabled options across every group, since
      * Prime has no per-group select-all and the header checkbox speaks for the
      * whole panel.
      */
    def optionGroups(is: Seq[OptionItem[A]])(label: A => String): MultiSelect[A] =
        copy(optionsV = optionsV ++ is.toList, labelF = label)

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
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): MultiSelect[A] = copy(valueRef = Present(ref))

    /** Binds the panel visibility two-way to `ref` (optional — the panel is
      * self-managed otherwise).
      */
    def open(ref: SignalRef[Boolean]): MultiSelect[A] = copy(openRefV = Present(ref))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): MultiSelect[A] = copy(placeholderV = v)

    /** Trigger selection rendering: comma-joined text (default) or removable
      * chips (`.p-multiselect-display-chip`).
      */
    def display(v: MultiSelectDisplay): MultiSelect[A] = copy(displayV = v)

    /** Beyond `n` selected options the trigger shows the [[selectedItemsLabel]]
      * summary instead of the joined labels/chips (Prime's `maxSelectedLabels`).
      */
    def maxSelectedLabels(n: Int): MultiSelect[A] = copy(maxSelectedLabelsV = Present(n))

    /** Summary template used beyond [[maxSelectedLabels]]: `{0}` is replaced by the selection count
      * (default "{0} items selected", Prime's locale default). A `Signal[String]` re-renders the summary in
      * place on emission, and the emitted string is still a `{0}` template interpolated with the count.
      */
    def selectedItemsLabel(v: String | Signal[String]): MultiSelect[A] = copy(selectedItemsLabelV = Present(ReactiveValue(v)))

    /** Renders Prime's header filter (an IconField `.p-multiselect-filter-container`
      * with the `input.p-multiselect-filter`) over a query the control allocates
      * itself, filtering the options by a case-insensitive contains on the label
      * projection; the query resets on every open. Select-all then applies to the
      * FILTERED options (Prime). Same word and same meaning on
      * [[Select.filterable]] and [[Listbox.filterable]].
      */
    def filterable(v: Boolean): MultiSelect[A] = copy(filterableFlag = v)

    /** Renders the same header filter over a query the APP owns: typing writes
      * `ref`, external writes re-filter the panel. Implies [[filterable]].
      */
    def filterQuery(ref: SignalRef[String]): MultiSelect[A] = copy(filterQueryRefV = Present(ref))

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

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): MultiSelect[A] = copy(emptyContentV = v)

    def disabled(v: Boolean): MultiSelect[A] = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected keys (the div trigger has no native form value).
      */
    def name(v: String): MultiSelect[A] = copy(nameV = Present(v))

    private[uic] def withTooltip(v: Maybe[TextValue]): MultiSelect[A] = copy(tooltipV = v)

    /** Size: `.p-multiselect-sm` / default / `.p-multiselect-lg`. */
    def size(v: Size): MultiSelect[A] = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): MultiSelect[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-multiselect-fluid`). */
    def fluid(v: Boolean): MultiSelect[A] = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): MultiSelect[A]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): MultiSelect[A]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): MultiSelect[A] = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): MultiSelect[A] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): MultiSelect[A] = copy(accNameRefV = v)

    /** Fired with the FULL updated key set after every selection change (toggle,
      * select-all, chip remove, clear).
      */
    def onChange(f: Set[String] => Any < Async): MultiSelect[A] = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the current selection — unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): MultiSelect[A] = copy(onBlurF = Present(f))

    /** Every selectable option in render order, groups flattened away. The value
      * binding, the key-collision check, select-all and the keyboard all work on
      * this.
      */
    private def items: List[A] = OptionItem.flatten(optionsV)

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    private def isOptionDisabled(a: A): Boolean = optionDisabledF.exists(_(a))

    /** Whether the header filter renders: asked for by [[filterable]], or implied
      * by an app-owned [[filterQuery]].
      */
    private def filtering: Boolean = filterableFlag || filterQueryRefV.isDefined

    /** The wired interaction state, allocated per mount. */
    final private case class State(
        open: SignalRef[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        q: SignalRef[String],
        qV: String,
        /** The id the highlight is announced through, as [[Select]] and [[Listbox]] carry it: a
          * caller's own `id` wins, and a minted one stands in where there is none.
          */
        idBase: Maybe[String]
    )

    private[uic] def render(using Frame): UI =
        // The interaction state (open/highlight/filter) lives in signals allocated
        // by this effectful mount; static projections render the same closed
        // anatomy inert (the Select precedent).
        val stat: UI = withValue(cur => body(cur, Absent))
        UI.mounted {
            for
                cmds <- UI.commands
                base <- idV.map(v => Kyo.lift(v)).getOrElse(cmds.freshId)
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                hi <- Signal.initRef(-1)
                // Same shape as `open` above: an app-owned query ref takes over the
                // slot the component would otherwise allocate for itself.
                q <- filterQueryRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef("")
            yield wired(open, hi, q, Present(base))
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String], base: Maybe[String] = Absent)(
        using Frame
    ): UI =
        open.render { o =>
            hi.render { h =>
                q.render { qq =>
                    withValue(cur => body(cur, Present(State(open, o, hi, h, q, qq, base))))
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

        /** Where an opening key lands: the first option already selected, or the first one a
          * highlight may sit on where none is. Opening has to land ON an option, or the arrow that
          * opened the panel has moved nothing and the reader presses it twice to reach the first
          * row. `-1` only where there is no such row at all.
          */
        val openHighlight: Int =
            val sel = items.indexWhere(a => current.contains(key(a)))
            if sel >= 0 then sel else items.indexWhere(a => !isOptionDisabled(a))

        // Opens with a fresh query and the highlight on the first selected option.
        def openPanel(s: State): Any < Async =
            for
                _ <- s.q.set("")
                _ <- s.hi.set(openHighlight)
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
                            // The count is build-time, so the whole label is a projection of the one
                            // string signal: it goes in as a text child and is patched in place.
                            List(span(s.map { raw =>
                                val text = raw.replace("{0}", selected.size.toString)
                                if text.isEmpty then " " else text
                            }))
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
            case Size.Small  => el = el.cssClass("p-multiselect-sm")
            case Size.Large  => el = el.cssClass("p-multiselect-lg")
            case Size.Normal => ()
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
        // WHICH element is the combobox follows the filter header, and follows it for the same
        // reason [[Select]] does: the combobox is whatever holds focus while the list is open,
        // because `aria-activedescendant` is read off the focused element or off nothing at all.
        el = el.role(if filtering then "button" else "combobox")
            .aria("haspopup", "listbox")
            .aria("expanded", isOpen.toString)
        st.flatMap(_.idBase).foreach { b =>
            el = el.aria("controls", listId(b))
            if !filtering then
                st.foreach { s =>
                    val (_, _, _, hiEff) = shownState(s)
                    if s.isOpen && hiEff >= 0 then el = el.aria("activedescendant", optionId(b, hiEff))
                }
            end if
        }
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            // The trigger keeps focus for as long as the panel is open, so it is where the panel's
            // keyboard lives: closed, the three opening keys; open, the whole option keyboard. A
            // filter header is the exception, taking focus into the panel and the keys with it.
            st.foreach { s =>
                val (_, shown, navigable, hiEff) = shownState(s)
                el = el.onKeyDown { e =>
                    if !s.isOpen then
                        e.key match
                            case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel(s)
                            case _                                                    => ()
                    else if filtering then ()
                    else panelKey(shown, navigable, hiEff, s, toggleOption, toggleAllOf(current, shown))(e)
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
    /** The options the panel is showing, the positions the highlight may land on, and where it is
      * now. Derived in one place because the TRIGGER reads it too: the keyboard lives on whatever
      * holds focus, and that is no longer the panel.
      *
      * Prime's multiselect stops at the ends rather than cycling, as its select does, so `wrap` is
      * false wherever the navigable list is read.
      */
    private def shownState(s: State): (List[OptionItem[A]], Seq[A], List[Int], Int) =
        val shownGroups =
            if filtering && s.qV.nonEmpty then
                OptionItem.filter(optionsV, a => labelF(a).toLowerCase.contains(s.qV.toLowerCase))
            else optionsV
        val shown = OptionItem.flatten(shownGroups)
        val hiEff = if shown.isEmpty then -1 else math.min(s.hiV, shown.size - 1)
        (shownGroups, shown, shown.indices.toList.filterNot(i => isOptionDisabled(shown(i))), hiEff)
    end shownState

    /** Prime's select-all semantics: checked while every visible enabled option is selected. */
    private def isAllSelected(current: Set[String], shown: Seq[A]): Boolean =
        val enabled = shown.filterNot(isOptionDisabled)
        enabled.nonEmpty && enabled.forall(a => current.contains(key(a)))

    /** The other half of them: checking replaces the WHOLE value with the visible enabled keys,
      * unchecking clears the selection entirely. Read by the header checkbox and by the chord that
      * reaches it from the keyboard, which is why it is a method rather than a local.
      */
    private def toggleAllOf(current: Set[String], shown: Seq[A])(using Frame): Any < Async =
        if isAllSelected(current, shown) then write(Set.empty)
        else write(shown.filterNot(isOptionDisabled).map(key).toSet)

    /** The id of the header's select-all box: what tells a key pressed ON it apart from one
      * pressed on the trigger, since both reach the trigger's handler.
      */
    private def toggleAllId(base: String): String = s"$base-all"

    private def overlayPanel(current: Set[String], s: State, toggleOption: A => Any < Async)(using Frame): UI =
        val (shownGroups, shown, navigable, hiEff) = shownState(s)

        val allSelected = isAllSelected(current, shown)
        val toggleAll   = toggleAllOf(current, shown)

        // The inert Prime checkbox anatomy on a row (the Tree precedent: no native
        // input — the row's own click handler toggles, so an input would double-fire).
        def rowCheckbox(isSel: Boolean): UI =
            var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
            if isSel then cb = cb.cssClass("p-checkbox-checked")
            val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
            cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*)))
        end rowCheckbox

        val rows: List[UI] = OptionItem.rows(shownGroups, "p-multiselect-option-group") { (a, i) =>
            val isSel = current.contains(key(a))
            val isDis = isOptionDisabled(a)
            var row   = li.cssClass("p-multiselect-option").role("option").aria("selected", isSel.toString)
            // Prime gates the highlighted row skin behind highlightOnSelect (default
            // OFF): by default the checked box alone marks a selected row.
            if isSel && highlightOnSelectFlag then row = row.cssClass("p-multiselect-option-selected")
            if isDis then row = row.cssClass("p-disabled").aria("disabled", "true")
            if i == hiEff then row = row.cssClass("p-focus").scrollAuto(true)
            s.idBase.foreach(b => row = row.id(optionId(b, i)))
            if !isDis then row = row.onClick(toggleOption(a))
            row(List[UI](rowCheckbox(isSel), span(labelF(a))).map(toChild)*)
        }
        val emptyRow: List[UI] =
            if shown.isEmpty then
                List(EmptyContent.render(emptyContentV, "No results found")(c =>
                    li.cssClass("p-multiselect-empty-message").role("option")(c)
                ))
            else Nil

        val listUI: UI =
            div.cssClass("p-multiselect-list-container")(
                toChild(
                    {
                        // Named by `aria-controls` and holding the option ids; the announcement
                        // itself rides the focused element, which is never this list.
                        var list = ul.cssClass("p-multiselect-list").role("listbox").aria("multiselectable", "true")
                        s.idBase.foreach(b => list = list.id(listId(b)))
                        list((rows ++ emptyRow).map(toChild)*)
                    }
                )
            )

        // Header: the REAL uic.CheckBox (interactive select-all — const-checked, so
        // no nested subscription) plus Prime's IconField filter container.
        val headerKids: List[UI] =
            (if showToggleAllFlag then
                 // Not a tab stop: the panel's keyboard lives on the element focus is already on,
                 // and a key pressed in here would reach that element a second time by bubbling.
                 // Ctrl/Cmd+A is how the keyboard reaches it instead.
                 var allBox = CheckBox()
                     .checked(allSelected)
                     .accessibleName("Toggle All")
                     .tabbable(false)
                     .onChange(_ => toggleAll)
                 s.idBase.foreach(b => allBox = allBox.id(toggleAllId(b)))
                 List(allBox.render)
             else Nil)
            ++
                (if filtering then
                     var filterEl = input
                         .cssClass("p-multiselect-filter")
                         .cssClass("p-inputtext")
                         .cssClass("p-component")
                         .cssClass("p-uic-iconfield-end")
                         .role("combobox")
                         .aria("label", "Filter")
                         .aria("haspopup", "listbox")
                         .aria("expanded", "true")
                         .value(s.q)
                         .onInput(_ => s.hi.set(0))
                         // The header is where focus goes and therefore where the option keyboard
                         // lives: the arrows move the highlight, the caret keeps Space and the
                         // printable keys, Escape closes.
                         .focusAuto(true)
                         .focusRestore(true)
                         .preventScrollKeys
                         .onKeyDown(panelKey(shown, navigable, hiEff, s, toggleOption, toggleAll))
                     s.idBase.foreach { b =>
                         filterEl = filterEl.aria("controls", listId(b))
                         if hiEff >= 0 then filterEl = filterEl.aria("activedescendant", optionId(b, hiEff))
                     }
                     List(
                         div.cssClass("p-iconfield").cssClass("p-multiselect-filter-container")(
                             toChild(filterEl),
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
            // Nothing in this panel takes focus (a filter header seeds its own input), so nothing
            // in it receives a key either.
            .seedFocus(false)
            .dismissOnEscape(false)((header ++ keyCollisionCard ++ List(listUI))*)
            .render
    end overlayPanel

    private def optionId(base: String, index: Int): String = s"$base-option-$index"

    /** The id of the option list, which is what the combobox points `aria-controls` at. */
    private def listId(base: String): String = s"$base-list"

    /** One key over the open panel.
      *
      * The same shape [[Select]] carries, with one difference the pattern asks for: an activation
      * TOGGLES the highlighted option and leaves the panel open, since a reader picking several
      * things should not have to reopen the list between them. Prime does the same.
      *
      * A filter header holds a text field, so Space and a printable key belong to the caret there
      * and to the list otherwise. Escape closes: focus never enters the panel, so the key is
      * delivered to the trigger or to the filter header's input and never reaches the panel.
      * Tab closes it too, in both directions: the panel's keyboard lives on the element the Tab is
      * carrying the reader away from, so a panel left open behind them is one nothing answers.
      *
      * Ctrl/Cmd+A toggles the select-all the header shows, which is the ARIA listbox chord for it
      * and the only route to that box from the keyboard: the box itself is no tab stop, because a
      * key pressed on it bubbles into this same handler and would be read twice. The chord is
      * honored in the filter header too, where the browser also selects the query text: the panel's
      * options are what the reader is steering there, and the box is otherwise out of reach.
      *
      * A key that arrives FROM the select-all box (a pointer can still focus it) belongs to the
      * box, so nothing here answers it.
      */
    private def panelKey(
        shown: Seq[A],
        navigable: List[Int],
        hiEff: Int,
        s: State,
        toggleOption: A => Any < Async,
        toggleAll: Any < Async
    )(
        e: KeyboardEvent
    )(using Frame): Any < Async =
        val caretOwns     = filtering && (e.key == Keyboard.Space || e.key.charValue.isDefined)
        val fromToggleAll = s.idBase.exists(b => e.targetId.contains(toggleAllId(b)))
        val selectsAll = showToggleAllFlag && (e.modifiers.ctrl || e.modifiers.meta) &&
            (e.key match
                case Keyboard.Char(c) => c.toLower == 'a'
                case _                => false)
        if fromToggleAll then ()
        else if e.key == Keyboard.Tab then s.open.set(false)
        else if selectsAll then toggleAll
        else if caretOwns then ()
        else
            ListNav.onKey(navigable, hiEff, e.key, wrap = false) match
                case Present(step) if step.dismiss => s.open.set(false)
                case Present(step) =>
                    val toggled: Any < Async =
                        if step.activate && shown.isDefinedAt(step.focus) then toggleOption(shown(step.focus)) else ()
                    s.hi.set(step.focus).andThen(toggled)
                case Absent =>
                    e.key match
                        case Keyboard.Char(c) =>
                            Typeahead.jump(shown.map(labelF), navigable, hiEff, c) match
                                case Present(to) => s.hi.set(to)
                                case Absent      => ()
                        case _ => ()
        end if
    end panelKey

    /** The loud card shown at the top of the panel when the option keys collide (see
      * [[KeyDiagnostics]]). A `Set[String]` value makes the failure worse here than
      * in [[Select]]: colliding keys also merge two options into one selection.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        val dups = KeyDiagnostics.duplicates(items.map(key))
        if dups.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "MultiSelect",
                if keyF.isEmpty then
                    "option labels are not unique and optionKey is unset, so options share one selection entry; set optionKey"
                else "optionKey is not unique across the options, so options share one selection entry",
                dups
            ))
        end if
    end keyCollisionCard
end MultiSelect

object MultiSelect:
    def apply[A](): MultiSelect[A] = new MultiSelect[A](Nil, _.toString)
