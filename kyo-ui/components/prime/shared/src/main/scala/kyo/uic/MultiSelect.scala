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

/** MultiSelect: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * keeps it, or a filter header's input does), but `selection` binds a
  * `SignalRef[Set[A]]` of the chosen options and PICKING DOES NOT
  * CLOSE: clicking a row (or Enter on the `.p-focus` highlight) toggles the key
  * and the panel stays open, exactly Prime's multiselect semantics.
  *
  * The header select-all checkbox follows PrimeVue: checked while every visible
  * non-disabled option is selected (with the filter on, "visible" means the
  * FILTERED options), unchecked otherwise (Prime renders no indeterminate
  * state here); checking selects exactly the visible enabled options (replacing
  * the whole value, like Prime), unchecking clears the selection entirely.
  *
  * Trigger label: comma-joined labels of the selected options in OPTIONS order
  * (a `Set` carries no pick order); `maxSelectedLabels(n)` switches to the
  * `selectedItemsLabel` summary ("{0} items selected", `{0}` = count) beyond
  * `n` selections; `display(MultiSelectDisplay.Chip)` renders removable
  * [[Chip]]s instead.
  *
  * The field is the one element listening for a click, as in Prime: a click
  * anywhere on it toggles the panel, a chip's body included. The clear button,
  * each chip's remove button and the panel end their clicks where they land
  * (kyo's `stopPropagation`), so a removal or a pick never also toggles.
  *
  * Honest deferrals: virtual scrolling, per-option templates, and
  * `selectionLimit` are not implemented. Arrow keys on the open panel no longer
  * scroll the page underneath: the panel declares `preventScrollKeys`.
  *
  * `id` lands on the field root, the combobox that holds the tab stop, and the panel derives `$id-list` and
  * `$id-option-<n>` from it (a minted base stands in where none is given).
  */
final class MultiSelect[A] private[uic] (private[uic] val p: MultiSelect.Props[A]) extends Node, FormControl[Set[A]],
      HasEmptyContent,
      HasTooltip, HasPlaceholder, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = MultiSelect[A]

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new MultiSelect(p.copy(access = v))

    /** Appends typed options with their text projection: a collection, or a signal of options for a list that changes
      * while the field is shown (read in the field's own region, so the field is not built again around it).
      */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): MultiSelect[A] =
        new MultiSelect(p.copy(optionSources = optionSources :+ OptionSource.of(is), labelF = label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): MultiSelect[A] =
        new MultiSelect(p.copy(
            optionSources = optionSources :+ OptionSource.of(ev.substituteCo[[X] =>> Iterable[X] | Signal[Seq[X]]](is)),
            labelF = a => ev.flip(a)
        ))

    /** Appends grouped options: [[OptionItem.group]] rows render their options
      * under a labelled header, [[OptionItem.item]] rows beside them.
      *
      * A separate name rather than an `options` overload, because both would
      * erase to the same signature. Grouping changes the panel only. Select-all
      * keeps counting the visible enabled options across every group, since
      * Prime has no per-group select-all and the header checkbox speaks for the
      * whole panel.
      */
    def optionGroups(is: Iterable[OptionItem[A]] | Signal[Seq[OptionItem[A]]])(label: A => String): MultiSelect[A] =
        new MultiSelect(p.copy(optionSources = optionSources :+ OptionSource.rows(is), labelF = label))

    /** Stable per-option key: the value written into the bound set (defaults to
      * the label projection).
      */
    def optionKey(f: A => String): MultiSelect[A] = new MultiSelect(p.copy(keyF = Present(f)))

    /** Renders matching options disabled (`.p-disabled` rows, shown in the panel
      * but not pickable; the keyboard highlight and select-all skip them).
      */
    def optionDisabled(f: A => Boolean): MultiSelect[A] = new MultiSelect(p.copy(optionDisabledF = Present(f)))

    /** The set of chosen options. A `SignalRef` binds two way: toggles write the options that carry the chosen
      * [[optionKey]]s, ref changes reselect the rows whose keys its members carry. A plain `Signal` or a constant binds
      * one way: the field shows it and a toggle only reports the set it asks for to [[onChange]], so the app writes its
      * own state; with no [[onChange]] that is reported in a card. A member no option carries stays in the set while the
      * others toggle, and the trigger shows it from the value itself.
      */
    def selection(v: Set[A] | Signal[Set[A]]): MultiSelect[A] =
        new MultiSelect(p.copy(choice = Present(MultiChoice(ReactiveValue[Set[A]](v)))))

    /** Says the options are still arriving: the dropdown icon turns into Prime's spinner, and a held option no row carries
      * yet is shown from the value itself rather than reported in a `.p-uic-diagnostic` card.
      */
    def loading(v: Boolean | Signal[Boolean]): MultiSelect[A] = new MultiSelect(p.copy(loadingV = Present(ReactiveValue(v))))

    /** Binds the panel visibility. A `SignalRef` binds two way: the trigger, a pick that closes, Escape, Tab and an
      * outside click write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the panel
      * follows it and the field only reports. A constant nobody hears is fixed: `false` draws the field read-only;
      * `true` keeps the panel open with nothing that closes it (no backdrop, no click on the field), while the field stays
      * the tab stop with the option keyboard and a pick is still the reader's. The combobox has no readonly state for its
      * list, so that says nothing beyond `aria-expanded`. Unbound, the field keeps the state itself, starting closed. A
      * signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): MultiSelect[A] = new MultiSelect(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the panel visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): MultiSelect[A] = new MultiSelect(p.copy(openSlot = openSlot.hear(f)))

    /** Asks for the panel open or closed: a two-way binding is written, then the change reported. */
    private def askOpen(open: Controlled[Boolean], v: Boolean)(using Frame): UI.Action = open.change(v)

    /** A panel bound closed by a constant nobody hears: nothing the reader does may open it. */
    private def openFixedShut: Boolean = openSlot.fixedAt(false)

    /** A panel held open by a constant nobody hears: nothing the reader does may close it. */
    private def openFixedOpen: Boolean = openSlot.fixedAt(true)

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): MultiSelect[A] = new MultiSelect(p.copy(placeholderV = v))

    /** Trigger selection rendering: comma-joined text (default) or removable
      * chips (`.p-multiselect-display-chip`).
      */
    def display(v: MultiSelectDisplay): MultiSelect[A] = new MultiSelect(p.copy(displayV = v))

    /** Beyond `n` selected options the trigger shows the [[selectedItemsLabel]]
      * summary instead of the joined labels/chips (Prime's `maxSelectedLabels`).
      */
    def maxSelectedLabels(n: Int): MultiSelect[A] = new MultiSelect(p.copy(maxSelectedLabelsV = Present(n)))

    /** Summary template used beyond [[maxSelectedLabels]]: `{0}` is replaced by the selection count
      * (default "{0} items selected", Prime's locale default). A `Signal[String]` re-renders the summary in
      * place on emission, and the emitted string is still a `{0}` template interpolated with the count.
      */
    def selectedItemsLabel(v: String | Signal[String]): MultiSelect[A] =
        new MultiSelect(p.copy(selectedItemsLabelV = Present(ReactiveValue(v))))

    /** Switches Prime's header filter (an IconField `.p-multiselect-filter-container` with the
      * `input.p-multiselect-filter`), over a query the control allocates itself unless [[filterQuery]] hands it one;
      * while it is on, the query starts fresh on every open. Select-all then applies to the FILTERED options (Prime).
      *
      * Without it, setting [[filter]] or [[filterQuery]] turns the filter on. Set, constant or signal, it decides
      * alone, so `filterable(false)` keeps the filter off whatever else is set. Off, the filter input is gone and
      * every option shows, but the query is left as it is: switched back on, the input shows it and filters by it
      * again. A signal never rebuilds the field: closed, only the trigger's role follows it; open, the panel stays
      * open and only the filter input comes and goes. Same word and same meaning on [[Select.filterable]] and
      * [[Listbox.filterable]].
      */
    def filterable(v: Boolean | Signal[Boolean]): MultiSelect[A] = new MultiSelect(p.copy(filterableV = Present(ReactiveValue(v))))

    /** How an option matches the query typed into the header filter, as `(option, query) => Boolean` with the query
      * as typed. [[Filter]] holds the ready-made ones: `filter(Filter.contains(_.name, _.email))` finds an option by
      * either text. Without it the match is a case-insensitive contains on the label projection. Setting it turns the
      * header filter on unless [[filterable]] says otherwise.
      */
    def filter(f: (A, String) => Boolean): MultiSelect[A] = new MultiSelect(p.copy(filterF = Present(f)))

    /** Renders the same header filter over a query the APP holds, as [[Select.filterQuery]] does: a `SignalRef` is written
      * by typing, a plain `Signal` filters the panel and typing is only reported and painted back, a constant is the
      * starting text. Turns the header filter on unless [[filterable]] says otherwise.
      */
    def filterQuery(v: String | Signal[String]): MultiSelect[A] = new MultiSelect(p.copy(filterQuerySlot = filterQuerySlot.bind(v)))

    /** Fired with the query the reader types, and with the empty query a new open starts from. */
    def onFilterQueryChange(f: String => UI.Action)(using Frame): MultiSelect[A] =
        new MultiSelect(p.copy(filterQuerySlot = filterQuerySlot.hear(f)))

    /** Whether the header renders the select-all checkbox (default true,
      * Prime's `showToggleAll`).
      */
    def showToggleAll(v: Boolean): MultiSelect[A] = new MultiSelect(p.copy(showToggleAllFlag = v))

    /** Whether selected rows ALSO get Prime's highlighted row skin
      * (`.p-multiselect-option-selected`). Default false, PrimeVue's default:
      * the checked box alone marks a selected row.
      */
    def highlightOnSelect(v: Boolean): MultiSelect[A] = new MultiSelect(p.copy(highlightOnSelectFlag = v))

    /** Renders the clear affordance on the trigger while the selection is
      * non-empty; it clears the bound set and fires `onChange(Set.empty)`.
      */
    def showClear(v: Boolean): MultiSelect[A] = new MultiSelect(p.copy(showClearFlag = v))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): MultiSelect[A] = new MultiSelect(p.copy(emptyContentV = v))

    def disabled(v: Boolean): MultiSelect[A] = new MultiSelect(p.copy(disabledFlag = v))

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected keys (the div trigger has no native form value).
      */
    def name(v: String): MultiSelect[A] = new MultiSelect(p.copy(nameV = Present(v)))

    private[uic] def withTooltip(v: Maybe[Prop[String]]): MultiSelect[A] = new MultiSelect(p.copy(tooltipV = v))

    /** Size: `.p-multiselect-sm` / default / `.p-multiselect-lg`. */
    def size(v: Size): MultiSelect[A] = new MultiSelect(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): MultiSelect[A] = new MultiSelect(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-multiselect-fluid`). */
    def fluid(v: Boolean): MultiSelect[A] = new MultiSelect(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                            = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self                    = new MultiSelect(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Set[A]])(using Frame): MultiSelect[A]  = selection(ref)
    private[uic] def hearBlur(f: Set[A] => UI.Action)(using Frame): MultiSelect[A] = onBlur(f)

    /** Fired with the FULL set of options now held after every selection change (toggle,
      * select-all, chip remove, clear). A gesture that changes nothing fires nothing.
      */
    @targetName("onChangeSet")
    def onChange(f: Set[A] => UI.Action)(using Frame): MultiSelect[A] = new MultiSelect(p.copy(onChangeF = Present(Callback.of(f))))

    /** Fires on focus loss (native `blur`) with the current selection; unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    def onBlur(f: Set[A] => UI.Action)(using Frame): MultiSelect[A] = new MultiSelect(p.copy(onBlurF = Present(Callback.of(f))))

    /** Every selectable option in render order, groups flattened away. The value
      * binding, the key-collision check, select-all and the keyboard all work on
      * this.
      */
    private def items: List[A] = OptionItem.flatten(optionsV)

    // Lets the one region skip a write that holds what it already showed; options are still matched by key.
    private given CanEqual[A, A] = CanEqual.derived

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    private def isOptionDisabled(a: A): Boolean = optionDisabledF.exists(_(a))

    /** Whether the header filter is on: an explicit [[filterable]] decides alone, as an explicit `invalid` does in
      * [[FieldValidity]]; without one, a [[filter]] or an app-owned [[filterQuery]] turns it on.
      */
    private def filterOn: BoolValue =
        filterableV.getOrElse(BoolValue.Const(filterF.isDefined || filterQuerySlot.binding.isDefined))

    /** How an option matches the query: [[filter]], else a contains on the label projection. */
    private def matcher: (A, String) => Boolean = filterF.getOrElse(Filter.contains(labelF))

    /** Whether the panel is open, paired with whether the filter is on. A signal switch is read only while the panel is
      * open: a closed field reads the switch nowhere but in its role, which is a channel (see `body`), so switching
      * it re-renders nothing.
      */
    private def openFiltering(open: Signal[Boolean])(using Frame): Signal[(Boolean, Boolean)] =
        filterOn match
            case BoolValue.Dyn(on)   => open.switchMap(o => if o then on.map(f => (true, f)) else Signal.initConst((false, false)))
            case BoolValue.Const(on) => open.map(o => (o, on))

    /** Clears the query for a new open while the filter is on. While it is off the query is not the panel's to touch,
      * so switching the filter back on finds it as it was.
      */
    private def freshQuery(q: Controlled[String])(using Frame): UI.Action =
        val fresh: UI.Action = q.change("")
        filterOn match
            case BoolValue.Dyn(on)     => on.current.map(isOn => if isOn then fresh else (): UI.Action)
            case BoolValue.Const(true) => fresh
            case _                     => ()
        end match
    end freshQuery

    /** The wired interaction state, allocated per mount. */
    final private case class Wired(
        open: Controlled[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        q: Controlled[String],
        qV: String,
        /** Whether the filter is on. Read only while the panel is open: closed, `false` stands in for a switch that
          * is not subscribed, and the trigger's role follows the switch itself.
          */
        filtering: Boolean,
        /** The id the highlight is announced through, as [[Select]] and [[Listbox]] carry it: a
          * caller's own `id` wins, and a minted one stands in where there is none.
          */
        idBase: Maybe[String]
    )

    private[uic] def render(using Frame): UI =
        // A slot bound one way that nothing hears: every change the reader asks for there is painted away again.
        Diagnostics.before(renderLive)("MultiSelect")(
            Diagnostic.unheard(openSlot, MultiSelect.openName, on = !disabledFlag),
            Diagnostic.unheard(filterQuerySlot, MultiSelect.queryName, on = !disabledFlag)
        )
    end render

    private def renderLive(using Frame): UI =
        // The interaction state (open/highlight/filter) lives in signals allocated
        // by this mount, which draws in the render pass (the Select precedent).
        val validity = FieldValidity(validityV)
        UI.mounted {
            for
                base <- access.id.map(v => Kyo.lift(v)).getOrElse(UI.id)
                // Unbound, the field keeps the panel state itself, starting closed.
                open <- openSlot.owned(false)
                hi   <- Signal.initRef(-1)
                // An app-owned query takes over the slot the component would otherwise allocate for itself; a constant query
                // is the starting text, kept here like an unbound one.
                q <- filterQuerySlot.ownedFrom("")
                // Unbound, the field keeps the options the reader picks itself.
                own <-
                    if choice.isEmpty then Signal.initRef(Set.empty[A]).map(r => Present(r))
                    else Kyo.lift(Maybe.empty[SignalRef[Set[A]]])
                self = own.fold(this)(r => new MultiSelect(p.copy(choice = Present(MultiChoice(ReactiveValue(r))))))
            yield new MultiSelect(self.p.copy(openSlot = open)).wiredOver(hi, q, Present(base))
        }
    end renderLive

    /** A constant nobody hears is fixed: the field is drawn read-only, reachable and readable with no panel to open and
      * no chip to remove.
      */
    private def fixed: Boolean = openFixedShut || choice.exists(_.fixed(onChangeF))

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String], base: Maybe[String] = Absent)(
        using Frame
    ): UI =
        new MultiSelect(p.copy(openSlot = openSlot.bind(open))).wiredOver(hi, filterQuerySlot.bind(q), base)
    end wired

    /** [[wired]] over the panel state however it is bound. */
    private def wiredOver(
        hi: SignalRef[Int],
        q: Controlled[String],
        base: Maybe[String]
    )(using Frame): UI =
        val validity = FieldValidity(validityV)
        val panel    = ReactiveValue.Dyn(openFiltering(openSlot.reading(false).signal)).and(ReactiveValue.Dyn(hi)).and(q.reading(""))
        panel.and(shown).and(OptionItem.reading(optionSources)).render { case (((((o, f), h), qq), (held, ld, ph)), opts) =>
            val self = new MultiSelect(p.copy(optionsV = opts))
            self.body(held, self.Wired(openSlot, o, hi, h, q, qq, f, base), ph, ld, validity)
        }
    end wiredOver

    /** What the field shows whatever its panel does: the chosen options, whether the options are still loading, and the
      * placeholder. Read beside the panel's state as ONE region (see [[ReactiveValue.and]]); the validity is stamped on the field,
      * never read here.
      */
    private def shown(using Frame): ReactiveValue[(Set[A], Boolean, Maybe[String])] =
        val held: ReactiveValue[Set[A]] = choice.fold(ReactiveValue.Const(Set.empty[A]))(_.reading)
        held.and(ReactiveValue.optional(loadingV)).and(ReactiveValue.optional(placeholderV.map(ReactiveValue[String](_)))).map {
            case ((h, ld), ph) =>
                (h, ld.getOrElse(false), ph)
        }
    end shown

    /** Writes the options that carry the keys `next` into the bound set, and fires `onChange` with the set now held where
      * that changed it. Unbound, there is no set to keep, and the options `next` names are reported as they are.
      */
    private def write(next: Set[String])(using Frame): UI.Action =
        val changed: Maybe[Set[A]] < Sync =
            choice match
                case Present(c) => c.update(items, key)(_ => next)
                case Absent     => Present(items.filter(a => next.contains(key(a))).toSet)
        changed.map {
            case Present(now) =>
                onChangeF match
                    case Present(g) => g(now)
                    case Absent     => (): UI.Action
            case Absent => (): UI.Action
        }
    end write

    private def body(
        held: Set[A],
        st: Wired,
        placeholder: Maybe[String],
        loading: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        val isOpen = st.isOpen
        // Whether the filter is on, as far as this render reads it: only an open panel subscribes a signal switch.
        val filtering = st.filtering
        val current   = held.map(key)
        // Held options no row carries: the trigger still shows them from the value itself, after the known ones.
        val strays   = held.filterNot(a => items.exists(i => key(i) == key(a))).toList.sortBy(labelF)
        val selected = items.filter(a => current.contains(key(a))) ++ strays

        // Toggles one option key; the panel STAYS OPEN (Prime's multiselect pick).
        def toggleOption(a: A): UI.Action =
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
        def openPanel(s: Wired): UI.Action =
            for
                _ <- freshQuery(s.q)
                _ <- s.hi.set(openHighlight)
                _ <- askOpen(s.open, true)
            yield ()

        def toggle(s: Wired): UI.Action =
            if s.isOpen then askOpen(s.open, false) else openPanel(s)

        // === closed trigger ======================================================
        val overMax         = maxSelectedLabelsV.exists(m => selected.size > m)
        val showPlaceholder = selected.isEmpty && placeholder.isDefined
        val commaText       = selected.map(labelF).mkString(", ")

        val labelChildren: List[UI] =
            displayV match
                case MultiSelectDisplay.Chip if selected.nonEmpty && !overMax =>
                    selected.map { a =>
                        span.cssClass("p-multiselect-chip-item")(
                            Chip(labelF(a))
                                .extraClass("p-multiselect-chip")
                                // A disabled field changes nothing, so its chips offer no removal.
                                .removable(!disabledFlag && !fixed)
                                .isolateRemove(true)
                                .onRemove(write(current - key(a)))
                                .render
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
        val labelUI: UI = lbl(UI.children(labelChildren)*)

        val labelContainerUI: UI = div.cssClass("p-multiselect-label-container")(labelUI)

        val clearEffect: UI.Action = write(Set.empty)

        val clearUI: List[UI] =
            if showClearFlag && !disabledFlag && !fixed && selected.nonEmpty then
                List(
                    button
                        .cssClass("p-multiselect-clear-icon")
                        .jsProp("type", "button")
                        .aria(AriaKey.label -> "Clear")
                        .onClick(clearEffect)
                        // The clear sits inside the field, whose click opens the panel.
                        .stopPropagation(true)(GlyphSvg(Icons.times, "p-icon"))
                )
            else Nil

        val dd             = div.cssClass("p-multiselect-dropdown")
        val dropdownUI: UI =
            if loading then dd(GlyphSvg(Icons.spinner, "p-multiselect-loading-icon", "p-icon", "p-icon-spin"))
            else dd(GlyphSvg(Icons.chevronDown, "p-multiselect-dropdown-icon", "p-icon"))

        // Hidden form carrier: comma-joined selected keys in options order.
        val hiddenCarrier: List[UI] =
            nameV.toList.map { n =>
                hiddenInput.jsProp("name", n).value(items.filter(a => current.contains(key(a))).map(key).mkString(","))
            }

        // === floating panel ======================================================
        val panelUI: List[UI] = if st.isOpen then List(overlayPanel(current, st, toggleOption)) else Nil

        // === field root ==========================================================
        var el = div.cssClass("p-multiselect").cssClass("p-component").cssClass("p-inputwrapper")
        // On the root, the combobox that holds the tab stop, not on the label container: see
        // [[HasElementId]]. The panel's parts derive from the same value (`$id-list`, `$id-option-<n>`).
        access.id.foreach(v => el = el.id(v))
        if displayV == MultiSelectDisplay.Chip then el = el.cssClass("p-multiselect-display-chip")
        if selected.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if loading then el = el.aria(AriaKey.busy -> "true")
        if isOpen then el = el.cssClass("p-multiselect-open").cssClass("p-uic-overlay-anchor")
        el = validity.mark(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small  => el = el.cssClass("p-multiselect-sm")
            case Size.Large  => el = el.cssClass("p-multiselect-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-multiselect-fluid")
        el = el.title(tooltipV)
        access.name.foreach(v => el = el.aria(AriaKey.label -> v))
        access.nameRef.foreach(v => el = el.aria(AriaKey.labelledby -> v))
        // WHICH element is the combobox follows the filter header, and follows it for the same
        // reason [[Select]] does: the combobox is whatever holds focus while the list is open,
        // because `aria-activedescendant` is read off the focused element or off nothing at all.
        // A signal switch is a `role` attribute CHANNEL rather than a read: a closed field subscribes nothing else
        // of it, so switching the filter patches the role in place and rebuilds nothing.
        el = filterOn match
            case BoolValue.Dyn(on) => el.withReactiveAttr("role", on.map(isOn => if isOn then "button" else "combobox"))
            case _                 => el.role(if filtering then "button" else "combobox")
        el = el
            .aria(AriaKey.haspopup -> "listbox")
            .aria(AriaKey.expanded -> isOpen)
        st.idBase.foreach { b =>
            el = el.aria(AriaKey.controls -> listId(b))
            if !filtering then
                val (_, _, _, hiEff) = shownState(st)
                if st.isOpen && hiEff >= 0 then el = el.aria(AriaKey.activedescendant -> optionId(b, hiEff))
            end if
        }
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            if fixed then el = el.aria(AriaKey.readonly -> "true")
            // The one click listener: a click anywhere on the field toggles, a chip's body included.
            // The clear button, the chips' remove buttons and the panel (`Overlay.containClicks`)
            // end their own clicks where they land.
            // A panel fixed open has no close for a click to ask for, so the field takes none.
            if !fixed && !openFixedOpen then el = el.onClick(toggle(st))
            // The trigger keeps focus for as long as the panel is open, so it is where the panel's
            // keyboard lives: closed, the three opening keys; open, the whole option keyboard. A
            // filter header is the exception, taking focus into the panel and the keys with it.
            if !fixed then
                val (_, shown, navigable, hiEff) = shownState(st)
                el = el.onKeyDown { e =>
                    if !st.isOpen then
                        e.key match
                            case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel(st)
                            case _                                                    => ()
                    else if st.filtering then ()
                    else panelKey(shown, navigable, hiEff, st, toggleOption, toggleAllOf(current, shown))(e)
                }
            end if
            // Focus-loss on the trigger reports the current selection: the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(choice match
                    case Present(c) => c.read(f)
                    case Absent     => f(held))
            }
        end if

        val field = validity.follow(el(UI.children((labelContainerUI :: clearUI) ++ (dropdownUI :: hiddenCarrier) ++ panelUI)*))
        // A selection bound one way that nothing hears: every toggle is painted away again.
        Diagnostics.after(field)("MultiSelect")(
            Diagnostic.unknown(if loading then Nil else strays.map(labelF)),
            Diagnostic(!disabledFlag && choice.exists(_.deaf(onChangeF)), MultiSelect.selectionName.advice)
        )
    end body

    /** The options the panel is showing, the positions the highlight may land on, and where it is
      * now. Derived in one place because the TRIGGER reads it too: the keyboard lives on whatever
      * holds focus, and that is no longer the panel.
      *
      * Prime's multiselect stops at the ends rather than cycling, as its select does, so `wrap` is
      * false wherever the navigable list is read.
      */
    private def shownState(s: Wired): (List[OptionItem[A]], Seq[A], List[Int], Int) =
        val shownGroups =
            if s.filtering && s.qV.nonEmpty then
                val m = matcher
                OptionItem.filter(optionsV, a => m(a, s.qV))
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
    private def toggleAllOf(current: Set[String], shown: Seq[A])(using Frame): UI.Action =
        if isAllSelected(current, shown) then write(Set.empty)
        else write(shown.filterNot(isOptionDisabled).map(key).toSet)

    /** The id of the header's select-all box: what tells a key pressed ON it apart from one
      * pressed on the trigger, since both reach the trigger's handler.
      */
    private def toggleAllId(base: String): String = s"$base-all"

    /** The floating option panel: Prime's `.p-multiselect-overlay` skin on the
      * [[Overlay]] primitive: header (select-all + filter), then the checkbox
      * option rows.
      */
    private def overlayPanel(current: Set[String], s: Wired, toggleOption: A => UI.Action)(using Frame): UI =
        val (shownGroups, shown, navigable, hiEff) = shownState(s)

        val allSelected = isAllSelected(current, shown)
        val toggleAll   = toggleAllOf(current, shown)

        // The inert Prime checkbox anatomy on a row (the Tree precedent: no native
        // input: the row's own click handler toggles, so an input would double-fire).
        def rowCheckbox(isSel: Boolean): UI =
            var cb = div.cssClass("p-checkbox").cssClass("p-component").aria(AriaKey.hidden -> "true")
            if isSel then cb = cb.cssClass("p-checkbox-checked")
            val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
            cb(div.cssClass("p-checkbox-box")(UI.children(icon)*))
        end rowCheckbox

        val rows: List[UI] = OptionItem.rows(shownGroups, "p-multiselect-option-group") { (a, i) =>
            val isSel = current.contains(key(a))
            val isDis = isOptionDisabled(a)
            var row   = li.cssClass("p-multiselect-option").role("option").aria(AriaKey.selected -> isSel)
            // Prime gates the highlighted row skin behind highlightOnSelect (default
            // OFF): by default the checked box alone marks a selected row.
            if isSel && highlightOnSelectFlag then row = row.cssClass("p-multiselect-option-selected")
            if isDis then row = row.cssClass("p-disabled").aria(AriaKey.disabled -> "true")
            if i == hiEff then row = row.cssClass("p-focus").scrollAuto(true)
            s.idBase.foreach(b => row = row.id(optionId(b, i)))
            if !isDis then row = row.onClick(toggleOption(a))
            row(UI.children(List[UI](rowCheckbox(isSel), span(labelF(a))))*)
        }
        val emptyRow: List[UI] =
            if shown.isEmpty then
                List(EmptyContent.render(emptyContentV, "No results found")(c =>
                    li.cssClass("p-multiselect-empty-message").role("option")(c)
                ))
            else Nil

        val listUI: UI =
            div.cssClass("p-multiselect-list-container")(
                {
                    // Named by `aria-controls` and holding the option ids; the announcement
                    // itself rides the focused element, which is never this list.
                    var list = ul.cssClass("p-multiselect-list").role("listbox").aria(AriaKey.multiselectable -> "true")
                    s.idBase.foreach(b => list = list.id(listId(b)))
                    list(UI.children(rows ++ emptyRow)*)
                }
            )

        // Header: the REAL uic.CheckBox (interactive select-all, const-checked so
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
                (if s.filtering then
                     var filterEl = input
                         .cssClass("p-multiselect-filter")
                         .cssClass("p-inputtext")
                         .cssClass("p-component")
                         .cssClass("p-uic-iconfield-end")
                         .role("combobox")
                         .aria(AriaKey.label -> "Filter")
                         .aria(AriaKey.haspopup -> "listbox")
                         .aria(AriaKey.expanded -> "true")
                         // Typing re-filters and highlights the first match; one way it is only reported, and kyo-ui puts
                         // the field back to the owner's query.
                         .onInput(text =>
                             s.hi.set(0)
                                 .andThen(s.q.change(text))
                         )
                         // The header is where focus goes and therefore where the option keyboard
                         // lives: the arrows move the highlight, the caret keeps Space and the
                         // printable keys, Escape closes.
                         .focusAuto(true)
                         .focusRestore(true)
                         .preventScrollKeys
                         .onKeyDown(panelKey(shown, navigable, hiEff, s, toggleOption, toggleAll))
                     // Two way, the field writes the query itself; one way it shows what the owner holds.
                     filterEl = s.q.writable.fold(filterEl.value(s.qV))(r => filterEl.value(r))
                     s.idBase.foreach { b =>
                         filterEl = filterEl.aria(AriaKey.controls -> listId(b))
                         if hiEff >= 0 then filterEl = filterEl.aria(AriaKey.activedescendant -> optionId(b, hiEff))
                     }
                     List(
                         div.cssClass("p-iconfield").cssClass("p-multiselect-filter-container")(
                             filterEl,
                             span.cssClass("p-inputicon")(
                                 GlyphSvg(Icons.search, "p-icon")
                             )
                         )
                     )
                 else Nil)

        val header: List[UI] =
            if headerKids.nonEmpty then List(div.cssClass("p-multiselect-header")(UI.children(headerKids)*))
            else Nil

        Overlay.hosted(s.open)
            .panelClass("p-multiselect-overlay")
            .panelClass("p-component")
            // Nothing in this panel takes focus (a filter header seeds its own input), so nothing
            // in it receives a key either.
            .seedFocus(false)
            .dismissOnEscape(false)
            .containClicks(true)((header ++ keyCollisionCard ++ List(listUI))*)
            .renderShown(s.idBase.map(b => s"$b-panel"))
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
        s: Wired,
        toggleOption: A => UI.Action,
        toggleAll: UI.Action
    )(
        e: KeyboardEvent
    )(using Frame): UI.Action =
        val caretOwns     = s.filtering && (e.key == Keyboard.Space || e.key.charValue.isDefined)
        val fromToggleAll = s.idBase.exists(b => e.targetId.contains(toggleAllId(b)))
        val selectsAll    = showToggleAllFlag && (e.modifiers.ctrl || e.modifiers.meta) &&
            (e.key match
                case Keyboard.Char(c) => c.toLower == 'a'
                case _                => false)
        if fromToggleAll then ()
        else if e.key == Keyboard.Tab then askOpen(s.open, false)
        else if selectsAll then toggleAll
        else if caretOwns then ()
        else
            ListNav.onKey(navigable, hiEff, e.key, wrap = false) match
                case Present(step) if step.dismiss => askOpen(s.open, false)
                case Present(step)                 =>
                    val toggled: UI.Action =
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
      * [[Diagnostics]]). A `Set[String]` value makes the failure worse here than
      * in [[Select]]: colliding keys also merge two options into one selection.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        Diagnostics.cards("MultiSelect")(Diagnostic.naming(
            if keyF.isEmpty then
                "option labels are not unique and optionKey is unset, so options share one selection entry; set optionKey"
            else "optionKey is not unique across the options, so options share one selection entry",
            Diagnostics.duplicates(items.map(key))
        )).toList
    end keyCollisionCard
end MultiSelect

object MultiSelect:
    final private[uic] case class Props[A](
        // The rows the body draws, resolved from `optionSources` by the region that draws them.
        optionsV: List[OptionItem[A]],
        labelF: A => String,
        keyF: Maybe[A => String] = Absent,
        optionDisabledF: Maybe[A => Boolean] = Absent,
        choice: Maybe[MultiChoice[A]] = Absent,
        openSlot: Controlled[Boolean] = Controlled(),
        placeholderV: Maybe[Prop[String]] = Absent,
        displayV: MultiSelectDisplay = MultiSelectDisplay.Comma,
        maxSelectedLabelsV: Maybe[Int] = Absent,
        selectedItemsLabelV: Maybe[TextValue] = Absent,
        filterableV: Maybe[BoolValue] = Absent,
        filterF: Maybe[(A, String) => Boolean] = Absent,
        filterQuerySlot: Controlled[String] = Controlled(),
        showToggleAllFlag: Boolean = true,
        highlightOnSelectFlag: Boolean = false,
        showClearFlag: Boolean = false,
        emptyContentV: Maybe[EmptyContent] = Absent,
        disabledFlag: Boolean = false,
        nameV: Maybe[String] = Absent,
        tooltipV: Maybe[Prop[String]] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onChangeF: Maybe[Set[A] => UI.Action] = Absent,
        onBlurF: Maybe[Set[A] => UI.Action] = Absent,
        loadingV: Maybe[BoolValue] = Absent,
        optionSources: List[OptionSource[A]] = Nil
    )

    private val openName      = Controlled.Name("MultiSelect", "the panel", "an open or a close", "onOpenChange")
    private val queryName     = Controlled.Name("MultiSelect", "the filter query", "the reader type", "onFilterQueryChange")
    private val selectionName = Controlled.Name("MultiSelect", "the selection", "a pick", "onChange")

    def apply[A](): MultiSelect[A] = new MultiSelect[A](Props(Nil, _.toString))
end MultiSelect
