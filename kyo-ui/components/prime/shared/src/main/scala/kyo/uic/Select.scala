package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Select: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Select
  * anatomy: `div.p-select.p-component[.p-invalid][.p-disabled][.p-select-sm|-lg]
  * [.p-variant-filled][.p-select-fluid][.p-select-open]` > `span.p-select-label`
  * + optional `button.p-select-clear-icon` + `span.p-select-dropdown` with the
  * chevron, floating `div.p-select-overlay` panel > optional
  * `div.p-select-header` filter > `div.p-select-list-container` >
  * `ul.p-select-list[role=listbox]` of `li.p-select-option` rows), so the
  * extracted `@primeuix` select CSS applies verbatim.
  *
  * The option panel is the REAL floating panel, built on the [[Overlay]]
  * primitive: the trigger toggles it, an outside click or Escape closes it, and
  * the panel takes NO focus: the trigger keeps it, which is what makes the
  * trigger the combobox and lets its `aria-activedescendant` be read at all.
  * ArrowDown/ArrowUp move the `.p-focus` highlight (disabled options skipped),
  * Home/End reach the ends, a printable key jumps to the option it starts, Enter
  * or Space picks the highlighted option, Escape closes. A filter header is the
  * one exception: it holds a text box, so focus is seeded THERE and that box
  * becomes the combobox, exactly as in [[AutoComplete]]. `filterable(true)`
  * renders Prime's header filter input over a query the select owns,
  * `filterQuery(ref)` over one the app owns, and `filter` says how an option
  * matches it (case-insensitive contains on the label projection by default, see
  * [[Filter]]); `showClear(true)` the clear affordance on the trigger while a value
  * is set; `checkmark(true)` Prime's per-option check column.
  *
  * Options are TYPED: `options(items)(label)` projects any `A` to its visible
  * text; `optionKey` (defaults to the label projection) supplies the stable key
  * options are matched by. `optionGroups(...)` takes [[OptionItem]] rows
  * instead, rendering each group under a labelled header; grouping is a panel
  * concern only, so the value model, the key projection and the keyboard address
  * the same flat option sequence either way, and a filter that empties a group
  * removes its header with it. `selection(ref)` binds the chosen option two-way
  * (over `A`, or over `Maybe[A]` for a field that may stay empty), and
  * `optionsWithNone` adds a labelled none row to a select over `Maybe`;
  * `itemTemplate` draws the rows and the chosen value; `loading` says the options
  * are still arriving, and without it a held option no row carries is named in a
  * `.p-uic-diagnostic` card. `open(ref)`
  * optionally exposes the panel visibility (it is self-managed otherwise). The interaction
  * state (open/highlight/filter query) lives in signals allocated by an
  * `UI.mounted` region, which draws in the render pass: a static page shows the real field.
  * Form participation: `name(...)` emits a hidden `<input>` carrying the current
  * key (a div trigger has no native form value).
  *
  * Honest deferrals: editable mode and virtual scrolling are not implemented yet. Arrow keys on the open panel no longer scroll the
  * page underneath: the panel declares `preventScrollKeys`.
  *
  * `id` lands on the field root, the combobox that holds the tab stop, and the panel derives `$id-list` and
  * `$id-option-<n>` from it (a minted base stands in where none is given).
  */
final class Select[A] private[uic] (private[uic] val p: Select.Props[A]) extends Node, FormControl[Maybe[A]], HasEmptyContent, HasTooltip,
      HasPlaceholder, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = Select[A]

    /** Package-internal class hook: hosts (Paginator) stamp Prime's contextual
      * class (`p-paginator-rpp-dropdown`) onto the field root.
      */
    private[uic] def extraClass(cls: String): Select[A] = new Select(p.copy(extraClassesV = extraClassesV :+ cls))

    /** Package-internal: FloatLabel stamps Prime's wrapper-state classes
      * (`p-inputwrapper` + `p-inputwrapper-filled` while a value is selected or a
      * non-empty placeholder shows text), stamped INSIDE the component's own
      * subscription so the filled state tracks the bound ref.
      */
    private[uic] def inputwrapper: Select[A] = new Select(p.copy(inputwrapperFlag = true))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Select(p.copy(access = v))

    /** Package-internal: hosts that already render inside their own subscription
      * supply the resolved selection directly (no nested subscription; writes go
      * through `onChange`).
      */
    private[uic] def current(v: Maybe[A]): Select[A] = new Select(p.copy(currentV = Present(v)))

    /** Appends typed options with their text projection: a collection, or a signal of options for a list that changes
      * while the field is shown (read in the field's own region, so the field is not built again around it).
      */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): Select[A] =
        new Select(p.copy(optionSources = optionSources :+ OptionSource.of(is), labelF = label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): Select[A] =
        new Select(p.copy(
            optionSources = optionSources :+ OptionSource.of(ev.substituteCo[[X] =>> Iterable[X] | Signal[Seq[X]]](is)),
            labelF = a => ev.flip(a)
        ))

    /** Appends grouped options: [[OptionItem.group]] rows render their options
      * under a labelled header, [[OptionItem.item]] rows beside them.
      *
      * A separate name rather than an `options` overload, because both would
      * erase to the same signature. Grouping changes the panel only: the value
      * model, the key projection and the keyboard all address the same flat
      * option sequence they do without it.
      */
    def optionGroups(is: Iterable[OptionItem[A]] | Signal[Seq[OptionItem[A]]])(label: A => String): Select[A] =
        new Select(p.copy(optionSources = optionSources :+ OptionSource.rows(is), labelF = label))

    /** Stable per-option key: the value written into the bound ref.
      *
      * Defaults to the label projection, which is correct only while the labels are
      * unique. When they are not, two options share one key and a pick applies to
      * the wrong one, a failure that appears only with the data that triggers it,
      * so it survives development and shows up in production. The panel therefore
      * renders a loud `.p-uic-diagnostic` card whenever the derived keys collide,
      * naming the offending keys.
      */
    def optionKey(f: A => String): Select[A] = new Select(p.copy(keyF = Present(f)))

    /** Renders matching options disabled (`.p-disabled` rows, shown in the panel
      * but not pickable; the keyboard highlight skips them).
      */
    def optionDisabled(f: A => Boolean): Select[A] = new Select(p.copy(optionDisabledF = Present(f)))

    /** Appends `none` and then `is` as the options of a select over `Maybe[B]`: `Absent` first, labelled `none`, then
      * `Present(b)` for each value, labelled `label(b)`.
      *
      * The none option is a row like any other, so a filter's "all" is one the reader picks; a field that may be left empty
      * binds `selection` over a `Maybe` with a placeholder instead. Only a select whose options are `Maybe[B]` takes it.
      */
    def optionsWithNone[B](none: String, is: Iterable[B])(label: B => String)(using ev: Maybe[B] =:= A): Select[A] =
        val all: List[Maybe[B]] = (Absent: Maybe[B]) :: is.toList.map(b => Present(b): Maybe[B])
        new Select(p.copy(
            optionSources = optionSources :+ OptionSource.Fixed(all.map(m => OptionItem.Item(ev(m)))),
            labelF = a => ev.flip(a).fold(none)(label)
        ))
    end optionsWithNone

    /** Custom option content: arbitrary UI drawn in each option row and, for the chosen option, in the trigger, in place of
      * the text projection. The label projection stays each row's accessible name and the text the filter and the
      * printable keys match, and through [[optionKey]] the key.
      */
    def itemTemplate(f: A => UI): Select[A] = new Select(p.copy(itemTemplateF = Present(f)))

    /** Binds the selection to the option itself: the field shows the row whose [[optionKey]] the value carries, and a pick
      * writes the option. The value always holds an option, so there is no clear (`showClear` draws nothing).
      * A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      * With neither event set, a card says the reader cannot change it.
      */
    def selection(v: A | Signal[A]): Select[A] = new Select(p.copy(choice = Present(SingleChoice.Value(ReactiveValue(v)))))

    /** Binds the selection to the option or none: `Absent` shows the placeholder, a pick writes `Present(option)`, the clear
      * writes `Absent`. Two-way and one-way as for the option binding above.Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      * With neither event set, a card says the reader cannot change it.
      * `@targetName` because both `selection` bindings erase to one signature.
      */
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): Select[A] =
        new Select(p.copy(choice = Present(SingleChoice.Optional(ReactiveValue(v)))))

    /** Binds the panel visibility. A `SignalRef` binds two way: the trigger, a pick, Escape, Tab and an outside click
      * write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the panel follows it and the
      * select only reports. A constant nobody hears is fixed: `false` draws the select read-only; `true` keeps the panel
      * open with nothing that closes it (no backdrop, no click on the field), while the field stays the tab stop with the
      * option keyboard and a pick is still the reader's. The combobox has no readonly state for its list, so that says
      * nothing beyond `aria-expanded`. Unbound, the select keeps the state itself, starting closed. A signal nothing hears
      * is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): Select[A] = new Select(p.copy(openSlot = openSlot.bind(v)))

    /** A panel held open by a constant nobody hears: nothing the reader does may close it. */
    private def openFixedOpen: Boolean = openSlot.fixedAt(true)

    /** Fired with the panel visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Select[A] = new Select(p.copy(openSlot = openSlot.hear(f)))

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): Select[A] = new Select(p.copy(placeholderV = v))

    /** Switches Prime's header filter (`div.p-select-header` > `input.p-select-filter`), over a query the select
      * allocates itself unless [[filterQuery]] hands it one; while it is on, the query starts fresh on every open.
      *
      * Without it, setting [[filter]] or [[filterQuery]] turns the header on. Set, constant or signal, it decides
      * alone, so `filterable(false)` keeps the header off whatever else is set. Off, the header is gone and every
      * option shows, but the query is left as it is: switched back on, the header shows it and filters by it again. A
      * signal never rebuilds the field: closed, only the trigger's role follows it; open, the panel stays open and
      * only its header comes and goes. Same word and same meaning on [[MultiSelect.filterable]] and
      * [[Listbox.filterable]].
      */
    def filterable(v: Boolean | Signal[Boolean]): Select[A] = new Select(p.copy(filterableV = Present(ReactiveValue(v))))

    /** How an option matches the query typed into the header filter, as `(option, query) => Boolean` with the query
      * as typed. [[Filter]] holds the ready-made ones: `filter(Filter.contains(_.name, _.email))` finds an option by
      * either text. Without it the match is a case-insensitive contains on the label projection. Setting it turns the
      * header filter on unless [[filterable]] says otherwise.
      */
    def filter(f: (A, String) => Boolean): Select[A] = new Select(p.copy(filterF = Present(f)))

    /** Renders the same header filter over a query the APP holds. A `SignalRef` binds two way: typing writes it, external
      * writes re-filter the panel, and [[onFilterQueryChange]] hears every change. A plain `Signal` binds one way: the panel
      * filters by it, typing is only reported, and the field is painted back to what the owner holds. A constant is the
      * starting text, the way a text field's value is. Turns the header filter on unless [[filterable]] says otherwise. A
      * signal nothing hears is reported in a card. Server-driven option lists bind this and re-`options` on emission; the
      * matching binding on [[Listbox.filterQuery]] takes the same shape.
      */
    def filterQuery(v: String | Signal[String]): Select[A] = new Select(p.copy(filterQuerySlot = filterQuerySlot.bind(v)))

    /** Fired with the query the reader types, and with the empty query a new open starts from. */
    def onFilterQueryChange(f: String => UI.Action)(using Frame): Select[A] =
        new Select(p.copy(filterQuerySlot = filterQuerySlot.hear(f)))

    /** Renders the clear affordance on the trigger while an option is chosen: it writes `Absent` into a `selection` over
      * `Maybe[A]` and fires `onChange(Absent)`. A `selection` over `A` cannot hold none, so it draws nothing there.
      */
    def showClear(v: Boolean): Select[A] = new Select(p.copy(showClearFlag = v))

    /** Whether the floating options panel (and its backdrop) re-home to `document.body`
      * ([[Overlay.portal]]), the escape from ancestor `overflow` clipping and `transform`
      * containing blocks, e.g. for a Select inside a ScrollPanel or DataTable cell (default false).
      */
    def portal(v: Boolean): Select[A] = new Select(p.copy(portalFlag = v))

    /** How the open options panel responds to scroll outside it (default
      * [[Overlay.Scroll.Close]], Prime's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Select[A] = new Select(p.copy(scrollV = v))

    /** Renders Prime's option check column: the check glyph on the selected row
      * (`.p-select-option-check-icon`), a blank placeholder on the rest.
      */
    def checkmark(v: Boolean): Select[A] = new Select(p.copy(checkmarkFlag = v))

    /** Says the options are still arriving: the dropdown icon turns into Prime's spinner, and a held option no option row
      * carries yet is shown from the value itself rather than reported. Without it, such an option is a mistake the field
      * names in a `.p-uic-diagnostic` card, since a typed value that matches no option can only mean the data and the
      * binding disagree. A `Signal[Boolean]` follows the load.
      */
    def loading(v: Boolean | Signal[Boolean]): Select[A] = new Select(p.copy(loadingV = Present(ReactiveValue(v))))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): Select[A] = new Select(p.copy(emptyContentV = v))

    def disabled(v: Boolean): Select[A] = new Select(p.copy(disabledFlag = v))

    /** `readonly`: the field reports `aria-readonly`, answers no key and opens no
      * panel (`.p-uic-select-readonly`), but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): Select[A] = new Select(p.copy(readonlyFlag = v))

    /** Marks the field required (`aria-required`). */
    def required(v: Boolean): Select[A] = new Select(p.copy(requiredFlag = v))

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * current option key (the div trigger has no native form value).
      */
    def name(v: String): Select[A] = new Select(p.copy(nameV = Present(v)))

    private[uic] def withTooltip(v: Maybe[Prop[String]]): Select[A] = new Select(p.copy(tooltipV = v))

    /** Size: `.p-select-sm` / default / `.p-select-lg`. */
    def size(v: Size): Select[A] = new Select(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): Select[A] = new Select(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-select-fluid`). */
    def fluid(v: Boolean): Select[A] = new Select(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self                 = new Select(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Maybe[A]])(using Frame): Select[A]  = selection(ref)
    private[uic] def hearBlur(f: Maybe[A] => UI.Action)(using Frame): Select[A] = onBlur(f)

    /** Fired with the option now chosen, after the write (`Absent` on clear). A pick of the option already chosen changes
      * nothing and fires nothing. Fires before [[onPick]].
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): Select[A] = new Select(p.copy(onChangeF = Present(Callback.of(f))))

    /** Fired with the option picked, after a pick that changed what the field holds: the same moments as [[onChange]]
      * except a clear, which picks nothing. A field whose value is never empty reads its picks here without unwrapping a
      * `Maybe`, and a one-way [[selection]] writes its own state here. Fires after [[onChange]] when both are set.
      */
    def onPick(f: A => UI.Action)(using Frame): Select[A] = new Select(p.copy(onPickF = Present(Callback.of(f))))

    /** Fires when the trigger loses focus, with the option held (`Absent` for none): the form-validation layer's Blur
      * trigger.
      */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): Select[A] = new Select(p.copy(onBlurF = Present(Callback.of(f))))

    /** Every pickable option in render order, groups flattened away. The value
      * binding, the key-collision check and the keyboard all work on this.
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

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

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
        /** The id the highlight is announced through. A caller's own `id` wins, and where there is
          * none a minted one stands in, so `aria-activedescendant` is not something a reader has to
          * opt into to be told which option is highlighted. Absent only for a test that names none.
          */
        idBase: Maybe[String]
    ):
        /** Asks for the panel open or closed: a two-way binding is written, then the change reported. */
        def setOpen(v: Boolean)(using Frame): UI.Action = open.change(v)
    end Wired

    private[uic] def render(using Frame): UI =
        // A constant nobody hears is fixed: drawn read-only, reachable and readable with no panel to open. A panel fixed
        // closed is the same: nothing the reader does may open it.
        if (choice.exists(_.fixed(onChangeF.orElse(onPickF))) || openSlot.fixedAt(false)) && !readonlyFlag then
            new Select(p.copy(readonlyFlag = true)).render
        else
            // A slot bound one way that nothing hears: every change the reader asks for there is painted away again.
            Diagnostics.before(renderLive)("Select")(
                Diagnostic.unheard(openSlot, Select.openName, on = interactive),
                Diagnostic.unheard(filterQuerySlot, Select.queryName, on = interactive)
            )
        end if
    end render

    private def renderLive(using Frame): UI =
        // The interaction state (open/highlight/filter) lives in signals allocated
        // by this effectful mount. Static projections (SSG, the SSR page GET,
        // initial render) show the placeholder (the same closed anatomy, inert)
        // until the transport attaches and the mount publishes the wired tree.
        val validity = FieldValidity(validityV)
        UI.mounted {
            for
                base <- access.id.map(v => Kyo.lift(v)).getOrElse(UI.id)
                // Unbound, the select keeps the panel state itself, starting closed.
                open <- openSlot.owned(false)
                hi   <- Signal.initRef(-1)
                // An app-owned query takes over the slot the component would otherwise allocate for itself; a constant query
                // is the starting text, kept here like an unbound one.
                q <- filterQuerySlot.ownedFrom("")
                // Unbound, and with no host supplying what it holds, the field keeps what the reader picks itself.
                own <-
                    if choice.isEmpty && currentV.isEmpty then Signal.initRef(Maybe.empty[A]).map(r => Present(r))
                    else Kyo.lift(Maybe.empty[SignalRef[Maybe[A]]])
                self = own.fold(this)(r => new Select(p.copy(choice = Present(SingleChoice.Optional(ReactiveValue(r))))))
            yield new Select(self.p.copy(openSlot = open)).wiredOver(hi, q, Present(base))
        }
    end renderLive

    /** The tree the mount draws: the seam a test renders directly to hand it refs of its own.
      */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String], base: Maybe[String] = Absent)(
        using Frame
    ): UI =
        new Select(p.copy(openSlot = openSlot.bind(open))).wiredOver(hi, filterQuerySlot.bind(q), base)
    end wired

    /** [[wired]] over the panel and query state however they are bound. */
    private def wiredOver(
        hi: SignalRef[Int],
        q: Controlled[String],
        base: Maybe[String]
    )(using Frame): UI =
        val validity = FieldValidity(validityV)
        val panel    = ReactiveValue.Dyn(openFiltering(openSlot.reading(false).signal)).and(ReactiveValue.Dyn(hi)).and(q.reading(""))
        panel.and(shown).and(OptionItem.reading(optionSources)).render { case (((((o, f), h), qq), (held, ld, ph)), opts) =>
            val self = new Select(p.copy(optionsV = opts))
            self.body(held, self.Wired(openSlot, o, hi, h, q, qq, f, base), ph, ld, validity)
        }
    end wiredOver

    /** What the field shows whatever its panel does: the held option, whether the options are still loading, and the
      * placeholder. Hosts supply `currentV` resolved, a bound ref is read live, an unbound select is empty. Read beside
      * the panel's state as ONE region (see [[ReactiveValue.and]]); the validity is stamped on the field, never read here.
      */
    private def shown(using Frame): ReactiveValue[(Maybe[A], Boolean, Maybe[String])] =
        val held: ReactiveValue[Maybe[A]] =
            currentV match
                case Present(cur) => ReactiveValue.Const(cur)
                case Absent       => choice.fold(ReactiveValue.Const(Maybe.empty[A]))(_.reading)
        held.and(ReactiveValue.optional(loadingV)).and(ReactiveValue.optional(placeholderV.map(ReactiveValue[String](_)))).map {
            case ((h, ld), ph) =>
                (h, ld.getOrElse(false), ph)
        }
    end shown

    private def body(
        held: Maybe[A],
        st: Wired,
        placeholder: Maybe[String],
        loading: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        val isOpen = st.isOpen
        // Whether the filter is on, as far as this render reads it: only an open panel subscribes a signal switch.
        val filtering = st.filtering
        val all       = items
        val current   = held.fold("")(key)
        val selIdx    = all.indexWhere(a => key(a) == current)
        // The trigger shows the option the held value matches, and the value itself where no option carries it yet,
        // so a field whose options are still arriving does not read empty.
        val shownOpt: Maybe[A] = if selIdx >= 0 then Present(all(selIdx)) else held
        val selText            = shownOpt.fold("")(labelF)
        // The chosen option drawn by the template, where there is one and an option is chosen.
        val selDrawn: Maybe[UI] = shownOpt.flatMap(a => itemTemplateF.map(_(a)))
        // A held option no row carries, once the options are said to be complete.
        val strays: Seq[String] =
            held match
                case Present(a) if selIdx < 0 && !loading => Seq(labelF(a))
                case _                                    => Nil

        /** Where an opening key lands: the selected option, or the first one a highlight may sit
          * on where nothing is selected. Opening has to land ON an option, or the arrow that
          * opened the panel has moved nothing and the reader presses it twice to reach the first
          * row. `-1` only where there is no such row at all.
          */
        val openHighlight: Int =
            if selIdx >= 0 then selIdx else all.indexWhere(a => !isOptionDisabled(a))

        // Writes the picked option, fires onChange where that changed it, closes the panel.
        def pick(a: A)(s: Wired): UI.Action =
            write(Present(a)).andThen(s.setOpen(false))

        // Opens with the highlight seeded on the selected option and a fresh query.
        def openPanel(s: Wired): UI.Action =
            for
                _ <- freshQuery(s.q)
                _ <- s.hi.set(openHighlight)
                _ <- s.setOpen(true)
            yield ()

        def toggle(s: Wired): UI.Action =
            if s.isOpen then s.setOpen(false) else openPanel(s)

        // === closed trigger ======================================================
        val showPlaceholder = held.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selText

        var lbl = span.cssClass("p-select-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if labelText.isEmpty then lbl = lbl.cssClass("p-select-label-empty")
        // NBSP keeps the empty label's line box (Prime renders &nbsp;).
        val labelUI: UI =
            selDrawn match
                case Present(drawn) if !showPlaceholder => lbl(drawn)
                case _                                  => lbl(if labelText.isEmpty then " " else labelText)

        val clearEffect: UI.Action = write(Absent)

        val clearUI: List[UI] =
            if showClearFlag && interactive && held.isDefined && choice.forall(_.clearable) then
                List(
                    button
                        .cssClass("p-select-clear-icon")
                        .jsProp("type", "button")
                        .aria("label" -> "Clear")
                        .onClick(clearEffect)
                        // The clear sits inside the field, whose click opens the panel.
                        .stopPropagation(true)(GlyphSvg(Icons.times, "p-icon"))
                )
            else Nil

        val dd = span.cssClass("p-select-dropdown")
        // PrimeVue's loading slot: the spinner takes the chevron's place in the dropdown cell.
        val dropdownUI: UI =
            if loading then dd(GlyphSvg(Icons.spinner, "p-select-loading-icon", "p-icon", "p-icon-spin"))
            else dd(GlyphSvg(Icons.chevronDown, "p-select-dropdown-icon", "p-icon"))

        // Hidden form carrier: a div trigger has no native form value.
        val hiddenCarrier: List[UI] =
            nameV.toList.map(n => hiddenInput.jsProp("name", n).value(current))

        // === floating panel ======================================================
        val panelUI: List[UI] = if st.isOpen then List(overlayPanel(current, st, pick)) else Nil

        // === field root ==========================================================
        var el = div.cssClass("p-select").cssClass("p-component")
        // The id goes on the root because the root is the control: it holds the tab stop and is
        // the combobox (or, with a filter header, the button that opens it), so this is what a
        // `Label.forId`, `Commands.focusId` and the form's focus-first-invalid have to reach. The
        // panel's parts derive from the same value (`$id-list`, `$id-option-<n>`).
        access.id.foreach(v => el = el.id(v))
        extraClassesV.foreach(c => el = el.cssClass(c))
        if inputwrapperFlag then
            el = el.cssClass("p-inputwrapper")
            // The float hook: a selected value fills the field; a non-empty placeholder
            // shows text too, so the label must float over it.
            if held.isDefined || placeholder.exists(_.nonEmpty) then el = el.cssClass("p-inputwrapper-filled")
        end if
        if isOpen then el = el.cssClass("p-select-open").cssClass("p-uic-overlay-anchor")
        el = validity.mark(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        if readonlyFlag then el = el.cssClass("p-uic-select-readonly").aria("readonly" -> "true")
        if requiredFlag then el = el.aria("required" -> "true")
        if loading then el = el.aria("busy" -> "true")
        sizeV match
            case Size.Small  => el = el.cssClass("p-select-sm")
            case Size.Large  => el = el.cssClass("p-select-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-select-fluid")
        el = el.title(tooltipV)
        access.name.foreach(v => el = el.aria("label" -> v))
        access.nameRef.foreach(v => el = el.aria("labelledby" -> v))
        // WHICH element is the combobox is decided by whether there is a filter header, and it is
        // decided once: the combobox is whatever holds focus while the list is open, because
        // `aria-activedescendant` is read off the focused element or off nothing at all. Without a
        // header focus never leaves this trigger, so the trigger is the combobox. With one, focus
        // goes into the header's text box and that box is the combobox, exactly as in
        // [[AutoComplete]]; the trigger is then the button that reveals it.
        // A signal switch is a `role` attribute CHANNEL rather than a read: a closed field subscribes nothing else
        // of it, so switching the filter patches the role in place and rebuilds nothing.
        el = filterOn match
            case BoolValue.Dyn(on) => el.withReactiveAttr("role", on.map(isOn => if isOn then "button" else "combobox"))
            case _                 => el.role(if filtering then "button" else "combobox")
        el = el
            .aria("haspopup" -> "listbox")
            .aria("expanded" -> isOpen)
        st.idBase.foreach { b =>
            el = el.aria("controls" -> listId(b))
            if !filtering then
                val (_, _, _, hiEff) = shownState(st)
                if st.isOpen && hiEff >= 0 then el = el.aria("activedescendant" -> optionId(b, hiEff))
            end if
        }
        // readonly keeps the field FOCUSABLE and says `aria-readonly` above; it simply answers no
        // key and opens nothing. `disabled` is what takes a control out of the tab order.
        if readonlyFlag && !disabledFlag then el = el.tabIndex(0)
        if interactive then
            el = el.tabIndex(0).preventScrollKeys
            // The one click listener: the field is the control, so a click anywhere on it toggles,
            // its padding and a click addressed by its id included. What inside does work of its
            // own ends the click there: the clear button, and the panel (`Overlay.containClicks`). A panel fixed open has
            // no close for a click to ask for, so the field takes none rather than one that changes nothing.
            if !openFixedOpen then el = el.onClick(toggle(st))
            // The trigger keeps focus for as long as the panel is open, so it is where the panel's
            // keyboard lives: closed, the three opening keys; open, the whole option keyboard.
            // The one exception is a filter header, which takes focus into the panel and the keys
            // with it, because a reader typing into a text box is not steering a list from outside it.
            val (_, shown, navigable, hiEff) = shownState(st)
            el = el.onKeyDown { e =>
                if !st.isOpen then
                    e.key match
                        case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel(st)
                        case _                                                    => ()
                else if st.filtering then ()
                else panelKey(shown, navigable, hiEff, st, pick)(e)
            }
            // Focus-loss on the trigger reports the currently bound key (or the resolved
            // current value when unbound): the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(choice match
                    case Present(c) => c.read(f)
                    case Absent     => f(held))
            }
        end if

        val field = validity.follow(el(UI.children((labelUI :: clearUI) ++ (dropdownUI :: hiddenCarrier) ++ panelUI)*))
        // A selection bound one way that nothing hears: every pick is painted away again. A disabled field takes no pick,
        // so it has nothing to say.
        Diagnostics.after(field)("Select")(
            Diagnostic.unknown(strays),
            Diagnostic(interactive && choice.exists(_.deaf(onChangeF.orElse(onPickF))), Select.selectionName.advice)
        )
    end body

    /** Writes `next` through the binding, then fires `onChange` and `onPick` where the write changed what the field holds
      * ([[SingleChoice.report]]). An unbound field (a host supplying `current`) has nothing to write and reports the pick as
      * it is.
      */
    private def write(next: Maybe[A])(using Frame): UI.Action =
        val changed: Maybe[Maybe[A]] < Sync =
            choice match
                case Present(c) => c.write(next, key)
                case Absent     => Present(next)
        changed.map(SingleChoice.report(_, onChangeF, onPickF))
    end write

    private def optionId(base: String, index: Int): String = s"$base-option-$index"

    /** The id of the option list, which is what the combobox points `aria-controls` at. */
    private def listId(base: String): String = s"$base-list"

    /** One key over the open panel.
      *
      * [[ListNav]] carries the movement, which is where Home and End come from and why a
      * disabled option is stepped over rather than landed on. Two keys are held back when the
      * panel has a FILTER HEADER, because then the panel holds a text field and the ARIA combobox
      * pattern gives it the caret's keys: Space types a space rather than picking, and a printable
      * key types rather than jumping. Without a header both belong to the list, and the printable
      * one jumps to the option it starts, which is what [[Typeahead]] answers.
      *
      * Escape closes the panel. It is read HERE rather than by [[Overlay]] because focus never
      * enters the panel, so an Escape pressed by the reader is delivered to whatever holds focus
      * (the trigger, or the filter header's input) and never reaches the panel at all.
      *
      * Tab closes it too, in both directions. The panel's keyboard lives on the element the Tab is
      * carrying the reader away from, so a panel left open behind them is one nothing answers: it
      * floats over the page with a highlight on it while the keys belong to whatever they landed
      * on next.
      */
    private def panelKey(
        shown: Seq[A],
        navigable: List[Int],
        hiEff: Int,
        s: Wired,
        pick: A => Wired => UI.Action
    )(e: KeyboardEvent)(using Frame): UI.Action =
        val caretOwns = s.filtering && (e.key == Keyboard.Space || e.key.charValue.isDefined)
        if e.key == Keyboard.Tab then s.setOpen(false)
        else if caretOwns then ()
        else
            ListNav.onKey(navigable, hiEff, e.key, wrap = false) match
                case Present(step) if step.dismiss => s.setOpen(false)
                case Present(step)                 =>
                    val picked: UI.Action =
                        if step.activate && shown.isDefinedAt(step.focus) then pick(shown(step.focus))(s) else ()
                    s.hi.set(step.focus).andThen(picked)
                case Absent =>
                    e.key match
                        case Keyboard.Char(c) =>
                            Typeahead.jump(shown.map(labelF), navigable, hiEff, c) match
                                case Present(to) => s.hi.set(to)
                                case Absent      => ()
                        case _ => ()
        end if
    end panelKey

    /** The options the panel is showing, the positions the highlight may land on, and where it
      * is now. Derived in one place because the TRIGGER reads it too: the keyboard lives on
      * whatever holds focus, and that is no longer the panel.
      *
      * Prime's select stops at the ends rather than cycling, so `wrap` is false wherever the
      * navigable list is read.
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

    /** The floating option panel: Prime's `.p-select-overlay` skin on the
      * [[Overlay]] primitive, holding the optional filter header and the option
      * list.
      */
    private def overlayPanel(current: String, s: Wired, pick: A => Wired => UI.Action)(using Frame): UI =
        val (shownGroups, shown, navigable, hiEff) = shownState(s)

        val rows: List[UI] = OptionItem.rows(shownGroups, "p-select-option-group") { (a, i) =>
            val isSel               = key(a) == current
            val isDis               = isOptionDisabled(a)
            val checkSlot: List[UI] =
                if checkmarkFlag then
                    if isSel then List(GlyphSvg(Icons.check, "p-select-option-check-icon", "p-icon"))
                    else List(span.cssClass("p-select-option-blank-icon").cssClass("p-icon").aria("hidden" -> "true"))
                else Nil
            var row = li.cssClass("p-select-option").role("option").aria("selected" -> isSel)
            if isSel then row = row.cssClass("p-select-option-selected")
            if isDis then row = row.cssClass("p-disabled").aria("disabled" -> "true")
            if i == hiEff then row = row.cssClass("p-focus").scrollAuto(true)
            // The id the list points `aria-activedescendant` at. Stamped on every option rather
            // than on the highlighted one alone, so the attribute names a target that is already
            // in the document whichever row the highlight is on.
            s.idBase.foreach(b => row = row.id(optionId(b, i)))
            if !isDis then row = row.onClick(pick(a)(s))
            val content: UI = itemTemplateF match
                case Present(t) =>
                    // The template may hold no text at all, so the row names itself.
                    row = row.aria("label" -> labelF(a))
                    t(a)
                case Absent => span.cssClass("p-select-option-label")(labelF(a))
            row(UI.children(checkSlot :+ content)*)
        }
        val emptyRow: List[UI] =
            if shown.isEmpty then
                List(EmptyContent.render(emptyContentV, "No results found")(c =>
                    li.cssClass("p-select-empty-message").role("presentation")(c)
                ))
            else Nil

        val listUI: UI =
            div.cssClass("p-select-list-container")(
                {
                    // The list is named by `aria-controls` and holds the option ids; it does NOT
                    // carry the announcement, because it never holds focus and the attribute is
                    // read off the element that does.
                    var list = ul.cssClass("p-select-list").role("listbox")
                    s.idBase.foreach(b => list = list.id(listId(b)))
                    list(UI.children(rows ++ emptyRow)*)
                }
            )

        // Prime wraps the filter in an IconField with a trailing search icon; the
        // sibling :not(:last-child) sheet rule pads the input for the icon.
        val header: List[UI] =
            if s.filtering then
                var filterEl = input
                    .cssClass("p-select-filter")
                    .cssClass("p-inputtext")
                    .cssClass("p-component")
                    // The bound input renders inside a reactive wrapper span, which resets sibling
                    // positions; the sheet's :not(:last-child) padding cannot see the trailing
                    // icon, so stamp the deterministic IconField padding class.
                    .cssClass("p-uic-iconfield-end")
                    .role("combobox")
                    .aria("label" -> "Filter")
                    .aria("haspopup" -> "listbox")
                    .aria("expanded" -> "true")
                // Two way, the field writes the query itself; one way it follows what the owner holds and reports what the reader
                // types, and kyo-ui puts it back to the owner's query.
                s.q.bound.foreach(v => filterEl = filterEl.value(v))
                filterEl = filterEl
                    // Typing re-filters; highlight the first match (Prime behavior).
                    .onInput(text =>
                        s.hi.set(0)
                            .andThen(s.q.change(text))
                    )
                    // The header is where focus goes and therefore where the option keyboard lives:
                    // the arrows move the highlight, the caret keeps Space and the printable keys,
                    // Escape closes.
                    .focusAuto(true)
                    .focusRestore(true)
                    .preventScrollKeys
                    .onKeyDown(panelKey(shown, navigable, hiEff, s, pick))
                s.idBase.foreach { b =>
                    filterEl = filterEl.aria("controls" -> listId(b))
                    if hiEff >= 0 then filterEl = filterEl.aria("activedescendant" -> optionId(b, hiEff))
                }
                List(
                    div.cssClass("p-select-header")(
                        div.cssClass("p-iconfield")(
                            filterEl,
                            span.cssClass("p-inputicon")(
                                GlyphSvg(Icons.search, "p-icon")
                            )
                        )
                    )
                )
            else Nil

        Overlay.hosted(s.open)
            .portal(portalFlag)
            .scroll(scrollV)
            .panelClass("p-select-overlay")
            .panelClass("p-component")
            // Nothing in this panel takes focus (a filter header seeds its own input), so nothing
            // in it receives a key either: no seed, no key handler, and no Escape of its own.
            .seedFocus(false)
            .dismissOnEscape(false)
            .containClicks(true)((header ++ keyCollisionCard ++ List(listUI))*)
            .renderShown(s.idBase.map(b => s"$b-panel"))
    end overlayPanel

    /** The loud card shown at the top of the panel when the option keys collide, so
      * a duplicate-label data set cannot silently select the wrong row (see
      * [[Diagnostics]]). Empty in the healthy case, which is every existing
      * render.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        Diagnostics.cards("Select")(Diagnostic.naming(
            if keyF.isEmpty then
                "option labels are not unique and optionKey is unset, so picks apply to the wrong option; set optionKey"
            else "optionKey is not unique across the options, so picks apply to the wrong option",
            Diagnostics.duplicates(items.map(key))
        )).toList
    end keyCollisionCard
end Select

object Select:
    /** How a field that always holds an option binds: through `selection` over `A` ([[HasSetter]]). */
    given selection[A]: HasSetter[Select[A], "selection", UI.Prop[A]] with
        def apply(c: Select[A], v: UI.Prop[A])(using Frame): Select[A] = c.selection(v)

    final private[uic] case class Props[A](
        // The rows the body draws, resolved from `optionSources` by the region that draws them.
        optionsV: List[OptionItem[A]],
        labelF: A => String,
        keyF: Maybe[A => String] = Absent,
        optionDisabledF: Maybe[A => Boolean] = Absent,
        choice: Maybe[SingleChoice[A]] = Absent,
        itemTemplateF: Maybe[A => UI] = Absent,
        openSlot: Controlled[Boolean] = Controlled(),
        placeholderV: Maybe[Prop[String]] = Absent,
        filterableV: Maybe[BoolValue] = Absent,
        filterF: Maybe[(A, String) => Boolean] = Absent,
        filterQuerySlot: Controlled[String] = Controlled(),
        showClearFlag: Boolean = false,
        checkmarkFlag: Boolean = false,
        emptyContentV: Maybe[EmptyContent] = Absent,
        disabledFlag: Boolean = false,
        readonlyFlag: Boolean = false,
        requiredFlag: Boolean = false,
        nameV: Maybe[String] = Absent,
        tooltipV: Maybe[Prop[String]] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onChangeF: Maybe[Maybe[A] => UI.Action] = Absent,
        onPickF: Maybe[A => UI.Action] = Absent,
        onBlurF: Maybe[Maybe[A] => UI.Action] = Absent,
        currentV: Maybe[Maybe[A]] = Absent,
        loadingV: Maybe[BoolValue] = Absent,
        extraClassesV: List[String] = Nil,
        inputwrapperFlag: Boolean = false,
        portalFlag: Boolean = false,
        scrollV: Overlay.Scroll = Overlay.Scroll.Close,
        optionSources: List[OptionSource[A]] = Nil
    )

    private val openName      = Controlled.Name("Select", "the panel", "an open or a close", "onOpenChange")
    private val queryName     = Controlled.Name("Select", "the filter query", "the reader type", "onFilterQueryChange")
    private val selectionName = Controlled.Name("Select", "the selection", "a pick", "onChange or onPick")

    def apply[A](): Select[A] = new Select[A](Props(Nil, _.toString))
end Select
