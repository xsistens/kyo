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
  * `.p-uic-diagnostic` card. `open(ref)` optionally exposes the panel visibility.
  *
  * One instance per place: the choice the reader makes where nothing binds it, the open panel, the filter query and the
  * highlight are the instance's own, so a host region that runs again hands it the run's props instead of starting it over
  * (see [[Select.drawn]]). Form participation: `name(...)` emits a hidden `<input>` carrying the current key (a div trigger has
  * no native form value).
  *
  * Honest deferrals: editable mode and virtual scrolling are not implemented yet. Arrow keys on the open panel no longer scroll the
  * page underneath: the panel declares `preventScrollKeys`.
  *
  * `id` lands on the field root, the combobox that holds the tab stop, and the panel derives `$id-list` and
  * `$id-option-<n>` from it (a minted base stands in where none is given).
  */
final class Select[A] private[uic] (private[uic] val p: Select.Props[A]) extends Component(Setters[Select.Props[A], Select[A]]),
      Node, FormControl[Maybe[A]], HasEmptyContent, HasTooltip, HasPlaceholder, HasAccessibleNameRef:

    type Self = Select[A]

    // Lets the instance compare its props and a region skip a value it already showed; options are still matched by key.
    private given CanEqual[A, A] = CanEqual.derived

    /** Package-internal class hook: hosts (Paginator) stamp Prime's contextual
      * class (`p-paginator-rpp-dropdown`) onto the field root.
      */
    private[uic] def extraClass(cls: String): Select[A] = new Select(p.copy(extraClasses = p.extraClasses :+ cls))

    /** Package-internal: FloatLabel stamps Prime's wrapper-state classes
      * (`p-inputwrapper` + `p-inputwrapper-filled` while a value is selected or a
      * non-empty placeholder shows text), stamped INSIDE the component's own
      * region so the filled state tracks the held option.
      */
    private[uic] def inputwrapper: Select[A] = new Select(p.copy(inputwrapper = true))

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new Select(p.copy(access = v))

    /** Package-internal: hosts that already render inside their own region supply the held option directly (writes go through
      * `onChange`), and the field keeps no choice of its own.
      */
    private[uic] def current(v: Maybe[A]): Select[A] = new Select(p.copy(current = Present(v), selection = p.selection.unowned))

    /** Appends typed options with their text projection: a collection, or a signal of options for a list that changes
      * while the field is shown (read in the field's own region, so the field is not built again around it).
      */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): Select[A] =
        new Select(p.copy(sources = p.sources :+ OptionSource.of(is), label = label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): Select[A] =
        new Select(p.copy(
            sources = p.sources :+ OptionSource.of(ev.substituteCo[[X] =>> Iterable[X] | Signal[Seq[X]]](is)),
            label = a => ev.flip(a)
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
        new Select(p.copy(sources = p.sources :+ OptionSource.rows(is), label = label))

    /** Appends `none` and then `is` as the options of a select over `Maybe[B]`: `Absent` first, labelled `none`, then
      * `Present(b)` for each value, labelled `label(b)`.
      *
      * The none option is a row like any other, so a filter's "all" is one the reader picks; a field that may be left empty
      * binds `selection` over a `Maybe` with a placeholder instead. Only a select whose options are `Maybe[B]` takes it.
      */
    def optionsWithNone[B](none: String, is: Iterable[B])(label: B => String)(using ev: Maybe[B] =:= A): Select[A] =
        val all: List[Maybe[B]] = (Absent: Maybe[B]) :: is.toList.map(b => Present(b): Maybe[B])
        new Select(p.copy(
            sources = p.sources :+ OptionSource.Fixed(all.map(m => OptionItem.Item(ev(m)))),
            label = a => ev.flip(a).fold(none)(label)
        ))
    end optionsWithNone

    /** Binds the selection to the option itself: the field shows the row whose `optionKey` the value carries, and a pick
      * writes the option. The value always holds an option, so there is no clear (`showClear` draws nothing).
      * A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through `onChange` and
      * [[Select.Props.onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a
      * ref out. With neither event set, a constant is fixed (drawn read-only) and a signal draws a card saying the reader
      * cannot change it.
      */
    def selection(v: Prop[A])(using Frame): Select[A] = new Select(p.copy(selection = p.selection.value(v)))

    /** Binds the selection to the option or none: `Absent` shows the placeholder, a pick writes `Present(option)`, the clear
      * writes `Absent`. Two-way and one-way as for the option binding above. `@targetName` because both `selection` bindings
      * erase to one signature.
      */
    @targetName("selectionMaybe")
    def selection(v: Prop[Maybe[A]]): Select[A] = new Select(p.copy(selection = p.selection.bind(v)))

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): Select[A] = new Select(p.copy(placeholder = v))

    /** How an option matches the query typed into the header filter, as `(option, query) => Boolean` with the query as typed.
      * [[Filter]] holds the ready-made ones: `filter(Filter.contains(_.name, _.email))` finds an option by either text. Unset,
      * the match is a case-insensitive contains on the label projection. Setting it turns the header filter on unless
      * `filterable` says otherwise. Written here, not generated: on Scala.js every value has a `filter` (`UndefOrOps`), which a
      * call would reach instead.
      */
    def filter(f: (A, String) => Boolean): Select[A] = new Select(p.copy(filter = Opt.set(f)))

    /** Renders the same header filter over a query the APP holds. A `SignalRef` binds two way: typing writes it, external
      * writes re-filter the panel, and `onFilterQueryChange` hears every change. A plain `Signal` binds one way: the panel
      * filters by it, typing is only reported, and the field is painted back to what the owner holds. A constant holds
      * the query at that text, the way a text field's value does (see [[kyo.UI.Bound]]); unset, the query starts empty and the
      * instance keeps what the reader types. Turns the header filter on unless
      * [[Select.Props.filterable]] says otherwise. A signal nothing hears is reported in a card. Server-driven option lists
      * bind this and re-`options` on emission; the matching binding on [[Listbox.filterQuery]] takes the same shape.
      */
    def filterQuery(v: Prop[String]): Select[A] = new Select(p.copy(filterQuery = p.filterQuery.bind(v), queried = true))

    /** The query the header filter starts with; the reader owns it from there, in the instance, as a text field's `defaultValue`.
      * Turns the header filter on unless [[Select.Props.filterable]] says otherwise, as [[filterQuery]] does.
      */
    def defaultFilterQuery(v: String): Select[A] = new Select(p.copy(filterQuery = p.filterQuery.startAt(v), queried = true))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): Select[A]        = new Select(p.copy(emptyContent = v))
    private[uic] def withTooltip(v: Maybe[Prop[String]]): Select[A]             = new Select(p.copy(tooltip = v))
    private[uic] def validitySlots: FieldValidity.Slots                         = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self                 = new Select(p.copy(validity = v))
    private[uic] def bindRef(ref: SignalRef[Maybe[A]])(using Frame): Select[A]  = selection(ref)
    private[uic] def hearBlur(f: Maybe[A] => UI.Action)(using Frame): Select[A] = this.onBlur(f)

    /** One instance per place, drawn once: it holds the choice, the open panel, the query and the highlight the reader gives an
      * unbound select, and a host region that runs again hands it the run's props instead of starting it over (see
      * [[Select.drawn]]).
      */
    private[uic] def render(using Frame): UI = UI.mountedWith(p)(Select.drawn)

    /** The tree the instance draws, with the panel and the query bound to `open` and `q` and the highlight held in `hi`: the
      * seam a test renders directly to hand it refs of its own. `base` is the id the panel's parts derive from.
      */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String], base: Maybe[String] = Absent)(
        using Frame
    ): UI =
        Select.drawnOver(new UI.Live(p.copy(open = p.open.bind(open), filterQuery = p.filterQuery.bind(q)), Absent), hi, _ => base)
end Select

object Select:
    /** How a field that always holds an option binds: through `selection` over `A` ([[HasSetter]]). */
    given selection[A]: HasSetter[Select[A], "selection", UI.Prop[A]] with
        def apply(c: Select[A], v: UI.Prop[A])(using Frame): Select[A] = c.selection(v)

    /** What a select holds. Each field is a setter of its name (`uic.Select[String]().size(Size.Small)`), which takes a constant
      * or a signal where the field is a `Prop`, the value itself where it is an `Opt` and the function itself for a callback or an
      * optional function (`optionKey`, `itemTemplate`);
      * `open` binds and `onOpenChange` hears it, `onChange` hears `selection` and `onFilterQueryChange` the query. The options,
      * `selection`, `filter` and `filterQuery` are written by hand, and the fields marked `@noSetter`
      * by them, by hosts and by the shared traits.
      */
    final private[uic] case class Props[A](
        /** Where the options come from, in the order the setters received them, read as one list in the field's region. */
        @noSetter sources: List[OptionSource[A]] = Nil,
        /** The text projection an option is shown and matched by. */
        @noSetter label: A => String = (a: A) => a.toString,
        /** Stable per-option key, what options are matched by. Defaults to the label projection, which is correct only while the
          * labels are unique. When they are not, two options share one key and a pick applies to the wrong one, a failure that
          * appears only with the data that triggers it, so it survives development and shows up in production. The panel therefore
          * renders a loud `.p-uic-diagnostic` card whenever the derived keys collide, naming the offending keys.
          */
        optionKey: Opt[A => String] = Opt.unset,
        /** Renders matching options disabled (`.p-disabled` rows, shown in the panel but not pickable; the keyboard highlight
          * skips them).
          */
        optionDisabled: A => Boolean = (_: A) => false,
        /** Custom option content: arbitrary UI drawn in each option row and, for the chosen option, in the trigger, in place of
          * the text projection. The label projection stays each row's accessible name and the text the filter and the printable
          * keys match, and through `optionKey` the key.
          */
        itemTemplate: Opt[A => UI] = Opt.unset,
        /** The option chosen. Its event `onChange` fires with the option now chosen, after the write (`Absent` on clear); a pick
          * of the option already chosen changes nothing and fires nothing, and it fires before `onPick`.
          */
        @eventName("onChange") selection: ChoiceSlot[A] = ChoiceSlot[A](),
        /** The panel visibility. A `SignalRef` binds two way: the trigger, a pick, Escape, Tab and an outside click write it,
          * then `onOpenChange` hears the new state. A plain `Signal` binds one way: the panel follows it and the select only
          * reports. A constant nobody hears is fixed: `false` draws the select read-only; `true` keeps the panel open with
          * nothing that closes it (no backdrop, no click on the field), while the field stays the tab stop with the option
          * keyboard and a pick is still the reader's. The combobox has no readonly state for its list, so that says nothing
          * beyond `aria-expanded`. Unbound, the instance keeps the state itself, starting closed. A signal nothing hears is
          * reported in a card.
          */
        open: Controlled[Boolean] = Controlled(start = Present(false)),
        @noSetter placeholder: Maybe[Prop[String]] = Absent,
        /** Switches Prime's header filter (`div.p-select-header` > `input.p-select-filter`), over a query the instance keeps
          * unless `filterQuery` hands it one; while it is on, the query starts fresh on every open.
          *
          * Unset, setting `filter` or `filterQuery` turns the header on. Set, constant or signal, it decides alone, so
          * `filterable(false)` keeps the header off whatever else is set. Off, the header is gone and every option shows, but
          * the query is left as it is: switched back on, the header shows it and filters by it again. A signal never draws the
          * field again: closed, only the trigger's role follows it; open, the panel stays open and only its header comes and
          * goes. Same word and same meaning on [[MultiSelect.filterable]] and [[Listbox.filterable]].
          */
        filterable: Opt[Prop[Boolean]] = Opt.unset,
        /** How an option matches the query, written by [[Select.filter]]. */
        @noSetter filter: Opt[(A, String) => Boolean] = Opt.unset,
        /** The query typed into the header filter. Its event `onFilterQueryChange` fires with the query the reader types, and
          * with the empty query a new open starts from.
          */
        filterQuery: Controlled[String] = Controlled(start = Present("")),
        /** Whether `filterQuery` was handed a query, which turns the header filter on where `filterable` is unset. */
        @noSetter queried: Boolean = false,
        /** Renders the clear affordance on the trigger while an option is chosen: it writes `Absent` into a `selection` over
          * `Maybe[A]` and fires `onChange(Absent)`. A `selection` over `A` cannot hold none, so it draws nothing there.
          */
        showClear: Prop[Boolean] = false,
        /** Renders Prime's option check column: the check glyph on the selected row (`.p-select-option-check-icon`), a blank
          * placeholder on the rest.
          */
        checkmark: Prop[Boolean] = false,
        @noSetter emptyContent: Maybe[EmptyContent] = Absent,
        disabled: Prop[Boolean] = false,
        /** `readonly`: the field reports `aria-readonly`, answers no key and opens no panel (`.p-uic-select-readonly`), but
          * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can land on it and read its
          * value, and every key and click that would change it runs into nothing.
          */
        readonly: Prop[Boolean] = false,
        /** Marks the field required (`aria-required`). */
        required: Prop[Boolean] = false,
        /** HTML form participation: emits a hidden `<input name=...>` carrying the current option key (the div trigger has no
          * native form value).
          */
        name: Opt[Prop[String]] = Opt.unset,
        @noSetter tooltip: Maybe[Prop[String]] = Absent,
        /** Size: `.p-select-sm` / default / `.p-select-lg`. */
        size: Prop[Size] = Size.Normal,
        /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
        variant: Prop[FieldVariant] = FieldVariant.Outlined,
        /** Spans the full width of its container (`.p-select-fluid`). */
        fluid: Prop[Boolean] = false,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        /** Fired with the option picked, after a pick that changed what the field holds: the same moments as `onChange` except
          * a clear, which picks nothing. A field whose value is never empty reads its picks here without unwrapping a `Maybe`,
          * and a one-way `selection` writes its own state here. Fires after `onChange` when both are set.
          */
        onPick: Maybe[A => UI.Action] = Absent,
        /** Fires when the trigger loses focus, with the option held (`Absent` for none): the form-validation layer's Blur
          * trigger.
          */
        onBlur: Maybe[Maybe[A] => UI.Action] = Absent,
        @noSetter current: Maybe[Maybe[A]] = Absent,
        /** Says the options are still arriving: the dropdown icon turns into Prime's spinner, and a held option no option row
          * carries yet is shown from the value itself rather than reported. Without it, such an option is a mistake the field
          * names in a `.p-uic-diagnostic` card, since a typed value that matches no option can only mean the data and the
          * binding disagree.
          */
        loading: Prop[Boolean] = false,
        @noSetter extraClasses: List[String] = Nil,
        @noSetter inputwrapper: Boolean = false,
        /** Whether the floating options panel (and its backdrop) re-home to `document.body` ([[Overlay.portal]]), the escape
          * from ancestor `overflow` clipping and `transform` containing blocks, e.g. for a Select inside a ScrollPanel or
          * DataTable cell (default false).
          */
        portal: Boolean = false,
        /** How the open options panel responds to scroll outside it (default [[Overlay.Scroll.Close]], Prime's hide-on-scroll).
          * See [[Overlay.Scroll]].
          */
        scroll: Overlay.Scroll = Overlay.Scroll.Close
    ) derives CanEqual

    def apply[A](): Select[A] = new Select[A](Props())

    private val openName      = Controlled.Name("Select", "the panel", "an open or a close", "onOpenChange")
    private val queryName     = Controlled.Name("Select", "the filter query", "the reader type", "onFilterQueryChange")
    private val selectionName = Controlled.Name("Select", "the selection", "a pick", "onChange or onPick")

    /** What of the props builds the field and has no channel of its own, read as one value: no hearer is in it, only whether
      * somebody hears.
      *
      * @param fixed
      *   the selection is a constant nobody hears, or the panel is fixed closed: drawn read-only
      * @param fixedOpen
      *   the panel is held open by a constant nobody hears: nothing the reader does may close it
      */
    final private[uic] case class Base[A](
        label: A => String,
        key: A => String,
        optionDisabled: A => Boolean,
        itemTemplate: Maybe[A => UI],
        matcher: (A, String) => Boolean,
        fixed: Boolean,
        fixedOpen: Boolean,
        deaf: Boolean,
        openDeaf: Boolean,
        queryDeaf: Boolean,
        clearable: Boolean,
        blurs: Boolean,
        current: Maybe[Maybe[A]],
        emptyContent: Maybe[EmptyContent],
        tooltip: Maybe[Prop[String]],
        validity: FieldValidity.Slots,
        access: Accessibility,
        extraClasses: List[String],
        inputwrapper: Boolean,
        portal: Boolean,
        scroll: Overlay.Scroll
    ) derives CanEqual

    private object Base:
        /** The base of `q`, as the engine hands it: an unbound selection, panel and query are bound to the instance's own refs. */
        def apply[A](q: Props[A])(using CanEqual[A, A]): Base[A] =
            val picks = q.onPick.isDefined
            Base(
                q.label,
                q.optionKey.getOrElse(q.label),
                q.optionDisabled,
                q.itemTemplate,
                q.filter.getOrElse(Filter.contains(q.label)),
                q.selection.fixed(picks) || q.open.fixedAt(false),
                q.open.fixedAt(true),
                q.selection.deaf(picks),
                q.open.deaf,
                q.filterQuery.deaf,
                q.selection.clearable,
                q.onBlur.isDefined,
                q.current,
                q.emptyContent,
                q.tooltip,
                q.validity,
                q.access,
                q.extraClasses,
                q.inputwrapper,
                q.portal,
                q.scroll
            )
        end apply
    end Base

    /** The field, drawn once over `props`, which hand it the selection, the panel and the query bound: to the instance's own
      * refs where a run binds nothing (carrying on from the binding when a run drops it). The highlight and the id the panel's
      * parts derive from are made here, once per instance.
      */
    private def drawn[A](props: UI.Live[Props[A]])(using CanEqual[A, A], Frame): UI < Sync =
        for
            hi     <- Signal.initRef(-1)
            minted <- UI.id
        yield drawnOver(props, hi, b => b.access.id.orElse(Present(minted)))

    /** What the field holds as a prop: what a host supplies, else what the selection binds (the instance's own where nothing is
      * bound).
      */
    private def heldOf[A](q: Props[A]): Prop[Maybe[A]] =
        q.current match
            case Present(cur) => cur
            case Absent       => q.selection.held

    /** The panel state at the moment an event comes, read from the run it reaches: what a key, an opening and a pick work on. */
    final private case class Now[A](
        b: Base[A],
        isOpen: Boolean,
        filtering: Boolean,
        opts: List[OptionItem[A]],
        held: Maybe[A],
        hiV: Int,
        qV: String
    ):
        def all: List[A] = OptionItem.flatten(opts)

        /** Where an opening key lands: the selected option, or the first one a highlight may sit on where nothing is selected.
          * Opening has to land ON an option, or the arrow that opened the panel has moved nothing and the reader presses it twice
          * to reach the first row. `-1` only where there is no such row at all.
          */
        def openHighlight: Int =
            val current = held.fold("")(b.key)
            val selIdx  = all.indexWhere(a => b.key(a) == current)
            if selIdx >= 0 then selIdx else all.indexWhere(a => !b.optionDisabled(a))
        end openHighlight
    end Now

    /** What a pick or a key changes, as the run the event reaches holds it. */
    final private case class Acts[A](props: UI.Live[Props[A]], hi: SignalRef[Int])(using CanEqual[A, A]):

        /** Asks for the panel open or closed: a two-way binding is written, then the change reported. */
        def setOpen(v: Boolean)(using Frame): UI.Action = props.call(_.open).map(_.change(v))

        /** A query the reader types: a two-way binding is written, then the change reported. */
        def setQuery(v: String)(using Frame): UI.Action = props.call(_.filterQuery).map(_.change(v))

        /** The panel state now. */
        def now(using Frame): Now[A] < Sync =
            props.call(identity).map { q =>
                for
                    o    <- q.open.current(false)
                    on   <- q.filterable.read.map(_.getOrElse(q.filter.isDefined || q.queried))
                    opts <- Prop.current(OptionItem.reading(q.sources).bound)
                    held <- Prop.current(heldOf(q))
                    h    <- hi.get
                    qq   <- q.filterQuery.current("")
                yield Now(Base(q), o, on, opts, held, h, qq)
            }

        /** Writes `next` through the run's binding, then fires `onChange` and `onPick` where the write changed what the field
          * holds ([[SingleChoice.report]]). A field whose host supplies what it holds has nothing to write and reports the pick
          * as it is.
          */
        def write(next: Maybe[A])(using Frame): UI.Action =
            props.call(identity).map { q =>
                val changed: Maybe[Maybe[A]] < Sync =
                    q.selection.choice match
                        case Present(c) => c.write(next, q.optionKey.getOrElse(q.label))
                        case Absent     => Present(next)
                changed.map(SingleChoice.report(_, q.selection.hearer, q.onPick))
            }

        /** Writes the picked option, fires onChange where that changed it, closes the panel. */
        def pick(a: A)(using Frame): UI.Action = write(Present(a)).andThen(setOpen(false))

        /** Opens with the highlight seeded on the selected option and a fresh query. While the filter is off the query is not
          * the panel's to touch, so switching the filter back on finds it as it was.
          */
        def openPanel(using Frame): UI.Action =
            for
                n <- now
                _ <- props.call(_.filterQuery).map(q => if n.filtering then q.change("") else (): UI.Action)
                _ <- hi.set(n.openHighlight)
                _ <- setOpen(true)
            yield ()

        /** A click on the field: closes an open panel, opens a closed one. */
        def toggle(using Frame): UI.Action =
            props.call(_.open).map(_.current(false)).map(o => if o then setOpen(false) else openPanel)

        /** A key on the trigger. Closed, the three opening keys open the panel; open, the whole option keyboard, unless a filter
          * header holds focus, whose own handler takes the keys (they reach the trigger too, since the header is inside it).
          */
        def triggerKey(e: KeyboardEvent)(using Frame): UI.Action =
            now.map { n =>
                if !n.isOpen then
                    e.key match
                        case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel
                        case _                                                    => (): UI.Action
                else if n.filtering then (): UI.Action
                else panelKey(n, this)(e)
            }

        /** A key on the filter header, which holds focus while it is shown. */
        def headerKey(e: KeyboardEvent)(using Frame): UI.Action = now.map(panelKey(_, this)(e))

        /** Focus leaving the trigger, told the option the run holds at that moment: the validation layer's Blur trigger. */
        def blur(using Frame): UI.Action =
            props.call(identity).map { q =>
                q.onBlur match
                    case Absent     => Kyo.unit: UI.Action
                    case Present(f) => Prop.current(heldOf(q)).map(f): UI.Action
            }
    end Acts

    /** The field over `props`, with the highlight in `hi` and the panel's id base `idOf` its base gives.
      *
      * Each part reads what it shows. The field's root is one block over what decides its structure, and that changes rarely:
      * whether the panel is open, `disabled`, `readonly`, `loading`, and values derived from the held option that stay equal
      * across most picks (whether one is held, the options no row carries). Its classes and attributes that follow a prop are
      * channels: the size, the variant, the width, `required`, the filled hook, `aria-activedescendant` and the hidden input's
      * key. The label is a block of its own over the shown option and the placeholder, and the open panel one over the
      * highlight, the query, the options and the held key. So a pick redraws the label, an arrow key the panel's rows, and
      * neither the field around them; an open panel stays the same element while its rows redraw. A closed field reads
      * nothing of the panel, and a signal filter switch only in its role. A pick, a key and a blur read the run when they come.
      */
    private[uic] def drawnOver[A](props: UI.Live[Props[A]], hi: SignalRef[Int], idOf: Base[A] => Maybe[String])(using
        CanEqual[A, A],
        Frame
    ): UI =
        given CanEqual[List[OptionItem[A]], List[OptionItem[A]]] = CanEqual.derived
        given CanEqual[Seq[String], Seq[String]]                 = CanEqual.derived
        val base                                                 = props(Base(_))
        val open                                                 = props(_.open.value(false))
        // Whether the filter is on: an explicit `filterable` decides alone, as an explicit `invalid` does in [[FieldValidity]];
        // unset, a `filter` or a `filterQuery` turns it on.
        val switched: Prop[Boolean] =
            Prop.map(Prop.zip(props(_.filterable), props(q => q.filter.isDefined || q.queried)))((on, set) => on.getOrElse(set))
        // Whether the panel is open, paired with whether the filter is on. A signal switch is followed only while the panel is
        // open, and a block cannot say that: it would follow the switch while the panel is closed too, and a closed field reads a
        // signal switch nowhere but in its role (`roleOf`).
        val panel: Prop[(Boolean, Boolean)] =
            switched match
                case on: Boolean => Prop.map(open)(o => (o, on))
                case _           =>
                    Prop.signal(open).switchMap { o =>
                        if !o then Signal.initConst((false, false)) else Prop.signal(switched).map(on => (true, on))
                    }
        val query       = props(_.filterQuery.value(""))
        val held        = props(heldOf(_))
        val options     = props(q => OptionItem.reading(q.sources).bound)
        val placeholder = props(_.placeholder)
        val loading     = props(_.loading)
        val acts        = Acts(props, hi)
        // Values derived from the held option, each compared by its value, so a pick that leaves it equal reaches nobody.
        val shownOpt: Prop[Maybe[A]] = UI.reactive {
            val b   = base.read
            val h   = held.read
            val all = OptionItem.flatten(options.read)
            h.map(a => all.find(o => b.key(o) == b.key(a)).getOrElse(a))
        }
        val hasValue: Prop[Boolean] = UI.reactive(held.read.isDefined)
        val filled: Prop[Boolean]   = UI.reactive(held.read.isDefined || placeholder.read.exists(_.nonEmpty))
        val heldKey: Prop[String]   = UI.reactive {
            val b = base.read
            held.read.fold("")(b.key)
        }
        // A held option no row carries, once the options are said to be complete.
        val strays: Prop[Seq[String]] = UI.reactive {
            val b   = base.read
            val h   = held.read
            val all = OptionItem.flatten(options.read)
            val ld  = loading.read
            h match
                case Present(a) if !ld && !all.exists(o => b.key(o) == b.key(a)) => Seq(b.label(a))
                case _                                                           => Nil
        }
        // The option the trigger announces while it is the combobox: the highlighted row, while the panel is open and no filter
        // header has taken focus. Closed, it reads nothing of the panel.
        val active: Prop[Maybe[String]] = UI.reactive {
            val b      = base.read
            val (o, f) = panel.read
            if o && !f then
                val h  = hi.read
                val qq = query.read
                val st = shownState(b, options.read, filtering = false, h, qq)
                idOf(b).flatMap(id => if st.hiEff >= 0 then Present(optionId(id, st.hiEff)) else Absent)
            else Absent
            end if
        }
        val parts = Parts(
            shownOpt,
            filled,
            heldKey,
            active,
            placeholder,
            props(_.size),
            props(_.variant),
            props(_.fluid),
            props(_.required),
            props(_.checkmark),
            hi,
            query,
            options,
            roleOf(switched)
        )
        UI.reactive {
            val b      = base.read
            val (o, f) = panel.read
            root(
                b,
                acts,
                parts,
                Flags(
                    o,
                    f,
                    props(_.disabled).read,
                    props(_.readonly).read || b.fixed,
                    loading.read,
                    props(_.showClear).read && hasValue.read,
                    props(_.name).read,
                    strays.read
                ),
                idOf(b)
            )
        }
    end drawnOver

    /** The trigger's role while the switch is a signal, `button` while the filter header is on and `combobox` while it is off: a
      * channel, so a closed field subscribes nothing else of the switch and switching it patches the role in place. kyo-ui's
      * `role` takes a constant only, so a signal goes through the attribute channel.
      */
    private def roleOf(switched: Prop[Boolean])(using Frame): Maybe[Signal[String]] =
        switched match
            case sig: Signal[Boolean] @unchecked => Present(sig.map(on => if on then "button" else "combobox"))
            case _                               => Absent

    /** What the root's structure follows, read by its block. */
    final private case class Flags(
        isOpen: Boolean,
        filtering: Boolean,
        disabled: Boolean,
        readonly: Boolean,
        loading: Boolean,
        clear: Boolean,
        name: Maybe[String],
        strays: Seq[String]
    ):
        def interactive: Boolean = !disabled && !readonly
    end Flags

    /** What the root's parts read on their own: the label's and the panel's blocks, and the root's channels. */
    final private case class Parts[A](
        shownOpt: Prop[Maybe[A]],
        filled: Prop[Boolean],
        heldKey: Prop[String],
        active: Prop[Maybe[String]],
        placeholder: Prop[Maybe[String]],
        size: Prop[Size],
        variant: Prop[FieldVariant],
        fluid: Prop[Boolean],
        required: Prop[Boolean],
        checkmark: Prop[Boolean],
        hi: Signal[Int],
        query: Prop[String],
        options: Prop[List[OptionItem[A]]],
        role: Maybe[Signal[String]]
    )

    /** The field's root, drawn again only when its [[Flags]] or its base change. */
    private def root[A](b: Base[A], acts: Acts[A], parts: Parts[A], fl: Flags, idBase: Maybe[String])(using Frame): UI =
        val interactive = fl.interactive

        val clearUI: Maybe[UI] =
            if fl.clear && interactive && b.clearable then
                Present(
                    button
                        .cssClass("p-select-clear-icon")
                        .jsProp("type", "button")
                        .aria(Aria.label -> "Clear")
                        .onClick(acts.write(Absent))
                        // The clear sits inside the field, whose click opens the panel.
                        .stopPropagation(true)(GlyphSvg(Icons.times, "p-icon"))
                )
            else Absent

        // PrimeVue's loading slot: the spinner takes the chevron's place in the dropdown cell.
        val dropdownUI: UI =
            span.cssClass("p-select-dropdown")(
                if fl.loading then GlyphSvg(Icons.spinner, "p-select-loading-icon", "p-icon", "p-icon-spin")
                else GlyphSvg(Icons.chevronDown, "p-select-dropdown-icon", "p-icon")
            )

        // Hidden form carrier: a div trigger has no native form value.
        val hiddenCarrier: Maybe[UI] =
            fl.name.map(n => hiddenInput.jsProp("name", n).value(parts.heldKey))

        val panelUI: Maybe[UI] = if fl.isOpen then Present(overlayPanel(b, acts, parts, fl.filtering, idBase)) else Absent

        // WHICH element is the combobox is decided by whether there is a filter header, and it is decided once: the combobox
        // is whatever holds focus while the list is open, because `aria-activedescendant` is read off the focused element or
        // off nothing at all. Without a header focus never leaves this trigger, so the trigger is the combobox. With one,
        // focus goes into the header's text box and that box is the combobox, exactly as in [[AutoComplete]]; the trigger is
        // then the button that reveals it. A signal switch is a `role` attribute channel (see `roleOf`).
        def roled(el: Ast.Div): Ast.Div =
            parts.role match
                case Present(sig) => el.withReactiveAttr("role", sig)
                case Absent       => el.role(if fl.filtering then "button" else "combobox")

        // The id goes on the root because the root is the control: it holds the tab stop and is the combobox (or, with a
        // filter header, the button that opens it), so this is what a `Label.forId`, `Commands.focusId` and the form's
        // focus-first-invalid have to reach. The panel's parts derive from the same value (`$id-list`, `$id-option-<n>`).
        val validity = FieldValidity(b.validity)
        val rooted   = validity.mark(
            b.extraClasses.foldLeft(div.cssClass("p-select").cssClass("p-component").when(b.access.id)(_.id(_)))(_.cssClass(_))
                // The float hook: a selected value fills the field; a non-empty placeholder shows text too, so the label must
                // float over it.
                .when(b.inputwrapper)(_.cssClass("p-inputwrapper"))
                .when(b.inputwrapper)(_.when(parts.filled)(_.cssClass("p-inputwrapper-filled")))
                .when(fl.isOpen)(_.cssClass("p-select-open", "p-uic-overlay-anchor"))
        )
            .when(fl.disabled)(_.cssClass("p-disabled"))
            .when(fl.readonly)(_.cssClass("p-uic-select-readonly").aria(Aria.readonly -> "true"))
            .aria(Aria.required -> Prop.map[Boolean, Maybe[String]](parts.required)(r => if r then Present("true") else Absent))
            .when(fl.loading)(_.aria(Aria.busy -> "true"))
            .cssClass.matching(parts.size) {
                case Size.Small  => "p-select-sm"
                case Size.Normal => Absent
                case Size.Large  => "p-select-lg"
            }
            .cssClass.matching(parts.variant) {
                case FieldVariant.Outlined => Absent
                case FieldVariant.Filled   => "p-variant-filled"
            }
            .when(parts.fluid)(_.cssClass("p-select-fluid"))
            .title(b.tooltip)
            .aria(Aria.label -> b.access.name, Aria.labelledby -> b.access.nameRef)
        val el = roled(rooted)
            .aria(Aria.haspopup -> "listbox", Aria.expanded -> fl.isOpen)
            .when(idBase) { (e, id) =>
                e.aria(Aria.controls -> listId(id)).when(fl.isOpen && !fl.filtering)(_.aria(Aria.activedescendant -> parts.active))
            }
            // readonly keeps the field FOCUSABLE and says `aria-readonly` above; it simply answers no key and opens nothing.
            // `disabled` is what takes a control out of the tab order.
            .when(fl.readonly && !fl.disabled)(_.tabIndex(0))
            .when(interactive)(_.tabIndex(0).preventScrollKeys)
            // The one click listener: the field is the control, so a click anywhere on it toggles, its padding and a click
            // addressed by its id included. What inside does work of its own ends the click there: the clear button, and the
            // panel (`Overlay.containClicks`). A panel fixed open has no close for a click to ask for, so the field takes none
            // rather than one that changes nothing.
            .when(interactive && !b.fixedOpen)(_.onClick(acts.toggle))
            .when(interactive)(_.onKeyDown(acts.triggerKey))
            // Focus-loss on the trigger reports the option held as focus leaves: the validation layer's Blur trigger.
            .when(interactive && b.blurs)(_.onBlur(acts.blur))

        val labelUI = labelOf(b, parts)
        val field   = validity.follow(el(UI.children(labelUI, clearUI, dropdownUI, hiddenCarrier, panelUI)*))
        // A selection bound one way that nothing hears: every pick is painted away again. A disabled field takes no pick, so
        // it has nothing to say.
        val checked = Diagnostics.after(field)("Select")(
            Diagnostic.unknown(fl.strays),
            Diagnostic(interactive && b.deaf, selectionName.advice)
        )
        // A panel or a query bound one way that nothing hears: every change the reader asks for there is painted away again.
        Diagnostics.before(checked)("Select")(
            Diagnostic(interactive && b.openDeaf, openName.advice),
            Diagnostic(interactive && b.queryDeaf, queryName.advice)
        )
    end root

    /** The label: the shown option (drawn by the template where there is one), the placeholder where nothing is held, or a
      * non-breaking space, which keeps the empty label's line box (Prime renders `&nbsp;`). A block of its own, so a pick
      * redraws the label alone.
      */
    private def labelOf[A](b: Base[A], parts: Parts[A])(using Frame): UI =
        UI.reactive {
            val shown           = parts.shownOpt.read
            val ph              = parts.placeholder.read
            val showPlaceholder = shown.isEmpty && ph.isDefined
            val text            = if showPlaceholder then ph.getOrElse("") else shown.fold("")(b.label)
            val lbl             = span.cssClass("p-select-label")
                .when(showPlaceholder)(_.cssClass("p-placeholder"))
                .when(text.isEmpty)(_.cssClass("p-select-label-empty"))
            shown.flatMap(a => b.itemTemplate.map(_(a))) match
                case Present(drawn) if !showPlaceholder => lbl(drawn)
                case _                                  => lbl(if text.isEmpty then " " else text)
            end match
        }
    end labelOf

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
    private def panelKey[A](n: Now[A], acts: Acts[A])(e: KeyboardEvent)(using Frame): UI.Action =
        val st        = shownState(n.b, n.opts, n.filtering, n.hiV, n.qV)
        val caretOwns = n.filtering && (e.key == Keyboard.Space || e.key.charValue.isDefined)
        if e.key == Keyboard.Tab then acts.setOpen(false)
        else if caretOwns then ()
        else
            ListNav.onKey(st.navigable, st.hiEff, e.key, wrap = false) match
                case Present(step) if step.dismiss => acts.setOpen(false)
                case Present(step)                 =>
                    val picked: UI.Action =
                        if step.activate && st.shown.isDefinedAt(step.focus) then acts.pick(st.shown(step.focus)) else ()
                    acts.hi.set(step.focus).andThen(picked)
                case Absent =>
                    e.key match
                        case Keyboard.Char(c) =>
                            Typeahead.jump(st.shown.map(n.b.label), st.navigable, st.hiEff, c) match
                                case Present(to) => acts.hi.set(to)
                                case Absent      => ()
                        case _ => ()
        end if
    end panelKey

    /** The options the panel is showing (grouped and flat), the positions the highlight may land on, and where it is now.
      * Derived in one place because the TRIGGER reads it too: the keyboard lives on whatever holds focus, and that is not the
      * panel. Prime's select stops at the ends rather than cycling, so `wrap` is false wherever the navigable list is read.
      */
    final private case class ShownState[A](groups: List[OptionItem[A]], shown: Seq[A], navigable: List[Int], hiEff: Int)

    private def shownState[A](b: Base[A], opts: List[OptionItem[A]], filtering: Boolean, hiV: Int, qV: String): ShownState[A] =
        val groups =
            if filtering && qV.nonEmpty then OptionItem.filter(opts, a => b.matcher(a, qV))
            else opts
        val shown = OptionItem.flatten(groups)
        val hiEff = if shown.isEmpty then -1 else math.min(hiV, shown.size - 1)
        ShownState(groups, shown, shown.indices.toList.filterNot(i => b.optionDisabled(shown(i))), hiEff)
    end shownState

    /** The floating option panel: Prime's `.p-select-overlay` skin on the [[Overlay]] primitive, drawn only while open. What
      * it shows (the filter header, the key-collision card, the rows) is one block over the highlight, the query, the options
      * and the held key, so an arrow key redraws the rows and not the field, and the panel stays the same element.
      */
    private def overlayPanel[A](b: Base[A], acts: Acts[A], parts: Parts[A], filtering: Boolean, idBase: Maybe[String])(using
        Frame
    ): UI =
        val content = UI.reactive {
            val h       = parts.hi.read
            val qq      = parts.query.read
            val opts    = parts.options.read
            val current = parts.heldKey.read
            val check   = parts.checkmark.read
            val st      = shownState(b, opts, filtering, h, qq)
            fragment((filterHeader(b, acts, parts, st, idBase, filtering).toList ++ keyCollisionCard(b, OptionItem.flatten(opts)) ++
                List(optionList(b, acts, st, current, check, idBase)))*)
        }
        // The panel is drawn only while open, so its slot says only who hears a dismissal: the run's open binding when the event
        // comes, or nobody where a constant holds the panel open, which then has no backdrop to dismiss it.
        val shownOpen = Controlled[Boolean](bound = Present(true))
        Overlay.hosted(if b.fixedOpen then shownOpen else shownOpen.hear(acts.setOpen))
            .portal(b.portal)
            .scroll(b.scroll)
            .panelClass("p-select-overlay")
            .panelClass("p-component")
            // Nothing in this panel takes focus (a filter header seeds its own input), so nothing in it receives a key either: no
            // seed, no key handler, and no Escape of its own.
            .seedFocus(false)
            .dismissOnEscape(false)
            .containClicks(true)(content)
            .renderShown(idBase.map(id => s"$id-panel"))
    end overlayPanel

    /** The option rows and the empty row, in Prime's list container. */
    private def optionList[A](b: Base[A], acts: Acts[A], st: ShownState[A], current: String, checkmark: Boolean, idBase: Maybe[String])(
        using Frame
    ): UI =
        val rows: List[UI] = OptionItem.rows(st.groups, "p-select-option-group") { (a, i) =>
            val isSel                = b.key(a) == current
            val isDis                = b.optionDisabled(a)
            val checkSlot: Maybe[UI] =
                if !checkmark then Absent
                else if isSel then Present(GlyphSvg(Icons.check, "p-select-option-check-icon", "p-icon"))
                else Present(span.cssClass("p-select-option-blank-icon").cssClass("p-icon").aria(Aria.hidden -> "true"))
            val row = li.cssClass("p-select-option").role("option").aria(Aria.selected -> isSel)
                .when(isSel)(_.cssClass("p-select-option-selected"))
                .when(isDis)(_.cssClass("p-disabled").aria(Aria.disabled -> "true"))
                .when(i == st.hiEff)(_.cssClass("p-focus").scrollAuto(true))
                // The id the list points `aria-activedescendant` at. Stamped on every option rather than on the highlighted
                // one alone, so the attribute names a target that is already in the document whichever row the highlight is on.
                .when(idBase)((r, id) => r.id(optionId(id, i)))
                .when(!isDis)(_.onClick(acts.pick(a)))
                // The template may hold no text at all, so the row names itself.
                .when(b.itemTemplate.isDefined)(_.aria(Aria.label -> b.label(a)))
            val content: UI = b.itemTemplate.fold(span.cssClass("p-select-option-label")(b.label(a)): UI)(_(a))
            row(UI.children(checkSlot, content)*)
        }
        val emptyRow: Maybe[UI] =
            if st.shown.isEmpty then
                Present(EmptyContent.render(b.emptyContent, "No results found")(c =>
                    li.cssClass("p-select-empty-message").role("presentation")(c)
                ))
            else Absent
        // The list is named by `aria-controls` and holds the option ids; it does NOT carry the announcement, because it never
        // holds focus and the attribute is read off the element that does.
        div.cssClass("p-select-list-container")(
            ul.cssClass("p-select-list").role("listbox").when(idBase)((l, id) => l.id(listId(id)))(UI.children(rows, emptyRow)*)
        )
    end optionList

    /** Prime's header filter, wrapped in an IconField with a trailing search icon; the sibling :not(:last-child) sheet rule
      * pads the input for the icon.
      */
    private def filterHeader[A](b: Base[A], acts: Acts[A], parts: Parts[A], st: ShownState[A], idBase: Maybe[String], on: Boolean)(
        using Frame
    ): Maybe[UI] =
        if !on then Absent
        else
            val filterEl = input
                .cssClass("p-select-filter")
                .cssClass("p-inputtext")
                .cssClass("p-component")
                // The bound input renders inside a reactive wrapper span, which resets sibling positions; the sheet's
                // :not(:last-child) padding cannot see the trailing icon, so stamp the deterministic IconField padding class.
                .cssClass("p-uic-iconfield-end")
                .role("combobox")
                .aria(
                    Aria.label    -> "Filter",
                    Aria.haspopup -> "listbox",
                    Aria.expanded -> "true"
                )
                // Bound to the query itself (the instance's own, or the owner's): typing writes a two-way binding and reports,
                // and kyo-ui puts a one-way field back to the owner's query.
                .value(parts.query)
                // Typing re-filters; highlight the first match (Prime behavior).
                .onInput(text => acts.hi.set(0).andThen(acts.setQuery(text)))
                // The header is where focus goes and therefore where the option keyboard lives: the arrows move the highlight,
                // the caret keeps Space and the printable keys, Escape closes.
                .focusAuto(true)
                .focusRestore(true)
                .preventScrollKeys
                .onKeyDown(acts.headerKey)
                .when(idBase) { (e, id) =>
                    e.aria(Aria.controls -> listId(id)).when(st.hiEff >= 0)(_.aria(Aria.activedescendant -> optionId(id, st.hiEff)))
                }
            Present(
                div.cssClass("p-select-header")(
                    div.cssClass("p-iconfield")(
                        filterEl,
                        span.cssClass("p-inputicon")(GlyphSvg(Icons.search, "p-icon"))
                    )
                )
            )
    end filterHeader

    /** The loud card shown at the top of the panel when the option keys collide, so a duplicate-label data set cannot silently
      * select the wrong row (see [[Diagnostics]]). Empty in the healthy case, which is every existing render.
      */
    private def keyCollisionCard[A](b: Base[A], all: List[A])(using Frame): Chunk[UI] =
        Diagnostics.cards("Select")(Diagnostic.naming(
            if b.key eq b.label then
                "option labels are not unique and optionKey is unset, so picks apply to the wrong option; set optionKey"
            else "optionKey is not unique across the options, so picks apply to the wrong option",
            Diagnostics.duplicates(all.map(b.key))
        ))
    end keyCollisionCard
end Select
