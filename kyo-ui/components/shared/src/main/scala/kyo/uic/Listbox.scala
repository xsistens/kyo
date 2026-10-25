package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One option of a [[Listbox]], a hand-authored carrier (not a `kyo.UI`): a
  * `text` label, a stable `id` of the app's own type `K` that the selection holds
  * and `onItemClick` reports, an optional leading `icon` glyph (kyo extension,
  * `.p-uic-option-icon`), an optional right-aligned `additionalText` (kyo
  * extension, `.p-uic-option-extra`), and a native `tooltip` (title attribute).
  */
final case class ListItem[K](
    text: TextValue,
    id: K,
    icon: Maybe[IconGlyph] = Absent,
    additionalText: Maybe[String] = Absent,
    tooltip: Maybe[String] = Absent
)

object ListItem:
    /** Ergonomic public constructor with a constant label: the primary `text: TextValue` field is
      * package-private, so this is how callers outside `kyo.uic` build an item that also carries an
      * `additionalText` badge or a `tooltip` (the [[Listbox.item]] builders cover only text+icon).
      * A distinct name (not an `apply` overload) because Scala 3 forbids two `apply` variants that both
      * carry default arguments.
      */
    def of[K](
        text: String,
        id: K,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        tooltip: Maybe[String] = Absent
    ): ListItem[K] = ListItem(TextValue.Const(text), id, icon, additionalText, tooltip)
end ListItem

/** A Listbox option, built directly or as `_.item(...)` (see [[ListItemScope]]). */
type ListItemEntry[K] = Entry[ListItem[K], ListItemScope[K]]

/** A [[ListItemEntry]] or a signal of options, for a list whose options change while it is shown (see [[LiveEntry]]). */
type ListItemLiveEntry[K] = LiveEntry[ListItem[K], ListItemScope[K]]

/** What `_` stands for in [[Listbox.items]] and inside a group of [[Listbox.optionGroups]]: `_.item("Paris", "par")`. The label
  * takes a `Signal[String]` too, as [[Listbox.item]] does.
  */
final class ListItemScope[K] private ():
    /** An option keyed by `id`, as [[ListItem.of]], with a constant or reactive label. */
    def item(
        text: String | Signal[String],
        id: K,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        tooltip: Maybe[String] = Absent
    ): ListItem[K] = ListItem(ReactiveValue(text), id, icon, additionalText, tooltip)
end ListItemScope

object ListItemScope:
    // The scope holds nothing of its own type, so one instance serves every `K`.
    private val scope = new ListItemScope[Any]

    private[uic] def resolve[K](entries: Seq[ListItemEntry[K]]): List[ListItem[K]] =
        Entry.resolve(scope.asInstanceOf[ListItemScope[K]], entries)

    /** The entries as option sources, in order: a signal stays one, everything else is a fixed row. */
    private[uic] def sources[K](entries: Seq[ListItemLiveEntry[K]]): List[OptionSource[ListItem[K]]] =
        entries.toList.map {
            case sig: Signal[?] => OptionSource.Values(sig.asInstanceOf[Signal[Seq[ListItem[K]]]])
            case entry          => OptionSource.Fixed(resolve(Seq(entry.asInstanceOf[ListItemEntry[K]])).map(OptionItem.Item(_)))
        }
end ListItemScope

/** An option or a group of options, built directly or as `_.item(...)` or `_.group(...)(...)` (see [[ListOptionScope]]). */
type ListOptionEntry[K] = Entry[OptionItem[ListItem[K]], ListOptionScope[K]]

/** A [[ListOptionEntry]] or a signal of rows (see [[LiveEntry]]). */
type ListOptionLiveEntry[K] = LiveEntry[OptionItem[ListItem[K]], ListOptionScope[K]]

/** What `_` stands for in [[Listbox.optionGroups]]: `_.item(...)` for an option beside the groups and `_.group("Europe")(...)` for a
  * labelled group, whose own entries are built the way [[Listbox.items]] builds them.
  */
final class ListOptionScope[K] private ():
    /** An ungrouped option, as [[OptionItem.item]] over [[ListItemScope.item]]. */
    def item(
        text: String | Signal[String],
        id: K,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        tooltip: Maybe[String] = Absent
    ): OptionItem[ListItem[K]] = OptionItem.Item(ListItem(ReactiveValue(text), id, icon, additionalText, tooltip))

    /** A group labelled `label`, as [[OptionItem.group]]. */
    def group(label: String)(items: ListItemEntry[K]*): OptionItem[ListItem[K]] =
        OptionItem.Group(label, ListItemScope.resolve(items))
end ListOptionScope

object ListOptionScope:
    // As in [[ListItemScope]]: nothing in the scope depends on `K`.
    private val scope = new ListOptionScope[Any]

    private[uic] def resolve[K](entries: Seq[ListOptionEntry[K]]): List[OptionItem[ListItem[K]]] =
        Entry.resolve(scope.asInstanceOf[ListOptionScope[K]], entries)

    /** The entries as option sources, in order: a signal stays one, everything else is a fixed row. */
    private[uic] def sources[K](entries: Seq[ListOptionLiveEntry[K]]): List[OptionSource[ListItem[K]]] =
        entries.toList.map {
            case sig: Signal[?] => OptionSource.Rows(sig.asInstanceOf[Signal[Seq[OptionItem[ListItem[K]]]]])
            case entry          => OptionSource.Fixed(resolve(Seq(entry.asInstanceOf[ListOptionEntry[K]])))
        }
end ListOptionScope

/** Listbox: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Listbox anatomy: `div.p-listbox.p-component` > `div.p-listbox-list-container`
  * > `ul.p-listbox-list[role=listbox]` of `li.p-listbox-option[role=option]`
  * rows, with `.p-listbox-option-selected` on picked rows and an optional
  * `li.p-listbox-empty-message` when there is nothing to show), so the
  * extracted `@primeuix` listbox CSS applies verbatim.
  *
  * Selection binds two-way to a `SignalRef[Set[K]]` of item ids: the list re-renders
  * reactively so each row's `aria-selected` tracks the set, and clicking a row
  * (when a [[SelectionMode]] other than `None` is set) writes the updated set
  * back before firing `onItemClick`. `checkmark(true)` renders Prime's per-row
  * check-icon column.
  *
  * `optionGroups(...)` takes [[OptionItem]] rows instead of plain items,
  * rendering each group under a labelled header. Grouping is a list concern
  * only: selection stays keyed by `id` across the whole listbox. `K` is the app's
  * own id type (`uic.Listbox[CityId]()`), so the set a page reads is never a set of
  * strings standing in for its values.
  *
  * Keyboard (WAI-ARIA listbox, the shared [[ListNav]] machine): the list is ONE tab stop and
  * roves Prime's `.p-focus` highlight inside it, rather than making every option its own tab
  * stop. ArrowDown/ArrowUp move the highlight and stop at the ends (Prime's listbox does not
  * cycle), Home/End reach them directly, and Enter or Space picks the highlighted option
  * through the same write a click makes. `id(...)` wires `aria-activedescendant` to the
  * highlighted row, and a listbox with no `id` of its own mints one for that hook alone.
  * The page as first rendered, before the mount has a highlight to rove, already has that one
  * tab stop on the list, and no option is a second one. [[OrderList]] and [[PickList]] embed
  * the same list and hand it a highlight their own mount mints.
  *
  * Two levels say where the keyboard is, and they never mix: a ring around the whole
  * component (`.p-listbox:has(.p-listbox-list:focus)`, kyo glue, since Prime clears the
  * list's own outline and stamps no ring at all) means the list holds focus, and the
  * highlight on one row means that row is current. Arriving on the list highlights the
  * selected row, or the first one, so the reader can see where the arrows will move from;
  * leaving drops the highlight and fires `onBlur`.
  *
  * Filtering has the two shapes shared by every picker in this module:
  * `filterable(true)` renders Prime's header filter over a query the listbox
  * allocates itself (the same word and the same meaning as
  * [[Select.filterable]] / [[MultiSelect.filterable]]), and `filterQuery(ref)`
  * renders it over a query the app owns. Either way the options are filtered by
  * a case-insensitive contains on the label at render time.
  *
  * `id` lands on the `ul.p-listbox-list`, the listbox that holds the one tab stop, and each option derives
  * `$id-option-<n>` from it (a minted base stands in where none is given).
  */
final case class Listbox[K] private (
    // The rows the body draws, resolved from `optionSources` by the region that draws them.
    optionsV: List[OptionItem[ListItem[K]]] = Nil,
    optionSources: List[OptionSource[ListItem[K]]] = Nil,
    selectionModeV: SelectionMode = SelectionMode.None,
    metaKeyFlag: Boolean = false,
    selectedRef: Maybe[SetBinding[K]] = Absent,
    filterQueryRef: Maybe[SignalRef[String]] = Absent,
    filterableV: Maybe[BoolValue] = Absent,
    onItemClickF: Maybe[K => UI.Action] = Absent,
    templateF: Maybe[ListItem[K] => UI] = Absent,
    checkmarkFlag: Boolean = false,
    disabledFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    emptyContentV: Maybe[EmptyContent] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[K] => UI.Action] = Absent,
    idV: Maybe[String] = Absent,
    hostKeyF: Maybe[UI.KeyboardEvent => Maybe[Listbox.HostKey]] = Absent
) extends Node, MultiChoiceFormControl[K], HasEmptyContent, HasAccessibleNameRef:
    type Self = Listbox[K]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Listbox[K] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Chords the HOST claims, tried before the list's own machine sees them.
      *
      * [[OrderList]] and [[PickList]] put their move and transfer keys here, because the list is
      * the element that holds the focus and so the only one that can answer them. Returning
      * `Absent` leaves the chord to the list and then to the browser, so a host cannot swallow the
      * navigation it is nested in.
      */
    private[uic] def onHostKey(f: UI.KeyboardEvent => Maybe[Listbox.HostKey]): Listbox[K] = copy(hostKeyF = Present(f))

    /** Appends the given options, each built directly or from `_` (see [[ListItemScope]]), or a signal of options for a
      * list whose options change while it is shown (see [[LiveEntry]]).
      */
    def items(is: ListItemLiveEntry[K]*): Listbox[K] = copy(optionSources = optionSources ++ ListItemScope.sources(is))

    /** Appends a single option (ergonomic builder; no need to construct [[ListItem]]). A
      * `Signal[String]` label re-renders the option in place on emission; `id` stays the stable
      * selection key either way, and `icon` keeps its default for both label shapes.
      */
    def item(text: String | Signal[String], id: K, icon: Maybe[IconGlyph] = Absent): Listbox[K] =
        copy(optionSources = optionSources :+ OptionSource.Fixed(List(OptionItem.Item(ListItem(ReactiveValue(text), id, icon)))))

    /** Appends grouped options: [[OptionItem.group]] rows render their items
      * under a labelled header, [[OptionItem.item]] rows beside them.
      *
      * Grouping changes the list only. Selection stays keyed by each item's `id`
      * across the whole listbox, and the filter drops a group once it has
      * emptied it. Each entry is built directly or from `_` (see [[ListOptionScope]]).
      */
    def optionGroups(is: ListOptionLiveEntry[K]*): Listbox[K] = copy(optionSources = optionSources ++ ListOptionScope.sources(is))

    /** Selection semantics: `None` (default: rows inert unless `onItemClick` is
      * set), `Single` (picking replaces the selection; re-picking clears it), or
      * `Multiple` (picking toggles the row in the set).
      */
    def selectionMode(v: SelectionMode): Listbox[K] = copy(selectionModeV = v)

    /** Whether picking takes a modifier key (Prime's `metaKeySelection`).
      *
      * Off (the default, and Prime's) every click toggles its row. On, a plain click replaces
      * the selection with the row it landed on and Ctrl or Cmd toggles (`metaKey || ctrlKey`,
      * never one of the two). `Checkbox` mode ignores it: a checkbox already says the set is
      * additive. See [[SelectionPick]] for the exact rule, which every component offering it
      * shares.
      *
      * There is no shift-range here, matching Prime, which ranges on shift in `DataTable` only.
      */
    def metaKeySelection(v: Boolean): Listbox[K] = copy(metaKeyFlag = v)

    /** Binds the selection two-way to the set of chosen item ids: clicks update the set, ref changes re-render
      * `aria-selected`. Spelled `selection` like every other picker's bound choice: a Listbox is the always-visible
      * member of that family, not a different concept.
      */
    @targetName("selectionSet")
    def selection(ref: SignalRef[Set[K]]): Listbox[K] = copy(selectedRef = Present(SetBinding.of(ref)))

    /** Package-internal: an embedding host's typed selection, read and written through its own keys. */
    private[uic] def selectionBinding(b: SetBinding[K]): Listbox[K] = copy(selectedRef = Present(b))

    /** Switches Prime's header filter input (`div.p-listbox-header` > `input.p-listbox-filter.p-inputtext`), over a
      * query the listbox allocates itself unless [[filterQuery]] hands it one: the same word, and the same meaning, as
      * [[Select.filterable]] and [[MultiSelect.filterable]].
      *
      * Without it, [[filterQuery]] turns the header on. Set, constant or signal, it decides alone, so
      * `filterable(false)` keeps the header off with a query bound. Off, the header is gone and every option shows,
      * but the query is left as it is: switched back on, the header shows it and filters by it again. A signal shows
      * and hides the header in a region of its own inside the listbox, and the list repaints only where the options
      * it shows change.
      */
    def filterable(v: Boolean | Signal[Boolean]): Listbox[K] = copy(filterableV = Present(ReactiveValue(v)))

    /** Renders the same header filter, bound two-way to `ref`: typing writes the
      * query, external writes re-filter the options (a case-insensitive contains
      * on the label). Turns the header filter on unless [[filterable]] says otherwise.
      */
    def filterQuery(ref: SignalRef[String]): Listbox[K] = copy(filterQueryRef = Present(ref))

    /** Fired with the id of the row clicked or picked by key, after the selection write. */
    def onItemClick(f: K => UI.Action)(using Frame): Listbox[K] = copy(onItemClickF = Present(Callback.of(f)))

    /** Custom option content (Prime's `#option` slot): renders instead of the plain
      * text label; the checkmark column and leading icon keep their places.
      */
    def itemTemplate(f: ListItem[K] => UI): Listbox[K] = copy(templateF = Present(f))

    /** Renders Prime's option check-icon column: the check glyph on selected rows
      * (`.p-listbox-option-check-icon`), a blank placeholder on the rest.
      */
    def checkmark(v: Boolean): Listbox[K] = copy(checkmarkFlag = v)

    /** Disables the whole listbox (`.p-disabled`): rows keep their look but stop
      * responding to clicks.
      */
    def disabled(v: Boolean): Listbox[K] = copy(disabledFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): Listbox[K]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): Listbox[K]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Listbox[K] = copy(invalidMsgDynV = v)

    /** Fires on focus loss with the current selection: the validation layer's Blur
      * trigger.
      */
    @targetName("onBlurSet")
    def onBlur(f: Set[K] => UI.Action)(using Frame): Listbox[K] = copy(onBlurF = Present(Callback.of(f)))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): Listbox[K] = copy(emptyContentV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Listbox[K] = copy(accessibleNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Listbox[K] = copy(accessibleNameRefV = v)

    /** Whether the header filter is on: an explicit [[filterable]] decides alone, as an explicit `invalid` does in
      * [[FieldValidity]]; without one, an app-owned [[filterQuery]] turns it on.
      */
    // An id is the identity (see "A choice binds the option" in this module's CLAUDE.md), so its own equality decides.
    private given CanEqual[K, K]                     = CanEqual.derived
    private given CanEqual[ListItem[K], ListItem[K]] = CanEqual.derived

    private def filterOn: BoolValue = filterableV.getOrElse(BoolValue.Const(filterQueryRef.isDefined))

    /** Whether rows respond to clicks at all (selecting or an explicit item handler). */
    private def interactive: Boolean =
        !disabledFlag && (selectionModeV != SelectionMode.None || onItemClickF.isDefined)

    /** Package-internal: hosts (OrderList/PickList) that already subscribe to the
      * same selection ref in an enclosing render supply the resolved set directly;
      * a second nested subscription to the same ref would race the inner update
      * against the outer replace (the Overlay renderOpen lesson). The click
      * handlers still write through the BOUND refs.
      */
    private[uic] def resolved(sel: Set[K], query: String, hi: Maybe[SignalRef[Int]] = Absent, focused: Int = -1)(
        using Frame
    ): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        // The host reads the selection in its own region; options it hands over are fixed, so this adds no region, and a
        // signal among them is still read rather than dropped.
        validity.follow(OptionItem.reading(optionSources).render(opts =>
            copy(optionsV = opts).body(sel, query, Absent, hi, idV, focused)(validity)
        ))
    end resolved

    private[uic] def render(using Frame): UI =
        renderQuery(FieldValidity(invalidV, invalidMsgV, invalidMsgDynV))

    /** One mount for the two signals this listbox owns: the keyboard highlight, and the
      * filter query when `filterable` asked for a header without handing over a ref.
      *
      * Both are allocated OUTSIDE the selection and query subscriptions, since a mount
      * inside a subscribed region re-runs on every emission and would allocate a new
      * highlight on each keystroke. The static projection renders through the placeholder,
      * where the list is already the one tab stop and carries the id, with no highlight yet.
      */
    private def renderQuery(validity: FieldValidity)(using Frame): UI =
        // A switch that may turn on (a signal, or `true`) needs a query to filter by; only a constant `false` does not.
        val ownsQuery = filterQueryRef.isEmpty && !filterOn.const.exists(on => !on)
        UI.mounted {
            for
                cmds <- UI.commands
                // The id the highlight is announced through. A caller's own `id` wins, and where
                // there is none a minted one stands in, so `aria-activedescendant` is not
                // something a reader has to opt into to be told which option is highlighted.
                base <- idV.map(v => Kyo.lift(v)).getOrElse(cmds.freshId)
                hi   <- Signal.initRef(-1)
                q    <-
                    if ownsQuery then Signal.initRef("").map(r => Present(r): Maybe[SignalRef[String]])
                    else filterQueryRef: Maybe[SignalRef[String]] < Sync
            yield subscribed(q, Present(hi), Present(base), validity)
        }.placeholder(subscribed(filterQueryRef, Absent, Absent, validity))
    end renderQuery

    /** The selection/query subscriptions around one [[body]] render. */
    private def subscribed(
        qRef: Maybe[SignalRef[String]],
        hiRef: Maybe[SignalRef[Int]],
        idBase: Maybe[String],
        validity: FieldValidity
    )(using Frame): UI =
        // The selection, the query and the highlight are read as ONE region (see [[Reading]]): a row click writes the
        // highlight and the selection, and a nested region would repaint the values it closed over.
        val shown = shownQuery(qRef)
        val sel   = selectedRef.fold(Reading.Fixed(Set.empty[K]))(s => Reading.Live(s.signal))
        val query = shown.fold(Reading.Fixed(""))(Reading.Live(_))
        val hi    = hiRef.fold(Reading.Fixed(-1))(Reading.Live(_))
        validity.follow(sel.and(query).and(hi).and(OptionItem.reading(optionSources)).render { case (((s, q), h), opts) =>
            copy(optionsV = opts).body(s, q, if shown.isDefined then qRef else Absent, hiRef, idBase, h)(validity)
        })
    end subscribed

    /** The query as the options see it: the typed one while the filter is on, none while it is off, and nothing to
      * subscribe where the switch is a constant `false`. A signal switch is folded in here rather than read in a region
      * of its own, so switching it over an empty query emits nothing and repaints nothing.
      */
    private def shownQuery(qRef: Maybe[SignalRef[String]])(using Frame): Maybe[Signal[String]] =
        qRef.flatMap { q =>
            filterOn match
                case BoolValue.Dyn(on)     => Present(q.combineLatest(on).map((typed, isOn) => if isOn then typed else ""))
                case BoolValue.Const(true) => Present(q)
                case _                     => Absent
        }

    private def body(
        sel: Set[K],
        query: String,
        qRef: Maybe[SignalRef[String]],
        hiRef: Maybe[SignalRef[Int]],
        idBase: Maybe[String],
        hi: Int = -1
    )(validity: FieldValidity)(using Frame): UI =
        val shownGroups =
            if query.isEmpty then optionsV
            else
                OptionItem.filter(
                    optionsV,
                    it =>
                        it.text match
                            case TextValue.Const(t) => t.toLowerCase.contains(query.toLowerCase)
                            case TextValue.Dyn(_)   => true
                )
        val shown = OptionItem.flatten(shownGroups)

        // Every option on the screen can take the highlight: a ListItem carries no disabled
        // flag, and the filter has already dropped the rows that are not there.
        val navigable: List[Int] = shown.indices.toList
        val focused: Int         = if navigable.contains(hi) then hi else -1

        val navigate: KeyboardEvent => UI.Action = e =>
            hiRef match
                case Present(ref) =>
                    // Prime's listbox stops at the ends rather than cycling, unlike the menu
                    // family: a reader holding ArrowDown expects to arrive at the last option.
                    ListNav.onKey(navigable, focused, e.key, wrap = false) match
                        case Present(step) =>
                            val pick: UI.Action =
                                if step.activate && shown.isDefinedAt(step.focus) then activate(shown(step.focus).id)
                                else ()
                            ref.set(step.focus).andThen(pick)
                        case Absent => ()
                case Absent => ()

        // The host is asked first, because the chords it claims (a carried arrow, a transfer) are
        // ones the list's own machine would otherwise read as plain navigation.
        val keyHandler: KeyboardEvent => UI.Action = e =>
            val claimed: Maybe[Listbox.HostKey] = hostKeyF match
                case Present(f) => f(e)
                case Absent     => Absent
            claimed match
                case Present(claim) => claim.effect
                case Absent         => navigate(e)

        // Arriving on the list highlights a row, rather than leaving the reader to guess where
        // the first arrow will land. The selected row wins, since that is where a reader
        // expects to continue; with nothing selected the highlight starts at the top.
        val seedFocus: UI.Action = hiRef match
            case Present(ref) if focused < 0 && navigable.nonEmpty =>
                val selectedRow = shown.indexWhere(o => sel.contains(o.id))
                ref.set(if selectedRow >= 0 then selectedRow else navigable.head)
            case _ => ()

        // Blur fires even without a pick: the validation layer's Blur trigger. Reads the
        // bound ref LIVE, so it reports the selection at blur time.
        val fireBlur: UI.Action = onBlurF match
            case Present(f) => selectedRef match
                    case Present(r) => r.read(f)
                    case Absent     => f(sel)
            case Absent => ()

        // Leaving drops the highlight: a row left highlighted on a list that no longer holds
        // the keyboard claims a position the next arrow key would not honour anyway.
        val leaveList: UI.Action = hiRef match
            case Present(ref) => ref.set(-1).andThen(fireBlur)
            case Absent       => fireBlur

        val rows: List[UI] = OptionItem.rows(shownGroups, "p-listbox-option-group")((it, i) =>
            renderOption(it, sel, i, i == focused, hiRef, idBase)
        )
        val emptyRow: List[UI] =
            if shown.isEmpty then
                EmptyContent.whenSet(emptyContentV)(c =>
                    li.cssClass("p-listbox-empty-message").role("presentation")(c)
                )
            else Nil

        var list = ul.cssClass("p-listbox-list").role("listbox")
        // The id goes on the list, the listbox that holds the one tab stop, so a `Label.forId` and
        // the form's focus-first-invalid reach it; the options derive theirs from the same value.
        idV.foreach(v => list = list.id(v))
        if selectionModeV == SelectionMode.Multiple || selectionModeV == SelectionMode.Checkbox then
            list = list.aria("multiselectable", "true")
        // The listbox is ONE tab stop and roves a highlight inside it, which is what the ARIA
        // listbox pattern asks for and what Prime does: a reader tabbing through a form should
        // not have to press Tab once per option to get past a list of forty. That holds in the
        // page as first rendered too, before the mount has a highlight to rove: the id is on the
        // list, so a `Label.forId` or a focus-by-id has to find the tab stop there, and focus the
        // reader puts on the list then stays on it once the client attaches.
        // Focus and blur sit on the LIST, not on the root: a Focus/Blur event fires only on the
        // element it targets, and the element that takes focus here is the list. Declared on the
        // root they would never run, which is how `onBlur` came to be silently dead.
        if interactive then
            list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler).onFocus(seedFocus).onBlur(leaveList)
            if focused >= 0 then idBase.foreach(base => list = list.aria("activedescendant", optionId(base, focused)))
        accessibleNameV match
            case Present(TextValue.Const(v)) => list = list.aria("label", v)
            case Present(TextValue.Dyn(s))   => list = list.aria("label", s)
            case Absent                      => ()
        end match
        accessibleNameRefV.foreach(v => list = list.aria("labelledby", v))
        val listUI: UI = list((rows ++ emptyRow).map(toChild)*)

        def header: UI =
            var field = input
                .cssClass("p-listbox-filter")
                .cssClass("p-inputtext")
                .cssClass("p-component")
                .role("searchbox")
                .aria("label", "Filter")
            // Absent only in the static projection of a listbox that owns its query, before
            // the mount allocates it: the header still renders, it just does not filter yet.
            qRef.foreach(r => field = field.value(r))
            div.cssClass("p-listbox-header")(toChild(field))
        end header

        // A signal switch shows and hides the header in a region of its own, so the list beside it stays put.
        val headerSlot: List[UI] =
            filterOn match
                case BoolValue.Dyn(on)     => List(on.render(isOn => if isOn then header else UI.empty))
                case BoolValue.Const(true) => List(header)
                case _                     => Nil

        var root = div.cssClass("p-listbox").cssClass("p-component")
        if disabledFlag then root = root.cssClass("p-disabled")
        root = validity.mark(root)
        root((headerSlot :+ (div.cssClass("p-listbox-list-container")(toChild(listUI)): UI)).map(toChild)*)
    end body

    /** The id the roving highlight addresses one option by, derived from the listbox's own. */
    private def optionId(base: String, index: Int): String = s"$base-option-$index"

    private def renderOption(
        it: ListItem[K],
        sel: Set[K],
        index: Int,
        focused: Boolean,
        hiRef: Maybe[SignalRef[Int]],
        idBase: Maybe[String]
    )(using Frame): UI =
        val isSel = sel.contains(it.id)

        // Prime's checkmark column: the check glyph on selected rows, a blank
        // 1rem placeholder (sized by .p-icon) on the rest.
        val checkSlot: List[UI] =
            if checkmarkFlag then
                if isSel then List(GlyphSvg(Icons.check, "p-listbox-option-check-icon", "p-icon"))
                else List(span.cssClass("p-listbox-option-blank-icon").cssClass("p-icon").aria("hidden", "true"))
            else Nil
        val iconSlot: List[UI]  = it.icon.toList.map(g => GlyphSvg(g, "p-uic-option-icon"))
        val extraSlot: List[UI] =
            it.additionalText.toList.map(t => span.cssClass("p-uic-option-extra")(t))

        var row = li.cssClass("p-listbox-option").role("option").aria("selected", isSel.toString)
        if isSel then row = row.cssClass("p-listbox-option-selected")
        if focused then
            row = row.cssClass("p-focus").scrollAuto(true)
            idBase.foreach(base => row = row.id(optionId(base, index)))
        it.tooltip.foreach(t => row = row.jsProp("title", t))
        // A row carries NO tabindex. `tabindex="-1"` keeps a row out of the Tab order but leaves
        // it click-focusable, so a click parked the real focus on one row; the next key press
        // promoted it to :focus-visible and the browser drew its own ring there, which then sat
        // still while the highlight moved on. With no tabindex the click lands on the list, which
        // is what holds focus anyway. The click also seeds the highlight where there is one, so
        // the next arrow key continues from the row the reader just picked rather than from the top.
        if interactive then
            row = row.onClick { (e: MouseEvent) =>
                val pick: UI.Action = activate(it.id, e.modifiers.meta || e.modifiers.ctrl)
                val eff: UI.Action  = hiRef match
                    case Present(ref) => ref.set(index).andThen(pick)
                    case Absent       => pick
                eff
            }
        end if
        val labelSlot: UI = templateF match
            case Present(f) => f(it)
            case Absent     =>
                it.text match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => signalStringToUI(s)
        row(((checkSlot ++ iconSlot :+ labelSlot) ++ extraSlot).map(toChild)*)
    end renderOption

    /** Clicking a row updates the bound selection set (per the mode), then fires `onItemClick`. */
    private def activate(id: K, meta: Boolean = false)(using Frame): UI.Action =
        val setSelection: UI.Action = selectedRef match
            case Present(ref) if selectionModeV != SelectionMode.None =>
                ref.update(cur => SelectionPick.next(selectionModeV, metaKeyFlag, meta, id, cur))
            case _ => ()
        val fireClick: UI.Action = onItemClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end activate

end Listbox

object Listbox:
    def apply[K](): Listbox[K] = new Listbox[K]()

    /** One chord a host claims, carrying the effect it answers with.
      *
      * A bare `Maybe[UI.Action]` cannot say this: the value type is `Any`, so the compiler has to
      * assume the effect could itself be an `Absent` and the match over it is not exhaustive.
      */
    final private[uic] case class HostKey(effect: UI.Action)
end Listbox
