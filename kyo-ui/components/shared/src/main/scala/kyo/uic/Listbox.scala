package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One option of a [[Listbox]] — a hand-authored carrier (not a `kyo.UI`): a
  * `text` label, a stable `id` used as the selection key and the `onItemClick`
  * payload, an optional leading `icon` glyph (kyo extension —
  * `.p-uic-option-icon`), an optional right-aligned `additionalText` (kyo
  * extension — `.p-uic-option-extra`), and a native `tooltip` (title attribute).
  */
final case class ListItem(
    text: TextValue,
    id: String,
    icon: Maybe[IconGlyph] = Absent,
    additionalText: Maybe[String] = Absent,
    tooltip: Maybe[String] = Absent
)

object ListItem:
    /** Ergonomic public constructor with a constant label — the primary `text: TextValue` field is
      * package-private, so this is how callers outside `kyo.uic` build an item that also carries an
      * `additionalText` badge or a `tooltip` (the [[Listbox.item]] builders cover only text+icon).
      * A distinct name (not an `apply` overload) because Scala 3 forbids two `apply` variants that both
      * carry default arguments.
      */
    def of(
        text: String,
        id: String,
        icon: Maybe[IconGlyph] = Absent,
        additionalText: Maybe[String] = Absent,
        tooltip: Maybe[String] = Absent
    ): ListItem = ListItem(TextValue.Const(text), id, icon, additionalText, tooltip)
end ListItem

/** Listbox — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Listbox anatomy: `div.p-listbox.p-component` > `div.p-listbox-list-container`
  * > `ul.p-listbox-list[role=listbox]` of `li.p-listbox-option[role=option]`
  * rows, with `.p-listbox-option-selected` on picked rows and an optional
  * `li.p-listbox-empty-message` when there is nothing to show), so the
  * extracted `@primeuix` listbox CSS applies verbatim.
  *
  * Selection binds two-way to a `SignalRef[Set[String]]`: the list re-renders
  * reactively so each row's `aria-selected` tracks the set, and clicking a row
  * (when a [[SelectionMode]] other than `None` is set) writes the updated set
  * back before firing `onItemClick`. `checkmark(true)` renders Prime's per-row
  * check-icon column.
  *
  * Filtering has the two shapes shared by every picker in this module:
  * `filterable(true)` renders Prime's header filter over a query the listbox
  * allocates itself (the same word and the same meaning as
  * [[Select.filterable]] / [[MultiSelect.filterable]]), and `filterQuery(ref)`
  * renders it over a query the app owns. Either way the options are filtered by
  * a case-insensitive contains at render time.
  */
final case class Listbox private (
    items: List[ListItem] = Nil,
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    filterQueryRef: Maybe[SignalRef[String]] = Absent,
    filterableFlag: Boolean = false,
    onItemClickF: Maybe[String => Any < Async] = Absent,
    templateF: Maybe[ListItem => UI] = Absent,
    checkmarkFlag: Boolean = false,
    disabledFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    emptyMessageV: Maybe[TextValue] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[String] => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, MultiSelectFormControl:
    type Self = Listbox

    /** Native `id` on the option list — pair with `Label.forId`; the form layer stamps
      * the bound field's id here so focus-first-invalid can target it.
      */
    def id(v: String): Listbox = copy(idV = Present(v))

    /** Appends the given options. */
    def items(is: ListItem*): Listbox = copy(items = items ++ is.toList)

    /** Appends a single option (ergonomic builder — no need to construct [[ListItem]]). */
    def item(text: String, id: String, icon: Maybe[IconGlyph] = Absent): Listbox =
        copy(items = items :+ ListItem(TextValue.Const(text), id, icon))

    /** Reactive variant of [[item]] — the option label re-renders in place on signal
      * emission; `id` stays the stable selection key.
      */
    def item(text: Signal[String], id: String, icon: Maybe[IconGlyph]): Listbox =
        copy(items = items :+ ListItem(TextValue.Dyn(text), id, icon))

    /** Selection semantics: `None` (default — rows inert unless `onItemClick` is
      * set), `Single` (picking replaces the selection; re-picking clears it), or
      * `Multiple` (picking toggles the row in the set).
      */
    def selectionMode(v: SelectionMode): Listbox = copy(selectionModeV = v)

    /** Binds selection two-way to `ref`: clicks update the set, ref changes re-render
      * `aria-selected`. Spelled `value` like every other picker's bound selection — a
      * Listbox is the always-visible member of that family, not a different concept.
      */
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): Listbox = copy(selectedRef = Present(ref))

    /** Renders Prime's header filter input (`div.p-listbox-header` >
      * `input.p-listbox-filter.p-inputtext`) over a query the listbox allocates
      * itself — the same word, and the same on/off meaning, as
      * [[Select.filterable]] and [[MultiSelect.filterable]]. Use [[filterQuery]]
      * when the app needs to read or write the query.
      */
    def filterable(v: Boolean): Listbox = copy(filterableFlag = v)

    /** Renders the same header filter, bound two-way to `ref`: typing writes the
      * query, external writes re-filter the options (a case-insensitive contains
      * on the label).
      */
    def filterQuery(ref: SignalRef[String]): Listbox = copy(filterQueryRef = Present(ref))

    def onItemClick(f: String => Any < Async): Listbox = copy(onItemClickF = Present(f))

    /** Custom option content (Prime's `#option` slot): renders instead of the plain
      * text label; the checkmark column and leading icon keep their places.
      */
    def itemTemplate(f: ListItem => UI): Listbox = copy(templateF = Present(f))

    /** Renders Prime's option check-icon column: the check glyph on selected rows
      * (`.p-listbox-option-check-icon`), a blank placeholder on the rest.
      */
    def checkmark(v: Boolean): Listbox = copy(checkmarkFlag = v)

    /** Disables the whole listbox (`.p-disabled`): rows keep their look but stop
      * responding to clicks.
      */
    def disabled(v: Boolean): Listbox = copy(disabledFlag = v)

    /** Marks the listbox invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Listbox = copy(invalidV = Present(BoolValue.Const(v)))

    /** Reactive validity: the bound signal toggles the invalid state on emission. */
    def invalid(sig: Signal[Boolean]): Listbox = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Message rendered below the list while it is invalid (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): Listbox = copy(invalidMsgV = Present(v))

    /** Reactive invalid message — `Present` shows the row and (by default) marks the list
      * invalid; `Absent` clears both.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Listbox = copy(invalidMsgDynV = Present(sig))

    /** Fires on focus loss with the current selection — the validation layer's Blur
      * trigger.
      */
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): Listbox = copy(onBlurF = Present(f))

    /** Text shown as the `li.p-listbox-empty-message` row when no options render. */
    def emptyMessage(v: String): Listbox = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): Listbox = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    /** `aria-label` for the listbox. */
    def accessibleName(v: String): Listbox = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Listbox = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** `aria-labelledby` id reference for the listbox. */
    def accessibleNameRef(v: String): Listbox = copy(accessibleNameRefV = Present(v))

    /** Whether rows respond to clicks at all (selecting or an explicit item handler). */
    private def interactive: Boolean =
        !disabledFlag && (selectionModeV != SelectionMode.None || onItemClickF.isDefined)

    /** Package-internal: hosts (OrderList/PickList) that already subscribe to the
      * same selection ref in an enclosing render supply the resolved set directly —
      * a second nested subscription to the same ref would race the inner update
      * against the outer replace (the Overlay renderOpen lesson). The click
      * handlers still write through the BOUND refs.
      */
    private[uic] def resolved(sel: Set[String], query: String)(using Frame): UI =
        body(sel, query, Absent)

    private[uic] def render(using Frame): UI =
        // The validity boundary sits OUTSIDE the query subscriptions but resolves before
        // the `filterable` mount, so an emission never re-mounts and drops the query.
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderQuery
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderQuery
                )

    private def renderQuery(using Frame): UI =
        filterQueryRef match
            case Present(_)               => subscribed(filterQueryRef)
            case Absent if filterableFlag =>
                // No app-owned query, but the header is asked for: the query lives in a
                // signal this mount allocates (Select's pattern). The static projection
                // renders the same header, inert, until the transport attaches.
                UI.mounted(Signal.initRef("").map(q => subscribed(Present(q))))
                    .placeholder(subscribed(Absent))
            case Absent => subscribed(Absent)

    /** The selection/query subscriptions around one [[body]] render. */
    private def subscribed(qRef: Maybe[SignalRef[String]])(using Frame): UI =
        (selectedRef, qRef) match
            case (Present(s), Present(f)) => s.render(sel => f.render(q => body(sel, q, qRef)))
            case (Present(s), Absent)     => s.render(sel => body(sel, "", Absent))
            case (Absent, Present(f))     => f.render(q => body(Set.empty, q, qRef))
            case _                        => body(Set.empty, "", Absent)

    private def body(sel: Set[String], query: String, qRef: Maybe[SignalRef[String]])(using Frame): UI =
        val shown =
            if query.isEmpty then items
            else
                items.filter { it =>
                    it.text match
                        case TextValue.Const(t) => t.toLowerCase.contains(query.toLowerCase)
                        case TextValue.Dyn(_)   => true
                }

        val rows: List[UI] = shown.map(renderOption(_, sel))
        val emptyRow: List[UI] =
            if shown.isEmpty then
                emptyMessageV.toList.map {
                    case TextValue.Const(t) => li.cssClass("p-listbox-empty-message").role("presentation")(t): UI
                    case TextValue.Dyn(s)   => s.render(t => li.cssClass("p-listbox-empty-message").role("presentation")(t))
                }
            else Nil

        var list = ul.cssClass("p-listbox-list").role("listbox")
        if selectionModeV == SelectionMode.Multiple || selectionModeV == SelectionMode.Checkbox then
            list = list.aria("multiselectable", "true")
        accessibleNameV match
            case Present(TextValue.Const(v)) => list = list.aria("label", v)
            case Present(TextValue.Dyn(s))   => list = list.aria("label", s)
            case Absent                      => ()
        end match
        accessibleNameRefV.foreach(v => list = list.aria("labelledby", v))
        val listUI: UI = list((rows ++ emptyRow).map(toChild)*)

        val headerSlot: List[UI] =
            if !filterableFlag && filterQueryRef.isEmpty then Nil
            else
                var field = input
                    .cssClass("p-listbox-filter")
                    .cssClass("p-inputtext")
                    .cssClass("p-component")
                    .role("searchbox")
                    .aria("label", "Filter")
                // Absent only in the static projection of a `filterable` listbox, before
                // the mount allocates the query: the header still renders, it just does
                // not filter yet.
                qRef.foreach(r => field = field.value(r))
                List(div.cssClass("p-listbox-header")(toChild(field)))

        var root = div.cssClass("p-listbox").cssClass("p-component")
        idV.foreach(v => root = root.id(v))
        if disabledFlag then root = root.cssClass("p-disabled")
        if invalidV.constTrue then root = root.cssClass("p-invalid").aria("invalid", "true")
        // Blur fires even without a pick — the validation layer's Blur trigger. Reads the
        // bound ref LIVE, so it reports the selection at blur time.
        onBlurF.foreach { f =>
            root = root.onBlur(selectedRef match
                case Present(r) => r.use(f)
                case Absent     => f(sel))
        }
        FieldInvalid.withMessage(
            root((headerSlot :+ (div.cssClass("p-listbox-list-container")(toChild(listUI)): UI)).map(toChild)*),
            invalidV.constTrue,
            invalidMsgV
        )
    end body

    private def renderOption(it: ListItem, sel: Set[String])(using Frame): UI =
        val isSel = sel.contains(it.id)

        // Prime's checkmark column: the check glyph on selected rows, a blank
        // 1rem placeholder (sized by .p-icon) on the rest.
        val checkSlot: List[UI] =
            if checkmarkFlag then
                if isSel then List(GlyphSvg(Icons.check, "p-listbox-option-check-icon", "p-icon"))
                else List(span.cssClass("p-listbox-option-blank-icon").cssClass("p-icon").aria("hidden", "true"))
            else Nil
        val iconSlot: List[UI] = it.icon.toList.map(g => GlyphSvg(g, "p-uic-option-icon"))
        val extraSlot: List[UI] =
            it.additionalText.toList.map(t => span.cssClass("p-uic-option-extra")(t))

        var row = li.cssClass("p-listbox-option").role("option").aria("selected", isSel.toString)
        if isSel then row = row.cssClass("p-listbox-option-selected")
        it.tooltip.foreach(t => row = row.jsProp("title", t))
        if interactive then
            row = row
                .tabIndex(0)
                .onClick(activate(it.id))
                .onKeyDown { e =>
                    e.key match
                        case Keyboard.Enter | Keyboard.Space => activate(it.id)
                        case _                               => ()
                }
        end if
        val labelSlot: UI = templateF match
            case Present(f) => f(it)
            case Absent =>
                it.text match
                    case TextValue.Const(t) => stringToUI(t)
                    case TextValue.Dyn(s)   => s.render(t => stringToUI(t))
        row(((checkSlot ++ iconSlot :+ labelSlot) ++ extraSlot).map(toChild)*)
    end renderOption

    /** Clicking a row updates the bound selection set (per the mode), then fires `onItemClick`. */
    private def activate(id: String)(using Frame): Any < Async =
        val setSelection: Any < Async = selectedRef match
            case Present(ref) if selectionModeV != SelectionMode.None =>
                ref.getAndUpdate(cur => nextSelection(id, cur))
            case _ => ()
        val fireClick: Any < Async = onItemClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end activate

    /** The selection set after activating `id`, honouring the current mode
      * (`Radio` follows single-, `Checkbox` multi-select semantics).
      */
    private def nextSelection(id: String, current: Set[String]): Set[String] =
        selectionModeV match
            case SelectionMode.None => current
            case SelectionMode.Single | SelectionMode.Radio =>
                if current == Set(id) then Set.empty else Set(id)
            case SelectionMode.Multiple | SelectionMode.Checkbox =>
                if current.contains(id) then current - id else current + id
end Listbox

object Listbox:
    def apply(): Listbox = new Listbox()
