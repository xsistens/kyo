package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** One entry of a [[CascadeSelect]] option tree: the typed nested analogue of
  * Select's flat options: a `Leaf` carries a pickable value `A` (projected to
  * text by the component's label function), a `Group` carries its own label and
  * nested children (opening a side sub-panel).
  */
enum CascadeItem[A] derives CanEqual:
    case Leaf(value: A)
    case Group(label: String, children: List[CascadeItem[A]])

object CascadeItem:
    /** A pickable leaf option. */
    def leaf[A](value: A): CascadeItem[A] = CascadeItem.Leaf(value)

    /** A group labelled `label` opening a nested sub-panel of `children`. */
    def group[A](label: String)(children: CascadeItem[A]*): CascadeItem[A] =
        CascadeItem.Group(label, children.toList)

    /** A cascade's options nest the way a menu's items do, so [[MenuNav]] navigates them: a
      * group's children are its own, a leaf has none, and nothing is skipped, since a cascade
      * carries neither separators nor disabled rows.
      */
    private[uic] given navNodes[A]: MenuNav.Nodes[CascadeItem[A]] with
        def children(n: CascadeItem[A]): List[CascadeItem[A]] = n match
            case CascadeItem.Leaf(_)      => Nil
            case CascadeItem.Group(_, cs) => cs
        def skip(n: CascadeItem[A]): Boolean = false
    end navNodes
end CascadeItem

/** CascadeSelect: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * CascadeSelect anatomy: `div.p-cascadeselect.p-component.p-inputwrapper
  * [.p-invalid][.p-disabled][.p-cascadeselect-sm|-lg][.p-variant-filled]
  * [.p-cascadeselect-fluid][.p-cascadeselect-open]` > `span.p-cascadeselect-label`
  * + `div.p-cascadeselect-dropdown` with the chevron, floating
  * `div.p-cascadeselect-overlay` panel > `div.p-cascadeselect-list-container` >
  * `ul.p-cascadeselect-list[role=tree]` of `li.p-cascadeselect-option` rows;
  * groups (`.p-cascadeselect-option-group`, trailing `.p-cascadeselect-group-icon`)
  * open SIDE-NESTED sub-panels stamped `.p-cascadeselect-overlay
  * .p-cascadeselect-option-list`), so the extracted `@primeuix` cascadeselect
  * CSS applies verbatim.
  *
  * Keyboard: a cascade IS a vertical menu, so the menu family's [[MenuNav]] state machine
  * runs it over [[CascadeItem]] through that type's own [[MenuNav.Nodes]]. ArrowDown/ArrowUp
  * move Prime's `.p-focus` highlight within a panel and cycle at its ends, ArrowRight opens
  * the focused group and steps into it, ArrowLeft closes back out to the parent row, Enter or
  * Space picks a leaf or opens a group, Home/End reach the ends of the current panel, and
  * Escape leaves one level or closes the field. The handler rides on the ROOT panel, which is
  * what holds focus on open and what every nested panel's keydown bubbles up to.
  *
  * Options are a TYPED tree of [[CascadeItem]]s: `options(items)(label)`
  * projects leaf values to text, groups carry their own label; `optionKey`
  * (default: the label projection) supplies the stable key leaves are matched
  * by; `selection(ref)` binds the chosen leaf itself (over `A`, or over
  * `Maybe[A]`). Clicking a group row opens exactly its chain (the Menubar
  * `openExactly` machinery: sibling branches close); clicking a LEAF writes
  * it, fires `onChange`, and closes the whole chain. `loading` says the tree is
  * still arriving; without it a held leaf the tree does not carry is named in a
  * `.p-uic-diagnostic` card. Every panel is a real
  * [[Overlay]]: an outside click closes ONE level (the topmost backdrop catches
  * it), Escape closes ONE level too (each panel consumes its own keydown,
  * the Overlay per-level story), and no panel seeds focus: it stays on the trigger, which is the
  * combobox, wears the whole cascade keyboard, and announces the highlight wherever in the chain it
  * has walked to. Group opening is click-driven (a
  * deliberate deviation from Prime's hover-open, since a server round-trip per hover
  * is heavy; the sheet's hover styling is untouched).
  *
  * DEVIATION: a nested sub-panel is a `div` Overlay panel holding a real
  * `ul.p-cascadeselect-list` (Prime renders the nested `ul` itself as the
  * panel): the panel div carries the `.p-cascadeselect-overlay` skin, the
  * inner `ul` Prime's list padding, so the visible box computes identically.
  *
  * Honest deferrals: the mobile query mode, option templates, and showClear.
  *
  * `id` lands on the field root, the combobox that holds the tab stop, and the chain derives `$id-list` and the
  * travelling highlight `$id-active` from it (a minted base stands in where none is given).
  */
final class CascadeSelect[A] private[uic] (private[uic] val p: CascadeSelect.Props[A]) extends Node, ChoiceFormControl[A], HasTooltip,
      HasPlaceholder, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = CascadeSelect[A]

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new CascadeSelect(p.copy(access = v))

    /** Appends typed option-tree roots with the leaf text projection: a collection, or a signal of roots for a tree
      * that changes while the field is shown, read in the field's own region.
      */
    def options(is: Iterable[CascadeItem[A]] | Signal[Seq[CascadeItem[A]]])(label: A => String): CascadeSelect[A] =
        val more: List[LiveEntry[CascadeItem[A], Unit]] = is match
            case sig: Signal[?] => List(sig.asInstanceOf[Signal[Seq[CascadeItem[A]]]])
            case roots          => roots.asInstanceOf[Iterable[CascadeItem[A]]].toList
        new CascadeSelect(p.copy(itemSources = itemSources ++ more, labelF = label))
    end options

    /** Stable per-leaf key: the value written into the bound ref (defaults to
      * the label projection).
      */
    def optionKey(f: A => String): CascadeSelect[A] = new CascadeSelect(p.copy(keyF = Present(f)))

    /** Binds the selection to the leaf itself: the field shows the leaf whose [[optionKey]] the value carries, and a pick
      * writes the leaf. A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      * With neither event set, a card says the reader cannot change it.
      */
    def selection(v: A | Signal[A]): CascadeSelect[A] = new CascadeSelect(p.copy(choice = Present(SingleChoice.Value(ReactiveValue(v)))))

    /** Binds the selection to the leaf or none: `Absent` shows the placeholder, a pick writes `Present(leaf)`. Two-way and
      * one-way as for the leaf binding above. `@targetName` because both `selection` bindings erase to one signature.
      */
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): CascadeSelect[A] =
        new CascadeSelect(p.copy(choice = Present(SingleChoice.Optional(ReactiveValue(v)))))

    /** Binds the panel visibility. A `SignalRef` binds two way: the trigger, a pick that closes, Escape, Tab and an
      * outside click write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the panel
      * follows it and the field only reports. A constant nobody hears is fixed: `false` draws the field read-only;
      * `true` keeps the panel open with nothing that closes it (no backdrop, no click on the field), while the field stays
      * the tab stop with the cascade keyboard and a pick is still the reader's. The combobox has no readonly state for its
      * list, so that says nothing beyond `aria-expanded`. Unbound, the field keeps the state itself, starting closed. A
      * signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): CascadeSelect[A] = new CascadeSelect(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the panel visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): CascadeSelect[A] = new CascadeSelect(p.copy(openSlot = openSlot.hear(f)))

    /** Asks for the panel open or closed: a two-way binding is written, then the change reported. */
    private def askOpen(open: Slot[Boolean], v: Boolean)(using Frame): UI.Action = open.change(v)

    /** A panel bound closed by a constant nobody hears: nothing the reader does may open it. */
    private def openFixedShut: Boolean = openSlot.fixedAt(false)

    /** A panel held open by a constant nobody hears: nothing the reader does may close it. */
    private def openFixedOpen: Boolean = openSlot.fixedAt(true)

    private[uic] def withPlaceholder(v: Maybe[TextValue]): CascadeSelect[A] = new CascadeSelect(p.copy(placeholderV = v))

    def disabled(v: Boolean): CascadeSelect[A] = new CascadeSelect(p.copy(disabledFlag = v))

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * current leaf key.
      */
    def name(v: String): CascadeSelect[A] = new CascadeSelect(p.copy(nameV = Present(v)))

    private[uic] def withTooltip(v: Maybe[Prop[String]]): CascadeSelect[A] = new CascadeSelect(p.copy(tooltipV = v))

    /** Size: `.p-cascadeselect-sm` / default / `.p-cascadeselect-lg`. */
    def size(v: Size): CascadeSelect[A] = new CascadeSelect(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CascadeSelect[A] = new CascadeSelect(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-cascadeselect-fluid`). */
    def fluid(v: Boolean): CascadeSelect[A] = new CascadeSelect(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new CascadeSelect(p.copy(validityV = v))

    /** Fired with the leaf now chosen, after the write. A pick of the leaf already chosen changes nothing and fires nothing.
      * Fires before [[onPick]].
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): CascadeSelect[A] = new CascadeSelect(p.copy(onChangeF = Present(Callback.of(f))))

    /** Fired with the leaf picked, after a pick that changed what the field holds: [[onChange]] without the `Maybe`, for a
      * field that is never empty or a one-way [[selection]] that writes its own state here. Fires after [[onChange]].
      */
    def onPick(f: A => UI.Action)(using Frame): CascadeSelect[A] = new CascadeSelect(p.copy(onPickF = Present(Callback.of(f))))

    /** Fires on focus loss (native `blur`) with the leaf held (`Absent` for none). Unlike `onChange` it fires even when
      * nothing was changed. The form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): CascadeSelect[A] = new CascadeSelect(p.copy(onBlurF = Present(Callback.of(f))))

    /** Says the option tree is still arriving: the dropdown icon turns into Prime's spinner, and a held leaf no leaf of the
      * tree carries yet is shown from the value itself rather than reported in a `.p-uic-diagnostic` card.
      */
    def loading(v: Boolean | Signal[Boolean]): CascadeSelect[A] = new CascadeSelect(p.copy(loadingV = Present(ReactiveValue(v))))

    /** The stable leaf key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    // Lets the one region skip a write that holds what it already showed; leaves are still matched by key.
    private given CanEqual[A, A] = CanEqual.derived

    /** Every leaf value of the tree, in tree order: the options a typed binding writes back. */
    private def leaves: List[A] =
        def loop(is: List[CascadeItem[A]]): List[A] =
            is.flatMap {
                case CascadeItem.Leaf(a)            => List(a)
                case CascadeItem.Group(_, children) => loop(children)
            }
        loop(items)
    end leaves

    private[uic] def render(using Frame): UI =
        // A panel bound one way that nothing hears: every open and close the reader asks for is painted away again.
        Diagnostics.before(renderLive)("CascadeSelect")(Diagnostic.unheard(openSlot, CascadeSelect.openName, on = !disabledFlag))
    end render

    private def renderLive(using Frame): UI =
        // One open/closed signal for the root panel plus the open groups, allocated
        // by this mount, which draws in the render pass.
        val validity = FieldValidity(validityV)
        UI.mounted {
            for
                // Unbound, the field keeps the panel state itself, starting closed.
                open  <- openSlot.owned(false)
                subs  <- Submenus.init()
                focus <- Anchored.init(List.empty[Int])
                base  <- access.id.map(v => Kyo.lift(v)).getOrElse(UI.id)
                // Unbound, the field keeps the leaf the reader picks itself.
                own <-
                    if choice.isEmpty then Signal.initRef(Maybe.empty[A]).map(r => Present(r))
                    else Kyo.lift(Maybe.empty[SignalRef[Maybe[A]]])
                self = own.fold(this)(r => new CascadeSelect(p.copy(choice = Present(SingleChoice.Optional(ReactiveValue(r))))))
            yield new CascadeSelect(self.p.copy(openSlot = open)).wiredOver(subs, focus, Present(base))
        }
    end renderLive

    /** A constant nobody hears is fixed: the field is drawn read-only, reachable and readable with no panel to open. */
    private def fixed: Boolean = openFixedShut || choice.exists(_.fixed(onChangeF.orElse(onPickF)))

    /** The subscription tree the mount publishes (golden-test seam): ONE reactive
      * region over the root ref and the open groups (see [[Submenus]]).
      */
    private[uic] def wired(
        open: SignalRef[Boolean],
        subs: Submenus,
        focus: Anchored[List[Int]],
        base: Maybe[String] = Absent
    )(using Frame): UI =
        new CascadeSelect(p.copy(openSlot = openSlot.bind(open))).wiredOver(subs, focus, base)
    end wired

    /** [[wired]] over the panel state however it is bound. */
    private def wiredOver(
        subs: Submenus,
        focus: Anchored[List[Int]],
        base: Maybe[String]
    )(using Frame): UI =
        val validity = FieldValidity(validityV)
        val panel    = openSlot.reading(false).and(subs.reading).and(focus.reading)
        panel.and(shown).and(roots).render { case ((((o, openSet), f), (held, ld, ph)), rs) =>
            // The open groups and the highlight follow their root options over the roots shown ([[Anchored]]).
            val sv = subs.view(openSet, rs)
            val fv = focus.view(f, rs)
            val st = CascadeSelect.Wired(openSlot, sv, fv, fv.value, base)
            new CascadeSelect(p.copy(items = rs)).body(held, o, sv.open, st, ph, ld, validity)
        }
    end wiredOver

    /** The roots as ONE reading: fixed while none of them came as a signal. */
    private def roots(using Frame): ReactiveValue[List[CascadeItem[A]]] = Entry.reading((), itemSources)

    /** What the field shows whatever its panels do: the held leaf, whether the options are still loading, and the
      * placeholder. Read beside the panels' state as ONE region (see [[ReactiveValue.and]]); the validity is stamped on the field,
      * never read here.
      */
    private def shown(using Frame): ReactiveValue[(Maybe[A], Boolean, Maybe[String])] =
        val held: ReactiveValue[Maybe[A]] = choice.fold(ReactiveValue.Const(Maybe.empty[A]))(_.reading)
        held.and(ReactiveValue.optional(loadingV)).and(ReactiveValue.optional(placeholderV)).map { case ((h, ld), ph) =>
            (h, ld.getOrElse(false), ph)
        }
    end shown

    private def body(
        held: Maybe[A],
        isOpen: Boolean,
        openMap: Set[List[Int]],
        st: CascadeSelect.Wired,
        placeholder: Maybe[String],
        loading: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        val current = held.fold("")(key)
        // A held leaf no leaf of the tree carries, once the tree is said to be complete.
        val strays: Seq[String] =
            held match
                case Present(a) if !leaves.exists(l => key(l) == current) && !loading => Seq(labelF(a))
                case _                                                                => Nil

        def closeAll: UI.Action =
            for
                _ <- st.submenus.openExactly(Absent)
                _ <- st.focusRef.set(Nil)
                _ <- askOpen(st.open, false)
            yield ()

        // Opens with every group closed and the highlight on the first root option. Not on the
        // selected one: the chain opens closed, so a leaf chosen three levels down has no row on
        // the screen to land on.
        def openPanel: UI.Action =
            for
                _ <- st.submenus.openExactly(Absent)
                _ <- st.focusRef.set(MenuNav.firstFocus(items).getOrElse(Nil))
                _ <- askOpen(st.open, true)
            yield ()

        def toggleRoot: UI.Action =
            if isOpen then closeAll else openPanel

        // Writes the picked leaf, fires onChange and onPick where that changed it, closes the whole chain.
        def pick(a: A): UI.Action =
            val changed: Maybe[Maybe[A]] < Sync =
                choice match
                    case Present(c) => c.write(Present(a), key)
                    case Absent     => Present(Present(a))
            val fire: UI.Action = changed.map(SingleChoice.report(_, onChangeF, onPickF))
            fire.andThen(closeAll)
        end pick

        // === closed trigger ======================================================
        // The trigger shows the leaf the held value matches, and the value itself where the tree does not carry it yet.
        val selText         = Maybe.fromOption(leaves.find(l => key(l) == current)).orElse(held).fold("")(labelF)
        val showPlaceholder = held.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selText

        var lbl = span.cssClass("p-cascadeselect-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if labelText.isEmpty then lbl = lbl.cssClass("p-cascadeselect-label-empty")
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        val dd             = div.cssClass("p-cascadeselect-dropdown")
        val dropdownUI: UI =
            if loading then dd(GlyphSvg(Icons.spinner, "p-cascadeselect-loading-icon", "p-icon", "p-icon-spin"))
            else dd(GlyphSvg(Icons.chevronDown, "p-cascadeselect-dropdown-icon", "p-icon"))

        val hiddenCarrier: List[UI] =
            nameV.toList.map(n => hiddenInput.jsProp("name", n).value(current))

        /** The whole chain's keyboard, one [[MenuNav]] walk over the option tree.
          *
          * A cascade IS a vertical menu: Down and Up move within a panel, Right opens the
          * focused group and steps into it, Left closes back out, Enter picks a leaf or opens a
          * group, and Escape leaves one level or closes the field. So the menu family's state
          * machine runs it, over [[CascadeItem]] through its own [[MenuNav.Nodes]].
          */
        def panelKey(state: CascadeSelect.Wired): KeyboardEvent => UI.Action = e =>
            // Tab carries the reader away from the element the whole chain's keyboard lives on, so
            // the chain goes with them rather than floating over the page unanswered.
            if e.key == Keyboard.Tab then closeAll
            else
                MenuNav.onKey(items, MenuNav.Orientation.Vertical, state.focus, e.key) match
                    case Present(step) =>
                        val reopen: UI.Action = step.open match
                            case MenuNav.OpenOp.Keep      => ()
                            case MenuNav.OpenOp.Close     => state.submenus.openExactly(Absent)
                            case MenuNav.OpenOp.OpenTo(p) => state.submenus.openExactly(Present(p))
                        val chosen: UI.Action =
                            if step.activate then
                                MenuNav.itemAt(items, step.focus) match
                                    case Present(CascadeItem.Leaf(a)) => pick(a)
                                    case _                            => ()
                            else ()
                        val shut: UI.Action = if step.dismiss then askOpen(state.open, false) else ()
                        state.focusRef.set(step.focus).andThen(reopen).andThen(chosen).andThen(shut)
                    case Absent => ()

        // === floating panel chain ================================================
        val panelUI: List[UI] = List(st).collect {
            case state if isOpen =>
                Overlay.hosted(state.open)
                    .animate(false)
                    .panelClass("p-cascadeselect-overlay")
                    .panelClass("p-component")
                    // No panel in the chain takes focus: it stays on the trigger, which is where
                    // the keyboard is and where the highlight is announced. A panel that seeded
                    // focus would take the announcement away from the one element reading it.
                    .seedFocus(false)
                    .dismissOnEscape(false)
                    // The chain renders inside the field, whose click toggles it: every click in the
                    // chain (a leaf, a group row, a nested panel) ends at this root panel.
                    .containClicks(true)(
                        div.cssClass("p-cascadeselect-list-container")(
                            list(items, Nil, current, openMap, state, pick)
                        )
                    )
                    .renderOpen
        }

        // === field root ==========================================================
        var el = div.cssClass("p-cascadeselect").cssClass("p-component").cssClass("p-inputwrapper")
        // On the root, the combobox that holds the tab stop, not on the label text inside it: see
        // [[HasElementId]]. The chain's parts derive from the same value.
        access.id.foreach(v => el = el.id(v))
        if held.isDefined then el = el.cssClass("p-inputwrapper-filled")
        if loading then el = el.aria("busy", "true")
        if isOpen then el = el.cssClass("p-cascadeselect-open").cssClass("p-uic-overlay-anchor")
        el = validity.mark(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small  => el = el.cssClass("p-cascadeselect-sm")
            case Size.Large  => el = el.cssClass("p-cascadeselect-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-cascadeselect-fluid")
        el = el.title(tooltipV)
        access.name.foreach(v => el = el.aria("label", v))
        access.nameRef.foreach(v => el = el.aria("labelledby", v))
        el = el.role("combobox").aria("haspopup", "tree").aria("expanded", isOpen.toString)
        st.idBase.foreach { b =>
            el = el.aria("controls", listId(b))
            // One travelling id, wherever in the nested chain the highlight has walked to, because
            // `aria-activedescendant` names exactly one element and the trigger is the one thing
            // holding focus.
            if st.focus.nonEmpty then el = el.aria("activedescendant", activeId(b))
        }
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            if fixed then el = el.aria("readonly", "true")
            // The one click listener: a click anywhere on the field toggles the chain.
            // A panel fixed open has no close for a click to ask for, so the field takes none.
            if !fixed && !openFixedOpen then el = el.onClick(toggleRoot)
            // Focus never leaves the trigger, however deep the chain of panels goes, so the whole
            // cascade keyboard lives here: closed, the three opening keys; open, one [[MenuNav]]
            // walk that steps into groups, back out of them, picks a leaf and closes on Escape.
            if !fixed then
                el = el.onKeyDown { e =>
                    if isOpen then panelKey(st)(e)
                    else
                        e.key match
                            case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel
                            case _                                                    => ()
                }
            end if
            // Focus-loss on the trigger reports the held leaf, the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(choice match
                    case Present(c) => c.read(f)
                    case Absent     => f(held))
            }
        end if

        val field = validity.follow(el(UI.children((labelUI :: dropdownUI :: hiddenCarrier) ++ panelUI)*))
        // A selection bound one way that nothing hears: every pick is painted away again. A disabled field takes no pick, so
        // it has nothing to say.
        Diagnostics.after(field)("CascadeSelect")(
            Diagnostic.unknown(strays),
            Diagnostic(!disabledFlag && choice.exists(_.deaf(onChangeF.orElse(onPickF))), CascadeSelect.selectionName.advice)
        )
    end body

    private def activeId(base: String): String = s"$base-active"

    /** The id of the ROOT option list, which is what the combobox points `aria-controls` at. */
    private def listId(base: String): String = s"$base-list"

    /** One panel level: `ul.p-cascadeselect-list` (role `tree` at the root,
      * `group` nested) of leaf and group rows; an OPEN group row anchors its
      * side-nested Overlay sub-panel (host-gated single-subscription form; the
      * wired chain already subscribes to every group ref).
      */
    private def list(
        its: List[CascadeItem[A]],
        path: List[Int],
        current: String,
        openMap: Set[List[Int]],
        st: CascadeSelect.Wired,
        pick: A => UI.Action
    )(using Frame): UI =
        val rows: List[UI] = its.zipWithIndex.map {
            case (CascadeItem.Leaf(a), i) =>
                val isSel = key(a) == current
                var row   = li.cssClass("p-cascadeselect-option").role("treeitem").aria("selected", isSel.toString)
                if isSel then row = row.cssClass("p-cascadeselect-option-selected")
                if st.focus == (path :+ i) then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    st.idBase.foreach(b => row = row.id(activeId(b)))
                var content = div.cssClass("p-cascadeselect-option-content")
                content = content.onClick(pick(a))
                row(content(span.cssClass("p-cascadeselect-option-text")(labelF(a))))

            case (CascadeItem.Group(label, children), i) =>
                val p                      = path :+ i
                val groupOpen              = openMap(p)
                def toggleGroup: UI.Action =
                    val target: Maybe[List[Int]] =
                        if groupOpen then (if p.size > 1 then Present(p.init) else Absent)
                        else Present(p)
                    st.submenus.openExactly(target)
                end toggleGroup
                var row = li
                    .cssClass("p-cascadeselect-option")
                    .cssClass("p-cascadeselect-option-group")
                    .cssClass("p-uic-overlay-anchor")
                    .role("treeitem")
                    .aria("expanded", groupOpen.toString)
                    .aria("level", (p.size).toString)
                if groupOpen then row = row.cssClass("p-cascadeselect-option-active")
                if st.focus == p then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    st.idBase.foreach(b => row = row.id(activeId(b)))
                var content = div.cssClass("p-cascadeselect-option-content")
                content = content.onClick(toggleGroup)
                val contentUI: UI = content(
                    span.cssClass("p-cascadeselect-option-text")(label),
                    span.cssClass("p-cascadeselect-group-icon-container")(
                        GlyphSvg(Icons.angleRight, "p-cascadeselect-group-icon")
                    )
                )
                // The side sub-panel: Prime's nested list skin on the Overlay primitive
                // (RightStart = the sheet's `inset-inline-start: 100%; inset-block-start: 0`).
                val panel: List[UI] =
                    if groupOpen then
                        List(
                            Overlay.gated(st.submenus.close(p))
                                .anchor(OverlayAnchor.RightStart)
                                .matchWidth(false)
                                .animate(false)
                                .panelClass("p-cascadeselect-overlay")
                                .panelClass("p-cascadeselect-option-list")
                                // No panel in the chain seeds focus, so none of them ever
                                // receives a key: the trigger keeps focus and wears the
                                // whole cascade keyboard, however deep the chain goes.
                                .seedFocus(false)
                                .dismissOnEscape(false)(
                                    list(children, p, current, openMap, st, pick)
                                )
                                .renderOpen
                        )
                    else Nil
                row(UI.children(contentUI :: panel)*)
        }
        // Named by `aria-controls`; the announcement itself rides the trigger, which is the one
        // element focus is ever on.
        var listEl = ul.cssClass("p-cascadeselect-list").role(if path.isEmpty then "tree" else "group")
        if path.isEmpty then st.idBase.foreach(b => listEl = listEl.id(listId(b)))
        listEl(UI.children(rows)*)
    end list
end CascadeSelect

object CascadeSelect:
    final private[uic] case class Props[A](
        // The roots the body draws, resolved from `itemSources` by the region that draws them.
        items: List[CascadeItem[A]],
        labelF: A => String,
        keyF: Maybe[A => String] = Absent,
        choice: Maybe[SingleChoice[A]] = Absent,
        openSlot: Slot[Boolean] = Slot(),
        placeholderV: Maybe[TextValue] = Absent,
        disabledFlag: Boolean = false,
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
        loadingV: Maybe[BoolValue] = Absent,
        // A root is not a function, so each one is an entry of its own and a signal one list of them (see [[LiveEntry]]).
        itemSources: List[LiveEntry[CascadeItem[A], Unit]] = Nil
    )

    private val openName      = Slot.Name("CascadeSelect", "the panel", "an open or a close", "onOpenChange")
    private val selectionName = Slot.Name("CascadeSelect", "the selection", "a pick", "onChange or onPick")

    def apply[A](): CascadeSelect[A] = new CascadeSelect[A](Props(Nil, _.toString))

    /** The wired interaction state, allocated per mount: the root panel's open flag, the
      * open groups the nested panels ride on, and the keyboard focus path.
      */
    final private[uic] case class Wired(
        open: Slot[Boolean],
        submenus: Submenus.View,
        focusRef: Anchored.View[List[Int]],
        focus: List[Int],
        /** The id the highlight is announced through. ONE id that travels with the highlight,
          * which is how the menu family carries it: the rows live across nested panels, so a
          * per-row id would be an id scheme spanning a tree, and `aria-activedescendant` only ever
          * names one of them.
          */
        idBase: Maybe[String] = Absent
    )
end CascadeSelect
