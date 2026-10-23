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
  * `.p-uic-key-error` card. Every panel is a real
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
final case class CascadeSelect[A] private (
    items: List[CascadeItem[A]],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    choice: Maybe[SingleChoice[A]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    placeholderV: Maybe[TextValue] = Absent,
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
    onChangeF: Maybe[Maybe[A] => UI.Action] = Absent,
    onPickF: Maybe[A => UI.Action] = Absent,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[Maybe[A] => UI.Action] = Absent,
    loadingV: Maybe[BoolValue] = Absent
) extends Node, ChoiceFormControl[A], HasTooltip, HasPlaceholder, HasAccessibleNameRef:
    type Self = CascadeSelect[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): CascadeSelect[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends typed option-tree roots with the leaf text projection. */
    def options(is: Iterable[CascadeItem[A]])(label: A => String): CascadeSelect[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Stable per-leaf key: the value written into the bound ref (defaults to
      * the label projection).
      */
    def optionKey(f: A => String): CascadeSelect[A] = copy(keyF = Present(f))

    /** Binds the selection to the leaf itself: the field shows the leaf whose [[optionKey]] the value carries, and a pick
      * writes the leaf. A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      */
    def selection(v: A | Signal[A]): CascadeSelect[A] = copy(choice = Present(SingleChoice.Value(ReactiveValue(v))))

    /** Binds the selection to the leaf or none: `Absent` shows the placeholder, a pick writes `Present(leaf)`. Two-way and
      * one-way as for the leaf binding above. `@targetName` because both `selection` bindings erase to one signature.
      */
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): CascadeSelect[A] =
        copy(choice = Present(SingleChoice.Optional(ReactiveValue(v))))

    /** Binds the ROOT panel visibility two-way to `ref` (optional, self-managed
      * otherwise); the nested group panels stay component-internal.
      */
    def open(ref: SignalRef[Boolean]): CascadeSelect[A] = copy(openRefV = Present(ref))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): CascadeSelect[A] = copy(placeholderV = v)

    def disabled(v: Boolean): CascadeSelect[A] = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * current leaf key.
      */
    def name(v: String): CascadeSelect[A] = copy(nameV = Present(v))

    private[uic] def withTooltip(v: Maybe[TextValue]): CascadeSelect[A] = copy(tooltipV = v)

    /** Size: `.p-cascadeselect-sm` / default / `.p-cascadeselect-lg`. */
    def size(v: Size): CascadeSelect[A] = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CascadeSelect[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-cascadeselect-fluid`). */
    def fluid(v: Boolean): CascadeSelect[A] = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): CascadeSelect[A]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): CascadeSelect[A]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): CascadeSelect[A] = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): CascadeSelect[A] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): CascadeSelect[A] = copy(accNameRefV = v)

    /** Fired with the leaf now chosen, after the write. A pick of the leaf already chosen changes nothing and fires nothing.
      * Fires before [[onPick]].
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): CascadeSelect[A] = copy(onChangeF = Present(Callback.of(f)))

    /** Fired with the leaf picked, after a pick that changed what the field holds: [[onChange]] without the `Maybe`, for a
      * field that is never empty or a one-way [[selection]] that writes its own state here. Fires after [[onChange]].
      */
    def onPick(f: A => UI.Action)(using Frame): CascadeSelect[A] = copy(onPickF = Present(Callback.of(f)))

    /** Fires on focus loss (native `blur`) with the leaf held (`Absent` for none). Unlike `onChange` it fires even when
      * nothing was changed. The form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): CascadeSelect[A] = copy(onBlurF = Present(Callback.of(f)))

    /** Says the option tree is still arriving: the dropdown icon turns into Prime's spinner, and a held leaf no leaf of the
      * tree carries yet is shown from the value itself rather than reported in a `.p-uic-key-error` card.
      */
    def loading(v: Boolean | Signal[Boolean]): CascadeSelect[A] = copy(loadingV = Present(ReactiveValue(v)))

    /** The stable leaf key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    /** Paths (index chains from the root) of every GROUP item, in tree order;
      * one open/closed ref is allocated per path (the Menubar machinery).
      */
    private[uic] def groupPaths: List[List[Int]] =
        def loop(is: List[CascadeItem[A]], prefix: List[Int]): List[List[Int]] =
            is.zipWithIndex.flatMap {
                case (CascadeItem.Group(_, children), i) =>
                    val p = prefix :+ i
                    p :: loop(children, p)
                case _ => Nil
            }
        loop(items, Nil)
    end groupPaths

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
        // One open/closed signal for the root panel plus one per group, allocated
        // by this effectful mount; static projections render the closed anatomy inert.
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val stat: UI = withValue(cur => body(cur, false, Map.empty.withDefaultValue(false), Absent, validity))
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                refs  <- Kyo.foreach(groupPaths)(p => Signal.initRef(false).map(p -> _))
                focus <- Signal.initRef(List.empty[Int])
                cmds  <- UI.commands
                base  <- idV.map(v => Kyo.lift(v)).getOrElse(cmds.freshId)
            yield wired(open, refs.toList, focus, Present(base))
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam): ONE reactive
      * chain over the root ref plus every group ref (MenuRender precedent).
      */
    private[uic] def wired(
        open: SignalRef[Boolean],
        refs: List[(List[Int], SignalRef[Boolean])],
        focus: SignalRef[List[Int]],
        base: Maybe[String] = Absent
    )(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        open.render { o =>
            MenuRender.renderAll(refs) { openMap =>
                focus.render { f =>
                    withValue(cur =>
                        body(cur, o, openMap.withDefaultValue(false), Present(CascadeSelect.State(open, refs, focus, f, base)), validity)
                    )
                }
            }
        }
    end wired

    private def withValue(f: Maybe[A] => UI)(using Frame): UI =
        choice match
            case Present(c) => c.render(f)
            case Absent     => f(Absent)

    private def body(
        held: Maybe[A],
        isOpen: Boolean,
        openMap: Map[List[Int], Boolean],
        st: Maybe[CascadeSelect.State],
        validity: FieldValidity
    )(using Frame): UI =
        // Reactive-placeholder gate (INSIDE the mount subscription, never around the UI.mounted node). The
        // resolved placeholder text threads through; the validity is stamped on the field, never re-rendered.
        BoolValue.reactive(loadingV): ld =>
            val loading = ld.constTrue
            TextValue.reactive(placeholderV): ph =>
                bodyStatic(held, isOpen, openMap, st, ph, loading, validity)

    private def bodyStatic(
        held: Maybe[A],
        isOpen: Boolean,
        openMap: Map[List[Int], Boolean],
        st: Maybe[CascadeSelect.State],
        placeholder: Maybe[String],
        loading: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        val current = held.fold("")(key)
        // A held leaf no leaf of the tree carries, once the tree is said to be complete.
        val unknownCard: List[UI] =
            held match
                case Present(a) if !leaves.exists(l => key(l) == current) && !loading =>
                    List(KeyDiagnostics.unknown("CascadeSelect", Seq(labelF(a))))
                case _ => Nil

        def closeAll: UI.Action =
            st match
                case Present(state) =>
                    for
                        _ <- MenuRender.openExactly(state.refs, Absent)
                        _ <- state.focusRef.set(Nil)
                        _ <- state.open.set(false)
                    yield ()
                case Absent => ()

        // Opens with every group closed and the highlight on the first root option. Not on the
        // selected one: the chain opens closed, so a leaf chosen three levels down has no row on
        // the screen to land on.
        def openPanel: UI.Action =
            st match
                case Present(state) =>
                    for
                        _ <- MenuRender.openExactly(state.refs, Absent)
                        _ <- state.focusRef.set(MenuNav.firstFocus(items).getOrElse(Nil))
                        _ <- state.open.set(true)
                    yield ()
                case Absent => ()

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
        if !disabledFlag && st.isDefined then lbl = lbl.onClick(toggleRoot)
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        var dd = div.cssClass("p-cascadeselect-dropdown")
        if !disabledFlag && st.isDefined then dd = dd.onClick(toggleRoot)
        val dropdownUI: UI =
            if loading then dd(toChild(GlyphSvg(Icons.spinner, "p-cascadeselect-loading-icon", "p-icon", "p-icon-spin")))
            else dd(toChild(GlyphSvg(Icons.chevronDown, "p-cascadeselect-dropdown-icon", "p-icon")))

        val hiddenCarrier: List[UI] =
            nameV.toList.map(n => hiddenInput.jsProp("name", n).value(current))

        /** The whole chain's keyboard, one [[MenuNav]] walk over the option tree.
          *
          * A cascade IS a vertical menu: Down and Up move within a panel, Right opens the
          * focused group and steps into it, Left closes back out, Enter picks a leaf or opens a
          * group, and Escape leaves one level or closes the field. So the menu family's state
          * machine runs it, over [[CascadeItem]] through its own [[MenuNav.Nodes]].
          */
        def panelKey(state: CascadeSelect.State): KeyboardEvent => UI.Action = e =>
            // Tab carries the reader away from the element the whole chain's keyboard lives on, so
            // the chain goes with them rather than floating over the page unanswered.
            if e.key == Keyboard.Tab then closeAll
            else
                MenuNav.onKey(items, MenuNav.Orientation.Vertical, state.focus, e.key) match
                    case Present(step) =>
                        val reopen: UI.Action = step.open match
                            case MenuNav.OpenOp.Keep      => ()
                            case MenuNav.OpenOp.Close     => MenuRender.openExactly(state.refs, Absent)
                            case MenuNav.OpenOp.OpenTo(p) => MenuRender.openExactly(state.refs, Present(p))
                        val chosen: UI.Action =
                            if step.activate then
                                MenuNav.itemAt(items, step.focus) match
                                    case Present(CascadeItem.Leaf(a)) => pick(a)
                                    case _                            => ()
                            else ()
                        val shut: UI.Action = if step.dismiss then state.open.set(false) else ()
                        state.focusRef.set(step.focus).andThen(reopen).andThen(chosen).andThen(shut)
                    case Absent => ()

        // === floating panel chain ================================================
        val panelUI: List[UI] = st.toList.collect {
            case state if isOpen =>
                Overlay(state.open)
                    .animate(false)
                    .panelClass("p-cascadeselect-overlay")
                    .panelClass("p-component")
                    // No panel in the chain takes focus: it stays on the trigger, which is where
                    // the keyboard is and where the highlight is announced. A panel that seeded
                    // focus would take the announcement away from the one element reading it.
                    .seedFocus(false)
                    .dismissOnEscape(false)(
                        div.cssClass("p-cascadeselect-list-container")(
                            toChild(list(items, Nil, current, openMap, Present(state), pick))
                        )
                    )
                    .renderOpen
        }

        // === field root ==========================================================
        var el = div.cssClass("p-cascadeselect").cssClass("p-component").cssClass("p-inputwrapper")
        // On the root, the combobox that holds the tab stop, not on the label text inside it: see
        // [[HasElementId]]. The chain's parts derive from the same value.
        idV.foreach(v => el = el.id(v))
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
        el = el.role("combobox").aria("haspopup", "tree").aria("expanded", isOpen.toString)
        st.flatMap(_.idBase).foreach { b =>
            el = el.aria("controls", listId(b))
            // One travelling id, wherever in the nested chain the highlight has walked to, because
            // `aria-activedescendant` names exactly one element and the trigger is the one thing
            // holding focus.
            st.foreach(state => if state.focus.nonEmpty then el = el.aria("activedescendant", activeId(b)))
        }
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            // Focus never leaves the trigger, however deep the chain of panels goes, so the whole
            // cascade keyboard lives here: closed, the three opening keys; open, one [[MenuNav]]
            // walk that steps into groups, back out of them, picks a leaf and closes on Escape.
            st.foreach { state =>
                el = el.onKeyDown { e =>
                    if isOpen then panelKey(state)(e)
                    else
                        e.key match
                            case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel
                            case _                                                    => ()
                }
            }
            // Focus-loss on the trigger reports the held leaf, the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(choice match
                    case Present(c) => c.read(f)
                    case Absent     => f(held))
            }
        end if

        val field = validity.follow(el(((labelUI :: dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*))
        if unknownCard.isEmpty then field else fragment((field :: unknownCard)*)
    end bodyStatic

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
        openMap: Map[List[Int], Boolean],
        st: Maybe[CascadeSelect.State],
        pick: A => UI.Action
    )(using Frame): UI =
        val rows: List[UI] = its.zipWithIndex.map {
            case (CascadeItem.Leaf(a), i) =>
                val isSel = key(a) == current
                var row   = li.cssClass("p-cascadeselect-option").role("treeitem").aria("selected", isSel.toString)
                if isSel then row = row.cssClass("p-cascadeselect-option-selected")
                if st.exists(_.focus == (path :+ i)) then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    st.flatMap(_.idBase).foreach(b => row = row.id(activeId(b)))
                var content = div.cssClass("p-cascadeselect-option-content")
                if st.isDefined then content = content.onClick(pick(a))
                row(toChild(content(toChild(span.cssClass("p-cascadeselect-option-text")(labelF(a))))))

            case (CascadeItem.Group(label, children), i) =>
                val p                      = path :+ i
                val groupOpen              = openMap(p)
                def toggleGroup: UI.Action =
                    st match
                        case Present(state) =>
                            val refs                     = state.refs
                            val target: Maybe[List[Int]] =
                                if groupOpen then (if p.size > 1 then Present(p.init) else Absent)
                                else Present(p)
                            MenuRender.openExactly(refs, target)
                        case Absent => ()
                var row = li
                    .cssClass("p-cascadeselect-option")
                    .cssClass("p-cascadeselect-option-group")
                    .cssClass("p-uic-overlay-anchor")
                    .role("treeitem")
                    .aria("expanded", groupOpen.toString)
                    .aria("level", (p.size).toString)
                if groupOpen then row = row.cssClass("p-cascadeselect-option-active")
                if st.exists(_.focus == p) then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    st.flatMap(_.idBase).foreach(b => row = row.id(activeId(b)))
                var content = div.cssClass("p-cascadeselect-option-content")
                if st.isDefined then content = content.onClick(toggleGroup)
                val contentUI: UI = content(
                    toChild(span.cssClass("p-cascadeselect-option-text")(label)),
                    toChild(
                        span.cssClass("p-cascadeselect-group-icon-container")(
                            toChild(GlyphSvg(Icons.angleRight, "p-cascadeselect-group-icon"))
                        )
                    )
                )
                // The side sub-panel: Prime's nested list skin on the Overlay primitive
                // (RightStart = the sheet's `inset-inline-start: 100%; inset-block-start: 0`).
                val panel: List[UI] = st match
                    case Present(state) if groupOpen =>
                        val refs = state.refs
                        refs.collectFirst { case (`p`, ref) => ref } match
                            case Some(ref) =>
                                List(
                                    Overlay(ref)
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
                            case None => Nil
                        end match
                    case _ => Nil
                row((contentUI :: panel).map(toChild)*)
        }
        // Named by `aria-controls`; the announcement itself rides the trigger, which is the one
        // element focus is ever on.
        var listEl = ul.cssClass("p-cascadeselect-list").role(if path.isEmpty then "tree" else "group")
        if path.isEmpty then st.foreach(state => state.idBase.foreach(b => listEl = listEl.id(listId(b))))
        listEl(rows.map(toChild)*)
    end list
end CascadeSelect

object CascadeSelect:
    def apply[A](): CascadeSelect[A] = new CascadeSelect[A](Nil, _.toString)

    /** The wired interaction state, allocated per mount: the root panel's open flag, the
      * per-group open refs the nested panels ride on, and the keyboard focus path.
      */
    final private[uic] case class State(
        open: SignalRef[Boolean],
        refs: List[(List[Int], SignalRef[Boolean])],
        focusRef: SignalRef[List[Int]],
        focus: List[Int],
        /** The id the highlight is announced through. ONE id that travels with the highlight,
          * which is how the menu family carries it: the rows live across nested panels, so a
          * per-row id would be an id scheme spanning a tree, and `aria-activedescendant` only ever
          * names one of them.
          */
        idBase: Maybe[String] = Absent
    )
end CascadeSelect
