package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** TreeSelect: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TreeSelect anatomy: `div.p-treeselect.p-component.p-inputwrapper
  * [.p-invalid][.p-disabled][.p-treeselect-sm|-lg][.p-variant-filled]
  * [.p-treeselect-fluid][.p-treeselect-open]` > `div.p-treeselect-label-container`
  * > `div.p-treeselect-label` + `div.p-treeselect-dropdown` with the chevron,
  * floating `div.p-treeselect-overlay` panel > `div.p-treeselect-tree-container`
  * hosting the EXISTING [[Tree]] component's `.p-tree` anatomy), so the
  * extracted `@primeuix` treeselect CSS applies verbatim: its
  * `.p-treeselect-overlay .p-tree` rule reaches through the composition.
  *
  * Select's tree-shaped sibling: the same [[Overlay]]-based floating panel
  * (outside click / Escape close it, and the panel takes no focus: the trigger
  * keeps it and wears the hosted tree's own keyboard), the value
  * a `SignalRef[Set[K]]` of selected NODE ids, `K` being the app's own id type. The panel hosts the real
  * `uic.Tree`: its [[TreeNode]] model, expansion refs, and [[SelectionMode]]
  * semantics are reused wholesale: `Single` replaces the selection and CLOSES
  * the panel on pick (Prime), `Multiple`/`Checkbox` toggle ids and keep the
  * panel open (Checkbox renders Prime's per-node checkbox column and inherits
  * Tree's cascading tri-state: a click carries over the whole subtree, and an
  * ancestor holding only part of its descendants renders indeterminate).
  * Expansion state is component-internal unless `expanded(ref)` binds it out.
  *
  * Options come in the same shape as the flat pickers': `options(roots)(label)
  * (children)` takes the values themselves as node ids, and `options(roots)(label,
  * id)(children)` projects any `A` to a typed id. The extra `children`
  * projection is the whole difference between a tree-shaped picker and a flat
  * one. `nodes(TreeNode*)` remains for hand-authored trees whose nodes carry an
  * icon, a tooltip or their own accessible name. `loading` says the tree is still
  * arriving; without it a selected id no node carries is named in a
  * `.p-uic-diagnostic` card.
  *
  * The trigger shows the selected nodes' labels (tree order, comma-joined) or
  * the placeholder while empty.
  *
  * Keyboard: the panel's tree is the hosted [[Tree]]'s own, so it navigates exactly as a bare
  * tree does (ArrowDown/ArrowUp over the visible rows, ArrowRight and ArrowLeft to open and
  * close, Home/End, Enter or Space to select). The handler rides on the PANEL rather than on
  * the list, because the panel is what holds focus when it opens and a keydown there never
  * reaches a handler further down; it is the tree's handler either way, so the two keyboards
  * cannot drift. The trigger opens on ArrowDown, Enter or Space, and Escape closes.
  *
  * Honest deferrals: chip display mode, the header filter, and showClear.
  *
  * `id` lands on the field root, the combobox that holds the tab stop, and the tree derives `$id-list` and
  * `$id-node-<n>` from it (a minted base stands in where none is given).
  */
final case class TreeSelect[K] private (
    // The roots the body draws, resolved from `nodeSources` by the region that draws them.
    nodeList: List[TreeNode[K]] = Nil,
    valueSlot: Slot[Set[K]] = Slot[Set[K]](),
    expandedSlot: Slot[Set[K]] = Slot[Set[K]](),
    openSlot: Slot[Boolean] = Slot(),
    selectionModeV: SelectionMode = SelectionMode.Single,
    placeholderV: Maybe[TextValue] = Absent,
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
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[K] => UI.Action] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    nodeSources: List[TreeSelect.NodeSource[K]] = Nil
) extends Node, MultiChoiceFormControl[K], HasEmptyContent, HasTooltip, HasPlaceholder, HasAccessibleNameRef:
    type Self = TreeSelect[K]

    // Ids compare the way the sets that hold them already compare them, by `equals`; the expansion ref this field
    // allocates for itself needs that said.
    private given CanEqual[K, K] = CanEqual.derived

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): TreeSelect[K] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends typed root options in the picker family's shape, the values themselves serving as node ids: `label`
      * projects one to its visible text and `children` to its sub-options, so the tree structure arrives as a projection
      * like every other key, and the selection holds the values.
      *
      * The family shape does not fit every tree, because a `TreeNode` also carries
      * an icon, a tooltip and an accessible name that no `A => String` supplies;
      * reach for [[nodes]] with the hand-authored model when you need those.
      */
    def options(roots: Iterable[K] | Signal[Seq[K]])(label: K => String)(children: K => Iterable[K]): TreeSelect[K] =
        copy(nodeSources = nodeSources :+ TreeSelect.projected(roots, label, (k: K) => k, children))

    /** [[options]] over any `A`, with `id` projecting each to the node id the selection holds. */
    def options[A](roots: Iterable[A] | Signal[Seq[A]])(label: A => String, id: A => K)(children: A => Iterable[A]): TreeSelect[K] =
        copy(nodeSources = nodeSources :+ TreeSelect.projected(roots, label, id, children))

    /** Appends root nodes of the hand-authored [[TreeNode]] model, the escape hatch
      * from [[options]] when a node needs an icon, a tooltip or its own accessible
      * name. [[Listbox.items]] stands in the same relation to [[Listbox.item]]. Each node is built
      * directly or from `_` (see [[TreeNodeScope]]), or as a signal of nodes for a tree whose roots change while the
      * field is shown (see [[LiveEntry]]).
      */
    def nodes(ns: TreeNodeLiveEntry[K]*): TreeSelect[K] = copy(nodeSources = nodeSources :+ TreeSelect.NodeSource.Entries(ns.toList))

    /** The set of selected node ids. A `SignalRef` binds two way: picks write the updated set back, ref changes reselect
      * the matching nodes. A plain `Signal` or a constant binds one way: the field shows it and a pick only reports the
      * set it asks for to [[onChange]]; with no [[onChange]] that is reported in a card.
      */
    @targetName("selectionSet")
    def selection(v: Set[K] | Signal[Set[K]]): TreeSelect[K] = copy(valueSlot = valueSlot.bind(v))

    /** The expanded node ids of the panel's tree (optional; expansion is component-internal otherwise). A `SignalRef`
      * binds two way; a plain `Signal` or a constant binds one way, and a toggle only reports the set it asks for to
      * [[onExpandedChange]], which a one-way binding needs.
      */
    def expanded(v: Set[K] | Signal[Set[K]]): TreeSelect[K] = copy(expandedSlot = expandedSlot.bind(v))

    /** Fired with the nodes open after a toggle in the panel, after a two-way [[expanded]] is written. */
    def onExpandedChange(f: Set[K] => UI.Action)(using Frame): TreeSelect[K] = copy(expandedSlot = expandedSlot.hear(f))

    /** Says the tree is still arriving: the dropdown icon turns into Prime's spinner, and a selected id no node carries
      * yet is left alone rather than reported in a `.p-uic-diagnostic` card.
      */
    def loading(v: Boolean | Signal[Boolean]): TreeSelect[K] = copy(loadingV = Present(ReactiveValue(v)))

    /** Binds the panel visibility. A `SignalRef` binds two way: the trigger, a pick that closes, Escape, Tab and an
      * outside click write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the panel
      * follows it and the field only reports. A constant nobody hears is fixed: `false` draws the field read-only;
      * `true` keeps the panel open with nothing that closes it (no backdrop, no click on the field), while the field stays
      * the tab stop with the tree's keyboard and a pick is still the reader's. The combobox has no readonly state for its
      * tree, so that says nothing beyond `aria-expanded`. Unbound, the field keeps the state itself, starting closed. A
      * signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): TreeSelect[K] = copy(openSlot = openSlot.bind(v))

    /** Fired with the panel visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): TreeSelect[K] = copy(openSlot = openSlot.hear(f))

    /** Asks for the panel open or closed: a two-way binding is written, then the change reported. */
    private def askOpen(open: Slot[Boolean], v: Boolean)(using Frame): UI.Action = open.change(v)

    /** A panel bound closed by a constant nobody hears: nothing the reader does may open it. */
    private def openFixedShut: Boolean = openSlot.fixedAt(false)

    /** A panel held open by a constant nobody hears: nothing the reader does may close it. */
    private def openFixedOpen: Boolean = openSlot.fixedAt(true)

    /** Selection semantics (Tree's modes): `Single` (default: pick replaces the
      * selection and closes the panel), `Multiple` (toggle, panel stays open), or
      * `Checkbox` (toggle with Prime's per-node checkbox column, panel stays open).
      */
    def selectionMode(v: SelectionMode): TreeSelect[K] = copy(selectionModeV = v)

    private[uic] def withPlaceholder(v: Maybe[TextValue]): TreeSelect[K] = copy(placeholderV = v)

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): TreeSelect[K] = copy(emptyContentV = v)

    def disabled(v: Boolean): TreeSelect[K] = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected node ids.
      */
    def name(v: String): TreeSelect[K] = copy(nameV = Present(v))

    private[uic] def withTooltip(v: Maybe[TextValue]): TreeSelect[K] = copy(tooltipV = v)

    /** Size: `.p-treeselect-sm` / default / `.p-treeselect-lg`. */
    def size(v: Size): TreeSelect[K] = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TreeSelect[K] = copy(variantV = v)

    /** Spans the full width of its container (`.p-treeselect-fluid`). */
    def fluid(v: Boolean): TreeSelect[K] = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): TreeSelect[K]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): TreeSelect[K]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): TreeSelect[K] = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): TreeSelect[K] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): TreeSelect[K] = copy(accNameRefV = v)

    /** Fired with the FULL updated node id set after every selection change. */
    @targetName("onChangeSet")
    def onChange(f: Set[K] => UI.Action)(using Frame): TreeSelect[K] = copy(valueSlot = valueSlot.hear(f))

    /** Fires on focus loss (native `blur`) with the current selection. Unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    @targetName("onBlurSet")
    def onBlur(f: Set[K] => UI.Action)(using Frame): TreeSelect[K] = copy(onBlurF = Present(Callback.of(f)))

    /** (id, label) pairs of every node in tree order: the trigger-label lookup. */
    private def flatNodes: List[(K, String)] =
        def loop(ns: List[TreeNode[K]]): List[(K, String)] =
            ns.flatMap(n => (n.id, n.text) :: loop(n.children))
        loop(nodeList)
    end flatNodes

    /** The loud card shown at the top of the panel when two nodes share an id (see
      * [[Diagnostics]]). The ids are the selection keys, so a collision merges two
      * nodes into one selection entry, and [[options]] defaults the key to the label,
      * exactly the way [[Select.optionKey]] does, so the same duplicate-label data set
      * produces it.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        Diagnostics.cards("TreeSelect")(Diagnostic.naming(
            "node ids are not unique, so those nodes share one selection entry; give options an id " +
                "projection, or distinct TreeNode ids",
            Diagnostics.duplicates(flatNodes.map(_._1)).map(_.toString)
        )).toList
    end keyCollisionCard

    private[uic] def render(using Frame): UI =
        // A panel bound one way that nothing hears: every open and close the reader asks for is painted away again.
        Diagnostics.before(renderLive)("TreeSelect")(Diagnostic.unheard(openSlot, TreeSelect.openName, on = !disabledFlag))
    end render

    private def renderLive(using Frame): UI =
        // Open + (unless bound) expansion state live in signals allocated by this
        // effectful mount; static projections render the closed anatomy inert.
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val stat: UI = roots.and(shown).render { (rs, s) =>
            val (sel, ld, ph) = s
            copy(nodeList = rs).bodyStatic(sel, Set.empty, false, Absent, ph, ld, validity)
        }
        UI.mounted {
            for
                cmds <- UI.commands
                // A caller's own id is the base the tree's parts derive from; a minted one stands in.
                base <- idV.map(v => Kyo.lift(v)).getOrElse(cmds.freshId)
                // Unbound, the field keeps the panel state itself, starting closed.
                open <- openSlot.owned(false)
                exp  <- expandedSlot.owned(Set.empty[K])
                hi   <- Signal.initRef(-1)
                // Unbound, the field keeps the nodes the reader picks itself.
                value <- valueSlot.owned(Set.empty[K])
            yield copy(valueSlot = value, openSlot = open).wiredOver(exp, hi, base)
        }.placeholder(stat)
    end renderLive

    /** A constant selection nobody hears is fixed: the field is drawn read-only, reachable and readable with no panel to
      * open.
      */
    private def fixed: Boolean = openFixedShut || valueSlot.fixed

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(
        open: SignalRef[Boolean],
        exp: Set[K] | Signal[Set[K]],
        hi: SignalRef[Int],
        idBase: String
    )(using Frame): UI =
        copy(openSlot = openSlot.bind(open)).wiredOver(expandedSlot.bind(exp), hi, idBase)
    end wired

    /** [[wired]] over the panel state however it is bound. */
    private def wiredOver(
        exp: Slot[Set[K]],
        hi: SignalRef[Int],
        idBase: String
    )(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val panel    = openSlot.reading(false).and(exp.reading(Set.empty[K])).and(ReactiveValue.Dyn(hi))
        panel.and(shown).and(roots).render { case ((((o, e), h), (sel, ld, ph)), rs) =>
            copy(nodeList = rs).bodyStatic(sel, e, o, Present(TreeSelect.State(openSlot, exp, hi, h, idBase)), ph, ld, validity)
        }
    end wiredOver

    /** The roots as ONE reading, in the order the setters received them: fixed while none of them is a signal. */
    private[uic] def roots(using Frame): ReactiveValue[List[TreeNode[K]]] =
        TreeNodeScope.reading(nodeSources.flatMap {
            case TreeSelect.NodeSource.Entries(es)  => es
            case TreeSelect.NodeSource.Projected(f) => List(f(summon[Frame]))
        })

    /** What the field shows whatever its panel does: the selected ids, whether the nodes are still loading, and the
      * placeholder. Read beside the panel's state as ONE region (see [[ReactiveValue.and]]); the validity is stamped on the field,
      * never read here.
      */
    private def shown(using Frame): ReactiveValue[(Set[K], Boolean, Maybe[String])] =
        val held: ReactiveValue[Set[K]] = valueSlot.reading(Set.empty[K])
        held.and(ReactiveValue.optional(loadingV)).and(ReactiveValue.optional(placeholderV)).map { case ((sel, ld), ph) =>
            (sel, ld.getOrElse(false), ph)
        }
    end shown

    private def bodyStatic(
        current: Set[K],
        exp: Set[K],
        isOpen: Boolean,
        st: Maybe[TreeSelect.State[K]],
        placeholder: Maybe[String],
        loading: Boolean,
        validity: FieldValidity
    )(using Frame): UI =
        val selectedLabels = flatNodes.collect { case (id, text) if current.contains(id) => text }
        // Selected ids no node carries, once the tree is said to be complete.
        val known               = flatNodes.map(_._1).toSet
        val strays: Seq[String] = if loading then Nil else current.filterNot(known.contains).toList.map(_.toString).sorted

        /** Where an opening key lands: the first selected row, or the first row there is.
          * Opening has to land ON a row, or the arrow that opened the panel has moved nothing and
          * the reader presses it twice to reach the top of the tree.
          */
        val openHighlight: Int =
            val rows = Tree.visibleRows(nodeList, exp)
            val sel  = rows.indexWhere(r => current.contains(r.key))
            if sel >= 0 then sel else if rows.isEmpty then -1 else 0
        end openHighlight

        def openPanel: UI.Action =
            st match
                case Present(s) => s.hi.set(openHighlight).andThen(askOpen(s.open, true))
                case Absent     => ()

        // A pointer opens through the same path as a key, so the highlight lands on a row either way.
        def toggle: UI.Action =
            st match
                case Present(s) => if isOpen then askOpen(s.open, false) else openPanel
                case Absent     => ()

        // Fired by the hosted Tree after its selection change: Single mode closes the panel (Prime). The new set reaches
        // onChange through the tree's onSelectionChange, which carries it whether the binding is two-way or one-way.
        def afterPick: UI.Action =
            st match
                case Present(s) =>
                    selectionModeV match
                        case SelectionMode.Single | SelectionMode.Radio => askOpen(s.open, false)
                        case _                                          => ()
                case Absent => ()

        // === closed trigger ======================================================
        val showPlaceholder = selectedLabels.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selectedLabels.mkString(", ")

        var lbl = div.cssClass("p-treeselect-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if selectedLabels.isEmpty && placeholder.isEmpty then lbl = lbl.cssClass("p-treeselect-label-empty")
        // NBSP keeps the empty label's line box (Prime renders 'empty').
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        val labelContainerUI: UI = div.cssClass("p-treeselect-label-container")(labelUI)

        val dd             = div.cssClass("p-treeselect-dropdown").role("button").aria("haspopup", "tree").aria("expanded", isOpen.toString)
        val dropdownUI: UI =
            if loading then dd(GlyphSvg(Icons.spinner, "p-treeselect-loading-icon", "p-icon", "p-icon-spin"))
            else dd(GlyphSvg(Icons.chevronDown, "p-treeselect-dropdown-icon", "p-icon"))

        val hiddenCarrier: List[UI] =
            nameV.toList.map { n =>
                hiddenInput.jsProp("name", n).value(flatNodes.collect { case (id, _) if current.contains(id) => id.toString }.mkString(","))
            }

        // === floating panel ======================================================
        // The hosted tree, configured once: it renders the panel's rows AND supplies the keyboard
        // the TRIGGER wears, so the two cannot disagree about what a key does.
        val hosted: Maybe[Tree[K]] =
            if nodeList.isEmpty then Absent
            else
                st.map { state =>
                    var t = Tree[K]()
                        .rooted(nodeList)
                        .selectionMode(selectionModeV)
                        .onItemClick(_ => afterPick)
                    state.exp.bound.foreach(v => t = t.expanded(v))
                    state.exp.hearer.foreach(f => t = t.onExpandedChange(f))
                    valueSlot.bound.foreach(v => t = t.selection(v))
                    valueSlot.hearer.foreach(f => t = t.onSelectionChange(f))
                    t
                }

        val panelUI: List[UI] = st.toList.filter(_ => isOpen).map { state =>
            val roving     = Roving(state.hi, state.hiV, state.idBase, ownsFocus = false)
            val treeUI: UI = hosted match
                case Absent =>
                    EmptyContent.render(emptyContentV, "No results found")(c =>
                        div.cssClass("p-treeselect-empty-message")(c)
                    )
                // The enclosing wired render already subscribes to the expansion, value and
                // highlight refs, so the resolved seam renders the tree WITHOUT its own nested
                // subscriptions (handlers still write through the bound refs), and the highlight
                // is handed across so the panel's keyboard is the tree's own.
                case Present(t) => t.resolved(exp, current, Present(roving))
            // Nothing in this panel takes focus: it stays on the trigger, which is where the
            // tree's keyboard and the highlight announcement both live. A panel that seeded focus
            // would take the announcement away from the one element reading it.
            val panel = Overlay.hosted(state.open)
                .panelClass("p-treeselect-overlay")
                .panelClass("p-component")
                .seedFocus(false)
                .dismissOnEscape(false)
                // The panel renders inside the field, whose click toggles it: a row pick, an
                // expand toggle or a click on empty space ends here.
                .containClicks(true)
            panel(
                (keyCollisionCard :+ (div.cssClass("p-treeselect-tree-container")(treeUI): UI))*
            )
                .renderShown(Present(s"${state.idBase}-panel"))
        }

        // === field root ==========================================================
        var el = div.cssClass("p-treeselect").cssClass("p-component").cssClass("p-inputwrapper")
        // On the root, the combobox that holds the tab stop, not on the label container: see
        // [[HasElementId]]. The tree's row and list ids derive from the same value.
        idV.foreach(v => el = el.id(v))
        if selectedLabels.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if loading then el = el.aria("busy", "true")
        if isOpen then el = el.cssClass("p-treeselect-open").cssClass("p-uic-overlay-anchor")
        el = validity.mark(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small  => el = el.cssClass("p-treeselect-sm")
            case Size.Large  => el = el.cssClass("p-treeselect-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-treeselect-fluid")
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
        st.foreach { s =>
            el = el.aria("controls", Tree.rootListId(s.idBase))
            if isOpen && s.hiV >= 0 then el = el.aria("activedescendant", Tree.rowId(s.idBase, s.hiV))
        }
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            if fixed then el = el.aria("readonly", "true")
            // The one click listener: a click anywhere on the field toggles the panel.
            // A panel fixed open has no close for a click to ask for, so the field takes none.
            if st.isDefined && !fixed && !openFixedOpen then el = el.onClick(toggle)
            // Focus stays on the trigger while the panel is open, so the tree's own keyboard runs
            // from here: closed, the three opening keys; open, the tree's arrows and Escape. The
            // tree supplies the handler rather than a second copy of it living here.
            st.filter(_ => !fixed).foreach { state =>
                val roving  = Roving(state.hi, state.hiV, state.idBase, ownsFocus = false)
                val treeKey = hosted.map(_.keyHandler(exp, roving))
                el = el.onKeyDown { e =>
                    if !isOpen then
                        e.key match
                            case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space => openPanel
                            case _                                                    => ()
                    // Escape closes, and so does Tab in either direction: the panel's keyboard
                    // lives here, on the element the Tab is carrying the reader away from, so a
                    // panel left open behind them is one nothing answers.
                    else if e.key == Keyboard.Escape || e.key == Keyboard.Tab then askOpen(state.open, false)
                    else
                        treeKey match
                            case Present(f) => f(e)
                            case Absent     => ()
                }
            }
            // Focus-loss on the trigger reports the currently bound selection (or the
            // resolved current set when unbound): the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueSlot.current(current).map(f))
            }
        end if

        val field = validity.follow(el(UI.children((labelContainerUI :: dropdownUI :: hiddenCarrier) ++ panelUI)*))
        // Bindings made one way that nothing hears: every pick or toggle is painted away again.
        Diagnostics.after(field)("TreeSelect")(
            Diagnostic.unknown(strays),
            Diagnostic.unheard(valueSlot, TreeSelect.selectionName, on = !disabledFlag),
            Diagnostic.unheard(expandedSlot, TreeSelect.expandedName, on = !disabledFlag)
        )
    end bodyStatic
end TreeSelect

object TreeSelect:

    /** The wired interaction state, allocated per mount: the panel's open flag, the expansion
      * the hosted [[Tree]] reads, and the keyboard highlight that tree roves, with the id it
      * addresses its rows by.
      */
    final private[uic] case class State[K](
        open: Slot[Boolean],
        exp: Slot[Set[K]],
        hi: SignalRef[Int],
        hiV: Int,
        idBase: String
    )

    def apply[K](): TreeSelect[K] = new TreeSelect[K]()

    private val openName      = Slot.Name("TreeSelect", "the panel", "an open or a close", "onOpenChange")
    private val selectionName = Slot.Name("TreeSelect", "selection", "a pick", "onChange")
    private val expandedName  = Slot.Name("TreeSelect", "expanded", "a toggle", "onExpandedChange")

    /** Projects one typed option (and, recursively, its sub-options) into the
      * [[TreeNode]] model the panel's [[Tree]] renders. Termination is the caller's
      * `children` projection: a cyclic one does not terminate, exactly as a cyclic
      * hand-authored `TreeNode` structure would not.
      */
    private def project[A, K](a: A, label: A => String, id: A => K, children: A => Iterable[A]): TreeNode[K] =
        TreeNode(label(a), id(a), children = children(a).toList.map(project(_, label, id, children)))

    /** Where some of the roots come from: entries as [[nodes]] takes them, or roots that [[options]] projects, which for a
      * signal can only happen once the region has a `Frame` to map it with. Its own case rather than a function among the
      * entries, since an entry may already be one (`_.node(...)`).
      */
    private[uic] enum NodeSource[K]:
        case Entries(entries: List[TreeNodeLiveEntry[K]])
        case Projected(make: Frame => Signal[Seq[TreeNode[K]]])

    /** The roots `options` was given, projected: as fixed nodes for a collection, deferred for a signal. */
    private def projected[A, K](roots: Iterable[A] | Signal[Seq[A]], label: A => String, id: A => K, children: A => Iterable[A])
        : NodeSource[K] =
        roots match
            case sig: Signal[?] =>
                NodeSource.Projected { frame =>
                    given CanEqual[Seq[TreeNode[K]], Seq[TreeNode[K]]] = CanEqual.derived
                    sig.asInstanceOf[Signal[Seq[A]]].map(_.map(project(_, label, id, children)))(using summon, frame)
                }
            case is =>
                // Fixed nodes are entries of their own: a node is not a function, so each is one entry.
                NodeSource.Entries(is.asInstanceOf[Iterable[A]].toList.map(project(_, label, id, children)))
end TreeSelect
