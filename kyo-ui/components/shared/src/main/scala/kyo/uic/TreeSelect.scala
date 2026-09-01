package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** TreeSelect — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TreeSelect anatomy: `div.p-treeselect.p-component.p-inputwrapper
  * [.p-invalid][.p-disabled][.p-treeselect-sm|-lg][.p-variant-filled]
  * [.p-treeselect-fluid][.p-treeselect-open]` > `div.p-treeselect-label-container`
  * > `div.p-treeselect-label` + `div.p-treeselect-dropdown` with the chevron,
  * floating `div.p-treeselect-overlay` panel > `div.p-treeselect-tree-container`
  * hosting the EXISTING [[Tree]] component's `.p-tree` anatomy), so the
  * extracted `@primeuix` treeselect CSS applies verbatim — its
  * `.p-treeselect-overlay .p-tree` rule reaches through the composition.
  *
  * Select's tree-shaped sibling: the same [[Overlay]]-based floating panel
  * (outside click / Escape close it, and the panel takes no focus: the trigger
  * keeps it and wears the hosted tree's own keyboard), the value
  * a `SignalRef[Set[String]]` of selected NODE ids. The panel hosts the real
  * `uic.Tree` — its [[TreeNode]] model, expansion refs, and [[SelectionMode]]
  * semantics are reused wholesale: `Single` replaces the selection and CLOSES
  * the panel on pick (Prime), `Multiple`/`Checkbox` toggle ids and keep the
  * panel open (Checkbox renders Prime's per-node checkbox column and inherits
  * Tree's cascading tri-state: a click carries over the whole subtree, and an
  * ancestor holding only part of its descendants renders indeterminate).
  * Expansion state is component-internal unless `expanded(ref)` binds it out.
  *
  * Options come in the same shape as the flat pickers': `options(roots)(label)
  * (children)` projects any `A` to its text and its sub-options, with the option
  * key defaulting to the label as in [[Select.optionKey]]. The extra `children`
  * projection is the whole difference between a tree-shaped picker and a flat
  * one. `nodes(TreeNode*)` remains for hand-authored trees whose nodes carry an
  * icon, a tooltip or their own accessible name.
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
  */
final case class TreeSelect private (
    nodeList: List[TreeNode] = Nil,
    valueRef: Maybe[SignalRef[Set[String]]] = Absent,
    expandedRefV: Maybe[SignalRef[Set[String]]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
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
    onChangeF: Maybe[Set[String] => Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[String] => Any < Async] = Absent
) extends Node, MultiSelectFormControl, HasEmptyContent, HasTooltip, HasPlaceholder, HasAccessibleNameRef:
    type Self = TreeSelect

    /** Native `id` on the trigger — pair with `Label.forId`. */
    def id(v: String): TreeSelect = copy(idV = Present(v))

    /** Appends typed root options in the picker family's shape: `label` projects an
      * `A` to its visible text and `children` to its sub-options, so the tree
      * structure arrives as a projection like every other key. The option key
      * defaults to the label, exactly as in [[Select.optionKey]]; use the
      * three-projection overload when the label is not a stable identity.
      *
      * The family shape does not fit every tree, because a `TreeNode` also carries
      * an icon, a tooltip and an accessible name that no `A => String` supplies —
      * reach for [[nodes]] with the hand-authored model when you need those.
      */
    def options[A](roots: Seq[A])(label: A => String)(children: A => Seq[A]): TreeSelect =
        options(roots)(label, label)(children)

    /** [[options]] with an explicit stable key per option — the id written into the
      * bound selection set.
      */
    def options[A](roots: Seq[A])(label: A => String, optionKey: A => String)(children: A => Seq[A]): TreeSelect =
        copy(nodeList = nodeList ++ roots.toList.map(a => TreeSelect.project(a, label, optionKey, children)))

    /** Appends root nodes of the hand-authored [[TreeNode]] model — the escape hatch
      * from [[options]] when a node needs an icon, a tooltip or its own accessible
      * name. [[Listbox.items]] stands in the same relation to [[Listbox.item]].
      */
    def nodes(ns: TreeNode*): TreeSelect = copy(nodeList = nodeList ++ ns.toList)

    /** Binds the selected node id set two-way to `ref`: picks write the updated
      * set back, ref changes reselect the matching nodes.
      */
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): TreeSelect = copy(valueRef = Present(ref))

    /** Binds the expansion set two-way to `ref` (optional — expansion is
      * component-internal otherwise).
      */
    def expanded(ref: SignalRef[Set[String]]): TreeSelect = copy(expandedRefV = Present(ref))

    /** Binds the panel visibility two-way to `ref` (optional — self-managed
      * otherwise).
      */
    def open(ref: SignalRef[Boolean]): TreeSelect = copy(openRefV = Present(ref))

    /** Selection semantics (Tree's modes): `Single` (default — pick replaces the
      * selection and closes the panel), `Multiple` (toggle, panel stays open), or
      * `Checkbox` (toggle with Prime's per-node checkbox column, panel stays open).
      */
    def selectionMode(v: SelectionMode): TreeSelect = copy(selectionModeV = v)

    private[uic] def withPlaceholder(v: Maybe[TextValue]): TreeSelect = copy(placeholderV = v)

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): TreeSelect = copy(emptyContentV = v)

    def disabled(v: Boolean): TreeSelect = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected node ids.
      */
    def name(v: String): TreeSelect = copy(nameV = Present(v))

    private[uic] def withTooltip(v: Maybe[TextValue]): TreeSelect = copy(tooltipV = v)

    /** Size: `.p-treeselect-sm` / default / `.p-treeselect-lg`. */
    def size(v: Size): TreeSelect = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TreeSelect = copy(variantV = v)

    /** Spans the full width of its container (`.p-treeselect-fluid`). */
    def fluid(v: Boolean): TreeSelect = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): TreeSelect                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): TreeSelect                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): TreeSelect = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): TreeSelect = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): TreeSelect = copy(accNameRefV = v)

    /** Fired with the FULL updated node id set after every selection change. */
    def onChange(f: Set[String] => Any < Async): TreeSelect = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the current selection — unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): TreeSelect = copy(onBlurF = Present(f))

    /** (id, label) pairs of every node in tree order — the trigger-label lookup. */
    private def flatNodes: List[(String, String)] =
        def loop(ns: List[TreeNode]): List[(String, String)] =
            ns.flatMap(n => (n.id, n.text) :: loop(n.children))
        loop(nodeList)
    end flatNodes

    /** The loud card shown at the top of the panel when two nodes share an id (see
      * [[KeyDiagnostics]]). The ids are the selection keys, so a collision merges two
      * nodes into one selection entry — and [[options]] defaults the key to the label,
      * exactly the way [[Select.optionKey]] does, so the same duplicate-label data set
      * produces it.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        val dups = KeyDiagnostics.duplicates(flatNodes.map(_._1))
        if dups.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "TreeSelect",
                "node ids are not unique, so those nodes share one selection entry; give options an explicit key " +
                    "projection, or distinct TreeNode ids",
                dups
            ))
        end if
    end keyCollisionCard

    private[uic] def render(using Frame): UI =
        // Open + (unless bound) expansion state live in signals allocated by this
        // effectful mount; static projections render the closed anatomy inert.
        val stat: UI = withValue(sel => body(sel, Set.empty, false, Absent))
        UI.mounted {
            for
                cmds <- UI.commands
                base <- cmds.freshId
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                exp <- expandedRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(Set.empty[String])
                hi <- Signal.initRef(-1)
            yield wired(open, exp, hi, base)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(
        open: SignalRef[Boolean],
        exp: SignalRef[Set[String]],
        hi: SignalRef[Int],
        idBase: String
    )(using Frame): UI =
        open.render { o =>
            exp.render { e =>
                hi.render { h =>
                    withValue(sel => body(sel, e, o, Present(TreeSelect.State(open, exp, hi, h, idBase))))
                }
            }
        }

    private def withValue(f: Set[String] => UI)(using Frame): UI =
        valueRef match
            case Present(ref) => ref.render(f)
            case Absent       => f(Set.empty)

    private def body(
        current: Set[String],
        exp: Set[String],
        isOpen: Boolean,
        st: Maybe[TreeSelect.State]
    )(using Frame): UI =
        // Reactive-placeholder + -invalid gates (INSIDE the mount subscription — never around the
        // UI.mounted node): with a reactive slot set, re-render the field + message through the shared
        // helper; otherwise the untouched static form. The resolved placeholder text threads through.
        TextValue.reactive(placeholderV): ph =>
            (invalidV.dynSig, invalidMsgDynV) match
                case (Absent, Absent) => bodyStatic(current, exp, isOpen, st, ph)
                case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                        copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                            .bodyStatic(current, exp, isOpen, st, ph)
                    )

    private def bodyStatic(
        current: Set[String],
        exp: Set[String],
        isOpen: Boolean,
        st: Maybe[TreeSelect.State],
        placeholder: Maybe[String]
    )(using Frame): UI =
        val selectedLabels = flatNodes.collect { case (id, text) if current.contains(id) => text }

        /** Where an opening key lands: the first selected row, or the first row there is.
          * Opening has to land ON a row, or the arrow that opened the panel has moved nothing and
          * the reader presses it twice to reach the top of the tree.
          */
        val openHighlight: Int =
            val rows = Tree.visibleRows(nodeList, exp)
            val sel  = rows.indexWhere(r => current.contains(r.key))
            if sel >= 0 then sel else if rows.isEmpty then -1 else 0
        end openHighlight

        def openPanel: Any < Async =
            st match
                case Present(s) => s.hi.set(openHighlight).andThen(s.open.set(true))
                case Absent     => ()

        def toggle: Any < Async =
            st match
                case Present(s) => s.open.set(!isOpen)
                case Absent     => ()

        // Fired by the hosted Tree AFTER its selection write: report the new set
        // via onChange; Single mode also closes the panel (Prime).
        def afterPick: Any < Async =
            st match
                case Present(s) =>
                    for
                        _ <- onChangeF match
                            case Present(g) =>
                                valueRef match
                                    case Present(r) => r.get.map(g)
                                    case Absent     => (): Any < Async
                            case Absent => (): Any < Async
                        _ <- selectionModeV match
                            case SelectionMode.Single | SelectionMode.Radio => s.open.set(false)
                            case _                                          => (): Any < Async
                    yield ()
                case Absent => ()

        // === closed trigger ======================================================
        val showPlaceholder = selectedLabels.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selectedLabels.mkString(", ")

        var lbl = div.cssClass("p-treeselect-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if selectedLabels.isEmpty && placeholder.isEmpty then lbl = lbl.cssClass("p-treeselect-label-empty")
        // NBSP keeps the empty label's line box (Prime renders 'empty').
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        var lblContainer = div.cssClass("p-treeselect-label-container")
        idV.foreach(v => lblContainer = lblContainer.id(v))
        if !disabledFlag && st.isDefined then lblContainer = lblContainer.onClick(toggle)
        val labelContainerUI: UI = lblContainer(toChild(labelUI))

        var dd = div.cssClass("p-treeselect-dropdown").role("button").aria("haspopup", "tree").aria("expanded", isOpen.toString)
        if !disabledFlag && st.isDefined then dd = dd.onClick(toggle)
        val dropdownUI: UI = dd(toChild(GlyphSvg(Icons.chevronDown, "p-treeselect-dropdown-icon", "p-icon")))

        val hiddenCarrier: List[UI] =
            nameV.toList.map { n =>
                hiddenInput.jsProp("name", n).value(flatNodes.collect { case (id, _) if current.contains(id) => id }.mkString(","))
            }

        // === floating panel ======================================================
        // The hosted tree, configured once: it renders the panel's rows AND supplies the keyboard
        // the TRIGGER wears, so the two cannot disagree about what a key does.
        val hosted: Maybe[Tree] =
            if nodeList.isEmpty then Absent
            else
                st.map { state =>
                    var t = Tree()
                        .nodes(nodeList*)
                        .expanded(state.expRef)
                        .selectionMode(selectionModeV)
                        .onItemClick(_ => afterPick)
                    valueRef.foreach(r => t = t.selected(r))
                    t
                }

        val panelUI: List[UI] = st.toList.map { state =>
            val roving = Roving(state.hi, state.hiV, state.idBase, ownsFocus = false)
            val treeUI: UI = hosted match
                case Absent =>
                    EmptyContent.render(emptyContentV, "No results found")(c =>
                        div.cssClass("p-treeselect-empty-message")(c)
                    )
                // The enclosing wired render already subscribes to the expansion, value and
                // highlight refs — the resolved seam renders the tree WITHOUT its own nested
                // subscriptions (handlers still write through the bound refs), and the highlight
                // is handed across so the panel's keyboard is the tree's own.
                case Present(t) => t.resolved(exp, current, Present(roving))
            // Nothing in this panel takes focus: it stays on the trigger, which is where the
            // tree's keyboard and the highlight announcement both live. A panel that seeded focus
            // would take the announcement away from the one element reading it.
            val panel = Overlay(state.open)
                .panelClass("p-treeselect-overlay")
                .panelClass("p-component")
                .seedFocus(false)
                .dismissOnEscape(false)
            panel(
                (keyCollisionCard :+ (div.cssClass("p-treeselect-tree-container")(toChild(treeUI)): UI))*
            )
                .render
        }

        // === field root ==========================================================
        var el = div.cssClass("p-treeselect").cssClass("p-component").cssClass("p-inputwrapper")
        if selectedLabels.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if isOpen then el = el.cssClass("p-treeselect-open").cssClass("p-uic-overlay-anchor")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
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
            // Focus stays on the trigger while the panel is open, so the tree's own keyboard runs
            // from here: closed, the three opening keys; open, the tree's arrows and Escape. The
            // tree supplies the handler rather than a second copy of it living here.
            st.foreach { state =>
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
                    else if e.key == Keyboard.Escape || e.key == Keyboard.Tab then state.open.set(false)
                    else
                        treeKey match
                            case Present(f) => f(e)
                            case Absent     => ()
                }
            }
            // Focus-loss on the trigger reports the currently bound selection (or the
            // resolved current set when unbound) — the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueRef match
                    case Present(r) => r.use(f)
                    case Absent     => f(current))
            }
        end if

        FieldInvalid.withMessage(
            el(((labelContainerUI :: dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*),
            invalidV.constTrue,
            invalidMsgV
        )
    end bodyStatic
end TreeSelect

object TreeSelect:

    /** The wired interaction state, allocated per mount: the panel's open flag, the expansion
      * the hosted [[Tree]] reads, and the keyboard highlight that tree roves, with the id it
      * addresses its rows by.
      *
      * On the companion rather than inside the class, because the validity path renders through
      * a `copy` of the field and a state typed against the original instance does not fit the
      * copy's own path-dependent one.
      */
    final private[uic] case class State(
        open: SignalRef[Boolean],
        expRef: SignalRef[Set[String]],
        hi: SignalRef[Int],
        hiV: Int,
        idBase: String
    )

    def apply(): TreeSelect = new TreeSelect()

    /** Projects one typed option (and, recursively, its sub-options) into the
      * [[TreeNode]] model the panel's [[Tree]] renders. Termination is the caller's
      * `children` projection: a cyclic one does not terminate, exactly as a cyclic
      * hand-authored `TreeNode` structure would not.
      */
    private def project[A](a: A, label: A => String, key: A => String, children: A => Seq[A]): TreeNode =
        TreeNode(label(a), key(a), children = children(a).toList.map(project(_, label, key, children)))
end TreeSelect
