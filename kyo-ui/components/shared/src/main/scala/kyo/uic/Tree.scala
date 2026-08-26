package kyo.uic

import kyo.*
import kyo.UI.*

/** One node of a [[Tree]] — a recursive hand-authored carrier: a `text` label, a
  * stable `id` (the expansion / selection key and the `onNodeToggle` payload), an
  * optional leading `icon` glyph, its `children` (empty for a leaf), a native
  * `tooltip` (title attribute), and an `accessibleName` override for the
  * treeitem.
  */
final case class TreeNode(
    text: String,
    id: String,
    icon: Maybe[IconGlyph] = Absent,
    children: List[TreeNode] = Nil,
    tooltip: Maybe[String] = Absent,
    accessibleName: Maybe[String] = Absent
)

/** Tree — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Tree
  * anatomy: `div.p-tree.p-component` > `div.p-tree-root` >
  * `ul.p-tree-root-children[role=tree]` of `li.p-tree-node[role=treeitem]`
  * entries, each a `div.p-tree-node-content` — toggle
  * `button.p-tree-node-toggle-button` with the chevron
  * `.p-tree-node-toggle-icon`, an optional Prime checkbox for multi-select, an
  * optional `.p-tree-node-icon` glyph, and the `span.p-tree-node-label` — over a
  * nested `ul.p-tree-node-children[role=group]` while expanded; leaves carry
  * `.p-tree-node-leaf`, which hides their toggle button), so the extracted
  * `@primeuix` tree CSS applies verbatim. Indentation comes from the real
  * nesting (`--p-tree-indent` on the child lists), exactly like Prime.
  *
  * Expansion and selection each bind two-way to a `SignalRef[Set[String]]`: the
  * tree re-renders reactively, a node's toggle button flips its id in the
  * `expanded` set before firing `onNodeToggle`, and clicking a node's content
  * updates the `selected` set per the [[SelectionMode]] before firing
  * `onItemClick`. Recursion is pure and bounded by the provided node structure.
  *
  * The two multi-selection modes differ in how the checkbox behaves.
  * `SelectionMode.Multiple` is FLAT: clicking a node toggles only its own id in
  * the `selected` set, and each box is a plain check/unchecked. `SelectionMode.Checkbox`
  * is Prime's CASCADING tri-state: the `selected` set holds every FULLY-checked id
  * (leaves and fully-checked parents), and each node's box state is DERIVED — a
  * leaf is checked iff its id is in the set, a parent is fully checked iff all its
  * children are (recursively), and indeterminate iff some but not all descendants
  * are checked. Toggling a node adds or removes its whole subtree, then a single
  * bottom-up `normalize` pass fixes every ancestor's membership to match
  * "all children fully checked". The cascade math is pure over the node tree and
  * the current set.
  */
final case class Tree private (
    nodeList: List[TreeNode] = Nil,
    expandedRef: Maybe[SignalRef[Set[String]]] = Absent,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    onNodeToggleF: Maybe[String => Any < Async] = Absent,
    onItemClickF: Maybe[String => Any < Async] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    emptyContentV: Maybe[EmptyContent] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = Tree

    /** Appends the given root nodes. */
    def nodes(ns: TreeNode*): Tree = copy(nodeList = nodeList ++ ns.toList)

    /** Binds expansion two-way to `ref`: toggles update the set, ref changes re-render `aria-expanded`. */
    def expanded(ref: SignalRef[Set[String]]): Tree = copy(expandedRef = Present(ref))

    /** Binds selection two-way to `ref`: clicking a node updates the set, ref changes re-render selection. */
    def selected(ref: SignalRef[Set[String]]): Tree = copy(selectedRef = Present(ref))

    def onNodeToggle(f: String => Any < Async): Tree = copy(onNodeToggleF = Present(f))

    /** Fired with the node id after a content click (after the selection write). */
    def onItemClick(f: String => Any < Async): Tree = copy(onItemClickF = Present(f))

    /** Selection semantics: `None` (default — nodes inert unless `onItemClick` is
      * set), `Single` (clicking replaces the selection; re-clicking clears it),
      * `Multiple` (flat multi-select — clicking toggles only the node's own id and
      * renders a plain per-node checkbox), or `Checkbox` (Prime's cascading
      * tri-state — the `selected` set stores every fully-checked id, checking a node
      * checks its whole subtree and re-checks any ancestor that becomes complete,
      * and parents render indeterminate while partially checked).
      */
    def selectionMode(v: SelectionMode): Tree = copy(selectionModeV = v)

    /** Text shown as the `li.p-tree-empty-message` row when the tree has no nodes. */
    def emptyContent(v: String): Tree = copy(emptyContentV = Present(EmptyContent.const(v)))

    /** Reactive text: re-renders the empty slot in place on signal emission. */
    def emptyContent(sig: Signal[String]): Tree = copy(emptyContentV = Present(EmptyContent.dyn(sig)))

    /** Arbitrary UI for the empty state: an icon over a line of explanation and the
      * button that creates the first record, rendered in the same slot the text would
      * occupy.
      */
    def emptyContent(ui: UI): Tree = copy(emptyContentV = Present(EmptyContent.ui(ui)))

    /** `aria-label` for the tree. */
    def accessibleName(v: String): Tree = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Tree = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** Whether nodes respond to content clicks (selecting or an explicit handler). */
    private def selectable: Boolean =
        selectionModeV != SelectionMode.None || onItemClickF.isDefined

    /** Both multi modes render Prime's per-node checkbox; they differ only in how
      * the box state derives from the set (`Multiple` flat, `Checkbox` cascading).
      */
    private def multiSelect: Boolean =
        selectionModeV == SelectionMode.Multiple || selectionModeV == SelectionMode.Checkbox

    /** Package-internal: hosts (TreeSelect) that already subscribe to the same
      * refs in an enclosing render supply the resolved sets directly — a second
      * nested subscription to the same refs would race the inner update against
      * the outer replace (the Overlay renderOpen lesson). The toggle/selection
      * handlers still write through the BOUND refs.
      */
    private[uic] def resolved(exp: Set[String], sel: Set[String])(using Frame): UI =
        body(exp, sel)

    private[uic] def render(using Frame): UI =
        // React to whichever refs are bound; nested reactive nodes render through in SSR.
        (expandedRef, selectedRef) match
            case (Present(e), Present(s)) => e.render(exp => s.render(sel => body(exp, sel)))
            case (Present(e), Absent)     => e.render(exp => body(exp, Set.empty))
            case (Absent, Present(s))     => s.render(sel => body(Set.empty, sel))
            case _                        => body(Set.empty, Set.empty)

    private def body(exp: Set[String], sel: Set[String])(using Frame): UI =
        val nodes = nodeList.map(n => renderNode(n, exp, sel))
        val emptyRow: List[UI] =
            if nodeList.isEmpty then
                EmptyContent.whenSet(emptyContentV)(c =>
                    li.cssClass("p-tree-empty-message").role("presentation")(c)
                )
            else Nil
        var list = ul.cssClass("p-tree-root-children").role("tree")
        accessibleNameV match
            case Present(TextValue.Const(v)) => list = list.aria("label", v)
            case Present(TextValue.Dyn(s))   => list = list.aria("label", s)
            case Absent                      => ()
        end match
        if multiSelect then list = list.aria("multiselectable", "true")

        var root = div.cssClass("p-tree").cssClass("p-component")
        if selectable then root = root.cssClass("p-tree-selectable")
        root(
            toChild(
                div.cssClass("p-tree-root")(
                    toChild(list((nodes ++ emptyRow).map(toChild)*))
                )
            )
        )
    end body

    /** Renders one node (and, when expanded, its children). Pure recursion bounded
      * by `node.children`.
      */
    private def renderNode(node: TreeNode, exp: Set[String], sel: Set[String])(using Frame): UI =
        val hasChildren = node.children.nonEmpty
        val isExpanded  = exp.contains(node.id)
        // In Checkbox mode "selected" (the highlighted, fully-checked state) is DERIVED
        // from the whole subtree; every other mode keys off the node's own id.
        val isSelected =
            if selectionModeV == SelectionMode.Checkbox then isFullyChecked(node, sel)
            else sel.contains(node.id)
        val isIndeterminate =
            selectionModeV == SelectionMode.Checkbox && isPartiallyChecked(node, sel)

        // The toggle button always renders; leaves hide it via the
        // `.p-tree-node-leaf > .p-tree-node-content .p-tree-node-toggle-button`
        // visibility rule, exactly like Prime.
        var toggleBtn = button
            .cssClass("p-tree-node-toggle-button")
            .jsProp("type", "button")
            .aria("hidden", "true")
            .tabIndex(-1)
        if hasChildren then toggleBtn = toggleBtn.onClick(toggleNode(node.id))
        val toggle: UI = toggleBtn(
            toChild(GlyphSvg(if isExpanded then Icons.chevronDown else Icons.chevronRight, "p-tree-node-toggle-icon"))
        )

        // Multi-select renders Prime's checkbox anatomy per node (the same box the
        // CheckBox component emits); clicking it toggles the selection. In Checkbox
        // mode the box is tri-state — a fully-checked node shows the check glyph, a
        // partially-checked parent the minus glyph on the neutral box (no checked
        // class), mirroring CheckBox's indeterminate skin.
        val checkboxSlot: List[UI] =
            if multiSelect then
                var cb = div.cssClass("p-checkbox").cssClass("p-component").cssClass("p-tree-node-checkbox").aria("hidden", "true")
                if isSelected then cb = cb.cssClass("p-checkbox-checked")
                if selectionModeV == SelectionMode.Checkbox then
                    cb = cb.aria("checked", if isSelected then "true" else if isIndeterminate then "mixed" else "false")
                if selectable then cb = cb.onClick(selectNode(node.id))
                val icon: List[UI] =
                    if isSelected then List(GlyphSvg(Icons.check, "p-checkbox-icon"))
                    else if isIndeterminate then List(GlyphSvg(Icons.minus, "p-checkbox-icon"))
                    else Nil
                List(cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*))))
            else Nil

        val iconSlot: List[UI] = node.icon.toList.map(g => GlyphSvg(g, "p-tree-node-icon"))
        // The selection click sits on the label (not the content div): kyo's event
        // dispatch bubbles, so a content-level handler would ALSO fire on toggle
        // button presses. The label stretches across the free row width
        // (.p-tree-node-label { flex: 1 } remainder), so the row still feels clickable.
        var label = span.cssClass("p-tree-node-label")
        if selectable then label = label.onClick(selectNode(node.id))
        val labelEl: UI = label(node.text)

        var content = div.cssClass("p-tree-node-content")
        if isSelected then content = content.cssClass("p-tree-node-selected")
        if selectable then content = content.cssClass("p-tree-node-selectable")
        node.tooltip.foreach(t => content = content.jsProp("title", t))
        val contentEl = content(((toggle :: checkboxSlot) ++ iconSlot :+ labelEl).map(toChild)*)

        val childrenUI: List[UI] =
            if hasChildren && isExpanded then
                List(
                    ul.cssClass("p-tree-node-children").role("group")(
                        node.children.map(c => toChild(renderNode(c, exp, sel)))*
                    )
                )
            else Nil

        var item = li.cssClass("p-tree-node").role("treeitem")
        if !hasChildren then item = item.cssClass("p-tree-node-leaf")
        if hasChildren then item = item.aria("expanded", isExpanded.toString)
        if isSelected then item = item.aria("selected", "true")
        node.accessibleName.foreach(v => item = item.aria("label", v))
        item((contentEl :: childrenUI).map(toChild)*)
    end renderNode

    /** Toggling a node flips its id in the bound `expanded` set, then fires `onNodeToggle`. */
    private def toggleNode(id: String)(using Frame): Any < Async =
        val setExpanded: Any < Async = expandedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()
        val fireToggle: Any < Async = onNodeToggleF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setExpanded
            r <- fireToggle
        yield r
        end for
    end toggleNode

    /** Clicking a node's content updates the bound `selected` set per the selection
      * mode, then fires `onItemClick`.
      */
    private def selectNode(id: String)(using Frame): Any < Async =
        val setSelection: Any < Async = (selectedRef, selectionModeV) match
            case (Present(ref), SelectionMode.Single | SelectionMode.Radio) =>
                ref.getAndUpdate(cur => if cur == Set(id) then Set.empty else Set(id))
            case (Present(ref), SelectionMode.Multiple) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case (Present(ref), SelectionMode.Checkbox) =>
                ref.getAndUpdate(cur => cascadeToggle(id, cur))
            case _ => ()
        val fireClick: Any < Async = onItemClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end selectNode

    /** The node's own id plus every descendant id (its whole subtree). */
    private[uic] def subtreeIds(node: TreeNode): Set[String] =
        node.children.foldLeft(Set(node.id))((acc, c) => acc ++ subtreeIds(c))

    /** Whether `node` is fully checked: a leaf iff its id is in `set`, a parent iff
      * ALL its children are fully checked (recursively).
      */
    private[uic] def isFullyChecked(node: TreeNode, set: Set[String]): Boolean =
        if node.children.isEmpty then set.contains(node.id)
        else node.children.forall(isFullyChecked(_, set))

    /** Whether `node` is indeterminate: not fully checked, yet some descendant is
      * checked. Always false for a leaf.
      */
    private[uic] def isPartiallyChecked(node: TreeNode, set: Set[String]): Boolean =
        !isFullyChecked(node, set) && subtreeIds(node).exists(set.contains)

    /** Finds the node carrying `id` anywhere in `nodes` (depth-first). */
    private def findNode(nodes: List[TreeNode], id: String): Maybe[TreeNode] =
        nodes match
            case Nil => Absent
            case n :: rest =>
                if n.id == id then Present(n)
                else
                    findNode(n.children, id) match
                        case p @ Present(_) => p
                        case Absent         => findNode(rest, id)

    /** Bottom-up (post-order) reconciliation: every parent's membership is rewritten
      * to match "all children fully checked", so ancestors of a change stay
      * consistent after a naive subtree add/remove. Leaves are left untouched.
      */
    private[uic] def normalize(nodes: List[TreeNode], set: Set[String]): Set[String] =
        nodes.foldLeft(set) { (acc, node) =>
            if node.children.isEmpty then acc
            else
                val reconciled = normalize(node.children, acc)
                if node.children.forall(isFullyChecked(_, reconciled)) then reconciled + node.id
                else reconciled - node.id
        }

    /** Toggling `id` in Checkbox mode: a fully-checked node has its whole subtree
      * removed, otherwise the subtree is added; then a single `normalize` pass
      * reconciles every ancestor's membership.
      */
    private[uic] def cascadeToggle(id: String, cur: Set[String]): Set[String] =
        findNode(nodeList, id) match
            case Present(node) =>
                val sub   = subtreeIds(node)
                val naive = if isFullyChecked(node, cur) then cur -- sub else cur ++ sub
                normalize(nodeList, naive)
            case Absent => cur
end Tree

object Tree:
    def apply(): Tree = new Tree()
