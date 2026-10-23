package kyo.uic

import kyo.*
import kyo.UI.*

/** One node of an [[OrganizationChart]] — a recursive hand-authored carrier: a
  * `label`, a stable `id` of the app's own type `K` (what the expansion and
  * selection sets hold, and the event payload),
  * its `children`, an optional `template` slot that wins over the plain label,
  * and an optional extra `className` on the node box (Prime's `styleClass`).
  */
final case class OrgChartNode[K](
    label: String,
    id: K,
    children: List[OrgChartNode[K]] = Nil,
    template: Maybe[UI] = Absent,
    className: Maybe[String] = Absent
)

/** OrganizationChart — native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's OrganizationChart anatomy: per node one nested
  * `table.p-organizationchart-table` whose first row holds the
  * `div.p-organizationchart-node[.p-organizationchart-node-selectable]
  * [.p-organizationchart-node-selected]` box (label/template + the round
  * `a.p-organizationchart-node-toggle-button` with the chevron — down while
  * expanded, up while collapsed, Prime's glyphs), a connector row with the
  * vertical `div.p-organizationchart-connector-down` line, a
  * `tr.p-organizationchart-connectors` row of paired
  * `td.p-organizationchart-connector-left/-right[.p-organizationchart-connector-top]`
  * line cells, and a `tr.p-organizationchart-node-children` row with one
  * `td[colspan=2]` per child subtree), so the extracted `@primeuix`
  * organizationchart CSS applies verbatim.
  *
  * Exactly like Prime, a collapsed subtree KEEPS its layout rows and hides them
  * via `visibility` (Prime's inline `childStyle`, expressed as the
  * `.p-uic-oc-hidden` remainder class).
  *
  * `expanded` binds two-way to a `SignalRef[Set[K]]` of EXPANDED ids, the
  * same polarity and the same name as [[Tree.expanded]] and
  * [[TreeTable.expanded]], so a ref moves between the three tree-shaped
  * components without inverting. Prime models this one collapse-keyed
  * (`collapsedKeys`); the inversion is absorbed here rather than exposed.
  * Binding the ref is also what makes the chart interactive: with a ref, every
  * node that has children renders its toggle button (Tree's contract); without
  * one the chart is static, which is Prime's `collapsible=false`.
  *
  * Selection (`Single`/`Multiple` + `selection` ref) follows the Tree contract.
  * Every nested table puts its rows in a real `tbody`, which is what the sheet's
  * `.p-organizationchart-table > tbody > tr > td` cell rule is scoped to.
  */
final case class OrganizationChart[K] private (
    rootNode: Maybe[OrgChartNode[K]] = Absent,
    expandedRef: Maybe[SignalRef[Set[K]]] = Absent,
    selectedRef: Maybe[SignalRef[Set[K]]] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    onNodeToggleF: Maybe[K => UI.Action] = Absent,
    onNodeClickF: Maybe[K => UI.Action] = Absent
) extends Node:
    type Self = OrganizationChart[K]

    /** The root node of the chart. */
    def node(n: OrgChartNode[K]): OrganizationChart[K] = copy(rootNode = Present(n))

    /** Binds the EXPANDED subtrees two-way, exactly like [[Tree.expanded]] and
      * [[TreeTable.expanded]]: the set holds the ids whose children are shown, an
      * empty set shows the root alone, and toggles flip membership. Binding a ref
      * also renders the toggle buttons on nodes with children; a chart with no ref
      * is static and fully open, since there would be no toggle to open it with.
      */
    def expanded(ref: SignalRef[Set[K]]): OrganizationChart[K] = copy(expandedRef = Present(ref))

    /** Binds the selection two-way to the set of chosen node ids. */
    def selection(ref: SignalRef[Set[K]]): OrganizationChart[K] = copy(selectedRef = Present(ref))

    /** Selection semantics: `Single`/`Multiple` on node click; `None` (default)
      * leaves nodes inert.
      */
    def selectionMode(v: SelectionMode): OrganizationChart[K] = copy(selectionModeV = v)

    /** Fired with the node id after a toggle press (after the collapse write). */
    def onNodeToggle(f: K => UI.Action)(using Frame): OrganizationChart[K] = copy(onNodeToggleF = Present(Callback.of(f)))

    /** Fired with the node id after a node-box click (after the selection write). */
    def onNodeClick(f: K => UI.Action)(using Frame): OrganizationChart[K] = copy(onNodeClickF = Present(Callback.of(f)))

    private def selectable: Boolean =
        selectionModeV != SelectionMode.None || onNodeClickF.isDefined

    private[uic] def render(using Frame): UI =
        (expandedRef, selectedRef) match
            case (Present(e), Present(s)) => e.render(exp => s.render(sel => body(exp.contains, sel)))
            case (Present(e), Absent)     => e.render(exp => body(exp.contains, Set.empty))
            // A chart with no `expanded` ref renders no toggles, so an empty set would leave every subtree hidden with
            // nothing to open it: a static chart shows its whole hierarchy.
            case (Absent, Present(s)) => s.render(sel => body(_ => true, sel))
            case _                    => body(_ => true, Set.empty)

    private def body(isExpanded: K => Boolean, sel: Set[K])(using Frame): UI =
        val inner: List[UI] = rootNode.toList.map(n => renderNode(n, isExpanded, sel))
        div.cssClass("p-organizationchart").cssClass("p-component")(inner.map(toChild)*)

    /** One node's nested table: node row, connector rows (visibility-hidden while
      * collapsed, exactly Prime), and the children row of recursive subtrees.
      */
    private def renderNode(node: OrgChartNode[K], expandedAt: K => Boolean, sel: Set[K])(using Frame): UI =
        val hasChildren = node.children.nonEmpty
        val isExpanded  = expandedAt(node.id)
        val isSelected  = sel.contains(node.id)
        val colspan     = if hasChildren then node.children.length * 2 else 1

        // The node box: label/template + the round toggle anchor (chevron-down
        // while expanded, chevron-up while collapsed — Prime's glyphs).
        var box = div.cssClass("p-organizationchart-node")
        if selectable then box = box.cssClass("p-organizationchart-node-selectable").onClick(clickNode(node.id))
        if isSelected then box = box.cssClass("p-organizationchart-node-selected")
        node.className.foreach(c => box = box.cssClass(c))
        val labelSlot: UI = node.template match
            case Present(u) => u
            case Absent     => stringToUI(node.label)
        val toggle: List[UI] =
            if expandedRef.isDefined && hasChildren then
                List(
                    a
                        .cssClass("p-organizationchart-node-toggle-button")
                        .role("button")
                        .tabIndex(0)
                        .aria("expanded", isExpanded.toString)
                        .onClick(toggleNode(node.id))
                        // Prime's toggle is an anchor with no `href`, which the browser treats as
                        // ordinary text: it neither focuses it nor turns Enter into a click. The
                        // tab stop is this component's own, so the keys have to be too.
                        .onKeyDown { evt =>
                            evt.key match
                                case Keyboard.Enter | Keyboard.Space => toggleNode(node.id)
                                case _                               => ()
                        }
                        .stopPropagation(true)(
                            toChild(
                                GlyphSvg(
                                    if isExpanded then Icons.chevronDown else Icons.chevronUp,
                                    "p-organizationchart-node-toggle-button-icon"
                                )
                            )
                        )
                )
            else Nil
        val nodeRow: UI = tr(td.colspan(colspan)(box((toChild(labelSlot) :: toggle.map(toChild))*)))

        // The subtree rows render whenever there ARE children; a collapsed subtree
        // keeps its layout and hides via visibility (Prime's childStyle).
        val subtreeRows: List[UI] =
            if !hasChildren then Nil
            else
                val downRow: UI =
                    var r = tr.cssClass("p-organizationchart-connectors")
                    if !isExpanded then r = r.cssClass("p-uic-oc-hidden")
                    r(td.colspan(colspan)(div.cssClass("p-organizationchart-connector-down")))
                end downRow

                val linesRow: UI =
                    var r = tr.cssClass("p-organizationchart-connectors")
                    if !isExpanded then r = r.cssClass("p-uic-oc-hidden")
                    if node.children.length == 1 then
                        r(td.colspan(colspan)(div.cssClass("p-organizationchart-connector-down")))
                    else
                        val cells: List[UI] = node.children.zipWithIndex.flatMap { (_, i) =>
                            var left = td.cssClass("p-organizationchart-connector-left")
                            if i != 0 then left = left.cssClass("p-organizationchart-connector-top")
                            var right = td.cssClass("p-organizationchart-connector-right")
                            if i != node.children.length - 1 then right = right.cssClass("p-organizationchart-connector-top")
                            List(left(" "), right(" "))
                        }
                        r(cells.map(toChild)*)
                    end if
                end linesRow

                val childrenRow: UI =
                    var r = tr.cssClass("p-organizationchart-node-children")
                    if !isExpanded then r = r.cssClass("p-uic-oc-hidden")
                    r(node.children.map(c => toChild(td.colspan(2)(toChild(renderNode(c, expandedAt, sel)))))*)
                end childrenRow

                List(downRow, linesRow, childrenRow)

        table.cssClass("p-organizationchart-table")(
            toChild(tbody((nodeRow :: subtreeRows).map(toChild)*))
        )
    end renderNode

    /** A toggle press flips the node in the bound `expanded` set, then fires
      * `onNodeToggle`.
      */
    private def toggleNode(id: K)(using Frame): UI.Action =
        val write: UI.Action = expandedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()
        val fire: UI.Action = onNodeToggleF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end toggleNode

    /** A node-box click updates the bound selection per the mode, then fires
      * `onNodeClick`.
      */
    private def clickNode(id: K)(using Frame): UI.Action =
        val write: UI.Action = (selectedRef, selectionModeV) match
            case (Present(ref), SelectionMode.Single | SelectionMode.Radio) =>
                ref.getAndUpdate(cur => if cur.sizeIs == 1 && cur.contains(id) then Set.empty else Set(id))
            case (Present(ref), SelectionMode.Multiple | SelectionMode.Checkbox) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
        val fire: UI.Action = onNodeClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end clickNode
end OrganizationChart

object OrganizationChart:
    def apply[K](): OrganizationChart[K] = new OrganizationChart[K]()
end OrganizationChart
