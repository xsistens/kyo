package kyo.uic

import kyo.*
import kyo.UI.*

/** One node of an [[OrganizationChart]]: a recursive hand-authored carrier: a
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

/** OrganizationChart: native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's OrganizationChart anatomy: per node one nested
  * `table.p-organizationchart-table` whose first row holds the
  * `div.p-organizationchart-node[.p-organizationchart-node-selectable]
  * [.p-organizationchart-node-selected]` box (label/template + the round
  * `a.p-organizationchart-node-toggle-button` with the chevron: down while
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
  * `expanded` binds a `Set[K]` of EXPANDED ids (a constant, a signal or a ref), the
  * same polarity and the same name as [[Tree.expanded]] and
  * [[TreeTable.expanded]], so a ref moves between the three tree-shaped
  * components without inverting. Prime models this one collapse-keyed
  * (`collapsedKeys`); the inversion is absorbed here rather than exposed.
  * Binding it is also what makes the chart collapsible: then every node that has
  * children renders its toggle button (Tree's contract), and `expanded()` does the
  * same over subtrees the chart keeps itself; without either the chart is static,
  * which is Prime's `collapsible=false`.
  *
  * Selection (`Single`/`Multiple` + `selection` ref) follows the Tree contract. A chart the
  * reader picks in is ONE tab stop, on the node box its cursor is on (else the chosen node,
  * else the root): the arrows move the cursor the way the chart is drawn ([[OrgChartNav]]),
  * Enter and Space pick the node as a click does, and a click moves the cursor there too.
  * Every nested table puts its rows in a real `tbody`, which is what the sheet's
  * `.p-organizationchart-table > tbody > tr > td` cell rule is scoped to.
  */
final class OrganizationChart[K] private[uic] (private[uic] val s: OrganizationChart.State[K]) extends Node, Stated:
    import s.*

    type Self = OrganizationChart[K]

    /** The root node of the chart. */
    def node(n: OrgChartNode[K]): OrganizationChart[K] = new OrganizationChart(s.copy(rootNode = Present(n)))

    /** Binds the EXPANDED subtrees with the polarity of [[Tree.expanded]] and [[TreeTable.expanded]]: the set holds the
      * ids whose children are shown, an empty set shows the root alone, and a toggle flips membership. Binding it renders
      * the toggle buttons on nodes with children. A `SignalRef` is written, then [[onExpandedChange]] hears the set; a
      * plain `Signal` is followed and a toggle only reported; a constant nobody hears is fixed, its toggles reachable and
      * inert. A signal nothing hears is reported in a card. Unbound, the chart is static and fully open; [[expanded]]
      * with no argument gives it toggles over state it keeps itself.
      */
    def expanded(v: Set[K] | Signal[Set[K]]): OrganizationChart[K] = new OrganizationChart(s.copy(expandedSlot = expandedSlot.bind(v)))

    /** Whether the chart has toggle buttons. Set, it decides alone, and a signal switches them in place. Without it a binding
      * of [[expanded]] turns them on, and an unbound chart is static.
      *
      * On and unbound, the chart keeps the expanded subtrees itself, starting fully open. Under a signal it holds them while
      * the switch is off, when the whole hierarchy shows, and comes back to them. Off over a binding the subtrees follow the
      * binding and only the toggles go. [[onExpandedChange]] hears every toggle either way.
      */
    def expandable(v: Boolean | Signal[Boolean]): OrganizationChart[K] =
        new OrganizationChart(s.copy(expandableV = Present(ReactiveValue(v))))

    /** Fired with the expanded ids after a toggle press, after a two-way [[expanded]] is written and before
      * [[onNodeToggle]].
      */
    def onExpandedChange(f: Set[K] => UI.Action)(using Frame): OrganizationChart[K] =
        new OrganizationChart(s.copy(expandedSlot = expandedSlot.hear(f)))

    /** Binds the selection to the set of chosen node ids. A `SignalRef` binds two way and [[onSelectionChange]] hears
      * every click; a plain `Signal` binds one way and the chart only reports; a constant nobody hears is fixed, the nodes
      * inert. Unbound, a chart with a [[selectionMode]] keeps the selection itself. A signal nothing hears is reported in
      * a card; [[onNodeClick]] hears it too.
      */
    def selection(v: Set[K] | Signal[Set[K]]): OrganizationChart[K] = new OrganizationChart(s.copy(selectedSlot = selectedSlot.bind(v)))

    /** Fired with the selected ids after a node click, after a two-way [[selection]] is written and before
      * [[onNodeClick]].
      */
    def onSelectionChange(f: Set[K] => UI.Action)(using Frame): OrganizationChart[K] =
        new OrganizationChart(s.copy(selectedSlot = selectedSlot.hear(f)))

    // Node ids compare the way the sets that hold them already compare them, by `equals`.
    private given CanEqual[K, K] = CanEqual.derived

    /** The switch as this chart reads it: set, it decides alone; unset, a binding of [[expanded]] turns the toggles on. */
    private def expandableOn: BoolValue = expandableV.getOrElse(ReactiveValue.Const(expandedSlot.binding.isDefined))

    /** Every node that has children: what a chart that opens fully starts with. */
    private def parents: Set[K] =
        def walk(n: OrgChartNode[K]): List[K] = (if n.children.nonEmpty then List(n.id) else Nil) ++ n.children.flatMap(walk)
        rootNode.toList.flatMap(walk).toSet

    /** Selection semantics: `Single`/`Multiple` on node click; `None` (default)
      * leaves nodes inert.
      */
    def selectionMode(v: SelectionMode): OrganizationChart[K] = new OrganizationChart(s.copy(selectionModeV = v))

    /** Fired with the node id after a toggle press (after the collapse write). */
    def onNodeToggle(f: K => UI.Action)(using Frame): OrganizationChart[K] =
        new OrganizationChart(s.copy(onNodeToggleF = Present(Callback.of(f)), expandedSlot = expandedSlot.heardToo))

    /** Fired with the node id after a node-box click (after the selection write). */
    def onNodeClick(f: K => UI.Action)(using Frame): OrganizationChart[K] =
        new OrganizationChart(s.copy(onNodeClickF = Present(Callback.of(f)), selectedSlot = selectedSlot.heardToo))

    /** Whether a node box is something the reader picks: there is a mode to select in, or an `onNodeClick` to hear it. */
    private def pickable: Boolean = selectionModeV != SelectionMode.None || onNodeClickF.isDefined

    /** Whether a pick does something: a fixed selection keeps its node cursor and its keys, and takes no pick. */
    private def selectable: Boolean = pickable && !selectedSlot.fixed

    private[uic] def render(using Frame): UI =
        // A binding one way that nothing hears: every toggle or click there is painted away again.
        val selected = Diagnostic.unheard(selectedSlot, OrganizationChart.selectionName)
        // The toggles can come and go, so what is said about the expansion goes with them.
        if expandedSlot.deaf then
            val cards = Diagnostics.cardsWhile(expandableOn)("OrganizationChart")(on =>
                Seq(Diagnostic.unheard(expandedSlot, OrganizationChart.expandedName, on), selected)
            )
            fragment((cards :+ charted)*)
        else Diagnostics.before(charted)("OrganizationChart")(selected)
        end if
    end render

    /** One mount for what the chart keeps itself, beside the id the node boxes derive theirs from: the subtrees [[expandable]]
      * keeps, starting fully open, the selection of a chart with a mode to select in, and the node cursor of a chart the reader
      * picks in. Before the mount runs the page already holds the one tab stop the attached chart puts on the chosen node or
      * the root, and draws the subtrees from a stand-in of where they start; only the keys wait for it.
      */
    private def charted(using Frame): UI =
        val ownsExpanded = expandedSlot.binding.isEmpty && BoolValue.mayTurnOn(expandableV)
        val ownsSelected = selectedSlot.binding.isEmpty && selectionModeV != SelectionMode.None
        if !pickable && !ownsExpanded then drawn(Absent, ownsExpanded = false)
        else
            UI.mounted {
                for
                    cmds   <- UI.commands
                    base   <- cmds.freshId
                    cursor <- Signal.initRef(Absent: Maybe[K])(using summon[Frame], CanEqual.derived)
                    exp    <- if ownsExpanded then expandedSlot.owned(parents) else Kyo.lift(expandedSlot)
                    sel    <- if ownsSelected then selectedSlot.owned(Set.empty[K]) else Kyo.lift(selectedSlot)
                yield new OrganizationChart(s.copy(expandedSlot = exp, selectedSlot = sel)).drawn(
                    if pickable then Present(OrganizationChart.Cursor(cursor, base, id => cmds.focusId(id))) else Absent,
                    ownsExpanded
                )
            }.placeholder(
                (if ownsExpanded then new OrganizationChart(s.copy(expandedSlot = expandedSlot.standIn(parents))) else this).drawn(
                    Absent,
                    ownsExpanded
                )
            )
        end if
    end charted

    /** The chart over its bindings and its node cursor, read as ONE region: a pick writes the selection and the cursor
      * together, and the switch is read with them. With the toggles off an empty set would leave every subtree hidden with
      * nothing to open it, so a chart that keeps the subtrees itself (`ownsExpanded`) shows its whole hierarchy while off, and a
      * binding is followed.
      */
    private def drawn(cursor: Maybe[OrganizationChart.Cursor[K]], ownsExpanded: Boolean)(using Frame): UI =
        given CanEqual[Set[K], Set[K]]     = CanEqual.derived
        given CanEqual[Maybe[K], Maybe[K]] = CanEqual.derived
        val exp                            = ReactiveValue.optional(expandedSlot.binding)
        val sel                            = selectedSlot.reading(Set.empty[K])
        val at                             = cursor.fold(ReactiveValue.Const(Maybe.empty[K]))(c => ReactiveValue.Dyn(c.ref))
        val on                             = expandableOn
        exp.and(sel).and(at).and(on).render { case (((e, s), c), toggles) =>
            val applied = if toggles || !ownsExpanded then e else Absent
            body(id => applied.forall(_.contains(id)), s, c, cursor, toggles)
        }
    end drawn

    private def body(isExpanded: K => Boolean, sel: Set[K], at: Maybe[K], cursor: Maybe[OrganizationChart.Cursor[K]], toggles: Boolean)(
        using Frame
    ): UI =
        // The one tab stop, in the order the reader would expect: the cursor, else the node they chose, else the root. A
        // cursor on a node a collapse took off the screen falls back the same way.
        val shown          = rootNode.toList.flatMap(r => OrgChartNav.visible(r, isExpanded)).map(_.id)
        val stop: Maybe[K] =
            if !pickable then Absent
            else at.filter(shown.contains).orElse(Maybe.fromOption(shown.find(sel.contains))).orElse(Maybe.fromOption(shown.headOption))
        val inner: List[UI] = rootNode.toList.map(n => renderNode(n, isExpanded, sel, stop, cursor, toggles))
        div.cssClass("p-organizationchart").cssClass("p-component")(UI.children(inner)*)
    end body

    /** Every node's place in the chart, root first, which is what a node box's id is derived from: a collapse does not
      * renumber what it hides.
      */
    private lazy val places: Map[K, Int] =
        def walk(n: OrgChartNode[K]): List[K] = n.id :: n.children.flatMap(walk)
        rootNode.toList.flatMap(walk).zipWithIndex.toMap

    private def nodeId(base: String, id: K): String = s"$base-n${places.getOrElse(id, -1)}"

    /** One node's nested table: node row, connector rows (visibility-hidden while
      * collapsed, exactly Prime), and the children row of recursive subtrees.
      */
    private def renderNode(
        node: OrgChartNode[K],
        expandedAt: K => Boolean,
        sel: Set[K],
        stop: Maybe[K],
        cursor: Maybe[OrganizationChart.Cursor[K]],
        toggles: Boolean
    )(using Frame): UI =
        val hasChildren = node.children.nonEmpty
        val isExpanded  = expandedAt(node.id)
        val isSelected  = sel.contains(node.id)
        val colspan     = if hasChildren then node.children.length * 2 else 1

        // The node box: label/template + the round toggle anchor (chevron-down
        // while expanded, chevron-up while collapsed, Prime's glyphs).
        var box = div.cssClass("p-organizationchart-node")
        // A pointer seeds the cursor too, so a click leaves the chart where the next arrow reads.
        val seed: UI.Action = cursor.fold[UI.Action](())(_.ref.set(Present(node.id)))
        if selectable then box = box.cssClass("p-organizationchart-node-selectable").onClick(seed.andThen(clickNode(node.id)))
        // The chart is ONE tab stop where the reader picks: the node box the cursor is on, which the arrows move. A fixed
        // selection keeps it, and its keys pick nothing.
        if stop.contains(node.id) then box = box.tabIndex(0)
        else if pickable && cursor.isDefined then box = box.tabIndex(-1)
        cursor.foreach { c =>
            box = box.id(nodeId(c.base, node.id)).preventScrollKeys.onKeyDown { e =>
                e.key match
                    case Keyboard.Enter | Keyboard.Space => if selectable then seed.andThen(clickNode(node.id)) else ()
                    case key                             =>
                        rootNode.flatMap(r => OrgChartNav.onKey(r, expandedAt, node.id, key))
                            .fold[UI.Action](())(to => c.ref.set(Present(to)).andThen(c.focus(nodeId(c.base, to))))
            }
        }
        if isSelected then box = box.cssClass("p-organizationchart-node-selected")
        node.className.foreach(c => box = box.cssClass(c))
        val labelSlot: UI = node.template match
            case Present(u) => u
            case Absent     => stringToUI(node.label)
        val toggle: List[UI] =
            if toggles && hasChildren then
                val shell = a
                    .cssClass("p-organizationchart-node-toggle-button")
                    .role("button")
                    .tabIndex(0)
                    .aria("expanded", isExpanded.toString)
                // A constant nobody hears is fixed: the toggle stays a tab stop and says it changes nothing.
                val pressable =
                    if expandedSlot.fixed then shell.aria("disabled", "true")
                    else
                        shell
                            .onClick(toggleNode(node.id))
                            // Prime's toggle is an anchor with no `href`, which the browser treats as
                            // ordinary text: it neither focuses it nor turns Enter into a click. The
                            // tab stop is this component's own, so the keys have to be too.
                            .onKeyDown { evt =>
                                evt.key match
                                    case Keyboard.Enter | Keyboard.Space => toggleNode(node.id)
                                    case _                               => ()
                            }
                List(
                    pressable
                        .stopPropagation(true)(
                            GlyphSvg(
                                if isExpanded then Icons.chevronDown else Icons.chevronUp,
                                "p-organizationchart-node-toggle-button-icon"
                            )
                        )
                )
            else Nil
        val nodeRow: UI = tr(td.colspan(colspan)(box(UI.children(labelSlot :: toggle)*)))

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
                        r(UI.children(cells)*)
                    end if
                end linesRow

                val childrenRow: UI =
                    var r = tr.cssClass("p-organizationchart-node-children")
                    if !isExpanded then r = r.cssClass("p-uic-oc-hidden")
                    r(UI.children(node.children.map(c => td.colspan(2)(renderNode(c, expandedAt, sel, stop, cursor, toggles))))*)
                end childrenRow

                List(downRow, linesRow, childrenRow)

        table.cssClass("p-organizationchart-table")(
            tbody(UI.children(nodeRow :: subtreeRows)*)
        )
    end renderNode

    /** A toggle press flips the node in the bound `expanded` set, then fires
      * `onNodeToggle`.
      */
    private def toggleNode(id: K)(using Frame): UI.Action =
        val write: UI.Action = expandedSlot.update(cur => if cur.contains(id) then cur - id else cur + id)
        val fire: UI.Action  = onNodeToggleF match
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
        val write: UI.Action = selectionModeV match
            case SelectionMode.Single | SelectionMode.Radio =>
                selectedSlot.update(cur => if cur.sizeIs == 1 && cur.contains(id) then Set.empty else Set(id))
            case SelectionMode.Multiple | SelectionMode.Checkbox =>
                selectedSlot.update(cur => if cur.contains(id) then cur - id else cur + id)
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
    final private[uic] case class State[K](
        rootNode: Maybe[OrgChartNode[K]] = Absent,
        expandedSlot: Slot[Set[K]] = Slot[Set[K]](),
        expandableV: Maybe[BoolValue] = Absent,
        selectedSlot: Slot[Set[K]] = Slot[Set[K]](),
        selectionModeV: SelectionMode = SelectionMode.None,
        onNodeToggleF: Maybe[K => UI.Action] = Absent,
        onNodeClickF: Maybe[K => UI.Action] = Absent
    )

    private val expandedName             = Slot.Name("OrganizationChart", "the expanded subtrees", "a toggle", "onExpandedChange")
    private val selectionName            = Slot.Name("OrganizationChart", "the selection", "a click", "onSelectionChange")
    def apply[K](): OrganizationChart[K] = new OrganizationChart[K](State())

    /** The node cursor a chart's mount mints: the node the reader is on, the id base its node boxes derive theirs from,
      * and the command that moves focus to one of them.
      */
    final private[uic] case class Cursor[K](ref: SignalRef[Maybe[K]], base: String, focus: String => UI.Action)
end OrganizationChart
