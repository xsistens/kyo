package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** One row of a [[TreeTable]]: a recursive hand-authored carrier: the typed row
  * `data` plus its `children` (empty for a leaf). Row identity comes from the
  * table's `rowKey` projection over `data`.
  */
final case class TreeTableNode[A](data: A, children: List[TreeTableNode[A]] = Nil) derives CanEqual

/** A TreeTable row node, built directly or as `_.node(...)` (see [[TreeTableNodeScope]]). */
type TreeTableNodeEntry[A] = Entry[TreeTableNode[A], TreeTableNodeScope[A]]

/** A [[TreeTableNodeEntry]] or a signal of nodes, for a table whose roots change while it is shown (see [[LiveEntry]]). */
type TreeTableNodeLiveEntry[A] = LiveEntry[TreeTableNode[A], TreeTableNodeScope[A]]

/** What `_` stands for in [[TreeTable.nodes]]: `_.node(row, children = Seq(_.node(child)))`, as [[TreeTableNode.apply]], with the
  * children in the same two spellings.
  */
final class TreeTableNodeScope[A] private ():
    /** A row node over `data`, as [[TreeTableNode.apply]]. */
    def node(data: A, children: Seq[TreeTableNodeEntry[A]] = Nil): TreeTableNode[A] =
        TreeTableNode(data, TreeTableNodeScope.resolve(children))
end TreeTableNodeScope

object TreeTableNodeScope:
    private[uic] def resolve[A](entries: Seq[TreeTableNodeEntry[A]]): List[TreeTableNode[A]] =
        Entry.resolve(new TreeTableNodeScope[A], entries)

    private[uic] def reading[A](entries: Seq[TreeTableNodeLiveEntry[A]])(using CanEqual[A, A], Frame): Reading[List[TreeTableNode[A]]] =
        Entry.reading(new TreeTableNodeScope[A], entries)
end TreeTableNodeScope

/** TreeTable: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TreeTable anatomy: `div.p-treetable.p-component[.p-treetable-hoverable]
  * [.p-treetable-gridlines][.p-treetable-sm|-lg]` >
  * `div.p-treetable-table-container` > `table.p-treetable-table[role=treegrid]`
  * with `th.p-treetable-header-cell[.p-treetable-sortable-column]
  * [.p-treetable-column-sorted]` headers (each a
  * `div.p-treetable-column-header-content` > `span.p-treetable-column-title` +
  * the sort icon) over recursive body rows; every cell wraps its content in
  * `div.p-treetable-body-cell-content`, and the FIRST column carries the
  * `button.p-treetable-node-toggle-button` whose indent is `depth × 1rem`
  * (Prime's inline `marginLeft`; leaf togglers keep their box but hide, Prime's
  * inline `visibility`)), so the extracted `@primeuix` treetable CSS applies.
  *
  * The header and body rows sit in real `thead.p-treetable-thead` and
  * `tbody.p-treetable-tbody` row groups (the DataTable precedent), which is what
  * the extracted sheet's row, cell, hover, selection and gridline rules are
  * scoped to.
  *
  * Rows are TYPED ([[TreeTableNode]] over `A`) and reuse the DataTable
  * [[Column]] carrier; every behavior is pure `(nodes, ui-state refs) → markup`:
  *   - `expanded(ref)`: a `Set[A]` of open rows, matched by `rowKey`; the toggler
  *     flips membership before firing `onNodeToggle`; children render only while
  *     their parent is expanded.
  *   - `selectionMode` + `selection(ref)`: `Single`/`Multiple` select on row
  *     click (Prime's checkbox cascade with partial states is deferred).
  *   - `sort(ref)`: DataTable's ordered [[SortKey]] spec, applied PER SIBLING LEVEL
  *     (children sort within their parent). Clicks follow the DataTable contract: a
  *     plain click cycles the only sorted column through all three states but merely
  *     reverses one of several, Ctrl or Cmd adds a column or advances one in place.
  *
  * Pagination is deferred (Prime paginates root rows only; revisit with a
  * concrete need).
  */
final case class TreeTable[A] private (
    // The roots `body` draws, resolved from `nodeSources` by the render.
    nodeList: List[TreeTableNode[A]] = Nil,
    nodeSources: List[TreeTableNodeLiveEntry[A]] = Nil,
    rowKeyF: Maybe[A => String] = Absent,
    cols: List[Column[A, AnyTable]] = Nil,
    expandedTyped: Maybe[SignalRef[Set[A]]] = Absent,
    selectedTyped: Maybe[SignalRef[Set[A]]] = Absent,
    sortSlot: Maybe[ReactiveValue[List[SortKey]]] = Absent,
    onSortF: Maybe[List[SortKey] => UI.Action] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    metaKeyFlag: Boolean = false,
    removableSortFlag: Boolean = true,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyContentV: Maybe[EmptyContent] = Absent,
    onNodeToggleF: Maybe[A => UI.Action] = Absent,
    onRowClickF: Maybe[A => UI.Action] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    hiddenCols: List[Column[A, AnyTable]] = Nil
) extends Node, HasEmptyContent, HasAccessibleName:
    type Self = TreeTable[A]

    // A bound row is matched by its own equality where no [[rowKey]] names it (see `keyFor`), so that equality decides.
    private given CanEqual[A, A] = CanEqual.derived

    /** Appends root nodes, each built directly or from `_` (see [[TreeTableNodeScope]]), or a signal of roots that change
      * while the table is shown (see [[LiveEntry]]).
      */
    def nodes(ns: TreeTableNodeLiveEntry[A]*): TreeTable[A] = copy(nodeSources = nodeSources ++ ns)

    /** The roots as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): Reading[List[TreeTableNode[A]]] = TreeTableNodeScope.reading(nodeSources)

    /** Stable row identity: the expansion/selection key and the event payloads.
      * Required for reliable expansion/selection (the fallback is the row's
      * position in the tree as authored, which does not survive data changes, and
      * matches a bound row by its own equality).
      */
    def rowKey(f: A => String): TreeTable[A] = copy(rowKeyF = Present(f))

    /** Appends columns (the DataTable [[Column]] carrier: text projection, body
      * template, `sortBy`, alignment). Each argument is authored against the table's row
      * type, so [[column]] needs no type argument of its own.
      *
      * The carrier is shared, but two of its options are not: `footer` and `rowSpan` mean
      * nothing over a hierarchy, and both return a [[FlatOnly]] column. The
      * [[AnyTableColumn]] evidence is what refuses one here, at compile time and with its
      * own message, rather than letting the option through to be dropped at render.
      */
    def columns[K <: FlatOnly](cs: ColumnOf[A, K]*)(using shared: AnyTableColumn[K]): TreeTable[A] =
        given ColumnScope[A] = new ColumnScope[A]()
        copy(cols = cols ++ cs.map(c => shared.widen((c: Column[A, K]))).toList)
    end columns

    /** Binds expansion two-way to `ref`, the set of expanded rows matched by [[rowKey]]. */
    def expanded(ref: SignalRef[Set[A]]): TreeTable[A] = copy(expandedTyped = Present(ref))

    /** Binds the selection two-way to `ref`, the set of chosen rows matched by [[rowKey]]. */
    def selection(ref: SignalRef[Set[A]]): TreeTable[A] = copy(selectedTyped = Present(ref))

    /** The ordered sort spec (applied per sibling level): [[SortKey]] entries, the first
      * sorting one being the primary key. Presses follow the DataTable contract, plain for
      * single-key and Ctrl or Cmd for multi-key in place.
      *
      * A `SignalRef` binds two way: a press writes it, then [[onSortChange]] hears the new
      * spec. A plain `Signal` or a constant binds one way: the table sorts by it and only
      * reports a press. A one-way sort with no [[onSortChange]] is reported in a card.
      */
    def sort(v: List[SortKey] | Signal[List[SortKey]]): TreeTable[A] = copy(sortSlot = Present(ReactiveValue[List[SortKey]](v)))

    /** Fired with the spec a header press asks for, after a two-way [[sort]] is written. */
    def onSortChange(f: List[SortKey] => UI.Action)(using Frame): TreeTable[A] = copy(onSortF = Present(Callback.of(f)))

    /** Whether a header click cycle reaches `Unsorted` (default) or stops at ascending
      * and descending, which is Prime's default.
      */
    def removableSort(v: Boolean): TreeTable[A] = copy(removableSortFlag = v)

    /** Selection semantics: `Single`/`Multiple` select on row click; `None`
      * (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): TreeTable[A] = copy(selectionModeV = v)

    /** Whether picking rows takes a modifier key (Prime's `metaKeySelection`).
      *
      * Off (the default, and Prime's) every click toggles its row. On, a plain click replaces
      * the selection with the row it landed on and Ctrl or Cmd toggles (`metaKey || ctrlKey`,
      * never one of the two). See [[SelectionPick]] for the rule every component offering this
      * shares.
      *
      * No shift-range, matching Prime, which ranges in `DataTable` only, and here that is also
      * the honest answer, since a range over a tree would have to say what it means to reach
      * across a collapsed branch.
      */
    def metaKeySelection(v: Boolean): TreeTable[A] = copy(metaKeyFlag = v)

    /** Cell borders on every edge (`.p-treetable-gridlines`). */
    def showGridlines(v: Boolean): TreeTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-treetable-sm` / default / `.p-treetable-lg` cell paddings. */
    def size(v: Size): TreeTable[A] = copy(sizeV = v)

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): TreeTable[A] = copy(emptyContentV = v)

    /** Fired with the row after a toggler press (after the expansion write). */
    def onNodeToggle(f: A => UI.Action)(using Frame): TreeTable[A] = copy(onNodeToggleF = Present(Callback.of(f)))

    /** Fired with the row after a row click (after the selection write). */
    def onRowClick(f: A => UI.Action)(using Frame): TreeTable[A] = copy(onRowClickF = Present(Callback.of(f)))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): TreeTable[A] = copy(accNameV = v)

    // ---- render ----

    private def keyOf(a: A, fallback: => String): String =
        rowKeyF match
            case Present(f) => f(a)
            case Absent     => fallback

    /** Every row of the tree as authored, with the key it is matched by, in tree order. The key falls back
      * to the row's authored path, so it does not move when a sort reorders the siblings.
      */
    private def keyedRows: List[(String, A)] =
        def loop(ns: List[TreeTableNode[A]], prefix: String): List[(String, A)] =
            ns.zipWithIndex.flatMap { (n, i) =>
                val path = if prefix.isEmpty then i.toString else s"$prefix.$i"
                (keyOf(n.data, path), n.data) :: loop(n.children, path)
            }
        loop(nodeList, "")
    end keyedRows

    /** The key a bound row is matched by: its [[rowKey]], else the authored path of the row equal to it. */
    private def keyFor(a: A): String =
        rowKeyF match
            case Present(f) => f(a)
            case Absent     => keyedRows.find(_._2.equals(a)).map(_._1).getOrElse("")

    /** The row a key names. */
    private def rowFor(key: String): Maybe[A] = Maybe.fromOption(keyedRows.find(_._1 == key).map(_._2))

    /** Applies `f` to a bound row set's keys and writes back the rows they name, the rows already held kept as
      * the caller wrote them.
      */
    private def updateRows(ref: SignalRef[Set[A]])(f: Set[String] => Set[String])(using Frame): UI.Action =
        ref.getAndUpdate { prev =>
            val now   = f(prev.map(keyFor))
            val kept  = prev.filter(a => now.contains(keyFor(a)))
            val known = kept.map(keyFor)
            kept ++ now.filterNot(known.contains).flatMap(k => rowFor(k).toList)
        }.unit

    private def rowInteractive: Boolean =
        selectionModeV != SelectionMode.None || onRowClickF.isDefined

    private[uic] def render(using Frame): UI =
        UI.mounted {
            for
                cmds   <- UI.commands
                base   <- cmds.freshId
                cursor <- Signal.initRef(Absent: Maybe[String])
            yield buildAll(base, Present(cursor), id => cmds.focusId(id))
        }.placeholder(buildAll("", Absent, _ => ()))

    /** The seam the golden tests render, since a mount shows only its placeholder there.
      *
      * The cursor is the mount's to mint, so the seam takes it rather than making one up: a
      * caller that hands `Absent` gets the table a placeholder shows, which is the shape the
      * golden renders assert.
      */
    private[uic] def wired(base: String, cursor: Maybe[SignalRef[Maybe[String]]], focus: String => UI.Action)(
        using Frame
    ): UI =
        buildAll(base, cursor, focus)

    private def buildAll(base: String, cursorRef: Maybe[SignalRef[Maybe[String]]], focus: String => UI.Action)(
        using Frame
    ): UI =
        // Where the reader last stood, which is what the tab stop follows. A key that moves the
        // cursor writes it and so does a click, so the pointer and the keyboard leave the table in
        // the same state.
        val seed: String => UI.Action = cursorRef match
            case Present(r) => (k: String) => r.set(Present(k))
            case Absent     => (_: String) => ()
        // Everything the table reads is ONE region (see [[Reading]]): a key writes the expansion, the cursor and the
        // selection together, and a region nested per signal would repaint the rows against the values it closed over.
        // The reactive `visible` and `sortable` flags are resolved here too, each to a plain boolean.
        val visible  = Reading.keyed(cols.zipWithIndex.flatMap((c, i) => c.visibleSig.toList.map(sig => (i, sig))))
        val sortable = Reading.keyed(cols.flatMap(c => c.sortableSig.toList.map(sig => (c.headerV, sig))))
        // The bound rows are matched to keys inside the region, against the roots it read: without a [[rowKey]] a row's
        // key is its place in the tree, which a live list moves.
        val exp    = Reading.orElse(expandedTyped, Set.empty[A])
        val sel    = Reading.orElse(selectedTyped, Set.empty[A])
        val sort   = sortSlot.fold(Reading.Fixed(List.empty[SortKey]))(Reading.of(_))
        val cursor = Reading.orElse(cursorRef, Absent: Maybe[String])
        entries.and(visible).and(sortable).and(exp).and(sel).and(sort).and(cursor).render {
            case ((((((ns, vis), flags), e), s), so), cu) =>
                val table = copy(nodeList = ns)
                table.narrowed(vis).body(e.map(table.keyFor), s.map(table.keyFor), so, cu, seed, flags, base, focus)
        }
    end buildAll

    /** The table without the columns their [[Column.visible]] flags hide (`vis` holds the reactive ones, resolved), as
      * DataTable does. The hidden ones are kept rather than dropped, since the sort spec still sorts by them: hiding a
      * column changes what the reader sees and not the order the rows are in.
      *
      * Hiding the first column moves the node toggler onto the one that takes its place,
      * which is what keeps the hierarchy readable however many columns are on the screen.
      */
    private def narrowed(vis: Map[Int, Boolean]): TreeTable[A] =
        if vis.isEmpty && cols.forall(_.visibleConst) then this
        else
            val (shown, hidden) = cols.zipWithIndex.partition((c, i) => vis.getOrElse(i, c.visibleConst))
            copy(cols = shown.map(_._1), hiddenCols = hidden.map(_._1))
    end narrowed

    private def body(
        exp: Set[String],
        sel: Set[String],
        sort: List[SortKey],
        cursor: Maybe[String],
        seed: String => UI.Action,
        flags: Map[String, Boolean],
        base: String,
        focus: String => UI.Action
    )(using
        Frame
    ): UI =
        // The paths whose headers the reader can actually click; the rest of the spec is
        // the caller's to keep, so no click may clear it.
        val interactive: Set[List[String]] =
            if sortSlot.isEmpty then Set.empty
            else
                cols.collect {
                    case c if c.isSortable(flags.getOrElse(c.headerV, c.sortableConst)) => List(c.headerV)
                }.toSet

        val headRow: UI =
            tr(cols.map(c =>
                toChild(headerCell(c, sort, flags.getOrElse(c.headerV, c.sortableConst), interactive))
            )*)

        val bodyRows: List[UI] =
            if nodeList.isEmpty then
                List(
                    tr.cssClass("p-treetable-empty-message")(
                        toChild(EmptyContent.render(emptyContentV, "No records found")(c =>
                            td.colspan(math.max(cols.length, 1))(c)
                        ))
                    )
                )
            else
                val seen    = visibleRows(nodeList, depth = 0, prefix = "", exp, sort)
                val navRows =
                    seen.map((n, d, path) => TreeNav.Row(d, keyOf(n.data, path), n.children.nonEmpty, exp.contains(keyOf(n.data, path))))
                val stop = tabStop(navRows, sel, cursor)
                seen.zipWithIndex.map { case ((n, d, path), i) =>
                    renderRow(n, d, path, i, navRows, stop, exp, sel, seed, base, focus)
                }

        // One `col` per column when any of them is sized, which is the only place a width
        // can go: a width on a cell sizes the row it is in, not the column it is under.
        val colGroupUI: List[UI] =
            if !cols.exists(_.widthV.isDefined) then Nil
            else
                val cs: List[UI] = cols.map(c =>
                    c.widthV match
                        case Present(w) => col.style(_.width(w.px))
                        case Absent     => col
                )
                List(colgroup(cs.map(toChild)*))

        var tbl = table.cssClass("p-treetable-table").role("treegrid")
        // A grid that takes more than one row at a time has to say so: a reader who cannot see the
        // rows has no other way to know whether picking a second one keeps the first.
        selectionModeV match
            case SelectionMode.Multiple | SelectionMode.Checkbox => tbl = tbl.aria("multiselectable", "true")
            case _                                               => ()
        if colGroupUI.nonEmpty then tbl = tbl.cssClass("p-uic-table-fixed")
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        val tableEl: UI = tbl(
            (colGroupUI ++ List[UI](
                thead.cssClass("p-treetable-thead")(toChild(headRow)),
                tbody.cssClass("p-treetable-tbody")(bodyRows.map(toChild)*)
            )).map(toChild)*
        )

        var root = div.cssClass("p-treetable").cssClass("p-component")
        // The arrows drive the cursor, so they must not also scroll the page under it. Only once
        // the mount has run: without a cursor there is nothing here that consumes them. No class
        // goes with it, unlike DataTable's `.p-uic-dt-nav`: that one scopes a ring the extracted
        // sheet does not have for a cell, and the sheet already rings a focused row itself, under
        // `:focus-visible`, which is where a keyboard ring belongs.
        if base.nonEmpty then root = root.preventScrollKeys
        if rowInteractive then root = root.cssClass("p-treetable-hoverable")
        if gridlinesFlag then root = root.cssClass("p-treetable-gridlines")
        sizeV match
            case Size.Small  => root = root.cssClass("p-treetable-sm")
            case Size.Large  => root = root.cssClass("p-treetable-lg")
            case Size.Normal => ()
        end match
        root((unheardSort :+ div.cssClass("p-treetable-table-container")(toChild(tableEl))).map(toChild)*)
    end body

    /** A sort bound one way that nothing hears: the headers take presses that change nothing. */
    private def unheardSort(using Frame): List[UI] =
        val sortable = cols.filter(c => c.isSortable(c.sortableConst)).map(_.headerV)
        if sortable.isEmpty || sortSlot.isEmpty || ReactiveValue.heard(sortSlot, onSortF) then Nil
        else
            List(KeyDiagnostics.card(
                "TreeTable",
                "the sort is bound one way and nothing hears a header press, so the reader cannot change it; add " +
                    "onSortChange, or bind a SignalRef",
                sortable
            ))
        end if
    end unheardSort

    /** Applies the sort spec to one sibling list (stable, back-to-front: the
      * DataTable technique, per level).
      */
    private def sortSiblings(ns: List[(TreeTableNode[A], Int)], sort: List[SortKey]): List[(TreeTableNode[A], Int)] =
        SortKey.sorting(sort).reverse.foldLeft(ns) { (rs, k) =>
            (cols ++ hiddenCols).find(c => k.path == List(c.headerV)).flatMap(_.orderingV.toOption) match
                case Some(ord) =>
                    val dir     = if k.direction == SortDirection.Ascending then ord else ord.reverse
                    val nodeOrd = Ordering.by[(TreeTableNode[A], Int), A](_._1.data)(using dir)
                    rs.sorted(using nodeOrd)
                case None => rs
        }

    /** One sortable/plain header cell with Prime's header-content anatomy. */
    private def headerCell(c: Column[A, AnyTable], sort: List[SortKey], flag: Boolean, interactive: Set[List[String]])(
        using Frame
    ): UI =
        val sortable  = c.isSortable(flag) && sortSlot.isDefined
        val sortingKs = SortKey.sorting(sort)
        // A TreeTable renders one header row, so a column's path is its header alone.
        val path      = List(c.headerV)
        val rank      = sortingKs.indexWhere(_.path == path)
        val direction = sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted)

        var cell = th.cssClass("p-treetable-header-cell")
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell
                .cssClass("p-treetable-sortable-column")
                .tabIndex(0)
                .onClick(e => toggleSort(path, e, interactive))
                // A `th` is not a button: the tab stop above is this table's own, so the two keys
                // that operate it are too, modifiers and all.
                .onKeyDown(e => activationOf(e).map(m => toggleSort(path, m, interactive)).getOrElse(()))
        end if
        if direction.isSorting then
            cell = cell
                .cssClass("p-treetable-column-sorted")
                .aria("sort", if direction == SortDirection.Ascending then "ascending" else "descending")
        end if

        // The neutral icon is the affordance and the directional one is state, so a column
        // the reader may not re-sort still says which way it currently sorts.
        val sortIcon: List[UI] =
            if !sortable && !direction.isSorting then Nil
            else
                val glyph = direction match
                    case SortDirection.Ascending  => Icons.sortAmountUpAlt
                    case SortDirection.Descending => Icons.sortAmountDown
                    case SortDirection.Unsorted   => Icons.sortAlt
                List(GlyphSvg(glyph, "p-treetable-sort-icon"))

        val sortBadge: List[UI] =
            if rank < 0 || sortingKs.length < 2 then Nil
            else List(Badge((rank + 1).toString).size(Size.Small).hostClass("p-treetable-sort-badge").render)

        cell(
            div.cssClass("p-treetable-column-header-content")(
                ((span.cssClass("p-treetable-column-title")(c.headerV): UI) :: (sortIcon ++ sortBadge)).map(toChild)*
            )
        )
    end headerCell

    /** Header click, the DataTable contract: plain sorts by this column alone, Ctrl or
      * Cmd adds it or advances it in place.
      */
    /** The keyboard's stand-in for a click, or `Absent` when the key was not an activation.
      *
      * The same shape [[DataTable]] uses, for the same reason: a `th` or a `tr` carries this
      * table's own tab stop rather than a native control's, so Enter and Space have to reach the
      * handler a click reaches, with the Ctrl or Cmd that adds a column to the sort intact.
      */
    private def activationOf(e: KeyboardEvent): Maybe[MouseEvent] =
        e.key match
            case Keyboard.Enter | Keyboard.Space => Present(MouseEvent(e.targetId, e.modifiers))
            case _                               => Absent

    private def toggleSort(path: List[String], e: MouseEvent, interactive: Set[List[String]])(using
        Frame
    ): UI.Action =
        val multi = e.modifiers.ctrl || e.modifiers.meta
        ReactiveValue.update(
            sortSlot,
            cur =>
                if multi then SortKey.cycle(cur, path, removableSortFlag, interactive.contains)
                else SortKey.plain(cur, path, removableSortFlag, interactive.contains),
            onSortF
        )
    end toggleSort

    /** The rows as the reader sees them, flat and in order: a collapsed node contributes one
      * row and its children none.
      *
      * The render and the keyboard both read THIS list, which is the point of having it. Two
      * traversals that walked the tree separately would disagree the moment one of them
      * changed, and disagreeing about which row is row 5 is a keyboard that moves the reader
      * somewhere they did not aim at.
      */
    private def visibleRows(
        nodes: List[TreeTableNode[A]],
        depth: Int,
        prefix: String,
        exp: Set[String],
        sort: List[SortKey]
    ): List[(TreeTableNode[A], Int, String)] =
        // The path counts the row's place as authored, not as sorted, so a sort leaves its key where it was.
        sortSiblings(nodes.zipWithIndex, sort).flatMap { (n, i) =>
            val path = if prefix.isEmpty then i.toString else s"$prefix.$i"
            val kids =
                if n.children.nonEmpty && exp.contains(keyOf(n.data, path)) then
                    visibleRows(n.children, depth + 1, path, exp, sort)
                else Nil
            (n, depth, path) :: kids
        }

    /** Which row is in the Tab order: where the reader last stood, else the row they chose, else
      * the first one.
      *
      * A roving tab stop that never moved would put a reader who tabs out of a long tree and back
      * at the top of it again, with the way back to their place being every arrow press they had
      * already made. The cursor outranks the selection because it is the more recent of the two,
      * and a cursor on a row that a collapse has taken off the screen falls back the same way.
      */
    private def tabStop(navRows: List[TreeNav.Row[String]], sel: Set[String], cursor: Maybe[String]): Int =
        val at = cursor.map(k => navRows.indexWhere(_.key == k)).getOrElse(-1)
        if at >= 0 then at
        else
            val chosen = navRows.indexWhere(r => sel.contains(r.key))
            if chosen >= 0 then chosen else 0
        end if
    end tabStop

    /** One row of the table. */
    private def renderRow(
        node: TreeTableNode[A],
        depth: Int,
        path: String,
        index: Int,
        navRows: List[TreeNav.Row[String]],
        tabStop: Int,
        exp: Set[String],
        sel: Set[String],
        seed: String => UI.Action,
        base: String,
        focus: String => UI.Action
    )(using Frame): UI =
        val id          = keyOf(node.data, path)
        val hasChildren = node.children.nonEmpty
        val isExp       = exp.contains(id)
        val isSel       = sel.contains(id)

        val tds: List[UI] = cols.zipWithIndex.map { (c, ci) =>
            var cell = td.role("cell")
            c.alignV match
                case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
                case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
                case ColumnAlign.Start  => ()
            end match

            val cellValue: HtmlChildVal = c.bodyF match
                case Present(f) => toChild(f(node.data))
                case Absent     => toChild(stringToUI(c.textF.map(_(node.data)).getOrElse("")))

            // The FIRST column carries the toggler: indent = depth × 1rem (Prime's
            // inline marginLeft); leaves keep the box but hide it (Prime's inline
            // visibility, expressed as the .p-uic-tt-toggle-hidden remainder class).
            val contentKids: List[HtmlChildVal] =
                if ci == 0 then
                    var toggler = button
                        .cssClass("p-treetable-node-toggle-button")
                        .jsProp("type", "button")
                        .tabIndex(-1)
                        .aria("label", if isExp then "Collapse" else "Expand")
                    if depth > 0 then
                        toggler = toggler.style(_.margin(Length.Px(0), Length.Px(0), Length.Px(0), Length.Calc(s"${depth}rem")))
                    if hasChildren then toggler = toggler.onClick(toggleNode(id)).stopPropagation(true)
                    else toggler = toggler.cssClass("p-uic-tt-toggle-hidden").aria("hidden", "true")
                    val togglerEl: UI = toggler(
                        toChild(GlyphSvg(if isExp then Icons.chevronDown else Icons.chevronRight, "p-treetable-node-toggle-icon"))
                    )
                    List(toChild(togglerEl), cellValue)
                else List(cellValue)

            var content = div.cssClass("p-treetable-body-cell-content")
            if ci == 0 then content = content.cssClass("p-treetable-body-cell-content-expander")
            cell(content(contentKids*))
        }

        var row = tr.role("row").aria("level", (depth + 1).toString)
        if hasChildren then row = row.aria("expanded", isExp.toString)
        if rowInteractive then row = row.cssClass("p-treetable-selectable-row")
        if base.isEmpty then
            // The placeholder: no mount has run, so there is nothing to move focus with yet, but the
            // treegrid is already the one tab stop it will be, on the chosen row else the first.
            // A selectable row still answers a click and the two activation keys.
            row = row.tabIndex(if index == tabStop then 0 else -1)
            if rowInteractive then
                row = row
                    .onClick((e: MouseEvent) => activate(id, e.modifiers.meta || e.modifiers.ctrl))
                    // A key activation carries no pointer, so it is always the plain kind.
                    .onKeyDown(e => if activationOf(e).isDefined then activate(id) else ())
            end if
        else
            // A treegrid is ONE tab stop, and it sits where the reader is: the cursor, else the
            // chosen row, else the first. The rest are reachable by the arrows, which is what the
            // role has been claiming all along. Navigation does not wait for selection to be
            // bound: a tree that cannot be selected can still be read, and a click still leaves
            // the cursor where the pointer put it.
            val clicked: MouseEvent => UI.Action = e =>
                if rowInteractive then seed(id).andThen(activate(id, e.modifiers.meta || e.modifiers.ctrl))
                else seed(id)
            row = row
                .id(rowId(base, index))
                .tabIndex(if index == tabStop then 0 else -1)
                .onClick(clicked)
                .onKeyDown(rowKey(base, navRows, index, seed, focus))
        end if
        if isSel then row = row.cssClass("p-treetable-row-selected")
        if selectionModeV != SelectionMode.None then row = row.aria("selected", isSel.toString)
        row(tds.map(toChild)*)
    end renderRow

    private def rowId(base: String, index: Int): String = s"$base-r$index"

    /** One key over one row, read against the tree as it stands on the screen.
      *
      * [[TreeNav]] is the whole semantics, unchanged from [[Tree]]: Down and Up walk the visible
      * rows, Right opens a closed parent and then steps into it, Left closes an open one and
      * otherwise climbs to the parent, Home and End reach the ends, and Enter or Space selects
      * the row and toggles it when it has children.
      *
      * This is a ROW cursor rather than a cell cursor, which the ARIA treegrid pattern allows and
      * which is what this table can actually do: it has no cell-level operation to move a cursor
      * to. [[DataTable]] puts the cursor on cells because a cell there opens an editor;
      * a cell here would be motion with nothing at the end of it.
      *
      * `Absent` means the key was never ours, so the browser keeps it.
      */
    private def rowKey(
        base: String,
        navRows: List[TreeNav.Row[String]],
        index: Int,
        seed: String => UI.Action,
        focus: String => UI.Action
    )(using
        Frame
    ): KeyboardEvent => UI.Action = e =>
        TreeNav.onKey(navRows, index, e.key) match
            case Absent        => ()
            case Present(step) =>
                val opEff: UI.Action = step.op match
                    case TreeNav.Op.Keep        => ()
                    case TreeNav.Op.Expand(k)   => setExpanded(k, open = true)
                    case TreeNav.Op.Collapse(k) => setExpanded(k, open = false)
                val moveEff: UI.Action =
                    if step.focus != index && navRows.isDefinedAt(step.focus) then
                        focus(rowId(base, step.focus)).andThen(seed(navRows(step.focus).key))
                    else ()
                val actEff: UI.Action =
                    if step.activate && rowInteractive && navRows.isDefinedAt(index) then activate(navRows(index).key)
                    else ()
                opEff.andThen(moveEff).andThen(actEff)

    /** Opens or closes one row by key, then fires `onNodeToggle` as a click would.
      *
      * [[TreeNav]] names the row it means rather than asking for a flip, so a key that says
      * "open" cannot close a row that a re-render opened underneath it.
      */
    private def setExpanded(id: String, open: Boolean)(using Frame): UI.Action =
        val write: UI.Action = expandedTyped match
            case Present(ref) => updateRows(ref)(cur => if open then cur + id else cur - id)
            case Absent       => ()
        write.andThen(fireRow(onNodeToggleF, id))
    end setExpanded

    /** A toggler press flips the row in the bound `expanded` set, then fires
      * `onNodeToggle`.
      */
    private def toggleNode(id: String)(using Frame): UI.Action =
        val write: UI.Action = expandedTyped match
            case Present(ref) => updateRows(ref)(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()
        write.andThen(fireRow(onNodeToggleF, id))
    end toggleNode

    /** Fires a row callback with the row `id` names, where there is one. */
    private def fireRow(f: Maybe[A => UI.Action], id: String)(using Frame): UI.Action =
        (f, rowFor(id)) match
            case (Present(g), Present(row)) => g(row)
            case _                          => ()

    /** A row click updates the bound selection per the mode, then fires `onRowClick`. */
    private def activate(id: String, meta: Boolean = false)(using Frame): UI.Action =
        val write: UI.Action = (selectedTyped, selectionModeV) match
            case (Present(ref), m) if m != SelectionMode.None =>
                updateRows(ref)(cur => SelectionPick.next(m, metaKeyFlag, meta, id, cur))
            case _ => ()
        write.andThen(fireRow(onRowClickF, id))
    end activate
end TreeTable

object TreeTable:
    def apply[A](): TreeTable[A] = new TreeTable[A]()
