package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** One row of a [[TreeTable]] — a recursive hand-authored carrier: the typed row
  * `data` plus its `children` (empty for a leaf). Row identity comes from the
  * table's `rowKey` projection over `data`.
  */
final case class TreeTableNode[A](data: A, children: List[TreeTableNode[A]] = Nil)

/** TreeTable — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TreeTable anatomy: `div.p-treetable.p-component[.p-treetable-hoverable]
  * [.p-treetable-gridlines][.p-treetable-sm|-lg]` >
  * `div.p-treetable-table-container` > `table.p-treetable-table[role=treegrid]`
  * with `th.p-treetable-header-cell[.p-treetable-sortable-column]
  * [.p-treetable-column-sorted]` headers (each a
  * `div.p-treetable-column-header-content` > `span.p-treetable-column-title` +
  * the sort icon) over recursive body rows — every cell wraps its content in
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
  *   - `expanded(ref)` — a `Set[String]` of open row keys; the toggler flips
  *     membership before firing `onNodeToggle`; children render only while
  *     their parent is expanded.
  *   - `selectionMode` + `selected(ref)` — `Single`/`Multiple` select on row
  *     click (Prime's checkbox cascade with partial states is deferred).
  *   - `sort(ref)`: DataTable's ordered [[SortKey]] spec, applied PER SIBLING LEVEL
  *     (children sort within their parent). Clicks follow the DataTable contract: a
  *     plain click cycles the only sorted column through all three states but merely
  *     reverses one of several, Ctrl or Cmd adds a column or advances one in place.
  *
  * Pagination is deferred (Prime paginates root rows only — revisit with a
  * concrete need).
  */
final case class TreeTable[A] private (
    nodeList: List[TreeTableNode[A]] = Nil,
    rowKeyF: Maybe[A => String] = Absent,
    cols: List[Column[A]] = Nil,
    expandedRef: Maybe[SignalRef[Set[String]]] = Absent,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    sortRef: Maybe[SignalRef[List[SortKey]]] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    removableSortFlag: Boolean = true,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyContentV: Maybe[EmptyContent] = Absent,
    onNodeToggleF: Maybe[String => Any < Async] = Absent,
    onRowClickF: Maybe[String => Any < Async] = Absent,
    accNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = TreeTable[A]

    /** Appends root nodes. */
    def nodes(ns: TreeTableNode[A]*): TreeTable[A] = copy(nodeList = nodeList ++ ns.toList)

    /** Stable row identity — the expansion/selection key and the event payloads.
      * Required for reliable expansion/selection (the fallback is the row's
      * depth-first position, which does not survive data changes).
      */
    def rowKey(f: A => String): TreeTable[A] = copy(rowKeyF = Present(f))

    /** Appends columns (the DataTable [[Column]] carrier — text projection, body
      * template, `sortBy`, alignment). Each argument is authored against the table's
      * row type, so [[column]] needs no type argument of its own.
      */
    def columns(cs: ColumnOf[A]*): TreeTable[A] =
        given ColumnScope[A] = new ColumnScope[A]()
        copy(cols = cols ++ cs.map(c => (c: Column[A])).toList)
    end columns

    /** Binds expansion two-way to `ref` (a set of [[rowKey]] ids). */
    def expanded(ref: SignalRef[Set[String]]): TreeTable[A] = copy(expandedRef = Present(ref))

    /** Binds selection two-way to `ref` (a set of [[rowKey]] ids). */
    def selected(ref: SignalRef[Set[String]]): TreeTable[A] = copy(selectedRef = Present(ref))

    /** Binds the ordered sort spec two-way (applied per sibling level): [[SortKey]]
      * entries, the first sorting one being the primary key. Clicks follow the DataTable
      * contract, plain for single-key and Ctrl or Cmd for multi-key in place.
      */
    def sort(ref: SignalRef[List[SortKey]]): TreeTable[A] = copy(sortRef = Present(ref))

    /** Whether a header click cycle reaches `Unsorted` (default) or stops at ascending
      * and descending, which is Prime's default.
      */
    def removableSort(v: Boolean): TreeTable[A] = copy(removableSortFlag = v)

    /** Selection semantics: `Single`/`Multiple` select on row click; `None`
      * (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): TreeTable[A] = copy(selectionModeV = v)

    /** Cell borders on every edge (`.p-treetable-gridlines`). */
    def showGridlines(v: Boolean): TreeTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-treetable-sm` / default / `.p-treetable-lg` cell paddings. */
    def size(v: Size): TreeTable[A] = copy(sizeV = v)

    /** Content of the full-width `tr.p-treetable-empty-message` row when there are no
      * rows.
      */
    def emptyContent(v: String): TreeTable[A] = copy(emptyContentV = Present(EmptyContent.const(v)))

    /** Reactive text: re-renders the empty slot in place on signal emission. */
    def emptyContent(sig: Signal[String]): TreeTable[A] = copy(emptyContentV = Present(EmptyContent.dyn(sig)))

    /** Arbitrary UI for the empty state: an icon over a line of explanation and the
      * button that creates the first record, rendered in the same slot the text would
      * occupy.
      */
    def emptyContent(ui: UI): TreeTable[A] = copy(emptyContentV = Present(EmptyContent.ui(ui)))

    /** Fired with the row key after a toggler press (after the expansion write). */
    def onNodeToggle(f: String => Any < Async): TreeTable[A] = copy(onNodeToggleF = Present(f))

    /** Fired with the row key after a row click (after the selection write). */
    def onRowClick(f: String => Any < Async): TreeTable[A] = copy(onRowClickF = Present(f))

    /** `aria-label` for the tree grid. */
    def accessibleName(v: String): TreeTable[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): TreeTable[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    // ---- render ----

    private def keyOf(a: A, fallback: => String): String =
        rowKeyF match
            case Present(f) => f(a)
            case Absent     => fallback

    private def rowInteractive: Boolean =
        selectionModeV != SelectionMode.None || onRowClickF.isDefined

    private def withRef[T](ref: Maybe[SignalRef[T]], fallback: T)(k: T => UI)(using Frame): UI =
        ref match
            case Present(r) => r.render(k)
            case Absent     => k(fallback)

    private[uic] def render(using Frame): UI =
        withRef(expandedRef, Set.empty[String]) { exp =>
            withRef(selectedRef, Set.empty[String]) { sel =>
                withRef(sortRef, List.empty[SortKey]) { sort =>
                    body(exp, sel, sort)
                }
            }
        }

    private def body(exp: Set[String], sel: Set[String], sort: List[SortKey])(using Frame): UI =
        val headRow: UI =
            tr(cols.map(c => toChild(headerCell(c, sort)))*)

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
                sortSiblings(nodeList, sort).zipWithIndex.flatMap { (n, i) =>
                    renderNode(n, depth = 0, path = i.toString, exp, sel, sort)
                }

        var tbl = table.cssClass("p-treetable-table").role("treegrid")
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        val tableEl: UI = tbl(
            toChild(thead.cssClass("p-treetable-thead")(toChild(headRow))),
            toChild(tbody.cssClass("p-treetable-tbody")(bodyRows.map(toChild)*))
        )

        var root = div.cssClass("p-treetable").cssClass("p-component")
        if rowInteractive then root = root.cssClass("p-treetable-hoverable")
        if gridlinesFlag then root = root.cssClass("p-treetable-gridlines")
        sizeV match
            case Size.Small  => root = root.cssClass("p-treetable-sm")
            case Size.Large  => root = root.cssClass("p-treetable-lg")
            case Size.Normal => ()
        end match
        root(div.cssClass("p-treetable-table-container")(toChild(tableEl)))
    end body

    /** Applies the sort spec to one sibling list (stable, back-to-front — the
      * DataTable technique, per level).
      */
    private def sortSiblings(ns: List[TreeTableNode[A]], sort: List[SortKey]): List[TreeTableNode[A]] =
        SortKey.sorting(sort).reverse.foldLeft(ns) { (rs, k) =>
            cols.find(_.headerV == k.column).flatMap(_.orderingV.toOption) match
                case Some(ord) =>
                    val dir     = if k.direction == SortDirection.Ascending then ord else ord.reverse
                    val nodeOrd = Ordering.by[TreeTableNode[A], A](_.data)(using dir)
                    rs.sorted(using nodeOrd)
                case None => rs
        }

    /** One sortable/plain header cell with Prime's header-content anatomy. */
    private def headerCell(c: Column[A], sort: List[SortKey])(using Frame): UI =
        val sortable  = c.orderingV.isDefined && sortRef.isDefined
        val sortingKs = SortKey.sorting(sort)
        val rank      = sortingKs.indexWhere(_.column == c.headerV)
        val direction = sort.find(_.column == c.headerV).map(_.direction).getOrElse(SortDirection.Unsorted)

        var cell = th.cssClass("p-treetable-header-cell")
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell.cssClass("p-treetable-sortable-column").tabIndex(0).onClick(e => toggleSort(c.headerV, e))
        if direction.isSorting then
            cell = cell
                .cssClass("p-treetable-column-sorted")
                .aria("sort", if direction == SortDirection.Ascending then "ascending" else "descending")
        end if

        val sortIcon: List[UI] =
            if !sortable then Nil
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
    private def toggleSort(key: String, e: MouseEvent)(using Frame): Any < Async =
        sortRef match
            case Present(ref) =>
                val multi = e.modifiers.ctrl || e.modifiers.meta
                ref.getAndUpdate(cur =>
                    if multi then SortKey.cycle(cur, key, removableSortFlag)
                    else SortKey.plain(cur, key, removableSortFlag)
                )
            case Absent => ()

    /** One row (plus, while expanded, its recursively rendered children). */
    private def renderNode(
        node: TreeTableNode[A],
        depth: Int,
        path: String,
        exp: Set[String],
        sel: Set[String],
        sort: List[SortKey]
    )(using Frame): List[UI] =
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
        if rowInteractive then row = row.cssClass("p-treetable-selectable-row").tabIndex(0).onClick(activate(id))
        if isSel then row = row.cssClass("p-treetable-row-selected")
        if selectionModeV != SelectionMode.None then row = row.aria("selected", isSel.toString)
        val rowEl: UI = row(tds.map(toChild)*)

        val childRows: List[UI] =
            if hasChildren && isExp then
                sortSiblings(node.children, sort).zipWithIndex.flatMap { (c, i) =>
                    renderNode(c, depth + 1, s"$path.$i", exp, sel, sort)
                }
            else Nil

        rowEl :: childRows
    end renderNode

    /** A toggler press flips the row in the bound `expanded` set, then fires
      * `onNodeToggle`.
      */
    private def toggleNode(id: String)(using Frame): Any < Async =
        val write: Any < Async = expandedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()
        val fire: Any < Async = onNodeToggleF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end toggleNode

    /** A row click updates the bound selection per the mode, then fires `onRowClick`. */
    private def activate(id: String)(using Frame): Any < Async =
        val write: Any < Async = (selectedRef, selectionModeV) match
            case (Present(ref), SelectionMode.Single | SelectionMode.Radio) =>
                ref.getAndUpdate(cur => if cur == Set(id) then Set.empty else Set(id))
            case (Present(ref), SelectionMode.Multiple | SelectionMode.Checkbox) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
        val fire: Any < Async = onRowClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end TreeTable

object TreeTable:
    def apply[A](): TreeTable[A] = new TreeTable[A]()
