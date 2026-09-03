package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal
import kyo.internal.NumberFormat
import kyo.uic.form.ErrorTranslator
import kyo.uic.form.FieldError

/** The address of one cell: the row's [[DataTable.rowKey]] and the column's path, the
  * same path the sort spec names a column by. It is what [[DataTable.editingCell]] holds,
  * and a pair rather than a single key because a cell is the crossing of the two.
  */
final case class CellPath(row: String, column: List[String]) derives CanEqual

/** What a right-click over a data row was aimed at: the row under the pointer, and the rows
  * selected at that moment.
  *
  * Both halves, because a context menu over a table is one question with two answers behind it.
  * A reader who right-clicks a row inside their selection means the selection; one who
  * right-clicks outside it means that row. Which of the two a given menu item follows is the
  * caller's rule, not the table's, so the table hands over what it knows and decides nothing:
  * `selected.contains(row)` is the whole of the usual test.
  *
  * `selected` is the ROWS, resolved against the table's full set in its own order, not the keys
  * the selection is stored as: a menu acts on records, and looking them back up is work the table
  * has already done. A [[DataTable.source]] table is the one place this cannot be complete, since a
  * lazily loaded table only holds the window it has fetched, so a selected row outside it is not
  * there to hand over.
  */
final case class RowContext[A](row: A, selected: Seq[A]) derives CanEqual

/** What the table is currently editing: the rows in row mode, the one cell in cell mode,
  * the per-column drafts an open editor writes into, and the error a refused commit left
  * behind. Threaded to the row renderer as one value, since all four reach the same cells.
  *
  * `drafts` is keyed by COLUMN path and not by cell, because only one row edits at a time:
  * one draft per editable column covers a single open cell and a whole open row alike, and
  * a fixed set of refs is what a mount can allocate once. It is deliberately a ref map and
  * not resolved values: the table never subscribes to a draft, so typing re-renders the
  * editor's own cell and nothing else.
  */
final private[uic] case class EditState(
    rows: Set[String],
    cell: Maybe[CellPath],
    drafts: Map[List[String], SignalRef[String]] = Map.empty,
    errorRef: Maybe[SignalRef[Maybe[(CellPath, FieldError)]]] = Absent,
    error: Maybe[(CellPath, FieldError)] = Absent,
    // False in the static projection, where the mount has not run and there are no drafts
    // to write into. The anatomy still renders; what it does not do is offer an
    // affordance that could not work, which is the same trade Select's placeholder makes.
    live: Boolean = false
):
    private[uic] def draftOf(path: List[String]): Maybe[SignalRef[String]] = Maybe.fromOption(drafts.get(path))

    private[uic] def errorAt(cell: CellPath): Maybe[FieldError] =
        error.flatMap((c, e) => if c == cell then Present(e) else Absent)
end EditState

/** What the keyboard needs to know about the grid it is moving over: the rows that are
  * actually on the screen, in render order, the id prefix their cells are addressed by,
  * and the command that moves DOM focus.
  *
  * There is no cursor here, and that is the design: the focused cell IS the focused `td`.
  * A cursor in a ref would make every arrow key a re-render of the table, and the browser
  * already holds the one piece of state involved.
  */
final private[uic] case class NavState[A](
    on: Boolean = false,
    idPrefix: String = "",
    rows: Vector[A] = Vector.empty[A],
    page: Int = 1,
    focus: String => Any < Async = (_: String) => ()
):
    private[uic] def indexOf(key: String, keyOf: A => String): Maybe[Int] =
        val i = rows.indexWhere(r => keyOf(r) == key)
        if i < 0 then Absent else Present(i)
end NavState

/** What the filter row needs to know: the filters that are bound, which of them this
  * table cannot apply, the open state of each column's mode menu, and whether the mount
  * has run.
  *
  * The specs are the CALLER's, so they are read fresh on every render; the open refs are
  * the table's own and are allocated once, one per filterable column, which is what lets
  * two menus be independent without a second piece of caller state.
  */
final private[uic] case class FilterState(
    open: Map[List[String], SignalRef[Boolean]] = Map.empty,
    live: Boolean = false,
    specs: Map[List[String], ColumnFilter] = Map.empty,
    unusable: Set[List[String]] = Set.empty,
    // A menu edits a DRAFT and applies it, so a table of a hundred thousand rows is not
    // re-filtered on the way to the second rule. A row display has no draft: one input is
    // the whole filter, and the keystroke that changes it is the apply.
    drafts: Map[List[String], SignalRef[ColumnFilter]] = Map.empty
):
    private[uic] def openOf(path: List[String]): Maybe[SignalRef[Boolean]]       = Maybe.fromOption(open.get(path))
    private[uic] def draftOf(path: List[String]): Maybe[SignalRef[ColumnFilter]] = Maybe.fromOption(drafts.get(path))
end FilterState

/** What one grab of a column boundary holds on to: where the pointer started, and the
  * two widths it started from. The paths are the handler's own, since a boundary only
  * ever moves the pair it sits between.
  */
final private[uic] case class ColumnGrab(startX: Double, width: Double, next: Double) derives CanEqual

/** A header cell on the move: the block of leaf columns it covers, the boundary the
  * reader has pulled it to, and the boundaries it is allowed to land on.
  *
  * `edges` are the absolute x of every boundary in the header, measured once when the
  * pointer goes down; a reorder changes no width, so what was measured on the grab is
  * still true when the pointer lets go, which is what keeps a drag from measuring the
  * header again on every frame the way Prime's does. `allowed` is the subset a drop may
  * land on. Both are kept, and not just the allowed ones, because the boundary the
  * pointer is nearest has to be found before it can be judged: snapping to the nearest
  * ALLOWED one instead would drag a column somewhere the reader never pointed as soon as
  * only one place was left to put it.
  *
  * `moved` is what separates a drag from a click: a header that sorts is also a header
  * that can be dragged, and a pointer that went down and up without travelling meant the
  * click. `done` carries that decision to the click which follows a real drag, and the
  * sort handler swallows it rather than re-sorting a column the reader only moved.
  */
final private[uic] case class ColumnDrag(
    from: Int,
    until: Int,
    target: Int,
    startX: Double,
    edges: List[Double] = Nil,
    allowed: List[Int] = Nil,
    moved: Boolean = false,
    done: Boolean = false
) derives CanEqual

/** A row drag in flight: which row was picked up, where a drop would put it, and where
  * the rendered rows sit, so a pointer position becomes one of those places.
  *
  * The edges are measured ONCE, on the grab, one per rendered row plus the bottom of the
  * last: moving a row changes no height, so the rows the reader picked up from are the
  * rows they let go over. They are measured in one round trip rather than one per row,
  * which is what makes this affordable over a page of rows. `first` is the index the
  * edges start at, counted in the whole list, which is what makes a drop on page three
  * land in the list rather than in the page.
  */
final private[uic] case class RowDrag(
    from: Int,
    target: Int,
    startY: Double,
    edges: List[Double] = Nil,
    first: Int = 0,
    moved: Boolean = false,
    done: Boolean = false
) derives CanEqual

/** What a reorderable row list needs once the mount has run: somewhere to park the drag,
  * the drag itself resolved for the rows that render it, and the index the rendered rows
  * start at, which is the page's offset into the list a drop rewrites.
  */
final private[uic] case class MoveState[A](
    drag: Maybe[SignalRef[Maybe[RowDrag]]] = Absent,
    held: Maybe[RowDrag] = Absent,
    all: Seq[A] = Nil,
    base: Int = 0,
    count: Int = 0,
    idPrefix: String = "",
    live: Boolean = false,
    measure: Seq[String] => Chunk[Rect] < Async = (_: Seq[String]) => Chunk.empty
)

/** What the column order needs once the mount has run: somewhere to park the drag that
  * is rewriting it, and the drag itself, resolved for the cells that render it.
  */
final private[uic] case class OrderState(
    drag: Maybe[SignalRef[Maybe[ColumnDrag]]] = Absent,
    held: Maybe[ColumnDrag] = Absent,
    requested: List[List[String]] = Nil
)

/** What the column widths need: the bound map, and, once the mount has run, the way to
  * measure a header cell and somewhere to park a grab.
  *
  * The widths are the CALLER's, read fresh on every render like the sort spec, so a
  * seeded map opens the table on the widths it names. What the table owns is the grab,
  * which lives only between a pointer going down on a boundary and coming back up.
  */
final private[uic] case class SizeState(
    widths: Map[List[String], Double] = Map.empty,
    idPrefix: String = "",
    live: Boolean = false,
    measure: Seq[String] => Chunk[Rect] < Async = (_: Seq[String]) => Chunk.empty,
    grab: Maybe[SignalRef[Maybe[ColumnGrab]]] = Absent
)

/** Where the scroll position of a windowed body lives once the mount has run.
  *
  * The ref is the table's own, since no caller supplies one and none has a use for it: it
  * says which rows are drawn, and the browser is what writes it. A static projection has
  * none and draws the window at rest.
  */
final private[uic] case class ScrollState(
    ref: Maybe[SignalRef[Double]] = Absent,
    headTop: Maybe[Signal[Maybe[Int]]] = Absent
)

/** Where one frozen cell holds: which edge it holds against, and how far from it.
  *
  * The distance is a [[Length]] and not a number because the columns it sums over are
  * not all measured in the same unit. A data column's width is the caller's, in pixels;
  * the checkbox, expander and row-editor columns are the component's, in a CSS variable
  * a theme can move, so an offset that reaches past one of them is a `calc` over both.
  */
final private[uic] case class FrozenSlot(edge: FrozenEdge, offset: Length)

/** Which cells of a row hold against an edge, resolved once per render.
  *
  * `cells` is one entry per VISIBLE leaf, so hiding a frozen column shifts the ones
  * behind it without anything else being asked. The leading cells (expander, checkbox)
  * and the trailing one (the row editor) hold with them: they sit between a frozen
  * column and its edge, so a scroll that carried them away would leave the frozen
  * column standing over a gap.
  */
final private[uic] case class FrozenPlan(
    lead: List[FrozenSlot] = Nil,
    cells: List[Maybe[FrozenSlot]] = Nil,
    trail: List[FrozenSlot] = Nil
):
    private[uic] def at(i: Int): Maybe[FrozenSlot]      = Maybe.fromOption(cells.lift(i)).flatten
    private[uic] def leadAt(i: Int): Maybe[FrozenSlot]  = Maybe.fromOption(lead.lift(i))
    private[uic] def trailAt(i: Int): Maybe[FrozenSlot] = Maybe.fromOption(trail.lift(i))

    /** The slot a header cell spanning leaves `from` until `until` holds in: the
      * outermost of theirs while they all hold against the same edge, and nothing at all
      * otherwise, since one cell cannot half scroll.
      */
    private[uic] def span(from: Int, until: Int): Maybe[FrozenSlot] =
        val slots = (from until until).toList.map(at)
        slots match
            case Present(first) :: _ if slots.forall(_.exists(_.edge == first.edge)) =>
                if first.edge == FrozenEdge.Start then Present(first) else slots.last
            case _ => Absent
        end match
    end span
end FrozenPlan

/** The row before and after a committed cell edit, plus the column that wrote it. */
final case class CellChange[A](rowKey: String, column: List[String], before: A, after: A)

/** The row before and after a committed row edit. */
final case class RowChange[A](rowKey: String, before: A, after: A)

/** Everything a reader changed about how a [[DataTable]] shows its rows, as one value.
  *
  * Prime persists the same set itself, under a `stateKey` in local or session storage,
  * because in Prime the state lives INSIDE the component and a caller has no other way to
  * reach it. Here every field is already a ref the caller bound, so this is a bundle
  * rather than a store: [[DataTable.state]] reads them, [[DataTable.restore]] writes them,
  * and where the value is kept between visits is the application's, which is the only
  * place that knows whether it belongs in a cookie, a URL, a profile row or nowhere.
  *
  * A field whose ref is not bound reads as its default and is not restored, since there
  * is nowhere for it to go.
  */
final case class TableState(
    sort: List[SortKey] = Nil,
    globalFilter: String = "",
    columnFilters: Map[List[String], ColumnFilter] = Map.empty,
    page: Int = 0,
    selected: Set[String] = Set.empty,
    expanded: Set[String] = Set.empty,
    expandedGroups: Set[GroupPath] = Set.empty,
    columnWidths: Map[List[String], Double] = Map.empty,
    columnOrder: List[List[String]] = Nil
) derives CanEqual

/** A row moved to another place: where it came from, where it went, and the list that
  * came out, both indices counted in that list rather than in the page it was seen on.
  */
final case class RowMove[A](from: Int, to: Int, rows: Seq[A])

/** DataTable — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DataTable anatomy: `div.p-datatable.p-component[.p-datatable-hoverable]
  * [.p-datatable-striped][.p-datatable-gridlines][.p-datatable-sm|-lg]` >
  * `div.p-datatable-table-container` > `table.p-datatable-table` with
  * `th.p-datatable-header-cell[.p-datatable-sortable-column][.p-datatable-column-sorted]`
  * headers (each a `div.p-datatable-column-header-content` >
  * `span.p-datatable-column-title` + `span.p-datatable-sort-icon` + the
  * multi-sort `span.p-datatable-sort-badge`) over body rows
  * carrying `.p-row-even`/`.p-row-odd`, `.p-datatable-selectable-row`, and
  * `.p-datatable-row-selected`, plus an embedded `div.p-paginator`), so the
  * extracted `@primeuix` datatable + paginator CSS applies.
  *
  * The header and body rows sit in real `thead.p-datatable-thead` and
  * `tbody.p-datatable-tbody` row groups, the `thead` holding one `tr` per level of
  * [[headerGroup]] nesting, which is what the extracted sheet's
  * row, cell, hover, selection, striping and gridline rules are scoped to.
  * Columns carrying a [[Column.footer]] add a `tfoot.p-datatable-tfoot` summary
  * row; `header`/`footer` are the two slots outside the table
  * (`div.p-datatable-header` above it, `div.p-datatable-footer` below the
  * paginator).
  *
  * Rows are TYPED and every behavior is pure `(data, ui-state refs) → markup`,
  * computed server-side at render:
  *   - `sort(ref)`: the table SORTS the rows itself, from an ordered [[SortKey]]
  *     spec whose first sorting entry is the primary key. An entry names its column
  *     by path, the labels of the [[headerGroup]]s around it followed by its own
  *     header, so a column in no group is named by its header alone and one header
  *     under two groups still names two different columns. What a plain header click does
  *     depends on how many columns sort. With one, it owns the whole cycle: ascending,
  *     descending, off. With several, it only REVERSES the clicked column, in place, so a
  *     spec built up over several clicks cannot lose a key because one header was clicked
  *     once too often; there, switching a column off belongs to Ctrl or Cmd. A plain
  *     click on a column that is not sorting makes it the single key the reader
  *     controls either way: entries they cannot reach, a column carrying
  *     `sortable(false)` or a path naming no column here, keep their slots ahead of
  *     it, since a click may only clear what a click could put back.
  *     Ctrl or Cmd click is the multi-key control: it adds a column, or advances the one
  *     already there WITHOUT moving it (ascending, descending, and, while
  *     [[removableSort]] is on, unsorted while keeping its slot). Holding the slot is
  *     what makes a mis-click cheap, since the next Ctrl click on the same header
  *     restores the column at the rank it had. While two or more columns sort, each
  *     sorted header carries its 1-based rank in a `.p-datatable-sort-badge`, which is
  *     the only thing that says which key wins.
  *   - `globalFilter(ref)` — case-insensitive contains-match over the columns'
  *     text projections.
  *   - `paginate(size)(pageRef)` — slices the (filtered, sorted) rows and renders
  *     the embedded Prime paginator; the page ref is 0-based and clamped.
  *   - `lazyRows(total)`: the rows are a WINDOW onto `total` rows filtered, sorted and
  *     paged somewhere else. The table renders them verbatim and paginates over the
  *     total, while every affordance keeps writing to the ref bound to it, which is the
  *     load event: a caller observing those refs fetches the page they describe. The
  *     total is a [[Total]], so a source that cannot count its rows says
  *     `Unknown(hasMore)` and the paginator grows a page at a time instead of counting
  *     them out. A column there sorts once it says so, with `sortable(true)` or a
  *     `sortBy` whose ordering goes unread. What the table holds is all it can name, so a
  *     select-all covers the page it was given and a `Column.footer` aggregate sums that
  *     page.
  *   - `source(rowSource)`: the same thing wired in one call from a [[RowSource]], which
  *     owns the block cache and the prefetch behind it, so stepping to the next page is
  *     served from the buffer and raises no busy mask at all.
  *   - `selectionMode` + `selected(ref)` — `Single`/`Multiple` select on row
  *     click; `Checkbox` renders Prime's checkbox column, whose header carries the
  *     binary select-all over every row that survived the global filter (`Radio`
  *     follows single-select semantics without a radio column for now).
  *   - `expanded(ref)` + `rowExpansionTemplate` — an expander-button column is
  *     auto-added; expanded rows are followed by a full-colspan
  *     `tr.p-datatable-row-expansion`.
  *   - `editingRows(ref)` / `editingCell(ref)` + `Column.editor`: the table decides
  *     WHICH cell shows its editor and nothing else, so the editor and the draft it
  *     writes into are the caller's. Row mode adds Prime's editor-button column at the
  *     trailing edge (init, save, cancel) and marks the row
  *     `tr.p-datatable-editing-row`; cell mode opens one cell at a time, marking the
  *     editable cells `.p-editable-column` and the open one `.p-cell-editing`. Enter
  *     commits and Escape discards in both, since a keystroke bubbles out of the
  *     caller's editor. `onRowEditInit` / `onCellEditInit` is where the draft gets
  *     seeded, the two `Save` and `Cancel` callbacks where it is committed or dropped;
  *     each runs BEFORE the state moves, so one that cannot complete leaves the cell
  *     where the reader can see it.
  *   - `groupBy(levels)`: one nested level per argument, outermost first. Each
  *     run of consecutive rows sharing a level's key becomes a group, headed by a
  *     `tr.p-datatable-row-group-header` and optionally closed by a summary row;
  *     `expandedGroups(ref)` makes them collapsible, keyed by [[GroupPath]].
  *   - `headerGroup(label)(columns)`: a labelled cell spanning the columns written
  *     under it, adding one header row per level of nesting. The spans are read off
  *     that tree rather than declared, so a header of any depth cannot fall out of
  *     step with the columns beneath it.
  *   - `Column.rowSpan(key)`: the other way to show a key, where that column's cells merge
  *     across their run, clipped by the merged columns left of them and by the
  *     innermost group, so two spans can never cross.
  *   - `loading(flag)`: a spinner over `.p-datatable-mask` covers the table.
  *   - `scrollHeight(css)`: caps and scrolls the container, pinning the row
  *     groups to its edges.
  *
  * `rowKey` supplies the stable id behind selection/expansion and the
  * `onRowClick` payload. It is REQUIRED in practice whenever any of those is
  * bound: without it rows fall back to their position in the ORIGINAL rows list,
  * which survives sorting and filtering (both computed here) but not a data
  * change — reorder the rows and every selection, expansion and click payload
  * re-associates with a different record, silently. A table that binds identity
  * without a key therefore renders a loud `.p-uic-key-error` card above itself
  * rather than shipping that failure to production data.
  */
final case class DataTable[A] private (
    rowsV: List[A] = Nil,
    rowKeyF: Maybe[A => String] = Absent,
    cols: List[ColumnTree[A]] = Nil,
    sortRef: Maybe[SignalRef[List[SortKey]]] = Absent,
    filterRef: Maybe[SignalRef[String]] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    paginatorF: Maybe[Paginator => Paginator] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedBinding: Maybe[ReactiveValue[Set[String]]] = Absent,
    selectableF: Maybe[A => Boolean] = Absent,
    selectedCellsRef: Maybe[SignalRef[Set[CellPath]]] = Absent,
    contextRowRef: Maybe[SignalRef[Maybe[String]]] = Absent,
    onRowContextF: Maybe[RowContext[A] => Any < Async] = Absent,
    csvSeparatorV: String = ",",
    rowClassF: Maybe[A => Seq[String]] = Absent,
    expandedRef: Maybe[SignalRef[Set[String]]] = Absent,
    expansionF: Maybe[A => UI] = Absent,
    groupsV: List[RowGroup[A]] = Nil,
    expandedGroupsRef: Maybe[SignalRef[Set[GroupPath]]] = Absent,
    removableSortFlag: Boolean = true,
    stripedFlag: Boolean = false,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyContentV: Maybe[EmptyContent] = Absent,
    onRowClickF: Maybe[String => Any < Async] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    headerV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    scrollHeightV: Maybe[String] = Absent,
    flexScrollFlag: Boolean = false,
    rowHeightV: Maybe[Int] = Absent,
    scrollOverscanV: Int = 3,
    sourceV: Maybe[RowSource[?, A]] = Absent,
    frozenRowsV: Maybe[ReactiveValue[Seq[A]]] = Absent,
    lazyTotalV: Maybe[ReactiveValue[Total]] = Absent,
    rowsSigV: Maybe[Signal[Seq[A]]] = Absent,
    editingRowsRef: Maybe[SignalRef[Set[String]]] = Absent,
    editingCellRef: Maybe[SignalRef[Maybe[CellPath]]] = Absent,
    rowsRefV: Maybe[SignalRef[Seq[A]]] = Absent,
    cellNavV: Maybe[Boolean] = Absent,
    onCellChangedF: Maybe[CellChange[A] => Any < Async] = Absent,
    onRowChangedF: Maybe[RowChange[A] => Any < Async] = Absent,
    translatorV: ErrorTranslator = ErrorTranslator.default,
    hiddenPaths: List[(List[String], Column[A, FlatOnly])] = Nil,
    columnFiltersRef: Maybe[SignalRef[Map[List[String], ColumnFilter]]] = Absent,
    filterDisplayV: FilterDisplay = FilterDisplay.Row,
    columnWidthsRef: Maybe[SignalRef[Map[List[String], Double]]] = Absent,
    resizeModeV: ColumnResizeMode = ColumnResizeMode.Fit,
    columnOrderRef: Maybe[SignalRef[List[List[String]]]] = Absent,
    reorderRowsFlag: Boolean = false,
    onRowReorderF: Maybe[RowMove[A] => Any < Async] = Absent,
    orderedPaths: List[List[String]] = Nil,
    idV: Maybe[String] = Absent
) extends Node, HasElementId, HasEmptyContent, HasAccessibleNameRef:
    type Self = DataTable[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): DataTable[A] = copy(idV = v)

    /** Appends data rows. */
    def rows(rs: Seq[A]): DataTable[A] = copy(rowsV = rowsV ++ rs.toList)

    /** Stable row identity — the selection/expansion key and the `onRowClick`
      * payload. Required for reliable selection/expansion.
      */
    def rowKey(f: A => String): DataTable[A] = copy(rowKeyF = Present(f))

    /** Appends columns. Each argument is authored against the table's row type, so
      * [[column]] needs no type argument of its own, and each may be a single [[column]]
      * or a [[headerGroup]] spanning several, which is what makes the header multi-row.
      */
    def columns(cs: HeaderOf[A]*): DataTable[A] =
        given ColumnScope[A] = new ColumnScope[A]()
        copy(cols = cols ++ cs.map(c => (c: ColumnTree[A])).toList)
    end columns

    /** The real columns, left to right, with any [[headerGroup]] flattened away. The
      * body, the footer and the filter are functions of these alone; only the header
      * reads the tree.
      */
    private lazy val leafCols: List[Column[A, FlatOnly]] = cols.flatMap(_.leaves)

    /** The same columns paired with the path that identifies them in the sort spec. */
    private lazy val leafPaths: List[(List[String], Column[A, FlatOnly])] = ColumnTree.leafPaths(cols)

    /** Every column, whether or not [[Column.visible]] hides it: the two lookups that
      * must not lose one. See [[withVisibleColumns]].
      */
    private def allPaths: List[(List[String], Column[A, FlatOnly])] = leafPaths ++ hiddenPaths

    /** The leaves that carry an edit pipeline, in column order. The drafts a mount
      * allocates are keyed by these paths, and a row edit walks them left to right.
      */
    private lazy val editableLeaves: List[(List[String], Column[A, FlatOnly])] = leafPaths.filter(_._2.isEditable)

    /** The leaves that carry a filter pipeline, in column order. A mount allocates one
      * menu-open ref per entry.
      */
    private lazy val filterableLeaves: List[(List[String], Column[A, FlatOnly])] = leafPaths.filter(_._2.isFilterable)

    /** Binds the ordered sort spec two-way: [[SortKey]] entries, the first sorting one
      * being the primary key.
      *
      * A plain header click sorts by that column alone. A click holding Ctrl (or Cmd)
      * adds the column to the spec, or advances the one already there WITHOUT moving it:
      * ascending, descending, and (while [[removableSort]] is on) unsorted while keeping
      * its slot. That last state is what makes an accidental click cheap, since one more
      * click on the same header puts the column back at the rank it had.
      */
    def sort(ref: SignalRef[List[SortKey]]): DataTable[A] = copy(sortRef = Present(ref))

    /** Whether a header click cycle reaches `Unsorted` (default) or stops at ascending
      * and descending, which is Prime's default.
      */
    def removableSort(v: Boolean): DataTable[A] = copy(removableSortFlag = v)

    /** Binds the global filter query: case-insensitive contains-match over the
      * columns' text projections.
      */
    def globalFilter(ref: SignalRef[String]): DataTable[A] = copy(filterRef = Present(ref))

    /** Binds the per-column filters two-way, keyed by the column path the sort spec
      * already names a column by, and gives the table its filter row: one input per
      * [[Column.filterBy]] column, with the mode menu beside it.
      *
      * The filters are read the way the reader sees the table. A column that is hidden
      * ([[Column.visible]]) does not filter, exactly as the global filter does not search
      * it, since an input the reader cannot reach is one they cannot clear either. That
      * is the opposite of the sort spec, which keeps sorting by a hidden column, and for
      * the same reason: filtering removes rows the reader is looking for, sorting only
      * moves them.
      *
      * Every bound filter has to pass, and the global filter with them.
      */

    /** Where a column's filter is edited (Prime's `filterDisplay`), and a row by default.
      *
      * `Row` is a second header row with one input per filterable column, which is the
      * shorter reach for one condition. `Menu` puts a funnel in each header cell instead,
      * opening Prime's filter popover: several conditions on one column, joined by Match
      * All or Match Any, with the buttons to add one, remove one, clear them and apply
      * them.
      *
      * The menu edits a DRAFT and applies it on the button, which is the one behavioural
      * difference and the reason Prime has the button at all: a table is not re-filtered
      * on the way to the second condition. Closing the panel without applying leaves the
      * table as it was.
      */
    def filterDisplay(v: FilterDisplay): DataTable[A] = copy(filterDisplayV = v)

    def columnFilters(ref: SignalRef[Map[List[String], ColumnFilter]]): DataTable[A] =
        copy(columnFiltersRef = Present(ref))

    /** Binds the column widths two-way, keyed by the column path the sort spec already
      * names a column by, and gives every boundary between two resizable columns a drag
      * handle.
      *
      * A seeded map opens the table on the widths it names; what the reader drags is
      * written straight back into it, so the widths outlive the table that rendered them
      * and a caller who wants them remembered has them already.
      *
      * A drag moves ONE boundary: the two columns it sits between trade width and the
      * total stays where it was, so the table never grows past the space it was given and
      * no column the reader is not touching moves. The last column has no boundary to its
      * right, and neither does one whose neighbour says [[Column.resizable]] is false.
      * [[columnResizeMode]] is what changes that.
      */
    def columnWidths(ref: SignalRef[Map[List[String], Double]]): DataTable[A] =
        copy(columnWidthsRef = Present(ref))

    /** What a resize drag moves, Prime's `columnResizeMode`, and `Fit` by default.
      *
      * Under `Expand` a drag moves one COLUMN rather than the boundary beside it: only
      * the grabbed column changes, and the table grows or shrinks by the same amount and
      * scrolls sideways in its container, which is what a table with more columns than
      * fit needs. The last column becomes draggable too, since it no longer needs a
      * neighbour to trade with, and Prime's `.p-datatable-resizable-table-fit` (which is
      * what hides that last handle) is not rendered.
      *
      * It costs one thing the fit mode does not: the table has to state its own width,
      * or the browser distributes what the columns leave over and the drag lands back
      * where it started. That width is the sum of the columns, so `Expand` needs every
      * visible column to have one, from [[Column.width]] or from the bound map. A column
      * without one is named in a card and the table resizes to fit instead, which is the
      * mode that needs no widths at all.
      */
    def columnResizeMode(v: ColumnResizeMode): DataTable[A] = copy(resizeModeV = v)

    /** Binds the order the columns render in, as their paths from left to right, and lets
      * the reader drag a header cell to another place.
      *
      * The order is the same currency the sort spec, the column filters and the widths
      * already speak, so a reordered table sorts and filters by the columns it was
      * authored with and nothing downstream has to be told they moved. A leaf the order
      * does not name keeps its authored place behind the ones it does, so a seeded
      * `List(List("Price"))` means "Price first" rather than "the rest is undefined", and
      * what a drag writes is the whole list, hidden columns included, so a column shown
      * again after a reorder comes back where it was and not at the end.
      *
      * What may be dropped where follows from the table rather than from a rule of its
      * own: a cell moves among its own siblings, since a header is a tree and one cell
      * cannot sit in two groups, and a drop that would leave a frozen column adrift is
      * not offered, since freezing is what the reader would silently lose.
      */
    def columnOrder(ref: SignalRef[List[List[String]]]): DataTable[A] =
        copy(columnOrderRef = Present(ref))

    /** Slices the rows into pages of `size` and renders the embedded paginator;
      * `ref` holds the 0-based page index (clamped at render).
      */
    def paginate(size: Int)(ref: SignalRef[Int]): DataTable[A] =
        copy(pageSizeV = Present(math.max(1, size)), pageRef = Present(ref))

    /** Configures the [[Paginator]] the table renders below its rows: which elements it
      * shows and in which order ([[Paginator.template]]), the current-page report, the
      * rows-per-page options, the size of the page-link window.
      *
      * Prime mirrors a handful of the paginator's own props onto the table
      * (`paginatorTemplate`, `currentPageReportTemplate`, `rowsPerPageOptions`, ...) and
      * a caller reaching for one it did not mirror has nowhere to go. Here the paginator
      * is a value, so the table takes a function over it and every setter it has is
      * reachable through one name.
      *
      * The four the table owns are applied AFTER `f` and cannot be overridden from here:
      * how many records there are, how many fit a page, which page is showing, and the
      * ref the page is written to. Those are the table's own state, and a paginator
      * disagreeing with the rows above it would page a list nobody is looking at.
      */
    def paginator(f: Paginator => Paginator): DataTable[A] = copy(paginatorF = Present(f))

    /** The rows are a WINDOW onto `total` rows filtered, sorted and paged somewhere else:
      * the table renders them verbatim and paginates over the total, instead of computing
      * any of the three itself.
      *
      * Binding the total is what says so, since locally the total IS the row count and a
      * table that is handed one has been handed something it could not have worked out.
      * Nothing else changes: the header still sorts, the filter row still takes queries,
      * the paginator still steps, and every one of them still writes to the ref bound to
      * it. There is no load event to bind because the refs already are one, a caller
      * observing them (`Signal.observe` over the spec they make up) fetches the page they
      * describe and stores it back.
      *
      * A column of such a table sorts once it SAYS it does, with `sortable(true)` or with
      * a `sortBy` whose ordering then goes unread, since the ordering is not what sorts it
      * and every column would otherwise offer a sort nobody asked for.
      *
      * What the table holds is also all it can name: a select-all covers the page it was
      * given, a `Column.footer` aggregate sums that page, and a `groupBy` run stops at its
      * edges.
      */
    def lazyRows(total: Int): DataTable[A] =
        lazyRows(Total.Known(math.max(0, total)))

    /** [[lazyRows]] where the size of the whole set is not a number: a cursor API or a
      * search index answers `Total.Unknown(hasMore)`, and the paginator then grows one
      * page at a time instead of counting them out. A `Signal[Total]` carries a total that
      * arrives with the page it counts.
      */
    def lazyRows(total: Total | Signal[Total]): DataTable[A] =
        copy(lazyTotalV = Present(ReactiveValue(total)))

    /** Takes the rows, the total, the busy flag and the paginator from a [[RowSource]],
      * which is the whole wiring of a lazily loaded table in one call.
      *
      * It sets what `rows`, `lazyRows`, `loading` and `paginate` set, so a later call to
      * any of those overrides it: bind the source first, then override.
      */
    def source(src: RowSource[?, A]): DataTable[A] =
        copy(
            sourceV = Present(src),
            rowsSigV = Present(src.rows),
            lazyTotalV = Present(ReactiveValue.Dyn(src.total)),
            loadingV = Present(BoolValue.Dyn(src.loading)),
            pageSizeV = Present(src.pageSize),
            pageRef = Present(src.page)
        )

    /** Selection semantics: `Single`/`Multiple` select on row click, `Checkbox`
      * via Prime's checkbox column; `None` (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): DataTable[A] = copy(selectionModeV = v)

    /** The selected rows, as a set of [[rowKey]] ids.
      *
      * A `SignalRef` binds two way — the table writes the reader's clicks back into it. A plain
      * `Signal` binds ONE way: the table paints what the signal says and never writes to it, which
      * is what a selection whose source of truth lives elsewhere needs — a URL, a parent, a record
      * in a normalized cache the rows are read from. A constant set is a selection that cannot
      * change at all.
      *
      * Reading needs no write access (`Signal.render` serves every case), so only four paths care
      * which form this is: a row click, the checkbox column, its select-all, and [[restore]]. Under
      * a one-way binding each of those has nowhere to write, so each does nothing — and
      * [[onRowClick]] still fires on every row click, which is how a caller owning the state
      * elsewhere closes the loop: the click writes THERE, and the new value arrives back through
      * this signal. `Checkbox` mode has no such outlet, and a one-way binding without an
      * `onRowClick` cannot move at all; both are reported as cards rather than left silent.
      */
    def selected(v: Set[String] | Signal[Set[String]]): DataTable[A] =
        copy(selectedBinding = Present(ReactiveValue(v)))

    /** The selection's write-back ref, where the caller bound one that can be written.
      *
      * `ReactiveVariable` is matched before `Dyn` on purpose: it IS-A `Dyn`, so the wrong order
      * silently swallows the two-way case (`kyo-ui/components/CLAUDE.md`).
      */
    private def selectedRef: Maybe[SignalRef[Set[String]]] = selectedBinding match
        case Present(ReactiveVariable(ref)) => Present(ref)
        case _                              => Absent

    /** Selects CELLS rather than rows (Prime's `cellSelection`), bound as a set of
      * [[CellPath]], which pairs a [[rowKey]] id with a column path.
      *
      * Binding it is the switch, and [[selectionMode]] says what a click means:
      * `Single` replaces the set, `Multiple` toggles the cell in it. A cell has no
      * identity until a row key and a column path are put together, which is why this is
      * a second binding and not a mode over the row one; binding both is a card, since
      * one click cannot mean two things.
      *
      * [[selectableWhen]] covers it too: a cell of a rejected row cannot be picked, which
      * is what Prime's Cell Selection disabled section does with `isDataSelectable`. Cell
      * EDITING claims the same click, so the two are a card as well, and editing keeps
      * the click.
      */
    def selectedCells(ref: SignalRef[Set[CellPath]]): DataTable[A] = copy(selectedCellsRef = Present(ref))

    /** Restricts selection to the rows `p` accepts (Prime's `isDataSelectable`).
      *
      * A rejected row still renders, still takes a row click through [[onRowClick]] and
      * still expands; what it loses is every path into the selection set: the click does
      * not select it, its checkbox is disabled, and select-all passes over it. The
      * predicate is read wherever the selection is written, so a row the caller starts
      * rejecting mid-session cannot stay selected by having been selectable earlier.
      */
    def selectableWhen(p: A => Boolean): DataTable[A] = copy(selectableF = Present(p))

    /** Binds the row a context menu was opened on, as a [[rowKey]] id (Prime's
      * `contextMenuSelection`).
      *
      * A right-click on a row writes the ref and marks that row with Prime's
      * `.p-datatable-contextmenu-row-selected`, which is a second, separate mark from
      * the selection: the reader is acting ON one row without changing what is selected.
      * Declaring it also suppresses the browser's own menu over the rows, which is what
      * a context menu of one's own has to do.
      *
      * The menu itself is [[kyo.uic.ContextMenu]], which wraps the table: this says which
      * row it was opened on, and the ref is the caller's to clear when the menu closes,
      * since the table cannot see a panel it does not render.
      */
    def contextMenuRow(ref: SignalRef[Maybe[String]]): DataTable[A] = copy(contextRowRef = Present(ref))

    /** Runs on a right-click over a row with what that click was aimed at (the row under the
      * pointer and the rows selected at that moment, as a [[RowContext]]), after [[contextMenuRow]]
      * is written.
      *
      * That is what a menu needs to decide what it should do, and why it is a pair: acting on the
      * selection when the reader right-clicked inside it and on the one row when they did not is
      * the rule almost every table wants, and neither half alone can express it. The table takes
      * no position on the rule itself and changes nothing about the selection.
      *
      * The menu is [[kyo.uic.ContextMenu]], which wraps the table and opens where the click was;
      * a menu item that runs later reads what this handler put in a ref of yours.
      */
    def onRowContextMenu(f: RowContext[A] => Any < Async): DataTable[A] = copy(onRowContextF = Present(f))

    /** The field separator [[csv]] writes (Prime's `csvSeparator`, and a comma by default). */
    def csvSeparator(v: String): DataTable[A] = copy(csvSeparatorV = v)

    /** `rows` as CSV, using this table's columns: one heading row from each exportable
      * column's [[Column.exportHeader]] or header, then one line per row from each
      * column's [[Column.exportAs]] or text projection.
      *
      * RFC 4180 quoting: a field is wrapped in quotes when it holds a quote, the
      * separator, or a line break, and its own quotes are doubled. Nothing else is
      * escaped, so a value survives the round trip through a spreadsheet.
      *
      * It takes the rows rather than reading them, which is what makes an export of the
      * selection one line: pass the rows the keys in the selection point at. [[csv(using
      * Frame)*]] is the other form, the rows as the reader is looking at them.
      *
      * A column [[Column.visible]] is hiding still exports. Visibility is about the
      * screen and [[Column.exportable]] is about the export, and a reader who narrowed
      * what they are looking at has not said anything about what a file should hold.
      */
    def csv(rows: Seq[A]): String =
        val cols  = leafPaths.map(_._2).filter(_.exportableFlag)
        val head  = cols.map(c => DataTable.csvField(c.exportTitle, csvSeparatorV))
        val lines = rows.toList.map(a => cols.map(c => DataTable.csvField(c.exportCell(a), csvSeparatorV)))
        (head :: lines).map(_.mkString(csvSeparatorV)).mkString("\n")
    end csv

    /** The rows the reader is looking at, as CSV: filtered and sorted the way the table
      * renders them, across every page rather than the one on the screen, which is what
      * Prime's `exportCSV()` exports too.
      *
      * It reads the bound sort and filter refs, so it is an effect rather than a value.
      * A prepared table ([[lazyRows]], [[source]]) exports the window it was handed, since
      * that is all it holds: the rest was never here to export.
      */
    def csv(using Frame): String < Async =
        for
            sort  <- currentOf(sortRef, List.empty[SortKey])
            query <- currentOf(filterRef, "")
            specs <- currentOf(columnFiltersRef, Map.empty[List[String], ColumnFilter])
            order <- currentOf(columnOrderRef, List.empty[List[String]])
            rows  <- currentRows
        yield
            // In the reader's column order too, since that is as much a part of what they
            // are looking at as the sort is.
            val shown = if order.isEmpty then this else copy(cols = ColumnTree.reorder(cols, order))
            shown.csv(arranged(rows.toList, sort, query, if lazyOn then Nil else filterReads(specs)))

    /** What the reader has changed about this table, read off the refs they changed it
      * through (Prime's `stateKey`/`stateStorage`, minus the storage).
      *
      * Prime persists this set itself because in Prime it lives inside the component;
      * here it is already the caller's, so this bundles it and where it is kept between
      * visits stays the application's. A ref that is not bound reads as its default.
      */
    def state(using Frame): TableState < Async =
        for
            sort   <- currentOf(sortRef, List.empty[SortKey])
            query  <- currentOf(filterRef, "")
            specs  <- currentOf(columnFiltersRef, Map.empty[List[String], ColumnFilter])
            page   <- currentOf(pageRef, 0)
            sel    <- currentValue(selectedBinding, Set.empty[String])
            exp    <- currentOf(expandedRef, Set.empty[String])
            groups <- currentOf(expandedGroupsRef, Set.empty[GroupPath])
            widths <- currentOf(columnWidthsRef, Map.empty[List[String], Double])
            order  <- currentOf(columnOrderRef, List.empty[List[String]])
        yield TableState(sort, query, specs, page, sel, exp, groups, widths, order)

    /** Puts a reader back where they were: writes each field of `s` into the ref it came
      * from, and skips the ones that are not bound, since they have nowhere to go.
      *
      * A one-way [[selected]] is skipped for the same reason a slot nobody bound is: the table does
      * not own that set, so restoring it is the caller's to do wherever they do own it.
      *
      * Nothing is validated here, because nothing has to be: a restored spec naming a
      * column this table no longer has, a width for one it never had, or a page past the
      * end are each already something the table reports or clamps at render.
      */
    def restore(s: TableState)(using Frame): Unit < Async =
        for
            _ <- writeIf(sortRef, s.sort)
            _ <- writeIf(filterRef, s.globalFilter)
            _ <- writeIf(columnFiltersRef, s.columnFilters)
            _ <- writeIf(pageRef, s.page)
            _ <- writeIf(selectedRef, s.selected)
            _ <- writeIf(expandedRef, s.expanded)
            _ <- writeIf(expandedGroupsRef, s.expandedGroups)
            _ <- writeIf(columnWidthsRef, s.columnWidths)
            _ <- writeIf(columnOrderRef, s.columnOrder)
        yield ()

    private def writeIf[T](ref: Maybe[SignalRef[T]], v: T)(using Frame): Unit < Async =
        ref match
            case Present(r) => r.set(v)
            case Absent     => ()

    /** One bound ref's current value, or the fallback where nothing is bound. */
    private def currentOf[T](ref: Maybe[SignalRef[T]], fallback: T)(using Frame): T < Async =
        ref match
            case Present(r) => r.get
            case Absent     => fallback

    /** The same for a slot bound as a [[ReactiveValue]], which may be one-way or constant.
      *
      * Reading is where the difference between the three bindings disappears, so this is what every
      * read of the selection goes through: asking `selectedRef` instead would read `Set.empty` for a
      * one-way binding and report it as "nothing selected".
      */
    private def currentValue[T](v: Maybe[ReactiveValue[T]], fallback: T)(using Frame): T < Async =
        v match
            case Present(ReactiveValue.Dyn(sig)) => sig.current
            case Present(ReactiveValue.Const(c)) => c
            case _                               => fallback

    /** The rows the table holds, whichever way it was given them. */
    private def currentRows(using Frame): Seq[A] < Async =
        if windowedSource then sourceV.get.window.current.map(_.rows)
        else
            rowsSigV match
                case Present(sig) => sig.current
                case Absent =>
                    rowsRefV match
                        case Present(ref) => ref.get
                        case Absent       => Kyo.lift(rowsV)

    /** Classes each data row carries beyond the ones the table gives it (Prime's
      * `rowClassName`).
      *
      * Prime takes a string or an object of class to condition; here it is the list a
      * class attribute is, so a condition is the caller's own `if` and an empty list is
      * no class. They are appended after the table's own, which leaves striping,
      * selection and the editing state saying what they say and lets a caller's rule
      * win on its own specificity rather than on order.
      *
      * A cell is styled through [[Column.body]], which is any UI; this is the row half
      * of the same question. Frozen rows are data rows and carry them too.
      */
    def rowClasses(f: A => Seq[String]): DataTable[A] = copy(rowClassF = Present(f))

    /** Binds row expansion two-way to `ref` (a set of [[rowKey]] ids); pair with
      * [[rowExpansionTemplate]].
      */
    def expanded(ref: SignalRef[Set[String]]): DataTable[A] = copy(expandedRef = Present(ref))

    /** Content of the full-colspan expansion row below an expanded data row; setting
      * it auto-adds the expander-button column.
      */
    def rowExpansionTemplate(f: A => UI): DataTable[A] = copy(expansionF = Present(f))

    /** Binds row editing two-way to `ref` (a set of [[rowKey]] ids): every column carrying
      * a [[Column.editable]] pipeline shows its editor for those rows, and an
      * editor-button column appears at the trailing edge with Prime's init, save and
      * cancel buttons.
      *
      * The table owns the draft as well as the state, and both come from the column: the
      * editor is the one its [[CellType]] carries, seeded from `read`, and save writes
      * back through `write`. Save fires [[onRowValueChanged]] with the row before and
      * after; a value that will not parse, or a rule that refuses it, leaves the row open
      * on the screen it failed on.
      */
    def editingRows(ref: SignalRef[Set[String]]): DataTable[A] = copy(editingRowsRef = Present(ref))

    /** Binds cell editing two-way to `ref`: at most one cell shows its column's editor,
      * and clicking a cell of an editable column moves the editor there. Enter commits,
      * Escape discards, and both clear the ref.
      *
      * A [[CellPath]] rather than a key, since a cell is a row crossed with a column.
      */
    def editingCell(ref: SignalRef[Maybe[CellPath]]): DataTable[A] = copy(editingCellRef = Present(ref))

    /** Whether the reader may move a cursor over the cells: arrows move it, Home and End
      * take it to the ends of a row (of the grid under ctrl or cmd), Page keys move it a
      * page, Enter and F2 open the cell it is on, and any printable key opens it on that
      * character. Tab visits the editable cells in reading order, wrapping into the next
      * row, and commits what it leaves.
      *
      * On by default once a column is editable, since editing without a keyboard is half a
      * feature; set it explicitly to give a read-only table a cursor, or to take one away
      * from an editable table that is driven by the mouse. A table that opts out renders
      * exactly the markup and the tab order it rendered before this existed.
      */
    def cellNavigation(v: Boolean): DataTable[A] = copy(cellNavV = Present(v))

    /** Binds the rows two-way, which is what lets the table apply a committed edit itself,
      * through the column's own `write`.
      *
      * The alternative is [[rows(rs:Seq[A])*]] plus [[onCellValueChanged]]: the table
      * computes the new row and hands it over instead of storing it, which is the mode for
      * a caller whose rows live somewhere the table cannot reach. Binding both is a
      * mistake and says so, since then two things claim to be the row list.
      *
      * The ref itself asks the row type for `CanEqual`, as every `Signal` does; a row type
      * that is a case class gets there with `derives CanEqual`.
      */
    def rows(ref: SignalRef[Seq[A]]): DataTable[A] = copy(rowsRefV = Present(ref))

    /** Binds the rows ONE-way to a derived signal: the table re-reads them on every
      * emission and writes nothing back.
      *
      * This is the shape a computed row list actually has. A list mapped out of a query
      * state, a paginated connection, a `combineLatestAll` over per-row signals — none of
      * them is a cell anyone can write into, so none of them is a `SignalRef`, and
      * mirroring one into a ref just to satisfy a binding puts a second thing in the
      * program that claims to be the row list. Reach for [[rows(ref:SignalRef[Seq[A]])*]]
      * when the table should be able to store an edit or a reorder itself, and for this
      * one otherwise; a read-only signal simply leaves those features unbound, exactly as
      * [[rows(rs:Seq[A])*]] does.
      *
      * A [[source]] binds the same slot, so binding both is the same mistake as binding
      * two row lists and says so.
      */
    def rows(sig: Signal[Seq[A]]): DataTable[A] = copy(rowsSigV = Present(sig))

    /** Runs after a committed cell edit, with the row before and after the column's write.
      * With [[rows(ref:SignalRef[Seq[A]])*]] bound this is a notification; without it, it
      * is the only exit, and storing the new row is the caller's.
      *
      * It does not fire when the value did not change, when the text will not parse, or
      * when a rule refuses it. A column-scoped variant is [[Column.onValueChanged]].
      */
    def onCellValueChanged(f: CellChange[A] => Any < Async): DataTable[A] = copy(onCellChangedF = Present(f))

    /** Runs after a committed row edit, once, with the row before and after every editable
      * column's write. The per-cell [[onCellValueChanged]] fires for each column that
      * actually changed, before this.
      */
    def onRowValueChanged(f: RowChange[A] => Any < Async): DataTable[A] = copy(onRowChangedF = Present(f))

    /** Lets the reader drag a row to another place, Prime's `reorderableRows`.
      *
      * It adds Prime's grip column at the leading edge, where Prime has the caller place
      * a `rowReorder` column of their own, and shows where a drop would land as a line on
      * the row it would land beside, where Prime positions two floating arrows measured
      * in JavaScript on every move.
      *
      * What a drop rewrites is the ROW LIST, so it needs somewhere to write: a bound
      * [[rows(ref:SignalRef[Seq[A]])*]] the table stores the new list into, or
      * [[onRowReorder]], which hands it over. It also needs the rows on the screen to be
      * the rows in the list, in that order, so while a sort spec, a filter or a row
      * grouping is deciding the order, and over a lazily loaded window where the order is
      * the query's, the grips are not offered and a card says which of them it is. Paging
      * is fine: a page is a contiguous slice, and both indices are counted in the list.
      */
    def reorderableRows(v: Boolean): DataTable[A] = copy(reorderRowsFlag = v)

    /** Runs after a completed row drag, with the list that came out of it.
      *
      * With [[rows(ref:SignalRef[Seq[A]])*]] bound this is a notification, the table has
      * already stored the list; without it, it is the only exit.
      */
    def onRowReorder(f: RowMove[A] => Any < Async): DataTable[A] = copy(onRowReorderF = Present(f))

    /** Turns a refused commit's [[kyo.uic.form.FieldError]] into the message shown under
      * the open editor. Defaults to `ErrorTranslator.default`, which shows the error's own
      * fallback text, or its code when it has none; an app with i18n passes its own, the
      * way a `Form` takes one.
      */
    def errorTranslator(t: ErrorTranslator): DataTable[A] = copy(translatorV = t)

    /** Groups the rows, one nested level per argument, outermost first. Every run of
      * CONSECUTIVE rows sharing a level's key becomes a group of that level, and the
      * levels below it split that group further.
      *
      * Grouping reads the order the table is about to render in, it does not impose one,
      * so pair it with a [[sort]] spec that leads with the same projections (or with rows
      * that already arrive ordered). Group a table by one key while it sorts by another
      * and the same key legitimately heads several runs, which is what the row order says.
      *
      * Each argument is authored against the table's row type, so [[group]] needs no type
      * argument of its own, and carries its own header row, summary row and visibility.
      */
    def groupBy(gs: GroupOf[A]*): DataTable[A] =
        given GroupScope[A] = new GroupScope[A]()
        copy(groupsV = groupsV ++ gs.map(g => (g: RowGroup[A])).toList)
    end groupBy

    /** Makes the groups collapsible and binds the set of EXPANDED paths two-way, so an
      * empty set starts with every group collapsed. Each level's header row grows a toggle
      * button; a collapsed group hides everything nested inside it, its own summary row
      * included, and keeps its slice of the page, since [[paginate]] slices rows before
      * they are grouped.
      *
      * The currency is [[GroupPath]] rather than a key, because a key alone does not
      * identify a group below the outermost level: the same brand occurs under every
      * category that sells one, and collapsing it in one place must not collapse it in the
      * others. A level with [[RowGroup.showHeader]] off has no toggle and stays open.
      */
    def expandedGroups(ref: SignalRef[Set[GroupPath]]): DataTable[A] = copy(expandedGroupsRef = Present(ref))

    /** Zebra striping (`.p-datatable-striped` + `.p-row-odd` rows). */
    def stripedRows(v: Boolean): DataTable[A] = copy(stripedFlag = v)

    /** Cell borders on every edge (`.p-datatable-gridlines`). */
    def showGridlines(v: Boolean): DataTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-datatable-sm` / default / `.p-datatable-lg` cell paddings. */
    def size(v: Size): DataTable[A] = copy(sizeV = v)

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): DataTable[A] = copy(emptyContentV = v)

    /** Fired with the row key after any selection write from a row click. */
    def onRowClick(f: String => Any < Async): DataTable[A] = copy(onRowClickF = Present(f))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): DataTable[A] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): DataTable[A] = copy(accNameRefV = v)

    /** Toolbar slot above the table (`div.p-datatable-header`), the place for a
      * global-filter input, a title, or action buttons.
      */
    def header(ui: UI): DataTable[A] = copy(headerV = Present(ui))

    /** Slot below the table and the paginator (`div.p-datatable-footer`). For per
      * column summary cells use [[Column.footer]], which renders a real `tfoot`.
      */
    def footer(ui: UI): DataTable[A] = copy(footerV = Present(ui))

    /** Busy state: a spinner over a dimming mask (`.p-datatable-mask`) covers the table while data is being
      * fetched. Bind a `Signal[Boolean]` to the data-fetch in-flight signal and the mask toggles in its own
      * sub-region without re-rendering the rows.
      */
    def loading(v: Boolean | Signal[Boolean]): DataTable[A] = copy(loadingV = Present(ReactiveValue(v)))

    /** Caps the table container at a CSS length and scrolls it, pinning the `thead`
      * (and the `tfoot`, when columns carry footers) to the container edges.
      */
    def scrollHeight(v: String): DataTable[A] = copy(scrollHeightV = Present(v))

    /** Scrolls the table against its PARENT's height instead of a length of its own
      * (Prime's `scrollHeight="flex"`).
      *
      * Prime says it with a magic value in the same string a length goes into; here it
      * is its own switch, because the two are different questions and only one of them
      * has an answer the table can read. A flex viewport needs a sized parent to be a
      * flex child of, so the height arrives at layout time and nothing in the render
      * knows it: [[scrollRows]] cannot window against it, and [[frozenRows]] can hold
      * against it, since holding needs a scroll container and not a number.
      */
    def flexScroll(v: Boolean): DataTable[A] = copy(flexScrollFlag = v)

    /** Renders only the rows the reader can see, plus `overscan` of them on either side,
      * inside the scroll container [[scrollHeight]] gives the table. `itemSize` is the row
      * height in px, and it is what turns a scroll position into a row index, so the rows
      * really have to be that tall. It reaches the row as a height, which the browser
      * reads as a floor: pick it at least as tall as the row renders on its own and every
      * row is exactly it, pick it shorter and the rows drift past the window that placed
      * them.
      *
      * The table then scrolls instead of paginating, so the page list is dropped and
      * nothing slices the rows. Over a [[source]] it is the demand that the scroll writes,
      * so nothing is held either: a list of any length costs the blocks the reader has
      * actually looked at, and how far it reaches comes from [[Total]], which is what
      * makes infinite scrolling fall out rather than need a mode. A row inside the window
      * that has not arrived yet is drawn as a row of `Skeleton` cells the same height, so
      * the geometry never moves under the reader, and the waiting is shown per row rather
      * than behind a mask over the whole table.
      *
      * Both halves of the arithmetic need every row to be exactly `itemSize` tall, which
      * three things break: [[groupBy]] and a [[RowGroup]] summary put rows of their own
      * between the data rows, [[rowExpansionTemplate]] puts one below the open row, and
      * [[Column.rowSpan]] merges cells across several. Each of them is named in a card and
      * leaves the table rendering every row, which is slower and right rather than faster
      * and wrong.
      */
    def scrollRows(itemSize: Int, overscan: Int = 3): DataTable[A] =
        copy(rowHeightV = Present(math.max(1, itemSize)), scrollOverscanV = math.max(0, overscan))

    /** Rows that hold under the header while the rest of the body scrolls past them (Prime's
      * `frozenValue`): a running total, the record being compared against, the one the reader pinned.
      *
      * They are a list of their OWN and not a subset of the body's, which is what lets them be a summary
      * rather than a duplicate: the table renders them in a second row group above the scrolling one, and a
      * row that is in both is a card, since two rows with one key are two rows the table cannot tell apart.
      *
      * Where they hold is the height of the header, which is the one number in this component nothing can
      * be told and nothing can compute: it is whatever the header cells came out as. The table observes it
      * and writes the offset, so a header that rewraps on a resize moves the frozen rows with it. Until the
      * first measurement lands they sit at the top of the body in flow, which is where they belong at rest.
      *
      * Needs a [[scrollHeight]], since a row can only hold against something that moves.
      *
      * A `Signal[Seq[A]]` tracks a pinned set the reader changes.
      */
    def frozenRows(v: Seq[A] | Signal[Seq[A]]): DataTable[A] = copy(frozenRowsV = Present(ReactiveValue(v)))
    // ---- render ----

    /** A row's identity. Without a [[rowKey]] it falls back to the row's position in the
      * appended list, which is why every feature that needs identity reports a missing
      * `rowKey` in a card: a bound row list has no appended one to index into, and a
      * position is not an identity across a sort in any case.
      */
    private def keyOf(a: A): String =
        rowKeyF match
            case Present(f) => f(a)
            case Absent     => rowsV.indexOf(a).toString

    private def rowClickSelects: Boolean =
        selectionModeV == SelectionMode.Single || selectionModeV == SelectionMode.Multiple ||
            selectionModeV == SelectionMode.Radio

    private def checkboxColumn: Boolean = selectionModeV == SelectionMode.Checkbox

    /** Whether a click picks CELLS. Cell editing claims the same click, so it wins, and
      * a mode that is about a checkbox column says nothing about a cell.
      */
    private def cellSelectOn: Boolean =
        selectedCellsRef.isDefined && editingCellRef.isEmpty && selectedBinding.isEmpty &&
            (selectionModeV == SelectionMode.Single || selectionModeV == SelectionMode.Multiple)

    /** Whether a right-click over a row means anything here. */
    private def contextRowOn: Boolean = contextRowRef.isDefined || onRowContextF.isDefined

    /** Whether this row may enter the selection at all ([[selectableWhen]]). */
    private def selectableAt(a: A): Boolean = selectableF.forall(_(a))

    private def expanderColumn: Boolean = expansionF.isDefined

    /** Whether Prime's row-reorder grip column is rendered. Asking for it IS the switch,
      * the way binding a state is for the expander and the row editor: the column is the
      * affordance, and a table that renders it with nothing behind it is what the cards
      * below are for.
      */
    private def handleColumn: Boolean = reorderRowsFlag

    /** Where a completed row drag can write. */
    private def rowMoveWritable: Boolean = rowsRefV.isDefined || onRowReorderF.isDefined

    /** What decides the row order instead of the list, which is what a drag cannot move
      * a row past. Empty means a drop lands where the reader let go.
      */
    private def rowOrderOwners(sort: List[SortKey], query: String, specs: Map[List[String], ColumnFilter]): List[String] =
        List(
            if SortKey.sorting(sort).isEmpty then Nil else List("a sort spec"),
            if query.isEmpty then Nil else List("a global filter"),
            if specs.values.forall(_.active.isEmpty) then Nil else List("a column filter"),
            if groupsV.isEmpty then Nil else List("row grouping"),
            if !lazyOn && sourceV.isEmpty then Nil else List("rows prepared elsewhere"),
            if !windowOn then Nil else List("a windowed body")
        ).flatten

    /** Row editing adds Prime's editor-button column, at the trailing edge. Binding the
      * state IS the switch, the way `expanded(ref)` is for the expander column: a second
      * flag would only be a way to bind one without the other.
      */
    private def editorColumn: Boolean = editingRowsRef.isDefined

    private def rowInteractive: Boolean = rowClickSelects || onRowClickF.isDefined

    /** Whether the rows are a window onto a larger set prepared elsewhere ([[lazyRows]]). */
    private def lazyOn: Boolean = lazyTotalV.isDefined

    /** Renders through whichever ui-state refs are bound (nested reactive nodes
      * render through in SSR).
      */
    private def withRef[T](ref: Maybe[SignalRef[T]], fallback: T)(k: T => UI)(using Frame): UI =
        ref match
            case Present(r) => r.render(k)
            case Absent     => k(fallback)

    /** The same for a [[ReactiveValue]] slot. A constant needs no reactive region at all, which is
      * what lets a constant selection paint under a pure render (no mount, no subscription).
      */
    private def withValue[T](v: Maybe[ReactiveValue[T]], fallback: T)(k: T => UI)(using Frame): UI =
        v match
            case Present(ReactiveValue.Dyn(sig)) => sig.render(k)
            case Present(ReactiveValue.Const(c)) => k(c)
            case _                               => k(fallback)

    /** Whether anything can be edited at all, which is what decides whether the table
      * needs state of its own.
      */
    private def editingBound: Boolean = editingCellRef.isDefined || editingRowsRef.isDefined

    /** Navigation follows editing unless the caller says otherwise, and never runs over a
      * windowed body: the cursor addresses cells by their position among the RENDERED
      * rows, and a window renders a few of them.
      */
    private def navOn: Boolean = !windowOn && cellNavV.getOrElse(leafCols.exists(_.isEditable))

    /** The viewport height in pixels, where [[scrollHeight]] is a length the window
      * arithmetic can read. Every other CSS length is one only the browser can resolve,
      * and how many rows fit is the first thing the arithmetic needs.
      */
    private def viewportPx: Maybe[Int] = scrollHeightV.flatMap(DataTable.pixels)

    /** Whether the table scrolls a viewport of its own, however that viewport is sized. */
    private def scrollingOn: Boolean = scrollHeightV.isDefined || flexScrollFlag

    /** A length and the parent's height cannot both say how tall the viewport is, and the
      * length is the one the table can read, so it wins and the other is reported.
      */
    private def flexOn: Boolean = flexScrollFlag && scrollHeightV.isEmpty

    /** The ways a table renders rows that are not all one height, which is what a windowed
      * body cannot have: each one is named in a card and turns the windowing off.
      */
    private def unevenRows: List[String] =
        List(
            if groupsV.nonEmpty then List("row grouping") else Nil,
            if expansionF.isDefined then List("row expansion") else Nil,
            leafPaths.collect { case (p, c) if c.rowSpanEq.isDefined => s"rowSpan on ${p.mkString(" / ")}" }
        ).flatten

    /** Whether rows hold under the header while the body scrolls past them. */
    private def frozenRowsOn: Boolean = frozenRowsV.isDefined

    /** Whether the body renders a window onto its rows rather than all of them. */
    private def windowOn: Boolean = rowHeightV.isDefined && viewportPx.isDefined && unevenRows.isEmpty

    /** Whether the window is fed by a [[RowSource]], which is the case where the rows the
      * table holds are a window too and the scroll writes what the next one should be.
      */
    private def windowedSource: Boolean = windowOn && sourceV.isDefined

    /** The window arithmetic, shared with [[VirtualScroller]] so a range asked for and a
      * range drawn are one calculation.
      */
    private def viewport: RowSource.Viewport =
        RowSource.Viewport(rowHeightV.getOrElse(1), viewportPx.getOrElse(1), scrollOverscanV)

    /** Whether the table owns state no caller supplies, which is what decides whether it
      * renders through a mount: the drafts and the standing error of an editing table,
      * and the menu-open state of a filter row.
      */
    private def ownsState: Boolean =
        editingBound || navOn || filterBound || resizeOn || reorderOn || windowOn || frozenRowsOn || handleColumn

    /** Whether the filter row is rendered: the filters have to be bound somewhere, and
      * some column has to carry a pipeline for the row to hold anything.
      */
    private def filterBound: Boolean = columnFiltersRef.isDefined && leafCols.exists(_.isFilterable)

    /** Whether the filters are edited in a second header row. */
    private def filterRowOn: Boolean = filterBound && filterDisplayV == FilterDisplay.Row

    /** Whether the filters are edited in a popover behind a funnel in each header cell. */
    private def filterMenuOn: Boolean = filterBound && filterDisplayV == FilterDisplay.Menu

    /** Whether any boundary is draggable: the widths have to be bound somewhere, and a
      * boundary needs a resizable column on both sides of it, so a single column has none
      * and neither does a table whose columns are all pinned.
      */
    private def resizeOn: Boolean =
        columnWidthsRef.isDefined && (resizeModeV match
            case ColumnResizeMode.Fit    => leafCols.sliding(2).exists(p => p.size == 2 && p.forall(_.resizableFlag))
            case ColumnResizeMode.Expand => leafCols.exists(_.resizableFlag))

    /** The visible columns with no width, which is what `Expand` cannot work without:
      * the table's own width is the sum of the columns, and a column with no width has
      * nothing to add to it.
      */
    private def unsizedLeaves(size: SizeState): List[List[String]] =
        leafPaths.collect { case (p, c) if widthOf(p, c, size).isEmpty => p }

    /** The mode a drag actually runs in: `Expand` falls back to `Fit` while a column has
      * no width, since the table would otherwise state a width it cannot compute.
      */
    private def resizeModeAt(size: SizeState): ColumnResizeMode =
        if resizeModeV == ColumnResizeMode.Expand && unsizedLeaves(size).isEmpty then ColumnResizeMode.Expand
        else ColumnResizeMode.Fit

    /** How wide the table states it is under `Expand`: the columns it renders, added up,
      * with the checkbox, expander and row-editor columns carried as the variables they
      * are sized by.
      */
    private def statedWidth(size: SizeState): Length =
        val px = leafPaths.flatMap((p, c) => widthOf(p, c, size).toList).sum
        offset(px, leadTerms ++ trailTerms)
    end statedWidth

    /** This column's width: the one the reader dragged if they have, the authored one
      * otherwise.
      */
    private def widthOf(path: List[String], c: Column[A, FlatOnly], size: SizeState): Maybe[Double] =
        Maybe.fromOption(size.widths.get(path)).orElse(c.widthV)

    /** Whether the boundary to the RIGHT of visible leaf `i` is draggable. A boundary
      * moves two columns, so it needs both of them to allow it, and the last column has
      * none: what is to its right is the edge of the table, which a fit-mode drag has
      * nothing to trade against.
      */
    private def resizableAt(i: Int, size: SizeState): Boolean =
        columnWidthsRef.isDefined && leafCols.lift(i).exists(_.resizableFlag) && (resizeModeAt(size) match
            case ColumnResizeMode.Fit    => i + 1 < leafCols.length && leafCols(i + 1).resizableFlag
            case ColumnResizeMode.Expand => true)

    /** The id one header cell carries so a grab can measure it. Every leaf header gets
      * one, the last included: it carries no handle itself, but it is the neighbour the
      * one before it trades width with.
      */
    private def headerId(i: Int, size: SizeState): String = s"${size.idPrefix}-h$i"

    /** The id the header row group carries so its height can be observed, which is what a
      * frozen row holds at.
      */
    private def headId(prefix: String): String = s"$prefix-head"

    /** Whether the table sizes its columns itself, which is what puts a `colgroup` in
      * front of the header and makes the widths in it authoritative.
      */
    private def hasWidths: Boolean = columnWidthsRef.isDefined || leafCols.exists(_.widthV.isDefined)

    /** Whether the reader may move a column, which needs the order bound to write into
      * and two columns willing to trade places.
      */
    private def reorderOn: Boolean = columnOrderRef.isDefined && leafCols.count(_.reorderableFlag) > 1

    /** The full order this table renders in, hidden columns included: what a drop rewrites.
      * Before a bound order has been resolved it is the authored one.
      */
    private def effectiveOrder: List[List[String]] =
        if orderedPaths.nonEmpty then orderedPaths else leafPaths.map(_._1)

    /** The leaf boundaries a header cell covering `[from, until)` may be dropped at.
      *
      * Two rules, and both are read off the table rather than declared. A cell moves among
      * its own SIBLINGS, since the header is a tree and a boundary inside another group
      * would ask one cell to sit in two places at once; and a boundary that would leave a
      * frozen column adrift is dropped from the list, since the reader would otherwise
      * lose the freezing by moving something else. The second is checked by moving the
      * columns and asking the frozen rule, so the two can never disagree.
      */
    private def dropStops(path: List[String], from: Int, until: Int): List[Int] =
        val parent = path.dropRight(1)
        val under  = leafPaths.zipWithIndex.collect { case ((p, _), i) if p.startsWith(parent) => i }
        if under.isEmpty then Nil
        else
            // The siblings tile the parent's leaves, so a change in the label at the
            // parent's own depth is where one sibling ends and the next begins.
            val starts = under.filter { i =>
                val label = leafPaths(i)._1.lift(parent.length)
                i == under.head || leafPaths(i - 1)._1.lift(parent.length) != label
            }
            val edges = leafCols.map(_.frozenV)
            val free  = leafCols.map(_.reorderableFlag)
            (starts :+ (under.last + 1)).filter { b =>
                (b < from || b > until) &&
                DataTable.adrift(DataTable.moveBlock(edges, from, until, b)).isEmpty &&
                DataTable.holdsPinned(free, from, until, b)
            }
        end if
    end dropStops

    /** Whether any column asks to be held against an edge, which is also what puts the
      * table in a scroll container: a column can only be frozen against something that
      * scrolls, and a wide table with no cap on its height still scrolls sideways.
      */
    private def frozenOn: Boolean = leafCols.exists(_.frozenV.isDefined)

    /** The component's own columns between the leading edge and the first data column,
      * as the CSS terms an offset reaching past them is written in.
      */
    private def leadTerms: List[String] =
        (if handleColumn then List(DataTable.HandleWidth) else Nil) ++
            (if expanderColumn then List(DataTable.ToggleWidth) else Nil) ++
            (if checkboxColumn then List(DataTable.SelectWidth) else Nil)

    /** Which leading column each of the three sits in, so a cell holds against the edge
      * at the distance the ones in front of it take up.
      */
    private def handleSlot: Int   = 0
    private def expanderSlot: Int = if handleColumn then 1 else 0
    private def checkboxSlot: Int = expanderSlot + (if expanderColumn then 1 else 0)

    private def trailTerms: List[String] = if editorColumn then List(DataTable.EditorWidth) else Nil

    /** One offset: the pixels of the data columns it reaches past, plus the variables of
      * the component's own. A sum with no variable in it stays a plain length, so a table
      * that freezes nothing but its own columns keeps `calc` out of its markup.
      */
    private def offset(px: Double, vars: List[String]): Length =
        if vars.isEmpty then Length.Px(px)
        else if px == 0.0 then Length.Calc(vars.mkString(" + "))
        else Length.Calc((vars :+ s"${NumberFormat.double(px)}px").mkString(" + "))

    /** The frozen columns with no width to compute an offset from. */
    private def unsizedFrozen(size: SizeState): List[List[String]] =
        leafPaths.collect { case (p, c) if c.frozenV.isDefined && widthOf(p, c, size).isEmpty => p }

    /** The frozen columns a free one stands between and their edge, which is the
      * arrangement that cannot be rendered: the free column scrolls, and it would carry
      * the frozen one along or leave it standing over the gap it left.
      */
    private def adriftFrozen: List[List[String]] =
        DataTable.adrift(leafCols.map(_.frozenV)).map(i => leafPaths(i)._1)

    /** The header groups spanning a frozen column and a free one, or two frozen against
      * opposite edges. One cell cannot half scroll, so the group says the columns under
      * it disagree about where they belong.
      */
    private def splitFrozenGroups: List[List[String]] =
        ColumnTree.spans(cols).flatten.collect {
            case sp if sp.node.asColumn.isEmpty && mixedEdges(sp.at, sp.at + sp.colspan) => sp.path
        }

    private def mixedEdges(from: Int, until: Int): Boolean =
        val edges = (from until until).toList.map(i => leafCols(i).frozenV)
        edges.exists(_.isDefined) && edges.distinct.length > 1

    /** Where every frozen cell of this table holds, or nothing at all.
      *
      * It is all or nothing on purpose. An offset is a sum over the columns between a
      * cell and its edge, so one column that cannot contribute its width makes every
      * offset behind it wrong, and a wrong offset is a column parked over the middle of
      * the table rather than a missing feature. The card says which column it was, the
      * same trade the filter row makes with a query it cannot read.
      */
    private def frozenPlan(size: SizeState): FrozenPlan =
        if !frozenOn || unsizedFrozen(size).nonEmpty || adriftFrozen.nonEmpty || splitFrozenGroups.nonEmpty then
            FrozenPlan()
        else
            val anyStart                                                 = leafCols.exists(_.frozenV.exists(_ == FrozenEdge.Start))
            val anyEnd                                                   = leafCols.exists(_.frozenV.exists(_ == FrozenEdge.End))
            def widthPx(p: List[String], c: Column[A, FlatOnly]): Double = widthOf(p, c, size).getOrElse(0.0)

            // Every one of the component's own leading columns stands between the edge and
            // every data column, so a data column's offset reaches past all of them and
            // the variable part of it is the same for all.
            val fromStart = leafPaths.foldLeft((0.0, List.empty[Maybe[FrozenSlot]])) {
                case ((px, acc), (p, c)) =>
                    if !c.frozenV.exists(_ == FrozenEdge.Start) then (px, acc :+ Absent)
                    else (px + widthPx(p, c), acc :+ Present(FrozenSlot(FrozenEdge.Start, offset(px, leadTerms))))
            }._2
            val fromEnd = leafPaths.reverse.foldLeft((0.0, List.empty[Maybe[FrozenSlot]])) {
                case ((px, acc), (p, c)) =>
                    if !c.frozenV.exists(_ == FrozenEdge.End) then (px, Absent :: acc)
                    else (px + widthPx(p, c), Present(FrozenSlot(FrozenEdge.End, offset(px, trailTerms))) :: acc)
            }._2
            FrozenPlan(
                lead = if !anyStart then Nil
                else leadTerms.indices.toList.map(i => FrozenSlot(FrozenEdge.Start, offset(0.0, leadTerms.take(i)))),
                cells = fromStart.zip(fromEnd).map((a, b) => a.orElse(b)),
                trail = if !anyEnd then Nil
                else trailTerms.indices.toList.map(i => FrozenSlot(FrozenEdge.End, offset(0.0, trailTerms.drop(i + 1))))
            )
        end if
    end frozenPlan

    /** Prime's reorderable header cell: the whole cell is the grip, which is what the
      * `cursor: move` in the extracted sheet says, plus the line showing where a drop
      * would land.
      *
      * Prime positions two floating arrows in JavaScript, measured against the container
      * on every move. The same information is a border on the cell the drop would land
      * beside, which needs no measurement at all and moves with the cell if anything else
      * re-renders underneath.
      */
    private def reorderCell(
        cell: Ast.Th,
        path: List[String],
        from: Int,
        until: Int,
        size: SizeState,
        order: OrderState,
        sorts: Boolean
    )(using Frame): Ast.Th =
        val mine  = (from until until).toList
        val stops = if !reorderOn || !mine.forall(i => leafCols(i).reorderableFlag) then Nil else dropStops(path, from, until)
        var c     = cell
        // Whatever cell the pointer was over when it let go still owes the browser a
        // click, and after the drop that is not the cell the drag started on: the columns
        // moved under it. A cell that does not sort takes that click and drops it, so the
        // press cannot reach the NEXT header the reader clicks.
        if reorderOn && size.live && !sorts then c = c.onClick(clearDrag(order))
        if stops.nonEmpty then
            c = c.cssClass("p-datatable-reorderable-column")
            // Before the mount runs there is nothing to park a drag in, so the cell says
            // what it is and does nothing, the trade the resize handle already makes.
            if size.live then
                c = c
                    .onPointerDown(e => beginReorder(from, until, stops, e, size, order))
                    .onPointerMove(e => dragReorder(e, order))
                    .onPointerUp(_ => endReorder(order))
            end if
        end if
        order.held match
            case Present(d) if d.moved && !d.done =>
                if d.from == from && d.until == until then c = c.cssClass("p-uic-dt-dragging")
                if d.target == from then c = c.cssClass("p-uic-dt-drop-before")
                else if d.target == until && until == leafCols.length then c = c.cssClass("p-uic-dt-drop-after")
            case _ => ()
        end match
        c
    end reorderCell

    /** A pointer going down on a header cell: measure the header once and remember where
      * every boundary this cell may land on sits.
      *
      * Measuring here and not on every move is what a reorder can afford and a resize
      * cannot: moving a column changes no width, so the header the reader grabbed is the
      * header they let go of.
      */
    private def beginReorder(from: Int, until: Int, stops: List[Int], e: PointerEvent, size: SizeState, order: OrderState)(
        using Frame
    ): Any < Async =
        order.drag match
            case Present(ref) =>
                for
                    rects <- size.measure(leafPaths.indices.toList.map(i => headerId(i, size)))
                    edges = rects.map(_.x).toList :+ rects.lastOption.map(r => r.x + r.width).getOrElse(0.0)
                    r <- ref.set(Present(ColumnDrag(from, until, from, e.rectX + e.x, edges, stops)))
                yield r
            case Absent => ()

    /** A pointer moving with a header cell: the drop lands on the boundary nearest to it.
      *
      * The state is written only when the answer CHANGES, which is what keeps a reorder
      * from re-rendering the table on every animation frame the way a resize has to: what
      * the reader sees is one line, and it moves when it moves to another boundary.
      */
    private def dragReorder(e: PointerEvent, order: OrderState)(using Frame): Any < Async =
        order.drag match
            case Present(ref) =>
                ref.get.map {
                    case Present(d) if !d.done =>
                        val x     = e.rectX + e.x
                        val moved = d.moved || math.abs(x - d.startX) >= DataTable.DragThreshold
                        val t     = if !moved then d.target else nearestStop(d, x)
                        if moved == d.moved && t == d.target then ()
                        else ref.set(Present(d.copy(target = t, moved = moved)))
                    case _ => ()
                }
            case Absent => ()

    /** The boundary a pointer at `x` is asking for: the nearest one in the header, taken
      * only if a drop may land there. A pointer over a place this column cannot go answers
      * where it started, so the line disappears and letting go writes nothing.
      */
    private def nearestStop(d: ColumnDrag, x: Double): Int =
        val near = d.edges.zipWithIndex.minByOption((at, _) => math.abs(at - x)).map(_._2).getOrElse(d.from)
        if d.allowed.contains(near) then near else d.from

    /** A pointer letting go: write the new order, or drop a press that never travelled.
      *
      * A completed drag leaves `done` behind rather than clearing the state, because the
      * browser still owes this cell the click that ends the press, and a header that
      * sorts would take it.
      */
    private def endReorder(order: OrderState)(using Frame): Any < Async =
        (order.drag, columnOrderRef) match
            case (Present(ref), Present(target)) =>
                ref.get.map {
                    case Present(d) if d.moved =>
                        val next               = reorderedPaths(d)
                        val write: Any < Async = if next == effectiveOrder then () else target.set(next)
                        write.andThen(ref.set(Present(ColumnDrag(d.from, d.until, d.target, d.startX, done = true))))
                    case _ => ref.set(Absent)
                }
            case _ => ()

    /** The full order a completed drag writes: the dragged block lifted out of the order
      * this table renders and put back in front of the column it was dropped on.
      *
      * It is written in PATHS and over the whole order, hidden columns included, so a
      * column a visibility flag is hiding keeps its place while the reader moves the
      * others, and comes back where it was rather than behind them.
      */
    private def reorderedPaths(d: ColumnDrag): List[List[String]] =
        if d.target >= d.from && d.target <= d.until then effectiveOrder
        else reorderedAround(d)

    private def reorderedAround(d: ColumnDrag): List[List[String]] =
        val block = (d.from until d.until).toList.map(i => leafPaths(i)._1)
        val rest  = effectiveOrder.filterNot(block.contains)
        if d.target >= leafPaths.length then rest ++ block
        else
            val i = rest.indexOf(leafPaths(d.target)._1)
            if i < 0 then rest ++ block else rest.take(i) ++ block ++ rest.drop(i)
        end if
    end reorderedAround

    /** A header click, which is a sort everywhere except right after a drag: the press
      * that moved a column also ends in a click, and re-sorting on it would answer a
      * gesture the reader did not make.
      */

    /** A pointer going down on a grip: measure the rendered rows once and remember where
      * each of them sits.
      *
      * Measured here and not on every move for the reason the column drag gives: moving a
      * row changes no height, so the rows the reader picked up from are the rows they let
      * go over. It is one round trip for the whole page rather than one per row, which is
      * what makes geometry over a list affordable at all.
      */
    private def beginRowMove(at: Int, e: PointerEvent, move: MoveState[A])(using Frame): Any < Async =
        move.drag match
            case Present(ref) =>
                val ids = (move.base until move.base + move.count).toList.map(DataTable.rowId(move.idPrefix, _))
                for
                    rects <- move.measure(ids)
                    edges = rects.map(_.y).toList :+ rects.lastOption.map(r => r.y + r.height).getOrElse(0.0)
                    r <- ref.set(Present(RowDrag(at, at, e.rectY + e.y, edges, move.base)))
                yield r
                end for
            case Absent => ()

    /** The pointer moving with a row held: the drop lands beside the nearest row edge.
      *
      * As with a column, the state is written only when the answer CHANGES, so a drag
      * re-renders the table once per row crossed rather than once per animation frame.
      */
    private def dragRowMove(e: PointerEvent, move: MoveState[A])(using Frame): Any < Async =
        move.drag match
            case Present(ref) =>
                ref.get.map {
                    case Present(d) if !d.done =>
                        val y     = e.rectY + e.y
                        val moved = d.moved || math.abs(y - d.startY) >= DataTable.DragThreshold
                        val t     = if !moved then d.target else DataTable.nearestRow(d, y)
                        if moved == d.moved && t == d.target then ()
                        else ref.set(Present(d.copy(target = t, moved = moved)))
                    case _ => ()
                }
            case Absent => ()

    /** A pointer letting go: write the list the drop produced, or drop a press that never
      * travelled. A completed drag is left parked rather than cleared, because the browser
      * still owes a click, and a row that selects would take it.
      */
    private def endRowMove(move: MoveState[A])(using Frame): Any < Async =
        move.drag match
            case Present(ref) =>
                ref.get.map {
                    case Present(d) if d.moved =>
                        writeRowMove(d, move).andThen(ref.set(Present(d.copy(done = true))))
                    case _ => ref.set(Absent)
                }
            case Absent => ()

    /** The list a completed drag produces, stored where it can be stored and handed on
      * either way. A drop back where the row came from, or on the boundary just below it,
      * moves nothing and is not a change.
      */
    private def writeRowMove(d: RowDrag, move: MoveState[A])(using Frame): Any < Async =
        if d.target == d.from || d.target == d.from + 1 then ()
        else
            val next = DataTable.moveBlock(move.all.toList, d.from, d.from + 1, d.target)
            val to   = if d.target > d.from then d.target - 1 else d.target
            val store: Any < Async = rowsRefV match
                case Present(ref) => ref.set(next)
                case Absent       => ()
            val notify: Any < Async = onRowReorderF match
                case Present(f) => f(RowMove(d.from, to, next))
                case Absent     => ()
            store.andThen(notify)
        end if
    end writeRowMove

    /** Takes the click a finished drag still owes and drops it, so the row the pointer
      * ended over is not also selected.
      */
    private def clearRowMove(move: MoveState[A])(using Frame): Any < Async =
        move.drag match
            case Present(ref) => ref.set(Absent)
            case Absent       => ()

    /** The keyboard's stand-in for a click, or `Absent` when the key was not an activation.
      *
      * A `th` or a `tr` carries this table's own tab stop rather than a native control's, so
      * Enter and Space have to reach the same handler a click does. Carrying the modifiers over
      * is what keeps Ctrl+Enter meaning what Ctrl+click means.
      */
    private def activationOf(e: KeyboardEvent): Maybe[MouseEvent] =
        e.key match
            case Keyboard.Enter | Keyboard.Space => Present(MouseEvent(e.targetId, e.modifiers))
            case _                               => Absent

    private def headerClick(path: List[String], e: MouseEvent, interactive: Set[List[String]], order: OrderState)(using
        Frame
    ): Any < Async =
        (order.drag, order.held) match
            case (Present(ref), Present(d)) if d.done => ref.set(Absent)
            case _                                    => toggleSort(path, e, interactive)

    /** Swallows the click a finished drag left behind, and nothing else. */
    private def clearDrag(order: OrderState)(using Frame): Any < Async =
        (order.drag, order.held) match
            case (Present(ref), Present(d)) if d.done => ref.set(Absent)
            case _                                    => ()

    /** Sticks one cell to its edge. Prime computes the same two properties in JavaScript,
      * measuring the cell before it on every render; here the widths are already known,
      * so the offset is written once, declaratively, and a resize drag moves it with the
      * column.
      */
    private def freeze(cell: Ast.Element, slot: Maybe[FrozenSlot]): Ast.Element =
        slot match
            case Absent => cell
            case Present(s) =>
                val stuck = cell.cssClass("p-datatable-frozen-column")
                if s.edge == FrozenEdge.Start then stuck.style(_.left(s.offset)) else stuck.style(_.right(s.offset))
        end match
    end freeze

    /** A table that edits owns two things no caller supplies: one draft per editable
      * column, and the error a refused commit left standing; a table that filters owns a
      * third, the open state of each column's mode menu. All of them are allocated in a
      * mount, since a pure render cannot, and the placeholder is the same table without
      * them, which is exactly what a table that does neither renders anyway.
      *
      * The drafts are allocated per COLUMN and not per cell because only one row edits at a
      * time, and a mount can only allocate a fixed set. They are never subscribed: typing
      * re-renders the editor's own cell and leaves the table alone.
      */
    private[uic] def render(using Frame): UI =
        def static: UI =
            renderWith(
                EditState(Set.empty, Absent),
                NavState[A](),
                FilterState(),
                SizeState(),
                OrderState(),
                ScrollState(),
                MoveState[A]()
            )
        if !ownsState then static
        else
            UI.mounted {
                for
                    cmds <- UI.commands
                    // A caller's own id wins; the mount mints one only when none was given,
                    // so the derived part ids (headers, rows, the frozen group) work unasked
                    // but are addressable when the caller cares. Same rule as Menu's base.
                    minted <- cmds.freshId
                    prefix = idV.getOrElse(minted)
                    drafts <- Kyo.foreach(editableLeaves)((path, _) => Signal.initRef("").map(path -> _))
                    err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
                    menus  <- Kyo.foreach(filterableLeaves)((path, _) => Signal.initRef(false).map(path -> _))
                    // One draft per filterable column, seeded when the menu opens. A row
                    // display never reads them, since one input is the whole filter.
                    drafts0 <- Kyo.foreach(filterableLeaves)((path, c) =>
                        Signal.initRef(ColumnFilter.empty(c.filterV.map(_.default).getOrElse(MatchMode.Contains)))
                            .map(path -> _)
                    )
                    held   <- Signal.initRef(Absent: Maybe[ColumnGrab])
                    moving <- Signal.initRef(Absent: Maybe[ColumnDrag])
                    rowsIn <- Signal.initRef(Absent: Maybe[RowDrag])
                    scroll <- Signal.initRef(0.0)
                    // The first range has to be asked for by someone, and no scroll has
                    // happened yet to ask for it.
                    _ <- ((sourceV, windowOn) match
                        case (Present(src), true) => src.demand.set(viewport.demand(0.0))
                        case _                    => ()
                    ): Unit < Async
                    // Where the frozen rows hold is the header's height, which is measured
                    // rather than declared, since it is whatever the header cells came out
                    // as. Writing it into a ref is what keeps a scroll, which re-measures
                    // too, from repainting anything: the height is the same, and a ref set
                    // to what it already holds tells nobody.
                    headTop <- Signal.initRef(Absent: Maybe[Int])
                yield
                    val tree = wired(
                        prefix,
                        drafts.toMap,
                        err,
                        id => cmds.focusId(id),
                        menus.toMap,
                        ids => cmds.requestMeasureByIds(ids),
                        Present(held),
                        Present(moving),
                        if windowOn then Present(scroll) else Absent,
                        if frozenRowsOn then Present(headTop) else Absent,
                        if handleColumn then Present(rowsIn) else Absent,
                        drafts0.toMap
                    )
                    if !frozenRowsOn then tree
                    else UI.fragment(headProbe(cmds, headId(prefix), headTop), tree)
            }.placeholder(static)
        end if
    end render

    /** An invisible sibling that starts watching the header once the table is IN the DOM.
      *
      * A nested mount's effect runs after the enclosing content is published, which is the
      * whole reason it exists: the observation measures its element immediately, and an id
      * requested before the publish resolves to nothing, so a measure started in the outer
      * mount would simply miss and the rows would sit unheld until something else moved.
      * The Overlay reposition trigger is the same shape.
      *
      * The height goes into a ref the table RENDERS rather than into a style patched onto
      * the group, because the group is inside the table's own subtree: a sort or a page
      * turn replaces it, and a patched style would go with it.
      */
    private def headProbe(cmds: UI.Commands, id: String, into: SignalRef[Maybe[Int]])(using Frame): UI =
        UI.mounted {
            cmds.observeViewportById(id).map(sig =>
                UI.fork(sig.observe(r => into.set(r.map(_.height.toInt))))
            ).andThen(UI.empty)
        }.placeholder(UI.empty)

    /** The subscription tree the mount publishes, and the seam golden tests render
      * directly: a top-down render shows a mounted region as its placeholder, so the
      * editing anatomy is only reachable here.
      */
    private[uic] def wired(
        idPrefix: String,
        drafts: Map[List[String], SignalRef[String]],
        errRef: SignalRef[Maybe[(CellPath, FieldError)]],
        focus: String => Any < Async,
        menus: Map[List[String], SignalRef[Boolean]] = Map.empty,
        measure: Seq[String] => Chunk[Rect] < Async = (_: Seq[String]) => Chunk.empty,
        held: Maybe[SignalRef[Maybe[ColumnGrab]]] = Absent,
        moving: Maybe[SignalRef[Maybe[ColumnDrag]]] = Absent,
        scroll: Maybe[SignalRef[Double]] = Absent,
        headTop: Maybe[Signal[Maybe[Int]]] = Absent,
        rowDrag: Maybe[SignalRef[Maybe[RowDrag]]] = Absent,
        filterDrafts: Map[List[String], SignalRef[ColumnFilter]] = Map.empty
    )(using Frame): UI =
        errRef.render(e =>
            withRef(editingRowsRef, Set.empty[String]) { editRows =>
                withRef(editingCellRef, Absent: Maybe[CellPath]) { editCell =>
                    withRef(moving, Absent: Maybe[ColumnDrag]) { drag =>
                        withRef(rowDrag, Absent: Maybe[RowDrag]) { rowHeld =>
                            renderWith(
                                EditState(editRows, editCell, drafts, Present(errRef), e, live = true),
                                NavState[A](on = navOn, idPrefix = idPrefix, focus = focus),
                                FilterState(open = menus, live = true, drafts = filterDrafts),
                                SizeState(idPrefix = idPrefix, live = true, measure = measure, grab = held),
                                OrderState(drag = moving, held = drag),
                                ScrollState(scroll, headTop),
                                MoveState[A](
                                    drag = rowDrag,
                                    held = rowHeld,
                                    idPrefix = idPrefix,
                                    live = true,
                                    measure = measure
                                )
                            )
                        }
                    }
                }
            }
        )

    private def renderWith(
        edit: EditState,
        nav: NavState[A],
        filter: FilterState,
        size: SizeState,
        order: OrderState,
        scroll: ScrollState,
        move: MoveState[A]
    )(using Frame): UI =
        withColumnOrder(order)((t, o) => t.withVisibleColumns(_.buildAll(edit, nav, filter, size, o, scroll, move)))

    /** Resolves a bound [[columnOrder]] and hands on the table with its columns in that
      * order, so the header, the body, the footer, the widths and the keyboard grid all
      * move together and no builder below has to ask where a column went.
      *
      * It runs BEFORE the visibility pass, which is what keeps the hidden columns' places:
      * the order this resolves to is the whole authored list, and it is the list a drop
      * rewrites, so hiding a column and moving another one does not lose the first one's
      * place.
      */
    private def withColumnOrder(order: OrderState)(k: (DataTable[A], OrderState) => UI)(using Frame): UI =
        columnOrderRef match
            case Absent => k(this, order)
            case Present(ref) =>
                ref.render { o =>
                    val tree = ColumnTree.reorder(cols, o)
                    k(copy(cols = tree, orderedPaths = ColumnTree.leafPaths(tree).map(_._1)), order.copy(requested = o))
                }

    /** Resolves every [[Column.visible]] flag and hands on the table WITHOUT the columns
      * they hide, so the header spans, the body cells, the footer, the filter, the colspans
      * and the keyboard grid all narrow at once and no builder below has to ask whether a
      * column is on the screen.
      *
      * The columns pruning removes are kept in `hiddenPaths` rather than dropped, for the
      * two questions the answer to which must not change when a column is hidden: which
      * spec entries this table can sort by, and whether anything here is editable. Both are
      * about what the table was AUTHORED with, so a reader hiding a column neither
      * reshuffles the rows nor raises a diagnostic.
      *
      * A table where no column carries the flag builds itself, so it renders exactly what
      * it rendered before the setter existed.
      */
    private def withVisibleColumns(k: DataTable[A] => UI)(using Frame): UI =
        val reactive = leafCols.zipWithIndex.flatMap((c, i) => c.visibleSig.toList.map(sig => (i, sig)))
        if reactive.isEmpty && leafCols.forall(_.visibleConst) then k(this)
        else
            def loop(rest: List[(Int, Signal[Boolean])], acc: Map[Int, Boolean]): UI =
                rest match
                    case Nil              => k(pruned(i => acc.getOrElse(i, leafCols(i).visibleConst)))
                    case (i, sig) :: tail => sig.render(b => loop(tail, acc + (i -> b)))
            loop(reactive, Map.empty)
        end if
    end withVisibleColumns

    /** This table narrowed to the columns `keep` accepts, with the rest parked in
      * `hiddenPaths`.
      */
    private def pruned(keep: Int => Boolean): DataTable[A] =
        copy(
            cols = ColumnTree.prune(cols, keep),
            hiddenPaths = leafPaths.zipWithIndex.collect { case (entry, i) if !keep(i) => entry }
        )

    private def buildAll(
        edit: EditState,
        nav: NavState[A],
        filter: FilterState,
        size: SizeState,
        order: OrderState,
        scroll: ScrollState,
        move: MoveState[A]
    )(using Frame): UI =
        withSortableFlags { flags =>
            withFrozenRows { held =>
                withRows { (rows, offset) =>
                    withRef(sortRef, List.empty[SortKey]) { sort =>
                        withRef(filterRef, "") { query =>
                            withRef(columnFiltersRef, Map.empty[List[String], ColumnFilter]) { specs =>
                                withRef(columnWidthsRef, Map.empty[List[String], Double]) { widths =>
                                    withRef(pageRef, 0) { page =>
                                        withValue(selectedBinding, Set.empty[String]) { sel =>
                                            withRef(contextRowRef, Absent: Maybe[String]) { ctx =>
                                                withRef(selectedCellsRef, Set.empty[CellPath]) { cells =>
                                                    withRef(expandedRef, Set.empty[String]) { exp =>
                                                        withRef(expandedGroupsRef, Set.empty[GroupPath]) { groups =>
                                                            withTotal { total =>
                                                                body(
                                                                    rows,
                                                                    offset,
                                                                    held,
                                                                    sort,
                                                                    query,
                                                                    page,
                                                                    total,
                                                                    sel,
                                                                    ctx,
                                                                    cells,
                                                                    exp,
                                                                    groups,
                                                                    flags,
                                                                    edit,
                                                                    nav,
                                                                    filter.copy(specs = specs),
                                                                    size.copy(widths = widths),
                                                                    order,
                                                                    scroll,
                                                                    move
                                                                )
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

    /** The rows a query, a filter spec and a sort leave, in the order they leave them in:
      * steps 1, 1b and 2 of what the body renders, without the paging step 3.
      *
      * It is one method rather than three lines inside the render because an export is the
      * same question asked away from the screen: the rows a reader is looking at, across
      * every page. Two implementations of that would be two answers.
      *
      * A prepared table ([[lazyRows]], [[source]]) skips all three: it was handed a window
      * that is already filtered, sorted and paged, so filtering a page, sorting a page and
      * slicing a slice would each be wrong.
      */
    private def arranged(
        rows: List[A],
        sort: List[SortKey],
        query: String,
        reads: List[(List[String], Maybe[A => Boolean])]
    ): List[A] =
        if lazyOn then rows
        else
            // 1. Global filter: contains-match over the columns' text projections.
            val global =
                if query.isEmpty then rows
                else
                    val q = query.toLowerCase
                    rows.filter(a => leafCols.exists(c => c.textF.exists(f => f(a).toLowerCase.contains(q))))
            // 1b. Column filters: every bound one has to pass. A query this table cannot
            //     read as a value of its column's type filters nothing and says so on its
            //     own input, rather than emptying the table behind a typo.
            val filtered = reads.foldLeft(global)((rs, r) => r._2.fold(rs)(p => rs.filter(p)))
            // 2. Sort: apply the SORTING entries back-to-front through stable sorts, so the
            //    first one ends up the primary key. Unsorted entries hold a slot in the
            //    priority order and contribute nothing here.
            SortKey.sorting(sort).reverse.foldLeft(filtered) { (rs, k) =>
                allPaths.find(_._1 == k.path).flatMap(_._2.orderingV.toOption) match
                    case Some(ord) =>
                        rs.sorted(using if k.direction == SortDirection.Ascending then ord else ord.reverse)
                    case None => rs
            }
        end if
    end arranged

    /** The rows that hold under the header, resolved before the table builds: the card that
      * catches a row held and scrolling at once needs them beside the body's, and the group
      * that renders them needs them anyway.
      */
    private def withFrozenRows(k: Seq[A] => UI)(using Frame): UI =
        frozenRowsV match
            case Absent => k(Nil)
            case Present(rv) =>
                rv.dyn match
                    case Present(sig) => sig.render(k)
                    case Absent       => k(rv.const.getOrElse(Nil))

    /** The rows the table renders, and the row index the first of them sits at: the bound
      * list when there is one, the appended list otherwise. A bound list is what lets a
      * committed edit reach the screen, since the table stores the new row into it.
      *
      * The offset is zero everywhere but under a windowed [[source]], which publishes a
      * window rather than a page: there the rows have to be placed by their own index, and
      * the one they were asked for at is not the one they came back at while a fetch is in
      * flight.
      */
    private def withRows(k: (Seq[A], Int) => UI)(using Frame): UI =
        if windowedSource then sourceV.get.window.render(w => k(w.rows, w.offset))
        else
            rowsSigV match
                case Present(sig) => sig.render(k(_, 0))
                case Absent =>
                    rowsRefV match
                        case Present(ref) => ref.render(k(_, 0))
                        case Absent       => k(rowsV, 0)

    /** How many rows the page the table was given came out of, zero where it was given no
      * total and computes its own. It is resolved innermost, so a total that moves without
      * its rows repaints the paginator and nothing above it.
      */
    private def withTotal(k: Total => UI)(using Frame): UI =
        lazyTotalV.flatMap(_.dyn) match
            case Present(sig) => sig.render(k)
            case _            => k(lazyTotalV.flatMap(_.const).getOrElse(Total.Unknown(false)))

    /** Resolves every reactive [[Column.sortable]] flag to a plain boolean before the table
      * builds, one nested subscription per signal-backed column.
      *
      * The flag reaches the class, the tab stop, the icon and the click handler of one
      * header cell, so patching it in place would mean keeping four channels in step. The
      * table already re-renders its whole subtree whenever a bound signal moves, sort,
      * filter, page, selection, expansion, groups, and this is the seventh of those: the
      * cost is a sort click's worth of render, and the cell code reads a plain boolean.
      */
    private def withSortableFlags(build: Map[List[String], Boolean] => UI)(using Frame): UI =
        val reactive = leafPaths.flatMap((p, c) => c.sortableSig.toList.map(sig => (p, sig)))
        def loop(rest: List[(List[String], Signal[Boolean])], acc: Map[List[String], Boolean]): UI =
            rest match
                case Nil              => build(acc)
                case (p, sig) :: tail => sig.render(b => loop(tail, acc + (p -> b)))
        loop(reactive, Map.empty)
    end withSortableFlags

    /** Every bound filter with something in it, paired with the row test its column reads
      * it as, or `Absent` where the column cannot: a query that is not a value of its
      * type, or a mode it does not offer.
      *
      * The list walks the VISIBLE leaves, which is what leaves a hidden column's filter
      * inert: an input the reader cannot reach is one they cannot clear either. Each
      * query is read once here, so the predicate a row is tested with closes over a value
      * that was parsed a single time.
      */
    private def filterReads(specs: Map[List[String], ColumnFilter]): List[(List[String], Maybe[A => Boolean])] =
        if specs.isEmpty then Nil
        else
            leafPaths.flatMap { (path, c) =>
                (Maybe.fromOption(specs.get(path)), c.filterV) match
                    case (Present(f), Present(cf)) if f.active.nonEmpty => List((path, cf.predicate(f)))
                    case _                                              => Nil
            }

    /** This column's resolved sortable flag: the reactive ones are in `flags`, the rest
      * carry theirs statically.
      */
    private def sortableFlag(c: Column[A, FlatOnly], path: List[String], flags: Map[List[String], Boolean]): Boolean =
        flags.getOrElse(path, c.sortableConst)

    /** Whether a sort spec may name this column. Locally that is an ordering, since an
      * ordering is what a sort acts on; a [[lazyRows]] table sorts elsewhere and its
      * columns need carry none, so there a column counts once it DECLARES itself sortable,
      * with `sortable(true)` or with a `sortBy` whose projection then goes unread. The
      * default stays no either way, or every column of such a table would offer a sort
      * nobody asked for.
      */
    private def canSortBy(c: Column[A, FlatOnly]): Boolean =
        if lazyOn then c.sortableV.isDefined || c.orderingV.isDefined else c.orderingV.isDefined

    /** Whether the READER may re-sort this column: one the spec can name, and one the flag
      * leaves free. A locked column still sorts by the spec, it only takes no clicks.
      */
    private def sortsHere(c: Column[A, FlatOnly], path: List[String], flags: Map[List[String], Boolean]): Boolean =
        canSortBy(c) && sortableFlag(c, path, flags)

    private def body(
        rowsIn: Seq[A],
        rowOffset: Int,
        held: Seq[A],
        sort: List[SortKey],
        query: String,
        page: Int,
        total: Total,
        sel: Set[String],
        ctx: Maybe[String],
        cells: Set[CellPath],
        exp: Set[String],
        openGroups: Set[GroupPath],
        flags: Map[List[String], Boolean],
        edit: EditState,
        nav: NavState[A],
        filterIn: FilterState,
        size: SizeState,
        order: OrderState,
        scroll: ScrollState,
        moveIn: MoveState[A]
    )(using Frame): UI =
        // A lazily loaded table is handed a window onto rows prepared elsewhere, so the
        // three passes below would filter a page, sort a page and slice a slice. It skips
        // all three and renders what it was given, in the order it was given.
        val prepared = lazyOn

        val rows   = rowsIn.toList
        val reads  = if prepared then Nil else filterReads(filterIn.specs)
        val filter = filterIn.copy(unusable = reads.collect { case (p, Absent) => p }.toSet)
        val sorted = arranged(rows, sort, query, reads)

        // 3. Paginate: clamp the 0-based page, slice, and embed the standalone Paginator
        //    (resolved page passed directly, since the table already renders inside its own
        //    page-ref subscription). A prepared table slices nothing, the rows ARE the page,
        //    and paginates over the total it was given, which is the one thing it knows
        //    about the pages it was not given.
        // A windowed table scrolls instead of paginating: the scrollbar answers the same
        // question the page list does, and only one of them can be right about which rows
        // are on the screen.
        val (paged, paginatorUI, pageBase) = if windowOn then (sorted, Nil, 0)
        else
            pageSizeV match
                case Present(size) =>
                    val at = math.max(page, 0)
                    // An unknown total counts the furthest the source has reached, never less
                    // than what is on the screen, and adds one page while anything follows.
                    // Counting the screen alone would shrink the page list on the way back.
                    val count =
                        if !prepared then sorted.size
                        else
                            total match
                                case Total.Known(n) => n
                                case Total.Unknown(more, atLeast) =>
                                    math.max(at * size + sorted.size, atLeast) + (if more then 1 else 0)
                    val totalPages = math.max(1, (count + size - 1) / size)
                    val cur        = math.min(at, totalPages - 1)
                    var pag = paginatorF.getOrElse(identity[Paginator])(Paginator())
                        .totalRecords(count)
                        .rows(size)
                        .currentPage(cur)
                        .hostClass("p-datatable-paginator-bottom")
                    pageRef.foreach(ref => pag = pag.page(ref))
                    (
                        if prepared then sorted else sorted.slice(cur * size, cur * size + size),
                        List(pag.render),
                        if prepared then 0 else cur * size
                    )
                case Absent => (sorted, Nil, 0)

        // The paths whose headers the reader can actually click, which is what both click
        // transitions may clear. Everything else in the spec is the caller's to keep.
        val interactive: Set[List[String]] =
            if sortRef.isEmpty then Set.empty
            else leafPaths.collect { case (p, c) if sortsHere(c, p, flags) => p }.toSet

        val colCount =
            leafCols.length + (if checkboxColumn then 1 else 0) + (if expanderColumn then 1 else 0) +
                (if handleColumn then 1 else 0) + (if editorColumn then 1 else 0)

        val frozen = frozenPlan(size)

        // A grip is only offered where a drop would land where the reader let go: the rows
        // on the screen have to be the rows in the list, in that order, and the list has to
        // be somewhere the table can write. Anything else and the column still renders,
        // since it is part of the anatomy, with nothing behind it and a card saying which.
        val move =
            if handleColumn && rowMoveWritable && rowOrderOwners(sort, query, filterIn.specs).isEmpty then
                moveIn.copy(all = rows, base = pageBase, count = paged.size)
            else moveIn.copy(live = false, held = Absent)

        // One tr per header level. The leading expander and checkbox cells belong to the
        // top row and reach down through every other one, so they line up with a column
        // whatever depth the header has.
        val headRows: List[UI] =
            val matrix = ColumnTree.spans(cols)
            val depth  = math.max(matrix.length, 1)
            val leading: List[UI] =
                val expanderTh: List[Ast.Element] =
                    if !expanderColumn then Nil
                    else
                        var cell = th.cssClass("p-datatable-header-cell")
                        if depth > 1 then cell = cell.rowspan(depth)
                        List(cell)
                val checkboxTh: List[Ast.Element] = if checkboxColumn then List(selectAllCell(sorted, sel, depth)) else Nil
                (expanderTh ++ checkboxTh).zipWithIndex.map((cell, i) => freeze(cell, frozen.leadAt(i)))
            end leading
            val rows = if matrix.isEmpty then List(Nil) else matrix
            // The editor-button column has no header of its own, but it still needs a cell,
            // and one that reaches down the header the way the leading ones do.
            val trailing: List[UI] =
                if !editorColumn then Nil
                else
                    var cell = th.cssClass("p-datatable-header-cell")
                    if depth > 1 then cell = cell.rowspan(depth)
                    List(freeze(cell, frozen.trailAt(0)))
            rows.zipWithIndex.map { (cells, i) =>
                val ths = cells.map(sp => headerSpanCell(sp, sort, flags, interactive, size, frozen, order, filter))
                tr((if i == 0 then leading ++ ths ++ trailing else ths).map(toChild)*)
            }
        end headRows

        // Prime's row filter display: one more header row, an inline filter per column
        // that carries a pipeline, an empty cell where a column carries none. The reader
        // types into the input and picks the mode from the funnel beside it.
        val filterRowUI: List[UI] =
            if !filterRowOn then Nil
            else
                val leading: List[UI] =
                    leadTerms.indices.toList.map(i => freeze(th.cssClass("p-datatable-header-cell"), frozen.leadAt(i)))
                val trailing: List[UI] =
                    trailTerms.indices.toList.map(i => freeze(th.cssClass("p-datatable-header-cell"), frozen.trailAt(i)))
                val cells = leafPaths.zipWithIndex.map((entry, i) => freeze(filterCell(entry._1, entry._2, filter), frozen.at(i)))
                List(tr((leading ++ cells ++ trailing).map(toChild)*))
            end if
        end filterRowUI

        // The rows the keyboard moves over are the ones on the SCREEN: a collapsed group
        // renders none of its own, so stepping by the paged index would land on a row
        // nobody can see.
        val navHere =
            if !nav.on then nav
            else
                val shown = RowGroup.visible(paged, groupsV, Nil, openGroups, expandedGroupsRef.isDefined)
                nav.copy(rows = shown.toVector, page = pageSizeV.getOrElse(math.max(shown.size, 1)))

        def emptyRow: UI =
            tr.cssClass("p-datatable-empty-message")(
                toChild(EmptyContent.render(emptyContentV, "No records found")(c => td.colspan(math.max(colCount, 1))(c)))
            )

        lazy val bodyRows: List[UI] =
            if paged.isEmpty then List(emptyRow)
            else
                groupSegments(paged.zipWithIndex, groupsV, Nil, sel, ctx, cells, exp, openGroups, colCount, edit, navHere, frozen, move)

        /** One spacer row, which is what holds the height of the rows that are not drawn.
          *
          * Prime's own, and the only shape that works: an empty `tr` collapses to nothing
          * in table layout, and `.p-datatable-virtualscroller-spacer` makes it a flex box,
          * which takes the height it is given. There is one of them ahead of the window
          * and one behind it, so the drawn rows sit at the scroll position they belong to
          * without any of them being positioned.
          */
        def padRow(px: Int)(using Frame): Ast.Element =
            tr.cssClass("p-datatable-virtualscroller-spacer").style(_.height(math.max(0, px).px))

        /** A row of the window the source has not reached yet: one `Skeleton` per column,
          * so the grid stays whole and the geometry never moves once the rows land.
          */
        def slotRow(using Frame): UI =
            val height = rowHeightV.getOrElse(1)
            // A line's worth of skeleton, which is what the cell would hold: the row is as
            // tall as every other one because the row says so, not because the slot does.
            def slot(f: Maybe[FrozenSlot]): UI =
                freeze(td(toChild(Skeleton().height("1rem").render)), f)
            val leading  = leadTerms.indices.toList.map(i => slot(frozen.leadAt(i)))
            val cells    = leafCols.indices.toList.map(i => slot(frozen.at(i)))
            val trailing = trailTerms.indices.toList.map(i => slot(frozen.trailAt(i)))
            tr.style(_.height(height.px))((leading ++ cells ++ trailing).map(toChild)*)
        end slotRow

        /** How many rows there are to scroll over, which does not depend on where the
          * reader is: the rows themselves locally, and how far the source reaches over one.
          */
        lazy val windowCount: Int =
            if windowedSource then viewport.extent(total, rowOffset + paged.size) else paged.size

        /** The rows of the window at `scrollTop`, between the two spacers.
          *
          * The row a slot shows is found by its ABSOLUTE index, and the rows the table holds
          * start at `rowOffset`, which over a source is the offset of the window it
          * published rather than the one this viewport last asked for. Those two differ for
          * exactly as long as a fetch is in flight, and drawing the published rows at the
          * asked-for offsets is how a list flickers through wrong rows while it loads.
          */
        def windowRows(scrollTop: Double)(using Frame): List[UI] =
            val vp        = viewport
            val held      = paged.toVector
            val loadedEnd = rowOffset + held.size
            val count     = windowCount
            if count <= 0 then List(emptyRow)
            else
                val (from, reach) = vp.span(vp.clamp(scrollTop, count))
                val until         = math.min(count, reach)
                val drawn = (from until until).toList.flatMap { i =>
                    if i >= rowOffset && i < loadedEnd then
                        dataRow(held(i - rowOffset), i, sel, ctx, cells, exp, colCount, Map.empty, edit, navHere, frozen, move, rowHeightV)
                    else List(slotRow)
                }
                // Both spacers are always emitted, one of them at nothing at either end of
                // the list, so a window is the same shape wherever it sits and the leading
                // one is always there to say where it starts.
                val lead = padRow(from * vp.itemSize)
                    .data("uic-vs-first", from.toString)
                    .data("uic-vs-count", (until - from).toString)
                (lead :: drawn) :+ padRow((count - until) * vp.itemSize)
            end if
        end windowRows

        /** The `tbody`. Over a windowed body the reactive region is its ROWS rather than the
          * group itself, and that is what keeps the scroll position: replacing a region
          * takes its nodes out before it puts the new ones in, and a table that loses its
          * whole row group for that instant is a table the browser clamps the scroll of.
          * The group stays put, and it carries the height of the whole list, which the two
          * spacers add up to anyway.
          */

        /** The row group that holds under the header, and the offset it holds at.
          *
          * The rows are drawn like any other, minus the keyboard grid: the cursor addresses
          * cells by their position among the SCROLLING rows, and these are not among them.
          * The offset is a measurement, so it is `Absent` on a first paint and in a static
          * projection, and the group then sits in flow at the top of the body, which is
          * where it belongs while nothing has scrolled.
          */
        def frozenGroup(rows: Seq[A], top: Maybe[Int])(using Frame): UI =
            var group = tbody.cssClass("p-datatable-tbody").cssClass("p-datatable-frozen-tbody")
            if size.idPrefix.nonEmpty then group = group.id(s"${size.idPrefix}-frozen")
            top.foreach(px => group = group.style(_.top(px.px)))
            val trs = rows.toList.zipWithIndex.flatMap((a, i) =>
                dataRow(
                    a,
                    i,
                    sel,
                    ctx,
                    cells,
                    exp,
                    colCount,
                    Map.empty,
                    edit,
                    navHere.copy(on = false),
                    frozen,
                    move.copy(live = false, held = Absent)
                )
            )
            group(trs.map(toChild)*)
        end frozenGroup

        val frozenBody: List[UI] =
            if !frozenRowsOn then Nil
            else
                List(scroll.headTop match
                    case Present(sig) => sig.render(top => frozenGroup(held, top))
                    case Absent       => frozenGroup(held, Absent))

        val tbodyEl: UI =
            if !windowOn then tbody.cssClass("p-datatable-tbody")(bodyRows.map(toChild)*)
            else
                val group = tbody
                    .cssClass("p-datatable-tbody")
                    .style(_.height((windowCount * viewport.itemSize).px))
                scroll.ref match
                    case Present(r) => group(toChild(r.render(top => UI.fragment(windowRows(top)*))))
                    case Absent     => group(windowRows(0.0).map(toChild)*)
            end if
        end tbodyEl

        // The footer aggregates over the FILTERED rows, not the visible page: a
        // column total that changed when the reader turned the page would be wrong.
        val footGroup: List[UI] =
            if !leafCols.exists(_.hasFooter) then Nil
            else
                val leadingTds: List[UI]  = leadTerms.indices.toList.map(i => freeze(td, frozen.leadAt(i)))
                val trailingTds: List[UI] = trailTerms.indices.toList.map(i => freeze(td, frozen.trailAt(i)))
                val footTds: List[UI] =
                    leafCols.zipWithIndex.map((c, i) => freeze(footerCell(c, sorted), frozen.at(i)))
                val footRow: UI = tr((leadingTds ++ footTds ++ trailingTds).map(toChild)*)
                List(tfoot.cssClass("p-datatable-tfoot")(toChild(footRow)))

        var tbl = table.cssClass("p-datatable-table")
        if scrollingOn || frozenOn || frozenRowsOn || resizeModeAt(size) == ColumnResizeMode.Expand then
            tbl = tbl.cssClass("p-datatable-scrollable-table")
        // Prime's own classes carry the clipping a sized column needs (a value too long
        // for its column is cut rather than widening it); the layout mode they leave to
        // the host, which is what the `.p-uic-table-fixed` rule supplies.
        if hasWidths then tbl = tbl.cssClass("p-uic-table-fixed")
        if resizeOn then
            tbl = tbl.cssClass("p-datatable-resizable-table")
            // Prime's `-fit` is what hides the last column's handle, which is the one thing
            // that is true of fitting and not of expanding: under `Expand` the last column
            // needs no neighbour to trade with, so it keeps its handle.
            if resizeModeAt(size) == ColumnResizeMode.Fit then
                tbl = tbl.cssClass("p-datatable-resizable-table-fit")
            else
                // Under `Expand` the table states its own width, or the browser hands what
                // the columns leave over back to them and a drag lands where it started.
                tbl = tbl.style(_.width(statedWidth(size)))
            end if
        end if
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => tbl = tbl.aria("labelledby", v))
        // The header carries an id only where a frozen row needs to hold at its height,
        // which is the one thing about this table nothing can be told and nothing can work
        // out on its own.
        val headEl: UI =
            var h = thead.cssClass("p-datatable-thead")
            if frozenRowsOn && size.idPrefix.nonEmpty then h = h.id(headId(size.idPrefix))
            h((headRows ++ filterRowUI).map(toChild)*)
        end headEl

        val tableEl: UI = tbl(
            (colGroup(size) ++ List[UI](
                headEl
            ) ++ frozenBody ++ List[UI](tbodyEl) ++ footGroup).map(toChild)*
        )

        var container = div.cssClass("p-datatable-table-container")
        // A windowed table hangs its height on the scroller instead, which is then the one
        // element that scrolls: the container holds exactly it, so nothing overflows there
        // and the sticky header and the frozen columns resolve against the same box in
        // both directions.
        if !windowOn then scrollHeightV.foreach(h => container = container.style(_.maxHeight(CssValue.length(h))))
        val containerEl: UI =
            if !windowOn then container(toChild(tableEl))
            else
                // Prime's own nesting for a virtually scrolled table, which the extracted
                // sheet names beside the plain one: the scroller sits between the
                // container and the table.
                val px = viewportPx.getOrElse(1)
                var vs = div
                    .cssClass("p-virtualscroller")
                    .cssClass("p-uic-vs-viewport")
                    .style(_.height(px.px).minHeight(px.px))
                scroll.ref.foreach { r =>
                    vs = vs.onScrollPosition { (e: ScrollPositionEvent) =>
                        // The position drawn at and the range asked for come out of the
                        // SAME reported number, so the window can never be drawn at a
                        // scroll position it did not ask rows for.
                        val top = math.max(0.0, e.scrollTop)
                        (sourceV match
                            case Present(src) => r.set(top).andThen(src.demand.set(viewport.demand(top)))
                            case Absent       => r.set(top)
                        ): Unit < Async
                    }
                }
                container(toChild(vs(toChild(tableEl))))
        end containerEl

        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-datatable-header")(toChild(h)))
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-datatable-footer")(toChild(f)))

        var root = div.cssClass("p-datatable").cssClass("p-component")
        idV.foreach(v => root = root.id(v))
        // Prime: hoverable whenever a selection mode is set (checkbox included) or
        // rows react to clicks.
        if selectionModeV != SelectionMode.None || onRowClickF.isDefined then
            root = root.cssClass("p-datatable-hoverable")
        if stripedFlag then root = root.cssClass("p-datatable-striped")
        if gridlinesFlag then root = root.cssClass("p-datatable-gridlines")
        // Prime's frozen rules only apply inside a scrollable table, and rightly so: a
        // column held against an edge means nothing until something moves past it.
        // An expanding table outgrows its container by design, which only means anything
        // where the container scrolls.
        if scrollingOn || frozenOn || frozenRowsOn || resizeModeAt(size) == ColumnResizeMode.Expand then
            root = root.cssClass("p-datatable-scrollable")
        if flexOn then root = root.cssClass("p-datatable-flex-scrollable")
        // The cursor keys must not ALSO scroll the page under the table. A kyo handler is
        // async and cannot decline the browser default in time, so the suppression is
        // declarative: the client reads the attribute before it posts the event. The class
        // is what gives the focused cell its ring.
        if nav.on then root = root.cssClass("p-uic-dt-nav").preventScrollKeys
        sizeV match
            case Size.Small  => root = root.cssClass("p-datatable-sm")
            case Size.Large  => root = root.cssClass("p-datatable-lg")
            case Size.Normal => ()
        end match
        root(
            (rowKeyCard ++ headerCards(
                sort,
                flags
            ) ++ editCards ++ filterCards(filter) ++ sizeCards(
                size
            ) ++ frozenCards(size) ++ orderCards(
                order
            ) ++ rowsCards ++ selectionCards ++ scrollCards ++ moveCards(sort, query, filter.specs) ++ windowCards ++ frozenRowCards(
                paged,
                held
            ) ++ lazyCards(
                rows,
                total
            ) ++ loadingMask ++ headerSlot ++ (containerEl :: paginatorUI) ++ footerSlot).map(toChild)*
        )
    end body

    /** The busy mask: Prime's absolute `.p-datatable-mask` composed with the dimming
      * `.p-overlay-mask` (the mask's own `position: fixed` loses to the datatable
      * rule), holding the spinner. The DataView precedent.
      */
    private def loadingMask(using Frame): List[UI] =
        def maskDiv: UI =
            div
                .cssClass("p-datatable-mask")
                .cssClass("p-overlay-mask")(
                    toChild(ProgressSpinner().size(Size.Small).accessibleName("Loading").render)
                )
        // A windowed table says what it is waiting for per ROW, in the slots the window
        // leaves for the rows that have not arrived. A mask over the whole of it would
        // dim the rows that did, on every scroll that reaches past the buffer.
        if windowOn then Nil
        else
            loadingV match
                case Present(BoolValue.Const(true))  => List(maskDiv)
                case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then maskDiv else UI.empty))
                case Present(BoolValue.Const(false)) => Nil
                case Absent                          => Nil
        end if
    end loadingMask

    /** One `tfoot` cell. Columns without a footer still render an empty `td` so the
      * footer row keeps the column grid.
      */
    private def footerCell(c: Column[A, FlatOnly], inFilter: List[A])(using Frame): Ast.Element =
        var cell = td
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        val content: List[UI] = (c.footerTextV, c.footerF) match
            case (Present(t), _) => List(span.cssClass("p-datatable-column-footer")(t))
            case (_, Present(f)) => List(span.cssClass("p-datatable-column-footer")(toChild(f(inFilter))))
            case _               => Nil
        cell(content.map(toChild)*)
    end footerCell

    /** The loud card rendered above the table when per-row IDENTITY is in use but
      * [[rowKey]] is unset (see [[KeyDiagnostics]]).
      *
      * The fallback key is the row's position, and position is not an identity: it
      * is stable under sorting and filtering (both are computed over the original
      * list here) but not under a data change. Reorder the underlying rows and every
      * selection, expansion and click payload silently re-associates with a
      * different record. The component cannot tell a static list from a live one, so
      * the moment identity is actually consumed the key stops being optional.
      */
    private def rowKeyCard(using Frame): List[UI] =
        val usesIdentity =
            selectedBinding.isDefined || expandedRef.isDefined || onRowClickF.isDefined ||
                editingRowsRef.isDefined || editingCellRef.isDefined
        if rowKeyF.isDefined || !usesIdentity then Nil
        else
            List(KeyDiagnostics.card(
                "DataTable",
                "selection, expansion or onRowClick is bound but rowKey is unset, so rows are keyed by position " +
                    "and a data change re-associates that state with the wrong record; set rowKey",
                Nil
            ))
        end if
    end rowKeyCard

    /** The loud cards for the ways a column tree and a sort spec mislead at render time
      * rather than at compile time (see [[KeyDiagnostics]]).
      *
      * A group holding no column occupies nothing, so it disappears from the header
      * instead of reporting itself. A spec entry that matches no sortable column does
      * nothing at all, which covers a mistyped part, a column that never got a `sortBy`,
      * and a path missing the group labels above it. And two sortable columns reachable
      * by the SAME path cannot be told apart by a spec at all: the table sorts by
      * whichever sits left and lights both headers up.
      */
    private def headerCards(sort: List[SortKey], flags: Map[List[String], Boolean])(using Frame): List[UI] =
        def show(path: List[String]): String = path.mkString(" / ")
        val empties                          = ColumnTree.emptyGroups(cols)
        val sortable                         = allPaths.collect { case (p, c) if canSortBy(c) => p }
        val unknown                          = if sortRef.isEmpty then Nil else sort.map(_.path).filterNot(sortable.contains).map(show)
        val ambiguous =
            if sortRef.isEmpty then Nil else KeyDiagnostics.duplicates(sortable.map(show))
        // A column asked to be sortable with nothing to sort by: the flag decides whether the
        // reader may change the spec, and an ordering is what a change would act on.
        // Not in a prepared table: there the flag is the whole declaration, and the
        // ordering it would be asking for is one nothing would read.
        val noOrdering =
            if lazyOn then Nil
            else
                leafPaths.collect {
                    case (p, c) if c.orderingV.isEmpty && c.sortableV.isDefined && sortableFlag(c, p, flags) => show(p)
                }
        val emptyCard =
            if empties.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a headerGroup holds no column, so it renders nothing; give it columns or drop it",
                    empties
                ))
        val unknownCard =
            if unknown.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the sort spec names a column this table cannot sort; a path is the group labels around " +
                        "the column followed by its header, and the column needs " +
                        (if lazyOn then "a sortBy or sortable(true), since a lazily loaded table sorts elsewhere"
                         else "a sortBy"),
                    unknown
                ))
        val ambiguousCard =
            if ambiguous.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "two sortable columns share a path, so no spec can tell them apart; rename one or put " +
                        "them under different headerGroups",
                    ambiguous
                ))
        val noOrderingCard =
            if noOrdering.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "sortable(true) on a column with no ordering has nothing to sort by; add sortBy, or drop " +
                        "the flag",
                    noOrdering
                ))
        emptyCard ++ unknownCard ++ ambiguousCard ++ noOrderingCard
    end headerCards

    /** The loud cards for a filter that cannot do anything.
      *
      * The two halves of a per-column filter are bound in different places, the pipeline
      * on the column and the state on the table, so either can be there without the other
      * and neither says anything by itself: a filter row with no pipeline behind it takes
      * input nothing reads, and a pipeline with no state bound has nowhere to put what the
      * reader types. And a spec entry naming no filterable column of this table filters
      * nothing at all, which is the same mistake the sort spec's card names.
      */
    private def filterCards(filter: FilterState)(using Frame): List[UI] =
        val filterable = leafPaths.exists(_._2.isFilterable) || hiddenPaths.exists(_._2.isFilterable)
        val nothing =
            if columnFiltersRef.isEmpty || filterable then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "columnFilters is bound but no column can be filtered, so the filter row has nothing to hold; " +
                        "add Column.filterBy",
                    Nil
                ))
        val nowhere =
            if !filterable || columnFiltersRef.isDefined then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a column carries filterBy and no columnFilters ref is bound, so there is nowhere to put what " +
                        "the reader types and no filter row to type it into; bind columnFilters",
                    Nil
                ))
        // The paths, not the visible ones: a filter on a hidden column is deliberately
        // inert, and reporting it as unknown would name a mistake nobody made.
        val known = (leafPaths ++ hiddenPaths).collect { case (p, c) if c.isFilterable => p }.toSet
        val unknown =
            if columnFiltersRef.isEmpty then Nil
            else filter.specs.keys.toList.filterNot(known.contains).map(_.mkString(" / ")).sorted
        val unknownCard =
            if unknown.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the column filters name a column this table cannot filter; a path is the group labels around " +
                        "the column followed by its header, and the column needs a filterBy",
                    unknown
                ))
        // A row display holds ONE condition per column, since it is one input. A seeded
        // filter carrying more is not narrowing the table the way it reads: the row shows
        // the first rule and typing into it replaces the lot.
        val crowded =
            if !filterRowOn then Nil
            else filter.specs.collect { case (p, f) if f.rules.sizeIs > 1 => p.mkString(" / ") }.toList.sorted
        val crowdedCard =
            if crowded.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a filter row is one input per column and these filters carry several conditions, of which it " +
                        "shows the first; use filterDisplay(FilterDisplay.Menu), which is where several belong",
                    crowded
                ))
        nothing ++ nowhere ++ unknownCard ++ crowdedCard
    end filterCards

    /** The loud cards for an editing binding that cannot do anything.
      *
      * The table only decides which cell shows its editor, so a bound editing state with no
      * [[Column.editor]] anywhere has nothing to show: the buttons would appear and drive a
      * set nothing reads. And the two modes address different things, one row against one
      * cell, so binding both leaves a cell inside an edited row showing its editor for two
      * reasons at once, with two ways out that do not agree.
      */

    /** The two width mistakes no type catches: a bound ref no boundary can ever write
      * into, and an entry naming a column this table does not have.
      */
    private def sizeCards(size: SizeState)(using Frame): List[UI] =
        val pinned =
            if columnWidthsRef.isEmpty || resizeOn then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "columnWidths is bound but no boundary is draggable, so nothing can ever write into it; a " +
                        "boundary needs a resizable column on both sides of it, and the last column has none",
                    Nil
                ))
        // Over the authored columns rather than the visible ones: a width on a hidden
        // column is what a reader showing it again gets back, not a mistake.
        val known = (leafPaths ++ hiddenPaths).map(_._1).toSet
        val unknown =
            if columnWidthsRef.isEmpty then Nil
            else size.widths.keys.toList.filterNot(known.contains).map(_.mkString(" / ")).sorted
        val unknownCard =
            if unknown.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the column widths name a column this table does not have; a path is the group labels around " +
                        "the column followed by its header",
                    unknown
                ))
        val unsized = if resizeModeV == ColumnResizeMode.Expand then unsizedLeaves(size) else Nil
        val expandCard =
            if unsized.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "an expanding table states its own width, which is the sum of its columns, so every column " +
                        "needs one; these have none and the table resizes to fit instead",
                    unsized.map(_.mkString(" / "))
                ))
        pinned ++ unknownCard ++ expandCard
    end sizeCards

    /** What a bound column order cannot do, which is the same three shapes the widths
      * report: nothing to write into it, a column it does not know, and an arrangement
      * this header cannot take.
      */
    private def orderCards(order: OrderState)(using Frame): List[UI] =
        if columnOrderRef.isEmpty then Nil
        else
            val known = (leafPaths ++ hiddenPaths).map(_._1).toSet
            val nothing =
                if reorderOn then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "columnOrder is bound but no column may be dragged, so nothing can ever write into it; a move " +
                            "needs two columns willing to trade places",
                        Nil
                    ))
            val unknown = order.requested.filterNot(known.contains).map(_.mkString(" / ")).sorted
            val strangers =
                if unknown.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "the column order names a column this table does not have; a path is the group labels around " +
                            "the column followed by its header",
                        unknown
                    ))
            // A group whose columns are not asked for together cannot be rendered, since one
            // header cell cannot sit in two places; they come back together where the first
            // of them was asked for.
            val split =
                ColumnTree.spans(cols).flatten.filter(_.node.asColumn.isEmpty).map(_.path).distinct.filter { g =>
                    val idx = order.requested.zipWithIndex.collect { case (p, i) if p.startsWith(g) => i }
                    idx.nonEmpty && idx.max - idx.min + 1 != idx.length
                }
            val torn =
                if split.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "the column order asks for a header group's columns apart, and a group is one cell; they are " +
                            "rendered together where the first of them was asked for",
                        split.map(_.mkString(" / ")).sorted
                    ))
            nothing ++ strangers ++ torn
        end if
    end orderCards

    /** What stops a window from being a window onto the set it names. Both are the same
      * mistake seen from either side, a total left behind by its rows or rows left behind
      * by their total, and neither shows in the table: the paginator offers pages that are
      * not there, or a page runs past its own end, while the rows on the screen look right
      * in both cases.
      */

    /** Two places to read the rows from. Nothing about the table says which one it took,
      * so a caller who bound a source and left an older `rows` behind would be looking at
      * one of them with no way to tell which.
      */
    private def rowsCards(using Frame): List[UI] =
        val bound = List(
            if sourceV.isDefined then List("source")
            else if rowsSigV.isDefined then List("rows(signal)")
            else Nil,
            if rowsRefV.isDefined then List("rows(ref)") else Nil,
            if rowsV.nonEmpty then List("rows(seq)") else Nil
        ).flatten
        if bound.size < 2 then Nil
        else
            List(KeyDiagnostics.card(
                "DataTable",
                "the rows are bound twice and only one binding is read; keep the one the table should show",
                bound
            ))
        end if
    end rowsCards

    /** A selection restriction over a table that has no selection restricts nothing, and
      * reads at the call site as though it does.
      */
    private def selectionCards(using Frame): List[UI] =
        val limited =
            if selectableF.isEmpty || selectionModeV != SelectionMode.None then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selectableWhen limits a selection this table does not have; set selectionMode to give it one",
                    Nil
                ))
        // One click cannot mean two things, so the three that claim it are named where
        // more than one is bound. Cell editing keeps the click, since it is the one that
        // does something a second click cannot undo.
        val claimed = List(
            if selectedBinding.isEmpty then Nil else List("selected, which picks rows"),
            if selectedCellsRef.isEmpty then Nil else List("selectedCells, which picks cells"),
            if editingCellRef.isEmpty then Nil else List("editingCell, which opens an editor")
        ).flatten
        val clash =
            if selectedCellsRef.isEmpty || claimed.sizeIs < 2 then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a click on a cell can mean one thing, and this table binds more than one; cell editing keeps " +
                        "the click, then row selection, and cell selection is off while either is bound",
                    claimed
                ))
        val modeless =
            if selectedCellsRef.isEmpty || selectionModeV == SelectionMode.Single ||
                selectionModeV == SelectionMode.Multiple
            then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "cell selection needs selectionMode Single or Multiple to say what a click means; Checkbox and " +
                        "Radio are about a column of the table, which a cell is not in",
                    List(selectionModeV.toString)
                ))
        // A one-way `selected` paints but never writes, which is the point of it — the caller owns
        // the set and closes the loop through `onRowClick`. Two shapes have no loop to close, and
        // both look exactly like a working selection until a reader clicks one.
        val oneWay = selectedBinding.isDefined && selectedRef.isEmpty
        val unwritable =
            if !oneWay || selectionModeV != SelectionMode.Checkbox then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selected is bound one way, so the checkbox column has nothing to write; a checkbox is not a row " +
                        "click, so onRowClick is no outlet for it either — bind a SignalRef, or pick Single/Multiple",
                    List(selectionModeV.toString)
                ))
        val inert =
            if !oneWay || !rowClickSelects || onRowClickF.isDefined then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selected is bound one way and no onRowClick is bound, so a row click has nowhere to go; the " +
                        "rows offer the pointer that says they can be picked and then do nothing",
                    List(selectionModeV.toString)
                ))
        limited ++ clash ++ modeless ++ unwritable ++ inert
    end selectionCards

    /** Two answers to how tall the viewport is, where the table can read only one. */
    private def scrollCards(using Frame): List[UI] =
        if !flexScrollFlag || scrollHeightV.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "DataTable",
                "flexScroll takes the viewport's height from the parent and scrollHeight states one; the stated " +
                    "length is what the table scrolls, so drop one of the two",
                scrollHeightV.toList
            ))
    end scrollCards

    /** What stops a row grip from being offered: nowhere to write the list a drop
      * produces, or something other than the list deciding what order the rows are in.
      * Either way the column still renders, since it is part of the anatomy, and the drag
      * is not wired rather than wired to nothing.
      */
    private def moveCards(sort: List[SortKey], query: String, specs: Map[List[String], ColumnFilter])(using
        Frame
    ): List[UI] =
        if !handleColumn then Nil
        else if !rowMoveWritable then
            List(KeyDiagnostics.card(
                "DataTable",
                "a row drag rewrites the row list and this table has nowhere to put it; bind rows(ref) for the " +
                    "table to store it, or onRowReorder to take it",
                Nil
            ))
        else
            val owners = rowOrderOwners(sort, query, specs)
            if owners.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the rows are on the screen in an order the list does not hold, so a drop cannot land where " +
                        "the reader let go; the grips are not offered while that is true",
                    owners
                ))
            end if
        end if
    end moveCards

    /** What stops a body from being windowed, and what a window costs the rest of the
      * table. The first two turn the windowing off and leave every row rendered, which is
      * slower and right rather than faster and wrong; the last two are the table dropping
      * something the caller asked for, since a scrollbar and a page list cannot both say
      * which rows are on the screen, and a cursor cannot step onto a row nobody drew.
      */
    private def windowCards(using Frame): List[UI] =
        if rowHeightV.isEmpty then Nil
        else
            val geometry =
                if viewportPx.isDefined then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "scrollRows works out how many rows fit from the scroll height, so it needs one, in px; " +
                            "the table renders every row instead",
                        scrollHeightV.toList.map(h => s"scrollHeight is $h") ++
                            (if flexOn then List("flexScroll takes its height from the parent, at layout time") else Nil)
                    ))
            val uneven =
                if viewportPx.isEmpty || unevenRows.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "a windowed body places its rows by counting them, so every row has to be the same height, " +
                            "and these render rows of their own; the table renders every row instead",
                        unevenRows
                    ))
            val paginated =
                if !windowOn || sourceV.isDefined || pageSizeV.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "a windowed table scrolls instead of paginating, so the page list is not rendered and the " +
                            "page size goes unread; drop one of the two",
                        pageSizeV.toList.map(n => s"a page of $n rows")
                    ))
            val navigable =
                if !windowOn || !cellNavV.getOrElse(leafCols.exists(_.isEditable)) then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "the keyboard cursor addresses cells by their position among the rendered rows, and a " +
                            "windowed body renders a window; navigation is off while the rows are windowed",
                        Nil
                    ))
            geometry ++ uneven ++ paginated ++ navigable
        end if
    end windowCards

    /** What stops a frozen row from holding, and what stops it from being one row.
      *
      * A row can only hold against something that moves, so without a scroll height there
      * is nothing for it to hold against and it renders at the top of the body like any
      * other. And a frozen row that is also in the body is TWO rows with one key: the
      * table cannot tell them apart, so a selection, an expansion or a click lands on both.
      */
    private def frozenRowCards(rows: List[A], held: Seq[A])(using Frame): List[UI] =
        if !frozenRowsOn then Nil
        else
            val loose =
                if scrollingOn then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "frozen rows hold against the edge of a scroll container, and this table has none; " +
                            "give it a scrollHeight or drop frozenRows",
                        Nil
                    ))
            val body  = rows.map(keyOf).toSet
            val twice = held.map(keyOf).filter(body.contains).distinct
            val both =
                if twice.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "a frozen row is in the body as well, so the table renders one record twice and cannot tell " +
                            "the two apart; frozen rows are a list of their own",
                        twice
                    ))
            loose ++ both
        end if
    end frozenRowCards

    private def lazyCards(rows: List[A], total: Total)(using Frame): List[UI] =
        if !lazyOn then Nil
        else
            val over = pageSizeV.toList.collect {
                case size if rows.size > size => s"${rows.size} rows over a page of $size"
            }
            val past = total match
                case Total.Known(n) if rows.size > n => List(s"${rows.size} rows out of a total of $n")
                case _                               => Nil
            val overCard =
                if over.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "a lazily loaded table was given more rows than one page holds, so the paginator and the " +
                            "rows disagree about where the page ends; hand it the page paginate was told the size of",
                        over
                    ))
            val pastCard =
                if past.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "a lazily loaded table was given more rows than the total it says exist; the total counts " +
                            "every row the query matched, not the ones on this page",
                        past
                    ))
            overCard ++ pastCard
        end if
    end lazyCards

    /** What stops a table from freezing at all. Each one is an arrangement whose offsets
      * cannot be worked out, and a table that renders them anyway parks a column over the
      * middle of itself; naming the column instead is the same trade the filter row makes
      * with a query it cannot read.
      */
    private def frozenCards(size: SizeState)(using Frame): List[UI] =
        if !frozenOn then Nil
        else
            def card(text: String, paths: List[List[String]]): List[UI] =
                if paths.isEmpty then Nil
                else List(KeyDiagnostics.card("DataTable", text, paths.map(_.mkString(" / ")).sorted))
            card(
                "a frozen column has no width, so how far from the edge it holds cannot be worked out; give it " +
                    "Column.width, or a width through the bound columnWidths",
                unsizedFrozen(size)
            ) ++ card(
                "a frozen column has a free column between it and its edge, which would carry it away as it " +
                    "scrolled; the frozen columns have to reach the edge they hold against",
                adriftFrozen
            ) ++ card(
                "a header group spans columns that do not agree where they belong, and one cell cannot half " +
                    "scroll; freeze every column under the group, or none of them",
                splitFrozenGroups
            )
        end if
    end frozenCards

    private def editCards(using Frame): List[UI] =
        val bound    = editingRowsRef.isDefined || editingCellRef.isDefined
        val editable = allPaths.exists(_._2.isEditable)
        val nothing =
            if !bound || editable then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "an editing state is bound but no column is editable, so nothing can be edited; add " +
                        "Column.editable",
                    Nil
                ))
        // The inverse mistake, and the more expensive one: a column says how to read and
        // write a value, and the table has nowhere to put the result, so a commit is
        // computed and dropped.
        val nowhere =
            if !editable || rowsRefV.isDefined || onCellChangedF.isDefined then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a column is editable but the table cannot save an edit; bind rows(ref) or add " +
                        "onCellValueChanged",
                    Nil
                ))
        // Two row lists means two answers to what the table shows, and a commit would
        // write into the one that is not being rendered.
        val twoSources =
            if !(rowsRefV.isDefined && rowsV.nonEmpty) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "rows(Seq) and rows(ref) are both set; the bound list is what renders, so the appended rows " +
                        "are dead",
                    Nil
                ))
        val both =
            if !(editingRowsRef.isDefined && editingCellRef.isDefined) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "editingRows and editingCell are both bound; a table edits by row or by cell, not both, so " +
                        "bind one",
                    Nil
                ))
        nothing ++ nowhere ++ twoSources ++ both
    end editCards

    /** The checkbox column's header cell: Prime's select-all, binary (no partial
      * state), checked while every row that survived the global filter is selected.
      *
      * It works on the FILTERED rows across every page, not the visible slice, which
      * is the only reading of "all" that stays stable while the reader pages through.
      * Rows the filter removed keep whatever selection they had, so the write is an
      * add-or-remove of the visible keys rather than a replacement of the whole set:
      * narrowing the filter, select-alling, then widening it again must not silently
      * drop what was selected before.
      */
    private def selectAllCell(inFilter: List[A], sel: Set[String], rows: Int)(using Frame): Ast.Element =
        val keys        = inFilter.filter(selectableAt).map(keyOf)
        val allSelected = keys.nonEmpty && keys.forall(sel.contains)
        val toggle: Any < Async = selectedRef match
            case Present(ref) => ref.getAndUpdate(cur => if allSelected then cur -- keys else cur ++ keys)
            case Absent       => ()
        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        cell(
            toChild(
                CheckBox()
                    .checked(allSelected)
                    .accessibleName("Select All")
                    .onChange(_ => toggle)
                    .render
            )
        )
    end selectAllCell

    /** One cell of the header matrix: a leaf renders its column header, a group a plain
      * title cell as wide as the leaves beneath it.
      */
    private def headerSpanCell(
        sp: ColumnTree.HeaderSpan[A],
        sort: List[SortKey],
        flags: Map[List[String], Boolean],
        interactive: Set[List[String]],
        size: SizeState,
        frozen: FrozenPlan,
        order: OrderState,
        filter: FilterState
    )(using Frame): UI =
        val at = sp.at
        sp.node.asColumn match
            case Present(c) =>
                headerCell(c, sp.path, sort, sp.rowspan, sortsHere(c, sp.path, flags), interactive, at, size, frozen, order, filter)
            case Absent =>
                var cell = th.cssClass("p-datatable-header-cell")
                if sp.colspan > 1 then cell = cell.colspan(sp.colspan)
                // A group moves as one, since its columns are its columns wherever it goes.
                val built = cell(
                    div.cssClass("p-datatable-column-header-content")(
                        toChild(span.cssClass("p-datatable-column-title")(sp.node.label))
                    )
                )
                freeze(
                    reorderCell(built, sp.path, at, at + sp.colspan, size, order, sorts = false),
                    frozen.span(at, at + sp.colspan)
                )
        end match
    end headerSpanCell

    /** One cell of the filter row: Prime's inline-filter anatomy over a column that
      * carries a pipeline, an empty header cell over one that does not.
      *
      * The input is uncontrolled and writes the bound map on every keystroke, so the
      * table re-filters as the reader types, the way the global filter already does. A
      * query this table cannot read is marked invalid rather than emptying the table:
      * a filter it cannot apply is not a filter, and hiding the rows behind a typo would
      * take away the data the reader is looking at.
      */
    private def filterCell(path: List[String], c: Column[A, FlatOnly], filter: FilterState)(using Frame): Ast.Element =
        val cell = th.cssClass("p-datatable-header-cell")
        c.filterV match
            case Absent => cell
            case Present(cf) =>
                val cur = filter.specs.getOrElse(path, ColumnFilter("", cf.default))
                val input = Input()
                    .value(cur.query)
                    .fluid(true)
                    .invalid(filter.unusable.contains(path))
                    .accessibleName(s"Filter by ${c.headerV}")
                    .onInput(t => writeFilter(path, cur.mode, t))
                val element = div.cssClass("p-datatable-filter-element-container")(toChild(input))
                cell(div.cssClass("p-datatable-inline-filter")((element :: modeMenu(path, c, cf, cur, filter)).map(toChild)*))
        end match
    end filterCell

    /** The funnel beside a filter input, and the constraint list it opens.
      *
      * A column offering one mode has nothing to pick, so it gets no funnel. Before the
      * mount runs there is no open state to write, so the button renders inert rather
      * than as an affordance that could not work, which is the trade the row editor's
      * buttons already make.
      */
    private def modeMenu(
        path: List[String],
        c: Column[A, FlatOnly],
        cf: CellFilter[A],
        cur: ColumnFilter,
        filter: FilterState
    )(using Frame): List[UI] =
        if cf.modes.sizeIs <= 1 then Nil
        else
            // Filled while the column is filtering, hollow while it is not: which columns
            // are narrowing the table has to be readable without opening anything.
            val glyph = if cur.query.trim.isEmpty then Icons.filter else Icons.filterFill
            // Prime renders this one as a Button too, and the extracted sheet carries no
            // rule of its own for it, so the component is what gives it its look and the
            // Prime class rides along as the hook a consumer's own CSS would reach for.
            def trigger(onOpen: Maybe[SignalRef[Boolean]]): UI =
                var b = Button()
                    .icon(glyph)
                    .variant(ButtonVariant.Text)
                    .severity(Severity.Secondary)
                    .rounded(true)
                    .size(Size.Small)
                    .accessibleName(s"Filter mode for ${c.headerV}")
                    .extraClass("p-datatable-column-filter-button")
                onOpen.foreach(ref => b = b.onClick(ref.getAndUpdate(!_)))
                b.render
            end trigger
            filter.openOf(path) match
                case Present(openRef) if filter.live =>
                    List(
                        Overlay(openRef)
                            .matchWidth(false)
                            .panelClass("p-datatable-filter-overlay")
                            .panelClass("p-component")
                            .trigger(trigger(Present(openRef)))(constraintList(path, cur, cf, openRef))
                            .render
                    )
                case _ => List(trigger(Absent))
            end match
        end if
    end modeMenu

    /** The modes one column offers, the current one marked. */
    private def constraintList(
        path: List[String],
        cur: ColumnFilter,
        cf: CellFilter[A],
        openRef: SignalRef[Boolean]
    )(using Frame): UI =
        ul.cssClass("p-datatable-filter-constraint-list")(
            cf.modes.map { m =>
                var item = li.cssClass("p-datatable-filter-constraint").tabIndex(0)
                if m == cur.mode then item = item.cssClass("p-datatable-filter-constraint-selected")
                val pick: Any < Async = pickMode(path, cur, m, openRef)
                // An `li` is not a control either: the constraint list is a row of tab stops, and
                // without this a reader could reach every one of them and pick none.
                item.onClick(pick).onKeyDown(e => if activationOf(e).isDefined then pick else ())(m.label)
            }.map(toChild)*
        )

    /** Prime's menu display: the funnel that sits at the end of a header cell, and the
      * popover it opens.
      *
      * `.p-datatable-popover-filter` is what pushes it to the trailing edge, which is the
      * one rule the extracted sheet has for it, and the panel carries Prime's popover
      * class rather than the select one the row display's constraint list uses: they are
      * two different boxes in the sheet, one a list and one a form.
      */
    private def filterFunnel(path: List[String], c: Column[A, FlatOnly], filter: FilterState)(using Frame): List[UI] =
        if !filterMenuOn then Nil
        else
            c.filterV match
                case Absent => Nil
                case Present(cf) =>
                    val cur = filter.specs.getOrElse(path, ColumnFilter.empty(cf.default))
                    // Filled while the column is filtering, hollow while it is not: which
                    // columns are narrowing the table has to be readable without opening
                    // anything.
                    val glyph = if cur.active.isEmpty then Icons.filter else Icons.filterFill
                    def trigger(open: Maybe[SignalRef[ColumnFilter]], openRef: Maybe[SignalRef[Boolean]]): UI =
                        var b = Button()
                            .icon(glyph)
                            .variant(ButtonVariant.Text)
                            .severity(Severity.Secondary)
                            .rounded(true)
                            .size(Size.Small)
                            .accessibleName(s"Filter by ${c.headerV}")
                            .extraClass("p-datatable-column-filter-button")
                        if cur.active.nonEmpty then b = b.extraClass("p-datatable-column-filter-button-active")
                        (open, openRef) match
                            case (Present(draft), Present(o)) => b = b.onClick(openFilterMenu(cur, draft, o))
                            case _                            => ()
                        b.render
                    end trigger
                    (filter.openOf(path), filter.draftOf(path)) match
                        case (Present(openRef), Present(draft)) if filter.live =>
                            List(
                                span.cssClass("p-datatable-popover-filter")(
                                    toChild(
                                        Overlay(openRef)
                                            .matchWidth(false)
                                            .panelClass("p-datatable-filter-overlay-popover")
                                            .panelClass("p-component")
                                            .trigger(trigger(Present(draft), Present(openRef)))(
                                                draft.render(d => filterMenu(path, c, cf, d, draft, openRef))
                                            )
                                            .render
                                    )
                                )
                            )
                        case _ => List(span.cssClass("p-datatable-popover-filter")(toChild(trigger(Absent, Absent))))
                    end match
            end match
        end if
    end filterFunnel

    /** Prime's filter popover, over the DRAFT: the operator, one row per rule, the buttons
      * that add and remove them, and the bar that clears or applies the lot.
      */
    private def filterMenu(
        path: List[String],
        c: Column[A, FlatOnly],
        cf: CellFilter[A],
        d: ColumnFilter,
        draft: SignalRef[ColumnFilter],
        openRef: SignalRef[Boolean]
    )(using Frame): UI =
        // One rule joins with nothing, so the operator has nothing to say until there are two.
        val operator: List[UI] =
            if d.rules.sizeIs < 2 then Nil
            else
                List(
                    Select[FilterOperator]()
                        .options(List(FilterOperator.And, FilterOperator.Or))(_.label)
                        .current(d.operator.label)
                        .accessibleName(s"Match mode for ${c.headerV}")
                        .extraClass("p-datatable-filter-operator-dropdown")
                        .onChange(v =>
                            draft.set(d.copy(operator =
                                if v == FilterOperator.Or.label then FilterOperator.Or
                                else FilterOperator.And
                            ))
                        )
                        .render
                )

        val rules: List[UI] = d.rules.zipWithIndex.map { (rule, i) =>
            val modes =
                Select[MatchMode]()
                    .options(cf.modes)(_.label)
                    .current(rule.mode.label)
                    .accessibleName(s"Condition ${i + 1} for ${c.headerV}")
                    .onChange(v => draft.set(setRule(d, i, r => r.copy(mode = cf.modes.find(_.label == v).getOrElse(r.mode)))))
                    .render
            val input =
                div.cssClass("p-datatable-filter-element-container")(
                    toChild(
                        Input()
                            .value(rule.query)
                            .fluid(true)
                            .accessibleName(s"Filter ${c.headerV} by condition ${i + 1}")
                            .onInput(t => draft.set(setRule(d, i, _.copy(query = t))))
                            .render
                    )
                )
            val remove: List[UI] =
                if d.rules.sizeIs < 2 then Nil
                else
                    List(
                        Button("Remove Rule")
                            .icon(Icons.trash)
                            .variant(ButtonVariant.Outlined)
                            .severity(Severity.Danger)
                            .size(Size.Small)
                            .extraClass("p-datatable-filter-remove-rule-button")
                            .onClick(draft.set(d.copy(rules = d.rules.patch(i, Nil, 1))))
                            .render
                    )
            div.cssClass("p-datatable-filter-rule")(((modes :: input :: Nil) ++ remove).map(toChild)*)
        }

        val add: UI =
            Button("Add Rule")
                .icon(Icons.plus)
                .variant(ButtonVariant.Text)
                .size(Size.Small)
                .extraClass("p-datatable-filter-add-rule-button")
                .onClick(draft.set(d.copy(rules = d.rules :+ FilterRule("", cf.default))))
                .render

        val bar: UI =
            div.cssClass("p-datatable-filter-buttonbar")(
                toChild(
                    Button("Clear")
                        .variant(ButtonVariant.Outlined)
                        .size(Size.Small)
                        .onClick(clearFilter(path, cf, draft, openRef))
                        .render
                ),
                toChild(Button("Apply").size(Size.Small).onClick(applyFilter(path, d, draft, openRef)).render)
            )

        UI.fragment(
            (operator ++ List(div.cssClass("p-datatable-filter-rule-list")(rules.map(toChild)*), add, bar))*
        )
    end filterMenu

    /** One rule of a draft, rewritten in place. */
    private def setRule(d: ColumnFilter, at: Int, f: FilterRule => FilterRule): ColumnFilter =
        d.copy(rules = d.rules.zipWithIndex.map((r, i) => if i == at then f(r) else r))

    /** Opening the menu seeds the draft from what is applied, so the panel shows the
      * filter the table is running and not whatever was last abandoned in it.
      */
    private def openFilterMenu(cur: ColumnFilter, draft: SignalRef[ColumnFilter], openRef: SignalRef[Boolean])(using
        Frame
    ): Any < Async =
        openRef.get.map(was =>
            if was then openRef.set(false)
            else
                draft.set(if cur.rules.isEmpty then ColumnFilter(List(FilterRule("", cur.mode))) else cur)
                    .andThen(openRef.set(true))
        )

    /** Apply: the draft becomes the filter. A draft asking for nothing takes the column
      * out of the map rather than leaving an empty entry, so what is in the map is what
      * is narrowing the table.
      */
    private def applyFilter(
        path: List[String],
        d: ColumnFilter,
        draft: SignalRef[ColumnFilter],
        openRef: SignalRef[Boolean]
    )(using Frame): Any < Async =
        val write: Any < Async = columnFiltersRef match
            case Present(ref) => ref.getAndUpdate(m => if d.active.isEmpty then m - path else m + (path -> d))
            case Absent       => ()
        write.andThen(openRef.set(false))
    end applyFilter

    /** Clear: the column stops filtering AND the draft goes back to one empty rule, since
      * a Clear that left the rules standing would be an apply away from coming back.
      */
    private def clearFilter(
        path: List[String],
        cf: CellFilter[A],
        draft: SignalRef[ColumnFilter],
        openRef: SignalRef[Boolean]
    )(using Frame): Any < Async =
        val write: Any < Async = columnFiltersRef match
            case Present(ref) => ref.getAndUpdate(_ - path)
            case Absent       => ()
        write.andThen(draft.set(ColumnFilter.empty(cf.default))).andThen(openRef.set(false))
    end clearFilter

    /** Writes what the reader typed, keeping the mode they picked: an emptied input is no
      * filter, but it is still the column's own mode, so clearing the text and typing
      * again does not silently go back to "contains".
      */
    private def writeFilter(path: List[String], mode: MatchMode, query: String)(using Frame): Any < Async =
        columnFiltersRef match
            case Present(ref) => ref.getAndUpdate(_ + (path -> ColumnFilter(query, mode)))
            case Absent       => ()

    private def pickMode(
        path: List[String],
        cur: ColumnFilter,
        mode: MatchMode,
        openRef: SignalRef[Boolean]
    )(using Frame): Any < Async =
        val write: Any < Async = columnFiltersRef match
            case Present(ref) => ref.getAndUpdate(_ + (path -> ColumnFilter(cur.query, mode)))
            case Absent       => ()
        write.andThen(openRef.set(false))
    end pickMode

    /** One sortable/plain header cell with Prime's header-content anatomy, reaching down
      * `rows` header rows so an ungrouped column lines up with a grouped one.
      */

    /** The `colgroup` in front of the header: one `col` per rendered column, carrying the
      * width of the ones that have one.
      *
      * This is the only structure that can size a column: a width on a cell sizes the row
      * it is in, and a grouped header's cell spans several columns and cannot name a width
      * for any single one of them. It also keeps the header, the body and the footer in
      * step for free, since all three read their widths off the same list.
      */
    private def colGroup(size: SizeState)(using Frame): List[UI] =
        if !hasWidths then Nil
        else
            // Sized rather than left to share the leftover: see DataTable.ToggleWidth.
            val leading: List[UI]  = leadTerms.map(v => col.style(_.width(Length.Calc(v))))
            val trailing: List[UI] = trailTerms.map(v => col.style(_.width(Length.Calc(v))))
            val cells: List[UI] = leafPaths.map { (p, c) =>
                widthOf(p, c, size) match
                    case Present(w) => col.style(_.width(w.px))
                    case Absent     => col
            }
            List(colgroup((leading ++ cells ++ trailing).map(toChild)*))
        end if
    end colGroup

    /** Prime's resize handle: a full-height strip on the trailing edge of a header cell,
      * carrying the `col-resize` cursor.
      *
      * The click handler is the point of the pair with `stopPropagation`: the handle sits
      * inside a header cell that may sort when clicked, and letting go of a drag on it
      * would otherwise re-sort the table the reader was only resizing. Before the mount
      * runs there is nothing to write a grab into, so the handle renders without one,
      * which is the trade the filter row's funnel already makes.
      */
    private def resizer(i: Int, size: SizeState)(using Frame): List[UI] =
        if !resizableAt(i, size) then Nil
        else
            var handle = span
                .cssClass("p-datatable-column-resizer")
                .aria("hidden", "true")
                .onClick(())
                .stopPropagation(true)
            if size.live then
                val path = leafPaths(i)._1
                val next = Maybe.fromOption(leafPaths.lift(i + 1)).map(_._1)
                handle = handle
                    .onPointerDown(e => beginResize(i, e, size))
                    .onPointerMove(e => dragResize(path, next, e, size))
            end if
            List(handle)
        end if
    end resizer

    /** A pointer going down on a boundary: measure the two columns it sits between and
      * remember them with the pointer's own x.
      *
      * The widths are MEASURED rather than read out of the bound map, since a column the
      * caller never gave one is as draggable as any other; what the reader grabs is what
      * the browser is currently showing.
      */
    private def beginResize(i: Int, e: PointerEvent, size: SizeState)(using Frame): Any < Async =
        size.grab match
            case Present(ref) =>
                // Under `Expand` the drag moves one column, so there is no neighbour to
                // measure and none to trade with; the grab still carries a second width so
                // the two modes share one held value.
                val ids =
                    if resizeModeAt(size) == ColumnResizeMode.Expand then List(headerId(i, size))
                    else List(headerId(i, size), headerId(i + 1, size))
                for
                    rects <- size.measure(ids)
                    a = rects.headOption.getOrElse(Rect(0, 0, 0, 0, 0, 0))
                    b = rects.lastOption.getOrElse(a)
                    r <- ref.set(Present(ColumnGrab(e.rectX + e.x, a.width, b.width)))
                yield r
                end for
            case Absent => ()

    /** The pointer moving with a boundary held: write both columns of the pair, computed
      * from where the grab started rather than from the last frame, so a drag that
      * outruns the render does not drift.
      */
    private def dragResize(path: List[String], next: Maybe[List[String]], e: PointerEvent, size: SizeState)(using
        Frame
    ): Any < Async =
        (size.grab, columnWidthsRef) match
            case (Present(g), Present(ref)) =>
                g.get.map {
                    case Present(held) =>
                        val delta = e.rectX + e.x - held.startX
                        (resizeModeAt(size), next) match
                            case (ColumnResizeMode.Fit, Present(n)) =>
                                val (a, b) = DataTable.resizeTo(held.width, held.next, delta)
                                ref.getAndUpdate(_ + (path -> a) + (n -> b))
                            case _ =>
                                ref.getAndUpdate(_ + (path -> DataTable.expandTo(held.width, delta)))
                        end match
                    case Absent => ()
                }
            case _ => ()

    private def headerCell(
        c: Column[A, FlatOnly],
        path: List[String],
        sort: List[SortKey],
        rows: Int,
        sorts: Boolean,
        interactive: Set[List[String]],
        index: Int,
        size: SizeState,
        frozen: FrozenPlan,
        order: OrderState,
        filter: FilterState
    )(using Frame): UI =
        val sortable  = sorts && sortRef.isDefined
        val sortingKs = SortKey.sorting(sort)
        val rank      = sortingKs.indexWhere(_.path == path)
        val direction = sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted)

        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        // Only once the mount has run: the id exists to be measured, and stamping it in the
        // static projection would put the same one on every table of a page.
        if size.live && (columnWidthsRef.isDefined || reorderOn) && index >= 0 then cell = cell.id(headerId(index, size))
        if index >= 0 && resizableAt(index, size) then cell = cell.cssClass("p-datatable-resizable-column")
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell
                .cssClass("p-datatable-sortable-column")
                .tabIndex(0)
                .onClick(e => headerClick(path, e, interactive, order))
                // A `th` is not a button: the tab stop above is this table's own, so the two keys
                // that operate it are too. The modifiers ride along, which is what keeps
                // Ctrl+Enter adding a column to the sort exactly as Ctrl+click does.
                .onKeyDown(e => activationOf(e).map(m => headerClick(path, m, interactive, order)).getOrElse(()))
        end if
        if direction.isSorting then
            cell = cell
                .cssClass("p-datatable-column-sorted")
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
                List(GlyphSvg(glyph, "p-datatable-sort-icon"))

        // Sorting by one column needs no ordinal; sorting by several does, because the
        // spec is ordered and the icons alone cannot say which key is primary.
        val sortBadge: List[UI] =
            if rank < 0 || sortingKs.length < 2 then Nil
            else
                List(
                    Badge((rank + 1).toString)
                        .size(Size.Small)
                        .hostClass("p-datatable-sort-badge")
                        .render
                )

        val content: UI =
            div.cssClass("p-datatable-column-header-content")(
                ((span.cssClass("p-datatable-column-title")(c.headerV): UI) ::
                    (sortIcon ++ sortBadge ++ filterFunnel(path, c, filter))).map(toChild)*
            )
        val built = cell((content :: (if index >= 0 then resizer(index, size) else Nil)).map(toChild)*)
        freeze(
            if index < 0 then built else reorderCell(built, path, index, index + 1, size, order, sortable),
            frozen.at(index)
        )
    end headerCell

    /** Header click. Plain: sort by this column alone, advancing it when it is already
      * the only key so a plain click still cycles. Ctrl or Cmd: add the column, or
      * advance the one already in the spec IN PLACE, which is what lets an accidental
      * click be undone by the next one.
      */
    private def toggleSort(path: List[String], e: MouseEvent, interactive: Set[List[String]])(using
        Frame
    ): Any < Async =
        sortRef match
            case Present(ref) =>
                val multi = e.modifiers.ctrl || e.modifiers.meta
                ref.getAndUpdate(cur =>
                    if multi then SortKey.cycle(cur, path, removableSortFlag, interactive.contains)
                    else SortKey.plain(cur, path, removableSortFlag, interactive.contains)
                )
            case Absent => ()

    private def isExpanded(a: A, exp: Set[String]): Boolean = expansionF.isDefined && exp.contains(keyOf(a))

    /** How many table rows one data row contributes: itself, plus its expansion row while
      * it is open. What a merged cell has to span is rows, not records.
      */
    private def trCount(a: A, exp: Set[String]): Int = if isExpanded(a, exp) then 2 else 1

    /** Renders one slice of rows under the remaining grouping levels: each level splits the
      * slice into runs, wraps every run in its header and summary rows, and hands the run
      * down. With no levels left the rows render directly.
      *
      * `path` is the chain of keys taken so far, which is what identifies a group at any
      * depth and therefore what [[expandedGroups]] holds.
      */
    private def groupSegments(
        rows: List[(A, Int)],
        levels: List[RowGroup[A]],
        path: List[String],
        sel: Set[String],
        ctx: Maybe[String],
        cells: Set[CellPath],
        exp: Set[String],
        openGroups: Set[GroupPath],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A]
    )(using Frame): List[UI] =
        levels match
            case Nil => leafRows(rows, sel, ctx, cells, exp, colCount, edit, nav, frozen, move)
            case level :: rest =>
                RowGroup.runs(rows)((a, _) => level.keyF(a)).flatMap { (key, run) =>
                    val groupPath = GroupPath(path :+ key)
                    val groupRows = run.map(_._1)
                    // A level without a header row has nowhere to put a toggle, so it can
                    // never be collapsed: collapsing it would hide its rows with no way
                    // back.
                    val collapsible = expandedGroupsRef.isDefined && level.showHeaderFlag
                    val open        = !collapsible || openGroups.contains(groupPath)

                    val headerRow: List[UI] =
                        if !level.showHeaderFlag then Nil
                        else List(groupHeaderRow(level, groupPath, groupRows, colCount, collapsible, open))
                    val innerRows: List[UI] =
                        if !open then Nil
                        else
                            groupSegments(run, rest, groupPath.keys, sel, ctx, cells, exp, openGroups, colCount, edit, nav, frozen, move)
                    val footerRow: List[UI] =
                        if !open then Nil
                        else
                            level.footerF.toList.map(f =>
                                tr.cssClass("p-datatable-row-group-footer")(
                                    toChild(td.colspan(math.max(colCount, 1))(toChild(f(groupPath, groupRows))))
                                )
                            )
                    headerRow ++ innerRows ++ footerRow
                }
    end groupSegments

    /** The innermost slice: the data rows themselves, carrying whatever merged cells the
      * [[Column.rowSpan]] columns resolve to over exactly this slice.
      */
    private def leafRows(
        rows: List[(A, Int)],
        sel: Set[String],
        ctx: Maybe[String],
        cells: Set[CellPath],
        exp: Set[String],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A]
    )(using Frame): List[UI] =
        val spans = spanCells(rows.map(_._1), exp)
        rows.zip(spans).flatMap((row, spanned) =>
            dataRow(row._1, row._2, sel, ctx, cells, exp, colCount, spanned, edit, nav, frozen, move)
        )
    end leafRows

    /** Resolves the merged cells of one slice: for each row, which of the marked columns it
      * heads a span of, and which it is covered by.
      *
      * The columns are taken left to right, and each one splits the blocks the previous one
      * produced rather than the whole slice. That nesting is what keeps two spans from
      * crossing, which the browser would render as a broken grid, and it is why the marked
      * columns read outer to inner in column order.
      */
    private def spanCells(rows: List[A], exp: Set[String]): List[Map[Int, SpanCell]] =
        val merged = leafCols.zipWithIndex.collect { case (c, i) if c.rowSpanEq.isDefined => (i, c.rowSpanEq.get) }
        if merged.isEmpty then List.fill(rows.size)(Map.empty)
        else
            val indexed                                                = rows.toVector
            val start: (Map[Int, Map[Int, SpanCell]], List[List[Int]]) = (Map.empty, List(rows.indices.toList))
            val (assigned, _) = merged.foldLeft(start) { case ((acc, blocks), (col, same)) =>
                val split = blocks.flatMap(block => RowGroup.blocks(block)((x, y) => same(indexed(x), indexed(y))))
                val next = split.foldLeft(acc) { (byRow, run) =>
                    val spanned = run.map(pos => trCount(indexed(pos), exp)).sum
                    run.zipWithIndex.foldLeft(byRow) { case (m, (pos, offset)) =>
                        val cell = if offset == 0 then SpanCell.Head(spanned) else SpanCell.Covered
                        m.updated(pos, m.getOrElse(pos, Map.empty) + (col -> cell))
                    }
                }
                (next, split)
            }
            rows.indices.toList.map(pos => assigned.getOrElse(pos, Map.empty))
        end if
    end spanCells

    /** One level's header row: Prime's `tr.p-datatable-row-group-header` over one
      * full-width cell, the collapse toggle ahead of the content while the level is
      * collapsible. Without a [[RowGroup.header]] template the cell shows the key alone.
      */
    private def groupHeaderRow(
        level: RowGroup[A],
        path: GroupPath,
        rows: List[A],
        colCount: Int,
        collapsible: Boolean,
        open: Boolean
    )(using Frame): UI =
        val toggle: List[UI] =
            if !collapsible then Nil
            else
                val glyph = if open then Icons.chevronDown else Icons.chevronRight
                List(
                    button
                        .cssClass("p-datatable-row-toggle-button")
                        .jsProp("type", "button")
                        .aria("expanded", open.toString)
                        .aria("label", if open then "Row Group Collapse" else "Row Group Expand")
                        .onClick(toggleGroup(path))(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon")))
                )
        val content: UI = level.headerF match
            case Present(f) => f(path, rows)
            case Absent     => stringToUI(path.key)
        tr.cssClass("p-datatable-row-group-header")(
            toChild(td.colspan(math.max(colCount, 1))((toggle :+ content).map(toChild)*))
        )
    end groupHeaderRow

    private def toggleGroup(path: GroupPath)(using Frame): Any < Async =
        expandedGroupsRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(path) then cur - path else cur + path)
            case Absent       => ()

    /** One data row (plus its expansion row while expanded). */
    private def dataRow(
        a: A,
        index: Int,
        sel: Set[String],
        ctx: Maybe[String],
        cells: Set[CellPath],
        exp: Set[String],
        colCount: Int,
        spans: Map[Int, SpanCell],
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A],
        height: Maybe[Int] = Absent
    )(using Frame): List[UI] =
        val id        = keyOf(a)
        val isSel     = sel.contains(id)
        val isExp     = exp.contains(id)
        val rowEdit   = edit.rows.contains(id)
        val navRow    = nav.indexOf(id, keyOf).getOrElse(-1)
        val canSelect = selectableAt(a)

        // Prime's grip. The whole cell is the grab surface, which is what the `cursor: move`
        // in the extracted sheet says, and it is where the pointer stream lives: a drag is
        // captured by the element the press landed on, so the rows the pointer travels over
        // never hear about it and the drop is worked out from geometry measured on the grab.
        val handleTd: List[UI] =
            if !handleColumn then Nil
            else
                var cell = td.cssClass("p-datatable-reorderable-row-handle").aria("hidden", "true")
                if move.live then
                    val at = move.base + index
                    cell = cell
                        .onPointerDown(e => beginRowMove(at, e, move))
                        .onPointerMove(e => dragRowMove(e, move))
                        .onPointerUp(_ => endRowMove(move))
                end if
                List(freeze(cell(toChild(GlyphSvg(Icons.bars))), frozen.leadAt(handleSlot)))

        val expanderTd: List[UI] =
            if !expanderColumn then Nil
            else
                val glyph = if isExp then Icons.chevronDown else Icons.chevronRight
                val cell = td(
                    button
                        .cssClass("p-datatable-row-toggle-button")
                        .jsProp("type", "button")
                        .aria("expanded", isExp.toString)
                        .aria("label", if isExp then "Row Collapse" else "Row Expand")
                        .onClick(toggleExpand(id))(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon")))
                )
                List(freeze(cell, frozen.leadAt(expanderSlot)))

        // Checkbox selection reuses Prime's checkbox anatomy (as Tree does).
        val checkboxTd: List[UI] =
            if !checkboxColumn then Nil
            else
                var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
                if isSel then cb = cb.cssClass("p-checkbox-checked")
                if canSelect then cb = cb.onClick(toggleSelect(id))
                else cb = cb.cssClass("p-disabled")
                val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
                val cell           = td(cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*))))
                List(freeze(cell, frozen.leadAt(checkboxSlot)))

        val dataTds: List[UI] = leafPaths.zipWithIndex.flatMap { (entry, i) =>
            val (path, c) = entry
            val cellSpan  = spans.get(i)
            if cellSpan.contains(SpanCell.Covered) then Nil
            else
                var cell = td
                cellSpan match
                    // A run of one row spans nothing; `rowspan="1"` is the default and
                    // stating it would only make the markup say something it already says.
                    case Some(SpanCell.Head(n)) if n > 1 => cell = cell.rowspan(n)
                    case _                               => ()
                end match
                c.alignV match
                    case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
                    case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
                    case ColumnAlign.Start  => ()
                end match
                // Cell editing addresses one cell, row editing every editable cell of a row.
                val here     = CellPath(id, path)
                val cellEdit = edit.cell.exists(_ == here)
                val openable = c.isEditableAt(a)
                val editing  = openable && (rowEdit || cellEdit)
                val cellMode = editingCellRef.isDefined && openable && edit.live
                if cellMode then
                    cell = cell.cssClass("p-editable-column").onClick(beginCellEditing(here, a, c, nav, edit))
                    // The click picks a cell, it does not also pick the row; and while the
                    // editor is open the keystrokes that leave it stop here rather than
                    // reaching the row's own handler.
                    cell = cell.stopPropagation(true)
                end if
                if cellEdit then cell = cell.cssClass("p-cell-editing")
                // A selected CELL: the extracted sheet carries a token for its border and no
                // rule that reaches a cell, so the class and the rule are both kyo's, over
                // Prime's own selected-row tokens.
                if cellSelectOn then
                    cell = cell.aria("selected", cells.contains(here).toString)
                    if cells.contains(here) then cell = cell.cssClass("p-uic-dt-cell-selected")
                    if canSelect then cell = cell.onClick(toggleCell(here)).stopPropagation(true)
                end if
                // With navigation on, every cell is addressable and the editable ones are
                // the tab stops: the DOM is row-major, so the browser's own tab order IS
                // "the next editable cell, wrapping into the next row", and nothing has to
                // be prevented to make Tab mean that.
                if nav.on && c.isNavigable then
                    val pos = GridNav.Pos(navRow, i)
                    cell = cell
                        .id(cellId(nav, pos))
                        .tabIndex(if openable then 0 else -1)
                        .onKeyDown(onCellKey(pos, here, a, c, cellEdit, rowEdit, nav, edit))
                        .stopPropagation(true)
                else if cellEdit then
                    cell = cell
                        .tabIndex(0)
                        .onKeyDown(e =>
                            e.key match
                                case Keyboard.Enter  => if rowEdit then commitRow(id, a, edit) else commitCell(here, a, c, edit)
                                case Keyboard.Escape => if rowEdit then cancelRow(id, edit) else cancelCell(edit)
                                case _               => ()
                        )
                end if
                if editing && edit.errorAt(here).isDefined then cell = cell.cssClass("p-invalid")
                val content: HtmlChildVal =
                    if editing then toChild(editorCell(here, a, c, nav, edit))
                    else
                        c.bodyF match
                            case Present(f) => toChild(f(a))
                            case Absent     => toChild(stringToUI(c.textF.map(_(a)).getOrElse("")))
                List(freeze(cell(content), frozen.at(i)))
            end if
        }

        // Prime's row editor: one button while the row rests, two while it is being edited.
        val editorTd: List[UI] =
            if !editorColumn then Nil
            else
                // Prime renders these as Buttons, and the extracted sheet carries no rules of
                // its own for them, so the component is what gives them their look; the Prime
                // class rides along as the hook a consumer's own CSS would reach for.
                def btn(cls: String, glyph: IconGlyph, label: String, action: Any < Async): UI =
                    var b = Button()
                        .icon(glyph)
                        .variant(ButtonVariant.Text)
                        .severity(Severity.Secondary)
                        .rounded(true)
                        .size(Size.Small)
                        .accessibleName(label)
                        .extraClass(cls)
                    // Inert in the static projection: the button is part of the anatomy, so
                    // it renders and the grid keeps its column count, but there is nothing
                    // for a click to write into until the mount has run.
                    if edit.live then b = b.onClick(action)
                    b.render
                end btn
                // Where focus goes when a BUTTON closed the row. Not onto the button that
                // replaces the one just pressed: that element is new, and a self-command
                // resolves `getElementById` the moment it lands, which can be before the
                // insert. The row's first editable cell is a `td` that was already there,
                // and it is where the keyboard would want to continue anyway.
                val backToRow: Any < Async =
                    editableLeaves.headOption match
                        case Some((path, _)) => focusCell(nav, CellPath(id, path))
                        case None            => ()
                val buttons: List[UI] =
                    if rowEdit then
                        List(
                            btn(
                                "p-datatable-row-editor-save",
                                Icons.check,
                                "Save Edit",
                                commitRow(id, a, edit).map(closed => if closed then backToRow else ())
                            ),
                            btn("p-datatable-row-editor-cancel", Icons.times, "Cancel Edit", cancelRow(id, edit).andThen(backToRow))
                        )
                    else
                        List(btn("p-datatable-row-editor-init", Icons.pencil, "Row Edit", beginRowEditing(id, a, edit)))
                List(freeze(td.cssClass("p-uic-dt-editor")(buttons.map(toChild)*), frozen.trailAt(0)))

        var row = tr.cssClass(if index % 2 == 0 then "p-row-even" else "p-row-odd")
        // A windowed row is placed by counting rows, so it says how tall it is rather than
        // leaving it to its content. The browser reads it as a floor and a row whose
        // content outgrows it still grows, which is the drift a scrollRows itemSize that
        // does not match the real row height shows up as.
        height.foreach(px => row = row.style(_.height(px.px)))
        // With cells being picked the row is not what a click selects, so it offers neither
        // the pointer that says it is nor the state a reader would be told about.
        if rowClickSelects && canSelect && !cellSelectOn then row = row.cssClass("p-datatable-selectable-row")
        if isSel then row = row.cssClass("p-datatable-row-selected")
        if selectionModeV != SelectionMode.None && !cellSelectOn then row = row.aria("selected", isSel.toString)
        // A second, separate mark: the reader is acting ON this row without changing what
        // is selected, which is why Prime gives it a class of its own.
        if ctx.contains(id) then row = row.cssClass("p-datatable-contextmenu-row-selected")
        if contextRowOn then row = row.onContextMenu(openRowContext(a, id))
        // After a drop the browser still owes a click, and it does not land on the grip the
        // press started on: the rows moved under the pointer. A row that would select takes
        // it and drops it, so the press cannot reach the next row the reader clicks.
        if rowInteractive then
            val act: Any < Async = if move.held.exists(_.done) then clearRowMove(move) else activate(id, canSelect)
            // A click that passed through a control of the reader's own is that control's, not the
            // row's: following a link in a cell, pressing the expander, starting a row edit. Without
            // this the row selects as well, and the control cannot decline the click for it — one
            // that navigates natively declares no kyo handler, so it has no `stopPropagation` to set
            // (and giving an anchor one costs it the middle click). The keyboard path below is
            // deliberately untouched: Enter on the ROW is the row's, whatever it contains.
            val clicked: MouseEvent => Any < Async = e => if e.onControl then () else act
            // The row's tab stop is this table's, not a control's, so Enter and Space are too.
            // A row being edited overwrites this handler below, which is the order that belongs:
            // while an editor is open Enter commits the row rather than re-selecting it.
            row = row.tabIndex(0).onClick(clicked).onKeyDown(e => if activationOf(e).isDefined then act else ())
        end if
        if rowEdit then
            // Enter and Escape reach here from whichever cell editor has focus, since a
            // keystroke bubbles the logical tree the way a click does.
            row = row
                .cssClass("p-datatable-editing-row")
                .onKeyDown(e =>
                    e.key match
                        case Keyboard.Enter  => commitRow(id, a, edit)
                        case Keyboard.Escape => cancelRow(id, edit)
                        case _               => ()
                )
        end if
        // Where a drop would land, shown on the row it would land beside. Prime positions
        // two floating arrows measured on every move; a border on the neighbour is the same
        // information, needs no measurement, and moves with the row if anything re-renders.
        move.held match
            case Present(d) if d.moved && !d.done =>
                val at = move.base + index
                if d.from == at then row = row.cssClass("p-uic-dt-dragging")
                if d.target == at then row = row.cssClass("p-datatable-dragpoint-top")
                else if d.target == at + 1 && d.target == d.first + d.edges.length - 1 then
                    row = row.cssClass("p-datatable-dragpoint-bottom")
            case _ => ()
        end match
        if move.live && move.idPrefix.nonEmpty then row = row.id(DataTable.rowId(move.idPrefix, move.base + index))
        rowClassF.foreach(f => f(a).foreach(cls => if cls.nonEmpty then row = row.cssClass(cls)))
        val rowEl: UI = row((handleTd ++ expanderTd ++ checkboxTd ++ dataTds ++ editorTd).map(toChild)*)

        val expansionRow: List[UI] =
            if isExp then
                expansionF.toList.map { f =>
                    tr.cssClass("p-datatable-row-expansion")(
                        td.colspan(math.max(colCount, 1))(toChild(f(a)))
                    )
                }
            else Nil

        rowEl :: expansionRow
    end dataRow

    private def toggleExpand(id: String)(using Frame): Any < Async =
        expandedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()

    /** A click on a cell: replace the set with it, or toggle it in, by the mode. */
    private def toggleCell(cell: CellPath)(using Frame): Any < Async =
        selectedCellsRef match
            case Present(ref) if selectionModeV == SelectionMode.Single =>
                ref.getAndUpdate(cur => if cur == Set(cell) then Set.empty else Set(cell))
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(cell) then cur - cell else cur + cell)
            case Absent       => ()

    /** A right-click over a row: remember which one, then tell the caller what it was aimed at.
      *
      * The selection is read HERE rather than taken from the render, because the row's handler is
      * a closure the render left behind: reading it now is reading what is selected at the moment
      * of the click. The keys are resolved against [[currentRows]], the table's full set, so a
      * selected row the reader has since filtered off the screen is still part of what the menu
      * is being asked about.
      */
    private def openRowContext(a: A, id: String)(using Frame): Any < Async =
        val write: Any < Async = contextRowRef match
            case Present(ref) => ref.set(Present(id))
            case Absent       => ()
        val fire: Any < Async = onRowContextF match
            case Present(f) =>
                for
                    // Through the BINDING, not the ref: a menu over a one-way selection is asked
                    // about that selection just the same, and reading it needs no write access.
                    keys <- currentValue(selectedBinding, Set.empty[String])
                    all  <- currentRows
                    _    <- f(RowContext(a, all.filter(r => keys.contains(keyOf(r)))))
                yield ()
            case Absent => ()
        write.andThen(fire)
    end openRowContext

    private def toggleSelect(id: String)(using Frame): Any < Async =
        selectedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()

    /** What one open cell shows: the column's editor over the table's draft, and under it
      * the message of a commit this cell refused.
      *
      * The editor binds the draft ref rather than a rendered value, so a keystroke
      * re-renders this cell and nothing above it, and an unrelated re-render (a push into
      * the bound row list, a sort) finds the text still in the ref rather than resetting
      * the field to the row's stored value.
      */
    private def editorCell(cell: CellPath, row: A, c: Column[A, FlatOnly], nav: NavState[A], edit: EditState)(
        using Frame
    ): UI =
        (c.editV, edit.draftOf(cell.column)) match
            case (Present(ed), Present(draft)) =>
                val params = EditorParams(
                    draft = draft,
                    id = editorId(nav, cell).getOrElse(""),
                    commit = commitCell(cell, row, c, edit),
                    cancel = cancelCell(edit)
                )
                edit.errorAt(cell) match
                    case Absent => ed.editor(params)
                    case Present(e) =>
                        fragment(
                            ed.editor(params),
                            div.cssClass("p-uic-dt-cell-error")(toChild(translatorV.translate(e).render(stringToUI)))
                        )
                end match
            case _ =>
                // A draft map with no entry for this column means the table is rendering
                // its placeholder (no mount, no refs), which is the static projection.
                stringToUI(c.editV.map(_.show(row)).getOrElse(""))

    // ---- keyboard navigation ----

    /** A cell's DOM id. Positional, because focus is positional: the cursor a reader moves
      * is "the cell below this one", not "this row's cell". The prefix is minted once per
      * mount, so two tables on a page cannot collide.
      */
    private def cellId(nav: NavState[A], pos: GridNav.Pos): String =
        s"${nav.idPrefix}-c${pos.row}-${pos.col}"

    private def rowAt(nav: NavState[A], pos: GridNav.Pos): Maybe[A] =
        if pos.row >= 0 && pos.row < nav.rows.size then Present(nav.rows(pos.row)) else Absent

    private def colAt(pos: GridNav.Pos): Maybe[(List[String], Column[A, FlatOnly])] =
        if pos.col >= 0 && pos.col < leafPaths.size then Present(leafPaths(pos.col)) else Absent

    /** The grid one key is read against, rebuilt per keystroke from what is on the screen.
      *
      * A cell counts as editable for the keyboard only in CELL mode: in row mode a row is
      * opened by its button and every editable cell of it opens at once, so there is no
      * single cell for Enter to open.
      */
    private def gridFor(nav: NavState[A]): GridNav.Grid =
        GridNav.Grid(
            rows = nav.rows.size,
            cols = leafPaths.size,
            editable = pos =>
                editingCellRef.isDefined && ((rowAt(nav, pos), colAt(pos)) match
                    case (Present(r), Present((_, c))) => c.isNavigable && c.isEditableAt(r)
                    case _                             => false),
            page = math.max(nav.page, 1),
            navigable = pos => colAt(pos).exists((_, c) => c.isNavigable)
        )

    /** Opens a cell on a seed: a printable key that started the edit, or the row's own
      * value when the reader pressed Enter or F2.
      */
    private def openCell(cell: CellPath, row: A, c: Column[A, FlatOnly], seed: Maybe[String], edit: EditState)(
        using Frame
    ): Any < Async =
        val fill: Any < Async = (seed, edit.draftOf(cell.column)) match
            case (Present(text), Present(ref)) => ref.set(text)
            case _                             => seedDraft(row, cell.column, c, edit)
        for
            _ <- fill
            _ <- clearError(edit)
            r <- setEditingCell(Present(cell))
        yield r
        end for
    end openCell

    /** Carries an open edit into the cell Tab is taking focus to. */
    private def openAt(pos: GridNav.Pos, nav: NavState[A], edit: EditState)(using Frame): Any < Async =
        (rowAt(nav, pos), colAt(pos)) match
            case (Present(r), Present((path, c))) => openCell(CellPath(keyOf(r), path), r, c, Absent, edit)
            case _                                => ()

    /** Puts DOM focus back on a cell, by the id it was rendered with.
      *
      * This is what an editor owes the cursor when it closes. While it is open the EDITOR
      * is the focused element, and closing removes it, so without this focus falls back to
      * the document and the next arrow key goes nowhere. The cell itself is never
      * replaced, only its content, so the command has a target the moment it arrives.
      */

    /** The id an open cell's editor stamps on whatever takes focus.
      *
      * Positional like [[cellId]] and for the same reason: it addresses a place on the
      * screen, so it stays put while the value in it changes. Absent for a cell that is not
      * among the rendered rows, which is a table that changed under an open editor.
      */
    private def editorId(nav: NavState[A], cell: CellPath): Maybe[String] =
        val col = leafPaths.indexWhere(_._1 == cell.column)
        (nav.indexOf(cell.row, keyOf), col) match
            case (Present(row), c) if c >= 0 => Present(s"${nav.idPrefix}-e$row-$c")
            case _                           => Absent
    end editorId

    /** Puts focus back INTO an open editor, which is where a refused commit leaves it. */
    private def focusEditor(nav: NavState[A], cell: CellPath)(using Frame): Any < Async =
        editorId(nav, cell) match
            case Present(id) => nav.focus(id)
            case Absent      => ()

    private def focusCell(nav: NavState[A], cell: CellPath)(using Frame): Any < Async =
        if !nav.on then ()
        else
            val col = leafPaths.indexWhere(_._1 == cell.column)
            (nav.indexOf(cell.row, keyOf), col) match
                case (Present(row), c) if c >= 0 => nav.focus(cellId(nav, GridNav.Pos(row, c)))
                case _                           => ()

    /** One key over one cell: read it against the grid, then do what it says.
      *
      * `Absent` from [[GridNav]] means the key was never ours, so the browser keeps it,
      * which is how a caret keeps its arrows and Ctrl-C keeps its meaning.
      */
    private def onCellKey(
        pos: GridNav.Pos,
        cell: CellPath,
        row: A,
        c: Column[A, FlatOnly],
        cellEdit: Boolean,
        rowEdit: Boolean,
        nav: NavState[A],
        edit: EditState
    )(e: KeyboardEvent)(using Frame): Any < Async =
        GridNav.onKey(gridFor(nav), pos, cellEdit || rowEdit, e.key, e.modifiers) match
            case Absent        => ()
            case Present(step) =>
                // A step with `moveFocus` false is Tab: the browser is already moving
                // focus, so the table only agrees about where. Inside an open ROW that
                // means doing nothing at all, since the row stays open and its drafts are
                // per column, not per cell.
                val isTab = !step.moveFocus
                // An editor that closes hands focus back to its cell. Not on Tab, where the
                // browser is moving focus itself and the cell it lands on opens instead.
                //
                // A REFUSED commit under Tab is the other way round: the browser has
                // already taken focus off a cell that is still being edited, so it goes
                // back INTO the editor, by the id the editor stamped, which is where the
                // reader was and where the value they have to fix is. The cell would not
                // do: it is the editor's container, so landing there is landing outside
                // the field. Under Enter focus never left the editor at all.
                def closing(closed: Boolean < Async): Boolean < Async =
                    closed.map { ok =>
                        val focus: Any < Async =
                            if ok && !isTab then focusCell(nav, cell)
                            else if !ok && isTab then focusEditor(nav, cell)
                            else ()
                        focus.andThen(ok)
                    }
                val editStep: Boolean < Async = step.edit match
                    case GridNav.EditOp.Keep       => true
                    case GridNav.EditOp.Open(seed) => openCell(cell, row, c, seed, edit).andThen(true)
                    case GridNav.EditOp.Commit =>
                        if rowEdit then (if isTab then Kyo.lift(true) else closing(commitRow(cell.row, row, edit)))
                        else closing(commitCell(cell, row, c, edit))
                    case GridNav.EditOp.Cancel =>
                        if rowEdit then closing(cancelRow(cell.row, edit).andThen(true))
                        else closing(cancelCell(edit).andThen(true))
                val focusStep: Any < Async = step.focus match
                    case Present(to) if step.moveFocus => nav.focus(cellId(nav, to))
                    case Present(to)                   => if rowEdit then () else openAt(to, nav, edit)
                    case Absent                        => ()
                val selectStep: Any < Async =
                    if step.selectRow && rowInteractive then activate(cell.row, selectableAt(row)) else ()
                for
                    // A refused commit stops the step that would move on: opening the next
                    // cell clears the error, so moving anyway left the refusal invisible
                    // and the edit gone.
                    ok <- editStep
                    _  <- (if ok then focusStep else ()): Any < Async
                    r  <- selectStep
                yield r
                end for
    end onCellKey

    /** Puts a row into the editing set, or takes it out. */
    private def setRowEditing(id: String, on: Boolean)(using Frame): Any < Async =
        editingRowsRef match
            case Present(ref) => ref.getAndUpdate(cur => if on then cur + id else cur - id)
            case Absent       => ()

    /** Moves the cell editor, or clears it. */
    private def setEditingCell(cell: Maybe[CellPath])(using Frame): Any < Async =
        editingCellRef match
            case Present(ref) => ref.set(cell)
            case Absent       => ()

    /** Fills a column's draft with what the row currently shows there, which is what an
      * editor opens on. The seed is the table's job precisely because the editor renders
      * in a pure position and cannot write into anything.
      */
    private def seedDraft(row: A, path: List[String], c: Column[A, FlatOnly], edit: EditState)(using Frame): Any < Async =
        (c.editV, edit.draftOf(path)) match
            case (Present(ed), Present(ref)) => ref.set(ed.show(row))
            case _                           => ()

    /** Seeds every editable column of one row: row editing opens them all at once. */
    private def seedRowDrafts(row: A, edit: EditState)(using Frame): Any < Async =
        Kyo.foreachDiscard(editableLeaves)((path, c) => seedDraft(row, path, c, edit))

    private def clearError(edit: EditState)(using Frame): Any < Async =
        edit.errorRef match
            case Present(ref) => ref.set(Absent)
            case Absent       => ()

    private def setError(cell: CellPath, e: FieldError, edit: EditState)(using Frame): Any < Async =
        edit.errorRef match
            case Present(ref) => ref.set(Present((cell, e)))
            case Absent       => ()

    /** Opens a cell: seed its draft from the row, drop any standing error, then move the
      * editor. Seeding first is what makes the editor's first paint the row's own value.
      */
    private def beginCellEditing(
        cell: CellPath,
        row: A,
        c: Column[A, FlatOnly],
        nav: NavState[A],
        edit: EditState
    )(using Frame): Any < Async =
        val move: Any < Async =
            for
                _ <- seedDraft(row, cell.column, c, edit)
                _ <- clearError(edit)
                r <- setEditingCell(Present(cell))
            yield r
        // Moving the editor is a way of leaving one, so it commits like every other: a
        // click from cell to cell used to open the next and drop what was typed into the
        // last, without asking anything and without saying so. A refusal keeps the reader
        // in the value they are fixing, the same as Enter and Tab do.
        edit.cell match
            case Present(open) if open != cell =>
                commitOpen(open, nav, edit).map { ok =>
                    val next: Any < Async = if ok then move else ()
                    next
                }
            case _ => move
        end match
    end beginCellEditing

    /** Commits whatever cell is open, wherever the gesture that ends it came from.
      *
      * The open cell is a path, so its row is found among the rendered ones by key and its
      * column among the leaves. A path naming neither is a table that has changed under an
      * open editor, and there is nothing left to commit.
      */
    private def commitOpen(open: CellPath, nav: NavState[A], edit: EditState)(using Frame): Boolean < Async =
        (nav.rows.find(r => keyOf(r) == open.row), leafPaths.find(_._1 == open.column)) match
            case (Some(r), Some((_, c))) => commitCell(open, r, c, edit)
            case _                       => true

    /** Opens a row: every editable column's draft is seeded before the row joins the set. */
    private def beginRowEditing(id: String, row: A, edit: EditState)(using Frame): Any < Async =
        for
            _ <- seedRowDrafts(row, edit)
            _ <- clearError(edit)
            r <- setRowEditing(id, true)
        yield r

    /** Writes one committed row back where the table can reach it, and reports it.
      *
      * With `rows(ref)` bound the table stores the row itself; without it the change
      * leaves through the callbacks and storing it is the caller's. Both run, so a bound
      * table can still observe.
      */
    private def applyCell(cell: CellPath, before: A, after: A, ed: CellEdit[A])(using Frame): Any < Async =
        val store: Any < Async = rowsRefV match
            case Present(ref) => ref.getAndUpdate(_.map(r => if keyOf(r) == cell.row then after else r))
            case Absent       => ()
        val column: Any < Async = ed.changed match
            case Present(f) => f(before, after)
            case Absent     => ()
        val table: Any < Async = onCellChangedF match
            case Present(f) => f(CellChange(cell.row, cell.column, before, after))
            case Absent     => ()
        for
            _ <- store
            _ <- column
            r <- table
        yield r
        end for
    end applyCell

    /** Commits one cell: read the draft, run the column's pipeline over it, and let the
      * result decide whether the cell closes.
      *
      * Text that will not parse, and a value a rule refuses, both keep the cell open on
      * what the reader typed, with the error under the editor. A value equal to what the
      * cell already showed closes without writing and without reporting, which is where
      * "fires only when the value changed" comes from: the comparison is on the TEXT, so
      * it needs no equality on the row type.
      *
      * Answers whether the cell CLOSED, which is what the caller needs to decide where
      * focus goes: a cell that stayed open still has the reader in it.
      */
    private def commitCell(cell: CellPath, row: A, c: Column[A, FlatOnly], edit: EditState)(using
        Frame
    ): Boolean < Async =
        (c.editV, edit.draftOf(cell.column)) match
            case (Present(ed), Present(ref)) =>
                for
                    text <- ref.get
                    res  <- ed.commit(row, text)
                    out <- res match
                        case Result.Success(after) =>
                            val moved: Any < Async = if ed.show(row) == text then () else applyCell(cell, row, after, ed)
                            for
                                _ <- moved
                                _ <- clearError(edit)
                                _ <- setEditingCell(Absent)
                            yield true
                            end for
                        case Result.Failure(e) => setError(cell, e, edit).andThen(false)
                        case Result.Panic(ex) =>
                            setError(cell, FieldError("error", Map.empty, Maybe(ex.getMessage)), edit).andThen(false)
                yield out
            case _ => setEditingCell(Absent).andThen(true)

    /** Cancels a cell: the draft is dropped by the next seed, so there is nothing to undo. */
    private def cancelCell(edit: EditState)(using Frame): Any < Async =
        for
            _ <- clearError(edit)
            r <- setEditingCell(Absent)
        yield r

    /** Commits a whole row, one editable column at a time, left to right.
      *
      * The first column that refuses stops the fold and leaves the row open with its error
      * showing, so a row never lands half written. Every column that changed reports
      * through [[applyCell]] before the row's own [[onRowValueChanged]] does, which is the
      * order AG Grid's `cellValueChanged` and `rowValueChanged` run in.
      */
    private def commitRow(id: String, row: A, edit: EditState)(using Frame): Boolean < Async =
        def loop(rest: List[(List[String], Column[A, FlatOnly])], acc: A): Result[(CellPath, FieldError), A] < Async =
            rest match
                case Nil => Kyo.lift(Result.succeed(acc))
                case (path, c) :: tail =>
                    val cell = CellPath(id, path)
                    (c.editV, edit.draftOf(path)) match
                        case (Present(ed), Present(ref)) =>
                            for
                                text <- ref.get
                                res  <- ed.commit(acc, text)
                                out <- res match
                                    case Result.Success(next) =>
                                        val moved: Any < Async =
                                            if ed.show(acc) == text then () else applyCell(cell, acc, next, ed)
                                        moved.andThen(loop(tail, next))
                                    case Result.Failure(e) => Kyo.lift(Result.fail((cell, e)))
                                    case Result.Panic(ex) =>
                                        Kyo.lift(Result.fail((cell, FieldError("error", Map.empty, Maybe(ex.getMessage)))))
                            yield out
                        case _ => loop(tail, acc)
                    end match
        for
            res <- loop(editableLeaves.filter((_, c) => c.editV.exists(_.when(row))), row)
            out <- res match
                case Result.Success(after) =>
                    val fire: Any < Async = onRowChangedF match
                        case Present(f) => f(RowChange(id, row, after))
                        case Absent     => ()
                    for
                        _ <- fire
                        _ <- clearError(edit)
                        _ <- setRowEditing(id, false)
                    yield true
                    end for
                case Result.Failure((cell, e)) => setError(cell, e, edit).andThen(false)
                case Result.Panic(ex)          => setRowEditing(id, false).andThen(true)
        yield out
        end for
    end commitRow

    /** Cancels a row edit: the drafts are re-seeded the next time it opens. */
    private def cancelRow(id: String, edit: EditState)(using Frame): Any < Async =
        for
            _ <- clearError(edit)
            r <- setRowEditing(id, false)
        yield r

    /** Clicking a row updates the bound selection set (per the mode), then fires `onRowClick`. */
    private def activate(id: String, canSelect: Boolean)(using Frame): Any < Async =
        val setSelection: Any < Async = (selectedRef, selectionModeV) match
            case _ if !canSelect => ()
            case (Present(ref), SelectionMode.Single | SelectionMode.Radio) =>
                ref.getAndUpdate(cur => if cur == Set(id) then Set.empty else Set(id))
            case (Present(ref), SelectionMode.Multiple) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
        val fireClick: Any < Async = onRowClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end activate
end DataTable

object DataTable:
    def apply[A](): DataTable[A] = new DataTable[A]()

    /** How narrow a drag may leave a column, in CSS pixels. Prime's own floor: low enough
      * that a reader can push a column almost out of the way, high enough that the handle
      * they would need to pull it back is still there.
      */
    private[uic] val MinColumnWidth = 15.0

    /** A CSS length in whole pixels, where it is one the window arithmetic can read.
      *
      * Only `px` is a length whose value is known here: every other unit needs the
      * browser, and how many rows fit in the viewport is the first thing a window has to
      * work out. A length in any other unit is reported rather than guessed at.
      */
    private[uic] def pixels(css: String): Maybe[Int] =
        val t = css.trim
        if !t.endsWith("px") then Absent
        else Maybe.fromOption(t.dropRight(2).trim.toDoubleOption).filter(_ >= 1).map(_.toInt)
    end pixels

    /** How wide the component's own columns are, as the CSS variables that carry it.
      *
      * They exist because a frozen column's offset reaches past whichever of them stand
      * between it and the edge, and a width and an offset that are two separate numbers
      * are two numbers that can disagree. Being one variable, a theme that widens the
      * checkbox column moves everything stuck beside it in the same step.
      *
      * They are also what the fixed layout needs. Under it a column with no width takes
      * an equal share of what the sized ones leave over, which over a checkbox is far
      * more than a checkbox can use; Prime never meets this, since it renders a
      * `colgroup` only for a scrollable table and lets the browser size those columns by
      * their content everywhere else.
      */
    private[uic] val HandleWidth = "var(--p-uic-dt-handle-width)"
    private[uic] val ToggleWidth = "var(--p-uic-dt-toggle-width)"
    private[uic] val SelectWidth = "var(--p-uic-dt-select-width)"
    private[uic] val EditorWidth = "var(--p-uic-dt-editor-width)"

    /** The two widths after the boundary between them moves by `delta`.
      *
      * Their sum is preserved, which is what keeps the table exactly as wide as the space
      * it was given and leaves every column the reader is not touching alone. The delta is
      * clamped before it is applied rather than each width after, so a drag past the end
      * of one column stops the boundary instead of quietly pushing width into the other.
      */

    /** How far a pointer travels before a press on a header becomes a drag rather than the
      * click that sorts it, in CSS pixels.
      */
    private[uic] val DragThreshold = 4.0

    /** `xs` with the block `[from, until)` lifted out and put back in front of what was at
      * `at`, which is the one move a reorder makes. `at` addresses the list BEFORE the
      * block is lifted, since that is what the reader is pointing at.
      */
    private[uic] def moveBlock[T](xs: List[T], from: Int, until: Int, at: Int): List[T] =
        if from < 0 || until > xs.length || from >= until || (at >= from && at <= until) then xs
        else
            val block  = xs.slice(from, until)
            val rest   = xs.take(from) ++ xs.drop(until)
            val insert = if at <= from then at else at - (until - from)
            rest.take(insert) ++ block ++ rest.drop(insert)

    /** The frozen columns a free one stands between and their edge, by index. A `Start`
      * column with anything free in front of it, or an `End` one with anything free
      * behind it, is adrift: what is between a frozen column and its edge scrolls, and it
      * would take the frozen one along or leave it standing over the gap.
      */
    private[uic] def adrift(edges: List[Maybe[FrozenEdge]]): List[Int] =
        def isEdge(e: Maybe[FrozenEdge], side: FrozenEdge) = e.exists(_ == side)
        edges.zipWithIndex.collect {
            case (e, i)
                if (isEdge(e, FrozenEdge.Start) && edges.take(i).exists(!isEdge(_, FrozenEdge.Start))) ||
                    (isEdge(e, FrozenEdge.End) && edges.drop(i + 1).exists(!isEdge(_, FrozenEdge.End))) =>
                i
        }
    end adrift

    /** Whether a move leaves every column that refused to be reordered exactly where it
      * was. A pinned column keeps its INDEX and not merely its neighbours, so a block
      * passing over it is refused rather than quietly carrying it one place along.
      */
    private[uic] def holdsPinned(free: List[Boolean], from: Int, until: Int, at: Int): Boolean =
        moveBlock(free.zipWithIndex, from, until, at).zipWithIndex.forall {
            case ((movable, was), now) => movable || was == now
        }

    /** The id one rendered row carries so a grab can measure it, by its index in the whole
      * list rather than in the page, which is the index a drop is written in.
      */
    private[uic] def rowId(prefix: String, at: Int): String = s"$prefix-rw$at"

    /** One CSV field, quoted where RFC 4180 says it has to be: it holds a quote, the
      * separator, or a line break. Quotes inside are doubled, and nothing else is touched.
      */
    private[uic] def csvField(value: String, separator: String): String =
        val needsQuotes =
            value.contains('"') || value.contains('\n') || value.contains('\r') ||
                (separator.nonEmpty && value.contains(separator))
        if !needsQuotes then value
        else "\"" + value.replace("\"", "\"\"") + "\""
    end csvField

    /** The place a pointer at `y` is asking a held row to go: the nearest row edge, which
      * is a boundary between two rows, or the bottom of the last one.
      */
    private[uic] def nearestRow(d: RowDrag, y: Double): Int =
        d.edges.zipWithIndex.minByOption((at, _) => math.abs(at - y)).map((_, i) => d.first + i).getOrElse(d.from)

    private[uic] def resizeTo(width: Double, next: Double, delta: Double): (Double, Double) =
        val d = math.max(math.min(delta, next - MinColumnWidth), MinColumnWidth - width)
        (width + d, next - d)

    /** The width one column takes under `Expand`. Nothing gives it back, so the only
      * bound is the floor every column keeps.
      */
    private[uic] def expandTo(width: Double, delta: Double): Double =
        math.max(MinColumnWidth, width + delta)
end DataTable
