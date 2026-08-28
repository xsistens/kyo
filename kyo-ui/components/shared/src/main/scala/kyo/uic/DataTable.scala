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
    unusable: Set[List[String]] = Set.empty
):
    private[uic] def openOf(path: List[String]): Maybe[SignalRef[Boolean]] = Maybe.fromOption(open.get(path))
end FilterState

/** What one grab of a column boundary holds on to: where the pointer started, and the
  * two widths it started from. The paths are the handler's own, since a boundary only
  * ever moves the pair it sits between.
  */
final private[uic] case class ColumnGrab(startX: Double, width: Double, next: Double) derives CanEqual

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
    measure: String => Rect < Async = (_: String) => Rect(0, 0, 0, 0, 0, 0),
    grab: Maybe[SignalRef[Maybe[ColumnGrab]]] = Absent
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
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
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
    editingRowsRef: Maybe[SignalRef[Set[String]]] = Absent,
    editingCellRef: Maybe[SignalRef[Maybe[CellPath]]] = Absent,
    rowsRefV: Maybe[SignalRef[Seq[A]]] = Absent,
    cellNavV: Maybe[Boolean] = Absent,
    onCellChangedF: Maybe[CellChange[A] => Any < Async] = Absent,
    onRowChangedF: Maybe[RowChange[A] => Any < Async] = Absent,
    translatorV: ErrorTranslator = ErrorTranslator.default,
    hiddenPaths: List[(List[String], Column[A, FlatOnly])] = Nil,
    columnFiltersRef: Maybe[SignalRef[Map[List[String], ColumnFilter]]] = Absent,
    columnWidthsRef: Maybe[SignalRef[Map[List[String], Double]]] = Absent
) extends Node:
    type Self = DataTable[A]

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
      */
    def columnWidths(ref: SignalRef[Map[List[String], Double]]): DataTable[A] =
        copy(columnWidthsRef = Present(ref))

    /** Slices the rows into pages of `size` and renders the embedded paginator;
      * `ref` holds the 0-based page index (clamped at render).
      */
    def paginate(size: Int)(ref: SignalRef[Int]): DataTable[A] =
        copy(pageSizeV = Present(math.max(1, size)), pageRef = Present(ref))

    /** Selection semantics: `Single`/`Multiple` select on row click, `Checkbox`
      * via Prime's checkbox column; `None` (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): DataTable[A] = copy(selectionModeV = v)

    /** Binds selection two-way to `ref` (a set of [[rowKey]] ids). */
    def selected(ref: SignalRef[Set[String]]): DataTable[A] = copy(selectedRef = Present(ref))

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

    /** Content of the full-width `tr.p-datatable-empty-message` row shown when no rows
      * survive filtering.
      */
    def emptyContent(v: String): DataTable[A] = copy(emptyContentV = Present(EmptyContent.const(v)))

    /** Reactive text: re-renders the empty slot in place on signal emission. */
    def emptyContent(sig: Signal[String]): DataTable[A] = copy(emptyContentV = Present(EmptyContent.dyn(sig)))

    /** Arbitrary UI for the empty state: an icon over a line of explanation and the
      * button that creates the first record, rendered in the same slot the text would
      * occupy.
      */
    def emptyContent(ui: UI): DataTable[A] = copy(emptyContentV = Present(EmptyContent.ui(ui)))

    /** Fired with the row key after any selection write from a row click. */
    def onRowClick(f: String => Any < Async): DataTable[A] = copy(onRowClickF = Present(f))

    /** `aria-label` for the table. */
    def accessibleName(v: String): DataTable[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): DataTable[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** `aria-labelledby` id reference for the table. */
    def accessibleNameRef(v: String): DataTable[A] = copy(accNameRefV = Present(v))

    /** Toolbar slot above the table (`div.p-datatable-header`), the place for a
      * global-filter input, a title, or action buttons.
      */
    def header(ui: UI): DataTable[A] = copy(headerV = Present(ui))

    /** Slot below the table and the paginator (`div.p-datatable-footer`). For per
      * column summary cells use [[Column.footer]], which renders a real `tfoot`.
      */
    def footer(ui: UI): DataTable[A] = copy(footerV = Present(ui))

    /** Busy state: a spinner over a dimming mask (`.p-datatable-mask`) covers the
      * table while data is being fetched.
      */
    def loading(v: Boolean): DataTable[A] = copy(loadingV = Present(BoolValue.Const(v)))

    /** Reactive busy state, bound to the data-fetch in-flight signal; the mask toggles
      * in its own sub-region without re-rendering the rows.
      */
    def loading(sig: Signal[Boolean]): DataTable[A] = copy(loadingV = Present(BoolValue.Dyn(sig)))

    /** Caps the table container at a CSS length and scrolls it, pinning the `thead`
      * (and the `tfoot`, when columns carry footers) to the container edges.
      */
    def scrollHeight(v: String): DataTable[A] = copy(scrollHeightV = Present(v))

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

    private def expanderColumn: Boolean = expansionF.isDefined

    /** Row editing adds Prime's editor-button column, at the trailing edge. Binding the
      * state IS the switch, the way `expanded(ref)` is for the expander column: a second
      * flag would only be a way to bind one without the other.
      */
    private def editorColumn: Boolean = editingRowsRef.isDefined

    private def rowInteractive: Boolean = rowClickSelects || onRowClickF.isDefined

    /** Renders through whichever ui-state refs are bound (nested reactive nodes
      * render through in SSR).
      */
    private def withRef[T](ref: Maybe[SignalRef[T]], fallback: T)(k: T => UI)(using Frame): UI =
        ref match
            case Present(r) => r.render(k)
            case Absent     => k(fallback)

    /** Whether anything can be edited at all, which is what decides whether the table
      * needs state of its own.
      */
    private def editingBound: Boolean = editingCellRef.isDefined || editingRowsRef.isDefined

    /** Navigation follows editing unless the caller says otherwise. */
    private def navOn: Boolean = cellNavV.getOrElse(leafCols.exists(_.isEditable))

    /** Whether the table owns state no caller supplies, which is what decides whether it
      * renders through a mount: the drafts and the standing error of an editing table,
      * and the menu-open state of a filter row.
      */
    private def ownsState: Boolean = editingBound || navOn || filterRowOn || resizeOn

    /** Whether the filter row is rendered: the filters have to be bound somewhere, and
      * some column has to carry a pipeline for the row to hold anything.
      */
    private def filterRowOn: Boolean = columnFiltersRef.isDefined && leafCols.exists(_.isFilterable)

    /** Whether any boundary is draggable: the widths have to be bound somewhere, and a
      * boundary needs a resizable column on both sides of it, so a single column has none
      * and neither does a table whose columns are all pinned.
      */
    private def resizeOn: Boolean =
        columnWidthsRef.isDefined && leafCols.sliding(2).exists(p => p.size == 2 && p.forall(_.resizableFlag))

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
    private def resizableAt(i: Int): Boolean =
        columnWidthsRef.isDefined && i + 1 < leafCols.length &&
            leafCols(i).resizableFlag && leafCols(i + 1).resizableFlag

    /** The id one header cell carries so a grab can measure it. Every leaf header gets
      * one, the last included: it carries no handle itself, but it is the neighbour the
      * one before it trades width with.
      */
    private def headerId(i: Int, size: SizeState): String = s"${size.idPrefix}-h$i"

    /** Whether the table sizes its columns itself, which is what puts a `colgroup` in
      * front of the header and makes the widths in it authoritative.
      */
    private def hasWidths: Boolean = columnWidthsRef.isDefined || leafCols.exists(_.widthV.isDefined)

    /** Whether any column asks to be held against an edge, which is also what puts the
      * table in a scroll container: a column can only be frozen against something that
      * scrolls, and a wide table with no cap on its height still scrolls sideways.
      */
    private def frozenOn: Boolean = leafCols.exists(_.frozenV.isDefined)

    private def frozenAt(i: Int, edge: FrozenEdge): Boolean =
        i >= 0 && i < leafCols.length && leafCols(i).frozenV.exists(_ == edge)

    /** The component's own columns between the leading edge and the first data column,
      * as the CSS terms an offset reaching past them is written in.
      */
    private def leadTerms: List[String] =
        (if expanderColumn then List(DataTable.ToggleWidth) else Nil) ++
            (if checkboxColumn then List(DataTable.SelectWidth) else Nil)

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
        leafPaths.zipWithIndex.collect {
            case ((p, c), i)
                if (c.frozenV.exists(_ == FrozenEdge.Start) && (0 until i).exists(j => !frozenAt(j, FrozenEdge.Start))) ||
                    (c.frozenV.exists(_ == FrozenEdge.End) &&
                        (i + 1 until leafCols.length).exists(j => !frozenAt(j, FrozenEdge.End))) =>
                p
        }

    /** The header groups spanning a frozen column and a free one, or two frozen against
      * opposite edges. One cell cannot half scroll, so the group says the columns under
      * it disagree about where they belong.
      */
    private def splitFrozenGroups: List[List[String]] =
        ColumnTree.spans(cols).flatMap(withLeafOffsets).collect {
            case (sp, at) if sp.node.asColumn.isEmpty && mixedEdges(at, at + sp.colspan) => sp.path
        }

    private def mixedEdges(from: Int, until: Int): Boolean =
        val edges = (from until until).toList.map(i => leafCols(i).frozenV)
        edges.exists(_.isDefined) && edges.distinct.length > 1

    /** Each header cell of one row paired with the index of its leftmost leaf. The spans
      * tile the columns left to right, so the running sum of the colspans IS that index,
      * and no second walk of the tree can disagree with the one the header rendered.
      */
    private def withLeafOffsets(cells: List[ColumnTree.HeaderSpan[A]]): List[(ColumnTree.HeaderSpan[A], Int)] =
        cells.scanLeft(0)((at, sp) => at + sp.colspan).zip(cells).map((at, sp) => (sp, at))

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
        if !ownsState then renderWith(EditState(Set.empty, Absent), NavState[A](), FilterState(), SizeState())
        else
            UI.mounted {
                for
                    cmds   <- UI.commands
                    prefix <- cmds.freshId
                    drafts <- Kyo.foreach(editableLeaves)((path, _) => Signal.initRef("").map(path -> _))
                    err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
                    menus  <- Kyo.foreach(filterableLeaves)((path, _) => Signal.initRef(false).map(path -> _))
                    held   <- Signal.initRef(Absent: Maybe[ColumnGrab])
                yield wired(
                    prefix,
                    drafts.toMap,
                    err,
                    id => cmds.focusId(id),
                    menus.toMap,
                    id => cmds.requestMeasureById(id),
                    Present(held)
                )
            }.placeholder(renderWith(EditState(Set.empty, Absent), NavState[A](), FilterState(), SizeState()))

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
        measure: String => Rect < Async = (_: String) => Rect(0, 0, 0, 0, 0, 0),
        held: Maybe[SignalRef[Maybe[ColumnGrab]]] = Absent
    )(using Frame): UI =
        errRef.render(e =>
            withRef(editingRowsRef, Set.empty[String]) { editRows =>
                withRef(editingCellRef, Absent: Maybe[CellPath]) { editCell =>
                    renderWith(
                        EditState(editRows, editCell, drafts, Present(errRef), e, live = true),
                        NavState[A](on = navOn, idPrefix = idPrefix, focus = focus),
                        FilterState(open = menus, live = true),
                        SizeState(idPrefix = idPrefix, live = true, measure = measure, grab = held)
                    )
                }
            }
        )

    private def renderWith(edit: EditState, nav: NavState[A], filter: FilterState, size: SizeState)(using Frame): UI =
        withVisibleColumns(_.buildAll(edit, nav, filter, size))

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

    private def buildAll(edit: EditState, nav: NavState[A], filter: FilterState, size: SizeState)(using Frame): UI =
        withSortableFlags { flags =>
            withRows { rows =>
                withRef(sortRef, List.empty[SortKey]) { sort =>
                    withRef(filterRef, "") { query =>
                        withRef(columnFiltersRef, Map.empty[List[String], ColumnFilter]) { specs =>
                            withRef(columnWidthsRef, Map.empty[List[String], Double]) { widths =>
                                withRef(pageRef, 0) { page =>
                                    withRef(selectedRef, Set.empty[String]) { sel =>
                                        withRef(expandedRef, Set.empty[String]) { exp =>
                                            withRef(expandedGroupsRef, Set.empty[GroupPath]) { groups =>
                                                body(
                                                    rows,
                                                    sort,
                                                    query,
                                                    page,
                                                    sel,
                                                    exp,
                                                    groups,
                                                    flags,
                                                    edit,
                                                    nav,
                                                    filter.copy(specs = specs),
                                                    size.copy(widths = widths)
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

    /** The rows the table renders: the bound list when there is one, the appended list
      * otherwise. A bound list is what lets a committed edit reach the screen, since the
      * table stores the new row into it.
      */
    private def withRows(k: Seq[A] => UI)(using Frame): UI =
        rowsRefV match
            case Present(ref) => ref.render(k)
            case Absent       => k(rowsV)

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
                    case (Present(f), Present(cf)) if f.query.trim.nonEmpty => List((path, cf.predicate(f)))
                    case _                                                  => Nil
            }

    /** This column's resolved sortable flag: the reactive ones are in `flags`, the rest
      * carry theirs statically.
      */
    private def sortableFlag(c: Column[A, FlatOnly], path: List[String], flags: Map[List[String], Boolean]): Boolean =
        flags.getOrElse(path, c.sortableConst)

    private def body(
        rowsIn: Seq[A],
        sort: List[SortKey],
        query: String,
        page: Int,
        sel: Set[String],
        exp: Set[String],
        openGroups: Set[GroupPath],
        flags: Map[List[String], Boolean],
        edit: EditState,
        nav: NavState[A],
        filterIn: FilterState,
        size: SizeState
    )(using Frame): UI =
        // 1. Global filter: contains-match over the columns' text projections.
        val rows = rowsIn.toList
        val global =
            if query.isEmpty then rows
            else
                val q = query.toLowerCase
                rows.filter(a => leafCols.exists(c => c.textF.exists(f => f(a).toLowerCase.contains(q))))

        // 1b. Column filters: every bound one has to pass. A query this table cannot read
        //     as a value of its column's type filters nothing and says so on its own
        //     input, rather than emptying the table behind a typo.
        val reads    = filterReads(filterIn.specs)
        val filter   = filterIn.copy(unusable = reads.collect { case (p, Absent) => p }.toSet)
        val filtered = reads.foldLeft(global)((rs, r) => r._2.fold(rs)(p => rs.filter(p)))

        // 2. Sort: apply the SORTING entries back-to-front through stable sorts, so the
        //    first one ends up the primary key. Unsorted entries hold a slot in the
        //    priority order and contribute nothing here.
        val sorted = SortKey.sorting(sort).reverse.foldLeft(filtered) { (rs, k) =>
            allPaths.find(_._1 == k.path).flatMap(_._2.orderingV.toOption) match
                case Some(ord) => rs.sorted(using if k.direction == SortDirection.Ascending then ord else ord.reverse)
                case None      => rs
        }

        // 3. Paginate: clamp the 0-based page, slice, and embed the standalone
        //    Paginator (resolved page passed directly — the table already renders
        //    inside its own page-ref subscription).
        val (paged, paginatorUI) = pageSizeV match
            case Present(size) =>
                val totalPages = math.max(1, (sorted.size + size - 1) / size)
                val cur        = math.min(math.max(page, 0), totalPages - 1)
                var pag = Paginator()
                    .totalRecords(sorted.size)
                    .rows(size)
                    .currentPage(cur)
                    .hostClass("p-datatable-paginator-bottom")
                pageRef.foreach(ref => pag = pag.page(ref))
                (sorted.slice(cur * size, cur * size + size), List(pag.render))
            case Absent => (sorted, Nil)

        // The paths whose headers the reader can actually click, which is what both click
        // transitions may clear. Everything else in the spec is the caller's to keep.
        val interactive: Set[List[String]] =
            if sortRef.isEmpty then Set.empty
            else leafPaths.collect { case (p, c) if c.isSortable(sortableFlag(c, p, flags)) => p }.toSet

        val colCount =
            leafCols.length + (if checkboxColumn then 1 else 0) + (if expanderColumn then 1 else 0) +
                (if editorColumn then 1 else 0)

        val frozen = frozenPlan(size)

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
                val ths = withLeafOffsets(cells).map((sp, at) => headerSpanCell(sp, at, sort, flags, interactive, size, frozen))
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

        val bodyRows: List[UI] =
            if paged.isEmpty then
                List(
                    tr.cssClass("p-datatable-empty-message")(
                        toChild(EmptyContent.render(emptyContentV, "No records found")(c =>
                            td.colspan(math.max(colCount, 1))(c)
                        ))
                    )
                )
            else groupSegments(paged.zipWithIndex, groupsV, Nil, sel, exp, openGroups, colCount, edit, navHere, frozen)

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
        if scrollHeightV.isDefined || frozenOn then tbl = tbl.cssClass("p-datatable-scrollable-table")
        // Prime's own classes carry the clipping a sized column needs (a value too long
        // for its column is cut rather than widening it); the layout mode they leave to
        // the host, which is what the `.p-uic-table-fixed` rule supplies.
        if hasWidths then tbl = tbl.cssClass("p-uic-table-fixed")
        if resizeOn then
            tbl = tbl.cssClass("p-datatable-resizable-table").cssClass("p-datatable-resizable-table-fit")
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => tbl = tbl.aria("labelledby", v))
        val tableEl: UI = tbl(
            (colGroup(size) ++ List[UI](
                thead.cssClass("p-datatable-thead")((headRows ++ filterRowUI).map(toChild)*),
                tbody.cssClass("p-datatable-tbody")(bodyRows.map(toChild)*)
            ) ++ footGroup).map(toChild)*
        )

        var container = div.cssClass("p-datatable-table-container")
        scrollHeightV.foreach(h => container = container.style(_.maxHeight(CssValue.length(h))))
        val containerEl: UI = container(toChild(tableEl))

        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-datatable-header")(toChild(h)))
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-datatable-footer")(toChild(f)))

        var root = div.cssClass("p-datatable").cssClass("p-component")
        // Prime: hoverable whenever a selection mode is set (checkbox included) or
        // rows react to clicks.
        if selectionModeV != SelectionMode.None || onRowClickF.isDefined then
            root = root.cssClass("p-datatable-hoverable")
        if stripedFlag then root = root.cssClass("p-datatable-striped")
        if gridlinesFlag then root = root.cssClass("p-datatable-gridlines")
        // Prime's frozen rules only apply inside a scrollable table, and rightly so: a
        // column held against an edge means nothing until something moves past it.
        if scrollHeightV.isDefined || frozenOn then root = root.cssClass("p-datatable-scrollable")
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
            ) ++ frozenCards(size) ++ loadingMask ++ headerSlot ++ (containerEl :: paginatorUI) ++ footerSlot).map(toChild)*
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
        loadingV match
            case Present(BoolValue.Const(true))  => List(maskDiv)
            case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then maskDiv else UI.empty))
            case Present(BoolValue.Const(false)) => Nil
            case Absent                          => Nil
        end match
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
            selectedRef.isDefined || expandedRef.isDefined || onRowClickF.isDefined ||
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
        val sortable                         = allPaths.filter(_._2.orderingV.isDefined).map(_._1)
        val unknown                          = if sortRef.isEmpty then Nil else sort.map(_.path).filterNot(sortable.contains).map(show)
        val ambiguous =
            if sortRef.isEmpty then Nil else KeyDiagnostics.duplicates(sortable.map(show))
        // A column asked to be sortable with nothing to sort by: the flag decides whether the
        // reader may change the spec, and an ordering is what a change would act on.
        val noOrdering = leafPaths.collect {
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
                        "the column followed by its header, and the column needs a sortBy",
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
        nothing ++ nowhere ++ unknownCard
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
        pinned ++ unknownCard
    end sizeCards

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
        val keys        = inFilter.map(keyOf)
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
        at: Int,
        sort: List[SortKey],
        flags: Map[List[String], Boolean],
        interactive: Set[List[String]],
        size: SizeState,
        frozen: FrozenPlan
    )(using Frame): UI =
        sp.node.asColumn match
            case Present(c) =>
                headerCell(c, sp.path, sort, sp.rowspan, sortableFlag(c, sp.path, flags), interactive, at, size, frozen)
            case Absent =>
                var cell = th.cssClass("p-datatable-header-cell")
                if sp.colspan > 1 then cell = cell.colspan(sp.colspan)
                freeze(
                    cell(
                        div.cssClass("p-datatable-column-header-content")(
                            toChild(span.cssClass("p-datatable-column-title")(sp.node.label))
                        )
                    ),
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
                item.onClick(pickMode(path, cur, m, openRef))(m.label)
            }.map(toChild)*
        )

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
            case Present(ref) => ref.getAndUpdate(_ + (path -> cur.copy(mode = mode)))
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
        if !resizableAt(i) then Nil
        else
            var handle = span
                .cssClass("p-datatable-column-resizer")
                .aria("hidden", "true")
                .onClick(())
                .stopPropagation(true)
            if size.live then
                val path = leafPaths(i)._1
                val next = leafPaths(i + 1)._1
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
                for
                    a <- size.measure(headerId(i, size))
                    b <- size.measure(headerId(i + 1, size))
                    r <- ref.set(Present(ColumnGrab(e.rectX + e.x, a.width, b.width)))
                yield r
            case Absent => ()

    /** The pointer moving with a boundary held: write both columns of the pair, computed
      * from where the grab started rather than from the last frame, so a drag that
      * outruns the render does not drift.
      */
    private def dragResize(path: List[String], next: List[String], e: PointerEvent, size: SizeState)(using
        Frame
    ): Any < Async =
        (size.grab, columnWidthsRef) match
            case (Present(g), Present(ref)) =>
                g.get.map {
                    case Present(held) =>
                        val (w, n) = DataTable.resizeTo(held.width, held.next, e.rectX + e.x - held.startX)
                        ref.getAndUpdate(_ + (path -> w) + (next -> n))
                    case Absent => ()
                }
            case _ => ()

    private def headerCell(
        c: Column[A, FlatOnly],
        path: List[String],
        sort: List[SortKey],
        rows: Int,
        flag: Boolean,
        interactive: Set[List[String]],
        index: Int,
        size: SizeState,
        frozen: FrozenPlan
    )(using Frame): UI =
        val sortable  = c.isSortable(flag) && sortRef.isDefined
        val sortingKs = SortKey.sorting(sort)
        val rank      = sortingKs.indexWhere(_.path == path)
        val direction = sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted)

        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        // Only once the mount has run: the id exists to be measured, and stamping it in the
        // static projection would put the same one on every table of a page.
        if size.live && columnWidthsRef.isDefined && index >= 0 then cell = cell.id(headerId(index, size))
        if index >= 0 && resizableAt(index) then cell = cell.cssClass("p-datatable-resizable-column")
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell.cssClass("p-datatable-sortable-column").tabIndex(0).onClick(e => toggleSort(path, e, interactive))
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
                ((span.cssClass("p-datatable-column-title")(c.headerV): UI) :: (sortIcon ++ sortBadge)).map(toChild)*
            )
        freeze(cell((content :: (if index >= 0 then resizer(index, size) else Nil)).map(toChild)*), frozen.at(index))
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
        exp: Set[String],
        openGroups: Set[GroupPath],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan
    )(using Frame): List[UI] =
        levels match
            case Nil => leafRows(rows, sel, exp, colCount, edit, nav, frozen)
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
                        else groupSegments(run, rest, groupPath.keys, sel, exp, openGroups, colCount, edit, nav, frozen)
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
        exp: Set[String],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan
    )(using Frame): List[UI] =
        val spans = spanCells(rows.map(_._1), exp)
        rows.zip(spans).flatMap((row, cells) => dataRow(row._1, row._2, sel, exp, colCount, cells, edit, nav, frozen))
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
        exp: Set[String],
        colCount: Int,
        spans: Map[Int, SpanCell],
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan
    )(using Frame): List[UI] =
        val id      = keyOf(a)
        val isSel   = sel.contains(id)
        val isExp   = exp.contains(id)
        val rowEdit = edit.rows.contains(id)
        val navRow  = nav.indexOf(id, keyOf).getOrElse(-1)

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
                List(freeze(cell, frozen.leadAt(0)))

        // Checkbox selection reuses Prime's checkbox anatomy (as Tree does).
        val checkboxTd: List[UI] =
            if !checkboxColumn then Nil
            else
                var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
                if isSel then cb = cb.cssClass("p-checkbox-checked")
                cb = cb.onClick(toggleSelect(id))
                val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
                val cell           = td(cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*))))
                List(freeze(cell, frozen.leadAt(if expanderColumn then 1 else 0)))

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
                    cell = cell.cssClass("p-editable-column").onClick(beginCellEditing(here, a, c, edit))
                    // The click picks a cell, it does not also pick the row; and while the
                    // editor is open the keystrokes that leave it stop here rather than
                    // reaching the row's own handler.
                    cell = cell.stopPropagation(true)
                end if
                if cellEdit then cell = cell.cssClass("p-cell-editing")
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
                    if editing then toChild(editorCell(here, a, c, edit))
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
        if rowClickSelects then row = row.cssClass("p-datatable-selectable-row")
        if isSel then row = row.cssClass("p-datatable-row-selected")
        if selectionModeV != SelectionMode.None then row = row.aria("selected", isSel.toString)
        if rowInteractive then row = row.tabIndex(0).onClick(activate(id))
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
        val rowEl: UI = row((expanderTd ++ checkboxTd ++ dataTds ++ editorTd).map(toChild)*)

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
    private def editorCell(cell: CellPath, row: A, c: Column[A, FlatOnly], edit: EditState)(using Frame): UI =
        (c.editV, edit.draftOf(cell.column)) match
            case (Present(ed), Present(draft)) =>
                val params = EditorParams(
                    draft = draft,
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
                def closing(closed: Boolean < Async): Any < Async =
                    closed.map(c => if c && !isTab then focusCell(nav, cell) else ())
                val editStep: Any < Async = step.edit match
                    case GridNav.EditOp.Keep       => ()
                    case GridNav.EditOp.Open(seed) => openCell(cell, row, c, seed, edit)
                    case GridNav.EditOp.Commit =>
                        if rowEdit then (if isTab then () else closing(commitRow(cell.row, row, edit)))
                        else closing(commitCell(cell, row, c, edit))
                    case GridNav.EditOp.Cancel =>
                        if rowEdit then closing(cancelRow(cell.row, edit).andThen(true))
                        else closing(cancelCell(edit).andThen(true))
                val focusStep: Any < Async = step.focus match
                    case Present(to) if step.moveFocus => nav.focus(cellId(nav, to))
                    case Present(to)                   => if rowEdit then () else openAt(to, nav, edit)
                    case Absent                        => ()
                val selectStep: Any < Async =
                    if step.selectRow && rowInteractive then activate(cell.row) else ()
                for
                    _ <- editStep
                    _ <- focusStep
                    r <- selectStep
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
    private def beginCellEditing(cell: CellPath, row: A, c: Column[A, FlatOnly], edit: EditState)(using Frame): Any < Async =
        for
            _ <- seedDraft(row, cell.column, c, edit)
            _ <- clearError(edit)
            r <- setEditingCell(Present(cell))
        yield r

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
    private def activate(id: String)(using Frame): Any < Async =
        val setSelection: Any < Async = (selectedRef, selectionModeV) match
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
    private[uic] def resizeTo(width: Double, next: Double, delta: Double): (Double, Double) =
        val d = math.max(math.min(delta, next - MinColumnWidth), MinColumnWidth - width)
        (width + d, next - d)
end DataTable
