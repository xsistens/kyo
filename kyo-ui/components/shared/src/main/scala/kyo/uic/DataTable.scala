package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal
import kyo.internal.NumberFormat
import kyo.uic.form.ErrorTranslator
import kyo.uic.form.FieldError

/** The address of one cell: the row itself and the column's path, the same path the sort spec
  * names a column by. It is what [[DataTable.editingCell]] and [[DataTable.selectedCells]] hold,
  * and a pair rather than a single value because a cell is the crossing of the two. The row is
  * matched by the table's [[DataTable.rowKey]], as a selected row is.
  */
final case class CellPath[A](row: A, column: List[String]) derives CanEqual

/** A cell as the table compares it inside: the row's key and the column's path. */
final private[uic] case class CellKey(row: String, column: List[String]) derives CanEqual

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
    cell: Maybe[CellKey],
    drafts: Map[List[String], SignalRef[String]] = Map.empty,
    errorRef: Maybe[SignalRef[Maybe[(CellKey, FieldError)]]] = Absent,
    error: Maybe[(CellKey, FieldError)] = Absent,
    // False in the static projection, where the mount has not run and there are no drafts
    // to write into. The anatomy still renders; what it does not do is offer an
    // affordance that could not work, which is the same trade Select's placeholder makes.
    live: Boolean = false
):
    private[uic] def draftOf(path: List[String]): Maybe[SignalRef[String]] = Maybe.fromOption(drafts.get(path))

    private[uic] def errorAt(cell: CellKey): Maybe[FieldError] =
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
    focus: String => UI.Action = (_: String) => ()
):
    private[uic] def indexOf(key: String, keyOf: A => String): Maybe[Int] =
        val i = rows.indexWhere(r => keyOf(r) == key)
        if i < 0 then Absent else Present(i)
end NavState

/** What a row click needs beyond the selection itself, once [[DataTable.metaKeySelection]] is
  * on: where a range would be measured, and where it would start.
  *
  * `keys` is the FILTERED rows in the order the reader sees them, across every page: the same
  * list the select-all header covers, so a range and a select-all cannot disagree about what
  * "all of them" means. It is empty when no range is possible, which is also how the render
  * without a mount says so.
  *
  * `anchor` is the last row picked WITHOUT shift. It is the one piece of selection state the
  * caller does not own (a `Set` has no order, so it cannot be derived from the selection), and
  * the only reason a table with modifier selection needs a mount at all. A shift-click READS it
  * and leaves it where it is, which is what makes widening and narrowing the same range one
  * gesture repeated rather than a walk.
  */
// The key list a shift-range spans used to live here. It is read from `bodyViewNow` at the
// moment of the click now, and carrying it as well was not merely redundant: this value goes
// into every row's spec, so a list that grew by one made all of them unequal and the body
// rendered every row again for an append.
final private[uic] case class SelectState[A](
    metaKey: Boolean = false,
    anchor: Maybe[ReactiveValue[Maybe[A]]] = Absent,
    onAnchor: Maybe[Maybe[A] => UI.Action] = Absent
)

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
    drafts: Map[List[String], SignalRef[ColumnFilter]] = Map.empty,
    // What the reader typed into a row input over filters bound one way, kept until they leave the field: the owner writes
    // the filters back when it likes, and an answer to an older keystroke must not replace a newer one on the screen.
    typed: Map[List[String], SignalRef[Maybe[String]]] = Map.empty,
    idPrefix: String = ""
):
    private[uic] def openOf(path: List[String]): Maybe[SignalRef[Boolean]]        = Maybe.fromOption(open.get(path))
    private[uic] def draftOf(path: List[String]): Maybe[SignalRef[ColumnFilter]]  = Maybe.fromOption(drafts.get(path))
    private[uic] def typedOf(path: List[String]): Maybe[SignalRef[Maybe[String]]] = Maybe.fromOption(typed.get(path))
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

/** Where the reader is in the list, and what that leaves on the screen. */
final private[uic] case class Paging[A](rows: List[A], base: Int, current: Int)

/** What the ROWS of a table are made of, as one value.
  *
  * The table's other bindings (column order, visibility, sort, filter, page, widths) rebuild
  * every cell when they move, so resolving them around the body is right and costs nothing. These
  * do not. A selection, a context row, a picked cell, an expanded row, a collapsed group each
  * change a handful of `<tr>`s; and the row list itself, when it re-emits, usually carries mostly
  * the rows it carried before. Held as one value, they can drive the row list rather than the
  * component around it.
  *
  * The rows belong here even though they look like the table's most fundamental input, and that is
  * the whole point: a caller whose selection lives in the DATA (a client field on the row, with
  * `selected` derived from the same signal) never emits a selection, only rows. Leaving the rows
  * outside would have made the reuse unreachable for exactly the callers who need it most.
  */
final private[uic] case class RowInputs[A](
    rows: Seq[A] = Nil,
    offset: Int = 0,
    sel: Set[String] = Set.empty,
    ctx: Maybe[String] = Absent,
    cells: Set[CellKey] = Set.empty,
    exp: Set[String] = Set.empty,
    groups: Set[GroupPath] = Set.empty,
    // Whether the rows are still on their way (the `loading` slot). Held with the rows because it
    // decides what an empty row list MEANS: a page not answered yet, or a query that matched nothing.
    waiting: Boolean = false,
    // What is known about the size of the whole (`lazyRows`). Held with the rows for the same
    // reason: while a page loads, it says how many rows the page will have.
    total: Total = Total.Unknown(false)
)

private[uic] object RowInputs:
    // Declared rather than derived: deriving would demand a `CanEqual[A, A]` from every caller,
    // and the comparison that matters here is the case class's own, which compares the rows by
    // their own equality whatever that is.
    given canEqual[A]: CanEqual[RowInputs[A], RowInputs[A]] = CanEqual.derived
end RowInputs

/** The whole-list values a row handler needs, as one value that can be read AT THE MOMENT IT RUNS.
  *
  * A handler closed over these is correct only for as long as every emission rebuilds it, which is
  * what a table that re-renders its whole body does today. The moment rows are retained across an
  * emission, a retained row keeps its handler, and a handler holding last emission's list will span
  * a range over rows that moved, write back an order missing what was appended since, or fail to
  * find the row it is committing and discard the reader's input.
  *
  * So they are read rather than closed over, which is the rule this module already writes down for
  * the drag ("reads the ref LIVE, never a closed-over render-time value") and for the context menu
  * ("reading it now is reading what is selected at the moment of the click"). The values come back
  * from the same bound refs the render read, through the same arrangement pipeline, so there is one
  * implementation of "what is on the screen" and not two.
  */
final private[uic] case class BodyView[A](
    selectKeys: IndexedSeq[String],
    navRows: Vector[A],
    navPage: Int,
    moveAll: Seq[A],
    moveBase: Int,
    moveCount: Int
)

/** One `<tr>` of a table body, as data rather than as markup.
  *
  * A body does not emit one element per row: a row with an expansion is two, a group contributes a
  * header and a summary that belong to no row, and a windowed body pads with spacers and draws slots
  * for rows the source has not reached. So the body's list is the stream of `<tr>`s, and this is one
  * element of it: a description complete enough that rendering it needs nothing else.
  *
  * Why the shape matters. A keyed list region reuses a row only when it paints as exactly ONE
  * element, and only when the value it was rendered from is `equals` to the previous one. Splitting
  * the stream this way is what makes the first true; carrying resolved values rather than the
  * table-wide sets they came from is what will make the second useful. Nothing depends on either
  * yet: today the body renders this list inline, exactly as it rendered the rows before.
  */
sealed private[uic] trait RowSpec[A]

private[uic] object RowSpec:

    /** Entries are compared to decide whether a row has to be rendered again, so equality here is
      * the question "would this `<tr>` come out differently". The cases are case classes, so it is
      * their fields, and a field holding a function compares by reference, which is why what a
      * handler needs about the whole list is READ at event time (see [[BodyView]]) rather than
      * carried here: a lambda rebuilt per emission would make every row unequal and reuse nothing.
      */
    given canEqual[A]: CanEqual[RowSpec[A], RowSpec[A]] = CanEqual.derived

    /** A data row.
      *
      * The four reader-state fields are NARROWED to this row on purpose. `dataRow` was handed the
      * table-wide sets and asked them about its own key; carrying those sets here instead would
      * make every row of the table unequal the moment any one row's selection changed, which is
      * reuse spent on nothing. What decides whether this `<tr>` is painted again has to be about
      * this `<tr>`.
      */
    final case class Data[A](
        a: A,
        index: Int,
        selected: Boolean,
        contextRow: Boolean,
        pickedCells: Set[List[String]],
        expanded: Boolean,
        colCount: Int,
        spans: Map[Int, SpanCell],
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A],
        select: SelectState[A],
        height: Maybe[Int] = Absent
    ) extends RowSpec[A]

    /** The second `<tr>` an expanded row emits. Its own entry, not part of the row's, because a row
      * that painted as two elements could never be reused.
      */
    final case class Expansion[A](a: A, colCount: Int) extends RowSpec[A]

    final case class GroupHead[A](
        level: RowGroup[A],
        path: GroupPath,
        rows: List[A],
        colCount: Int,
        collapsible: Boolean,
        open: Boolean
    ) extends RowSpec[A]

    final case class GroupFoot[A](
        render: (GroupPath, List[A]) => UI,
        path: GroupPath,
        rows: List[A],
        colCount: Int
    ) extends RowSpec[A]

    /** One of the two rows that hold the height of the window's undrawn rows. `marks` carries the
      * first drawn index and how many are drawn, which only the leading spacer states.
      */
    final case class Spacer[A](px: Int, marks: Maybe[(Int, Int)] = Absent) extends RowSpec[A]

    /** A row the rows have not reached yet: one skeleton per column. The column counts and the frozen
      * plan ride along because a slot has to hold the same grid as a real row. A window's slot is as
      * tall as its rows are declared to be; a page's, with no height declared, is as tall as a line of
      * text makes a row.
      */
    final case class Slot[A](
        index: Int,
        height: Maybe[Int],
        lead: Int,
        cells: Int,
        trail: Int,
        frozen: FrozenPlan
    ) extends RowSpec[A]

    /** A row standing in for a row still on their way when the caller set the loading slot: the `at`-th
      * of a run of `run` such rows, `index` naming it among the body's. Under `loadingContent` each is
      * one line of text tall (or `height`) and the first carries the content over the whole run: rows
      * rather than one tall cell, so the height is the rows' own in every theme and never a sum of
      * padding the stylesheet would have to know. Under `loadingRow` each holds the caller's row.
      */
    final case class Loading[A](index: Int, at: Int, run: Int, colCount: Int, height: Maybe[Int]) extends RowSpec[A]

    final case class Empty[A](colCount: Int) extends RowSpec[A]

end RowSpec

/** The row before and after a committed cell edit, plus the column that wrote it. */
final case class CellChange[A](column: List[String], before: A, after: A)

/** The row before and after a committed row edit. */
final case class RowChange[A](before: A, after: A)

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

/** DataTable: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  *   - `globalFilter(ref)`: case-insensitive contains-match over the columns'
  *     text projections.
  *   - `paginate(size)(page)`: slices the (filtered, sorted) rows and renders
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
  *   - `selectionMode` + `selected(ref)`: `Single`/`Multiple` select on row
  *     click; `Checkbox` renders Prime's checkbox column, whose header carries the
  *     binary select-all over every row that survived the global filter (`Radio`
  *     follows single-select semantics without a radio column for now).
  *   - `expanded(ref)` + `rowExpansionTemplate`: an expander-button column is
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
  *   - `loading(flag)`: a spinner over `.p-datatable-mask` covers the table (`loadingMask` switches
  *     it; loading content of the caller's own leaves it off unless asked for). While it is set the body already has the shape of the page that is
  *     coming: the rows it holds, cut to the page size, and the place of the rest, so it neither
  *     says "No records found" for a page nobody has answered yet nor grows or shrinks when the
  *     page lands. The place is a skeleton per cell, what `loadingContent(...)` puts over it, or
  *     the caller's own rows from `loadingRow`.
  *   - `scrollHeight(css)`: caps and scrolls the container, pinning the row
  *     groups to its edges.
  *
  * `rowKey` supplies the stable id behind selection/expansion and the
  * `onRowClick` payload. It is REQUIRED in practice whenever any of those is
  * bound: without it rows fall back to their position in the ORIGINAL rows list,
  * which survives sorting and filtering (both computed here) but not a data
  * change: reorder the rows and every selection, expansion and click payload
  * re-associates with a different record, silently. A table that binds identity
  * without a key therefore renders a loud `.p-uic-key-error` card above itself
  * rather than shipping that failure to production data.
  */
final case class DataTable[A] private (
    rowsV: List[A] = Nil,
    rowKeyF: Maybe[A => String] = Absent,
    cols: List[ColumnTree[A]] = Nil,
    sortSlot: Maybe[ReactiveValue[List[SortKey]]] = Absent,
    onSortF: Maybe[List[SortKey] => UI.Action] = Absent,
    filterSlot: Maybe[ReactiveValue[String]] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    // The page size as `paginate` was given it: a constant, a signal, or a ref the rows-per-page dropdown writes. While a
    // signal is bound, `pageSizeV` holds its value as of the render, written there by `renderWith`.
    pageSizeSlot: Maybe[ReactiveValue[Int]] = Absent,
    pageSlot: Maybe[ReactiveValue[Int]] = Absent,
    onPageF: Maybe[Int => UI.Action] = Absent,
    onPageSizeF: Maybe[Int => UI.Action] = Absent,
    paginatorF: Maybe[Paginator => Paginator] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedTyped: Maybe[ReactiveValue[Set[A]]] = Absent,
    metaKeyFlag: Boolean = false,
    onSelectionF: Maybe[Set[A] => UI.Action] = Absent,
    selectableF: Maybe[A => Boolean] = Absent,
    cellsTyped: Maybe[ReactiveValue[Set[CellPath[A]]]] = Absent,
    ownCellsFlag: Boolean = false,
    onCellsF: Maybe[Set[CellPath[A]] => UI.Action] = Absent,
    contextTyped: Maybe[ReactiveValue[Maybe[A]]] = Absent,
    onRowContextF: Maybe[RowContext[A] => UI.Action] = Absent,
    csvSeparatorV: String = ",",
    rowClassF: Maybe[A => Seq[String]] = Absent,
    expandedTyped: Maybe[ReactiveValue[Set[A]]] = Absent,
    onExpandedF: Maybe[Set[A] => UI.Action] = Absent,
    expansionF: Maybe[A => UI] = Absent,
    groupsV: List[RowGroup[A]] = Nil,
    expandedGroupsSlot: Maybe[ReactiveValue[Set[GroupPath]]] = Absent,
    onExpandedGroupsF: Maybe[Set[GroupPath] => UI.Action] = Absent,
    ownGroupsFlag: Boolean = false,
    removableSortFlag: Boolean = true,
    stripedFlag: Boolean = false,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyContentV: Maybe[EmptyContent] = Absent,
    loadingContentV: Maybe[LoadingContent] = Absent,
    onRowClickF: Maybe[A => UI.Action] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    headerV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    loadingMaskV: Maybe[BoolValue] = Absent,
    scrollHeightV: Maybe[String] = Absent,
    flexScrollFlag: Boolean = false,
    rowHeightV: Maybe[Int] = Absent,
    scrollOverscanV: Int = 3,
    sourceV: Maybe[RowSource[?, A]] = Absent,
    frozenRowsV: Maybe[ReactiveValue[Seq[A]]] = Absent,
    lazyTotalV: Maybe[ReactiveValue[Total]] = Absent,
    rowsSigV: Maybe[Signal[Seq[A]]] = Absent,
    editingRowsTyped: Maybe[SignalRef[Set[A]]] = Absent,
    editingCellTyped: Maybe[SignalRef[Maybe[CellPath[A]]]] = Absent,
    rowsRefV: Maybe[SignalRef[Seq[A]]] = Absent,
    cellNavV: Maybe[Boolean] = Absent,
    onCellChangedF: Maybe[CellChange[A] => UI.Action] = Absent,
    onRowChangedF: Maybe[RowChange[A] => UI.Action] = Absent,
    translatorV: ErrorTranslator = ErrorTranslator.default,
    hiddenPaths: List[(List[String], Column[A, FlatOnly])] = Nil,
    columnFiltersSlot: Maybe[ReactiveValue[Map[List[String], ColumnFilter]]] = Absent,
    onColumnFiltersF: Maybe[Map[List[String], ColumnFilter] => UI.Action] = Absent,
    filterDisplayV: FilterDisplay = FilterDisplay.Row,
    columnWidthsSlot: Maybe[ReactiveValue[Map[List[String], Double]]] = Absent,
    onColumnWidthsF: Maybe[Map[List[String], Double] => UI.Action] = Absent,
    ownWidthsFlag: Boolean = false,
    resizeModeV: ColumnResizeMode = ColumnResizeMode.Fit,
    columnOrderSlot: Maybe[ReactiveValue[List[List[String]]]] = Absent,
    onColumnOrderF: Maybe[List[List[String]] => UI.Action] = Absent,
    ownOrderFlag: Boolean = false,
    reorderRowsFlag: Boolean = false,
    onRowReorderF: Maybe[RowMove[A] => UI.Action] = Absent,
    orderedPaths: List[List[String]] = Nil,
    idV: Maybe[String] = Absent,
    anchorSlot: Maybe[ReactiveValue[Maybe[A]]] = Absent,
    onAnchorF: Maybe[Maybe[A] => UI.Action] = Absent
) extends Node, HasElementId, HasEmptyContent, HasLoadingContent, HasAccessibleNameRef:
    type Self = DataTable[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): DataTable[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends data rows. */
    def rows(rs: Seq[A]): DataTable[A] = copy(rowsV = rowsV ++ rs.toList)

    /** Stable row identity: the selection/expansion key and the `onRowClick`
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

    /** The ordered sort spec: [[SortKey]] entries, the first sorting one being the primary
      * key.
      *
      * A `SignalRef` binds two way: a header press writes the new spec, then
      * [[onSortChange]] hears it. A plain `Signal` or a constant binds one way: the table
      * sorts by it and hands a press to [[onSortChange]] without writing, which is how a
      * sort held in a route or a parent is changed there. A one-way sort with no
      * [[onSortChange]] cannot move and is reported in a card.
      *
      * A plain header click sorts by that column alone. A click holding Ctrl (or Cmd)
      * adds the column to the spec, or advances the one already there WITHOUT moving it:
      * ascending, descending, and (while [[removableSort]] is on) unsorted while keeping
      * its slot. That last state is what makes an accidental click cheap, since one more
      * click on the same header puts the column back at the rank it had.
      */
    def sort(v: List[SortKey] | Signal[List[SortKey]]): DataTable[A] = copy(sortSlot = Present(ReactiveValue[List[SortKey]](v)))

    /** Fired with the spec a header press (a click, Enter or Space, with Ctrl or Cmd for several columns) asks for, after
      * a two-way [[sort]] is written.
      */
    def onSortChange(f: List[SortKey] => UI.Action)(using Frame): DataTable[A] = copy(onSortF = Present(Callback.of(f)))

    /** Whether a header click cycle reaches `Unsorted` (default) or stops at ascending
      * and descending, which is Prime's default.
      */
    def removableSort(v: Boolean): DataTable[A] = copy(removableSortFlag = v)

    /** The global filter query: case-insensitive contains-match over the columns' text
      * projections. The table never writes it, since the field a reader types it into is
      * the caller's own, so a constant, a signal and a ref all just filter.
      */
    def globalFilter(v: String | Signal[String]): DataTable[A] = copy(filterSlot = Present(ReactiveValue[String](v)))

    /** The per-column filters, keyed by the column path the sort spec already names a
      * column by, which gives the table its filter row: one input per [[Column.filterBy]]
      * column, with the mode menu beside it.
      *
      * A `SignalRef` binds two way: what the reader types, applies, clears or picks as a
      * mode is written, then [[onColumnFiltersChange]] hears the new map. A plain `Signal`
      * or a constant binds one way: the table filters by it and only reports, which is
      * how filters held in a route or a parent are changed there. While the reader types
      * into a one-way filter row the field keeps their text until they leave it, so an
      * owner that answers late does not take a keystroke back. A one-way binding with no
      * [[onColumnFiltersChange]] is reported in a card.
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

    def columnFilters(v: Map[List[String], ColumnFilter] | Signal[Map[List[String], ColumnFilter]]): DataTable[A] =
        copy(columnFiltersSlot = Present(ReactiveValue[Map[List[String], ColumnFilter]](v)))

    /** Fired with the filters the reader asked for (a keystroke in the filter row, an apply, a clear or a mode pick in
      * the filter menu), after two-way [[columnFilters]] are written.
      */
    def onColumnFiltersChange(f: Map[List[String], ColumnFilter] => UI.Action)(using Frame): DataTable[A] =
        copy(onColumnFiltersF = Present(Callback.of(f)))

    /** The column widths, keyed by the column path the sort spec already names a column
      * by, which gives every boundary between two resizable columns a drag handle.
      *
      * A seeded map opens the table on the widths it names. A `SignalRef` binds two way:
      * every move of a drag writes the new widths into it, then [[onColumnWidthsChange]]
      * hears them, so the widths outlive the table that rendered them. A plain `Signal` or
      * a constant binds one way: the table lays out by it and reports every move, and the
      * columns follow the drag as fast as the owner writes back. A one-way binding with no
      * [[onColumnWidthsChange]] is reported in a card.
      *
      * A drag moves ONE boundary: the two columns it sits between trade width and the
      * total stays where it was, so the table never grows past the space it was given and
      * no column the reader is not touching moves. The last column has no boundary to its
      * right, and neither does one whose neighbour says [[Column.resizable]] is false.
      * [[columnResizeMode]] is what changes that.
      */
    def columnWidths(v: Map[List[String], Double] | Signal[Map[List[String], Double]]): DataTable[A] =
        copy(columnWidthsSlot = Present(ReactiveValue[Map[List[String], Double]](v)), ownWidthsFlag = false)

    /** Turns the boundary drag on with the widths kept by the table itself, for as long as it is shown: the
      * [[columnWidths]] drag with no ref to hand over, for a page that does not read the widths. [[onColumnWidthsChange]]
      * still hears every move.
      */
    def columnWidths(): DataTable[A] = copy(columnWidthsSlot = Absent, ownWidthsFlag = true)

    /** Fired with the widths after every move of a boundary drag, after two-way [[columnWidths]] are written. */
    def onColumnWidthsChange(f: Map[List[String], Double] => UI.Action)(using Frame): DataTable[A] =
        copy(onColumnWidthsF = Present(Callback.of(f)))

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
    def columnOrder(v: List[List[String]] | Signal[List[List[String]]]): DataTable[A] =
        copy(columnOrderSlot = Present(ReactiveValue[List[List[String]]](v)), ownOrderFlag = false)

    /** Turns the header drag on with the order kept by the table itself, for as long as it is shown: the [[columnOrder]]
      * drag with no ref to hand over, for a page that does not read the order. [[onColumnOrderChange]] still hears every
      * drop.
      */
    def columnOrder(): DataTable[A] = copy(columnOrderSlot = Absent, ownOrderFlag = true)

    /** Fired with the whole order a header drop asks for, after a two-way [[columnOrder]] is written. With a signal or a
      * constant bound the table lays out by that and only reports the drop; a one-way order with no handler here is
      * reported in a card.
      */
    def onColumnOrderChange(f: List[List[String]] => UI.Action)(using Frame): DataTable[A] = copy(onColumnOrderF = Present(Callback.of(f)))

    /** Slices the rows into pages of `size` and renders the embedded paginator; `page` is the 0-based page index
      * (clamped at render).
      *
      * Each takes a constant, a signal or a ref. A `SignalRef` binds two way: a page click writes the page, and the
      * paginator's rows-per-page dropdown ([[Paginator.rowsPerPageOptions]]) writes the size. A plain `Signal` binds one
      * way: the table pages by it and hands the reader's pick to [[onPageChange]] or [[onPageSizeChange]] without
      * writing, which is how a page held in a route or a parent is moved. A constant size with no [[onPageSizeChange]]
      * is one the reader cannot change, so the dropdown, if the paginator shows one, is drawn disabled.
      */
    def paginate(size: Int | Signal[Int])(page: Int | Signal[Int]): DataTable[A] =
        val pageV = Present(ReactiveValue[Int](page))
        ReactiveValue[Int](size) match
            case ReactiveValue.Const(n) =>
                val clamped = math.max(1, n)
                copy(pageSizeV = Present(clamped), pageSizeSlot = Present(ReactiveValue.Const(clamped)), pageSlot = pageV)
            case bound =>
                // The value is read with the rest of the table's state; one stands in until then, so that paging is on.
                copy(pageSizeV = Present(1), pageSizeSlot = Present(bound), pageSlot = pageV)
        end match
    end paginate

    /** Fired with the page the reader asked for (a page link, a navigation button, a jump control, or the page a
      * rows-per-page pick moves to), after a two-way [[paginate]] page is written. The write-side counterpart of a
      * one-way page.
      */
    def onPageChange(f: Int => UI.Action)(using Frame): DataTable[A] = copy(onPageF = Present(Callback.of(f)))

    /** Fired with the rows per page the reader picked in the paginator's dropdown, after a two-way [[paginate]] size is
      * written, and before the page the pick moves to is reported.
      */
    def onPageSizeChange(f: Int => UI.Action)(using Frame): DataTable[A] = copy(onPageSizeF = Present(Callback.of(f)))

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
      * how many records there are, how many fit a page (bound through [[paginate]], which a
      * rows-per-page dropdown writes), which page is showing, and the ref the page is
      * written to. Those are the table's own state, and a paginator
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
      * any of those overrides it: bind the source first, then override. The page size is
      * bound as the source was given it, so a source opened over a `SignalRef` size lets the
      * paginator's rows-per-page dropdown change it, and one opened over a constant does not.
      */
    def source(src: RowSource[?, A]): DataTable[A] =
        copy(
            sourceV = Present(src),
            rowsSigV = Present(src.rows),
            lazyTotalV = Present(ReactiveValue.Dyn(src.total)),
            loadingV = Present(BoolValue.Dyn(src.loading))
        ).paginate(src.pageSizeBinding)(src.page)

    /** Selection semantics: `Single`/`Multiple` select on row click, `Checkbox`
      * via Prime's checkbox column; `None` (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): DataTable[A] = copy(selectionModeV = v)

    /** The selected rows, matched by [[rowKey]].
      *
      * A `SignalRef` binds two way: the table writes the reader's clicks back into it. A plain
      * `Signal` binds ONE way: the table paints what the signal says and never writes to it, which
      * is what a selection whose source of truth lives elsewhere needs: a URL, a parent, a record
      * in a normalized cache the rows are read from. A constant set is a selection that cannot
      * change at all.
      *
      * Reading needs no write access (`Signal.render` serves every case), so only four paths care
      * which form this is: a row click, the checkbox column, its select-all, and [[restore]]. Under
      * a one-way binding each of those has nowhere to write, so each does nothing, and
      * [[onRowClick]] still fires on every row click, which is how a caller owning the state
      * elsewhere closes the loop: the click writes THERE, and the new value arrives back through
      * this signal. `Checkbox` mode has no such outlet, and a one-way binding without an
      * `onRowClick` cannot move at all; both are reported as cards rather than left silent.
      */
    def selection(v: Set[A] | Signal[Set[A]]): DataTable[A] =
        copy(selectedTyped = Present(ReactiveValue(v)))

    /** Whether picking rows takes a modifier key (Prime's `metaKeySelection`).
      *
      * Off (the default, and Prime's since 3.45.0) every click toggles its row, which is the
      * behaviour a checkbox list has without the checkboxes. On, a row click reads like a file
      * manager and a spreadsheet:
      *
      *   - a plain click REPLACES the selection with the row it landed on, including when that
      *     row was already selected, which collapses a selection to one rather than clearing it
      *   - Ctrl or Cmd + click toggles that row into or out of the selection
      *   - Shift + click selects the RANGE from the last row picked without shift up to this one,
      *     replacing the selection; a shift-click does not move that anchor, so widening and
      *     narrowing the same range is one gesture repeated
      *
      * '''Ctrl OR Cmd''', never one of the two: `event.metaKey || event.ctrlKey` is what Prime
      * tests and what every desktop convention expects, and pinning it to `metaKey` alone locks
      * out every reader who is not on a Mac.
      *
      * Two consequences worth knowing before turning it on. The range needs an ANCHOR: unless the
      * caller binds one with [[selectionAnchor]], the table mints it in a mount and renders as a
      * placeholder under a pure render, which is why it is opt-in rather than the shape every
      * selectable table pays for. And [[selectableWhen]] still applies: a range steps over the
      * rows it rejects rather than dragging them in.
      *
      * Deviation from Prime, deliberate: there, shift-ranging works in `Multiple` mode whether or
      * not `metaKeySelection` is set. Here the two arrive together, so a table that never asked
      * for modifier semantics keeps a render with no state in it.
      */
    def metaKeySelection(v: Boolean): DataTable[A] = copy(metaKeyFlag = v)

    /** Binds the row a shift-click measures its range from, so the caller owns it.
      *
      * The anchor is the one thing a selection cannot be asked for: a `Set` has no order, so
      * "the last row picked without shift" is not derivable from what is selected. Unbound, the
      * table mints it inside its own mount, and a mount's state lives exactly as long as the
      * region enclosing it, so a page that re-renders around the table re-allocates the anchor
      * and a shift-click reads one no click ever wrote. That is not a fault in the mount: keyed
      * continuity spans re-renders of the immediately enclosing region and ends with it, by
      * design. It is the same reason [[sort]], the page (bound through [[paginator]]), the column filters and the column
      * widths are all bindable: state that has to outlive the table's own render belongs to
      * the caller.
      *
      * Bind it and two things follow: the anchor survives whatever the page does, and ranging
      * no longer forces a mount, so a table that owns nothing else stays a pure render.
      *
      * A shift-click READS this and leaves it where it is; a click without shift moves it: a
      * `SignalRef` is written, then [[onSelectionAnchorChange]] hears the row; a plain `Signal`
      * is ranged from and the move only reported; a constant nobody hears is fixed, so every
      * range starts at that row. A signal nothing hears is reported in a card. Never read for
      * display, so nothing re-renders when it changes.
      */
    def selectionAnchor(v: Maybe[A] | Signal[Maybe[A]]): DataTable[A] = copy(anchorSlot = Present(ReactiveValue(v)))

    /** Fired with the new anchor when a click without shift moves it, after a two-way [[selectionAnchor]] is written. */
    def onSelectionAnchorChange(f: Maybe[A] => UI.Action)(using Frame): DataTable[A] = copy(onAnchorF = Present(Callback.of(f)))

    /** The selection's write-back ref, where the caller bound one that can be written.
      *
      * `ReactiveVariable` is matched before `Dyn` on purpose: it IS-A `Dyn`, so the wrong order
      * silently swallows the two-way case (`kyo-ui/components/CLAUDE.md`).
      */
    private def selectedRef: Maybe[SignalRef[Set[A]]] = selectedTyped match
        case Present(ReactiveVariable(ref)) => Present(ref)
        case _                              => Absent

    /** Selects CELLS rather than rows (Prime's `cellSelection`), bound as a set of
      * [[CellPath]], which pairs a row with a column path.
      *
      * Binding it is the switch, and [[selectionMode]] says what a click means:
      * `Single` replaces the set, `Multiple` toggles the cell in it. A `SignalRef` is written,
      * then [[onSelectedCellsChange]] hears the set; a plain `Signal` is followed and a click
      * only reported; a constant nobody hears is fixed, its cells shown picked and taking no
      * click. A signal nothing hears is reported in a card. A cell has no
      * identity until a row key and a column path are put together, which is why this is
      * a second binding and not a mode over the row one; binding both is a card, since
      * one click cannot mean two things.
      *
      * [[selectableWhen]] covers it too: a cell of a rejected row cannot be picked, which
      * is what Prime's Cell Selection disabled section does with `isDataSelectable`. Cell
      * EDITING claims the same click, so the two are a card as well, and editing keeps
      * the click.
      */
    def selectedCells(v: Set[CellPath[A]] | Signal[Set[CellPath[A]]]): DataTable[A] =
        copy(cellsTyped = Present(ReactiveValue(v)), ownCellsFlag = false)

    /** Turns cell selection on over picked cells the table keeps itself, starting with none: [[selectedCells]] with no
      * ref to hand over, for a page that does not read them. [[onSelectedCellsChange]] still hears every pick.
      */
    def selectedCells(): DataTable[A] = copy(cellsTyped = Absent, ownCellsFlag = true)

    /** Fired with the picked cells after a click changes them, after a two-way [[selectedCells]] is written. */
    def onSelectedCellsChange(f: Set[CellPath[A]] => UI.Action)(using Frame): DataTable[A] =
        copy(onCellsF = Present(Callback.of(f)))

    /** Restricts selection to the rows `p` accepts (Prime's `isDataSelectable`).
      *
      * A rejected row still renders, still takes a row click through [[onRowClick]] and
      * still expands; what it loses is every path into the selection set: the click does
      * not select it, its checkbox is disabled, and select-all passes over it. The
      * predicate is read wherever the selection is written, so a row the caller starts
      * rejecting mid-session cannot stay selected by having been selectable earlier.
      */
    def selectableWhen(p: A => Boolean): DataTable[A] = copy(selectableF = Present(p))

    /** Binds the row a context menu was opened on, as the row itself (Prime's
      * `contextMenuSelection`).
      *
      * The bound row is marked with Prime's `.p-datatable-contextmenu-row-selected`, which is
      * a second, separate mark from the selection: the reader is acting ON one row without
      * changing what is selected. A right-click on a row writes a `SignalRef` and then
      * reports through [[onContextMenuRowChange]]; a plain `Signal` is followed and the
      * right-click only reported; a constant nobody hears is fixed, so the rows take no
      * right-click of their own. Wherever a right-click is heard, the browser's own menu is
      * suppressed over the rows, which is what a context menu of one's own has to do. A
      * signal nothing hears is reported in a card.
      *
      * The menu itself is [[kyo.uic.ContextMenu]], which wraps the table: this says which
      * row it was opened on, and the binding is the caller's to clear when the menu closes,
      * since the table cannot see a panel it does not render.
      */
    def contextMenuRow(v: Maybe[A] | Signal[Maybe[A]]): DataTable[A] = copy(contextTyped = Present(ReactiveValue(v)))

    /** Runs on a right-click over a row with what that click was aimed at (the row under the
      * pointer and the rows selected at that moment, as a [[RowContext]]), after a two-way
      * [[contextMenuRow]] is written.
      *
      * That is what a menu needs to decide what it should do, and why it is a pair: acting on the
      * selection when the reader right-clicked inside it and on the one row when they did not is
      * the rule almost every table wants, and neither half alone can express it. The table takes
      * no position on the rule itself and changes nothing about the selection.
      *
      * The menu is [[kyo.uic.ContextMenu]], which wraps the table and opens where the click was;
      * a menu item that runs later reads what this handler put in a ref of yours.
      */
    def onContextMenuRowChange(f: RowContext[A] => UI.Action)(using Frame): DataTable[A] = copy(onRowContextF = Present(Callback.of(f)))

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
            sort  <- sortReading.signal.current
            query <- queryReading.signal.current
            specs <- filtersReading.signal.current
            order <- orderReading.signal.current
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
            sort   <- sortReading.signal.current
            query  <- queryReading.signal.current
            specs  <- filtersReading.signal.current
            page   <- pageReading.signal.current
            sel    <- currentValue(selectedBinding, Set.empty[String])
            exp    <- currentValue(expandedBinding, Set.empty[String])
            groups <- groupsReading.signal.current
            widths <- widthsReading.signal.current
            order  <- orderReading.signal.current
        yield TableState(sort, query, specs, page, sel, exp, groups, widths, order)

    /** Puts a reader back where they were: writes each field of `s` into the ref it came
      * from, and skips the ones that are not bound, since they have nowhere to go.
      *
      * The selection and the expansion are stored as [[rowKey]]s, which is what keeps a
      * `TableState` something a URL or a profile row can hold. Restoring turns each key back
      * into its row: the rows the table holds now first, then `byKey` for the ones it does not
      * (a lazily loaded table holds only the window it fetched). A key neither of them can
      * answer is left out.
      *
      * A one-way [[selection]] is skipped for the same reason a slot nobody bound is: the table does
      * not own that set, so restoring it is the caller's to do wherever they do own it.
      *
      * Nothing is validated here, because nothing has to be: a restored spec naming a
      * column this table no longer has, a width for one it never had, or a page past the
      * end are each already something the table reports or clamps at render.
      */
    def restore(s: TableState, byKey: String => Maybe[A] = (_: String) => Absent)(using Frame): Unit < Async =
        for
            all <- currentRows
            rows = (keys: Set[String]) =>
                keys.flatMap(k => Maybe.fromOption(all.find(r => keyOf(r) == k)).orElse(byKey(k)).toList)
            _ <- writeIf(sortSlot.flatMap(_.writable), s.sort)
            _ <- writeIf(filterSlot.flatMap(_.writable), s.globalFilter)
            _ <- writeIf(columnFiltersSlot.flatMap(_.writable), s.columnFilters)
            _ <- writeIf(pageSlot.flatMap(_.writable), s.page)
            _ <- writeIf(selectedRef, rows(s.selected))
            _ <- writeIf(expandedTyped.flatMap(_.writable), rows(s.expanded))
            _ <- writeIf(expandedGroupsSlot.flatMap(_.writable), s.expandedGroups)
            _ <- writeIf(columnWidthsSlot.flatMap(_.writable), s.columnWidths)
            _ <- writeIf(columnOrderSlot.flatMap(_.writable), s.columnOrder)
        yield ()

    private def writeIf[T](ref: Maybe[SignalRef[T]], v: T)(using Frame): Unit < Async =
        ref match
            case Present(r) => r.set(v)
            case Absent     => ()

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
                case Absent       =>
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

    /** The expanded rows, matched by [[rowKey]]; pair with [[rowExpansionTemplate]].
      *
      * A `SignalRef` binds two way: a row toggle writes it, then [[onExpandedChange]] hears the new set. A plain
      * `Signal` or a constant binds one way: the table opens those rows and only reports a toggle, which is how
      * expansion held in a route or a parent is changed there. A one-way binding with no [[onExpandedChange]] is
      * reported in a card.
      */
    def expanded(v: Set[A] | Signal[Set[A]]): DataTable[A] = copy(expandedTyped = Present(ReactiveValue[Set[A]](v)))

    /** Fired with the rows open after a row toggle, after a two-way [[expanded]] is written. */
    def onExpandedChange(f: Set[A] => UI.Action)(using Frame): DataTable[A] = copy(onExpandedF = Present(Callback.of(f)))

    /** Content of the full-colspan expansion row below an expanded data row; setting
      * it auto-adds the expander-button column.
      */
    def rowExpansionTemplate(f: A => UI): DataTable[A] = copy(expansionF = Present(f))

    /** Binds row editing two-way to `ref` (the set of rows being edited, matched by [[rowKey]]): every column carrying
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
    def editingRows(ref: SignalRef[Set[A]]): DataTable[A] = copy(editingRowsTyped = Present(ref))

    /** Binds cell editing two-way to `ref`: at most one cell shows its column's editor,
      * and clicking a cell of an editable column moves the editor there. Enter commits,
      * Escape discards, and both clear the ref.
      *
      * A [[CellPath]], since a cell is a row crossed with a column.
      */
    def editingCell(ref: SignalRef[Maybe[CellPath[A]]]): DataTable[A] = copy(editingCellTyped = Present(ref))

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
      * state, a paginated connection, a `combineLatestAll` over per-row signals: none of
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
    def onCellValueChanged(f: CellChange[A] => UI.Action)(using Frame): DataTable[A] = copy(onCellChangedF = Present(Callback.of(f)))

    /** Runs after a committed row edit, once, with the row before and after every editable
      * column's write. The per-cell [[onCellValueChanged]] fires for each column that
      * actually changed, before this.
      */
    def onRowValueChanged(f: RowChange[A] => UI.Action)(using Frame): DataTable[A] = copy(onRowChangedF = Present(Callback.of(f)))

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
    def onRowReorder(f: RowMove[A] => UI.Action)(using Frame): DataTable[A] = copy(onRowReorderF = Present(Callback.of(f)))

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

    /** Makes the groups collapsible over the set of EXPANDED paths, so an empty set
      * starts with every group collapsed. A `SignalRef` binds two way: a header toggle
      * writes it, then [[onExpandedGroupsChange]] hears the new set. A plain `Signal` or a
      * constant binds one way: the table opens those groups and only reports a toggle. A
      * one-way binding with no [[onExpandedGroupsChange]] is reported in a card. Each level's header row grows a toggle
      * button; a collapsed group hides everything nested inside it, its own summary row
      * included, and keeps its slice of the page, since [[paginate]] slices rows before
      * they are grouped.
      *
      * The currency is [[GroupPath]] rather than a key, because a key alone does not
      * identify a group below the outermost level: the same brand occurs under every
      * category that sells one, and collapsing it in one place must not collapse it in the
      * others. A level with [[RowGroup.showHeader]] off has no toggle and stays open.
      */
    def expandedGroups(v: Set[GroupPath] | Signal[Set[GroupPath]]): DataTable[A] =
        copy(expandedGroupsSlot = Present(ReactiveValue[Set[GroupPath]](v)), ownGroupsFlag = false)

    /** Makes the groups collapsible with the open ones kept by the table itself, for as long as it is shown, starting
      * with every group collapsed: the [[expandedGroups]] toggles with no ref to hand over, for a page that does not read
      * them. [[onExpandedGroupsChange]] still hears every toggle.
      */
    def expandedGroups(): DataTable[A] = copy(expandedGroupsSlot = Absent, ownGroupsFlag = true)

    /** Fired with the groups open after a group header toggle, after two-way [[expandedGroups]] are written. */
    def onExpandedGroupsChange(f: Set[GroupPath] => UI.Action)(using Frame): DataTable[A] =
        copy(onExpandedGroupsF = Present(Callback.of(f)))

    /** Zebra striping (`.p-datatable-striped` + `.p-row-odd` rows). */
    def stripedRows(v: Boolean): DataTable[A] = copy(stripedFlag = v)

    /** Cell borders on every edge (`.p-datatable-gridlines`). */
    def showGridlines(v: Boolean): DataTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-datatable-sm` / default / `.p-datatable-lg` cell paddings. */
    def size(v: Size): DataTable[A] = copy(sizeV = v)

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): DataTable[A] = copy(emptyContentV = v)

    private[uic] def withLoadingContent(v: Maybe[LoadingContent]): DataTable[A] = copy(loadingContentV = v)

    /** Fired with the row after any selection write from a row click. */
    def onRowClick(f: A => UI.Action)(using Frame): DataTable[A] = copy(onRowClickF = Present(Callback.of(f)))

    /** Runs with the selection the table WORKED OUT, whenever a click changes it.
      *
      * The write-side counterpart of a one-way [[selection]], and the only outlet that can carry
      * [[metaKeySelection]]: the table resolves the mode, the modifier and the range into a new
      * set, and hands that set over instead of writing it, so a caller whose selection lives
      * somewhere else (a URL, a parent, a record in a normalized cache) applies it there.
      * [[onRowClick]] cannot do this job once modifiers are on, because a row says which one was
      * clicked and not what the selection became.
      *
      * Every path that changes a selection goes through it: a row click, a row checkbox, and the
      * select-all header. With a two-way binding the ref is written first and this fires after,
      * so both forms see the same value and a caller may bind either or both.
      */
    def onSelectionChange(f: Set[A] => UI.Action)(using Frame): DataTable[A] = copy(onSelectionF = Present(Callback.of(f)))

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

    /** Whether `loading` lays Prime's mask over the table: the dimming `.p-datatable-mask` with its
      * spinner, which also takes every click while it is up. Unset, it is on over the skeletons, as in
      * Prime, and off once [[loadingContent]] or [[loadingRow]] is set, since content of your own
      * already says how the wait looks and the spinner would sit on top of it. Off, the table still
      * holds the place of the rows that are coming and still says `aria-busy`, so the wait stays
      * visible to the reader and to assistive technology, only nothing is dimmed or blocked: for a
      * table whose readers keep working while it loads, or one whose loading content they use. A
      * `Signal` switches the mask alone, as a region of its own beside the table.
      */
    def loadingMask(v: Boolean | Signal[Boolean]): DataTable[A] = copy(loadingMaskV = Present(ReactiveValue(v)))

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

    // ---- the caller's rows, as the keys the table compares them by ----
    //
    // Every binding the caller hands over holds rows (or cells of rows); everything inside the table
    // works on their keys. These are the one crossing: a binding is read as keys, and keys the table
    // writes go back as the rows that carry them, the rows a binding already holds kept as the caller
    // wrote them. A key no current row carries cannot become a row, so a write drops it.

    private def keysOf(rows: Set[A]): Set[String] = rows.map(keyOf)

    private def cellKey(c: CellPath[A]): CellKey = CellKey(keyOf(c.row), c.column)

    /** The rows `keys` name: those `held` already holds, then those of `all` not held yet. */
    private def rowsFor(keys: Set[String], held: Set[A], all: Seq[A]): Set[A] =
        val kept  = held.filter(a => keys.contains(keyOf(a)))
        val known = keysOf(kept)
        kept ++ all.filter(a => keys.contains(keyOf(a)) && !known.contains(keyOf(a)))
    end rowsFor

    /** The cells `keys` name, by the same rule. */
    private def cellsFor(keys: Set[CellKey], held: Set[CellPath[A]], all: Seq[A]): Set[CellPath[A]] =
        val kept  = held.filter(c => keys.contains(cellKey(c)))
        val known = kept.map(cellKey)
        kept ++ keys.filterNot(known.contains).flatMap(k => all.find(r => keyOf(r) == k.row).map(CellPath(_, k.column)))
    end cellsFor

    /** The row the key names, keeping `held` where it is that row already. */
    private def rowFor(key: Maybe[String], held: Maybe[A], all: Seq[A]): Maybe[A] =
        key match
            case Absent                                   => Absent
            case Present(k) if held.exists(keyOf(_) == k) => held
            case Present(k)                               => Maybe.fromOption(all.find(r => keyOf(r) == k))

    /** The selection as keys, whichever way it was bound. */
    private def selectedBinding(using Frame): Maybe[ReactiveValue[Set[String]]] =
        selectedTyped.map {
            case ReactiveValue.Const(rows) => ReactiveValue.Const(keysOf(rows))
            case ReactiveValue.Dyn(sig)    => ReactiveValue.Dyn(sig.map(keysOf))
        }

    /** The context row as a key, whichever way it was bound. */
    private def contextBinding(using Frame): Maybe[ReactiveValue[Maybe[String]]] =
        contextTyped.map {
            case ReactiveValue.Const(row) => ReactiveValue.Const(row.map(keyOf))
            case ReactiveValue.Dyn(sig)   => ReactiveValue.Dyn(sig.map(_.map(keyOf)))
        }

    /** The picked cells as keys, whichever way they were bound. */
    private def cellsBinding(using Frame): Maybe[ReactiveValue[Set[CellKey]]] =
        cellsTyped.map {
            case ReactiveValue.Const(cells) => ReactiveValue.Const(cells.map(cellKey))
            case ReactiveValue.Dyn(sig)     => ReactiveValue.Dyn(sig.map(_.map(cellKey)))
        }

    /** The expansion as keys, whichever way it was bound. */
    private def expandedBinding(using Frame): Maybe[ReactiveValue[Set[String]]] =
        expandedTyped.map {
            case ReactiveValue.Const(rows) => ReactiveValue.Const(keysOf(rows))
            case ReactiveValue.Dyn(sig)    => ReactiveValue.Dyn(sig.map(keysOf))
        }

    /** The open groups as the table reads them: the bound set, else none. */
    private def groupsReading(using Frame): Reading[Set[GroupPath]] =
        expandedGroupsSlot.fold(Reading.Fixed(Set.empty[GroupPath]))(Reading.of(_))
    private def editingRowsRef(using Frame): Maybe[Signal[Set[String]]]    = editingRowsTyped.map(_.map(keysOf))
    private def editingCellRef(using Frame): Maybe[Signal[Maybe[CellKey]]] = editingCellTyped.map(_.map(_.map(cellKey)))

    /** Applies `f` to a bound row set's keys and writes back the rows they name. */
    private def updateRows(ref: SignalRef[Set[A]])(f: Set[String] => Set[String])(using Frame): UI.Action =
        currentRows.map(all => ref.getAndUpdate(prev => rowsFor(f(keysOf(prev)), prev, all)).unit)

    private def rowClickSelects: Boolean =
        (selectionModeV == SelectionMode.Single || selectionModeV == SelectionMode.Multiple ||
            selectionModeV == SelectionMode.Radio) && !selectionFixed

    /** A constant sort nobody hears is fixed: the rows show it, and no header offers to change it. */
    private def sortFixed: Boolean = ReactiveValue.fixed(sortSlot, onSortF)

    /** Constant filters nobody hears are fixed: the rows show them filtered, and no filter row offers to change them. */
    private def filtersFixed: Boolean = ReactiveValue.fixed(columnFiltersSlot, onColumnFiltersF)

    /** Constant widths nobody hears are fixed: the columns keep them, and no boundary offers a drag. */
    private def widthsFixed: Boolean = ReactiveValue.fixed(columnWidthsSlot, onColumnWidthsF)

    /** A constant order nobody hears is fixed: the columns keep it, and no header offers a drag. */
    private def orderFixed: Boolean = ReactiveValue.fixed(columnOrderSlot, onColumnOrderF)

    /** A constant expansion nobody hears is fixed: the rows show it, and no toggle opens or closes one. */
    private def expansionFixed: Boolean = ReactiveValue.fixed(expandedTyped, onExpandedF)

    /** Constant open groups nobody hears are fixed: the groups show them, and no header toggle offers to change them. */
    private def groupsFixed: Boolean = ReactiveValue.fixed(expandedGroupsSlot, onExpandedGroupsF)

    /** A constant selection nobody hears is fixed: the rows show it, and no press, box or key picks one. */
    private def selectionFixed: Boolean =
        selectionModeV != SelectionMode.None && ReactiveValue.fixed(selectedTyped, onSelectionF.orElse(onRowClickF))

    private def checkboxColumn: Boolean = selectionModeV == SelectionMode.Checkbox

    /** Whether a click picks CELLS. Cell editing claims the same click, so it wins, and
      * a mode that is about a checkbox column says nothing about a cell.
      */
    private def cellSelectOn: Boolean =
        cellsOn && editingCellTyped.isEmpty && selectedTyped.isEmpty &&
            (selectionModeV == SelectionMode.Single || selectionModeV == SelectionMode.Multiple)

    /** Whether cell selection was asked for, bound or kept by the table ([[selectedCells]]). */
    private def cellsOn: Boolean = cellsTyped.isDefined || ownCellsFlag

    /** A constant cell set nobody hears: the picked cells show and a click picks nothing. */
    private def cellsFixed: Boolean = ReactiveValue.fixed(cellsTyped, onCellsF)

    /** Whether a right-click over a row means anything here. */
    private def contextRowOn: Boolean = ReactiveValue.heard(contextTyped, onRowContextF)

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
    private def editorColumn: Boolean = editingRowsTyped.isDefined

    private def rowInteractive: Boolean = rowClickSelects || onRowClickF.isDefined

    /** Whether the rows are a window onto a larger set prepared elsewhere ([[lazyRows]]). */
    private def lazyOn: Boolean = lazyTotalV.isDefined

    /** Whether the row bindings may be resolved BELOW the body rather than around it.
      *
      * They may whenever nothing outside the row stream reads them. Three features do read them
      * from outside: a select-all header asks the whole selection whether every row is picked, a
      * frozen row group renders a second body from the same five values, and a windowed body
      * already drives its rows from the scroll. Any of those and they stay where they were, which
      * is exactly what the table did before: no gain there yet, and no change either.
      */
    private def rowInputsBelow: Boolean =
        !checkboxColumn && !windowOn && !windowedSource && !hasFooter

    /** Whether any column renders a footer, which aggregates over the arranged rows. */
    private def hasFooter: Boolean = leafCols.exists(_.hasFooter)

    /** The row bindings as one signal, or `Absent` when none of them is one.
      *
      * Folded over the bound ones only, so a table binding exactly one of them gets a plain `map`
      * and keeps the ref projection that lets the region bind without a fiber. `combineLatest`
      * rather than `zip`: `zip` waits for BOTH sides to move, and a selection changing while the
      * expansion stands still has to reach the rows.
      */
    private def rowInputsSignal(using Frame): Maybe[Signal[RowInputs[A]]] =
        // The rows compare by their own equality, whatever the element type does; the region only
        // ever asks whether the pair changed, and that question is the case class's to answer.
        given CanEqual[Seq[A], Seq[A]] = CanEqual.derived
        // What the UNBOUND bindings read as. A table that binds its selection and states its rows
        // as a plain list still has to render those rows, so the fold starts from the constants
        // rather than from the empty value and overwrites only what a signal supplies.
        val base = RowInputs[A](
            rows = rowsV,
            sel = selectedBinding.flatMap(_.const).getOrElse(Set.empty[String]),
            ctx = contextBinding.flatMap(_.const).getOrElse(Absent),
            cells = cellsBinding.flatMap(_.const).getOrElse(Set.empty[CellKey]),
            exp = expandedBinding.flatMap(_.const).getOrElse(Set.empty[String]),
            groups = expandedGroupsSlot.flatMap(_.const).getOrElse(Set.empty[GroupPath]),
            waiting = BoolValue.const(loadingV).getOrElse(false),
            total = lazyTotalV.flatMap(_.const).getOrElse(Total.Unknown(false))
        )
        def fold[T](
            acc: Maybe[Signal[RowInputs[A]]],
            sig: Maybe[Signal[T]],
            set: (RowInputs[A], T) => RowInputs[A]
        )(using CanEqual[T, T]): Maybe[Signal[RowInputs[A]]] =
            sig match
                case Absent     => acc
                case Present(s) =>
                    acc match
                        case Absent       => Present(s.map(t => set(base, t)))
                        case Present(cur) => Present(cur.combineLatest(s).map((r, t) => set(r, t)))
        val rowsSig: Maybe[Signal[Seq[A]]]     = rowsSigV.orElse(rowsRefV.map(r => r: Signal[Seq[A]]))
        val selSig: Maybe[Signal[Set[String]]] = selectedBinding.flatMap {
            case ReactiveValue.Dyn(sig) => Present(sig)
            case ReactiveValue.Const(_) => Absent
        }
        val r = fold[Seq[A]](Absent, rowsSig, (i, v) => i.copy(rows = v))
        val a = fold[Set[String]](r, selSig, (i, v) => i.copy(sel = v))
        val b = fold[Maybe[String]](a, contextBinding.flatMap(_.dyn), (i, v) => i.copy(ctx = v))
        val c = fold[Set[CellKey]](b, cellsBinding.flatMap(_.dyn), (i, v) => i.copy(cells = v))
        val d = fold[Set[String]](c, expandedBinding.flatMap(_.dyn), (i, v) => i.copy(exp = v))
        val e = fold[Set[GroupPath]](d, expandedGroupsSlot.flatMap(_.dyn), (i, v) => i.copy(groups = v))
        val f = fold[Boolean](e, loadingSignal, (i, v) => i.copy(waiting = v))
        fold[Total](f, totalSignal, (i, v) => i.copy(total = v))
    end rowInputsSignal

    /** The `lazyRows` total as a signal, when it is one. */
    private def totalSignal: Maybe[Signal[Total]] = lazyTotalV match
        case Present(ReactiveValue.Dyn(sig)) => Present(sig)
        case _                               => Absent

    /** The `loading` slot as a signal, when it is one. */
    private def loadingSignal: Maybe[Signal[Boolean]] = loadingV match
        case Present(BoolValue.Dyn(sig)) => Present(sig)
        case _                           => Absent

    /** The row bindings where they are read around the body: the rows and where they start, the selection, the context
      * row, the selected cells, the expansion, the groups and the total, as one reading. [[renderWith]] reads them below
      * the body instead wherever [[rowInputsBelow]] allows it.
      */
    private def rowInputsReading(using CanEqual[Seq[A], Seq[A]], Frame): Reading[(RowInputs[A], Total)] =
        given CanEqual[RowInputs[A], RowInputs[A]] = CanEqual.derived
        val sel                                    = selectedBinding.fold(Reading.Fixed(Set.empty[String]))(Reading.of(_))
        rowsReading
            .and(sel)
            .and(contextBinding.fold(Reading.Fixed(Absent: Maybe[String]))(Reading.of(_)))
            .and(cellsBinding.fold(Reading.Fixed(Set.empty[CellKey]))(Reading.of(_)))
            .and(expandedBinding.fold(Reading.Fixed(Set.empty[String]))(Reading.of(_)))
            .and(groupsReading)
            .and(loadingV.fold(Reading.Fixed(false))(Reading.of(_)))
            .map { case (((((((rows, offset), s), ctx), cells), exp), groups), waiting) =>
                RowInputs(rows, offset, s, ctx, cells, exp, groups, waiting)
            }
            .and(totalReading)
            .map((in, t) => (in.copy(total = t), t))
    end rowInputsReading

    /** Whether anything can be edited at all, which is what decides whether the table
      * needs state of its own.
      */
    private def editingBound: Boolean = editingCellTyped.isDefined || editingRowsTyped.isDefined

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
        editingBound || navOn || filterBound || resizeOn || reorderOn || windowOn || frozenRowsOn ||
            handleColumn || mintsAnchor

    /** Whether a shift-click can range at all.
      *
      * Tied to [[metaKeySelection]] rather than to `Multiple` alone (where Prime puts it) because
      * an anchor the table mints forces a mount, and a mount is what makes a table render as a
      * placeholder in a pure render. Every selectable table paying that for a gesture it never
      * offered is the wrong trade; asking for modifier selection is the caller saying they want it.
      */
    private def rangeOn: Boolean = metaKeyFlag && rowClickSelects && selectedTyped.isDefined

    /** Whether the table has to MINT the anchor, which is the only reason ranging forces a mount.
      * A caller who bound one has already supplied it, and supplied it somewhere that outlives
      * this table's mount (see [[selectionAnchor]]).
      */
    private def mintsAnchor: Boolean = rangeOn && anchorSlot.isEmpty

    /** Whether the filter row is rendered: the filters have to be bound somewhere, and
      * some column has to carry a pipeline for the row to hold anything.
      */
    private def filterBound: Boolean = columnFiltersSlot.isDefined && !filtersFixed && leafCols.exists(_.isFilterable)

    /** Whether the filters are edited in a second header row. */
    private def filterRowOn: Boolean = filterBound && filterDisplayV == FilterDisplay.Row

    /** Whether the filters are edited in a popover behind a funnel in each header cell. */
    private def filterMenuOn: Boolean = filterBound && filterDisplayV == FilterDisplay.Menu

    /** Whether any boundary is draggable: the widths have to be bound somewhere, and a
      * boundary needs a resizable column on both sides of it, so a single column has none
      * and neither does a table whose columns are all pinned.
      */
    private def resizeOn: Boolean =
        widthsOn && !widthsFixed && (resizeModeV match
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
        widthsOn && !widthsFixed && leafCols.lift(i).exists(_.resizableFlag) && (resizeModeAt(size) match
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

    /** The id of a column's filter popover, by the column's place among the filterable ones: a column key may hold spaces. */
    private def filterPanelId(prefix: String, path: List[String]): String =
        s"$prefix-filter${filterableLeaves.indexWhere(_._1 == path)}-panel"

    /** Whether the page asked for the boundary drag, by binding the widths or by keeping them in the table. Before the
      * mount runs, a table that keeps them has no ref yet and draws the drag all the same, so the layout does not move.
      */
    private def widthsOn: Boolean = columnWidthsSlot.isDefined || ownWidthsFlag

    /** Whether the page asked for the header drag, by binding the order or by keeping it in the table. */
    private def orderOn: Boolean = columnOrderSlot.isDefined || ownOrderFlag

    /** Whether the page asked for collapsible groups, by binding the open ones or by keeping them in the table. */
    private def groupsOn: Boolean = expandedGroupsSlot.isDefined || ownGroupsFlag

    /** Whether the table sizes its columns itself, which is what puts a `colgroup` in
      * front of the header and makes the widths in it authoritative.
      */
    private def hasWidths: Boolean = widthsOn || leafCols.exists(_.widthV.isDefined)

    /** Whether the reader may move a column, which needs the order bound to write into
      * and two columns willing to trade places.
      */
    private def reorderOn: Boolean = orderOn && !orderFixed && leafCols.count(_.reorderableFlag) > 1

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
        sorts: Boolean,
        live: HeadLive
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
        // The drag marks follow the drag through class channels, so a move to another boundary patches two cells
        // rather than drawing the header again.
        def during(f: ColumnDrag => Boolean): Reading[Boolean] =
            live.held.map {
                case Present(d) if d.moved && !d.done => f(d)
                case _                                => false
            }
        c = classWhen(c, "p-uic-dt-dragging", during(d => d.from == from && d.until == until))
        c = classWhen(c, "p-uic-dt-drop-before", during(_.target == from))
        c = classWhen(c, "p-uic-dt-drop-after", during(d => d.target != from && d.target == until && until == leafCols.length))
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
    ): UI.Action =
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
    private def dragReorder(e: PointerEvent, order: OrderState)(using Frame): UI.Action =
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
    private def endReorder(order: OrderState)(using Frame): UI.Action =
        (order.drag, columnOrderSlot) match
            case (Present(ref), Present(_)) =>
                ref.get.map {
                    case Present(d) if d.moved =>
                        val next             = reorderedPaths(d)
                        val write: UI.Action =
                            if next == effectiveOrder then () else ReactiveValue.change(columnOrderSlot, next, onColumnOrderF)
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
    private def beginRowMove(at: Int, e: PointerEvent, move: MoveState[A])(using Frame): UI.Action =
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
    private def dragRowMove(e: PointerEvent, move: MoveState[A])(using Frame): UI.Action =
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
    private def endRowMove(move: MoveState[A])(using Frame): UI.Action =
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
    private def writeRowMove(d: RowDrag, move: MoveState[A])(using Frame): UI.Action =
        if d.target == d.from || d.target == d.from + 1 then ()
        else
            // The list is read at DROP time. This one writes its result back into the caller's ref,
            // so a stale copy does not merely display wrongly: it would store a list missing
            // whatever arrived while the row was being dragged.
            bodyViewNow.map { v =>
                val next             = DataTable.moveBlock(v.moveAll.toList, d.from, d.from + 1, d.target)
                val to               = if d.target > d.from then d.target - 1 else d.target
                val store: UI.Action = rowsRefV match
                    case Present(ref) => ref.set(next)
                    case Absent       => ()
                val notify: UI.Action = onRowReorderF match
                    case Present(f) => f(RowMove(d.from, to, next))
                    case Absent     => ()
                store.andThen(notify)
            }
        end if
    end writeRowMove

    /** Takes the click a finished drag still owes and drops it, so the row the pointer
      * ended over is not also selected.
      */
    private def clearRowMove(move: MoveState[A])(using Frame): UI.Action =
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
    ): UI.Action =
        // The drag as it is when the click lands, read from its ref: the header is not drawn again for a drag, so a value
        // closed over at render would be the one before it began.
        order.drag match
            case Present(ref) =>
                ref.get.map { held =>
                    val act: UI.Action = held match
                        case Present(d) if d.done => ref.set(Absent)
                        case _                    => toggleSort(path, e, interactive)
                    act
                }
            case Absent => toggleSort(path, e, interactive)

    /** Swallows the click a finished drag left behind, and nothing else. */
    private def clearDrag(order: OrderState)(using Frame): UI.Action =
        order.drag match
            case Present(ref) =>
                ref.get.map { held =>
                    val act: UI.Action = held match
                        case Present(d) if d.done => ref.set(Absent)
                        case _                    => ()
                    act
                }
            case Absent => ()

    /** Sticks one cell to its edge. Prime computes the same two properties in JavaScript,
      * measuring the cell before it on every render; here the widths are already known,
      * so the offset is written once, declaratively, and a resize drag moves it with the
      * column.
      */
    private def freeze(cell: Ast.Element, slot: Maybe[FrozenSlot]): Ast.Element =
        slot match
            case Absent     => cell
            case Present(s) =>
                val stuck = cell.cssClass("p-datatable-frozen-column")
                if s.edge == FrozenEdge.Start then stuck.style(_.left(s.offset)) else stuck.style(_.right(s.offset))
        end match
    end freeze

    /** [[freeze]] for a cell drawn outside the regions that read the widths (the header): where it holds and at which
      * offset follow the widths through a class and an inline-style channel, so the cell and whatever it holds are not
      * drawn again for a width. `slot` picks the cell's slot out of the plan the widths give.
      */
    private def freezeLive[E <: Ast.Element { type Self = E }](
        cell: E,
        widths: Reading[Map[List[String], Double]],
        slot: FrozenPlan => Maybe[FrozenSlot]
    )(using Frame): E =
        if !frozenOn then cell
        else
            given CanEqual[Maybe[FrozenSlot], Maybe[FrozenSlot]] = CanEqual.derived
            widths.map(w => slot(frozenPlan(SizeState(widths = w)))) match
                case Reading.Fixed(s)  => freeze(cell, s).asInstanceOf[E]
                case Reading.Live(sig) =>
                    given CanEqual[Style, Style] = CanEqual.derived
                    cell.cssClass("p-datatable-frozen-column", sig.map(_.isDefined)).inlineStyle(sig.map {
                        case Present(s) => if s.edge == FrozenEdge.Start then Style.left(s.offset) else Style.right(s.offset)
                        case Absent     => Style.empty
                    })
            end match
        end if
    end freezeLive

    /** A class that follows a reading: set statically from a fixed one, a class channel over a live one. */
    private def classWhen[E <: Ast.Element { type Self = E }](el: E, name: String, on: Reading[Boolean])(using Frame): E =
        on match
            case Reading.Fixed(b)  => if b then el.cssClass(name) else el
            case Reading.Live(sig) => el.cssClass(name, sig)

    /** What the header reads as it changes, once the header is no longer drawn again for every one of them: the sort,
      * the column filters, the widths and the column drag. Each reaches the cells through a channel or a region of its
      * own; fixed in a static projection.
      */
    final private case class HeadLive(
        sort: Reading[List[SortKey]],
        specs: Reading[Map[List[String], ColumnFilter]],
        widths: Reading[Map[List[String], Double]],
        held: Reading[Maybe[ColumnDrag]]
    )

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
        // Unbound, the table keeps what the reader sorts, filters, opens and picks itself, wherever a column or a mode
        // already says the reader may: `sortBy`, `filterBy`, `rowExpansionTemplate`, `selectionMode`.
        val ownsSort      = sortSlot.isEmpty && leafPaths.exists((_, c) => canSortBy(c))
        val ownsFilters   = columnFiltersSlot.isEmpty && (leafPaths.exists(_._2.isFilterable) || hiddenPaths.exists(_._2.isFilterable))
        val ownsExpansion = expandedTyped.isEmpty && expansionF.isDefined
        // Not under cell selection: there the mode says what a click on a CELL means, and a row selection kept here would
        // count as a bound one and switch the cells off.
        val ownsSelection = selectedTyped.isEmpty && selectionModeV != SelectionMode.None && !cellsOn
        // The layout the reader drags and the groups they open are kept only where the page asked for the feature
        // with no ref: `columnWidths()`, `columnOrder()`, `expandedGroups()`.
        // `selectedCells()` likewise.
        val ownsWidths = columnWidthsSlot.isEmpty && ownWidthsFlag
        val ownsOrder  = columnOrderSlot.isEmpty && ownOrderFlag
        val ownsGroups = expandedGroupsSlot.isEmpty && ownGroupsFlag
        val ownsCells  = cellsTyped.isEmpty && ownCellsFlag
        if !ownsSort && !ownsFilters && !ownsExpansion && !ownsSelection && !ownsWidths && !ownsOrder && !ownsGroups &&
            !ownsCells
        then
            renderBound
        else
            UI.mounted {
                for
                    sort <-
                        if ownsSort then Signal.initRef(List.empty[SortKey]).map(r => Maybe(ReactiveValue[List[SortKey]](r)))
                        else Kyo.lift(sortSlot)
                    filters <-
                        if ownsFilters then
                            Signal.initRef(Map.empty[List[String], ColumnFilter])(using summon[Frame], CanEqual.derived)
                                .map(r => Maybe(ReactiveValue[Map[List[String], ColumnFilter]](r)))
                        else Kyo.lift(columnFiltersSlot)
                    exp <-
                        if ownsExpansion then
                            Signal.initRef(Set.empty[A])(using summon[Frame], CanEqual.derived).map(r => Maybe(ReactiveValue[Set[A]](r)))
                        else Kyo.lift(expandedTyped)
                    sel <-
                        if ownsSelection then
                            Signal.initRef(Set.empty[A])(using summon[Frame], CanEqual.derived).map(r => Maybe(ReactiveValue[Set[A]](r)))
                        else Kyo.lift(selectedTyped)
                    widths <-
                        if ownsWidths then
                            Signal.initRef(Map.empty[List[String], Double])(using summon[Frame], CanEqual.derived)
                                .map(r => Maybe(ReactiveValue[Map[List[String], Double]](r)))
                        else Kyo.lift(columnWidthsSlot)
                    order <-
                        if ownsOrder then
                            Signal.initRef(List.empty[List[String]])(using summon[Frame], CanEqual.derived)
                                .map(r => Maybe(ReactiveValue[List[List[String]]](r)))
                        else Kyo.lift(columnOrderSlot)
                    groups <-
                        if ownsGroups then
                            Signal.initRef(Set.empty[GroupPath])(using summon[Frame], CanEqual.derived)
                                .map(r => Maybe(ReactiveValue[Set[GroupPath]](r)))
                        else Kyo.lift(expandedGroupsSlot)
                    cells <-
                        if ownsCells then
                            Signal.initRef(Set.empty[CellPath[A]])(using summon[Frame], CanEqual.derived)
                                .map(r => Maybe(ReactiveValue[Set[CellPath[A]]](r)))
                        else Kyo.lift(cellsTyped)
                yield copy(
                    sortSlot = sort,
                    columnFiltersSlot = filters,
                    expandedTyped = exp,
                    selectedTyped = sel,
                    columnWidthsSlot = widths,
                    columnOrderSlot = order,
                    expandedGroupsSlot = groups,
                    cellsTyped = cells,
                    ownCellsFlag = false
                ).renderBound
            }.placeholder(renderBound)
        end if
    end render

    private def renderBound(using Frame): UI =
        def static: UI =
            renderWith(Reading.Fixed(()), (), navOn = false) { _ =>
                DataTable.Wiring[A](
                    EditState(Set.empty, Absent),
                    NavState[A](),
                    FilterState(),
                    SizeState(),
                    OrderState(),
                    ScrollState(),
                    MoveState[A](),
                    // A caller-bound anchor works without a mount, which is the point of binding it:
                    // a table that ranges and owns nothing else stays a pure render.
                    SelectState(metaKey = metaKeyFlag, anchor = anchorSlot, onAnchor = onAnchorF)
                )
            }
        if !ownsState then static
        else
            val mount = UI.mounted {
                for
                    cmds <- UI.commands
                    // A caller's own id wins; the mount mints one only when none was given,
                    // so the derived part ids (headers, rows, the frozen group) work unasked
                    // but are addressable when the caller cares. Same rule as Menu's base.
                    minted <- cmds.freshId
                    prefix = idV.getOrElse(minted)
                    drafts <- Kyo.foreach(editableLeaves)((path, _) => Signal.initRef("").map(path -> _))
                    err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
                    menus  <- Kyo.foreach(filterableLeaves)((path, _) => Signal.initRef(false).map(path -> _))
                    // One draft per filterable column, seeded when the menu opens. A row
                    // display never reads them, since one input is the whole filter.
                    drafts0 <- Kyo.foreach(filterableLeaves)((path, c) =>
                        Signal.initRef(ColumnFilter.empty(c.filterV.map(_.default).getOrElse(MatchMode.Contains)))
                            .map(path -> _)
                    )
                    typed <- Kyo.foreach(filterableLeaves)((path, _) =>
                        Signal.initRef(Absent: Maybe[String])(using summon[Frame], CanEqual.derived).map(path -> _)
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
                    // The row a shift-click measures its range from. Allocated whether or not
                    // this table ranges, because a mount allocates a fixed set; only `rangeOn`
                    // decides whether it is handed on.
                    anchorRow <- Signal.initRef(Absent: Maybe[A])(using summon[Frame], CanEqual.derived)
                yield
                    val tree = wired(
                        prefix,
                        drafts.toMap,
                        err,
                        id => cmds.focusId(id),
                        menus.toMap,
                        // A grab needs every edge: one element gone mid-grab measures nothing.
                        ids =>
                            cmds.requestMeasureByIds(ids).map { rects =>
                                val found = rects.flatMap(_.toChunk)
                                if found.size == rects.size then found else Chunk.empty
                            },
                        Present(held),
                        Present(moving),
                        if windowOn then Present(scroll) else Absent,
                        if frozenRowsOn then Present(headTop) else Absent,
                        if handleColumn then Present(rowsIn) else Absent,
                        drafts0.toMap,
                        // A caller who bound the anchor owns it; the minted one is the fallback for
                        // a table that did not, and it lives exactly as long as this mount does.
                        if !rangeOn then Absent
                        else if anchorSlot.isDefined then anchorSlot
                        else Present(ReactiveValue[Maybe[A]](anchorRow)),
                        typed.toMap
                    )
                    if !frozenRowsOn then tree
                    else UI.fragment(headProbe(cmds, headId(prefix), headTop), tree)
            }.placeholder(static)
            // An id is the caller declaring which table this is, so it is also the identity the
            // instance keeps. Without one the mount stays keyless, which is what it always was.
            //
            // Everything this mount allocates is state the table owns and nobody else can hand
            // back: the shift anchor, the column grab and drag, the row drag, the scroll offset,
            // the measured frozen-header height, the editing drafts. Keyless, all of it is
            // re-allocated on every emission of whatever region encloses the table: a page that
            // re-renders on a store write rebuilds the table several times between two clicks,
            // and a shift-click then reads an anchor ref that no click ever wrote. Measured that
            // way: `anchorSet ref=70`, then `shift anchor=Absent ref=74`.
            //
            // kyo-ui's own keyless-remount warning does not catch this, because the identity is
            // minted INSIDE the mount and every run takes the next one, so the registry never
            // sees a path repeat to count.
            //
            // What a key costs is the documented price of any keyed mount: the effect runs once,
            // so the column set and the editable/filterable leaves are read at mount time.
            // Anything that must change afterwards belongs in a `Signal`, which is what `rows`,
            // `sort`, `page` and the bound refs already are.
            idV match
                case Present(id) => mount.keyed(id)
                case Absent      => mount
            end match
        end if
    end renderBound

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
        errRef: SignalRef[Maybe[(CellKey, FieldError)]],
        focus: String => UI.Action,
        menus: Map[List[String], SignalRef[Boolean]] = Map.empty,
        measure: Seq[String] => Chunk[Rect] < Async = (_: Seq[String]) => Chunk.empty,
        held: Maybe[SignalRef[Maybe[ColumnGrab]]] = Absent,
        moving: Maybe[SignalRef[Maybe[ColumnDrag]]] = Absent,
        scroll: Maybe[SignalRef[Double]] = Absent,
        headTop: Maybe[Signal[Maybe[Int]]] = Absent,
        rowDrag: Maybe[SignalRef[Maybe[RowDrag]]] = Absent,
        filterDrafts: Map[List[String], SignalRef[ColumnFilter]] = Map.empty,
        anchor: Maybe[ReactiveValue[Maybe[A]]] = Absent,
        filterTyped: Map[List[String], SignalRef[Maybe[String]]] = Map.empty
    )(using Frame): UI =
        // The caller's state is read in the table's one region (see `renderWith`): starting an edit writes the error
        // and the edited cell together.
        given errEq: CanEqual[Maybe[(CellKey, FieldError)], Maybe[(CellKey, FieldError)]] = CanEqual.derived
        given dragEq: CanEqual[Maybe[ColumnDrag], Maybe[ColumnDrag]]                      = CanEqual.derived
        given rowDragEq: CanEqual[Maybe[RowDrag], Maybe[RowDrag]]                         = CanEqual.derived
        val outer                                                                         = Reading.Live(errRef)
            .and(Reading.orElse(editingRowsRef, Set.empty[String]))
            .and(Reading.orElse(editingCellRef, Absent: Maybe[CellKey]))
            .and(Reading.orElse(moving, Absent: Maybe[ColumnDrag]))
            .and(Reading.orElse(rowDrag, Absent: Maybe[RowDrag]))
        renderWith(outer, ((((Absent, Set.empty[String]), Absent), Absent), Absent), navOn) {
            case ((((e, editRows), editCell), drag), rowHeld) =>
                DataTable.Wiring[A](
                    EditState(editRows, editCell, drafts, Present(errRef), e, live = true),
                    NavState[A](on = navOn, idPrefix = idPrefix, focus = focus),
                    FilterState(open = menus, live = true, drafts = filterDrafts, typed = filterTyped, idPrefix = idPrefix),
                    SizeState(idPrefix = idPrefix, live = true, measure = measure, grab = held),
                    OrderState(drag = moving, held = drag),
                    ScrollState(scroll, headTop),
                    MoveState[A](
                        drag = rowDrag,
                        held = rowHeld,
                        idPrefix = idPrefix,
                        live = true,
                        measure = measure
                    ),
                    SelectState(metaKey = metaKeyFlag, anchor = anchor, onAnchor = onAnchorF)
                )
        }
    end wired

    private type Drawn = DataTable.Drawn[A]

    /** The table's own state for one render, one value per concern, as the caller reads it. */
    private type Wiring = DataTable.Wiring[A]

    /** Renders the table: a root built once, holding the parts that follow the table's state as regions of their own,
      * and the parts that follow none of it outside any region.
      *
      * What follows the state (the caller's own, `outer`, turned into the table's by `wiring`; the column order, the
      * column flags, the frozen rows, the sort, the filters, the widths, the page and, where they are not read below the
      * body, the row bindings) is read as ONE reading (see [[Reading]]): an effect here writes several of them at once
      * (starting an edit writes the error and the edited cell, a column drop writes the order and the drag), and a
      * region nested per signal repaints against the values the inner one closed over. Two sibling regions draw from that
      * reading: the diagnostic cards and the table itself. Neither nests in the other, so neither can repaint an older
      * state over the other's newer one. The row bindings read BELOW the body ([[rowInputsBelow]]) stay a stream of
      * their own there.
      *
      * What follows none of it sits outside both: the header and footer slots, which are the page's own, and the
      * paginator, which reads the record count as a signal and the page from its ref. Each of those may hold a mount, and
      * a region above a mount starts it again on every repaint, which a sort, a search or an edit elsewhere is.
      */
    private def renderWith[O](outer: Reading[O], base: O, navOn: Boolean)(wiring: O => Wiring)(using CanEqual[O, O], Frame): UI =
        // The rows compare by their own equality, whatever the element type does; the region only
        // ever asks whether the values changed.
        given rowsEq: CanEqual[Seq[A], Seq[A]]                                                    = CanEqual.derived
        given specsEq: CanEqual[Map[List[String], ColumnFilter], Map[List[String], ColumnFilter]] = CanEqual.derived
        given widthsEq: CanEqual[Map[List[String], Double], Map[List[String], Double]]            = CanEqual.derived
        given inputsEq: CanEqual[RowInputs[A], RowInputs[A]]                                      = CanEqual.derived
        val order: Reading[Maybe[List[List[String]]]]                                             =
            columnOrderSlot.fold(Reading.Fixed(Maybe.empty[List[List[String]]]))(Reading.of(_).map(o => Present(o)))
        val columns = order.and(visibleReading).and(sortableReading)
        val view    = frozenRowsReading
            .and(sortReading)
            .and(queryReading)
            .and(filtersReading)
            .and(widthsReading)
            .and(pageReading)
        val live                                 = if rowInputsBelow then rowInputsSignal else Absent
        val rows: Reading[(RowInputs[A], Total)] =
            if live.isDefined then Reading.Fixed((RowInputs[A](), Total.Unknown(false)))
            else rowInputsReading
        // On the signal path the rows reach the diagnostic cards and the paginator through `chrome`, which reads them
        // (and the total) in a region of its own, so a rows emission repaints those and reconciles the rows.
        def chrome(inputs: RowInputs[A], total: Total): ((RowInputs[A], Total) => UI) => UI =
            live match
                case Present(sig) => f => Reading.Live(sig).and(totalReading).render((in, t) => f(in, t))
                case Absent       => f => f(inputs, total)
        val state = outer.and(columns).and(view).and(rows).and(pageSizeReading)
        // What the parts below the column layout read: the state but the layout, which the table container draws over.
        val inner = outer.and(view).and(rows).and(pageSizeReading)

        /** One pass over the state for the table `laid` (the layout's columns): `f` picks the part to draw. */
        def drawnAt(laid: DataTable[A], flags: Map[List[String], Boolean], requested: Maybe[List[List[String]]])(
            v: (
                (
                    (O, (((((Seq[A], List[SortKey]), String), Map[List[String], ColumnFilter]), Map[List[String], Double]), Int)),
                    (RowInputs[A], Total)
                ),
                Int
            )
        ): Drawn =
            val (((o, (((((held, sort), query), specs), widths), page)), (inputs, total)), size) = v
            val w                                                                                = wiring(o)
            laid.sized(size).body(
                held,
                sort,
                query,
                page,
                total,
                inputs,
                live,
                chrome(inputs, total),
                flags,
                w.edit,
                w.nav,
                w.filter.copy(specs = specs),
                w.size.copy(widths = widths),
                requested.fold(w.order)(r => w.order.copy(requested = r)),
                w.scroll,
                w.move,
                w.select
            )
        end drawnAt

        val cards: UI = state.render { case ((((o, ((requested, vis), flags)), view), rowsNow), size) =>
            drawnAt(requested.fold(this)(reordered).shown(vis), flags, requested)(((o, view), rowsNow), size).cards
        }
        val tablePart: UI = columns.render { case ((requested, vis), flags) =>
            val laid = requested.fold(this)(reordered).shown(vis)
            // The header's handlers read the drag through its ref when they fire; the order state still carries the layout's
            // requested order, as every other pass's does, so nothing below reads a default where the layout is bound.
            val w0 = wiring(base)
            val w  = requested.fold(w0)(r => w0.copy(order = w0.order.copy(requested = r)))
            laid.tableContainer(inner, live, w, flags)(v => drawnAt(laid, flags, requested)(v))
        }
        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-datatable-header")(toChild(h)))
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-datatable-footer")(toChild(f)))
        rootElement(order, navOn)(
            (List(cards) ++ loadingMask ++ headerSlot ++ (tablePart :: paginatorPart(order)) ++ footerSlot ++ sourceRun)
                .map(toChild)*
        )
    end renderWith

    /** A bound source fetches for as long as the table is mounted, through a mount of its own that shows nothing.
      *
      * Its own rather than the table's, because a table that owns no state has no mount, and a source is not a reason
      * to give it one: until this runs, the table renders itself loading from the source's signals, which is what lets
      * a page bind a source with no mount of its own and no copy of the table as that mount's placeholder.
      */
    private def sourceRun(using Frame): List[UI] =
        sourceV.toList.map(src => UI.mounted(src.run.andThen(UI.empty)).placeholder(UI.empty))

    /** The page size as the table pages by it: the bound one's value, else the constant (one where nothing pages). */
    private def pageSizeReading(using Frame): Reading[Int] =
        pageSizeSlot.fold(Reading.Fixed(pageSizeV.getOrElse(1)))(Reading.of(_).map(math.max(1, _)))

    /** The global query as the table filters by it: the bound one, else none. */
    private def queryReading(using Frame): Reading[String] = filterSlot.fold(Reading.Fixed(""))(Reading.of(_))

    /** The widths as the table lays out by them: the bound map, else none. */
    private def widthsReading(using Frame): Reading[Map[List[String], Double]] =
        columnWidthsSlot.fold(Reading.Fixed(Map.empty[List[String], Double]))(Reading.of(_))

    /** The column order as the table lays out by it: the bound list, else the authored one. */
    private def orderReading(using Frame): Reading[List[List[String]]] =
        columnOrderSlot.fold(Reading.Fixed(List.empty[List[String]]))(Reading.of(_))

    /** The page as the table pages by it: the bound one's value, else the first. */
    private def pageReading(using Frame): Reading[Int] = pageSlot.fold(Reading.Fixed(0))(Reading.of(_))

    /** The sort as the table sorts by it: the bound spec, else none. */
    private def sortReading(using Frame): Reading[List[SortKey]] = sortSlot.fold(Reading.Fixed(List.empty[SortKey]))(Reading.of(_))

    /** This table paging by `size`, where it pages at all. */
    private def sized(size: Int): DataTable[A] =
        if pageSizeV.isEmpty || pageSizeV.contains(size) then this else copy(pageSizeV = Present(size))

    /** The table's root. Its classes follow the table's configuration, and the one that also follows its state (whether
      * an `Expand` table states its width, which depends on which columns are shown and how wide they are) is a class
      * channel, so the root is built once and nothing under it is drawn again for it.
      */
    private def rootElement(order: Reading[Maybe[List[List[String]]]], navOn: Boolean)(using Frame): Ast.Div =
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
        if scrollingOn || frozenOn || frozenRowsOn then root = root.cssClass("p-datatable-scrollable")
        else if resizeModeV == ColumnResizeMode.Expand then
            given widthsEq: CanEqual[Map[List[String], Double], Map[List[String], Double]] = CanEqual.derived
            val expanding                                                                  = order.and(visibleReading).and(widthsReading)
                .map { case ((requested, vis), widths) =>
                    requested.fold(this)(reordered).shown(vis).resizeModeAt(SizeState(widths = widths)) == ColumnResizeMode.Expand
                }
            expanding match
                case Reading.Fixed(on) => if on then root = root.cssClass("p-datatable-scrollable")
                case Reading.Live(sig) => root = root.cssClass("p-datatable-scrollable", sig)
        end if
        if flexOn then root = root.cssClass("p-datatable-flex-scrollable")
        // The cursor keys must not ALSO scroll the page under the table. A kyo handler is
        // async and cannot decline the browser default in time, so the suppression is
        // declarative: the client reads the attribute before it posts the event. The class
        // is what gives the focused cell its ring.
        if navOn then root = root.cssClass("p-uic-dt-nav").preventScrollKeys
        sizeV match
            case Size.Small  => root = root.cssClass("p-datatable-sm")
            case Size.Large  => root = root.cssClass("p-datatable-lg")
            case Size.Normal => ()
        end match
        root
    end rootElement

    /** The table container for one column layout: drawn again only when the columns change (their order, which are shown,
      * which may sort), so the `table`, its `thead` and its `tbody` are built once per layout and every part that follows
      * the rest of the state reaches them as a channel or as a region of its own, never as a region around them:
      *
      *   - the header cells follow the sort, the widths and the column drag through class, attribute and inline-style
      *     channels, with the sort marks a region of their own; a column's filter is a region over its own filter and
      *     its menu's open state, and nothing else;
      *   - the rows are a list keyed by row over one signal of the state and the row bindings, so a row is drawn again
      *     only when what it shows changes, and an editor open in one sits under that list alone; the frozen rows are a
      *     second such list in a static group whose offset is a channel, and a windowed body one list over the state and
      *     the scroll position together, its height a channel;
      *   - the `colgroup` and the footer are regions over the state, siblings of the header and the body rather than
      *     parents of them.
      *
      * The container itself is a region over the column layout (the order, which columns are shown, which may sort),
      * and that is the contract rather than a leftover: a mount in a column that moves or hides belongs to the new
      * layout, so where the layout is bound it is the one region a mount sits under besides its own
      * (`DataTableRegionsTest` pins it). An unbound layout is fixed and adds none.
      *
      * `inner` is the state but the layout, and `drawn` the pass over one value of it. Siblings over one reading cannot
      * repaint an older state over each other, and the regions nested here read refs disjoint from the layout around
      * them, which is what keeps the one-region guarantee for state written together.
      */
    private def tableContainer[V](inner: Reading[V], live: Maybe[Signal[RowInputs[A]]], w0: Wiring, flags: Map[List[String], Boolean])(
        drawn: V => Drawn
    )(using CanEqual[V, V], Frame): UI =
        given widthsEq: CanEqual[Map[List[String], Double], Map[List[String], Double]]            = CanEqual.derived
        given specsEq: CanEqual[Map[List[String], ColumnFilter], Map[List[String], ColumnFilter]] = CanEqual.derived
        given dragEq: CanEqual[Maybe[ColumnDrag], Maybe[ColumnDrag]]                              = CanEqual.derived
        given specsListEq: CanEqual[List[RowSpec[A]], List[RowSpec[A]]]                           = CanEqual.derived
        given styleEq: CanEqual[Style, Style]                                                     = CanEqual.derived
        val head                                                                                  = HeadLive(
            sort = sortReading,
            specs = filtersReading,
            widths = widthsReading,
            held = w0.order.drag.fold(Reading.Fixed(Absent: Maybe[ColumnDrag]))(Reading.Live(_))
        )
        def part(f: Drawn => UI): UI = inner.render(v => f(drawn(v)))
        val size                     = w0.size

        // The last pass drawn, kept for the value it was drawn from: a scroll of a windowed body asks the same pass for
        // another window, and arranging the rows again for it would cost a sort per scroll tick.
        // Unsafe: one container's cache of its own last pass, read and replaced by whichever emission runs next; it never
        // leaves this container, and a stale entry is only ever a miss.
        val last                   = new java.util.concurrent.atomic.AtomicReference[Maybe[(V, Drawn)]](Absent)
        def drawnOnce(v: V): Drawn =
            last.get match
                case Present((at, d)) if at == v => d
                case _                           =>
                    val d = drawn(v)
                    last.set(Present((v, d)))
                    d

        /** A keyed row list over the specs a value of the state (and the row bindings, where they drive the rows from below)
          * gives, inside a static row group: the list is the one region above a row, so an editor in one sits under it
          * alone. Nothing bound renders inline, exactly as before, and stays free of markers.
          */
        def rowList(group: Ast.Tbody, specsOf: (Drawn, Maybe[RowInputs[A]]) => List[RowSpec[A]]): UI =
            val specs: Reading[List[RowSpec[A]]] = live match
                case Present(sig) => Reading.Live(inner.signal.combineLatest(sig).map((v, in) => specsOf(drawnOnce(v), Present(in))))
                case Absent       => inner.map(v => specsOf(drawnOnce(v), Absent))
            specs match
                case Reading.Fixed(ss) => group(ss.map(renderTr).map(toChild)*)
                case Reading.Live(sig) => group(toChild(sig.map(Chunk.from(_)).foreachKeyed(rowSpecKey)(renderTr)))
            end match
        end rowList

        // The paths whose headers the reader can actually click, which is what both click
        // transitions may clear. Everything else in the spec is the caller's to keep.
        val interactive: Set[List[String]] =
            if sortSlot.isEmpty then Set.empty
            else leafPaths.collect { case (p, c) if sortsHere(c, p, flags) => p }.toSet

        // One tr per header level. The leading expander and checkbox cells belong to the
        // top row and reach down through every other one, so they line up with a column
        // whatever depth the header has.
        val headRows: List[UI] =
            val matrix            = ColumnTree.spans(cols)
            val depth             = math.max(matrix.length, 1)
            val leading: List[UI] =
                val expanderTh: List[Ast.Th] =
                    if !expanderColumn then Nil
                    else
                        var cell = th.cssClass("p-datatable-header-cell")
                        if depth > 1 then cell = cell.rowspan(depth)
                        List(cell)
                // The select-all box reads the arranged rows and the selection, as a region of its own in its cell.
                val checkboxTh: List[Ast.Th] =
                    if !checkboxColumn then Nil
                    else
                        var cell = th.cssClass("p-datatable-header-cell")
                        if depth > 1 then cell = cell.rowspan(depth)
                        List(cell(toChild(part(_.selectAll))))
                (expanderTh ++ checkboxTh).zipWithIndex.map((cell, i) => freezeLive(cell, head.widths, _.leadAt(i)))
            end leading
            val rows = if matrix.isEmpty then List(Nil) else matrix
            // The editor-button column has no header of its own, but it still needs a cell,
            // and one that reaches down the header the way the leading ones do.
            val trailing: List[UI] =
                if !editorColumn then Nil
                else
                    var cell = th.cssClass("p-datatable-header-cell")
                    if depth > 1 then cell = cell.rowspan(depth)
                    List(freezeLive(cell, head.widths, _.trailAt(0)))
            rows.zipWithIndex.map { (cells, i) =>
                val ths = cells.map(sp => headerSpanCell(sp, head, flags, interactive, size, w0.order, w0.filter))
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
                    leadTerms.indices.toList.map(i => freezeLive(th.cssClass("p-datatable-header-cell"), head.widths, _.leadAt(i)))
                val trailing: List[UI] =
                    trailTerms.indices.toList.map(i => freezeLive(th.cssClass("p-datatable-header-cell"), head.widths, _.trailAt(i)))
                val cells = leafPaths.zipWithIndex.map((entry, i) =>
                    freezeLive(filterCell(entry._1, entry._2, w0.filter, head).asInstanceOf[Ast.Th], head.widths, _.at(i))
                )
                List(tr((leading ++ cells ++ trailing).map(toChild)*))
            end if
        end filterRowUI

        val tbodyEl: UI =
            if !windowOn then
                // The row bindings and the state meet in one signal of row specs, keyed by row: a
                // selection, a sort or an edit draws the rows whose spec it changed and no other.
                rowList(tbody.cssClass("p-datatable-tbody"), (d, in) => in.fold(d.staticSpecs)(d.specsFor))
            else
                // A windowed body: its height is the whole list's, a channel over the state, and its rows are ONE keyed
                // list over the window the scroll position and the state give, so a scroll of one row moves the two
                // spacers and draws one row, and the rows that stayed keep their nodes, handlers and open editors.
                // Over fixed rows the list maps the scroll ref alone, which binds without a fiber; over rows that move
                // it reads both in one `combineLatest`, which observes refs exactly (no repair timer).
                given CanEqual[Style, Style] = CanEqual.derived
                var group                    = tbody.cssClass("p-datatable-tbody")
                inner.map(v => Style.height(drawnOnce(v).windowHeight.px)) match
                    case Reading.Fixed(st) => group = group.style(st)
                    case Reading.Live(sig) => group = group.inlineStyle(sig)
                val window: Reading[List[RowSpec[A]]] = w0.scroll.ref match
                    case Present(r) =>
                        inner match
                            case Reading.Fixed(v)  => Reading.Live(r.map(top => drawnOnce(v).windowSpecs(top)))
                            case Reading.Live(sig) => Reading.Live(sig.combineLatest(r).map((v, top) => drawnOnce(v).windowSpecs(top)))
                    case Absent => inner.map(v => drawnOnce(v).windowSpecs(0.0))
                window match
                    case Reading.Fixed(ss) => group(ss.map(renderTr).map(toChild)*)
                    case Reading.Live(sig) => group(toChild(sig.map(Chunk.from(_)).foreachKeyed(rowSpecKey)(renderTr)))
                end match
            end if
        end tbodyEl

        // The rows that hold under the header: a static row group whose offset (the measured header height) is an
        // inline-style channel, and whose rows are a keyed list of their own, so an editor in one sits under that list
        // alone. They are drawn like any other row minus the keyboard grid and the drag. The two bodies are separate
        // lists, so a frozen row sharing a key with a body row is two keys in two namespaces rather than a duplicate.
        val frozenBody: List[UI] =
            if !frozenRowsOn then Nil
            else
                given CanEqual[Style, Style] = CanEqual.derived
                var group                    = tbody.cssClass("p-datatable-tbody").cssClass("p-datatable-frozen-tbody")
                if size.idPrefix.nonEmpty then group = group.id(s"${size.idPrefix}-frozen")
                w0.scroll.headTop.foreach(sig =>
                    group = group.inlineStyle(sig.map(_.fold(Style.empty)(px => Style.top(px.px))))
                )
                List(rowList(group, (d, in) => in.fold(d.staticFrozenSpecs)(d.frozenSpecsFor)))
            end if
        end frozenBody

        val expanding: Reading[Boolean] =
            if resizeModeV != ColumnResizeMode.Expand then Reading.Fixed(false)
            else head.widths.map(w => resizeModeAt(SizeState(widths = w)) == ColumnResizeMode.Expand)
        var tbl = table.cssClass("p-datatable-table")
        if scrollingOn || frozenOn || frozenRowsOn then tbl = tbl.cssClass("p-datatable-scrollable-table")
        else tbl = classWhen(tbl, "p-datatable-scrollable-table", expanding)
        // While rows are on their way the body stands in for them, which is what `aria-busy` tells
        // assistive technology: the rows it reads now are about to change.
        loadingV.foreach {
            case BoolValue.Dyn(sig) => tbl = tbl.aria("busy", sig.map(_.toString))
            case BoolValue.Const(b) => if b then tbl = tbl.aria("busy", "true")
        }
        // Prime's own classes carry the clipping a sized column needs (a value too long
        // for its column is cut rather than widening it); the layout mode they leave to
        // the host, which is what the `.p-uic-table-fixed` rule supplies.
        if hasWidths then tbl = tbl.cssClass("p-uic-table-fixed")
        if resizeOn then
            tbl = tbl.cssClass("p-datatable-resizable-table")
            // Prime's `-fit` is what hides the last column's handle, which is the one thing
            // that is true of fitting and not of expanding: under `Expand` the last column
            // needs no neighbour to trade with, so it keeps its handle. Under `Expand` the table
            // states its own width, or the browser hands what the columns leave over back to them
            // and a drag lands where it started; both follow the widths in place.
            tbl = classWhen(tbl, "p-datatable-resizable-table-fit", expanding.map(!_))
            head.widths.map(w =>
                if resizeModeAt(SizeState(widths = w)) == ColumnResizeMode.Expand then Style.width(statedWidth(SizeState(widths = w)))
                else Style.empty
            ) match
                case Reading.Fixed(st) => tbl = tbl.style(st)
                case Reading.Live(sig) => tbl = tbl.inlineStyle(sig)
            end match
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
        var headEl = thead.cssClass("p-datatable-thead")
        if frozenRowsOn && size.idPrefix.nonEmpty then headEl = headEl.id(headId(size.idPrefix))

        val tableEl: UI = tbl(
            ((if hasWidths then List(part(_.colgroup)) else Nil) ++ List[UI](headEl((headRows ++ filterRowUI).map(toChild)*)) ++
                frozenBody ++ List[UI](tbodyEl) ++
                (if leafCols.exists(_.hasFooter) then List(part(_.foot)) else Nil)).map(toChild)*
        )

        var container = div.cssClass("p-datatable-table-container")
        // A windowed table hangs its height on the scroller instead, which is then the one
        // element that scrolls: the container holds exactly it, so nothing overflows there
        // and the sticky header and the frozen columns resolve against the same box in
        // both directions.
        if !windowOn then
            scrollHeightV.foreach(h => container = container.style(_.maxHeight(CssValue.length(h))))
            container(toChild(tableEl))
        else
            // Prime's own nesting for a virtually scrolled table, which the extracted
            // sheet names beside the plain one: the scroller sits between the
            // container and the table.
            val px = viewportPx.getOrElse(1)
            var vs = div
                .cssClass("p-virtualscroller")
                .cssClass("p-uic-vs-viewport")
                .style(_.height(px.px).minHeight(px.px))
            w0.scroll.ref.foreach { r =>
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
        end if
    end tableContainer

    /** The paginator, outside every region of the table: it reads the record count as a signal (or a constant, where
      * nothing it is counted from can change) and the page from the page ref, and clamps the page itself, so a sort, a
      * search or an edit never draws it again and its two dropdowns never start their mounts again.
      */
    private def paginatorPart(order: Reading[Maybe[List[List[String]]]])(using Frame): List[UI] =
        if pageSizeV.isEmpty || windowOn then Nil
        else
            given specsEq: CanEqual[Map[List[String], ColumnFilter], Map[List[String], ColumnFilter]] = CanEqual.derived
            given inputsEq: CanEqual[RowInputs[A], RowInputs[A]]                                      = CanEqual.derived
            given rowsEq: CanEqual[Seq[A], Seq[A]]                                                    = CanEqual.derived
            val rowsAndTotal: Reading[(RowInputs[A], Total)]                                          =
                rowInputsSignal match
                    case Present(sig) if rowInputsBelow => Reading.Live(sig).and(totalReading)
                    case _                              => rowInputsReading
            val count: Reading[Int] =
                order.and(visibleReading)
                    .and(rowsAndTotal)
                    .and(queryReading)
                    .and(filtersReading)
                    .and(pageReading)
                    .and(pageSizeReading)
                    .map { case ((((((requested, vis), (in, total)), query), specs), page), size) =>
                        requested.fold(this)(reordered).shown(vis).sized(size).countOf(in, query, specs, page, total)
                    }
            var pag = paginatorF.getOrElse(identity[Paginator])(Paginator())
                .totalRecords(count.slot)
                .hostClass("p-datatable-paginator-bottom")
            // The table owns the page and how many rows it holds, whatever the caller handed the paginator: it hands over
            // its own bindings and its own events, so a pick is written where the table pages from and reported under the
            // table's names.
            pag = pageSizeSlot match
                case Present(size) => pag.hostRows(size, onPageSizeF, "onPageSizeChange")
                case Absent        => pag.fixedRows(pageSizeV.get)
            pag = pageSlot match
                case Present(page) => pag.hostPage(page, onPageF)
                case Absent        => pag.currentPage(0)
            List(pag.render)
        end if
    end paginatorPart

    /** How many records the paginator counts over `in`: the rows a query and the column filters leave, or, for a
      * prepared table, the total it was given. `body`'s arrangement counts the same way, through [[countFrom]].
      */
    private def countOf(in: RowInputs[A], query: String, specs: Map[List[String], ColumnFilter], page: Int, total: Total): Int =
        val reads = if lazyOn then Nil else filterReads(specs)
        countFrom(arranged(in.rows.toList, Nil, query, reads).size, page, total)

    /** The record count from the number of rows arranged, the page and the total: an unknown total counts the furthest
      * the source has reached, never less than what is on the screen, and adds one page while anything follows.
      * Counting the screen alone would shrink the page list on the way back.
      */
    private def countFrom(arrangedSize: Int, page: Int, total: Total): Int =
        pageSizeV match
            case Present(size) if lazyOn =>
                total match
                    case Total.Known(n)               => n
                    case Total.Unknown(more, atLeast) =>
                        math.max(math.max(page, 0) * size + arrangedSize, atLeast) + (if more then 1 else 0)
            case _ => arrangedSize

    /** This table with its columns in the bound [[columnOrder]], so the header, the body, the footer, the widths and the
      * keyboard grid all move together and no builder below has to ask where a column went.
      *
      * It applies BEFORE the visibility pass, which is what keeps the hidden columns' places:
      * the order this resolves to is the whole authored list, and it is the list a drop
      * rewrites, so hiding a column and moving another one does not lose the first one's
      * place.
      */
    private def reordered(o: List[List[String]]): DataTable[A] =
        val tree = ColumnTree.reorder(cols, o)
        copy(cols = tree, orderedPaths = ColumnTree.leafPaths(tree).map(_._1))

    /** This table WITHOUT the columns their [[Column.visible]] flags hide (`vis` holds the reactive ones, resolved), so
      * the header spans, the body cells, the footer, the filter, the colspans and the keyboard grid all narrow at once and
      * no builder below has to ask whether a column is on the screen.
      *
      * The columns pruning removes are kept in `hiddenPaths` rather than dropped, for the
      * two questions the answer to which must not change when a column is hidden: which
      * spec entries this table can sort by, and whether anything here is editable. Both are
      * about what the table was AUTHORED with, so a reader hiding a column neither
      * reshuffles the rows nor raises a diagnostic.
      *
      * A table where no column carries the flag is itself, so it renders exactly what it
      * rendered before the setter existed.
      */
    private def shown(vis: Map[List[String], Boolean]): DataTable[A] =
        if vis.isEmpty && leafCols.forall(_.visibleConst) then this
        else pruned(i => vis.getOrElse(leafPaths(i)._1, leafCols(i).visibleConst))

    /** This table narrowed to the columns `keep` accepts, with the rest parked in
      * `hiddenPaths`.
      */
    private def pruned(keep: Int => Boolean): DataTable[A] =
        copy(
            cols = ColumnTree.prune(cols, keep),
            hiddenPaths = leafPaths.zipWithIndex.collect { case (entry, i) if !keep(i) => entry }
        )

    /** Which of the arranged rows are on the screen, and where that slice starts.
      *
      * `count` only clamps the page number: it is the total the paginator shows, which over a
      * prepared table is the source's rather than the list's. Neither the rows nor the base depend
      * on it there, since a prepared table was handed its page already.
      */
    private def paging(sorted: List[A], page: Int, count: Int): Paging[A] =
        val prepared = lazyOn
        if windowOn then Paging(sorted, 0, 0)
        else
            pageSizeV match
                case Present(size) =>
                    val at         = math.max(page, 0)
                    val totalPages = math.max(1, (count + size - 1) / size)
                    val cur        = math.min(at, totalPages - 1)
                    Paging(
                        if prepared then sorted else sorted.slice(cur * size, cur * size + size),
                        if prepared then 0 else cur * size,
                        cur
                    )
                case Absent => Paging(sorted, 0, 0)
        end if
    end paging

    /** The whole-list values, off one arrangement. Called by the render for the values it paints
      * with, and by [[bodyViewNow]] for the ones a handler reads when it fires.
      */
    private def bodyViewOf(rows: List[A], sorted: List[A], pg: Paging[A], openGroups: Set[GroupPath])
        : BodyView[A] =
        // The rows the keyboard moves over are the ones on the SCREEN: a collapsed group renders
        // none of its own, so stepping by the paged index would land on a row nobody can see.
        val shown = RowGroup.visible(pg.rows, groupsV, Nil, openGroups, groupsOn)
        BodyView(
            selectKeys = sorted.filter(selectableAt).map(keyOf).toIndexedSeq,
            navRows = shown.toVector,
            navPage = pageSizeV.getOrElse(math.max(shown.size, 1)),
            moveAll = rows,
            moveBase = pg.base,
            moveCount = pg.rows.size
        )
    end bodyViewOf

    /** The same values, as they are NOW rather than as they were when the row was rendered.
      *
      * Every input is read from the ref the render read it from, and the arrangement is the same
      * one, so a handler that takes this sees what a fresh render would have painted. A table
      * bound to nothing reads its own constants back and this is the render's answer exactly.
      */
    private def bodyViewNow(using Frame): BodyView[A] < Async =
        for
            rowsNow <- currentRows
            sort    <- sortReading.signal.current
            query   <- queryReading.signal.current
            specs   <- filtersReading.signal.current
            page    <- pageReading.signal.current
            groups  <- groupsReading.signal.current
        yield
            val rows   = rowsNow.toList
            val reads  = if lazyOn then Nil else filterReads(specs)
            val sorted = arranged(rows, sort, query, reads)
            bodyViewOf(rows, sorted, paging(sorted, page, sorted.size), groups)
    end bodyViewNow

    /** The keyboard's view of the grid, as it is when the key arrives rather than when the cell was
      * rendered. With navigation off the render's own copy is returned untouched, which is empty
      * and is what every path here already reads it as.
      */
    private def navNow(nav: NavState[A])(using Frame): NavState[A] < Async =
        if !nav.on then Kyo.lift(nav)
        else bodyViewNow.map(v => nav.copy(rows = v.navRows, page = v.navPage))

    /** What the drag is doing NOW. The render's snapshot is one paint behind by definition; the
      * ref is the drag itself, and the module already reads it that way everywhere else.
      */
    private def heldNow(move: MoveState[A])(using Frame): Maybe[RowDrag] < Async =
        move.drag match
            case Present(ref) => ref.get
            case Absent       => Kyo.lift(move.held)

    /** Which cell is open NOW, for the same reason. */
    private def openCellNow(edit: EditState)(using Frame): Maybe[CellKey] < Async =
        editingCellTyped match
            case Present(ref) => ref.get.map(_.map(cellKey))
            case Absent       => Kyo.lift(edit.cell)

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

    /** The rows that hold under the header, read before the table builds: the card that
      * catches a row held and scrolling at once needs them beside the body's, and the group
      * that renders them needs them anyway.
      */
    private def frozenRowsReading(using CanEqual[Seq[A], Seq[A]], Frame): Reading[Seq[A]] =
        frozenRowsV.fold(Reading.Fixed(Seq.empty[A]))(Reading.of(_))

    /** The rows the table renders, and the row index the first of them sits at: the bound
      * list when there is one, the appended list otherwise. A bound list is what lets a
      * committed edit reach the screen, since the table stores the new row into it.
      *
      * The offset is zero everywhere but under a windowed [[source]], which publishes a
      * window rather than a page: there the rows have to be placed by their own index, and
      * the one they were asked for at is not the one they came back at while a fetch is in
      * flight.
      */
    private def rowsReading(using CanEqual[Seq[A], Seq[A]], Frame): Reading[(Seq[A], Int)] =
        if windowedSource then Reading.Live(sourceV.get.window.map(w => (w.rows, w.offset)))
        else
            rowsSigV.orElse(rowsRefV.map(r => r: Signal[Seq[A]]))
                .fold(Reading.Fixed((rowsV, 0)))(sig => Reading.Live(sig.map(rows => (rows, 0))))

    /** How many rows the page the table was given came out of, zero where it was given no
      * total and computes its own.
      */
    private def totalReading(using Frame): Reading[Total] =
        lazyTotalV.fold(Reading.Fixed(Total.Unknown(false): Total))(Reading.of(_))

    /** Every reactive [[Column.sortable]] flag, resolved to a plain boolean before the table
      * builds, keyed by the column's path.
      *
      * The flag reaches the class, the tab stop, the icon and the click handler of one
      * header cell, so patching it in place would mean keeping four channels in step. The
      * table already re-renders its whole subtree whenever a bound signal moves, sort,
      * filter, page, selection, expansion, groups, and this is the seventh of those: the
      * cost is a sort click's worth of render, and the cell code reads a plain boolean.
      */
    private def sortableReading(using Frame): Reading[Map[List[String], Boolean]] =
        Reading.keyed(leafPaths.flatMap((p, c) => c.sortableSig.toList.map(sig => (p, sig))))

    /** Every reactive [[Column.visible]] flag, keyed by the column's path, which a reorder leaves as it is. */
    private def visibleReading(using Frame): Reading[Map[List[String], Boolean]] =
        Reading.keyed(leafPaths.flatMap((p, c) => c.visibleSig.toList.map(sig => (p, sig))))

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
        held: Seq[A],
        sort: List[SortKey],
        query: String,
        page: Int,
        total: Total,
        staticInputs: RowInputs[A],
        liveInputs: Maybe[Signal[RowInputs[A]]],
        chrome: ((RowInputs[A], Total) => UI) => UI,
        flags: Map[List[String], Boolean],
        edit: EditState,
        nav: NavState[A],
        filterIn: FilterState,
        size: SizeState,
        order: OrderState,
        scroll: ScrollState,
        moveIn: MoveState[A],
        selectIn: SelectState[A]
    )(using Frame): Drawn =
        // A lazily loaded table is handed a window onto rows prepared elsewhere, so the
        // three passes below would filter a page, sort a page and slice a slice. It skips
        // all three and renders what it was given, in the order it was given.
        val prepared = lazyOn

        val reads  = if prepared then Nil else filterReads(filterIn.specs)
        val filter = filterIn.copy(unusable = reads.collect { case (p, Absent) => p }.toSet)

        /** One value of the row bindings, filtered, ordered and sliced.
          *
          * The whole of steps 1 to 3 for exactly one emission. It runs per emission rather than
          * once per render because the rows are now one of the bindings that drives the row list;
          * what that costs is a sort of the list, against a paint of every row, which is the trade
          * this component exists to make.
          *
          * `forTotal` reaches only `current`, the page number the paginator shows: which rows are
          * on the screen and where the slice starts never depend on it (a prepared table was handed
          * its page already), so a caller that does not have a total may pass any.
          */
        final case class Arranged(rows: List[A], sorted: List[A], pg: Paging[A], count: Int)
        def arrange(in: RowInputs[A], forTotal: Total): Arranged =
            val rows   = in.rows.toList
            val sorted = arranged(rows, sort, query, reads)
            // 3. Paginate: clamp the 0-based page and slice. A prepared table slices nothing, the
            //    rows ARE the page, and paginates over the total it was given, which is the one
            //    thing it knows about the pages it was not given. A windowed table scrolls instead:
            //    the scrollbar answers the same question the page list does, and only one of them
            //    can be right about which rows are on the screen.
            val count = countFrom(sorted.size, page, forTotal)
            Arranged(rows, sorted, paging(sorted, page, count), count)
        end arrange

        // The static path's own arrangement. On the signal path it is empty AND unread: the only
        // things below that take it are the select-all header, the footer and the windowed body,
        // which are exactly what `rowInputsBelow` excludes.
        val here     = arrange(staticInputs, total)
        val rows     = here.rows
        val sorted   = here.sorted
        val pg       = here.pg
        val paged    = pg.rows
        val pageBase = pg.base

        val colCount =
            leafCols.length + (if checkboxColumn then 1 else 0) + (if expanderColumn then 1 else 0) +
                (if handleColumn then 1 else 0) + (if editorColumn then 1 else 0)

        val frozen = frozenPlan(size)

        /** `n` rows standing in for rows that are still on their way: a skeleton per cell, or the
          * caller's [[loadingContent]] over all of them.
          */
        def loadingRows(n: Int, height: Maybe[Int]): List[RowSpec[A]] =
            val run = math.max(1, n)
            (0 until run).toList.map(i =>
                if loadingContentV.isDefined then RowSpec.Loading[A](i, i, run, colCount, height)
                else RowSpec.Slot(i, height, leadTerms.size, leafCols.size, trailTerms.size, frozen)
            )
        end loadingRows

        // A grip is only offered where a drop would land where the reader let go: the rows
        // on the screen have to be the rows in the list, in that order, and the list has to
        // be somewhere the table can write. Anything else and the column still renders,
        // since it is part of the anatomy, with nothing behind it and a card saying which.
        val moveOffered =
            handleColumn && rowMoveWritable && rowOrderOwners(sort, query, filterIn.specs).isEmpty
        def moveFor(view: BodyView[A]): MoveState[A] =
            if moveOffered then moveIn.copy(all = view.moveAll, base = view.moveBase, count = view.moveCount)
            else moveIn.copy(live = false, held = Absent)

        // The rows the keyboard moves over are the ones on the SCREEN: a collapsed group
        // renders none of its own, so stepping by the paged index would land on a row
        // nobody can see. Which groups are open is a row binding, so this is per emission.
        def navFor(in: RowInputs[A], ar: Arranged): NavState[A] =
            if !nav.on then nav
            else
                val v = bodyViewOf(ar.rows, ar.sorted, ar.pg, in.groups)
                nav.copy(rows = v.navRows, page = v.navPage)

        // The same three, off the static arrangement, for the two bodies that are only ever built
        // on the static path: the windowed one and the frozen row group.
        lazy val staticView: BodyView[A] =
            bodyViewOf(here.rows, here.sorted, here.pg, staticInputs.groups)
        lazy val staticNav: NavState[A]       = navFor(staticInputs, here)
        lazy val staticMove: MoveState[A]     = moveFor(staticView)
        lazy val staticSelect: SelectState[A] = selectIn

        /** The `<tr>` stream for one value of the row bindings.
          *
          * Everything the rows depend on is computed HERE, from `in`, and nothing is taken from the
          * enclosing render, which is what makes the stream a function of the bindings rather than
          * of the pass that happened to build it.
          */
        def specsFor(in: RowInputs[A]): List[RowSpec[A]] =
            val all = arrange(in, Total.Unknown(false))
            // While rows are on their way the body already has the page's shape: the rows it holds, no more
            // than the page will have, then the place of the rest. So it neither says "No records" for a page
            // nobody has answered yet nor grows, or shrinks, when the page lands.
            val expected = if in.waiting then expectedRows(in.total, all.pg.rows.size) else all.pg.rows.size
            val ar       = all.copy(pg = all.pg.copy(rows = all.pg.rows.take(expected)))
            val missing  =
                if !in.waiting then 0
                else if ar.pg.rows.isEmpty then math.max(1, expected)
                else expected - ar.pg.rows.size
            val view = bodyViewOf(ar.rows, ar.sorted, ar.pg, in.groups)
            val data =
                if ar.pg.rows.isEmpty then Nil
                else
                    groupSegments(
                        ar.pg.rows.zipWithIndex,
                        groupsV,
                        Nil,
                        in.sel,
                        in.ctx,
                        in.cells,
                        in.exp,
                        in.groups,
                        colCount,
                        edit,
                        navFor(in, ar),
                        frozen,
                        moveFor(view),
                        selectIn
                    )
            if missing > 0 then data ++ loadingRows(missing, Absent)
            else if data.isEmpty then List(RowSpec.Empty(colCount))
            else data
            end if
        end specsFor

        /** How many rows the page that is loading will hold. Over a paginated window onto rows prepared
          * elsewhere that is the page size, or what a known total leaves for the last page; a table that
          * slices its own rows already holds the page it shows, so it holds the place of a page only
          * while it has none.
          */
        def expectedRows(total: Total, held: Int): Int =
            pageSizeV match
                case Present(size) if lazyOn =>
                    total match
                        case Total.Known(n) => math.max(0, math.min(size, n - math.max(0, page) * size))
                        case _              => size
                case Present(size) if held == 0 => size
                case _                          => held

        /** How many rows there are to scroll over, which does not depend on where the
          * reader is: the rows themselves locally, and how far the source reaches over one.
          */
        lazy val windowCount: Int =
            if windowedSource then viewport.extent(total, staticInputs.offset + paged.size) else paged.size

        /** The rows of the window at `scrollTop`, between the two spacers.
          *
          * The row a slot shows is found by its ABSOLUTE index, and the rows the table holds
          * start at `rowOffset`, which over a source is the offset of the window it
          * published rather than the one this viewport last asked for. Those two differ for
          * exactly as long as a fetch is in flight, and drawing the published rows at the
          * asked-for offsets is how a list flickers through wrong rows while it loads.
          */
        def windowSpecs(scrollTop: Double): List[RowSpec[A]] =
            val vp        = viewport
            val held      = paged.toVector
            val loadedEnd = staticInputs.offset + held.size
            val count     = windowCount
            // Nothing to scroll over while the source has not answered yet holds the viewport's rows,
            // as slots at the height they will have, rather than saying "No records".
            if count <= 0 && staticInputs.waiting then loadingRows(vp.visible, Present(rowHeightV.getOrElse(1)))
            else if count <= 0 then List(RowSpec.Empty(colCount))
            else
                val (from, reach)           = vp.span(vp.clamp(scrollTop, count))
                val until                   = math.min(count, reach)
                val drawn: List[RowSpec[A]] = (from until until).toList.map { i =>
                    if i >= staticInputs.offset && i < loadedEnd then
                        RowSpec.Data(
                            held(i - staticInputs.offset),
                            i,
                            staticInputs.sel.contains(keyOf(held(i - staticInputs.offset))),
                            staticInputs.ctx.contains(keyOf(held(i - staticInputs.offset))),
                            staticInputs.cells.collect {
                                case c if c.row == keyOf(held(i - staticInputs.offset)) => c.column
                            },
                            staticInputs.exp.contains(keyOf(held(i - staticInputs.offset))),
                            colCount,
                            Map.empty,
                            edit,
                            staticNav,
                            frozen,
                            staticMove,
                            staticSelect,
                            rowHeightV
                        )
                    else if loadingContentV.isDefined then
                        // The rows not reached lie in one run above the held ones or one below them, and
                        // the caller's content stands over the whole run.
                        val (runFrom, runUntil) =
                            if i < staticInputs.offset then (from, math.min(until, staticInputs.offset))
                            else (math.max(from, loadedEnd), until)
                        RowSpec.Loading[A](i, i - runFrom, runUntil - runFrom, colCount, Present(rowHeightV.getOrElse(1)))
                    else
                        RowSpec.Slot(
                            i,
                            Present(rowHeightV.getOrElse(1)),
                            leadTerms.size,
                            leafCols.size,
                            trailTerms.size,
                            frozen
                        )
                }
                // Both spacers are always emitted, one of them at nothing at either end of
                // the list, so a window is the same shape wherever it sits and the leading
                // one is always there to say where it starts.
                val lead: RowSpec[A] =
                    RowSpec.Spacer(from * vp.itemSize, Present((from, until - from)))
                (lead :: drawn) :+ RowSpec.Spacer[A]((count - until) * vp.itemSize)
            end if
        end windowSpecs

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
        /** The frozen rows for one value of the row bindings.
          *
          * They are drawn like any other row minus the keyboard grid and the drag, so neither the
          * nav rows nor the move base reach them and none of the arrangement is needed: these rows
          * are the caller's own list, not a page of it.
          */
        def frozenSpecs(rows: Seq[A], in: RowInputs[A]): List[RowSpec[A]] =
            rows.toList.zipWithIndex.flatMap { (a, i) =>
                val rowId            = keyOf(a)
                val data: RowSpec[A] = RowSpec.Data(
                    a,
                    i,
                    in.sel.contains(rowId),
                    in.ctx.contains(rowId),
                    in.cells.collect { case c if c.row == rowId => c.column },
                    in.exp.contains(rowId),
                    colCount,
                    Map.empty,
                    edit,
                    nav.copy(on = false),
                    frozen,
                    moveIn.copy(live = false, held = Absent),
                    selectIn
                )
                if isExpanded(a, in.exp) then List(data, RowSpec.Expansion(a, colCount)) else List(data)
            }

        // The footer aggregates over the FILTERED rows, not the visible page: a
        // column total that changed when the reader turned the page would be wrong.
        lazy val footGroup: List[UI] =
            if !leafCols.exists(_.hasFooter) then Nil
            else
                val leadingTds: List[UI]  = leadTerms.indices.toList.map(i => freeze(td, frozen.leadAt(i)))
                val trailingTds: List[UI] = trailTerms.indices.toList.map(i => freeze(td, frozen.trailAt(i)))
                val footTds: List[UI]     =
                    leafCols.zipWithIndex.map((c, i) => freeze(footerCell(c, sorted), frozen.at(i)))
                val footRow: UI = tr((leadingTds ++ footTds ++ trailingTds).map(toChild)*)
                List(tfoot.cssClass("p-datatable-tfoot")(toChild(footRow)))

        DataTable.Drawn[A](
            // Three of the cards read the rows (a repeated rowKey, a frozen group taller than the
            // page, a lazy total that does not add up), so they answer for the rows the reader is
            // looking at and go through `chrome`, which is transparent unless the rows drive the
            // body themselves. Two calls rather than one, so the cards keep the order they had.
            // The rest are about the table's declaration and never move.
            cards0 = UI.fragment(
                (List(chrome((in, t) => UI.fragment(rowKeyCard(arrange(in, t).pg.rows)*))) ++ headerCards(
                    sort,
                    flags
                ) ++ editCards ++ filterCards(filter) ++ sizeCards(
                    size
                ) ++ frozenCards(size) ++ orderCards(
                    order
                ) ++ rowsCards ++ selectionCards ++ expansionCards ++ scrollCards ++ moveCards(
                    sort,
                    query,
                    filter.specs
                ) ++ windowCards ++ List(chrome { (in, t) =>
                    val ar = arrange(in, t)
                    UI.fragment((frozenRowCards(ar.pg.rows, held) ++ lazyCards(ar.rows, t))*)
                }))*
            ),
            colgroup0 = UI.fragment(colGroup(size)*),
            foot0 = UI.fragment(footGroup*),
            // A select-all header reads the whole selection, which is why a table that has one
            // resolves the row bindings above the body (see `rowInputsBelow`) and this value
            // is the real one rather than the empty stand-in.
            selectAll0 = selectAllBox(sorted, staticInputs.sel),
            staticSpecs0 = specsFor(staticInputs),
            specsFor = specsFor,
            staticFrozenSpecs0 = frozenSpecs(held, staticInputs),
            frozenSpecsFor = frozenSpecs(held, _),
            windowSpecs = windowSpecs,
            windowHeight0 = windowCount * viewport.itemSize
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
            // Up while the rows load AND the mask is wanted: `loadingMask` when it is set, else on over
            // the skeletons and off over loading content of the caller's own, which already says how the
            // wait looks and would have the spinner on top of it. Two constants decide it here; a signal
            // on either side makes it one region of its own.
            val loading = loadingV.fold(Reading.Fixed(false))(Reading.of(_))
            val wanted  = loadingMaskV.fold(Reading.Fixed(loadingContentV.isEmpty))(Reading.of(_))
            loading.and(wanted).map(_ && _) match
                case Reading.Fixed(true)  => List(maskDiv)
                case Reading.Fixed(false) => Nil
                case Reading.Live(sig)    => List(sig.render(b => if b then maskDiv else UI.empty))
            end match
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
    private def rowKeyCard(shown: Seq[A])(using Frame): List[UI] =
        val usesIdentity =
            selectedTyped.isDefined || expandedTyped.isDefined || onRowClickF.isDefined ||
                editingRowsTyped.isDefined || editingCellTyped.isDefined
        val unset =
            if rowKeyF.isDefined || !usesIdentity then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selection, expansion or onRowClick is bound but rowKey is unset, so rows are keyed by position " +
                        "and a data change re-associates that state with the wrong record; set rowKey",
                    Nil
                ))
        // A rowKey that is not an identity costs twice, and neither cost reaches the page: the
        // reader's selection lands on both rows at once, and the body's keyed list refuses to
        // reuse anything at all: it warns to the console and rebuilds the whole emission. The
        // column keys have had this card since they were introduced; the row keys read from the
        // caller's own data, which is where a duplicate is likelier and harder to see.
        val repeated =
            if rowKeyF.isEmpty then Nil
            else
                KeyDiagnostics.duplicates(shown.map(keyOf)) match
                    case Nil  => Nil
                    case dups =>
                        List(KeyDiagnostics.card(
                            "DataTable",
                            "rowKey repeats over the rows on the screen, so the rows sharing a key share their " +
                                "selection and expansion, and the body renders every row again on every change; " +
                                "project rowKey to something unique",
                            dups
                        ))
        unset ++ repeated
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
        val unknown                          = if sortSlot.isEmpty then Nil else sort.map(_.path).filterNot(sortable.contains).map(show)
        val ambiguous                        =
            if sortSlot.isEmpty then Nil else KeyDiagnostics.duplicates(sortable.map(show))
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
        val unheardCard =
            if sortable.isEmpty || !ReactiveValue.deaf(sortSlot, onSortF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the sort is bound one way and nothing hears a header press, so the reader cannot change it; add " +
                        "onSortChange, or bind a SignalRef",
                    sortable.map(show)
                ))
        emptyCard ++ unknownCard ++ ambiguousCard ++ noOrderingCard ++ unheardCard
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
        val nothing    =
            if columnFiltersSlot.isEmpty || filterable then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "columnFilters is bound but no column can be filtered, so the filter row has nothing to hold; " +
                        "add Column.filterBy",
                    Nil
                ))
        // The paths, not the visible ones: a filter on a hidden column is deliberately
        // inert, and reporting it as unknown would name a mistake nobody made.
        val known   = (leafPaths ++ hiddenPaths).collect { case (p, c) if c.isFilterable => p }.toSet
        val unknown =
            if columnFiltersSlot.isEmpty then Nil
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
        val unheard =
            if !filterable || !ReactiveValue.deaf(columnFiltersSlot, onColumnFiltersF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the column filters are bound one way and nothing hears what the reader types or applies, so the " +
                        "reader cannot change them; add onColumnFiltersChange, or bind a SignalRef",
                    Nil
                ))
        nothing ++ unknownCard ++ crowdedCard ++ unheard
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
            if !widthsOn || resizeOn then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "columnWidths is bound but no boundary is draggable, so nothing can ever write into it; a " +
                        "boundary needs a resizable column on both sides of it, and the last column has none",
                    Nil
                ))
        // Over the authored columns rather than the visible ones: a width on a hidden
        // column is what a reader showing it again gets back, not a mistake.
        val known   = (leafPaths ++ hiddenPaths).map(_._1).toSet
        val unknown =
            if !widthsOn then Nil
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
        val unsized    = if resizeModeV == ColumnResizeMode.Expand then unsizedLeaves(size) else Nil
        val expandCard =
            if unsized.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "an expanding table states its own width, which is the sum of its columns, so every column " +
                        "needs one; these have none and the table resizes to fit instead",
                    unsized.map(_.mkString(" / "))
                ))
        val unheard =
            if !resizeOn || !ReactiveValue.deaf(columnWidthsSlot, onColumnWidthsF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the column widths are bound one way and nothing hears a drag, so the reader cannot change them; add " +
                        "onColumnWidthsChange, or bind a SignalRef",
                    Nil
                ))
        pinned ++ unknownCard ++ expandCard ++ unheard
    end sizeCards

    /** What a bound column order cannot do, which is the same three shapes the widths
      * report: nothing to write into it, a column it does not know, and an arrangement
      * this header cannot take.
      */
    private def orderCards(order: OrderState)(using Frame): List[UI] =
        if !orderOn then Nil
        else
            val known   = (leafPaths ++ hiddenPaths).map(_._1).toSet
            val nothing =
                if reorderOn then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "columnOrder is bound but no column may be dragged, so nothing can ever write into it; a move " +
                            "needs two columns willing to trade places",
                        Nil
                    ))
            val unknown   = order.requested.filterNot(known.contains).map(_.mkString(" / ")).sorted
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
            val unheard =
                if !reorderOn || !ReactiveValue.deaf(columnOrderSlot, onColumnOrderF) then Nil
                else
                    List(KeyDiagnostics.card(
                        "DataTable",
                        "the column order is bound one way and nothing hears a drop, so the reader cannot change it; add " +
                            "onColumnOrderChange, or bind a SignalRef",
                        Nil
                    ))
            nothing ++ strangers ++ torn ++ unheard
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
    /** Expansion bound one way that nothing hears: the toggles take presses that open nothing. */
    private def expansionCards(using Frame): List[UI] =
        val rows =
            if expansionF.isEmpty || !ReactiveValue.deaf(expandedTyped, onExpandedF) then Nil
            else List("expanded is bound one way and nothing hears a row toggle: add onExpandedChange, or bind a SignalRef")
        val groups =
            if !groupsV.exists(_.showHeaderFlag) || !ReactiveValue.deaf(expandedGroupsSlot, onExpandedGroupsF) then Nil
            else
                List("expandedGroups is bound one way and nothing hears a group toggle: add onExpandedGroupsChange, or bind " +
                    "a SignalRef")
        val unheard = rows ++ groups
        if unheard.isEmpty then Nil
        else List(KeyDiagnostics.card("DataTable", "a binding the reader cannot move", unheard))
    end expansionCards

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
            if selectedTyped.isEmpty then Nil else List("selection, which picks rows"),
            if !cellsOn then Nil else List("selectedCells, which picks cells"),
            if editingCellTyped.isEmpty then Nil else List("editingCell, which opens an editor")
        ).flatten
        val clash =
            if !cellsOn || claimed.sizeIs < 2 then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a click on a cell can mean one thing, and this table binds more than one; cell editing keeps " +
                        "the click, then row selection, and cell selection is off while either is bound",
                    claimed
                ))
        val modeless =
            if !cellsOn || selectionModeV == SelectionMode.Single ||
                selectionModeV == SelectionMode.Multiple
            then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "cell selection needs selectionMode Single or Multiple to say what a click means; Checkbox and " +
                        "Radio are about a column of the table, which a cell is not in",
                    List(selectionModeV.toString)
                ))
        // A one-way `selected` paints but never writes, which is the point of it: the caller owns
        // the set and applies the change themselves. `onSelectionChange` is the outlet that
        // carries it; `onRowClick` covers the toggling case only, since a row key cannot say what
        // a range or a replacement became.
        // A signal, not a constant: a constant nobody hears is fixed, and draws nothing to pick.
        val oneWay     = selectedTyped.exists(_.dyn.isDefined) && selectedRef.isEmpty
        val noOutlet   = onSelectionF.isEmpty
        val unwritable =
            if !oneWay || !noOutlet || selectionModeV != SelectionMode.Checkbox then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selection is bound one way, so the checkbox column has nothing to write; a checkbox is not a row " +
                        "click, so onRowClick is no outlet for it; bind onSelectionChange, or a SignalRef",
                    List(selectionModeV.toString)
                ))
        val inert =
            if !oneWay || !noOutlet || !rowClickSelects || onRowClickF.isDefined then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "selection is bound one way with no onSelectionChange and no onRowClick, so a row click has " +
                        "nowhere to go; the rows offer the pointer that says they can be picked and then do nothing",
                    List(selectionModeV.toString)
                ))
        // The one combination that looks wired and is not: modifiers resolve into a whole new SET,
        // and a row key cannot carry a range or a replacement back to a caller who owns the state.
        val keyOnly =
            if !oneWay || !metaKeyFlag || !noOutlet || onRowClickF.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "metaKeySelection resolves a click into a new selection, and onRowClick reports the row clicked, which " +
                        "cannot express a range or a replacement; a one-way selection needs onSelectionChange",
                    List(selectionModeV.toString)
                ))
        val contextDeaf =
            if !ReactiveValue.deaf(contextTyped, onRowContextF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the context menu row is bound one way and nothing hears a right-click, so the browser's own menu " +
                        "opens over the rows; add onContextMenuRowChange, or bind a SignalRef",
                    Nil
                ))
        val cellsDeaf =
            if !cellSelectOn || !ReactiveValue.deaf(cellsTyped, onCellsF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the selected cells are bound one way and nothing hears a click, so the reader cannot pick one; add " +
                        "onSelectedCellsChange, or bind a SignalRef",
                    Nil
                ))
        val anchorDeaf =
            if !rangeOn || !ReactiveValue.deaf(anchorSlot, onAnchorF) then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the selection anchor is bound one way and nothing hears a click move it, so every range starts " +
                        "where the signal says; add onSelectionAnchorChange, or bind a SignalRef",
                    Nil
                ))
        limited ++ clash ++ modeless ++ unwritable ++ inert ++ keyOnly ++ contextDeaf ++ cellsDeaf ++ anchorDeaf
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
            val both  =
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
        val bound    = editingRowsTyped.isDefined || editingCellTyped.isDefined
        val editable = allPaths.exists(_._2.isEditable)
        val nothing  =
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
            if !(editingRowsTyped.isDefined && editingCellTyped.isDefined) then Nil
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
    private def selectAllBox(inFilter: List[A], sel: Set[String])(using Frame): UI =
        val keys              = inFilter.filter(selectableAt).map(keyOf)
        val allSelected       = keys.nonEmpty && keys.forall(sel.contains)
        val toggle: UI.Action =
            currentValue(selectedBinding, Set.empty[String])
                .map(cur => applySelection(if allSelected then cur -- keys else cur ++ keys))
        val box = CheckBox().checked(allSelected).accessibleName("Select All")
        // A fixed selection shows the box, read-only.
        (if selectionFixed then box else box.onChange(_ => toggle)).render
    end selectAllBox

    /** One cell of the header matrix: a leaf renders its column header, a group a plain
      * title cell as wide as the leaves beneath it.
      */
    private def headerSpanCell(
        sp: ColumnTree.HeaderSpan[A],
        live: HeadLive,
        flags: Map[List[String], Boolean],
        interactive: Set[List[String]],
        size: SizeState,
        order: OrderState,
        filter: FilterState
    )(using Frame): UI =
        val at = sp.at
        sp.node.asColumn match
            case Present(c) =>
                headerCell(c, sp.path, live, sp.rowspan, sortsHere(c, sp.path, flags), interactive, at, size, order, filter)
            case Absent =>
                var cell = th.cssClass("p-datatable-header-cell")
                if sp.colspan > 1 then cell = cell.colspan(sp.colspan)
                // A group moves as one, since its columns are its columns wherever it goes.
                val built = cell(
                    div.cssClass("p-datatable-column-header-content")(
                        toChild(span.cssClass("p-datatable-column-title")(sp.node.label))
                    )
                )
                freezeLive(
                    reorderCell(built, sp.path, at, at + sp.colspan, size, order, sorts = false, live),
                    live.widths,
                    _.span(at, at + sp.colspan)
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
    private def filterCell(path: List[String], c: Column[A, FlatOnly], filter: FilterState, live: HeadLive)(using
        Frame
    ): Ast.Element =
        val cell = th.cssClass("p-datatable-header-cell")
        c.filterV match
            case Absent      => cell
            case Present(cf) =>
                given CanEqual[ColumnFilter, ColumnFilter] = CanEqual.derived
                val spec: Reading[ColumnFilter]            = live.specs.map(_.getOrElse(path, ColumnFilter("", cf.default)))
                // The input, the funnel and an open constraint list are ONE region over this column's filter and its menu's
                // open state, and nothing else: a pick writes both, and the column's own cell is all either changes.
                val open: Reading[Boolean] =
                    filter.openOf(path) match
                        case Present(openRef) if filter.live && cf.modes.sizeIs > 1 => Reading.Live(openRef)
                        case _                                                      => Reading.Fixed(false)
                // Over one-way filters the field shows what the reader typed until they leave it (see `FilterState.typed`);
                // a two-way binding is written on every keystroke, in order, so its value is already what they typed.
                val typedRef: Maybe[SignalRef[Maybe[String]]] =
                    if columnFiltersSlot.exists(_.writable.isEmpty) then filter.typedOf(path) else Absent
                given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
                val typed: Reading[Maybe[String]]            = typedRef.fold(Reading.Fixed(Absent: Maybe[String]))(Reading.Live(_))
                cell(toChild(spec.and(open).and(typed).render { case ((held, isOpen), kept) =>
                    // The filter as the reader sees it: the field, its validity and the mode menu all read this one, so a
                    // mode picked before the owner caught up carries what the reader typed.
                    val cur = kept.fold(held)(q => ColumnFilter(q, held.mode))
                    // A query this table cannot read as a value of its column's type is marked on its own input.
                    val unusable = !lazyOn && cur.active.nonEmpty && cf.predicate(cur).isEmpty
                    var input    = Input()
                        .value(cur.query)
                        .fluid(true)
                        .invalid(unusable)
                        .accessibleName(s"Filter by ${c.headerV}")
                        .onInput(t => typedRef.fold[UI.Action](())(_.set(Present(t))).andThen(writeFilter(path, cur.mode, t)))
                    typedRef.foreach(ref => input = input.onBlur(_ => ref.set(Absent)))
                    val element = div.cssClass("p-datatable-filter-element-container")(toChild(input))
                    div.cssClass("p-datatable-inline-filter")((element :: modeMenu(path, c, cf, cur, filter, isOpen)).map(toChild)*)
                }))
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
        filter: FilterState,
        isOpen: Boolean
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
                    // Host-gated: the cell's region already reads the open state, so the list is drawn in it under
                    // the column's panel id, never in a mount of its own that the region would start again.
                    List(
                        Overlay(openRef)
                            .matchWidth(false)
                            .panelClass("p-datatable-filter-overlay")
                            .panelClass("p-component")
                            .trigger(trigger(Present(openRef)))(constraintList(path, cur, cf, openRef))
                            .renderGated(isOpen, Present(filterPanelId(filter.idPrefix, path)))
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
                val pick: UI.Action = pickMode(path, cur, m, openRef)
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
    private def filterFunnel(path: List[String], c: Column[A, FlatOnly], filter: FilterState, live: HeadLive)(using
        Frame
    ): List[UI] =
        if !filterMenuOn then Nil
        else
            c.filterV match
                case Absent      => Nil
                case Present(cf) =>
                    given CanEqual[ColumnFilter, ColumnFilter] = CanEqual.derived
                    val spec: Reading[ColumnFilter]            = live.specs.map(_.getOrElse(path, ColumnFilter.empty(cf.default)))
                    def trigger(cur: ColumnFilter, open: Maybe[SignalRef[ColumnFilter]], openRef: Maybe[SignalRef[Boolean]]): UI =
                        // Filled while the column is filtering, hollow while it is not: which
                        // columns are narrowing the table has to be readable without opening
                        // anything.
                        val glyph = if cur.active.isEmpty then Icons.filter else Icons.filterFill
                        var b     = Button()
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
                                    // The open state, the draft and the column's filter are read as ONE region (see
                                    // [[Reading]]): opening writes the draft and the open state together, and applying writes
                                    // the filter and closes. It is the only region above the menu: the header around it
                                    // follows the sort, the widths and the drag through channels.
                                    toChild(Reading.Live(openRef).and(Reading.Live(draft)).and(spec).render { case ((open, d), cur) =>
                                        Overlay(openRef)
                                            .matchWidth(false)
                                            .panelClass("p-datatable-filter-overlay-popover")
                                            .panelClass("p-component")
                                            .trigger(trigger(cur, Present(draft), Present(openRef)))(
                                                filterMenu(path, c, cf, d, draft, openRef)
                                            )
                                            .renderGated(open, Present(filterPanelId(filter.idPrefix, path)))
                                    })
                                )
                            )
                        case _ =>
                            List(span.cssClass("p-datatable-popover-filter")(toChild(spec.render(cur => trigger(cur, Absent, Absent)))))
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
                        .current(Present(d.operator))
                        .accessibleName(s"Match mode for ${c.headerV}")
                        .extraClass("p-datatable-filter-operator-dropdown")
                        .onChange(operatorPicked(draft, d))
                        .render
                )

        val rules: List[UI] = d.rules.zipWithIndex.map { (rule, i) =>
            val modes =
                Select[MatchMode]()
                    .options(cf.modes)(_.label)
                    .current(Present(rule.mode))
                    .accessibleName(s"Condition ${i + 1} for ${c.headerV}")
                    .onChange(modePicked(draft, d, i))
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
    /** What the filter menu's operator dropdown runs on a pick: the draft takes the operator. Typed like
      * [[Paginator.rowsChange]], for the same reason.
      */
    private[uic] def operatorPicked(draft: SignalRef[ColumnFilter], d: ColumnFilter)(using Frame): Maybe[FilterOperator] => UI.Action =
        _.fold[UI.Action](())(op => draft.set(d.copy(operator = op)))

    /** What the match-mode dropdown of rule `i` runs on a pick: the draft's rule takes the mode. */
    private[uic] def modePicked(draft: SignalRef[ColumnFilter], d: ColumnFilter, i: Int)(using Frame): Maybe[MatchMode] => UI.Action =
        _.fold[UI.Action](())(mode => draft.set(setRule(d, i, _.copy(mode = mode))))

    private def setRule(d: ColumnFilter, at: Int, f: FilterRule => FilterRule): ColumnFilter =
        d.copy(rules = d.rules.zipWithIndex.map((r, i) => if i == at then f(r) else r))

    /** Opening the menu seeds the draft from what is applied, so the panel shows the
      * filter the table is running and not whatever was last abandoned in it.
      */
    private def openFilterMenu(cur: ColumnFilter, draft: SignalRef[ColumnFilter], openRef: SignalRef[Boolean])(using
        Frame
    ): UI.Action =
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
    )(using Frame): UI.Action =
        stepFilters(m => if d.active.isEmpty then m - path else m + (path -> d)).andThen(openRef.set(false))
    end applyFilter

    /** Clear: the column stops filtering AND the draft goes back to one empty rule, since
      * a Clear that left the rules standing would be an apply away from coming back.
      */
    private def clearFilter(
        path: List[String],
        cf: CellFilter[A],
        draft: SignalRef[ColumnFilter],
        openRef: SignalRef[Boolean]
    )(using Frame): UI.Action =
        stepFilters(_ - path).andThen(draft.set(ColumnFilter.empty(cf.default))).andThen(openRef.set(false))
    end clearFilter

    /** Writes what the reader typed, keeping the mode they picked: an emptied input is no
      * filter, but it is still the column's own mode, so clearing the text and typing
      * again does not silently go back to "contains".
      */
    private def writeFilter(path: List[String], mode: MatchMode, query: String)(using Frame): UI.Action =
        stepFilters(_ + (path -> ColumnFilter(query, mode)))

    /** Every change the reader makes to the filters: written if they bind two way, then reported. */
    private def stepFilters(step: Map[List[String], ColumnFilter] => Map[List[String], ColumnFilter])(using Frame): UI.Action =
        ReactiveValue.update(columnFiltersSlot, step, onColumnFiltersF)

    /** The filters as the table filters by them: the bound map, else none. */
    private def filtersReading(using Frame): Reading[Map[List[String], ColumnFilter]] =
        columnFiltersSlot.fold(Reading.Fixed(Map.empty[List[String], ColumnFilter]))(Reading.of(_))

    private def pickMode(
        path: List[String],
        cur: ColumnFilter,
        mode: MatchMode,
        openRef: SignalRef[Boolean]
    )(using Frame): UI.Action =
        stepFilters(_ + (path -> ColumnFilter(cur.query, mode))).andThen(openRef.set(false))
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
            val cells: List[UI]    = leafPaths.map { (p, c) =>
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
    private def beginResize(i: Int, e: PointerEvent, size: SizeState)(using Frame): UI.Action =
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
    ): UI.Action =
        (size.grab, columnWidthsSlot) match
            case (Present(g), Present(_)) =>
                g.get.map {
                    case Present(held) =>
                        val delta                                                        = e.rectX + e.x - held.startX
                        val step: Map[List[String], Double] => Map[List[String], Double] = (resizeModeAt(size), next) match
                            case (ColumnResizeMode.Fit, Present(n)) =>
                                val (a, b) = DataTable.resizeTo(held.width, held.next, delta)
                                _ + (path -> a) + (n -> b)
                            case _ =>
                                _ + (path -> DataTable.expandTo(held.width, delta))
                        ReactiveValue.update(columnWidthsSlot, step, onColumnWidthsF)
                    case Absent => ()
                }
            case _ => ()

    private def headerCell(
        c: Column[A, FlatOnly],
        path: List[String],
        live: HeadLive,
        rows: Int,
        sorts: Boolean,
        interactive: Set[List[String]],
        index: Int,
        size: SizeState,
        order: OrderState,
        filter: FilterState
    )(using Frame): UI =
        given CanEqual[SortDirection, SortDirection] = CanEqual.derived
        val sortable                                 = sorts && sortSlot.isDefined && !sortFixed
        val direction: Reading[SortDirection]        =
            live.sort.map(sort => sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted))

        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        // Only once the mount has run: the id exists to be measured, and stamping it in the
        // static projection would put the same one on every table of a page.
        if size.live && (widthsOn || reorderOn) && index >= 0 then cell = cell.id(headerId(index, size))
        // Whether this boundary takes a grip follows the widths where they decide it: under `Expand` the table expands
        // only once every column is sized, which a drag may be what completes, so the class is a channel and the grip a
        // region over the widths alone. Under `Fit` the widths decide nothing here.
        val resizable: Reading[Boolean] =
            if index < 0 then Reading.Fixed(false)
            else if resizeModeV == ColumnResizeMode.Expand then live.widths.map(w => resizableAt(index, size.copy(widths = w)))
            else Reading.Fixed(resizableAt(index, size))
        cell = classWhen(cell, "p-datatable-resizable-column", resizable)
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
        // Which way the column sorts reaches the cell through a class and an attribute channel, so a sort click
        // patches the header rather than drawing it, and the column's filter menu in it, again. Prime's rule
        // (HeaderCell.ariaSort): every column a sort can name says how it sorts, `none` while it does not, and no other
        // column carries the attribute. One rule for the static page and the live one, so the attribute never has to
        // leave the cell. A column locked against the reader's clicks is still one a sort can name.
        cell = classWhen(cell, "p-datatable-column-sorted", direction.map(_.isSorting))
        if sortSlot.isDefined && !sortFixed && canSortBy(c) then
            given CanEqual[String, String] = CanEqual.derived
            direction.map {
                case SortDirection.Ascending  => "ascending"
                case SortDirection.Descending => "descending"
                case SortDirection.Unsorted   => "none"
            } match
                case Reading.Fixed(v)  => cell = cell.aria("sort", v)
                case Reading.Live(sig) => cell = cell.aria("sort", sig)
            end match
        end if

        // The neutral icon is the affordance and the directional one is state, so a column
        // the reader may not re-sort still says which way it currently sorts. Sorting by one column
        // needs no ordinal; sorting by several does, because the spec is ordered and the icons alone
        // cannot say which key is primary. Both follow the sort as a region of their own, which holds
        // nothing but an icon and a badge.
        val sortMarks: UI = live.sort.render { sort =>
            val sortingKs      = SortKey.sorting(sort)
            val rank           = sortingKs.indexWhere(_.path == path)
            val dir            = sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted)
            val icon: List[UI] =
                if !sortable && !dir.isSorting then Nil
                else
                    val glyph = dir match
                        case SortDirection.Ascending  => Icons.sortAmountUpAlt
                        case SortDirection.Descending => Icons.sortAmountDown
                        case SortDirection.Unsorted   => Icons.sortAlt
                    List(GlyphSvg(glyph, "p-datatable-sort-icon"))
            val badge: List[UI] =
                if rank < 0 || sortingKs.length < 2 then Nil
                else List(Badge((rank + 1).toString).size(Size.Small).hostClass("p-datatable-sort-badge").render)
            UI.fragment((icon ++ badge)*)
        }

        val content: UI =
            div.cssClass("p-datatable-column-header-content")(
                ((span.cssClass("p-datatable-column-title")(c.headerV): UI) ::
                    sortMarks :: filterFunnel(path, c, filter, live)).map(toChild)*
            )
        val grip: List[UI] =
            if index < 0 then Nil
            else if resizeModeV == ColumnResizeMode.Expand then
                List(live.widths.render(w => UI.fragment(resizer(index, size.copy(widths = w))*)))
            else resizer(index, size)
        val built = cell((content :: grip).map(toChild)*)
        freezeLive(
            if index < 0 then built else reorderCell(built, path, index, index + 1, size, order, sortable, live),
            live.widths,
            _.at(index)
        )
    end headerCell

    /** Header click. Plain: sort by this column alone, advancing it when it is already
      * the only key so a plain click still cycles. Ctrl or Cmd: add the column, or
      * advance the one already in the spec IN PLACE, which is what lets an accidental
      * click be undone by the next one.
      */
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
        cells: Set[CellKey],
        exp: Set[String],
        openGroups: Set[GroupPath],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A],
        select: SelectState[A]
    )(using Frame): List[RowSpec[A]] =
        levels match
            case Nil           => leafRows(rows, sel, ctx, cells, exp, colCount, edit, nav, frozen, move, select)
            case level :: rest =>
                RowGroup.runs(rows)((a, _) => level.keyF(a)).flatMap { (key, run) =>
                    val groupPath = GroupPath(path :+ key)
                    val groupRows = run.map(_._1)
                    // A level without a header row has nowhere to put a toggle, so it can
                    // never be collapsed: collapsing it would hide its rows with no way
                    // back.
                    val collapsible = groupsOn && level.showHeaderFlag
                    val open        = !collapsible || openGroups.contains(groupPath)

                    val headerRow: List[RowSpec[A]] =
                        if !level.showHeaderFlag then Nil
                        else List(RowSpec.GroupHead(level, groupPath, groupRows, colCount, collapsible, open))
                    val innerRows: List[RowSpec[A]] =
                        if !open then Nil
                        else
                            groupSegments(
                                run,
                                rest,
                                groupPath.keys,
                                sel,
                                ctx,
                                cells,
                                exp,
                                openGroups,
                                colCount,
                                edit,
                                nav,
                                frozen,
                                move,
                                select
                            )
                    val footerRow: List[RowSpec[A]] =
                        if !open then Nil
                        else level.footerF.toList.map(f => RowSpec.GroupFoot(f, groupPath, groupRows, colCount))
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
        cells: Set[CellKey],
        exp: Set[String],
        colCount: Int,
        edit: EditState,
        nav: NavState[A],
        frozen: FrozenPlan,
        move: MoveState[A],
        select: SelectState[A]
    )(using Frame): List[RowSpec[A]] =
        val spans = spanCells(rows.map(_._1), exp)
        rows.zip(spans).flatMap { (row, spanned) =>
            val (a, index)       = row
            val id               = keyOf(a)
            val data: RowSpec[A] =
                RowSpec.Data(
                    a,
                    index,
                    sel.contains(id),
                    ctx.contains(id),
                    cells.collect { case c if c.row == id => c.column },
                    exp.contains(id),
                    colCount,
                    spanned,
                    edit,
                    nav,
                    frozen,
                    move,
                    select
                )
            // The expansion is its own entry, and only when there is one to draw: an entry that
            // rendered to nothing would break the one-element-per-entry rule the reuse rests on.
            if isExpanded(a, exp) then List(data, RowSpec.Expansion(a, colCount)) else List(data)
        }
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
            val (assigned, _)                                          = merged.foldLeft(start) { case ((acc, blocks), (col, same)) =>
                val split = blocks.flatMap(block => RowGroup.blocks(block)((x, y) => same(indexed(x), indexed(y))))
                val next  = split.foldLeft(acc) { (byRow, run) =>
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
                var b     = button
                    .cssClass("p-datatable-row-toggle-button")
                    .jsProp("type", "button")
                    .aria("expanded", open.toString)
                    .aria("label", if open then "Row Group Collapse" else "Row Group Expand")
                // Fixed open groups show their toggle and open or close nothing with it.
                if !groupsFixed then b = b.onClick(toggleGroup(path))
                List(b(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon"))))
        val content: UI = level.headerF match
            case Present(f) => f(path, rows)
            case Absent     => stringToUI(path.key)
        tr.cssClass("p-datatable-row-group-header")(
            toChild(td.colspan(math.max(colCount, 1))((toggle :+ content).map(toChild)*))
        )
    end groupHeaderRow

    private def toggleGroup(path: GroupPath)(using Frame): UI.Action =
        ReactiveValue.update(expandedGroupsSlot, cur => if cur.contains(path) then cur - path else cur + path, onExpandedGroupsF)

    /** One data row (plus its expansion row while expanded). */
    private def dataRow(spec: RowSpec.Data[A])(using Frame): UI =
        import spec.*
        val id = keyOf(a)
        // Qualified: `selected` and `expanded` are also setter names on the table itself.
        val isSel     = spec.selected
        val isExp     = spec.expanded
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
                var b     = button
                    .cssClass("p-datatable-row-toggle-button")
                    .jsProp("type", "button")
                    .aria("expanded", isExp.toString)
                    .aria("label", if isExp then "Row Collapse" else "Row Expand")
                // A fixed expansion shows its toggle and opens or closes nothing with it.
                if !expansionFixed then b = b.onClick(toggleExpand(id))
                val cell = td(b(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon"))))
                List(freeze(cell, frozen.leadAt(expanderSlot)))

        // Checkbox selection reuses Prime's checkbox anatomy (as Tree does).
        val checkboxTd: List[UI] =
            if !checkboxColumn then Nil
            else
                var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
                if isSel then cb = cb.cssClass("p-checkbox-checked")
                if canSelect && !selectionFixed then cb = cb.onClick(toggleSelect(id))
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
                val here     = CellKey(id, path)
                val cellEdit = edit.cell.exists(_ == here)
                val openable = c.isEditableAt(a)
                val editing  = openable && (rowEdit || cellEdit)
                val cellMode = editingCellTyped.isDefined && openable && edit.live
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
                    cell = cell.aria("selected", pickedCells.contains(path).toString)
                    if pickedCells.contains(path) then cell = cell.cssClass("p-uic-dt-cell-selected")
                    if canSelect && !cellsFixed then cell = cell.onClick(toggleCell(here)).stopPropagation(true)
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
                def btn(cls: String, glyph: IconGlyph, label: String, action: UI.Action): UI =
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
                val backToRow: UI.Action =
                    editableLeaves.headOption match
                        case Some((path, _)) => focusCell(nav, CellKey(id, path))
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
        if contextRow then row = row.cssClass("p-datatable-contextmenu-row-selected")
        if contextRowOn then row = row.onContextMenu(openRowContext(a, id))
        // After a drop the browser still owes a click, and it does not land on the grip the
        // press started on: the rows moved under the pointer. A row that would select takes
        // it and drops it, so the press cannot reach the next row the reader clicks.
        if rowInteractive then
            def act(e: MouseEvent): UI.Action =
                // The drag's own ref, not the paint's copy of it: after a drop the browser still
                // owes a click, and whether it is owed is a fact about now.
                heldNow(move).map(h =>
                    if h.exists(_.done) then clearRowMove(move) else activate(a, id, canSelect, e, select)
                )
            // Enter and Space are a click with no pointer behind them, so they carry no
            // modifiers: a keyboard activation is always the plain kind.
            val actPlain: UI.Action = act(MouseEvent(Absent, Modifiers.none))
            // A click that passed through a control of the reader's own is that control's, not the
            // row's: following a link in a cell, pressing the expander, starting a row edit. Without
            // this the row selects as well, and the control cannot decline the click for it: one
            // that navigates natively declares no kyo handler, so it has no `stopPropagation` to set
            // (and giving an anchor one costs it the middle click). The keyboard path below is
            // deliberately untouched: Enter on the ROW is the row's, whatever it contains.
            val clicked: MouseEvent => UI.Action = e => if e.onControl then () else act(e)
            // The row's tab stop is this table's, not a control's, so Enter and Space are too.
            // A row being edited overwrites this handler below, which is the order that belongs:
            // while an editor is open Enter commits the row rather than re-selecting it.
            row = row.tabIndex(0).onClick(clicked)
                .onKeyDown(e => if activationOf(e).isDefined then actPlain else ())
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
        row((handleTd ++ expanderTd ++ checkboxTd ++ dataTds ++ editorTd).map(toChild)*)
    end dataRow

    private def emptyRow(colCount: Int)(using Frame): UI =
        tr.cssClass("p-datatable-empty-message")(
            toChild(EmptyContent.render(emptyContentV, "No records found")(c => td.colspan(math.max(colCount, 1))(c)))
        )

    /** One spacer row, which is what holds the height of the rows that are not drawn.
      *
      * Prime's own, and the only shape that works: an empty `tr` collapses to nothing
      * in table layout, and `.p-datatable-virtualscroller-spacer` makes it a flex box,
      * which takes the height it is given. There is one of them ahead of the window
      * and one behind it, so the drawn rows sit at the scroll position they belong to
      * without any of them being positioned.
      */
    private def padRow(px: Int)(using Frame): Ast.Element =
        tr.cssClass("p-datatable-virtualscroller-spacer").style(_.height(math.max(0, px).px))

    /** A row of the window the source has not reached yet: one `Skeleton` per column,
      * so the grid stays whole and the geometry never moves once the rows land.
      */
    private def slotRow(spec: RowSpec.Slot[A])(using Frame): UI =
        // A line's worth of skeleton, which is what the cell would hold. In a window the row is as
        // tall as every other one because the row says so, not because the slot does; with no height
        // declared the skeleton is exactly one line (`1lh`), so the row is as tall as a row of text.
        val line                           = if spec.height.isDefined then "1rem" else "1lh"
        def slot(f: Maybe[FrozenSlot]): UI =
            freeze(td(toChild(Skeleton().height(line).render)), f)
        val leading  = (0 until spec.lead).toList.map(i => slot(spec.frozen.leadAt(i)))
        val cells    = (0 until spec.cells).toList.map(i => slot(spec.frozen.at(i)))
        val trailing = (0 until spec.trail).toList.map(i => slot(spec.frozen.trailAt(i)))
        val row      = spec.height.fold(tr)(h => tr.style(_.height(h.px)))
        row((leading ++ cells ++ trailing).map(toChild)*)
    end slotRow

    /** A row standing in for a row on its way. Under [[loadingRow]] it holds the caller's row and
      * nothing else. Under [[loadingContent]] it holds one invisible line, so it is as tall as a row of
      * text, and the first of its run carries the content laid over the whole run
      * (`.p-uic-datatable-loading-content`, as tall as that many of these rows).
      */
    private def loadingRow(spec: RowSpec.Loading[A])(using Frame): UI =
        val row  = spec.height.fold(tr)(h => tr.style(_.height(h.px))).cssClass("p-uic-datatable-loading-row")
        val cell = td.colspan(math.max(spec.colCount, 1))
        loadingContentV match
            case Present(LoadingContent.Rows(f)) => row(toChild(cell(toChild(f(spec.at, spec.run)))))
            case content                         =>
                // One invisible line makes the row as tall as a row of text; the content lies over the run.
                val line: UI       = span.cssClass("p-uic-datatable-loading-line").aria("hidden", "true")("\u00a0")
                val over: List[UI] =
                    if spec.at != 0 then Nil
                    else
                        content.toList.flatMap(c =>
                            LoadingContent.over(c, spec.run)(inner =>
                                div.cssClass("p-uic-datatable-loading-content")
                                    .style(_.height(Length.Calc(s"${spec.run} * 100%")))(inner)
                            ).toList
                        )
                row.cssClass("p-uic-datatable-loading-over")(toChild(cell((line :: over).map(toChild)*)))
        end match
    end loadingRow

    /** The `<tr>` an expanded row emits under itself.
      *
      * An `Expansion` entry only exists where there is content to draw (`isExpanded` requires the
      * template), so this cannot be asked for one that has none.
      */
    private def expansionRow(spec: RowSpec.Expansion[A])(using Frame): UI =
        expansionF match
            case Present(f) =>
                tr.cssClass("p-datatable-row-expansion")(
                    td.colspan(math.max(spec.colCount, 1))(toChild(f(spec.a)))
                )
            case Absent => tr.cssClass("p-datatable-row-expansion")(td.colspan(math.max(spec.colCount, 1)))

    /** What identifies one entry of the `<tr>` stream across emissions.
      *
      * The prefixes keep the namespaces apart: a data row and its expansion are two entries about
      * the same record, and a group path and a row key are both strings a caller chose. Two entries
      * of one emission sharing a key would make a keyed region rebuild the whole list, so the
      * prefixes are not cosmetic.
      */
    private[uic] def rowSpecKey(spec: RowSpec[A]): String =
        spec match
            case d: RowSpec.Data[A]      => s"r:${keyOf(d.a)}"
            case e: RowSpec.Expansion[A] => s"x:${keyOf(e.a)}"
            case g: RowSpec.GroupHead[A] => s"gh:${g.path.keys.mkString(" ")}"
            case g: RowSpec.GroupFoot[A] => s"gf:${g.path.keys.mkString(" ")}"
            // The leading spacer is the one that states where the window starts; the trailing one
            // never does, which is what tells them apart without carrying a flag for it.
            case s: RowSpec.Spacer[A]  => if s.marks.isDefined then "sp:top" else "sp:bottom"
            case s: RowSpec.Slot[A]    => s"slot:${s.index}"
            case l: RowSpec.Loading[A] => s"loading:${l.index}"
            case _: RowSpec.Empty[A]   => "empty"
        end match
    end rowSpecKey

    /** One entry of the `<tr>` stream, painted. Exactly one element per entry, which is the
      * condition a keyed list region reuses rows under; `Expansion` is the one that could decline,
      * and it declines by never becoming an entry (see `leafRows`).
      */
    private[uic] def renderTr(spec: RowSpec[A])(using Frame): UI =
        spec match
            case d: RowSpec.Data[A]      => dataRow(d)
            case e: RowSpec.Expansion[A] => expansionRow(e)
            case g: RowSpec.GroupHead[A] =>
                groupHeaderRow(g.level, g.path, g.rows, g.colCount, g.collapsible, g.open)
            case g: RowSpec.GroupFoot[A] =>
                tr.cssClass("p-datatable-row-group-footer")(
                    toChild(td.colspan(math.max(g.colCount, 1))(toChild(g.render(g.path, g.rows))))
                )
            case s: RowSpec.Spacer[A] =>
                var pad = padRow(s.px)
                s.marks.foreach { (first, count) =>
                    pad = pad.data("uic-vs-first", first.toString).data("uic-vs-count", count.toString)
                }
                pad
            case s: RowSpec.Slot[A]    => slotRow(s)
            case l: RowSpec.Loading[A] => loadingRow(l)
            case e: RowSpec.Empty[A]   => emptyRow(e.colCount)
        end match
    end renderTr

    /** A row toggle, over rows rather than keys: a two-way binding is stepped atomically, a one-way one from the rows it
      * shows, and [[onExpandedChange]] hears the rows open after it either way.
      */
    private def toggleExpand(id: String)(using Frame): UI.Action =
        val step: Set[String] => Set[String] = cur => if cur.contains(id) then cur - id else cur + id
        currentRows.map { all =>
            ReactiveValue.update(expandedTyped, prev => rowsFor(step(keysOf(prev)), prev, all), onExpandedF)
        }
    end toggleExpand

    /** A click on a cell: replace the set with it, or toggle it in, by the mode. */
    private def toggleCell(cell: CellKey)(using Frame): UI.Action =
        val step: Set[CellKey] => Set[CellKey] =
            if selectionModeV == SelectionMode.Single then cur => if cur == Set(cell) then Set.empty else Set(cell)
            else cur => if cur.contains(cell) then cur - cell else cur + cell
        currentRows.map(all =>
            ReactiveValue.update(cellsTyped, prev => cellsFor(step(prev.map(cellKey)), prev, all), onCellsF)
        )
    end toggleCell

    /** A right-click over a row: remember which one, then tell the caller what it was aimed at.
      *
      * The selection is read HERE rather than taken from the render, because the row's handler is
      * a closure the render left behind: reading it now is reading what is selected at the moment
      * of the click. The keys are resolved against [[currentRows]], the table's full set, so a
      * selected row the reader has since filtered off the screen is still part of what the menu
      * is being asked about.
      */
    private def openRowContext(a: A, id: String)(using Frame): UI.Action =
        val write: UI.Action = contextTyped.flatMap(_.writable).fold[UI.Action](())(_.set(Present(a)))
        val fire: UI.Action  = onRowContextF match
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

    /** Writes a new selection wherever it can go: into the bound ref if there is one to write,
      * and to [[onSelectionChange]] either way.
      *
      * Both, not one or the other: a caller may bind a `SignalRef` and still want to hear about
      * the change, and a caller with a one-way binding has nothing BUT the callback. The ref is
      * written first so the two see the same value in the same order.
      */
    private def applySelection(next: Set[String])(using Frame): UI.Action =
        for
            all  <- currentRows
            held <- currentValue(selectedTyped, Set.empty[A])
            rows = rowsFor(next, held, all)
            _ <- selectedRef match
                case Present(ref) => ref.set(rows)
                case Absent       => (): UI.Action
            _ <- onSelectionF match
                case Present(f) => f(rows)
                case Absent     => (): UI.Action
        yield ()
    end applySelection

    private def toggleSelect(id: String)(using Frame): UI.Action =
        currentValue(selectedBinding, Set.empty[String]).map(cur =>
            applySelection(if cur.contains(id) then cur - id else cur + id)
        )

    /** What one open cell shows: the column's editor over the table's draft, and under it
      * the message of a commit this cell refused.
      *
      * The editor binds the draft ref rather than a rendered value, so a keystroke
      * re-renders this cell and nothing above it, and an unrelated re-render (a push into
      * the bound row list, a sort) finds the text still in the ref rather than resetting
      * the field to the row's stored value.
      */
    private def editorCell(cell: CellKey, row: A, c: Column[A, FlatOnly], nav: NavState[A], edit: EditState)(
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
                    case Absent     => ed.editor(params)
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
                editingCellTyped.isDefined && ((rowAt(nav, pos), colAt(pos)) match
                    case (Present(r), Present((_, c))) => c.isNavigable && c.isEditableAt(r)
                    case _                             => false),
            page = math.max(nav.page, 1),
            navigable = pos => colAt(pos).exists((_, c) => c.isNavigable)
        )

    /** Opens a cell on a seed: a printable key that started the edit, or the row's own
      * value when the reader pressed Enter or F2.
      */
    private def openCell(cell: CellKey, row: A, c: Column[A, FlatOnly], seed: Maybe[String], edit: EditState)(
        using Frame
    ): UI.Action =
        val fill: UI.Action = (seed, edit.draftOf(cell.column)) match
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
    private def openAt(pos: GridNav.Pos, nav: NavState[A], edit: EditState)(using Frame): UI.Action =
        (rowAt(nav, pos), colAt(pos)) match
            case (Present(r), Present((path, c))) => openCell(CellKey(keyOf(r), path), r, c, Absent, edit)
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
    private def editorId(nav: NavState[A], cell: CellKey): Maybe[String] =
        val col = leafPaths.indexWhere(_._1 == cell.column)
        (nav.indexOf(cell.row, keyOf), col) match
            case (Present(row), c) if c >= 0 => Present(s"${nav.idPrefix}-e$row-$c")
            case _                           => Absent
    end editorId

    /** Puts focus back INTO an open editor, which is where a refused commit leaves it. */
    private def focusEditor(nav: NavState[A], cell: CellKey)(using Frame): UI.Action =
        editorId(nav, cell) match
            case Present(id) => nav.focus(id)
            case Absent      => ()

    private def focusCell(nav: NavState[A], cell: CellKey)(using Frame): UI.Action =
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
        cell: CellKey,
        row: A,
        c: Column[A, FlatOnly],
        cellEdit: Boolean,
        rowEdit: Boolean,
        nav: NavState[A],
        edit: EditState
    )(e: KeyboardEvent)(using Frame): UI.Action =
        // The grid is read against the rows on the screen NOW: PageDown steps by a page height and
        // Down stops at the last row, and both are answers about a list this cell no longer owns.
        navNow(nav).map(live => onCellKeyAt(pos, cell, row, c, cellEdit, rowEdit, live, edit)(e))

    private def onCellKeyAt(
        pos: GridNav.Pos,
        cell: CellKey,
        row: A,
        c: Column[A, FlatOnly],
        cellEdit: Boolean,
        rowEdit: Boolean,
        nav: NavState[A],
        edit: EditState
    )(e: KeyboardEvent)(using Frame): UI.Action =
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
                def closing(closed: Boolean < (Async & Abort[Any])): Boolean < (Async & Abort[Any]) =
                    closed.map { ok =>
                        val focus: UI.Action =
                            if ok && !isTab then focusCell(nav, cell)
                            else if !ok && isTab then focusEditor(nav, cell)
                            else ()
                        focus.andThen(ok)
                    }
                val editStep: Boolean < (Async & Abort[Any]) = step.edit match
                    case GridNav.EditOp.Keep       => true
                    case GridNav.EditOp.Open(seed) => openCell(cell, row, c, seed, edit).andThen(true)
                    case GridNav.EditOp.Commit     =>
                        if rowEdit then (if isTab then Kyo.lift(true) else closing(commitRow(cell.row, row, edit)))
                        else closing(commitCell(cell, row, c, edit))
                    case GridNav.EditOp.Cancel =>
                        if rowEdit then closing(cancelRow(cell.row, edit).andThen(true))
                        else closing(cancelCell(edit).andThen(true))
                val focusStep: UI.Action = step.focus match
                    case Present(to) if step.moveFocus => nav.focus(cellId(nav, to))
                    case Present(to)                   => if rowEdit then () else openAt(to, nav, edit)
                    case Absent                        => ()
                val selectStep: UI.Action =
                    // The grid's own key step, which is a selection with no pointer and no
                    // anchor behind it: the plain kind, whatever the modifier setting is.
                    if step.selectRow && rowInteractive then
                        activate(row, cell.row, selectableAt(row), MouseEvent(Absent, Modifiers.none), SelectState[A]())
                    else ()
                for
                    // A refused commit stops the step that would move on: opening the next
                    // cell clears the error, so moving anyway left the refusal invisible
                    // and the edit gone.
                    ok <- editStep
                    _  <- (if ok then focusStep else ()): UI.Action
                    r  <- selectStep
                yield r
                end for
    end onCellKeyAt

    /** Puts a row into the editing set, or takes it out. */
    private def setRowEditing(id: String, on: Boolean)(using Frame): UI.Action =
        editingRowsTyped match
            case Present(ref) => updateRows(ref)(cur => if on then cur + id else cur - id)
            case Absent       => ()

    /** Moves the cell editor, or clears it. */
    private def setEditingCell(cell: Maybe[CellKey])(using Frame): UI.Action =
        editingCellTyped match
            case Present(ref) =>
                currentRows.map(all =>
                    ref.getAndUpdate(prev => Maybe.fromOption(cellsFor(cell.toList.toSet, prev.toList.toSet, all).headOption)).unit
                )
            case Absent => ()

    /** Fills a column's draft with what the row currently shows there, which is what an
      * editor opens on. The seed is the table's job precisely because the editor renders
      * in a pure position and cannot write into anything.
      */
    private def seedDraft(row: A, path: List[String], c: Column[A, FlatOnly], edit: EditState)(using Frame): UI.Action =
        (c.editV, edit.draftOf(path)) match
            case (Present(ed), Present(ref)) => ref.set(ed.show(row))
            case _                           => ()

    /** Seeds every editable column of one row: row editing opens them all at once. */
    private def seedRowDrafts(row: A, edit: EditState)(using Frame): UI.Action =
        Kyo.foreachDiscard(editableLeaves)((path, c) => seedDraft(row, path, c, edit))

    private def clearError(edit: EditState)(using Frame): UI.Action =
        edit.errorRef match
            case Present(ref) => ref.set(Absent)
            case Absent       => ()

    private def setError(cell: CellKey, e: FieldError, edit: EditState)(using Frame): UI.Action =
        edit.errorRef match
            case Present(ref) => ref.set(Present((cell, e)))
            case Absent       => ()

    /** Opens a cell: seed its draft from the row, drop any standing error, then move the
      * editor. Seeding first is what makes the editor's first paint the row's own value.
      */
    private def beginCellEditing(
        cell: CellKey,
        row: A,
        c: Column[A, FlatOnly],
        nav: NavState[A],
        edit: EditState
    )(using Frame): UI.Action =
        val move: UI.Action =
            for
                _ <- seedDraft(row, cell.column, c, edit)
                _ <- clearError(edit)
                r <- setEditingCell(Present(cell))
            yield r
        // Moving the editor is a way of leaving one, so it commits like every other: a
        // click from cell to cell used to open the next and drop what was typed into the
        // last, without asking anything and without saying so. A refusal keeps the reader
        // in the value they are fixing, the same as Enter and Tab do.
        // A click inside the cell that is already open (a number field's spinner, a click that places the caret) is no
        // way of opening it: seeding it again from the row threw away what the reader had typed or stepped.
        openCellNow(edit).map {
            case Present(open) if open == cell => (): UI.Action
            case Present(open)                 =>
                commitOpen(open, nav, edit).map { ok =>
                    val next: UI.Action = if ok then move else ()
                    next
                }
            case Absent => move
        }
    end beginCellEditing

    /** Commits whatever cell is open, wherever the gesture that ends it came from.
      *
      * The open cell is a path, so its row is found among the rendered ones by key and its
      * column among the leaves. A path naming neither is a table that has changed under an
      * open editor, and there is nothing left to commit.
      */
    private def commitOpen(open: CellKey, nav: NavState[A], edit: EditState)(using Frame): Boolean < (Async & Abort[Any]) =
        // Read live, because failing to find the row here does not report anything: it answers
        // "committed" and DROPS what the reader typed. A handler holding the rows as they were
        // would do that to any row that moved under an open editor.
        navNow(nav).map { now =>
            (now.rows.find(r => keyOf(r) == open.row), leafPaths.find(_._1 == open.column)) match
                case (Some(r), Some((_, c))) => commitCell(open, r, c, edit)
                case _                       => true
        }

    /** Opens a row: every editable column's draft is seeded before the row joins the set. */
    private def beginRowEditing(id: String, row: A, edit: EditState)(using Frame): UI.Action =
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
    private def applyCell(cell: CellKey, before: A, after: A, ed: CellEdit[A])(using Frame): UI.Action =
        val store: UI.Action = rowsRefV match
            case Present(ref) => ref.getAndUpdate(_.map(r => if keyOf(r) == cell.row then after else r))
            case Absent       => ()
        val column: UI.Action = ed.changed match
            case Present(f) => f(before, after)
            case Absent     => ()
        val table: UI.Action = onCellChangedF match
            case Present(f) => f(CellChange(cell.column, before, after))
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
    private def commitCell(cell: CellKey, row: A, c: Column[A, FlatOnly], edit: EditState)(using
        Frame
    ): Boolean < (Async & Abort[Any]) =
        (c.editV, edit.draftOf(cell.column)) match
            case (Present(ed), Present(ref)) =>
                for
                    text <- ref.get
                    res  <- ed.commit(row, text)
                    out  <- res match
                        case Result.Success(after) =>
                            val moved: UI.Action = if ed.show(row) == text then () else applyCell(cell, row, after, ed)
                            for
                                _ <- moved
                                _ <- clearError(edit)
                                _ <- setEditingCell(Absent)
                            yield true
                            end for
                        case Result.Failure(e) => setError(cell, e, edit).andThen(false)
                        case Result.Panic(ex)  =>
                            setError(cell, FieldError("error", Map.empty, Maybe(ex.getMessage)), edit).andThen(false)
                yield out
            case _ => setEditingCell(Absent).andThen(true)

    /** Cancels a cell: the draft is dropped by the next seed, so there is nothing to undo. */
    private def cancelCell(edit: EditState)(using Frame): UI.Action =
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
    private def commitRow(id: String, row: A, edit: EditState)(using Frame): Boolean < (Async & Abort[Any]) =
        def loop(rest: List[(List[String], Column[A, FlatOnly])], acc: A): Result[(CellKey, FieldError), A] < (Async & Abort[Any]) =
            rest match
                case Nil               => Kyo.lift(Result.succeed(acc))
                case (path, c) :: tail =>
                    val cell = CellKey(id, path)
                    (c.editV, edit.draftOf(path)) match
                        case (Present(ed), Present(ref)) =>
                            for
                                text <- ref.get
                                res  <- ed.commit(acc, text)
                                out  <- res match
                                    case Result.Success(next) =>
                                        val moved: UI.Action =
                                            if ed.show(acc) == text then () else applyCell(cell, acc, next, ed)
                                        moved.andThen(loop(tail, next))
                                    case Result.Failure(e) => Kyo.lift(Result.fail((cell, e)))
                                    case Result.Panic(ex)  =>
                                        Kyo.lift(Result.fail((cell, FieldError("error", Map.empty, Maybe(ex.getMessage)))))
                            yield out
                        case _ => loop(tail, acc)
                    end match
        for
            res <- loop(editableLeaves.filter((_, c) => c.editV.exists(_.when(row))), row)
            out <- res match
                case Result.Success(after) =>
                    val fire: UI.Action = onRowChangedF match
                        case Present(f) => f(RowChange(row, after))
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
    private def cancelRow(id: String, edit: EditState)(using Frame): UI.Action =
        for
            _ <- clearError(edit)
            r <- setRowEditing(id, false)
        yield r

    /** Clicking a row updates the bound selection set (per the mode), then fires `onRowClick`.
      *
      * Two rulesets, and which one applies is [[metaKeySelection]]. Without it every click
      * toggles its row and the modifiers mean nothing. With it the row reads the way a file
      * manager's does, and the shape below is Prime's `onRowClick`, kept deliberately close to
      * it: shift ranges when there is an anchor to range from, otherwise the anchor moves to
      * this row and Ctrl/Cmd decides between toggling and replacing.
      *
      * `onRowClick` fires either way and regardless of whether anything was written, which is
      * what lets a caller who owns the selection elsewhere (a one-way [[selection]]) close the
      * loop themselves.
      */
    private def activate(row: A, id: String, canSelect: Boolean, e: MouseEvent, select: SelectState[A])(using
        Frame
    ): UI.Action =
        // Ctrl OR Cmd. Prime tests `metaKey || ctrlKey` and so does every desktop convention;
        // pinning this to `meta` alone locks out everyone not on a Mac.
        val meta = e.modifiers.meta || e.modifiers.ctrl

        def pick: UI.Action =
            // A two-way anchor is written and the move reported; a one-way or constant one is only reported, so a constant
            // nobody hears stays where it was.
            def anchorTo(v: Set[String]): UI.Action =
                if select.anchor.isEmpty then applySelection(v)
                else ReactiveValue.change(select.anchor, Present(row), select.onAnchor).andThen(applySelection(v))
            (select.anchor, e.modifiers.shift, selectionModeV) match
                // A range needs somewhere to start. Without an anchor the shift-click is just the
                // first pick, which is what sets one.
                case (Present(a), true, SelectionMode.Multiple) if metaKeyFlag =>
                    a.current.map {
                        // The list is read HERE, not where this handler was built: a row kept across
                        // an emission keeps its handler, and a range spanned over the list as it was
                        // would silently degrade to a single pick the moment anything was appended.
                        case Present(from) =>
                            bodyViewNow.map(v => applySelection(DataTable.between(v.selectKeys, keyOf(from), id)))
                        case Absent => anchorTo(Set(id))
                    }
                case _ =>
                    currentValue(selectedBinding, Set.empty[String])
                        .map(cur => anchorTo(SelectionPick.next(selectionModeV, metaKeyFlag, meta, id, cur)))
            end match
        end pick

        val setSelection: UI.Action =
            if !canSelect || !rowClickSelects || selectedTyped.isEmpty then () else pick
        val fireClick: UI.Action = onRowClickF match
            case Present(f) => f(row)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end activate
end DataTable

object DataTable:

    /** A table's own state for one render, one value per concern, as the caller reads it. */
    final private[uic] case class Wiring[A](
        edit: EditState,
        nav: NavState[A],
        filter: FilterState,
        size: SizeState,
        order: OrderState,
        scroll: ScrollState,
        move: MoveState[A],
        select: SelectState[A] = SelectState[A]()
    )

    /** What one pass over the table's state draws, each part built only when asked for: the parts that follow the state
      * are regions of their own over it (see [[renderWith]]), and each region asks for its part alone.
      */
    final private[uic] class Drawn[A](
        cards0: => UI,
        colgroup0: => UI,
        foot0: => UI,
        selectAll0: => UI,
        staticSpecs0: => List[RowSpec[A]],
        val specsFor: RowInputs[A] => List[RowSpec[A]],
        staticFrozenSpecs0: => List[RowSpec[A]],
        val frozenSpecsFor: RowInputs[A] => List[RowSpec[A]],
        val windowSpecs: Double => List[RowSpec[A]],
        windowHeight0: => Int
    ):
        lazy val cards: UI     = cards0
        lazy val colgroup: UI  = colgroup0
        lazy val foot: UI      = foot0
        lazy val selectAll: UI = selectAll0
        // The frozen rows of the bindings read with the state, where nothing drives them from below.
        lazy val staticFrozenSpecs: List[RowSpec[A]] = staticFrozenSpecs0
        // How tall a windowed body is: the whole list's height, which the spacers add up to.
        lazy val windowHeight: Int = windowHeight0
        // The rows of the bindings read with the state, where nothing drives them from below.
        lazy val staticSpecs: List[RowSpec[A]] = staticSpecs0
    end Drawn

    def apply[A](): DataTable[A] = new DataTable[A]()

    /** The keys from `from` to `to` inclusive, in either direction: what a shift-click picks.
      *
      * `keys` is the reader's own order, so a range is what they see between the two rows and
      * not what the underlying list happens to hold. A key that is not there (the anchor row was
      * filtered away, or is on a page this table no longer holds) leaves the click meaning the
      * one row it landed on, which is the same thing an anchorless shift-click means.
      */
    private[uic] def between(keys: IndexedSeq[String], from: String, to: String): Set[String] =
        val a = keys.indexOf(from)
        val b = keys.indexOf(to)
        if a < 0 || b < 0 then Set(to)
        else keys.slice(math.min(a, b), math.max(a, b) + 1).toSet
    end between

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
