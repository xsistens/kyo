package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.uic.form.FieldError
import kyo.uic.form.Validator
import scala.annotation.implicitNotFound
import scala.annotation.tailrec

/** Horizontal alignment of one [[Column]] (applied to the header cell and every
  * body cell of the column via `.p-uic-dt-center` / `.p-uic-dt-end`).
  */
enum ColumnAlign derives CanEqual:
    case Start, Center, End

/** Which edge of the table a frozen [[Column]] holds on to while the rest scrolls past
  * it, Prime's `alignFrozen`.
  *
  * `Start` and `End` and not left and right, because that is what the rest of the
  * component says: an [[ColumnAlign]] of `End` puts a number against the trailing edge
  * whichever way the text runs.
  */
enum FrozenEdge derives CanEqual:
    case Start, End

/** What a resize drag moves, Prime's `columnResizeMode`.
  *
  * `Fit` moves a BOUNDARY: the pair it sits between trades width and the total is
  * preserved, so the table never outgrows the space it was given and the last column,
  * having no boundary to its right, cannot be dragged. `Expand` moves one COLUMN: only
  * the grabbed one changes and the table grows or shrinks by the same amount, so every
  * resizable column can be dragged and the table scrolls sideways in its container.
  */
enum ColumnResizeMode derives CanEqual:
    case Fit, Expand

/** Which tables accept a [[Column]], carried as the column's second, phantom type
  * argument so a table can refuse one it could not honor.
  *
  * `Column` is shared by [[DataTable]] and [[TreeTable]], and several of its options mean
  * nothing over a hierarchy: [[Column.footer]] fills a `tfoot` a TreeTable does not
  * render, [[Column.rowSpan]] merges runs of equal cells, which consecutive rows at
  * different depths do not form, [[Column.editable]] is read by an editing state a
  * TreeTable does not carry, [[Column.filterBy]] fills a filter row it does not render,
  * and [[Column.frozen]] holds a column against the edge of a scroll container it does
  * not put its table in. Every one of those setters returns a `FlatOnly` column, which
  * `TreeTable.columns` will not take.
  *
  * `AnyTable` extends `FlatOnly` because the subtyping runs that way round: a column
  * every table accepts is in particular one a flat table accepts. With `Column`
  * covariant in the parameter, that is what lets a mixed list splat into a DataTable.
  */
sealed trait FlatOnly

/** A column carrying only options every table honors. See [[FlatOnly]]. */
sealed trait AnyTable extends FlatOnly

/** A column that has a text projection AND carries a flat-table-only option. */
sealed trait TextFlatOnly extends FlatOnly

/** A column that has a text projection and carries no flat-table-only option, which is
  * what the text constructor returns and what [[Column.rowSpan]] needs to merge by.
  */
sealed trait TextAnyTable extends AnyTable, TextFlatOnly

/** A column carrying a typed cell-edit pipeline, which is what [[Column.editableWhen]]
  * and [[Column.onValueChanged]] refine. Those two say something about an edit, so a
  * column with no edit to speak of does not compile rather than reporting a card at
  * render time.
  */
sealed trait EditableFlatOnly extends FlatOnly

/** Evidence that a column is editable. See [[EditableFlatOnly]]. */
@implicitNotFound(
    "editableWhen and onValueChanged refine an edit, and this column has none. Call editable(read)(write) " +
        "(or editableAs) first, and call it before footer, rowSpan, filterBy or frozen, which reset the kind."
)
type IsEditable[K] = K <:< EditableFlatOnly

/** Evidence that a column has a text projection, which is what the argument-less
  * [[Column.rowSpan]] merges by. The kind carries the fact, so a column built without one
  * cannot reach that overload.
  */
@implicitNotFound(
    "rowSpan without a key merges by this column's text projection, and it has none. Give it a key, " +
        "rowSpan(_.field), or call rowSpan before footer, which drops the projection from the kind."
)
type HasText[K] = K <:< TextFlatOnly

/** Evidence that a column kind is one every table takes, which is what
  * `TreeTable.columns` asks for. Only `AnyTable` has an instance.
  */
@implicitNotFound(
    "A TreeTable takes plain columns only. It renders one header row, so a headerGroup has no place in it, and it " +
        "renders no tfoot, no merged runs, no editing state, no filter row and no scroll container, so a column " +
        "carrying footer, rowSpan, editable, filterBy or frozen does not fit either: a footer needs the tfoot, " +
        "rowSpan merges runs of equal cells, which rows at different depths do not form, an edit pipeline is read " +
        "by an editing state this table does not bind, filtering a hierarchy is a different question, since a row " +
        "that matches has to keep its parents, and a frozen column needs something to be frozen against."
)
sealed trait AnyTableColumn[-K <: FlatOnly]:
    /** Hands back what the evidence already proves: a `K` column is an `AnyTable` one.
      * Carrying the coercion here is what lets `TreeTable` store its columns at their
      * true kind without a cast.
      */
    private[uic] def widen[A](c: Column[A, K]): Column[A, AnyTable]
end AnyTableColumn

object AnyTableColumn:
    given AnyTableColumn[AnyTable] with
        private[uic] def widen[A](c: Column[A, AnyTable]): Column[A, AnyTable] = c

/** Either one [[Column]] or a [[HeaderGroup]] spanning several: what
  * `DataTable.columns` takes, and what gives a table a header of more than one row.
  *
  * The spans are DERIVED from this shape, never written down. Prime's ColumnGroup has
  * the caller author header cells with their own `colSpan` and `rowSpan` BESIDE the
  * column list rather than over it, so a column added to one and not to the other
  * renders a header and a body that disagree, silently and at every row. Here the
  * leaves ARE the columns: a group is as wide as the leaves under it and as tall as one
  * row, a column reaches from its own level to the bottom, and there is no second list
  * to keep in step.
  */
sealed trait ColumnTree[A]:
    /** The label of the header cell this node renders. */
    private[uic] def label: String

    /** The columns under this node, left to right; for a column, itself. */
    private[uic] def leaves: List[Column[A, FlatOnly]]

    /** The nodes one level down, empty for a column. */
    private[uic] def children: List[ColumnTree[A]]

    /** Header rows this node needs on its own: 1 for a column, one more than its
      * tallest child for a group.
      */
    private[uic] def height: Int

    /** How many header rows this node's cell spans, sitting at `row` of `depth`. A
      * group takes one row and hands the rest to its children; a column takes every row
      * left below it, which is what lines an ungrouped column up with a grouped one.
      */
    private[uic] def rowspan(depth: Int, row: Int): Int

    /** The column this node is, if it is one. Carrying the answer here keeps the header
      * renderer off a type test the erasure cannot check.
      */
    private[uic] def asColumn: Maybe[Column[A, FlatOnly]]

    /** This node with its children replaced, which only a group has: the same reason
      * [[asColumn]] lives here, so [[ColumnTree.prune]] rebuilds a narrowed group without
      * a type test.
      */
    private[uic] def withChildren(cs: List[ColumnTree[A]]): ColumnTree[A]
end ColumnTree

object ColumnTree:
    /** One header cell: the node it renders, its path from the outermost group down, and
      * the two spans read off the tree.
      */
    final private[uic] case class HeaderSpan[A](
        node: ColumnTree[A],
        path: List[String],
        colspan: Int,
        rowspan: Int,
        at: Int
    )

    /** The header cells, one list per header row, top row first.
      *
      * A node with no leaves under it occupies nothing and is dropped, which is what an
      * empty group amounts to; [[DataTable]] names those in a diagnostic card rather
      * than leaving the omission silent.
      */
    private[uic] def spans[A](nodes: List[ColumnTree[A]]): List[List[HeaderSpan[A]]] =
        val depth = nodes.map(_.height).maxOption.getOrElse(0)
        // The leaf offset is carried DOWN from the parent and not counted along each row,
        // because the rows do not tile the same columns: a column beside a group reaches
        // down through the header and appears in the first row alone, so a row that
        // counted its own cells from zero would put everything under it over the wrong
        // column.
        def spread(ns: List[ColumnTree[A]], prefix: List[String], at: Int): List[(ColumnTree[A], List[String], Int)] =
            ns.foldLeft((at, List.empty[(ColumnTree[A], List[String], Int)])) { case ((off, acc), n) =>
                (off + n.leaves.size, acc :+ (n, prefix, off))
            }._2
        @tailrec def loop(
            level: List[(ColumnTree[A], List[String], Int)],
            row: Int,
            acc: List[List[HeaderSpan[A]]]
        ): List[List[HeaderSpan[A]]] =
            val live = level.filter(_._1.leaves.nonEmpty)
            if live.isEmpty then acc.reverse
            else
                val cells =
                    live.map((n, prefix, at) => HeaderSpan(n, prefix :+ n.label, n.leaves.size, n.rowspan(depth, row), at))
                val next = live.flatMap((n, prefix, at) => spread(n.children, prefix :+ n.label, at))
                loop(next, row + 1, cells :: acc)
            end if
        end loop
        loop(spread(nodes, Nil, 0), 0, Nil)
    end spans

    /** Every column with the path that identifies it: the labels of the groups it sits
      * in, outermost first, then its own header. This is the currency of the sort spec,
      * which is why it is a list of parts rather than one joined string.
      */
    private[uic] def leafPaths[A](nodes: List[ColumnTree[A]]): List[(List[String], Column[A, FlatOnly])] =
        def walk(ns: List[ColumnTree[A]], prefix: List[String]): List[(List[String], Column[A, FlatOnly])] =
            ns.flatMap { n =>
                val path = prefix :+ n.label
                n.asColumn match
                    case Present(c) => List((path, c))
                    case Absent     => walk(n.children, path)
            }
        walk(nodes, Nil)
    end leafPaths

    /** The labels of the groups that hold no column, outermost only: a group inside one
      * of those is already covered by its parent.
      */
    private[uic] def emptyGroups[A](nodes: List[ColumnTree[A]]): List[String] =
        nodes.flatMap(n => if n.leaves.isEmpty then List(n.label) else emptyGroups(n.children))

    /** The tree without the columns `keep` rejects, addressed by their position among the
      * leaves in [[leafPaths]] order. A group that loses every column it had goes with
      * them, since a header cell over nothing would span nothing; a group authored with
      * no columns at all stays, so the diagnostic that names it still fires.
      */
    /** These nodes with every level's children put in the order `order` names them in,
      * where a node's place is the place of its FIRST leaf.
      *
      * The order is a flat list of leaf paths, which is what makes it the same currency
      * the sort spec, the column filters and the widths already speak, and what lets a
      * reader's reorder be one list rather than a shape mirroring the header tree. A leaf
      * the order does not name keeps its authored place behind the ones it does, so a
      * caller can say "Price first" without writing every column down.
      *
      * Reading a group's place off its first leaf is what keeps the header a tree: an
      * order that splits a group cannot be rendered, since one header cell cannot sit in
      * two places, so the group's columns come back together where its first leaf asks
      * for. [[DataTable]] names that in a card rather than leaving it silent.
      */
    private[uic] def reorder[A](nodes: List[ColumnTree[A]], order: List[List[String]]): List[ColumnTree[A]] =
        if order.isEmpty then nodes
        else
            val rank = order.zipWithIndex.toMap
            def walk(ns: List[ColumnTree[A]], prefix: List[String]): List[ColumnTree[A]] =
                val placed = ns.zipWithIndex.map { (n, authored) =>
                    val named = leafPaths(List(n)).flatMap(e => rank.get(prefix ++ e._1))
                    val moved = n.asColumn match
                        case Present(_) => n
                        case Absent     => n.withChildren(walk(n.children, prefix :+ n.label))
                    (moved, named.minOption, authored)
                }
                // Named nodes first, in the order they were named; the rest hold their
                // authored places behind them, which is what makes a partial order mean
                // "these first" rather than "everything else is undefined".
                val (named, rest) = placed.partition(_._2.isDefined)
                named.sortBy(_._2.getOrElse(0)).map(_._1) ++ rest.sortBy(_._3).map(_._1)
            end walk
            walk(nodes, Nil)
        end if
    end reorder

    private[uic] def prune[A](nodes: List[ColumnTree[A]], keep: Int => Boolean): List[ColumnTree[A]] =
        def walk(ns: List[ColumnTree[A]], next: Int): (List[ColumnTree[A]], Int) =
            ns.foldLeft((List.empty[ColumnTree[A]], next)) { case ((acc, i), n) =>
                n.asColumn match
                    case Present(_) => (if keep(i) then acc :+ n else acc, i + 1)
                    case Absent =>
                        val (kept, after) = walk(n.children, i)
                        val group         = n.withChildren(kept)
                        (if kept.nonEmpty || n.children.isEmpty then acc :+ group else acc, after)
            }
        walk(nodes, 0)._1
    end prune
end ColumnTree

/** One column of a [[DataTable]], a typed, hand-authored carrier: a `header`
  * label (which, preceded by the labels of the [[headerGroup]]s around it, is also the
  * path the `sort` spec addresses the column by), an optional plain-text
  * projection (used for cell text AND the global filter), an optional `body`
  * template rendering arbitrary UI per row, an optional `sortBy` ordering (which
  * makes the header clickable when the table has a `sort` ref), and an `align`.
  *
  * {{{
  * Column[Product]("Name")(_.name).sortBy(_.name)
  * Column[Product]("Price").body(p => span(fmt(p))).sortBy(_.price).align(ColumnAlign.End)
  * }}}
  */
/** The value round trip of one editable column, with the cell type closed over.
  *
  * `Column[A, +K]` has no type parameter to hold a cell type in, exactly as it has none
  * for `rowSpan`'s key, and the answer is the same one: keep the COMPOSED functions
  * rather than the typed halves. `V` is fixed where [[Column.editable]] is called and is
  * sealed in here, so the table moves text and rows and never learns a value type.
  *
  * `commit` is the whole write path in one function: parse the editor's text, run the
  * column's rules over the parsed value, then write it into the row. A failure comes back
  * as the `FieldError` that caused it, which is what keeps the cell open on the screen it
  * failed on.
  */
final private[uic] case class CellEdit[A](
    show: A => String,
    commit: (A, String) => Frame ?=> (Result[FieldError, A] < Async),
    editor: CellEditor,
    when: A => Boolean = (_: A) => true,
    changed: Maybe[(A, A) => Any < Async] = Absent
)

/** The filter pipeline of one column, with the cell type closed over, as [[CellEdit]]
  * closes over the one that edits it.
  *
  * `modes` is what the reader may pick from and `default` what an untouched column starts
  * on, both decided by the [[CellType]]: a type that compares gets the comparison modes,
  * everything else the text ones. `predicate` reads one filter into a row test, or answers
  * `Absent` when this column cannot apply it, which is a query that is not a value of its
  * type, or a mode it never offered.
  */
final private[uic] case class CellFilter[A](
    modes: List[MatchMode],
    default: MatchMode,
    predicate: ColumnFilter => Maybe[A => Boolean]
)

final case class Column[A, +K <: FlatOnly] private (
    headerV: String,
    textF: Maybe[A => String] = Absent,
    bodyF: Maybe[A => UI] = Absent,
    orderingV: Maybe[Ordering[A]] = Absent,
    alignV: ColumnAlign = ColumnAlign.Start,
    footerTextV: Maybe[String] = Absent,
    footerF: Maybe[Seq[A] => UI] = Absent,
    rowSpanEqF: Maybe[(A, A) => Boolean] = Absent,
    sortableV: Maybe[BoolValue] = Absent,
    editV: Maybe[CellEdit[A]] = Absent,
    navigableFlag: Boolean = true,
    visibleV: Maybe[BoolValue] = Absent,
    filterV: Maybe[CellFilter[A]] = Absent,
    widthV: Maybe[Double] = Absent,
    resizableFlag: Boolean = true,
    frozenV: Maybe[FrozenEdge] = Absent,
    reorderableFlag: Boolean = true
) extends ColumnTree[A]:
    private[uic] def label: String                        = headerV
    private[uic] def leaves: List[Column[A, FlatOnly]]    = List(this)
    private[uic] def children: List[ColumnTree[A]]        = Nil
    private[uic] def height: Int                          = 1
    private[uic] def rowspan(depth: Int, row: Int): Int   = depth - row
    private[uic] def asColumn: Maybe[Column[A, FlatOnly]] = Present(this)

    private[uic] def withChildren(cs: List[ColumnTree[A]]): ColumnTree[A] = this

    /** Custom cell content, replacing (or standing in for) the text projection. */
    def body(f: A => UI): Column[A, K] = copy(bodyF = Present(f))

    /** Makes this column editable: `read` projects the cell's value out of the row, `write`
      * puts an edited one back, and the [[CellType]] in scope supplies the editor, the
      * formatter and the parser between them.
      *
      * The whole round trip is the column's, which is the point. The table decides WHICH
      * cell is open and nothing else, so nothing downstream has to ask which column an
      * edit belongs to, or dispatch on a path to find the field it maps to. Where a value
      * comes from and where it goes is written once, here, and checked against the row
      * type.
      *
      * The table needs one of [[DataTable.editingCell]] or [[DataTable.editingRows]]
      * bound, or nothing to open the editor with, and a `rowKey`, since a commit finds its
      * row by key.
      */
    def editable[V](read: A => V)(write: (A, V) => A)(using ct: CellType[V]): Column[A, EditableFlatOnly] =
        editableAs(ct)(read)(write)

    /** [[editable]] with the cell type given explicitly: a domain type through
      * `CellType.of(values)(label)`, a second editor over a type that already has one, or
      * a type carrying its own rules through `CellType.validate`.
      */
    def editableAs[V](ct: CellType[V])(read: A => V)(write: (A, V) => A): Column[A, EditableFlatOnly] =
        copy(editV =
            Present(CellEdit[A](
                show = a => ct.format(read(a)),
                commit = (a, raw) =>
                    ct.parse(raw) match
                        case Result.Success(v) =>
                            ct.check.run(v).map {
                                case Absent     => Result.succeed(write(a, v))
                                case Present(e) => Result.fail(e)
                            }
                        case Result.Failure(e) => Result.fail(e)
                        case other             => other.asInstanceOf[Result[FieldError, A]]
                ,
                editor = ct.editor
            ))
        )

    /** Which rows of an editable column may actually be edited, AG Grid's `editable`
      * callback. A cell the predicate rejects stays navigable and shows its value, it just
      * does not open, and Tab passes over it.
      */
    def editableWhen(p: A => Boolean)(using IsEditable[K]): Column[A, K] =
        copy(editV = editV.map(_.copy(when = p)))

    /** Runs after a committed edit of THIS column, with the row before and after the
      * write. The table-wide `onCellValueChanged` fires for every column; this one is for
      * a rule that belongs to a single field.
      */
    def onValueChanged(f: (A, A) => Any < Async)(using IsEditable[K]): Column[A, K] =
        copy(editV = editV.map(_.copy(changed = Present(f))))

    /** Gives this column its own filter, over the value `read` projects out of the row.
      * The [[CellType]] in scope decides how it reads: a type that compares (every
      * provided number, anything through `CellType.ordered`) gets `=`, `<`, `>` and their
      * negations and reads the query as a VALUE; everything else is matched as text, with
      * "contains", "starts with" and the rest, over what the type formats the value as.
      *
      * That is the whole difference from a stringly filter: `filterBy(_.price)` on an
      * `Int` column answers `< 50` correctly, where a text match would put 100 before 50
      * and match 5 against 15. The reader picks the mode from the column's own list, so
      * nothing here has to be configured twice.
      *
      * The table needs [[DataTable.columnFilters]] bound, which is where the filters live
      * and what gives the table its filter row.
      */
    def filterBy[V](read: A => V)(using ct: CellType[V]): Column[A, FlatOnly] =
        copy(filterV = Present(cellFilter(read, ct)))

    /** Filters by this column's own text projection, which is the projection nine times
      * in ten and would otherwise be written twice on one line. [[HasText]] confines the
      * form to a column that has one, as it does for the bare [[rowSpan]].
      *
      * The text projection is text, so this always matches as text. A column whose values
      * should COMPARE names them: `filterBy(_.price)`.
      */
    def filterBy(using HasText[K]): Column[A, FlatOnly] =
        // The evidence is exactly the proof that this projection is there.
        textF.map(f =>
            copy(filterV =
                Present(CellFilter[A](
                    ColumnFilter.textModes,
                    MatchMode.Contains,
                    ColumnFilter.onText(f)
                ))
            )
        ).getOrElse(this)

    /** The filter pipeline for one value type, comparison where the type compares. */
    private def cellFilter[V](read: A => V, ct: CellType[V]): CellFilter[A] =
        ct.order match
            case Present(ord) =>
                CellFilter[A](ColumnFilter.orderedModes, MatchMode.Equals, ColumnFilter.onOrdered(read, ct.parse, ord))
            case Absent =>
                CellFilter[A](ColumnFilter.textModes, MatchMode.Contains, ColumnFilter.onText(a => ct.format(read(a))))

    /** Static footer label for this column; any column carrying a footer gives the
      * table a `tfoot`.
      */
    def footer(v: String): Column[A, FlatOnly] = copy(footerTextV = Present(v), footerF = Absent)

    /** Footer content computed from the rows that survive the table's global filter,
      * across every page rather than the visible one. The table owns filtering, so an
      * aggregate over what the reader is looking at cannot be computed by the caller.
      */
    def footer(f: Seq[A] => UI): Column[A, FlatOnly] = copy(footerF = Present(f), footerTextV = Absent)

    /** Makes the column sortable by the projected key (header clicks cycle
      * ascending → descending → unsorted when the table has a `sort` ref).
      *
      * This is HOW the column sorts; [[sortable]] is whether the reader may change it.
      */
    def sortBy[B](f: A => B)(using ord: Ordering[B]): Column[A, K] =
        copy(orderingV = Present(Ordering.by(f)))

    /** Whether the reader may change this column's place in the sort spec, which is a
      * separate question from whether the column HAS an ordering ([[sortBy]]) and from
      * whether it currently sorts (the spec).
      *
      * Unset, a column with an ordering is interactive, which is what it always was. Set
      * to false, the header goes inert: no click, no tab stop, no sort affordance. What it
      * keeps is the STATE, the sorted class, `aria-sort`, the direction icon and the rank
      * badge, because a spec that names the column still sorts it and hiding that would
      * misreport the rows the reader is looking at. That pair is the point: a column the
      * table sorts by and the reader may not re-sort.
      *
      * In a `DataTable.lazyRows` table it is also the whole declaration: the rows arrive
      * sorted, so there is no ordering for the flag to sit beside, and `sortable(true)` is
      * what marks a column the server sorts by.
      */
    def sortable(v: Boolean): Column[A, K] = copy(sortableV = Present(BoolValue.Const(v)))

    /** Reactive [[sortable]]: the affordance follows the signal, which is what suspends
      * re-sorting while a mutation is in flight.
      */
    def sortable(sig: Signal[Boolean]): Column[A, K] = copy(sortableV = Present(BoolValue.Dyn(sig)))

    /** Whether this column is rendered at all. A hidden column contributes no header
      * cell, no body cells and no footer cell, so the table is exactly as wide as the
      * columns the reader can see and every colspan follows.
      *
      * What it does NOT do is leave the table: the column is still authored, so the
      * sort spec keeps sorting by it and hiding one never reshuffles the rows under the
      * reader. The global filter is the other way round, and deliberately: a query
      * matches what is on the screen, so a hidden column's text is not searched.
      *
      * A [[headerGroup]] whose columns are all hidden disappears with them.
      */
    def visible(v: Boolean): Column[A, K] = copy(visibleV = Present(BoolValue.Const(v)))

    /** Reactive [[visible]], which is how a column becomes one the reader shows and
      * hides: bind the signal a toggle writes.
      */
    def visible(sig: Signal[Boolean]): Column[A, K] = copy(visibleV = Present(BoolValue.Dyn(sig)))

    /** How wide this column is, in CSS pixels.
      *
      * The width reaches the column through a `col` element and not through its cells,
      * which is the only place that can carry one: a cell sizes the row it is in, and a
      * grouped header's cell spans several columns and cannot say how wide any single one
      * of them is. It is authoritative rather than a suggestion, so a value too long for
      * it is clipped instead of widening the column under the reader.
      *
      * Columns with no width share what the ones that have it leave over.
      */
    def width(px: Double): Column[A, K] = copy(widthV = Present(px))

    /** Whether the reader may drag this column's width, which is a separate question from
      * whether it HAS one ([[width]]) and needs [[DataTable.columnWidths]] bound to write
      * the new one into.
      *
      * A drag moves the BOUNDARY between two columns and changes both, so the handle
      * appears only where both sides allow it, and a column that answers false pins its
      * own width and its neighbour's edge with it.
      */
    def resizable(v: Boolean): Column[A, K] = copy(resizableFlag = v)

    /** Holds this column against the leading edge of the table while the columns beside
      * it scroll past, Prime's `frozen`.
      *
      * A frozen column is one the reader keeps in sight, so it has to be somewhere: the
      * cell is stuck at the distance from the edge that the columns between it and the
      * edge take up, and the table works that distance out from their widths. That is why
      * a frozen column needs a [[width]], and why the frozen ones have to REACH the edge:
      * a free column between them and it would carry the frozen one away as it scrolled.
      * A table that breaks either rule freezes nothing and says so in a card.
      *
      * Declaring one puts the table in a scroll container, since a column can only be
      * frozen against something that scrolls. [[DataTable.scrollHeight]] adds a cap on
      * the height; freezing on its own scrolls sideways, which is all a wide table needs.
      */
    def frozen(v: Boolean): Column[A, FlatOnly] =
        copy(frozenV = if v then Present(FrozenEdge.Start) else Absent)

    /** [[frozen]] against a chosen edge: `End` holds the column against the trailing one,
      * which is where a column of row actions belongs.
      */
    def frozen(edge: FrozenEdge): Column[A, FlatOnly] = copy(frozenV = Present(edge))

    /** Whether the reader may drag this column to another place, which needs
      * [[DataTable.columnOrder]] bound to write the new order into.
      *
      * A column that answers false stays where it was authored and no drop lands beside
      * it, which is how a column that means something by its position (a leading
      * identifier, a trailing total) keeps it while the rest of the header stays free.
      */
    def reorderable(v: Boolean): Column[A, K] = copy(reorderableFlag = v)

    def align(v: ColumnAlign): Column[A, K] = copy(alignV = v)

    /** Whether the keyboard cursor may land on this column's cells, AG Grid's
      * `suppressNavigable`. A column that answers false is stepped OVER by the arrows
      * rather than absorbing them, and it is never a tab stop.
      *
      * It says nothing about the mouse or about the value: a non-navigable column still
      * renders, still sorts, and a click still reaches whatever it renders.
      */
    def navigable(v: Boolean): Column[A, K] = copy(navigableFlag = v)

    /** Merges this column's cells across consecutive rows whose key is equal: one cell per
      * run, spanning it. The marker rides on the column, so unlike a string-keyed prop it
      * cannot name a column the table does not have, and the key is explicit, so a column
      * rendering only a [[body]] template can merge as well as a text one.
      *
      * The key is never rendered, it only decides which rows count as the same, so it can
      * be any type the compiler will compare: an id, a tuple of two fields, an opaque
      * type with a derived `CanEqual`. Going through a `String` would allocate on every
      * comparison and, worse, merge two keys whose `toString` happened to agree. The
      * column keeps the comparison rather than the projection, since it has no type
      * parameter to hold the key's type in.
      *
      * Runs are clipped by every merged column to the LEFT and by the innermost group a
      * row sits in. That is not a nicety: a full-width group header row inside a merged
      * run would overlap the span and break the table, and two merged columns whose runs
      * crossed would do the same. Clipping makes both impossible, and it makes column
      * order the outer-to-inner order, which is how a merged table reads anyway.
      */
    def rowSpan[K2](key: A => K2)(using CanEqual[K2, K2]): Column[A, FlatOnly] =
        copy(rowSpanEqF = Present((x, y) => key(x) == key(y)))

    /** Merges by this column's own text projection, which is the key nine times in ten and
      * would otherwise be written twice on one line. [[HasText]] is what confines the form
      * to a column that has one: a body-only column has no text to merge by, and rather
      * than merging nothing it does not compile.
      */
    def rowSpan(using HasText[K]): Column[A, FlatOnly] =
        // The evidence is exactly the proof that this projection is there.
        textF.map(f => copy(rowSpanEqF = Present((x, y) => f(x) == f(y)))).getOrElse(this)

    private[uic] def hasFooter: Boolean = footerTextV.isDefined || footerF.isDefined

    /** Whether the keyboard cursor may land here. See [[navigable]]. */
    private[uic] def isNavigable: Boolean = navigableFlag

    /** Whether this column carries an edit pipeline at all. */
    private[uic] def isEditable: Boolean = editV.isDefined

    /** Whether this column carries a filter pipeline at all. */
    private[uic] def isFilterable: Boolean = filterV.isDefined

    /** Whether THIS row's cell may open, which is [[editableWhen]] over an editable
      * column and false over one that carries no pipeline.
      */
    private[uic] def isEditableAt(a: A): Boolean = editV.exists(_.when(a))

    /** Whether the header is interactive, given the flag resolved to a plain boolean.
      * An ordering is the precondition: without one there is nothing a click could do.
      */
    private[uic] def isSortable(flag: Boolean): Boolean = orderingV.isDefined && flag

    /** The sortable flag as a plain boolean where it is statically known, which is every
      * case except a signal-backed one; those are resolved by the host before it builds.
      */
    private[uic] def sortableConst: Boolean = BoolValue.const(sortableV).getOrElse(true)

    /** The signal behind a reactive [[sortable]], if it is reactive. */
    private[uic] def sortableSig: Maybe[Signal[Boolean]] = sortableV.dynSig

    /** The visible flag as a plain boolean where it is statically known; a signal-backed
      * one is resolved by the host before it builds, as the sortable flag is.
      */
    private[uic] def visibleConst: Boolean = BoolValue.const(visibleV).getOrElse(true)

    /** The signal behind a reactive [[visible]], if it is reactive. */
    private[uic] def visibleSig: Maybe[Signal[Boolean]] = visibleV.dynSig

    /** How this column decides that two rows belong to the same merged cell, if it
      * merges at all.
      */
    private[uic] def rowSpanEq: Maybe[(A, A) => Boolean] = rowSpanEqF
end Column

object Column:
    /** A column rendering (and filtering by) the plain-text projection. */
    def apply[A](header: String)(text: A => String): Column[A, TextAnyTable] =
        new Column[A, TextAnyTable](header, textF = Present(text))

    /** A column without a text projection; give it a [[Column.body]] template.
      * (No text projection also means the global filter cannot match it.)
      */
    def apply[A](header: String): Column[A, AnyTable] = new Column[A, AnyTable](header)

end Column

/** A labelled span over other columns, rendering one header cell above them and adding
  * one row to the header.
  *
  * Groups nest, so a header of any depth is a tree of these over the real columns. The
  * group itself carries nothing else: sorting, filtering, footers and merging all belong
  * to the leaves, which is what keeps the body a function of the columns alone.
  */
final case class HeaderGroup[A] private (labelV: String, childrenV: List[ColumnTree[A]]) extends ColumnTree[A]:
    private[uic] def label: String                        = labelV
    private[uic] def leaves: List[Column[A, FlatOnly]]    = childrenV.flatMap(_.leaves)
    private[uic] def children: List[ColumnTree[A]]        = childrenV
    private[uic] def height: Int                          = 1 + childrenV.map(_.height).maxOption.getOrElse(0)
    private[uic] def rowspan(depth: Int, row: Int): Int   = 1
    private[uic] def asColumn: Maybe[Column[A, FlatOnly]] = Absent

    private[uic] def withChildren(cs: List[ColumnTree[A]]): ColumnTree[A] = copy(childrenV = cs)
end HeaderGroup

object HeaderGroup:
    /** A group over columns, other groups, or a mix of both. */
    def apply[A](label: String)(children: ColumnTree[A]*): HeaderGroup[A] =
        new HeaderGroup[A](label, children.toList)
end HeaderGroup

/** The typing context a [[column]] constructor reads its row type from.
  *
  * @note
  *   The message covers the second way this given goes missing. ANY failure inside a
  *   `columns(...)` call makes the compiler retype the argument without an expected type,
  *   which drops the scope, so the missing scope gets reported instead of the real error:
  *   an argument-less `rowSpan` on a column with no text projection surfaces here rather
  *   than at [[HasText]], and so does a column setter reached for on a [[HeaderGroup]].
  *
  * `DataTable[A].columns` and `TreeTable[A].columns` take their arguments as context
  * functions over this type, which fixes `A` before the argument is typed. That is
  * what makes the type argument unnecessary: `Column("Name")(_.name)` already infers
  * `A` from the expected element type, but chaining a modifier types the receiver on
  * its own, `A` widens to `Any`, and `_.name` stops resolving.
  */
@implicitNotFound(
    "A column has to be written inside a columns(...) call, which is what fixes its row type. If it is, then the " +
        "argument itself does not compile and this message stands in front of the real one: an argument-less " +
        "rowSpan on a column with no text projection (give it a key, rowSpan(_.field)), or a column setter such " +
        "as sortBy, footer or rowSpan on a headerGroup, which carries none of them."
)
final class ColumnScope[A] private[uic] ()

object ColumnScope:
    /** Lifts a prepared list into the shape `columns` (or [[headerGroup]]) takes, so a
      * table built from a reusable `Seq[Column[A, AnyTable]]` still splats:
      * `columns(sharedCols*)`. A splat applies no per-element conversion, but it does
      * apply one to the sequence.
      *
      * It lives here, and not beside `Column`, because the scope is the one type every
      * such conversion mentions: one given then serves columns, groups and mixed lists
      * without two of them ever both matching.
      */
    given seqAsScoped[A, C]: Conversion[Seq[C], Seq[ColumnScope[A] ?=> C]] =
        cs => cs.map(c => (_: ColumnScope[A]) ?=> c)
end ColumnScope

/** A column authored inside a `columns(...)` call, reading its row type from the
  * enclosing [[ColumnScope]].
  */
type ColumnOf[A, K <: FlatOnly] = ColumnScope[A] ?=> Column[A, K]

/** A column or a group of them, authored inside a `columns(...)` call: what
  * `DataTable.columns` takes. Every [[ColumnOf]] is one of these, so a prepared column
  * list splats into it unchanged.
  */
type HeaderOf[A] = ColumnScope[A] ?=> ColumnTree[A]

/** A column whose row type comes from the table it is passed to, so it carries no type
  * argument of its own.
  *
  * {{{
  * DataTable[Product]().columns(
  *     column("Name")(_.name).sortBy(_.name),
  *     column("Price")(_.price.toString).align(ColumnAlign.End)
  * )
  * }}}
  *
  * Outside a `columns(...)` call there is no scope to read, so a standalone column list
  * still names its row type once: `Column[Product]("Name")(_.name)`.
  */
def column[A](header: String)(using ColumnScope[A])(text: A => String): Column[A, TextAnyTable] =
    Column[A](header)(text)

/** A scoped column without a text projection; give it a [[Column.body]] template. */
def column[A](header: String)(using ColumnScope[A]): Column[A, AnyTable] =
    Column[A](header)

/** A labelled header cell spanning the columns (or nested groups) written under it,
  * reading its row type from the enclosing [[ColumnScope]].
  *
  * {{{
  * DataTable[Sale]().columns(
  *     column("Code")(_.code),
  *     headerGroup("Revenue")(
  *         headerGroup("2025")(column("Q1")(_.q1), column("Q2")(_.q2)),
  *         column("Total")(_.total)
  *     )
  * )
  * }}}
  */
def headerGroup[A](label: String)(using ColumnScope[A])(children: ColumnTree[A]*): HeaderGroup[A] =
    HeaderGroup[A](label)(children*)
