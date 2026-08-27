package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.implicitNotFound
import scala.annotation.tailrec

/** Horizontal alignment of one [[Column]] (applied to the header cell and every
  * body cell of the column via `.p-uic-dt-center` / `.p-uic-dt-end`).
  */
enum ColumnAlign derives CanEqual:
    case Start, Center, End

/** Which tables accept a [[Column]], carried as the column's second, phantom type
  * argument so a table can refuse one it could not honor.
  *
  * `Column` is shared by [[DataTable]] and [[TreeTable]], and two of its options mean
  * nothing over a hierarchy: [[Column.footer]] fills a `tfoot` a TreeTable does not
  * render, and [[Column.rowSpan]] merges runs of equal cells, which consecutive rows at
  * different depths do not form. Both setters return a `FlatOnly` column, which
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
    "A TreeTable takes plain columns only. It renders one header row, so a headerGroup has no place in it, and " +
        "it renders neither a tfoot nor merged runs, so a column carrying footer or rowSpan does not fit either: " +
        "a footer needs the tfoot, and rowSpan merges runs of equal cells, which rows at different depths do not form."
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
end ColumnTree

object ColumnTree:
    /** One header cell: the node it renders, its path from the outermost group down, and
      * the two spans read off the tree.
      */
    final private[uic] case class HeaderSpan[A](
        node: ColumnTree[A],
        path: List[String],
        colspan: Int,
        rowspan: Int
    )

    /** The header cells, one list per header row, top row first.
      *
      * A node with no leaves under it occupies nothing and is dropped, which is what an
      * empty group amounts to; [[DataTable]] names those in a diagnostic card rather
      * than leaving the omission silent.
      */
    private[uic] def spans[A](nodes: List[ColumnTree[A]]): List[List[HeaderSpan[A]]] =
        val depth = nodes.map(_.height).maxOption.getOrElse(0)
        @tailrec def loop(
            level: List[(ColumnTree[A], List[String])],
            row: Int,
            acc: List[List[HeaderSpan[A]]]
        ): List[List[HeaderSpan[A]]] =
            val live = level.filter(_._1.leaves.nonEmpty)
            if live.isEmpty then acc.reverse
            else
                val cells = live.map((n, prefix) => HeaderSpan(n, prefix :+ n.label, n.leaves.size, n.rowspan(depth, row)))
                loop(live.flatMap((n, prefix) => n.children.map(c => (c, prefix :+ n.label))), row + 1, cells :: acc)
            end if
        end loop
        loop(nodes.map(n => (n, Nil)), 0, Nil)
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
final case class Column[A, +K <: FlatOnly] private (
    headerV: String,
    textF: Maybe[A => String] = Absent,
    bodyF: Maybe[A => UI] = Absent,
    orderingV: Maybe[Ordering[A]] = Absent,
    alignV: ColumnAlign = ColumnAlign.Start,
    footerTextV: Maybe[String] = Absent,
    footerF: Maybe[Seq[A] => UI] = Absent,
    rowSpanEqF: Maybe[(A, A) => Boolean] = Absent,
    sortableV: Maybe[BoolValue] = Absent
) extends ColumnTree[A]:
    private[uic] def label: String                        = headerV
    private[uic] def leaves: List[Column[A, FlatOnly]]    = List(this)
    private[uic] def children: List[ColumnTree[A]]        = Nil
    private[uic] def height: Int                          = 1
    private[uic] def rowspan(depth: Int, row: Int): Int   = depth - row
    private[uic] def asColumn: Maybe[Column[A, FlatOnly]] = Present(this)

    /** Custom cell content, replacing (or standing in for) the text projection. */
    def body(f: A => UI): Column[A, K] = copy(bodyF = Present(f))

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
      */
    def sortable(v: Boolean): Column[A, K] = copy(sortableV = Present(BoolValue.Const(v)))

    /** Reactive [[sortable]]: the affordance follows the signal, which is what suspends
      * re-sorting while a mutation is in flight.
      */
    def sortable(sig: Signal[Boolean]): Column[A, K] = copy(sortableV = Present(BoolValue.Dyn(sig)))

    def align(v: ColumnAlign): Column[A, K] = copy(alignV = v)

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
