package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.tailrec

/** The chain of group keys from the outermost grouping level down to one group, which is
  * what identifies that group among all others.
  *
  * A bare key does not: group by category and then by brand, and the brand "Rolex" occurs
  * under as many categories as sell one. The path distinguishes them, which is why
  * [[DataTable.expandedGroups]] binds a set of paths rather than a set of key strings.
  *
  * {{{
  * GroupPath("Accessories", "Rolex").key    // "Rolex", this group's own key
  * GroupPath("Accessories", "Rolex").depth  // 2
  * }}}
  */
final case class GroupPath(keys: List[String]) derives CanEqual:
    /** This group's own key, the last link of the chain. */
    def key: String = keys.lastOption.getOrElse("")

    /** How deep the group sits, counting from 1 for the outermost level. */
    def depth: Int = keys.size
end GroupPath

object GroupPath:
    def apply(keys: String*): GroupPath = GroupPath(keys.toList)

/** One grouping level of a [[DataTable]]: a key projection plus how the groups it forms
  * are presented. Levels nest in the order they are passed to [[DataTable.groupBy]], the
  * first being the outermost.
  *
  * A level draws a `tr.p-datatable-row-group-header` above each of its groups, showing the
  * key unless [[header]] fills it differently, and [[footer]] adds a summary row after the
  * group's rows. [[showHeader]] turns the header row off, for a level that exists only to
  * scope a summary row or to bound a merged column.
  *
  * {{{
  * group(_.category).header((path, rows) => span(s"${path.key} (${rows.size})"))
  * group(_.brand).showHeader(false).footer((_, rows) => span(total(rows)))
  * }}}
  */
final case class RowGroup[A] private (
    keyF: A => String,
    headerF: Maybe[(GroupPath, Seq[A]) => UI] = Absent,
    footerF: Maybe[(GroupPath, Seq[A]) => UI] = Absent,
    showHeaderFlag: Boolean = true
):
    /** Content of this level's header row, from the group's path and its rows. */
    def header(f: (GroupPath, Seq[A]) => UI): RowGroup[A] = copy(headerF = Present(f))

    /** A summary row after this level's rows, seeing exactly the rows of that one group.
      * [[Column.footer]] is the same idea for the table as a whole, per column.
      */
    def footer(f: (GroupPath, Seq[A]) => UI): RowGroup[A] = copy(footerF = Present(f))

    /** Whether this level draws a header row (default true). A level without one cannot be
      * collapsed either, since the toggle button lives in that row.
      */
    def showHeader(v: Boolean): RowGroup[A] = copy(showHeaderFlag = v)
end RowGroup

object RowGroup:
    /** A grouping level over the projected key, labelled by its `toString`.
      *
      * The key may be of any type, and the label it renders as is also its identity: two
      * groups that read the same ARE the same group, which is what already lets a key that
      * heads several runs share one collapse state. Projecting to the text you want shown
      * is therefore the way to control both at once, and for a `String` key the labelling
      * step is the identity function, so nothing changes.
      */
    def apply[A, K](key: A => K): RowGroup[A] = new RowGroup[A](a => String.valueOf(key(a)))

    /** Lifts a prepared level list into the shape `groupBy` takes, so a table built from a
      * reusable `Seq[RowGroup[A]]` still splats: `groupBy(sharedLevels*)`. A splat applies
      * no per-element conversion, but it does apply one to the sequence.
      */
    given seqAsGroupsOf[A]: Conversion[Seq[RowGroup[A]], Seq[GroupOf[A]]] =
        gs => gs.map(g => (_: GroupScope[A]) ?=> g)

    /** Splits `rows` into runs of CONSECUTIVE equal keys.
      *
      * Consecutive is the whole contract: the run boundary is the point where the key
      * changes in the order the rows are about to render, so grouping composes with the
      * sort spec instead of competing with it for authority over row order. A table
      * grouped by a key it is not sorted by therefore shows the same key on several
      * headers, which is what the underlying order says.
      */
    def runs[A](rows: List[A])(key: A => String): List[(String, List[A])] =
        @tailrec def loop(rest: List[A], acc: List[(String, List[A])]): List[(String, List[A])] =
            rest match
                case Nil => acc.reverse
                case head :: _ =>
                    val k           = key(head)
                    val (run, tail) = rest.span(a => key(a) == k)
                    loop(tail, (k, run) :: acc)
        loop(rows, Nil)
    end runs

    /** [[runs]] without the keys, split by an equivalence on the rows themselves rather
      * than by a projection, which is what a merged column carries: it compares by a key
      * whose type it does not keep.
      *
      * Every row is compared against the HEAD of its run, not against its predecessor, so
      * a run is the block of rows equivalent to the one that opened it. The head joins its
      * own run without being asked, which costs one comparison less and, more to the
      * point, keeps the split total: a caller's relation that is not reflexive would
      * otherwise leave the head in the tail and the walk would not advance.
      */
    def blocks[A](rows: List[A])(same: (A, A) => Boolean): List[List[A]] =
        @tailrec def loop(rest: List[A], acc: List[List[A]]): List[List[A]] =
            rest match
                case Nil => acc.reverse
                case head :: others =>
                    val (run, tail) = others.span(a => same(head, a))
                    loop(tail, (head :: run) :: acc)
        loop(rows, Nil)
    end blocks
end RowGroup

/** The typing context a [[group]] constructor reads its row type from, the [[ColumnScope]]
  * of grouping levels.
  */
final class GroupScope[A] private[uic] ()

/** A grouping level authored inside a `groupBy(...)` call, reading its row type from the
  * enclosing [[GroupScope]].
  */
type GroupOf[A] = GroupScope[A] ?=> RowGroup[A]

/** A grouping level whose row type comes from the table it is passed to, so it carries no
  * type argument of its own.
  *
  * {{{
  * DataTable[Product]().groupBy(
  *     group(_.category).header((path, rows) => span(s"${path.key} (${rows.size})")),
  *     group(_.brand)
  * )
  * }}}
  *
  * The key may be of any type; the label it renders as, and its identity, are its
  * `toString`. Outside a `groupBy(...)` call there is no scope to read, so a standalone
  * level list still names its row type once: `RowGroup[Product, String](_.category)`.
  */
def group[A, K](using GroupScope[A])(key: A => K): RowGroup[A] = RowGroup[A, K](key)

/** How one data row renders a column merged by [[Column.rowSpan]]: it either heads the run
  * and spans it, or is covered by the cell above and emits nothing.
  */
private[uic] enum SpanCell derives CanEqual:
    case Head(rows: Int)
    case Covered
