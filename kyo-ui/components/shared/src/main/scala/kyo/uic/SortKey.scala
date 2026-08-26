package kyo.uic

import kyo.*

/** Which way one column of a [[DataTable]] or [[TreeTable]] sorts, or that it holds a
  * slot in the priority order without currently sorting.
  *
  * `Unsorted` is what makes a multi-key sort repairable. Without it, switching a column
  * off means deleting it from the ordered spec, every column behind it moves up a rank,
  * and clicking that column again appends it at the end rather than putting it back. A
  * column that keeps its slot while unsorted is restored by the next click on the same
  * header, and nothing else moves.
  */
enum SortDirection derives CanEqual:
    case Ascending, Descending, Unsorted

    private[uic] def isSorting: Boolean = this != SortDirection.Unsorted

    /** The next direction in the modifier-held cycle, the only one that can switch a
      * column off. `removable` is what [[DataTable.removableSort]] controls: with it off
      * the cycle never reaches `Unsorted`, which is Prime's default.
      */
    private[uic] def next(removable: Boolean): SortDirection = this match
        case SortDirection.Ascending  => SortDirection.Descending
        case SortDirection.Descending => if removable then SortDirection.Unsorted else SortDirection.Ascending
        case SortDirection.Unsorted   => SortDirection.Ascending

    /** The plain-click transition, which reverses a column but never switches it off. */
    private[uic] def flipped: SortDirection = this match
        case SortDirection.Ascending  => SortDirection.Descending
        case SortDirection.Descending => SortDirection.Ascending
        case SortDirection.Unsorted   => SortDirection.Ascending
end SortDirection

/** One entry of a table's ordered sort spec: the column's `header` (its identity in the
  * spec) and the direction it currently sorts in. The list is ordered by priority, the
  * first sorting entry being the primary key.
  */
final case class SortKey(column: String, direction: SortDirection) derives CanEqual

object SortKey:
    /** A column entering the spec starts ascending. */
    def ascending(column: String): SortKey = SortKey(column, SortDirection.Ascending)

    /** Advances `column` inside `spec`, keeping its slot; a column not yet in the spec is
      * appended. Trailing `Unsorted` entries are dropped, since an unsorted entry behind
      * the last sorting one carries no priority a later click could not reproduce by
      * appending, and without the pruning the spec would only ever grow.
      */
    private[uic] def cycle(spec: List[SortKey], column: String, removable: Boolean): List[SortKey] =
        if spec.exists(_.column == column) then advance(spec, column, removable)
        else spec :+ ascending(column)

    /** The plain-click transition, which depends on how many columns are sorting.
      *
      * With ONE sorted column the plain click owns the whole cycle, ascending,
      * descending, off, because there is no priority order to damage and no reason to
      * make clearing a single sort reach for a modifier.
      *
      * With SEVERAL the plain click only REVERSES the clicked column, in place. There
      * the click is the direction control and nothing else: a spec built up over several
      * clicks must not lose a key because one header was clicked one time too many, so
      * switching a column off stays with the modifier, through [[cycle]].
      *
      * A column that is not sorting (absent, or holding a slot as `Unsorted`) becomes the
      * single key either way, which is how a spec collapses back to one column.
      */
    private[uic] def plain(spec: List[SortKey], column: String, removable: Boolean): List[SortKey] =
        spec.find(k => k.column == column && k.direction.isSorting) match
            case Some(k) if sorting(spec).sizeIs == 1 => advance(spec, column, removable)
            case Some(k) => spec.map(e => if e.column == column then e.copy(direction = k.direction.flipped) else e)
            case None    => List(ascending(column))

    /** Moves `column` to its next direction without moving it in the order, then drops
      * trailing `Unsorted` entries. An unsorted entry behind the last sorting one carries
      * no priority a later click could not reproduce by appending, and without the
      * pruning the spec would only ever grow.
      */
    private def advance(spec: List[SortKey], column: String, removable: Boolean): List[SortKey] =
        val advanced = spec.map(k => if k.column == column then k.copy(direction = k.direction.next(removable)) else k)
        advanced.reverse.dropWhile(!_.direction.isSorting).reverse
    end advance

    /** The sorting entries in priority order, which is what both the fold that sorts the
      * rows and the rank badges read.
      */
    private[uic] def sorting(spec: List[SortKey]): List[SortKey] = spec.filter(_.direction.isSorting)
end SortKey
