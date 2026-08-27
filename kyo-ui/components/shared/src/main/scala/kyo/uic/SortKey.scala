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

/** One entry of a table's ordered sort spec: the PATH that identifies the column, and
  * the direction it currently sorts in. The list is ordered by priority, the first
  * sorting entry being the primary key.
  *
  * The path is the column's header preceded by the labels of the [[headerGroup]]s it sits
  * in, outermost first, which is why it is a list of parts and not one joined string: no
  * separator ever becomes part of the API. A column in no group has a path of one part,
  * its header, so `SortKey.ascending("Category")` means what it always meant. Two columns
  * that share a header under different groups are told apart by the parts above them,
  * which is what lets a group carry the disambiguation instead of the cell label.
  */
final case class SortKey(path: List[String], direction: SortDirection) derives CanEqual:
    /** The column's own header, the last part of the path. */
    def column: String = path.lastOption.getOrElse("")

object SortKey:
    /** A column identified by its path, outermost group label first. */
    def apply(column: String, direction: SortDirection): SortKey = SortKey(List(column), direction)

    /** A column entering the spec starts ascending. Pass the group labels it sits in
      * ahead of its header; a column in no group needs only its header.
      */
    def ascending(path: String*): SortKey = SortKey(path.toList, SortDirection.Ascending)

    /** Advances `path` inside `spec`, keeping its slot; a column not yet in the spec is
      * appended. Trailing `Unsorted` entries are dropped, since an unsorted entry behind
      * the last sorting one carries no priority a later click could not reproduce by
      * appending, and without the pruning the spec would only ever grow.
      */
    private[uic] def cycle(spec: List[SortKey], path: List[String], removable: Boolean): List[SortKey] =
        if spec.exists(_.path == path) then advance(spec, path, removable)
        else spec :+ SortKey(path, SortDirection.Ascending)

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
    private[uic] def plain(spec: List[SortKey], path: List[String], removable: Boolean): List[SortKey] =
        spec.find(k => k.path == path && k.direction.isSorting) match
            case Some(k) if sorting(spec).sizeIs == 1 => advance(spec, path, removable)
            case Some(k) => spec.map(e => if e.path == path then e.copy(direction = k.direction.flipped) else e)
            case None    => List(SortKey(path, SortDirection.Ascending))

    /** Moves the column at `path` to its next direction without moving it in the order,
      * then drops trailing `Unsorted` entries. An unsorted entry behind the last sorting
      * one carries no priority a later click could not reproduce by appending, and without
      * the pruning the spec would only ever grow.
      */
    private def advance(spec: List[SortKey], path: List[String], removable: Boolean): List[SortKey] =
        val advanced = spec.map(k => if k.path == path then k.copy(direction = k.direction.next(removable)) else k)
        advanced.reverse.dropWhile(!_.direction.isSorting).reverse
    end advance

    /** The sorting entries in priority order, which is what both the fold that sorts the
      * rows and the rank badges read.
      */
    private[uic] def sorting(spec: List[SortKey]): List[SortKey] = spec.filter(_.direction.isSorting)
end SortKey
