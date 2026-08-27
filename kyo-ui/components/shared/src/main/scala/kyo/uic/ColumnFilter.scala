package kyo.uic

import kyo.*
import kyo.uic.form.FieldError

/** How a column reads what the reader typed against its own values.
  *
  * The set a column offers is decided by its [[CellType]] and not by the caller: a type
  * that carries an ordering is compared, everything else is matched as text. That is the
  * same reason the editor and the parser travel together in one `CellType`: a mode a
  * column cannot answer would be a filter that silently keeps every row.
  */
enum MatchMode derives CanEqual:
    case Contains, NotContains, StartsWith, EndsWith, Equals, NotEquals
    case Less, LessOrEqual, Greater, GreaterOrEqual

    /** The label the constraint list shows, in Prime's wording. */
    private[uic] def label: String = this match
        case Contains       => "Contains"
        case NotContains    => "Not contains"
        case StartsWith     => "Starts with"
        case EndsWith       => "Ends with"
        case Equals         => "Equals"
        case NotEquals      => "Not equals"
        case Less           => "Less than"
        case LessOrEqual    => "Less than or equal to"
        case Greater        => "Greater than"
        case GreaterOrEqual => "Greater than or equal to"
end MatchMode

/** One column's filter: what the reader typed, and how the column reads it.
  *
  * This is the unit of [[DataTable.columnFilters]], keyed by the same column path the
  * sort spec names a column by. Seed the map to open the table on a filter already
  * applied; the reader's typing writes it back.
  */
final case class ColumnFilter(query: String, mode: MatchMode) derives CanEqual

object ColumnFilter:

    /** The modes a column whose values are text can answer. */
    private[uic] val textModes: List[MatchMode] =
        List(
            MatchMode.StartsWith,
            MatchMode.Contains,
            MatchMode.NotContains,
            MatchMode.EndsWith,
            MatchMode.Equals,
            MatchMode.NotEquals
        )

    /** The modes a column whose values compare can answer. `Contains` is not among them:
      * a number is not a string, and reading it as one would match 1 against 21.
      */
    private[uic] val orderedModes: List[MatchMode] =
        List(
            MatchMode.Equals,
            MatchMode.NotEquals,
            MatchMode.Less,
            MatchMode.LessOrEqual,
            MatchMode.Greater,
            MatchMode.GreaterOrEqual
        )

    /** The row predicate for a text column, or `Absent` when the mode is one this column
      * cannot answer, which a seeded map is the only way to reach.
      *
      * The comparison is over the column's own formatted value and is case-insensitive,
      * as the global filter is: a reader typing into a table is naming what they can see,
      * not writing a case-sensitive query.
      */
    private[uic] def onText[A](show: A => String)(f: ColumnFilter): Maybe[A => Boolean] =
        val q                                                       = f.query.trim.toLowerCase
        def by(g: (String, String) => Boolean): Maybe[A => Boolean] = Present(a => g(show(a).toLowerCase, q))
        f.mode match
            case MatchMode.Contains    => by(_.contains(_))
            case MatchMode.NotContains => by(!_.contains(_))
            case MatchMode.StartsWith  => by(_.startsWith(_))
            case MatchMode.EndsWith    => by(_.endsWith(_))
            case MatchMode.Equals      => by(_ == _)
            case MatchMode.NotEquals   => by(_ != _)
            case _                     => Absent
        end match
    end onText

    /** The row predicate for a column whose values compare, or `Absent` when the query is
      * not a value of that column's type.
      *
      * The query is parsed ONCE, here, and the predicate closes over the result: a parse
      * per row would allocate down the whole table on every keystroke.
      */
    private[uic] def onOrdered[A, V](
        read: A => V,
        parse: String => Result[FieldError, V],
        ord: Ordering[V]
    )(f: ColumnFilter): Maybe[A => Boolean] =
        parse(f.query.trim) match
            case Result.Success(v) =>
                def by(g: Int => Boolean): Maybe[A => Boolean] = Present(a => g(ord.compare(read(a), v)))
                f.mode match
                    case MatchMode.Equals         => by(_ == 0)
                    case MatchMode.NotEquals      => by(_ != 0)
                    case MatchMode.Less           => by(_ < 0)
                    case MatchMode.LessOrEqual    => by(_ <= 0)
                    case MatchMode.Greater        => by(_ > 0)
                    case MatchMode.GreaterOrEqual => by(_ >= 0)
                    case _                        => Absent
                end match
            case _ => Absent
        end match
    end onOrdered
end ColumnFilter
