package kyo.uic

import kyo.*
import kyo.uic.form.FieldError

/** The pure half of per-column filtering: which modes a column offers, and what each one
  * makes of a row. What the filter row looks like is pinned in GoldenRenderTest, and what
  * typing into it does in DataTableTest.
  */
class ColumnFilterTest extends UicTest:

    private def text(f: FilterRule): Maybe[String => Boolean] = ColumnFilter.onText[String](identity)(f)

    private def keeps(p: Maybe[String => Boolean], value: String): Boolean =
        p match
            case Present(f) => f(value)
            case Absent     => throw new AssertionError("the column refused a filter it should read")

    private val price: FilterRule => Maybe[Int => Boolean] =
        ColumnFilter.onOrdered[Int, Int](identity, CellType.int.parse, Ordering[Int])

    private def keepsInt(f: FilterRule, value: Int): Boolean =
        price(f) match
            case Present(p) => p(value)
            case Absent     => throw new AssertionError("the column refused a filter it should read")

    "a text column matches by the six text modes" in {
        assert(keeps(text(FilterRule("amb", MatchMode.Contains)), "Bamboo"))
        assert(!keeps(text(FilterRule("amb", MatchMode.NotContains)), "Bamboo"))
        assert(keeps(text(FilterRule("Bam", MatchMode.StartsWith)), "Bamboo"))
        assert(!keeps(text(FilterRule("boo", MatchMode.StartsWith)), "Bamboo"))
        assert(keeps(text(FilterRule("boo", MatchMode.EndsWith)), "Bamboo"))
        assert(keeps(text(FilterRule("Bamboo", MatchMode.Equals)), "Bamboo"))
        assert(!keeps(text(FilterRule("Bam", MatchMode.Equals)), "Bamboo"))
        assert(keeps(text(FilterRule("Bam", MatchMode.NotEquals)), "Bamboo"))
    }

    // A reader typing into a table is naming what they can see, not writing a query, so
    // the case and the space around it are theirs, not the data's.
    "text matching ignores case and the space around the query" in {
        assert(keeps(text(FilterRule("BAMBOO", MatchMode.Equals)), "Bamboo"))
        assert(keeps(text(FilterRule("  amb  ", MatchMode.Contains)), "Bamboo"))
    }

    "a column that compares matches by value, not by the text a value prints as" in {
        assert(keepsInt(FilterRule("65", MatchMode.Equals), 65))
        assert(!keepsInt(FilterRule("65", MatchMode.NotEquals), 65))
        assert(keepsInt(FilterRule("70", MatchMode.Less), 65))
        assert(!keepsInt(FilterRule("65", MatchMode.Less), 65))
        assert(keepsInt(FilterRule("65", MatchMode.LessOrEqual), 65))
        assert(keepsInt(FilterRule("60", MatchMode.Greater), 65))
        assert(keepsInt(FilterRule("65", MatchMode.GreaterOrEqual), 65))
        // The whole point of comparing values: as text, 5 is not less than 21.
        assert(keepsInt(FilterRule("21", MatchMode.Less), 5))
    }

    // A filter the table cannot apply is not a filter. The table keeps every row and
    // marks the input instead, rather than emptying itself behind a typo.
    "a query that is not a value of the column's type is refused, not answered" in {
        assert(price(FilterRule("nope", MatchMode.Equals)) == Absent)
        assert(price(FilterRule("6.5", MatchMode.Less)) == Absent, "an Int column refuses a decimal")
        assert(price(FilterRule(" 65 ", MatchMode.Equals)).isDefined, "surrounding space is not a typo")
    }

    // Only a hand-seeded map can pair a column with a mode it never offered, and the
    // answer is the same as for a query it cannot read: this column does not filter.
    "a mode the column does not offer is refused by both readings" in {
        assert(text(FilterRule("5", MatchMode.Less)) == Absent)
        assert(price(FilterRule("5", MatchMode.Contains)) == Absent)
    }

    "the two mode sets are disjoint where they have to be" in {
        assert(ColumnFilter.textModes.contains(MatchMode.Contains))
        assert(!ColumnFilter.orderedModes.contains(MatchMode.Contains), "a number is not a string")
        assert(ColumnFilter.textModes.head == MatchMode.StartsWith, "Prime's own order")
        assert(ColumnFilter.orderedModes.contains(MatchMode.GreaterOrEqual))
        assert((ColumnFilter.textModes ++ ColumnFilter.orderedModes).distinct.size == MatchMode.values.length)
    }

    "every mode has a label, and no two share one" in {
        val labels = MatchMode.values.toList.map(_.label)
        assert(labels.forall(_.nonEmpty))
        assert(labels.distinct.size == labels.size)
        assert(MatchMode.LessOrEqual.label == "Less than or equal to")
    }

    // The parse is the CellType's, so a column filter refuses exactly what its editor
    // refuses, and reports the same failure.
    "the parser is the column's own" in {
        val e: Result[FieldError, Int] = CellType.int.parse("nope")
        assert(e.isFailure)
        assert(ColumnFilter.onOrdered[Int, Int](identity, CellType.int.parse, Ordering[Int])(
            FilterRule("nope", MatchMode.Equals)
        ) == Absent)
    }
end ColumnFilterTest
