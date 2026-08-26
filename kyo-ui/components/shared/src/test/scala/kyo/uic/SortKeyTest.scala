package kyo.uic

/** The pure sort-spec algebra behind the table headers. These are the transitions the
  * header click produces; the rendered consequences are pinned in GoldenRenderTest.
  */
class SortKeyTest extends UicTest:

    import SortDirection.*

    private def spec(ks: (String, SortDirection)*): List[SortKey] =
        ks.toList.map((c, d) => SortKey(c, d))

    private def cycle(s: List[SortKey], c: String): List[SortKey] = SortKey.cycle(s, c, removable = true)

    "a column entering the spec starts ascending, at the end" in {
        assert(cycle(Nil, "A") == spec("A" -> Ascending))
        assert(cycle(spec("A" -> Ascending), "B") == spec("A" -> Ascending, "B" -> Ascending))
    }

    "the cycle advances a column without moving it" in {
        val three = spec("A" -> Ascending, "B" -> Ascending, "C" -> Ascending)
        assert(cycle(three, "B") == spec("A" -> Ascending, "B" -> Descending, "C" -> Ascending))
        assert(cycle(cycle(three, "B"), "B") == spec("A" -> Ascending, "B" -> Unsorted, "C" -> Ascending))
    }

    // The reason SortDirection has a third case at all: switching the middle key off and
    // back on has to be two clicks on that one header, not a rebuild of the whole spec.
    "an unsorted column keeps its slot, so one more click restores the original spec" in {
        val three    = spec("A" -> Ascending, "B" -> Ascending, "C" -> Ascending)
        val off      = cycle(cycle(three, "B"), "B")
        val restored = cycle(off, "B")
        assert(SortKey.sorting(off).map(_.column) == List("A", "C"), "B stops sorting")
        assert(off.map(_.column) == List("A", "B", "C"), "but keeps its slot")
        assert(restored == three, "and comes back where it was")
    }

    "trailing unsorted entries are dropped, so the spec cannot only grow" in {
        assert(cycle(cycle(spec("A" -> Ascending), "A"), "A") == Nil)
        // An unsorted entry BEFORE a sorting one is load-bearing and stays.
        val mid = cycle(cycle(spec("A" -> Ascending, "B" -> Ascending), "A"), "A")
        assert(mid == spec("A" -> Unsorted, "B" -> Ascending))
    }

    "removableSort off keeps the cycle on two states" in {
        val once = SortKey.cycle(spec("A" -> Ascending), "A", removable = false)
        assert(once == spec("A" -> Descending))
        assert(SortKey.cycle(once, "A", removable = false) == spec("A" -> Ascending))
    }

    private def plain(s: List[SortKey], c: String): List[SortKey] = SortKey.plain(s, c, removable = true)

    // With one sorted column there is no priority order to damage, so the plain click
    // owns the whole cycle and clearing a single sort needs no modifier.
    "a plain click on the ONLY sorted column cycles all three states" in {
        val one = spec("A" -> Ascending)
        val two = plain(one, "A")
        assert(two == spec("A" -> Descending))
        assert(plain(two, "A") == Nil, "the third click clears it")
        assert(plain(Nil, "A") == spec("A" -> Ascending), "and the fourth starts over")
    }

    // With several keys the click is the direction control and nothing else: a spec built
    // over several clicks must not lose one because a header was clicked once too often.
    "a plain click with SEVERAL sorted columns only reverses, in place" in {
        val three = spec("A" -> Ascending, "B" -> Ascending, "C" -> Ascending)
        val once  = plain(three, "B")
        assert(once == spec("A" -> Ascending, "B" -> Descending, "C" -> Ascending))
        assert(plain(once, "B") == three, "and back again, forever")
        val many = List.iterate(three, 6)(s => plain(s, "B"))
        assert(many.forall(s => SortKey.sorting(s).sizeIs == 3), "no key is ever dropped")
    }

    // The boundary between the two rules: switching keys off until one is left hands the
    // full cycle back to the plain click.
    "the cycle returns to the plain click as soon as one key is left" in {
        val two = spec("A" -> Ascending, "B" -> Ascending)
        assert(plain(two, "A") == spec("A" -> Descending, "B" -> Ascending), "two keys: reverse only")
        // asc -> desc -> unsorted; the trailing unsorted entry is then pruned away
        val one = SortKey.cycle(SortKey.cycle(two, "B", removable = true), "B", removable = true)
        assert(one == spec("A" -> Ascending), "B off leaves one key")
        assert(plain(plain(one, "A"), "A") == Nil, "which the plain click can now clear")
    }

    "a plain click on a column that is not sorting makes it the single key" in {
        val three = spec("A" -> Ascending, "B" -> Ascending, "C" -> Ascending)
        assert(plain(three, "D") == spec("D" -> Ascending), "a column outside the spec")
        val held = spec("A" -> Ascending, "B" -> Unsorted, "C" -> Ascending)
        assert(plain(held, "B") == spec("B" -> Ascending), "a column holding a slot unsorted")
    }

    "sorting reads the entries in priority order, skipping the unsorted ones" in {
        val s = spec("A" -> Ascending, "B" -> Unsorted, "C" -> Descending)
        assert(SortKey.sorting(s) == spec("A" -> Ascending, "C" -> Descending))
    }
end SortKeyTest
