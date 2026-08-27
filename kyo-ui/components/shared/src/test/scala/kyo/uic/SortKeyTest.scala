package kyo.uic

/** The pure sort-spec algebra behind the table headers. These are the transitions the
  * header click produces; the rendered consequences are pinned in GoldenRenderTest.
  */
class SortKeyTest extends UicTest:

    import SortDirection.*

    private def spec(ks: (String, SortDirection)*): List[SortKey] =
        ks.toList.map((c, d) => SortKey(c, d))

    /** Every column reachable, which is what a table without a locked column passes. */
    private val free: List[String] => Boolean = _ => true

    private def cycle(s: List[SortKey], c: String): List[SortKey] =
        SortKey.cycle(s, List(c), removable = true, free)

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
        val once = SortKey.cycle(spec("A" -> Ascending), List("A"), removable = false, free)
        assert(once == spec("A" -> Descending))
        assert(SortKey.cycle(once, List("A"), removable = false, free) == spec("A" -> Ascending))
    }

    private def plain(s: List[SortKey], c: String): List[SortKey] =
        SortKey.plain(s, c :: Nil, removable = true, free)

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
        val one =
            SortKey.cycle(SortKey.cycle(two, List("B"), removable = true, free), List("B"), removable = true, free)
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

    // A column is identified by the path through the headerGroups around it, so the same
    // header under two groups is two different columns to the spec, which is the whole
    // reason the currency is a list of parts and not one joined string.
    "a path with the same header under two groups is two distinct entries" in {
        val a    = List("2024", "Q1")
        val b    = List("2025", "Q1")
        val both = SortKey.cycle(SortKey.cycle(Nil, a, removable = true, free), b, removable = true, free)
        assert(both == List(SortKey(a, Ascending), SortKey(b, Ascending)))
        // Advancing one leaves the other where it is.
        assert(SortKey.cycle(both, a, removable = true, free) == List(SortKey(a, Descending), SortKey(b, Ascending)))
    }

    // Every existing single-part spec means what it always meant: a column in no group has
    // a path of one part, and the String constructor still builds exactly that.
    "the single-argument forms build a one-part path" in {
        assert(SortKey.ascending("Category") == SortKey(List("Category"), Ascending))
        assert(SortKey("Price", Descending) == SortKey(List("Price"), Descending))
        assert(SortKey.ascending("2025", "Q1").path == List("2025", "Q1"))
        assert(SortKey.ascending("2025", "Q1").column == "Q1")
    }

    // A column carrying sortable(false) has no header the reader can click, so no click of
    // theirs may clear it. Without this, a plain click on a free column resets a locked one
    // by the long way round, which is the one thing the flag exists to prevent.
    "a click clears only the entries the reader could have cleared themselves" in {
        val locked: List[String] => Boolean = _ != List("L")
        val start                           = spec("L" -> Ascending, "A" -> Descending)
        // Plain click on a free column that is not sorting: the free entries go, L stays,
        // and the new key lands behind it.
        assert(SortKey.plain(start, List("B"), removable = true, locked) ==
            spec("L" -> Ascending, "B" -> Ascending))
        // The locked entry keeps its slot, so it keeps its rank over the new key.
        assert(SortKey.plain(start, List("B"), removable = true, locked).head.path == List("L"))
        // And the modifier click cannot reach it either.
        assert(SortKey.cycle(start, List("B"), removable = true, locked) ==
            spec("L" -> Ascending, "A" -> Descending, "B" -> Ascending))
    }

    // The pruning clears what the reader's own clicks left behind. A locked Unsorted entry
    // is the caller reserving a slot, and no click could bring it back.
    "trailing Unsorted entries are pruned only where the reader could restore them" in {
        val locked: List[String] => Boolean = _ != List("L")
        val withLocked                      = spec("A" -> Ascending, "L" -> Unsorted)
        assert(SortKey.cycle(withLocked, List("A"), removable = true, locked) ==
            spec("A" -> Descending, "L" -> Unsorted))
        assert(cycle(spec("A" -> Ascending, "B" -> Unsorted), "A") == spec("A" -> Descending))
    }

    // SEVERAL means several the reader can act on. A locked column beside their single key
    // would otherwise take the plain click's third state away, for a reason nothing on
    // screen explains.
    "a locked column does not cost the reader's only key its full cycle" in {
        val locked: List[String] => Boolean = _ != List("L")
        val start                           = spec("L" -> Ascending, "A" -> Ascending)
        val down                            = SortKey.plain(start, List("A"), removable = true, locked)
        assert(down == spec("L" -> Ascending, "A" -> Descending))
        assert(SortKey.plain(down, List("A"), removable = true, locked) == spec("L" -> Ascending))
    }

end SortKeyTest
