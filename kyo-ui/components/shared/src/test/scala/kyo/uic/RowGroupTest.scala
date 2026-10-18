package kyo.uic

/** The pure run decomposition behind the table's row groups, and the path that identifies
  * one group among all others. What the runs turn into (header rows, merged cells,
  * collapsed groups) is pinned in GoldenRenderTest.
  */
class RowGroupTest extends UicTest:

    private def runs(xs: String*): List[(String, List[String])] =
        RowGroup.runs(xs.toList)(_.take(1))

    "an empty list has no runs" in {
        assert(runs() == Nil)
    }

    "equal consecutive keys form one run, in order" in {
        assert(runs("a1", "a2", "b1") == List("a" -> List("a1", "a2"), "b" -> List("b1")))
    }

    "every row is kept exactly once, and the runs concatenate back to the input" in {
        val input = List("a1", "b1", "b2", "c1", "c2", "c3")
        assert(RowGroup.runs(input)(_.take(1)).flatMap(_._2) == input)
    }

    // The run boundary is the point the key CHANGES, not the key itself: grouping reads
    // the row order the sort spec produced rather than imposing one of its own, so a key
    // that comes back later legitimately heads a second run.
    "a key that reappears after another one heads a second run" in {
        assert(runs("a1", "b1", "a2") == List("a" -> List("a1"), "b" -> List("b1"), "a" -> List("a2")))
    }

    "a single key over every row is one run" in {
        assert(runs("a1", "a2", "a3") == List("a" -> List("a1", "a2", "a3")))
    }

    // A merged column keeps a comparison, not a projection, because it has no type
    // parameter to hold the key's type in. Same runs, driven by an equivalence.
    "blocks splits by an equivalence and compares against the run head, not the predecessor" in {
        def blocks(xs: Int*): List[List[Int]] = RowGroup.blocks(xs.toList)((a, b) => a / 10 == b / 10)
        assert(blocks() == Nil)
        assert(blocks(11, 12, 21) == List(List(11, 12), List(21)))
        assert(blocks(11, 21, 12) == List(List(11), List(21), List(12)))
        assert(RowGroup.blocks(List(1, 2, 3))((_, _) => true) == List(List(1, 2, 3)))
        assert(RowGroup.blocks(List(1, 2, 3))((_, _) => false) == List(List(1), List(2), List(3)))
    }

    // The key of a level is labelled by its toString, and that label is also its identity,
    // so a String key passes through untouched and anything else reads as it renders.
    "a level labels any key type through toString, and String is the identity case" in {
        assert(RowGroup[Int, Int](identity).keyF(42) == "42")
        assert(RowGroup[String, String](identity).keyF("Accessories") == "Accessories")
        assert(RowGroup[Int, (Int, Boolean)](i => (i, i > 0)).keyF(1) == "(1,true)")
    }

    // Below the outermost level a bare key is not an identity: the same brand sits under
    // every category that sells one, and collapsing it in one place must leave the others
    // alone. The path is what tells them apart.
    "a path carries its own key last and distinguishes equal keys under different parents" in {
        val one = GroupPath("Accessories", "Rolex")
        val two = GroupPath("Watches", "Rolex")
        assert(one.key == "Rolex" && two.key == "Rolex")
        assert(one.depth == 2)
        assert(one != two)
        assert(GroupPath("Accessories") == GroupPath(List("Accessories")))
    }

    // Keyboard navigation moves over the rows that are ON THE SCREEN, so the walk that
    // decides which those are is the same one the renderer makes, minus the rendering.
    "visible drops the rows of a collapsed group, and keeps render order" in {
        val rows    = List("a1", "a2", "b1", "c1")
        val levels  = List(RowGroup[String, String](_.take(1)))
        val allOpen = Set("a", "b", "c").map(k => GroupPath(List(k)))
        assert(RowGroup.visible(rows, levels, Nil, allOpen, collapsible = true) == rows)
        assert(
            RowGroup.visible(rows, levels, Nil, Set(GroupPath(List("b"))), collapsible = true) == List("b1"),
            "only the open group renders its rows"
        )
        assert(
            RowGroup.visible(rows, levels, Nil, Set.empty, collapsible = false) == rows,
            "a table that cannot collapse shows everything, whatever the set says"
        )
        assert(RowGroup.visible(rows, Nil, Nil, Set.empty, collapsible = true) == rows, "and an ungrouped one too")
    }

    "visible closes an inner group inside an open outer one" in {
        val rows   = List("ax", "ay", "bx")
        val levels = List(RowGroup[String, String](_.take(1)), RowGroup[String, String](_.drop(1)))
        val open   = Set(GroupPath(List("a")), GroupPath(List("a", "x")), GroupPath(List("b")))
        assert(RowGroup.visible(rows, levels, Nil, open, collapsible = true) == List("ax"))
    }

end RowGroupTest
