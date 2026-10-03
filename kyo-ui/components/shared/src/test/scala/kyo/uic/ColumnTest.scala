package kyo.uic

/** The pure header layout behind a multi-row column header: which cell sits in which row,
  * and how far it reaches. What the rows turn into is pinned in GoldenRenderTest.
  */
class ColumnTest extends UicTest:

    private case class R(a: String)

    private def col(h: String): Column[R, TextAnyTable]            = Column[R](h)(_.a)
    private def grp(l: String)(cs: ColumnTree[R]*): HeaderGroup[R] = HeaderGroup[R](l)(cs*)

    /** Every header cell as (label, colspan, rowspan), one list per header row. */
    private def spans(ns: ColumnTree[R]*): List[List[(String, Int, Int)]] =
        ColumnTree.spans(ns.toList).map(_.map(s => (s.node.label, s.colspan, s.rowspan)))

    /** The same, over the tree left after pruning. */
    private def prunedSpans(keep: Int => Boolean)(ns: ColumnTree[R]*): List[List[(String, Int, Int)]] =
        val kept = ColumnTree.prune(ns.toList, keep)
        ColumnTree.spans(kept).map(_.map(s => (s.node.label, s.colspan, s.rowspan)))

    "a table without columns has no header rows" in {
        assert(spans() == Nil)
    }

    "ungrouped columns are one row of single cells" in {
        assert(spans(col("A"), col("B")) == List(List(("A", 1, 1), ("B", 1, 1))))
    }

    "a group is as wide as the columns under it, which sit in the row below" in {
        assert(spans(grp("G")(col("A"), col("B"))) == List(
            List(("G", 2, 1)),
            List(("A", 1, 1), ("B", 1, 1))
        ))
    }

    // The whole point of deriving the spans: a column beside a group has no second list to
    // declare its height in, it simply reaches the bottom of whatever header it is in.
    "an ungrouped column beside a group reaches down through every header row" in {
        assert(spans(col("A"), grp("G")(col("B"), col("C"))) == List(
            List(("A", 1, 2), ("G", 2, 1)),
            List(("B", 1, 1), ("C", 1, 1))
        ))
    }

    "groups nest, and each level is one row deeper" in {
        assert(spans(grp("Sales")(grp("2025")(col("Q1"), col("Q2")), col("Total")), col("Code")) == List(
            List(("Sales", 3, 1), ("Code", 1, 3)),
            List(("2025", 2, 1), ("Total", 1, 2)),
            List(("Q1", 1, 1), ("Q2", 1, 1))
        ))
    }

    // A group with nothing under it spans nothing, so it cannot be rendered at all. The
    // table names it in a diagnostic card rather than letting the label go missing quietly.
    "a group holding no column occupies no cell and is reported instead" in {
        assert(spans(grp("Empty")(), col("A")) == List(List(("A", 1, 1))))
        assert(ColumnTree.emptyGroups(List(grp("Empty")(), col("A"))) == List("Empty"))
    }

    "only the outermost empty group is reported, since the ones inside it are its own doing" in {
        assert(ColumnTree.emptyGroups(List(grp("Outer")(grp("Inner")()))) == List("Outer"))
    }

    "the leaves keep the order they were written in, whatever the nesting" in {
        val tree = List[ColumnTree[R]](col("A"), grp("G")(grp("H")(col("B"), col("C")), col("D")))
        assert(tree.flatMap(_.leaves).map(_.headerV) == List("A", "B", "C", "D"))
    }

    // The path is what the sort spec addresses a column by, so a column in no group keeps
    // exactly its header and every existing single-part spec keeps meaning what it meant.
    "a column's path is the group labels around it followed by its own header" in {
        val tree = List[ColumnTree[R]](col("Code"), grp("Sales")(grp("2025")(col("Q1")), col("Total")))
        assert(ColumnTree.leafPaths(tree).map(_._1) == List(
            List("Code"),
            List("Sales", "2025", "Q1"),
            List("Sales", "Total")
        ))
        assert(ColumnTree.leafPaths(tree).map(_._2.headerV) == List("Code", "Q1", "Total"))
    }

    // The same header under two groups is two paths, which is what lets a group carry the
    // disambiguation instead of the cell label.
    "one header under two groups yields two distinct paths" in {
        val tree = List[ColumnTree[R]](grp("2024")(col("Q1")), grp("2025")(col("Q1")))
        assert(ColumnTree.leafPaths(tree).map(_._1) == List(List("2024", "Q1"), List("2025", "Q1")))
    }

    "the header cells carry the same paths the leaves do" in {
        val tree = List[ColumnTree[R]](col("Code"), grp("Sales")(col("Q1")))
        assert(ColumnTree.spans(tree).map(_.map(_.path)) == List(
            List(List("Code"), List("Sales")),
            List(List("Sales", "Q1"))
        ))
    }

    // Pruning is by leaf POSITION and not by label: two columns under different groups
    // share a header, and hiding one of them may not take the other with it.
    /** Every header cell as (label, leaf offset), one list per header row: which column
      * of the body each cell starts over.
      */
    private def offsets(ns: ColumnTree[R]*): List[List[(String, Int)]] =
        ColumnTree.spans(ns.toList).map(_.map(s => (s.node.label, s.at)))

    // The rows do not tile the same columns: a column beside a group reaches down through
    // the header and appears in the FIRST row only, so a second row that counted its own
    // cells from zero would put every cell under it over the wrong column.
    "a header cell knows which column it starts over, whatever row it sits in" in {
        assert(
            offsets(col("A"), grp("G")(col("B"), col("C")), col("D")) ==
                List(List(("A", 0), ("G", 1), ("D", 3)), List(("B", 1), ("C", 2)))
        )
    }

    "nested groups count from their parent, not from the row" in {
        assert(
            offsets(col("A"), grp("G")(grp("H")(col("B"), col("C")), col("D"))) ==
                List(List(("A", 0), ("G", 1)), List(("H", 1), ("D", 3)), List(("B", 1), ("C", 2)))
        )
    }

    /** The leaves of a reordered tree, by header, which is what a reader sees left to
      * right.
      */
    private def ordered(order: List[List[String]])(ns: ColumnTree[R]*): List[List[String]] =
        ColumnTree.leafPaths(ColumnTree.reorder(ns.toList, order)).map(_._1)

    "an empty order leaves the columns where they were authored" in {
        assert(ordered(Nil)(col("A"), col("B")) == List(List("A"), List("B")))
    }

    "the order the columns are named in is the order they render in" in {
        assert(ordered(List(List("B"), List("A")))(col("A"), col("B")) == List(List("B"), List("A")))
    }

    // A partial order means "these first", which is what lets a caller open a table on one
    // column without writing every other one down.
    "a column the order does not name keeps its authored place behind the ones it does" in {
        assert(
            ordered(List(List("C")))(col("A"), col("B"), col("C")) ==
                List(List("C"), List("A"), List("B"))
        )
    }

    "a group travels with its columns, and its place is its first one's" in {
        assert(
            ordered(List(List("G", "B")))(col("A"), grp("G")(col("B"), col("C"))) ==
                List(List("G", "B"), List("G", "C"), List("A"))
        )
    }

    // One header cell cannot sit in two places, so an order that asks for a group's
    // columns apart brings them back together where the first of them was asked for.
    "an order that splits a group renders it whole" in {
        assert(
            ordered(List(List("G", "B"), List("A"), List("G", "C")))(grp("G")(col("B"), col("C")), col("A")) ==
                List(List("G", "B"), List("G", "C"), List("A"))
        )
    }

    "columns inside a group reorder among themselves" in {
        assert(
            ordered(List(List("G", "C"), List("G", "B")))(grp("G")(col("B"), col("C"))) ==
                List(List("G", "C"), List("G", "B"))
        )
    }

    "prune drops the columns it rejects, counting leaves in the order they were written" in {
        val tree = List[ColumnTree[R]](col("A"), grp("G")(col("B"), col("C")), col("D"))
        assert(ColumnTree.prune(tree, i => i != 2).flatMap(_.leaves).map(_.headerV) == List("A", "B", "D"))
        assert(ColumnTree.prune(tree, _ => true).flatMap(_.leaves).map(_.headerV) == List("A", "B", "C", "D"))
        assert(ColumnTree.prune(tree, _ => false) == Nil)
    }

    "a group narrows to the columns it keeps, and the header rows follow" in {
        assert(prunedSpans(i => i != 1)(col("A"), grp("G")(col("B"), col("C"))) == List(
            List(("A", 1, 2), ("G", 1, 1)),
            List(("C", 1, 1))
        ))
    }

    // A header cell over nothing spans nothing, so a group that loses every column leaves
    // with them, and the columns beside it stop reaching down a row that is no longer there.
    "a group that loses every column disappears" in {
        assert(prunedSpans(i => i == 0)(col("A"), grp("G")(col("B"), col("C"))) == List(List(("A", 1, 1))))
    }

    // The group that never had a column is a different thing from one hidden away, and it
    // is a mistake: pruning leaves it standing so the diagnostic still names it.
    "a group authored with no column survives pruning, since it is still reported" in {
        val tree = List[ColumnTree[R]](grp("Empty")(), col("A"))
        assert(ColumnTree.emptyGroups(ColumnTree.prune(tree, _ => true)) == List("Empty"))
        assert(ColumnTree.emptyGroups(ColumnTree.prune(tree, _ => false)) == List("Empty"))
    }

    "a column is one row tall and a group one more than its tallest child" in {
        assert(col("A").height == 1)
        assert(grp("G")(col("A")).height == 2)
        assert(grp("G")(col("A"), grp("H")(col("B"))).height == 3)
    }
end ColumnTest
