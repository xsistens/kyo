package kyo

import kyo.Browser.*
import kyo.UI.foreachKeyed
import scala.language.implicitConversions

/** A keyed list of `<tr>` inside an authored `<tbody>`, in a real browser.
  *
  * The repo has keyed lists over `<li>` and reactive rows over `<tr>`, but never both at once, and a
  * component about to render a table body this way needs the combination pinned rather than inferred.
  * Two things have to hold together and only one of them is covered elsewhere:
  *
  *   - The MARKER invariant. A region directly under `<table>` gets a synthetic `<tbody>` range host;
  *     inside an authored `<tbody>` it gets plain comment anchors, which are legal siblings of `<tr>`.
  *     `HtmlRendererReactiveRangesTest` pins that for the initial HTML string; this pins that the
  *     browser then parses it into the tree the CSS assumes — rows as DIRECT children of the tbody,
  *     with nothing wrapping them.
  *   - The REUSE. A keyed row whose key and value both survive an emission must not be RENDERED
  *     again. Placement suites cannot see the difference, and neither can the DOM: rows are morphed
  *     in place either way, so every node survives whether or not its content was recomputed. The
  *     only honest reading is a count of render-function calls, which is what the second test takes.
  *
  * Node survival is read by stamping a JS expando on each row. Deliberately not an attribute: a
  * re-render rewrites the attributes of a node it KEEPS, so an attribute probe reports a reused node
  * as a lost one. That mistake cost a whole measurement in the spotify example before it was caught.
  */
class ListPatchTableDomTest extends UITest:

    final private case class Row(id: String, name: String) derives CanEqual

    /** Counts render-function calls. A row the region retained is never handed to it. */
    final private class Renders:
        private var n     = 0
        def bump(): Unit  = synchronized { n += 1 }
        def get: Int      = synchronized(n)
        def reset(): Unit = synchronized { n = 0 }
    end Renders

    /** Rows in an authored tbody, plus one button per mutation so a test drives a real signal write. */
    private def app(
        initial: Seq[Row],
        counted: Maybe[Renders],
        mutations: (String, Seq[Row])*
    ): UI < Async =
        for rows <- Signal.initRef(Chunk.from(initial))
        yield UI.div(
            UI.table(
                UI.tbody(
                    rows.foreachKeyed(_.id) { r =>
                        counted.foreach(_.bump())
                        UI.tr(UI.td(r.name).cssClass("cell")).cssClass("row")
                    }
                ).id("body")
            ).id("table"),
            UI.div(mutations.map((id, next) => UI.button(id).id(id).onClick(rows.set(Chunk.from(next))))*)
        )

    private def app(initial: Seq[Row], mutations: (String, Seq[Row])*): UI < Async =
        app(initial, Absent, mutations*)

    private val three = Seq(Row("a", "A"), Row("b", "B"), Row("c", "C"))

    /** Marks every current row, so a later count says how many of THOSE nodes are still there. */
    private val stamp =
        """(() => { const rs = document.querySelectorAll("#body > tr.row");
          |  rs.forEach((r, i) => { r.__kept = i; }); return rs.length; })()""".stripMargin

    private val kept =
        """(() => Array.from(document.querySelectorAll("#body > tr.row"))
          |  .filter(r => r.__kept !== undefined).length)()""".stripMargin

    "rows land as direct children of the authored tbody" in {
        withUI(app(three)) {
            for
                // Both selectors have to agree, or something wraps the rows.
                _ <- Browser.assertCount(Selector.css("#body > tr.row"), 3)
                _ <- Browser.assertCount(Selector.css("#table tr.row"), 3)
                // The region's own anchors are comment nodes, so the tbody has no element children
                // besides the rows: this is the shape `.p-datatable-tbody > tr > td` depends on.
                n <- Browser.evalInt("""document.querySelectorAll("#body > *:not(tr)").length""")
            yield assert(n == 0, s"the tbody carries $n non-row element children")
        }
    }

    "one changed row is the only row rendered again" in {
        // Only the middle row's value changes; a and c are equal to what they were.
        val counted = new Renders
        withUI(app(three, Present(counted), "go" -> Seq(Row("a", "A"), Row("b", "B2"), Row("c", "C")))) {
            for
                first <- Browser.evalInt(stamp).map(n => (n, counted.get))
                _ = counted.reset()
                _     <- Browser.click(Selector.id("go"))
                _     <- Browser.assertText(Selector.css("#body > tr.row:nth-child(2) > td"), "B2")
                after <- Browser.evalInt(kept)
            yield
                val (stamped, initialRenders) = first
                assert(stamped == 3)
                // Not compared against the first pass: bringing the list up calls the render function
                // several times per row (walk, then paint), so that number says nothing about what an
                // EMISSION costs. What an emission costs is this one.
                assert(initialRenders > 0, "the first pass rendered nothing at all")
                assert(counted.get == 1, s"one row changed and ${counted.get} rows were rendered")
                // And nothing was thrown away to do it: the rows are morphed, including the changed one.
                assert(after == 3, s"$after of 3 nodes survived")
        }
    }

    "reordering keeps every node and moves it" in {
        withUI(app(three, "go" -> Seq(Row("c", "C"), Row("a", "A"), Row("b", "B")))) {
            for
                before <- Browser.evalInt(stamp)
                _      <- Browser.click(Selector.id("go"))
                _      <- Browser.assertPageTextOrder(Seq("C", "A", "B"))
                after  <- Browser.evalInt(kept)
            yield
                assert(before == 3)
                assert(after == 3, s"nothing changed but the order, and $after of 3 nodes survived")
        }
    }

    "appending leaves the rows already there untouched" in {
        // The `fetchMore` shape: this is the emission an unkeyed body pays for in full.
        withUI(app(three, "go" -> (three :+ Row("d", "D")))) {
            for
                before <- Browser.evalInt(stamp)
                _      <- Browser.click(Selector.id("go"))
                _      <- Browser.assertCount(Selector.css("#body > tr.row"), 4)
                after  <- Browser.evalInt(kept)
            yield
                assert(before == 3)
                assert(after == 3, s"three rows were appended to and $after of them survived")
        }
    }

end ListPatchTableDomTest
