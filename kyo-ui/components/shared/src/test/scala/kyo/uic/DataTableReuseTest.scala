package kyo.uic

import kyo.*
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** What one emission COSTS: how many rows a table renders when one row changed.
  *
  * Every other suite here asks what the table renders. This one asks how much of it the table renders
  * again, which no assertion on markup can see: the same HTML comes out either way. The unit is a
  * count of column-body invocations, taken around the real engine — `normalize` plus `subscribe` —
  * because reuse is an engine property and a pure walk of the tree re-renders everything by
  * construction.
  *
  * The measured shape is the one the spotify example hits per click: forty bound rows, a selection
  * signal, one key added to it. That changes the markup of exactly one row.
  */
class DataTableReuseTest extends UicTest:

    final case class Item(id: String, name: String) derives CanEqual

    private val rowCount = 40
    private val items    = List.tabulate(rowCount)(i => Item(s"k$i", s"row-$i"))

    /** Counts column-body calls. The body is a pure `A => String`, so the counter is plain state. */
    final private class Renders:
        private var n     = 0
        def bump(): Unit  = synchronized { n += 1 }
        def get: Int      = synchronized(n)
        def reset(): Unit = synchronized { n = 0 }
    end Renders

    /** The engine needs a sink; nothing here asserts on the painted HTML. */
    private val quiet: UIExchange =
        new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async = ()

    // Was 80 for these forty rows — two full passes per write — before the body's reactive unit
    // became the row.
    "one selected row costs one rendered row" in {
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                sel  <- Signal.initRef(Set.empty[String])
                err  <- Signal.initRef(Absent: Maybe[(CellPath, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).selected(sel)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- Async.sleep(100.millis)
                first = counted.get
                _     = counted.reset()
                _ <- sel.set(Set("k5"))
                _ <- Async.sleep(300.millis)
                after = counted.get
            yield
                assert(first >= rowCount, s"the first pass has to render every row; it rendered $first")
                assert(
                    after == 1,
                    s"selecting one row re-rendered $after of $rowCount rows"
                )
            end for
        }
    }

    "a scroll of one row costs one row" in {
        Scope.run {
            val counted = new Renders
            val hundred = List.tabulate(100)(i => Item(s"k$i", s"row-$i"))
            for
                rows   <- Signal.initRef[Seq[Item]](hundred)
                scroll <- Signal.initRef(0.0)
                err    <- Signal.initRef(Absent: Maybe[(CellPath, kyo.uic.form.FieldError)])
                // Five rows fit the viewport (200 / 40), with no overscan.
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                    .scrollHeight("200px").scrollRows(40, overscan = 0)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- Async.sleep(100.millis)
                _ = counted.reset()
                // Exactly one row's worth: the window moves from [0, n) to [1, n + 1).
                _ <- scroll.set(40.0)
                _ <- Async.sleep(300.millis)
                after = counted.get
            yield assert(
                after == 1,
                s"scrolling by one row rendered $after rows; the window holds six"
            )
            end for
        }
    }

    "one changed row in a rows emission costs one rendered row" in {
        // The case that matters most and was the last to be fixed. A caller whose selection lives
        // in the DATA rather than in a selection ref — a `@client` field on the row, with
        // `selected` derived from the same signal — never emits a selection, only rows. While the
        // rows were resolved above the body like sort, filter and page, an emission rebuilt the
        // body and the keyed list inside it, whose row registry was then new and empty, and that
        // caller saw none of the reuse the tests above measure. It was 40 here.
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                err  <- Signal.initRef(Absent: Maybe[(CellPath, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- Async.sleep(100.millis)
                _ = counted.reset()
                // One row's value changes; the other thirty-nine are equal to what they were.
                _ <- rows.set(items.updated(5, Item("k5", "row-5-changed")))
                _ <- Async.sleep(300.millis)
                after = counted.get
            yield assert(
                after == 1,
                s"one row of $rowCount changed and $after rows were rendered"
            )
            end for
        }
    }

end DataTableReuseTest
