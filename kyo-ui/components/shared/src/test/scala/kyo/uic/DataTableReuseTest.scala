package kyo.uic

import java.util.concurrent.atomic.AtomicInteger
import kyo.*
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** What one emission COSTS: how many rows a table renders when one row changed.
  *
  * Every other suite here asks what the table renders. This one asks how much of it the table renders
  * again, which no assertion on markup can see: the same HTML comes out either way. The unit is a
  * count of column-body invocations, taken around the real engine (`normalize` plus `subscribe`),
  * because reuse is an engine property and a pure walk of the tree re-renders everything by
  * construction.
  *
  * The measured shape is the one the spotify example hits per click: forty bound rows, a selection
  * signal, one key added to it. That changes the markup of exactly one row.
  *
  * A count is read only once every region the table subscribed has handled the last write, which
  * the engine says itself: an observer parks on a ref again only after it handled the value before,
  * so each test waits for every ref it binds to hold as many waiters as the table subscribed on it.
  */
class DataTableReuseTest extends UicTest:

    final case class Item(id: String, name: String) derives CanEqual

    private val rowCount = 40
    private val items    = List.tabulate(rowCount)(i => Item(s"k$i", s"row-$i"))

    /** Counts column-body calls. The body is a pure `A => String` and cannot suspend, so it bumps the counter
      * directly; the test reads it, and starts it over, inside an effect.
      */
    final private class Renders:
        private val n                     = new AtomicInteger(0)
        def bump(): Unit                  = discard(n.incrementAndGet())
        def take(using Frame): Int < Sync = Sync.defer(n.getAndSet(0))
    end Renders

    /** Waits until `n` observers wait on `ref`. An observer parks on its signal again only once it has handled the
      * value before, so this is the point where everything it does for that value is done: a keyed body has adopted
      * or patched its rows. `n` counts registrations, not regions: a region reading the ref twice through one
      * `combineLatest` registers twice. The bound only turns a count that is never reached into a failure instead of a
      * hang; it is not what passes the test.
      */
    private def parked(ref: Signal.SignalRef[?], n: Int)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(ref.waiters.map(_ == n), within = 30.seconds)

    /** [[parked]] for every ref the table binds. Once each has its count nothing moves until the next write. */
    private def settled(waits: (Signal.SignalRef[?], Int)*)(using Frame, kyo.test.AssertScope): Unit < Async =
        Kyo.foreachDiscard(waits)((ref, n) => parked(ref, n))

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

    // Was 80 for these forty rows (two full passes per write) before the body's reactive unit
    // became the row.
    //
    // The waiters: the error ref is read by the wired region alone. Rows and selection meet in one
    // combineLatest, read by the keyed body and the two chrome regions (the rowKey card, and the
    // frozen and lazy cards), so each ref carries three.
    "one selected row costs one rendered row" in {
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                sel  <- Signal.initRef(Set.empty[Item])
                err  <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).selection(sel)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root  <- ReactiveUI.normalize(ui, Seq.empty)
                _     <- ReactiveUI.subscribe(root, quiet)
                _     <- settled(err -> 1, rows -> 3, sel -> 3)
                first <- counted.take
                _     <- sel.set(Set(items(5)))
                _     <- settled(err -> 1, rows -> 3, sel -> 3)
                after <- counted.take
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
                err    <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                // Five rows fit the viewport (200 / 40), with no overscan. A windowed body resolves the
                // rows around itself, so the waiters are one each: the wired region on the error ref,
                // the rows region, and the keyed window mapped from the scroll ref.
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                    .scrollHeight("200px").scrollRows(40, overscan = 0)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- settled(err -> 1, rows -> 1, scroll -> 1)
                _    <- counted.take
                // Exactly one row's worth: the window moves from [0, n) to [1, n + 1).
                _     <- scroll.set(40.0)
                _     <- settled(err -> 1, rows -> 1, scroll -> 1)
                after <- counted.take
            yield assert(
                after == 1,
                s"scrolling by one row rendered $after rows; the window holds six"
            )
            end for
        }
    }

    "one changed row in a rows emission costs one rendered row" in {
        // The case that matters most and was the last to be fixed. A caller whose selection lives
        // in the DATA rather than in a selection ref (a `@client` field on the row, with
        // `selection` derived from the same signal) never emits a selection, only rows. While the
        // rows were resolved above the body like sort, filter and page, an emission rebuilt the
        // body and the keyed list inside it, whose row registry was then new and empty, and that
        // caller saw none of the reuse the tests above measure. It was 40 here.
        //
        // The waiters: one on the error ref (the wired region), three on the rows (the keyed body
        // and the two chrome regions, each over the rows signal alone).
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                err  <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- settled(err -> 1, rows -> 3)
                _    <- counted.take
                // One row's value changes; the other thirty-nine are equal to what they were.
                _     <- rows.set(items.updated(5, Item("k5", "row-5-changed")))
                _     <- settled(err -> 1, rows -> 3)
                after <- counted.take
            yield assert(
                after == 1,
                s"one row of $rowCount changed and $after rows were rendered"
            )
            end for
        }
    }

    "the spotify example's own configuration, one client field written" in {
        // The shape that motivated all of this, assembled feature for feature: the selection is
        // derived from the SAME signal as the rows, because it lives in the row's own data, and the
        // table also carries a sort ref, a context row, a shift anchor and modifier selection.
        //
        // The waiters: the keyed body and the two chrome regions read one combineLatest of rows,
        // selection and context row, and the selection is a map of the rows, so each of the three
        // registers on the rows twice (six) and on the context row once (three). The sort ref has
        // its region around the body (one), the error ref the wired region (one). Only a click
        // reads the anchor, so nothing waits on it.
        Scope.run {
            val counted                              = new Renders
            def selectedOf(rs: Seq[Item]): Set[Item] =
                rs.filter(_.name.endsWith("*")).toSet
            for
                rows   <- Signal.initRef[Seq[Item]](items)
                sort   <- Signal.initRef(List.empty[uic.SortKey])
                ctx    <- Signal.initRef(Absent: Maybe[Item])
                anchor <- Signal.initRef(Absent: Maybe[Item])
                err    <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().id("t").rows(rows).rowKey(_.id).sort(sort)
                    .stripedRows(true).size(uic.Size.Small).contextMenuRow(ctx)
                    .emptyContent("Nothing to play here.")
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .selectionMode(uic.SelectionMode.Multiple)
                    .metaKeySelection(true)
                    .selectionAnchor(anchor)
                    .selection(rows.map(selectedOf))
                    .onSelectionChange(_ => ())
                    .wired("t", Map.empty, err, _ => (), anchor = Present(anchor))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- settled(err -> 1, rows -> 6, ctx -> 3, sort -> 1)
                _    <- counted.take
                // A click writes the client field on one row, and the query re-emits the list.
                _     <- rows.set(items.updated(5, Item("k5", "row-5*")))
                _     <- settled(err -> 1, rows -> 6, ctx -> 3, sort -> 1)
                after <- counted.take
            yield assert(after == 1, s"one row was written and $after of $rowCount rendered")
            end for
        }
    }

    "appending a row costs the row appended" in {
        // The list grows and nothing else moves, which is what a fetchMore does. It used to cost
        // every row: the key list a shift-range spans was carried in each row's spec, so a list
        // one longer made all of them unequal. Nothing reads it there (the range reads it live),
        // and it is gone.
        //
        // The waiters are those of the first test: one on the error ref, three each on the rows and
        // the selection, which the keyed body and the two chrome regions read through one
        // combineLatest.
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                sel  <- Signal.initRef(Set.empty[Item])
                err  <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).selection(sel)
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root  <- ReactiveUI.normalize(ui, Seq.empty)
                _     <- ReactiveUI.subscribe(root, quiet)
                _     <- settled(err -> 1, rows -> 3, sel -> 3)
                _     <- counted.take
                _     <- rows.set(items :+ Item("k40", "row-40"))
                _     <- settled(err -> 1, rows -> 3, sel -> 3)
                after <- counted.take
            yield assert(after == 1, s"one row was appended to $rowCount and $after rendered")
            end for
        }
    }

    "a frozen row group reuses its own rows" in {
        // Two bodies, two keyed lists, two key namespaces: a frozen row may share a key with a
        // body row and that is a diagnosed state, not a silent collision, because the regions are
        // separate. Here the same rows are in both, which is the worst case for telling them apart.
        //
        // The waiters: the frozen group is a second keyed list over the same combineLatest as the
        // body, so the rows and the selection carry four each (two bodies, two chrome regions), and
        // the error ref one.
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef[Seq[Item]](items)
                sel  <- Signal.initRef(Set.empty[Item])
                err  <- Signal.initRef(Absent: Maybe[(CellKey, kyo.uic.form.FieldError)])
                ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).selection(sel)
                    .frozenRows(items.take(3))
                    .columns(uic.column("Name") { i =>
                        counted.bump(); i.name
                    })
                    .wired("t", Map.empty, err, _ => ())
                root  <- ReactiveUI.normalize(ui, Seq.empty)
                _     <- ReactiveUI.subscribe(root, quiet)
                _     <- settled(err -> 1, rows -> 4, sel -> 4)
                first <- counted.take
                // The row is in BOTH bodies, so both have one row to draw again and no more.
                _     <- sel.set(Set(items(1)))
                _     <- settled(err -> 1, rows -> 4, sel -> 4)
                after <- counted.take
            yield
                assert(first >= rowCount, s"the first pass renders every row; it rendered $first")
                assert(after == 2, s"one row of two bodies changed and $after rows were rendered")
            end for
        }
    }

end DataTableReuseTest
