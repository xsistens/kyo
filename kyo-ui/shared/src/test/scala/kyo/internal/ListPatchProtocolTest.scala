package kyo.internal

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.UI.foreachKeyed
import kyo.internal.RecordingExchange.Op

/** The structural list command a keyed Foreach region emits in place of a whole-list fragment.
  *
  * The command exists for its `changed` flags: they are the region's statement of which rows it reused, and a
  * backend spends work in proportion to them. Wrong in the optimistic direction and stale rows stay on screen;
  * wrong in the pessimistic direction and the command buys nothing back. Neither shows up in painted output:
  * a fragment carrying every row paints exactly the same DOM — so these assert on the command itself.
  *
  * [[ForeachReuseTest]] is the sibling: it pins which rows the region re-renders. This pins that the same
  * decision reaches the transport intact.
  */
class ListPatchProtocolTest extends kyo.test.Test[Any]:

    final private class Recording:
        val exchange                 = new RecordingExchange(keepsRows = true)
        private def patches          = exchange.recorded.collect { case op: Op.List => op.rows }
        def fragments: Int           = exchange.recorded.count(_.isInstanceOf[Op.Paint])
        def count: Int               = patches.size
        def keys: Seq[String]        = patches.last.map(_.key)
        def changedKeys: Seq[String] = patches.last.filter(_.changed).map(_.key)
    end Recording

    /** Waits until the list region waits on `rows` again, which it does only once it has handled the value before: the first
      * time, once it has adopted the rows the initial paint put on screen.
      */
    private def parked(rows: Signal.SignalRef[?])(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(rows.waiters.map(_ == 1))

    /** Drive one keyed list through `mutate` and hand back what the region emitted. The initial paint is
      * already on screen (the region seeds from it), so any patch recorded here describes a real change.
      */
    private def run(initial: Seq[String])(mutate: Signal.SignalRef[Chunk[String]] => Unit < Async)(
        using
        Frame,
        kyo.test.AssertScope
    ): Recording < (Async & Scope) =
        for
            rows <- Signal.initRef(Chunk.from(initial))
            rec = new Recording
            ui  = UI.ul(rows.foreachKeyed(identity)(item => UI.li(item)))
            root <- ReactiveUI.normalize(ui, Seq.empty)
            _    <- ReactiveUI.subscribe(root, rec.exchange)
            _    <- parked(rows)
            _    <- mutate(rows)
            _    <- assertEventually(Sync.defer(rec.count >= 1))
        yield rec

    "appending flags only the added row" in {
        Scope.run {
            run(Seq("a", "b", "c"))(_.getAndUpdate(_.append("d")).unit).map { rec =>
                assert(rec.keys == Seq("a", "b", "c", "d"))
                assert(rec.changedKeys == Seq("d"))
            }
        }
    }

    "reordering flags no row at all" in {
        Scope.run {
            run(Seq("a", "b", "c"))(_.getAndUpdate(c => Chunk.from(c.toSeq.reverse)).unit).map { rec =>
                // The order is the whole payload of a reorder: no row's content changed, so a backend that
                // trusts these flags does nothing but move nodes.
                assert(rec.keys == Seq("c", "b", "a"))
                assert(rec.changedKeys.isEmpty)
            }
        }
    }

    "removing flags no row and drops it from the order" in {
        Scope.run {
            run(Seq("a", "b", "c"))(_.getAndUpdate(c => Chunk.from(c.toSeq.filterNot(_ == "b"))).unit).map { rec =>
                assert(rec.keys == Seq("a", "c"))
                assert(rec.changedKeys.isEmpty)
            }
        }
    }

    "a row that leaves and returns is flagged changed" in {
        Scope.run {
            for
                rec <- run(Seq("a", "b"))(rows =>
                    // Two emissions, not one: a signal is state, so the second set must wait until the first is handled.
                    rows.set(Chunk("a")).andThen(parked(rows)).andThen(rows.set(Chunk("a", "b")))
                )
                _ <- assertEventually(Sync.defer(rec.count >= 2))
            // Its RowInstance was torn down on the way out, so the returning row owns no live DOM and must
            // not be presented as reusable.
            yield assert(rec.keys == Seq("a", "b") && rec.changedKeys == Seq("b"))
        }
    }

    "a changed item value flags exactly its own row" in {
        Scope.run {
            for
                rows <- Signal.initRef(Chunk(("1", "one"), ("2", "two"), ("3", "three")))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(_._1)((_, label) => UI.li(label)))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows)
                _    <- rows.set(Chunk(("1", "one"), ("2", "TWO"), ("3", "three")))
                _    <- assertEventually(Sync.defer(rec.count >= 1))
            // Same key, different item: the key survived but the paint did not, which is the one case where
            // reuse must be refused even though nothing structural moved.
            yield assert(rec.keys == Seq("1", "2", "3") && rec.changedKeys == Seq("2"))
        }
    }

    "duplicate keys fall back to the whole-list fragment" in {
        Scope.run {
            for
                rows <- Signal.initRef(Chunk("a", "b"))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity)(item => UI.li(item)))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows)
                _    <- rows.set(Chunk("a", "a"))
                _    <- assertEventually(Sync.defer(rec.fragments >= 1))
            // A key names a slot, so two rows sharing one key cannot be addressed structurally at all. The
            // fragment path degrades positionally instead of aliasing two rows onto one node.
            yield assert(rec.count == 0)
        }
    }

    "an exchange that does not implement the command still receives every row" in {
        Scope.run {
            for
                seen <- Sync.defer(new AtomicReference(Maybe.empty[UI]))
                rows <- Signal.initRef(Chunk("a", "b"))
                // No onListPatch override: this is the server transport's shape, and it must lose nothing.
                exchange = new RecordingExchange(beforePaint = (_, changed) => Sync.defer(seen.set(Present(changed))))
                ui       = UI.ul(rows.foreachKeyed(identity)(item => UI.li(item)))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, exchange)
                _    <- parked(rows)
                _    <- rows.getAndUpdate(_.append("c"))
                _    <- assertEventually(Sync.defer(seen.get.isDefined))
                keys = seen.get.map {
                    case UI.Ast.Fragment(children) =>
                        children.toSeq.collect { case kc: UI.Ast.KeyedChild[?] => kc.key }
                    case _ => Seq.empty[String]
                }
            yield assert(keys == Present(Seq("a", "b", "c")))
        }
    }

end ListPatchProtocolTest
