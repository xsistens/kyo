package kyo.internal

import kyo.*
import kyo.UI.foreachKeyed
import kyo.internal.RecordingExchange.Op

/** What the callback binding of the reactive channels costs, asserted by counting rather than by timing.
  *
  * The binding is worth two countable properties:
  *
  *   - **One registration per signal, not per observer.** A thousand channels on one signal park one waiter on
  *     it and are walked from a shared dispatcher. On the fiber path each observer parks its own, so a write
  *     allocates a thousand waiters and CASes a thousand times onto the same chain.
  *   - **Delivery inside the writer's own `set`.** There is no scheduler hop per observer, so the count is exact
  *     the moment `set` returns.
  *
  * A lost binding has no functional symptom. If `bindChannel` starts to decline (a signal no longer recognised as
  * rooted in a `SignalRef`, an exchange that stops offering a sink), everything still works, through a fiber per
  * observer. `ReactiveChannelBindingTest` pins that the binding is correct; this suite pins that it is still taken.
  */
class ReactiveChannelCostTest extends kyo.test.Test[Any]:

    private val fanOut = 1000

    final private class Recording:
        // Leaves kept rows in place, as the DOM backend whose sinks this stands in for does. Answering `false` would have the region
        // bind them again after the paint, so a write right after it could land between the two bindings.
        val exchange     = new RecordingExchange(synchronous = true, keepsRows = true)
        def patches: Int = exchange.recorded.count { case _: Op.Attr | _: Op.Class => true; case _ => false }
        def paints: Int  = exchange.recorded.count { case _: Op.Paint | _: Op.List => true; case _ => false }
    end Recording

    "with no devtools sink a write comes back as it is, so the off path costs no wrapper" in {
        val write: Int => Unit = _ => ()
        val wrapped            = ReactiveUI.reporting[Int](Absent, throw new AssertionError("no sink, so no event is built"))(write)
        assert(wrapped eq write)
    }

    "a thousand class channels on one signal share one registration" in {
        Scope.run {
            for
                sel <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").when(sel: Signal[Boolean])(_.cssClass("hot")))*)
                root   <- ReactiveUI.normalize(ui, Seq.empty)
                _      <- ReactiveUI.subscribe(root, rec.exchange)
                parked <- sel.waiters
            // One shared waiter for the whole fan-out; a fiber per observer would park one each.
            yield assert(parked == 1, s"expected 1 shared registration, got $parked")
        }
    }

    "one write reaches every channel before `set` returns" in {
        Scope.run {
            for
                sel <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").when(sel: Signal[Boolean])(_.cssClass("hot")))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                before = rec.patches
                _ <- sel.set(true)
                // Every patch has already run when the write returns; on the fiber path this still reads `before`.
                after = rec.patches
            yield assert(before == 0 && after == fanOut, s"before=$before after=$after (expected 0 -> $fanOut)")
        }
    }

    "attribute channels share the registration on the same terms" in {
        Scope.run {
            for
                text <- Signal.initRef("a")
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").title(text: Signal[String]))*)
                root   <- ReactiveUI.normalize(ui, Seq.empty)
                _      <- ReactiveUI.subscribe(root, rec.exchange)
                parked <- text.waiters
                before = rec.patches
                _ <- text.set("b")
                after = rec.patches
            yield assert(
                parked == 1 && before == 0 && after == fanOut,
                s"parked=$parked before=$before after=$after"
            )
        }
    }

    "a write that does not move the signal costs nothing" in {
        Scope.run {
            for
                sel <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").when(sel: Signal[Boolean])(_.cssClass("hot")))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- sel.set(false)
            // The ref ignores a write equal to its current value, so the fan-out never wakes. A dispatcher that
            // re-delivered on every write would paint nothing wrong and still pay the whole fan-out.
            yield assert(rec.patches == 0, s"unchanged write delivered ${rec.patches} patches")
        }
    }

    "deduplication happens per channel image: a selection move costs two patches, not a thousand" in {
        Scope.run {
            for
                selected <- Signal.initRef(0)
                rec = new Recording
                ui  = UI.div((0 until fanOut).map(i => UI.span("x").when(selected.map(_ == i))(_.cssClass("hot")))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- selected.set(3)
                after3 = rec.patches
                _ <- selected.set(8)
                after8 = rec.patches
            // The row losing the selection and the row gaining it. The signal starts at 0, so the first move
            // deselects row 0. Comparing source values instead of each channel's image would deliver to all
            // thousand rows on every move and produce the same output.
            yield assert(after3 == 2 && after8 == 4, s"first=$after3 second=$after8 (expected 2 then 4)")
        }
    }

    "an evicted keyed row releases its channel bindings" in {
        Scope.run {
            for
                sel  <- Signal.initRef(false)
                rows <- Signal.initRef(Chunk.from(0 until fanOut))
                rec = new Recording
                ui  = UI.div(rows.foreachKeyed(_.toString)(_ => UI.span("x").when(sel: Signal[Boolean])(_.cssClass("hot"))))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- assertEventually(sel.waiters.map(_ == 1))
                _    <- rows.set(Chunk.from(0 until fanOut / 2))
                // The rows leave before the list paints, so a paint means the eviction is done.
                _ <- assertEventually(Sync.defer(rec.paints == 1))
                _ <- sel.set(true)
                after = rec.patches
            // The half that left must not be written to, and the half that stayed must, before `set` returns.
            yield assert(after == fanOut / 2, s"expected ${fanOut / 2} patches after evicting half the rows, got $after")
        }
    }

    "closing the region releases the shared registration" in {
        for
            sel <- Signal.initRef(false)
            rec = new Recording
            ui  = UI.div(List.fill(fanOut)(UI.span("x").when(sel: Signal[Boolean])(_.cssClass("hot")))*)
            _      <- Scope.run(ReactiveUI.normalize(ui, Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit))
            parked <- sel.waiters
            _      <- sel.set(true)
        // The registration sits on a masked promise that nothing interrupts, so a released channel that left it
        // armed would keep its subscriber reachable for the page's lifetime.
        yield assert(parked == 0 && rec.patches == 0, s"parked=$parked patches=${rec.patches}")
    }

end ReactiveChannelCostTest
