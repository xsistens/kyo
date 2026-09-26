package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

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
        val patches  = new AtomicInteger(0)
        val exchange = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async = Kyo.unit
            private def bump(): Unit     = discard(patches.incrementAndGet())
            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                Sync.defer(bump())
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                Sync.defer(bump())
            override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]   = Present((_, _, _) => bump())
            override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present((_, _, _) => bump())
    end Recording

    "a thousand class channels on one signal share one registration" in {
        Scope.run {
            for
                sel <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").cssClass("hot", sel: Signal[Boolean]))*)
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
                ui  = UI.div(List.fill(fanOut)(UI.span("x").cssClass("hot", sel: Signal[Boolean]))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                before = rec.patches.get
                _ <- sel.set(true)
                // Every patch has already run when the write returns; on the fiber path this still reads `before`.
                after = rec.patches.get
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
                before = rec.patches.get
                _ <- text.set("b")
                after = rec.patches.get
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
                ui  = UI.div(List.fill(fanOut)(UI.span("x").cssClass("hot", sel: Signal[Boolean]))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- sel.set(false)
            // The ref ignores a write equal to its current value, so the fan-out never wakes. A dispatcher that
            // re-delivered on every write would paint nothing wrong and still pay the whole fan-out.
            yield assert(rec.patches.get == 0, s"unchanged write delivered ${rec.patches.get} patches")
        }
    }

    "deduplication happens per channel image: a selection move costs two patches, not a thousand" in {
        Scope.run {
            for
                selected <- Signal.initRef(0)
                rec = new Recording
                ui  = UI.div((0 until fanOut).map(i => UI.span("x").cssClass("hot", selected.map(_ == i)))*)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- selected.set(3)
                after3 = rec.patches.get
                _ <- selected.set(8)
                after8 = rec.patches.get
            // The row losing the selection and the row gaining it. The signal starts at 0, so the first move
            // deselects row 0. Comparing source values instead of each channel's image would deliver to all
            // thousand rows on every move and produce the same output.
            yield assert(after3 == 2 && after8 == 4, s"first=$after3 second=$after8 (expected 2 then 4)")
        }
    }

    "closing the region releases the shared registration" in {
        for
            sel <- Signal.initRef(false)
            rec = new Recording
            ui  = UI.div(List.fill(fanOut)(UI.span("x").cssClass("hot", sel: Signal[Boolean]))*)
            _      <- Scope.run(ReactiveUI.normalize(ui, Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit))
            parked <- sel.waiters
            _      <- sel.set(true)
        // The registration sits on a masked promise that nothing interrupts, so a released channel that left it
        // armed would keep its subscriber reachable for the page's lifetime.
        yield assert(parked == 0 && rec.patches.get == 0, s"parked=$parked patches=${rec.patches.get}")
    }

end ReactiveChannelCostTest
