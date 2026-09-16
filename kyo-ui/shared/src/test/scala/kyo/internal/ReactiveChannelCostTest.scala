package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

/** The COST of the reactive channel binding, as invariants rather than timings.
  *
  * What the callback binding bought is not "faster" in any way a clock measures reliably. It is two exact,
  * countable properties, and both are what took a thousand-row selection from 160 ms to 6 ms:
  *
  *   - **One registration per signal, not per observer.** A thousand channels on one signal park ONE waiter on
  *     its next-promise and are walked from a shared dispatcher. On the fiber path each observer parks its own,
  *     so a write allocates a thousand waiters and CASes a thousand times onto the same chain.
  *   - **Delivery inside the writer's own `set`.** No scheduler hop per observer, so the count is exact the
  *     instant `set` returns.
  *
  * Both are asserted by counting, never by timing: a wall-clock threshold on a shared machine is a coin flip
  * (this campaign already had a benchmark run invalidated by a game running in the background), whereas a
  * registration count is the same on any machine. What the counts are worth was measured separately, on the
  * JVM, by driving the same fan-out through an exchange WITH a sink and one without — the latter being the
  * fiber path every channel used before the binding existed:
  *
  * {{{
  * n      callback                          fiber (the previous path)
  * 100    1 registration, 100/100 in set    100 registrations, 0/100 in set    149us vs   1019us
  * 1000   1 registration, 1000/1000 in set  1000 registrations, 0/1000 in set  280us vs   4292us
  * 5000   1 registration, 5000/5000 in set  5000 registrations, 0/5000 in set  916us vs  18533us
  * }}}
  *
  * Read those times as upper bounds (200us polling granularity), and as an UNDERSTATEMENT of the browser: on
  * the JVM the fiber path has a real multi-threaded scheduler, while Scala.js gives it one macrotask per fiber
  * on one thread, which is where 04_select measured 160.4ms before and 6.5ms after. The write itself does get
  * dearer — at n=1000 `set` returns after 272us instead of 173us — because the fan-out now happens inside it.
  *
  * The regression these guard against has NO functional symptom. If a change makes `bindChannel` decline — a
  * signal that stops being recognised as ref-rooted, an exchange that stops offering a sink — everything still
  * works, just through a fiber per observer again. [[ReactiveChannelBindingTest]] pins that the binding is
  * CORRECT; this pins that it is still being taken, and at what cost.
  */
class ReactiveChannelCostTest extends kyo.test.Test[Any]:

    private val fanOut = 1000

    final private class Recording:
        val patches = new AtomicInteger(0)
        val exchange = new UIExchange:
            def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async = Kyo.unit
            private def bump(): Unit                                                           = discard(patches.incrementAndGet())
            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                Sync.defer(bump())
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                Sync.defer(bump())
            override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]   = Present((_, _, _) => bump())
            override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present((_, _, _) => bump())
    end Recording

    "a thousand class channels on one signal share ONE registration" in {
        Scope.run {
            for
                sel <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(List.fill(fanOut)(UI.span("x").cssClass("hot", sel: Signal[Boolean]))*)
                root   <- ReactiveUI.normalize(ui, Seq.empty)
                _      <- ReactiveUI.subscribe(root, rec.exchange)
                parked <- sel.waiters
            // One shared waiter for the whole fan-out. A fiber per observer would report ~1000 here, and
            // nothing else about the app would look any different.
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
                // No sleep, no assertEventually: every one of the thousand patches has already run by the time
                // the write returns. On the fiber path this reads `before`.
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
                _    <- Async.sleep(50.millis)
            // The ref gates on inequality, so an unchanged write must not wake the fan-out at all — the case a
            // dispatcher that re-delivers on every write would get wrong without ever painting anything wrong.
            yield assert(rec.patches.get == 0, s"unchanged write delivered ${rec.patches.get} patches")
        }
    }

    "deduplication happens per channel IMAGE: a selection move costs two patches, not a thousand" in {
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
            // Two patches per move, never a thousand: the row losing the selection and the row gaining it. The
            // signal starts at 0, so even the first move is a pair — it deselects row 0 on the way. Comparing
            // SOURCE values instead would deliver to all thousand every time, which is the original 04_select
            // cost and produces byte-identical output.
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
        // The registration sits on a masked promise nothing interrupts, so a thousand released channels that
        // left it armed would keep a thousand dead subscribers reachable for the page's lifetime.
        yield assert(parked == 0 && rec.patches.get == 0, s"parked=$parked patches=${rec.patches.get}")
    }

end ReactiveChannelCostTest
