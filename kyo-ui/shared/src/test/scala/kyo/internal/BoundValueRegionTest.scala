package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

/** The element region of a two-way-bound field (`input.value(ref)`, `checkbox.checked(ref)`).
  *
  * Such an element re-renders from its ref on every ref change, and the emitted UI is deliberately the SAME
  * object each time — it keeps its `Bound.Ref` so the rendered HTML carries the auto-binding event markers, and
  * the value is read afresh at render time. That makes the region the one place where the emitted value cannot
  * be used to detect change, and an observation that deduplicates on it delivers the first paint and then goes
  * silent: the field stops tracking its own ref, with nothing failing loudly.
  *
  * The browser suite covers this end to end, but only through a 9-second attribute assertion that reports
  * "expected hi, got ''" — which says nothing about where the emission was lost. These count the emissions.
  */
class BoundValueRegionTest extends kyo.test.Test[Any]:

    final private class Recording:
        val changes = new AtomicInteger(0)
        val exchange = new UIExchange:
            def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async =
                Sync.defer(discard(changes.incrementAndGet()))
    end Recording

    "a value-bound input re-renders on EVERY ref change, not just the first" in {
        Scope.run {
            for
                ref <- Signal.initRef("")
                rec = new Recording
                root <- ReactiveUI.normalize(UI.div(UI.input.id("i").value(ref)), Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- Async.sleep(100.millis)
                base = rec.changes.get
                _ <- ref.set("first")
                _ <- assertEventually(Sync.defer(rec.changes.get >= base + 1))
                after1 = rec.changes.get
                _ <- ref.set("second")
                // The second edit is the one that matters: the first arrives even when change detection has
                // collapsed onto the emitted value, because there is no previous emission to compare against.
                _ <- assertEventually(Sync.defer(rec.changes.get >= after1 + 1))
                after2 = rec.changes.get
                _ <- ref.set("third")
                _ <- assertEventually(Sync.defer(rec.changes.get >= after2 + 1))
            yield succeed
        }
    }

    "a checked-bound checkbox re-renders on every ref change" in {
        Scope.run {
            for
                ref <- Signal.initRef(false)
                rec = new Recording
                root <- ReactiveUI.normalize(UI.div(UI.checkbox.id("c").checked(ref)), Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- Async.sleep(100.millis)
                base = rec.changes.get
                _ <- ref.set(true)
                _ <- assertEventually(Sync.defer(rec.changes.get >= base + 1))
                after1 = rec.changes.get
                // Booleans make the trap sharpest: two values, so a collapsed observation looks like it works
                // until the ref returns to a value it has already held.
                _ <- ref.set(false)
                _ <- assertEventually(Sync.defer(rec.changes.get >= after1 + 1))
                after2 = rec.changes.get
                _ <- ref.set(true)
                _ <- assertEventually(Sync.defer(rec.changes.get >= after2 + 1))
            yield succeed
        }
    }

    "an element with no bound ref emits nothing" in {
        Scope.run {
            for
                rec  <- Sync.defer(new Recording)
                root <- ReactiveUI.normalize(UI.div(UI.input.id("i").value("static")), Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- Async.sleep(200.millis)
            // A const node must not acquire a region of its own just because the bound-ref case now does.
            yield assert(rec.changes.get == 0)
        }
    }

end BoundValueRegionTest
