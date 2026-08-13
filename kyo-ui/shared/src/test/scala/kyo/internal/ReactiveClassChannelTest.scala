package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

/** The callback binding of a reactive CLASS channel (`ReactiveUI.bindClassChannel`).
  *
  * A class channel's handler is one `classList.toggle`, so where the exchange offers a synchronous sink it
  * binds as a callback on the signal's next-promise instead of a fiber. Three things have to hold, and none
  * of them is covered by the region/attr tests:
  *
  *   - the patch really lands inside the writer's own `set`, which is the entire point;
  *   - closing the owning Scope releases the registration — it sits on a MASKED promise that nothing
  *     interrupts, so a forgotten release retains the callback and the row behind it;
  *   - an exchange without a synchronous sink (the server transport) still gets the fiber path, unchanged.
  */
class ReactiveClassChannelTest extends kyo.test.Test[Any]:

    /** Counts the two sinks separately, so a test can tell WHICH path ran rather than just that a patch
      * happened. `fast = Absent` reproduces an exchange that has no synchronous sink.
      */
    final private class Recording(fast: Boolean):
        val sync  = new AtomicInteger(0)
        val async = new AtomicInteger(0)
        val exchange = new UIExchange:
            def onChange(path: Seq[String], changed: UI, mount: Boolean)(using Frame): Unit < Async = Kyo.unit
            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                Sync.defer(discard(async.incrementAndGet()))
            override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] =
                if fast then Present((_, _, _) => discard(sync.incrementAndGet())) else Absent
    end Recording

    private def ui(on: Signal[Boolean]) = UI.div(UI.span("x").cssClass("hot", on))

    "patches inside the writer's set, with no scheduler hop" in {
        Scope.run {
            for
                on <- Signal.initRef(false)
                rec = new Recording(fast = true)
                root <- ReactiveUI.normalize(ui(on), Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                // Rendered snapshot is `false`, so the initial emission is skipped on either path.
                before = rec.sync.get
                _ <- on.set(true)
                // Read with NO sleep and NO assertEventually: on the fiber path the patch would still be
                // queued here and this would read 0.
                after = rec.sync.get
            yield assert(before == 0 && after == 1 && rec.async.get == 0)
        }
    }

    "closing the scope releases the registration" in {
        for
            on <- Signal.initRef(false)
            rec = new Recording(fast = true)
            _ <- Scope.run {
                ReactiveUI.normalize(ui(on), Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit)
            }
            waiters <- on.waiters
            _       <- on.set(true)
        // The next-promise is masked and nothing interrupts it, so a binding that forgets to release stays
        // registered here — and keeps patching a subtree that is already gone.
        yield assert(waiters == 0 && rec.sync.get == 0)
    }

    "an exchange without a synchronous sink keeps the fiber path" in {
        Scope.run {
            for
                on <- Signal.initRef(false)
                rec = new Recording(fast = false)
                root <- ReactiveUI.normalize(ui(on), Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- on.set(true)
                _    <- assertEventually(Sync.defer(rec.async.get == 1))
            yield assert(rec.sync.get == 0)
        }
    }

end ReactiveClassChannelTest
