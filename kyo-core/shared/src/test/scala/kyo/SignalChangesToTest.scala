package kyo

import java.util.concurrent.atomic.AtomicInteger

/** `changesTo`, which carries a CONSTANT while tracking a source's changes.
  *
  * It exists because observation deduplicates on the emitted value, and a caller whose emitted value is a stable
  * handle — one whose content is rebuilt from the source at delivery time, as a signal-bound form field is —
  * gets exactly one delivery out of `map(_ => constant)` and then silence. That is the correct behaviour for a
  * projection and a silent failure for such a caller, so both halves are pinned here: what `changesTo` does, and
  * what `map` does, side by side.
  */
class SignalChangesToTest extends kyo.test.Test[Any]:

    private def countDeliveries[A](sig: Signal[A])(using Frame): AtomicInteger < (Async & Scope) =
        for
            seen <- Sync.defer(new AtomicInteger(0))
            _    <- Fiber.init(sig.observe(v => Sync.defer(discard(seen.incrementAndGet())).andThen(discard(v))))
            _    <- Async.sleep(100.millis)
        yield seen

    "delivers on every source change even though its own value never changes" in {
        Scope.run {
            for
                ref  <- Signal.initRef(0)
                seen <- countDeliveries(ref.changesTo("constant"))
                base = seen.get
                _ <- ref.set(1)
                _ <- assertEventually(Sync.defer(seen.get == base + 1))
                _ <- ref.set(2)
                _ <- assertEventually(Sync.defer(seen.get == base + 2))
                _ <- ref.set(3)
                _ <- assertEventually(Sync.defer(seen.get == base + 3))
            yield assert(base == 1)
        }
    }

    "map of the same shape goes silent after the first delivery" in {
        Scope.run {
            for
                ref  <- Signal.initRef(0)
                seen <- countDeliveries(ref.map(_ => "constant"))
                base = seen.get
                _ <- ref.set(1)
                _ <- ref.set(2)
                _ <- Async.sleep(300.millis)
            // Not a defect: the projection's value really did not move, and suppressing those deliveries is what
            // keeps one selection change off a thousand rows. It is only wrong for a caller that reads the SOURCE
            // at delivery time — hence changesTo.
            yield assert(base == 1 && seen.get == 1)
        }
    }

    "a baseline equal to the value skips exactly one delivery, not all of them" in {
        Scope.run {
            for
                ref  <- Signal.initRef(0)
                seen <- Sync.defer(new AtomicInteger(0))
                sig = ref.changesTo("constant")
                // The baseline says "I already painted this": the initial emission is skipped, and every source
                // change after it must still arrive. Skipping them all is the bug this whole method exists for.
                _ <- Fiber.init(sig.observe(Present("constant"), Signal.defaultRepairInterval)(_ =>
                    Sync.defer(discard(seen.incrementAndGet()))
                ))
                _ <- Async.sleep(100.millis)
                _ = assert(seen.get == 0)
                _ <- ref.set(1)
                _ <- assertEventually(Sync.defer(seen.get == 1))
                _ <- ref.set(2)
                _ <- assertEventually(Sync.defer(seen.get == 2))
            yield succeed
        }
    }

    "an unchanged write delivers nothing" in {
        Scope.run {
            for
                ref  <- Signal.initRef(0)
                seen <- countDeliveries(ref.changesTo("constant"))
                _    <- ref.set(0)
                _    <- Async.sleep(200.millis)
            // The source gates on inequality, so there is no change to carry.
            yield assert(seen.get == 1)
        }
    }

end SignalChangesToTest
