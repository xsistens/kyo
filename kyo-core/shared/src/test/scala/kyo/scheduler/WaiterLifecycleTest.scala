package kyo.scheduler

import kyo.*
import kyo.Result.Error

/** The lifecycle of a waiter, from the other end than the existing `waiters` block in
  * IOPromiseTest.
  *
  * That block only asserts what happens to waiters when the PROMISE completes. The opposite
  * direction — the WAITER dies while the promise lives on — is never stated, and that is the gap
  * this file closes.
  *
  * Group A pins the behaviour that must survive any change to waiter bookkeeping. It is also the
  * foundation the planned cancellation tests build on: each one names which property here it
  * relies on, so a regression shows up as a precise failure instead of a mysterious one.
  *
  * Group B covers the direction that had no coverage: the waiter's owner dies first. Both cases
  * fail against a build without `onCompleteCancellable`, which is what makes them a gate rather
  * than decoration.
  *
  * Deliberately no threads and no timing: every case below is deterministic. Concurrency in kyo
  * is expressed with Latch/Gate/Promise, and the one genuinely racy interleaving (cancelling
  * while a flush is in progress) is reachable single-threaded via A1+A2.
  */
class WaiterLifecycleTest extends kyo.test.Test[Any]:

    private val N = 100

    /** Spin until `read` reaches `target`, bounded so a wrong assumption fails an assert instead
      * of hanging the suite.
      */
    private def awaitCount(read: => Int < Sync, target: Int, tries: Int = 500)(using Frame): Unit < Async =
        read.map { got =>
            if got >= target || tries <= 0 then Kyo.unit
            else Async.sleep(2.millis).andThen(awaitCount(read, target, tries - 1))
        }

    "A — properties any waiter-bookkeeping change must preserve" - {

        "A1 callbacks run last-registered-first" in {
            import AllowUnsafe.embrace.danger
            val p     = new IOPromise[Nothing, Int]()
            val order = scala.collection.mutable.ListBuffer.empty[Int]
            for i <- 1 to 3 do p.onComplete(_ => discard(order += i))
            p.completeDiscard(Result.succeed(1))
            // Load-bearing for the planned "cancel mid-flush" case: registering the victim FIRST
            // and the canceller SECOND puts the canceller ahead of it in the flush.
            assert(order.toList == List(3, 2, 1))
        }

        "A2 a callback observes the promise while the flush is still running" in {
            import AllowUnsafe.embrace.danger
            val p    = new IOPromise[Nothing, Int]()
            val seen = scala.collection.mutable.ListBuffer.empty[String]
            p.onComplete(_ => discard(seen += "victim"))
            p.onComplete(_ => discard(seen += s"midflush:${p.waiters()}"))
            p.completeDiscard(Result.succeed(1))
            // Second load-bearing property: the canceller can act on promise state from inside
            // the flush, which is what makes the racy interleaving reachable without threads.
            assert(seen.size == 2)
            assert(seen.head.startsWith("midflush:"))
            assert(seen(1) == "victim")
        }

        "A3 a callback runs exactly once, even if completion is attempted twice" in {
            import AllowUnsafe.embrace.danger
            val p     = new IOPromise[Nothing, Int]()
            var calls = 0
            p.onComplete(_ => calls += 1)
            p.completeDiscard(Result.succeed(1))
            p.completeDiscard(Result.succeed(2))
            assert(calls == 1)
            assert(p.waiters() == 0)
        }

        "A4 a callback also runs when the promise is interrupted" in {
            import AllowUnsafe.embrace.danger
            val p       = new IOPromise[Nothing, Int]()
            var got     = Maybe.empty[Result[Nothing, Int]]
            val failure = Result.Panic(Interrupted(Frame.internal))
            p.onComplete(r => got = Maybe(r))
            p.interruptDiscard(failure)
            // Interrupt is a completion for waiter purposes: the chain is flushed either way.
            assert(got.exists(_.isPanic))
            assert(p.waiters() == 0)
        }

        "A5 completion flushes through a become link" in {
            import AllowUnsafe.embrace.danger
            val p1    = new IOPromise[Nothing, Int]()
            val p2    = new IOPromise[Nothing, Int]()
            var calls = 0
            p1.onComplete(_ => calls += 1)
            p1.becomeDiscard(p2)
            p2.completeDiscard(Result.succeed(1))
            // Basis for cancelling across a link: a waiter registered on p1 is reachable from p2.
            assert(calls == 1)
            assert(p1.waiters() == 0)
        }

        "A6 a masked promise survives the interruption of its subscribers" in {
            import AllowUnsafe.embrace.danger
            val masked = Promise.Unsafe.initMasked[Int, Any]().safe
            for
                fibers <- Kyo.foreach(Chunk.from(1 to N))(_ => Fiber.initUnscoped(masked.get))
                _      <- awaitCount(masked.waiters, N)
                parked <- masked.waiters
                _      <- Kyo.foreachDiscard(fibers)(f => f.interrupt.andThen(f.getResult.unit))
                done   <- masked.done
            yield
                assert(parked == N)
                // The whole point of masking (Signal.scala: "so that interrupting one subscriber
                // cannot propagate through"). Must stay true after the fix — a cancellation that
                // completes the shared promise would be a far worse bug than the leak.
                assert(!done)
            end for
        }

        "A7 on a plain promise the interrupt cascade clears the waiter — and takes the promise with it" in {
            for
                gate     <- Promise.init[Int, Any]
                fibers   <- Kyo.foreach(Chunk.from(1 to N))(_ => Fiber.initUnscoped(gate.get))
                _        <- awaitCount(gate.waiters, N)
                parked   <- gate.waiters
                _        <- Kyo.foreachDiscard(fibers)(f => f.interrupt.andThen(f.getResult.unit))
                after    <- gate.waiters
                gateDone <- gate.done
            yield
                assert(parked == N)
                // Today's only cleanup path, and it must not change: interrupting a parked fiber
                // cascades into the awaited promise, whose completion flushes the chain.
                assert(after == 0)
                assert(gateDone)
        }

        "A8 an interrupt link can be unlinked, unlike an onComplete waiter" in {
            import AllowUnsafe.embrace.danger
            val p1 = new IOPromise[Nothing, Int]()
            val p2 = new IOPromise[Nothing, Int]()
            p1.interrupts(p2)
            assert(p1.waiters() == 1)
            p1.removeInterrupt(p2)
            // The asymmetry this whole investigation turns on: removal exists for interrupt
            // links, and IOTask uses it precisely "so it doesn't accumulate" — but there is no
            // counterpart for the onComplete registration a parked fiber leaves behind.
            assert(p1.waiters() == 0)
        }
    }

    "B — releasing a waiter whose owner died first" - {

        "B1 an interrupted subscriber leaves no waiter on a masked promise" in {
            import AllowUnsafe.embrace.danger
            val masked = Promise.Unsafe.initMasked[Int, Any]().safe
            for
                fibers <- Kyo.foreach(Chunk.from(1 to N))(_ => Fiber.initUnscoped(masked.get))
                _      <- awaitCount(masked.waiters, N)
                parked <- masked.waiters
                _      <- Kyo.foreachDiscard(fibers)(f => f.interrupt.andThen(f.getResult.unit))
                after  <- masked.waiters
            yield
                assert(parked == N)
                assert(after == 0)
            end for
        }

        "B2 observers that come and go without a value change leave no waiters on a signal" in {
            for
                ref    <- Signal.initRef(0)
                fibers <- Kyo.foreach(Chunk.from(1 to N))(_ => Fiber.initUnscoped(ref.observe(_ => Kyo.unit)))
                _      <- awaitCount(ref.waiters, N)
                parked <- ref.waiters
                _      <- Kyo.foreachDiscard(fibers)(f => f.interrupt.andThen(f.getResult.unit))
                after  <- ref.waiters
            yield
                assert(parked >= N)
                assert(after == 0)
        }
    }

end WaiterLifecycleTest
