package kyo.apollo.testing

import kyo.*
import kyo.apollo.testing.Waits.*

/** Pins the two synchronization shapes [[Waits]] offers, and the properties that
  * make them worth offering rather than leaving every consumer to sleep.
  *
  * The load-dependence these replace cannot be reproduced in a test — that is
  * the whole complaint about it. What CAN be pinned is the difference in kind:
  * a source that settles later than any hand-picked sleep would have allowed is
  * still caught here, promptness is not traded away for that, and an expired
  * budget hands back a value instead of an error.
  */
class WaitsSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def elapsedOf[A](v: => A < Async)(using Frame): (A, Duration) < Async =
        for
            started <- Clock.nowMonotonic
            a       <- v
            ended   <- Clock.nowMonotonic
        yield (a, ended - started)

    "eventually" - {

        "catches a value that lands long after any hand-picked sleep would have given up" in {
            Scope.run {
                for
                    ref <- Signal.initRef(0)
                    // 400ms: past 30ms, past 50ms, past every constant a consumer
                    // has tuned its way to. A fixed sleep is a bet on this number.
                    _    <- Fiber.init(Async.sleep(400L.millis).andThen(ref.set(1)))
                    seen <- eventually(ref.current)(_ == 1)
                yield assert(seen == 1)
            }
        }

        "returns as soon as the condition holds, not when the budget expires" in {
            Scope.run {
                for
                    ref          <- Signal.initRef(0)
                    _            <- Fiber.init(Async.sleep(100L.millis).andThen(ref.set(1)))
                    (seen, took) <- elapsedOf(eventually(ref.current, within = 5.seconds)(_ == 1))
                yield
                    assert(seen == 1)
                    // The budget is fifty times the wait; spending it would mean the
                    // helper polls to the end regardless of the condition.
                    assert(took < 2.seconds, s"took $took")
            }
        }

        "sees a value that had already landed before the first poll" in {
            Scope.run {
                for
                    ref          <- Signal.initRef(7)
                    (seen, took) <- elapsedOf(eventually(ref.current)(_ == 7))
                yield
                    assert(seen == 7)
                    assert(took < 100L.millis, s"took $took")
            }
        }

        "hands back the last value when the budget expires, rather than aborting" in {
            Scope.run {
                for
                    ref <- Signal.initRef(3)
                    // The condition can never hold. The point is what comes back: the
                    // value, so the caller's own assertion says "was 3, expected 99"
                    // instead of a bare timeout that says only that something failed.
                    (seen, took) <- elapsedOf(eventually(ref.current, within = 100L.millis)(_ == 99))
                yield
                    assert(seen == 3)
                    assert(took >= 100L.millis, s"took $took")
            }
        }
    }

    "settles" - {

        "yields the unchanged value when nothing moves" in {
            Scope.run {
                for
                    ref  <- Signal.initRef(1)
                    seen <- settles(ref.current, over = 100L.millis)
                yield assert(seen == 1)
            }
        }

        "yields the changed value when something moves inside the window" in {
            Scope.run {
                for
                    ref <- Signal.initRef(1)
                    _   <- Fiber.init(Async.sleep(50L.millis).andThen(ref.set(2)))
                    // An absence proof has to FAIL when the absence is violated —
                    // this is the assertion that would go red in a consumer's spec.
                    seen <- settles(ref.current, over = 300L.millis)
                yield assert(seen == 2)
            }
        }

        "watches for the whole window before answering" in {
            Scope.run {
                for
                    ref          <- Signal.initRef(1)
                    (seen, took) <- elapsedOf(settles(ref.current, over = 200L.millis))
                yield
                    assert(seen == 1)
                    assert(took >= 200L.millis, s"took $took")
            }
        }
    }
end WaitsSpec
