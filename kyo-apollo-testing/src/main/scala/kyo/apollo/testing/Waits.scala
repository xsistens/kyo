package kyo.apollo.testing

import kyo.*

/** Condition-based synchronization for apollo specs — the thing every consumer
  * hand-rolls as a fixed `Async.sleep` and then tunes upward until CI stops
  * failing.
  *
  * A watcher settles when the store, the transport and the scheduler are done
  * with it, and none of those has a duration. A spec that waits 40ms is not
  * asserting anything about the library; it is betting that 40ms is enough on
  * whatever machine runs it next, and it loses that bet exactly when the suite
  * is loaded — which is to say, in CI and never on the desk of the person who
  * wrote it. The two shapes here replace the bet with a condition:
  *
  *   - [[eventually]] polls a read until the expected value arrives. A correct
  *     implementation can never fail it spuriously, however loaded the machine;
  *     a slow one costs latency, not a red suite.
  *   - [[settles]] observes a read over a bounded window and yields the last
  *     value, for proving something did NOT move. This is the only shape an
  *     absence check can have: no amount of waiting proves a value will never
  *     change, so a window is the honest statement, and a violation that lands
  *     inside it is caught deterministically.
  *
  * kyo-apollo's own `DataSignalSpec` made this move first (commit `b7fc5799b`)
  * with private copies of both; this is that pattern offered to the people
  * testing against the library instead of only used inside it.
  *
  * **Neither aborts on expiry.** The budget running out hands back the last
  * value seen, so the caller's own assertion produces the failure text — "state
  * was Loading, expected Germany" says what went wrong, where a bare
  * `TimeoutException` says only that something did. It also keeps the effect
  * type clean: specs that already run inside `Abort[ApolloException]` do not
  * grow a second error channel for a helper.
  *
  * **Polling, not observing.** A `Signal` delivers its latest value rather than
  * every intermediate one, so an observer attached a moment too late can miss
  * the emission it was waiting for. Re-reading `current` cannot: a value that
  * already landed is still there on the next poll. Promptness is bounded by the
  * cadence, and 10ms is far below the resolution any of these assertions need.
  *
  * Nothing here is apollo-specific — `read` is any effect, so a plain `var` on a
  * test engine (`eventually(engine.requests)(_ == 2)`) works as well as a signal.
  * It lives here because this is the module apollo consumers already depend on.
  */
object Waits:

    /** How long [[eventually]] keeps trying. Generous on purpose: the budget is
      * only spent by a test that is about to fail anyway.
      */
    val defaultWithin: Duration = 3.seconds

    /** [[eventually]]'s poll cadence. */
    val defaultEvery: Duration = 10.millis

    /** The window [[settles]] watches. */
    val defaultOver: Duration = 200.millis

    /** Poll `read` until `cond` holds, yielding the value either way.
      *
      * `read` is by-name and re-run on every poll, so it may be a signal read
      * (`sig.current`), a handle's state (`paged.state.current`) or a plain
      * mutable field on a test double.
      *
      * On expiry the last value observed is returned rather than an error — see
      * the class comment: the assertion that follows is the better diagnostic.
      */
    def eventually[A](
        read: => A < Async,
        within: Duration = defaultWithin,
        every: Duration = defaultEvery
    )(cond: A => Boolean)(using Frame): A < Async =
        Clock.nowMonotonic.map { started =>
            def loop: A < Async =
                read.map { a =>
                    if cond(a) then a
                    else
                        Clock.nowMonotonic.map { now =>
                            if now - started >= within then a
                            else Async.sleep(every).andThen(loop)
                        }
                }
            loop
        }

    /** Observe `read` across a bounded window and yield the LAST value — the
      * absence proof, for "this stayed put while I poked at it".
      *
      * A change that lands inside the window and persists is caught. One that
      * lands and reverts is not, and cannot be: a signal promises its latest
      * value, not every value it passed through, so a spec asserting on a
      * transient would be asserting on something the library never undertook to
      * deliver.
      */
    def settles[A](
        read: => A < Async,
        over: Duration = defaultOver,
        every: Duration = 20.millis
    )(using Frame): A < Async =
        Clock.nowMonotonic.map { started =>
            def loop(last: A): A < Async =
                Clock.nowMonotonic.map { now =>
                    if now - started >= over then last
                    else Async.sleep(every).andThen(read.map(loop))
                }
            read.map(loop)
        }
end Waits
