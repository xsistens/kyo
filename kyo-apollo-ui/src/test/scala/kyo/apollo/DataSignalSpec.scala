package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException

/** Effectful tests for the suspense-shape [[dataSignal]]: first-settled seeding
  * (immediate and after a pending phase), first-failure abort, tail following,
  * and the mandatory tail-failure surface (keep last data + invoke the handler).
  * Driven off a plain `Signal.initRef[QueryState[Int]]` source.
  *
  * Synchronization is condition-based, not sleep-based: [[eventually]] polls a
  * read until the expected value arrives (a correct implementation can never
  * fail it spuriously, however loaded the scheduler), and the two absence
  * proofs ([[settles]]) poll a bounded window in which the value must NOT
  * change — the only shape an absence check can have.
  */
class DataSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Poll `read` (10ms cadence, 3s budget) until `cond` holds, yielding the last
      * value either way — the assertion after it produces the real failure text.
      */
    private def eventually[A](read: => A < Async)(cond: A => Boolean)(using Frame): A < Async =
        def loop(remaining: Int): A < Async =
            read.map { a =>
                if cond(a) || remaining <= 0 then a
                else Async.sleep(10L.millis).andThen(loop(remaining - 1))
            }
        loop(300)
    end eventually

    /** Observe `read` over a bounded window (10 × 20ms) and yield the LAST value —
      * for asserting something stayed put. A violation usually lands within the
      * window; a correct implementation passes deterministically.
      */
    private def settles[A](read: => A < Async)(using Frame): A < Async =
        def loop(remaining: Int, last: A): A < Async =
            if remaining <= 0 then last
            else Async.sleep(20L.millis).andThen(read.map(loop(remaining - 1, _)))
        read.map(loop(10, _))
    end settles

    "dataSignal" - {

        "an already-settled Success seeds immediately" in {
            Scope.run {
                for
                    src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig    <- src.dataSignal(_ => ())
                    seeded <- sig.current
                yield assert(seeded == 1)
            }
        }

        "suspends through Loading and seeds on the first Success" in {
            Scope.run {
                for
                    src   <- Signal.initRef[QueryState[Int]](QueryState.Loading)
                    fiber <- Fiber.init(Scope.run(src.dataSignal(_ => ()).map(_.current)))
                    // No wait needed before the set: the first-settle observer re-reads
                    // the current state on attach, so it seeds whether it attached
                    // before or after this lands.
                    _      <- src.set(QueryState.Success(42, fromCache = false))
                    seeded <- fiber.get
                yield assert(seeded == 42)
            }
        }

        "PartialData seeds like Success" in {
            Scope.run {
                for
                    src    <- Signal.initRef[QueryState[Int]](QueryState.PartialData(7, Chunk.empty))
                    sig    <- src.dataSignal(_ => ())
                    seeded <- sig.current
                yield assert(seeded == 7)
            }
        }

        "a first-settled Failure aborts into the Apollo error channel" in {
            Scope.run {
                for
                    src <- Signal.initRef[QueryState[Int]](
                        QueryState.Failure(DefaultApolloException("first failure"))
                    )
                    result <- Abort.run[ApolloException](src.dataSignal(_ => ()))
                yield result match
                    case Result.Failure(ex) => assert(ex.getMessage == "first failure")
                    case other              => fail(s"expected Failure, got $other")
            }
        }

        "follows later data emissions" in {
            Scope.run {
                for
                    src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig    <- src.dataSignal(_ => ())
                    first  <- sig.current
                    _      <- src.set(QueryState.Success(2, fromCache = false))
                    second <- eventually(sig.current)(_ == 2)
                yield
                    assert(first == 1)
                    assert(second == 2)
            }
        }

        "a tail Failure keeps the last data and invokes the handler" in {
            Scope.run {
                for
                    seen     <- AtomicRef.init[Maybe[String]](Absent)
                    src      <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig      <- src.dataSignal(ex => seen.set(Present(ex.getMessage)))
                    _        <- src.set(QueryState.Failure(DefaultApolloException("tail failure")))
                    surfaced <- eventually(seen.get)(_.isDefined)
                    kept     <- sig.current
                yield
                    assert(kept == 1)
                    assert(surfaced == Present("tail failure"))
            }
        }

        "the follow observer stops when the caller's Scope is released" in {
            for
                src     <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                escaped <- Scope.run(src.dataSignal(_ => ()))
                // The Scope is closed; a later source emission must no longer follow.
                _      <- src.set(QueryState.Success(99, fromCache = false))
                frozen <- settles(escaped.current)
            yield assert(frozen == 1)
        }
    }

    "dataSignal (escalating, no handler)" - {

        "seeds from the first settled data and follows later emissions" in {
            Scope.run {
                for
                    src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig    <- src.dataSignal
                    first  <- sig.current
                    _      <- src.set(QueryState.Success(2, fromCache = false))
                    second <- eventually(sig.current)(_ == 2)
                yield
                    assert(first == 1)
                    assert(second == 2)
            }
        }

        "a first-settled Failure aborts into the Apollo error channel" in {
            Scope.run {
                for
                    src <- Signal.initRef[QueryState[Int]](
                        QueryState.Failure(DefaultApolloException("first failure"))
                    )
                    result <- Abort.run[ApolloException](src.dataSignal)
                yield result match
                    case Result.Failure(ex) => assert(ex.getMessage == "first failure")
                    case other              => fail(s"expected Failure, got $other")
            }
        }

        "a tail Failure keeps the last data and stops the follow (the fiber escalates)" in {
            Scope.run {
                for
                    src <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig <- src.dataSignal
                    _   <- src.set(QueryState.Failure(DefaultApolloException("tail failure")))
                    // A signal delivers the LATEST value, not every intermediate one — if 99
                    // landed immediately, the observer could legitimately never see the
                    // failure at all. Give it a settling window to observe (and die on) the
                    // failure first; only then is "stops the follow" testable.
                    _ <- settles(sig.current).unit
                    // The follow fiber died with the escalated failure: a later data emission
                    // must no longer be followed (on a supervising engine the enclosing mount
                    // node would have flipped at this point).
                    _     <- src.set(QueryState.Success(99, fromCache = false))
                    after <- settles(sig.current)
                yield assert(after == 1)
            }
        }
    }
end DataSignalSpec
