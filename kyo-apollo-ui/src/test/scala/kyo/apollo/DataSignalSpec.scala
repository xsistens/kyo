package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException

/** Effectful tests for the suspense-shape [[dataSignal]]: first-settled seeding
  * (immediate and after a pending phase), first-failure abort, tail following,
  * and the mandatory tail-failure surface (keep last data + invoke the handler).
  * Driven off a plain `Signal.initRef[QueryState[Int]]` source, with the
  * `Async.sleep` propagation waits the other exec specs use (live clock).
  */
class DataSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

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
                    src    <- Signal.initRef[QueryState[Int]](QueryState.Loading)
                    fiber  <- Fiber.init(Scope.run(src.dataSignal(_ => ()).map(_.current)))
                    _      <- Async.sleep(30L.millis)
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
                    _      <- Async.sleep(30L.millis)
                    second <- sig.current
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
                    _        <- Async.sleep(30L.millis)
                    kept     <- sig.current
                    surfaced <- seen.get
                yield
                    assert(kept == 1)
                    assert(surfaced == Present("tail failure"))
            }
        }

        "the follow observer stops when the caller's Scope is released" in {
            for
                src     <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                escaped <- Scope.run(src.dataSignal(_ => ()))
                _       <- Async.sleep(30L.millis)
                // The Scope is closed; a later source emission must no longer follow.
                _      <- src.set(QueryState.Success(99, fromCache = false))
                _      <- Async.sleep(30L.millis)
                frozen <- escaped.current
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
                    _      <- Async.sleep(30L.millis)
                    second <- sig.current
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
                    src  <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    sig  <- src.dataSignal
                    _    <- src.set(QueryState.Failure(DefaultApolloException("tail failure")))
                    _    <- Async.sleep(30L.millis)
                    kept <- sig.current
                    // The follow fiber died with the escalated failure: a later data emission
                    // must no longer be followed (on a supervising engine the enclosing mount
                    // node would have flipped at this point).
                    _     <- src.set(QueryState.Success(99, fromCache = false))
                    _     <- Async.sleep(30L.millis)
                    after <- sig.current
                yield
                    assert(kept == 1)
                    assert(after == 1)
            }
        }
    }
end DataSignalSpec
