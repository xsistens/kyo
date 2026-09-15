package kyo.apollo

import CountryFixture.awaitSignal
import kyo.*
import kyo.apollo.cache.LogProbe
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException

/** Effectful tests for the suspense-shape [[dataSignal]]: first-settled seeding
  * (immediate and after a pending phase), first-failure abort, update following,
  * and the mandatory failed-update surface (keep last data + invoke the handler).
  * Driven off a plain `Signal.initRef[QueryState[Int]]` source.
  *
  * Synchronization is on barriers, not on time: a value is awaited on the signal
  * that carries it ([[CountryFixture.awaitSignal]]), a notice on the promise the
  * sink completes. The two absence proofs wait until an observer that is still
  * open has seen the later value, so the one under test had its chance to follow.
  */
class DataSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A failure's own message — `getMessage` adds the KyoException framing. */
    private def messageOf(error: Throwable): String =
        error match
            case apollo: ApolloException => apollo.message
            case other                   => String.valueOf(other.getMessage)

    "dataSignal" - {

        "an already-settled Success seeds immediately" in {
            for
                src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                sig    <- src.dataSignal(UpdateFailure.Notify)
                seeded <- sig.current
            yield assert(seeded == 1)
        }

        "suspends through Loading and seeds on the first Success" in {
            for
                src   <- Signal.initRef[QueryState[Int]](QueryState.Loading)
                fiber <- Fiber.init(Scope.run(src.dataSignal(UpdateFailure.Notify).map(_.current)))
                // No wait needed before the set: the first-settle observer re-reads
                // the current state on attach, so it seeds whether it attached
                // before or after this lands.
                _      <- src.set(QueryState.Success(42, fromCache = false))
                seeded <- fiber.get
            yield assert(seeded == 42)
        }

        "PartialData seeds like Success" in {
            for
                src    <- Signal.initRef[QueryState[Int]](QueryState.PartialData(7, Chunk.empty))
                sig    <- src.dataSignal(UpdateFailure.Notify)
                seeded <- sig.current
            yield assert(seeded == 7)
        }

        "a first-settled Failure aborts into the Apollo error channel" in {
            for
                src <- Signal.initRef[QueryState[Int]](
                    QueryState.Failure(DefaultApolloException("first failure"))
                )
                result <- Abort.run[ApolloException](src.dataSignal(UpdateFailure.Notify))
            yield result match
                case Result.Failure(ex) => assert(ex.message == "first failure")
                case other              => fail(s"expected Failure, got $other")
        }

        "follows later data emissions" in {
            for
                src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                sig    <- src.dataSignal(UpdateFailure.Notify)
                first  <- sig.current
                _      <- src.set(QueryState.Success(2, fromCache = false))
                second <- awaitSignal(sig)(_ == 2)
            yield
                assert(first == 1)
                assert(second == 2)
        }

        "a failed update keeps the last data and reaches the app's notice sink" in {
            for
                seen <- Promise.init[String, Any]
                src  <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                res <- UI.notices(ex => seen.completeDiscard(Result.succeed(messageOf(ex)))) {
                    for
                        sig      <- src.dataSignal(UpdateFailure.Notify)
                        _        <- src.set(QueryState.Failure(DefaultApolloException("update failure")))
                        surfaced <- seen.get
                        kept     <- sig.current
                    yield (kept, surfaced)
                }
            yield
                assert(res._1 == 1)
                assert(res._2 == "update failure")
        }

        "the follow observer stops when the caller's Scope is released" in {
            for
                src     <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                escaped <- Scope.run(src.dataSignal(UpdateFailure.Notify))
                live    <- src.dataSignal(UpdateFailure.Notify)
                // The Scope is closed; a later source emission must no longer follow.
                _ <- src.set(QueryState.Success(99, fromCache = false))
                // An observer that is still open follows it: the emission was delivered.
                _      <- awaitSignal(live)(_ == 99)
                frozen <- escaped.current
            yield assert(frozen == 1)
        }
    }

    "dataSignal (escalating, no handler)" - {

        "seeds from the first settled data and follows later emissions" in {
            for
                src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                sig    <- src.dataSignal
                first  <- sig.current
                _      <- src.set(QueryState.Success(2, fromCache = false))
                second <- awaitSignal(sig)(_ == 2)
            yield
                assert(first == 1)
                assert(second == 2)
        }

        "a first-settled Failure aborts into the Apollo error channel" in {
            for
                src <- Signal.initRef[QueryState[Int]](
                    QueryState.Failure(DefaultApolloException("first failure"))
                )
                result <- Abort.run[ApolloException](src.dataSignal)
            yield result match
                case Result.Failure(ex) => assert(ex.message == "first failure")
                case other              => fail(s"expected Failure, got $other")
        }

        "a failed update keeps the last data and stops the follow (the fiber escalates)" in {
            for
                probe <- LogProbe.init
                src   <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                sig   <- probe.run(src.dataSignal)
                // An observer that stays open (it only notifies on a failure).
                live <- src.dataSignal(UpdateFailure.Notify)
                _    <- src.set(QueryState.Failure(DefaultApolloException("update failure")))
                // A signal delivers the LATEST value, not every intermediate one, so the
                // follow observer must have taken the failure before 99 lands: it logs the
                // failure right before its fiber fails with it.
                _ <- assertEventually(probe.errors.map(_.exists(_.error.exists(messageOf(_) == "update failure"))))
                // The follow fiber died with the escalated failure: a later data emission
                // must no longer be followed (on a supervising engine the enclosing mount
                // node would have flipped at this point). An open observer shows it was
                // delivered.
                _     <- src.set(QueryState.Success(99, fromCache = false))
                _     <- awaitSignal(live)(_ == 99)
                after <- sig.current
            yield assert(after == 1)
        }
    }
end DataSignalSpec
