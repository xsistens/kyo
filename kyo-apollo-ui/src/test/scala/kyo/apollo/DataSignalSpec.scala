package kyo.apollo

import CountryFixture.awaitSignal
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException
import kyo.internal.FailureFrame
import kyo.internal.Failures

/** Effectful tests for the suspense-shape [[dataSignal]]: first-settled seeding
  * (immediate and after a pending phase), first-failure abort, update following,
  * and the failed-update surface (keep the last data, hand the failure to the `onFailure` rules).
  * Driven off a plain `Signal.initRef[QueryState[Int]]` source.
  *
  * Synchronization is on barriers, not on time: a value is awaited on the signal
  * that carries it ([[CountryFixture.awaitSignal]]), a notice on the promise the
  * sink completes. The absence proof waits until an observer that is still
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
                sig    <- src.dataSignal(UI.Reaction.Notify)
                seeded <- sig.current
            yield assert(seeded == 1)
        }

        "suspends through Loading and seeds on the first Success" in {
            for
                src   <- Signal.initRef[QueryState[Int]](QueryState.Loading)
                fiber <- Fiber.init(Scope.run(src.dataSignal(UI.Reaction.Notify).map(_.current)))
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
                sig    <- src.dataSignal(UI.Reaction.Notify)
                seeded <- sig.current
            yield assert(seeded == 7)
        }

        "a first-settled Failure aborts into the Apollo error channel" in {
            for
                src <- Signal.initRef[QueryState[Int]](
                    QueryState.Failure(DefaultApolloException("first failure"))
                )
                result <- Abort.run[ApolloException](src.dataSignal(UI.Reaction.Notify))
            yield result match
                case Result.Failure(ex) => assert(ex.message == "first failure")
                case other              => fail(s"expected Failure, got $other")
        }

        "follows later data emissions" in {
            for
                src    <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                sig    <- src.dataSignal(UI.Reaction.Notify)
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
                res  <- Failures.within(FailureFrame.Boundary(UI.boundary.onNotice(ex =>
                    seen.completeDiscard(Result.succeed(messageOf(ex)))
                ))) {
                    for
                        sig      <- src.dataSignal(UI.Reaction.Notify)
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
                escaped <- Scope.run(src.dataSignal(UI.Reaction.Notify))
                live    <- src.dataSignal(UI.Reaction.Notify)
                // The Scope is closed; a later source emission must no longer follow.
                _ <- src.set(QueryState.Success(99, fromCache = false))
                // An observer that is still open follows it: the emission was delivered.
                _      <- awaitSignal(live)(_ == 99)
                frozen <- escaped.current
            yield assert(frozen == 1)
        }
    }

    "dataSignal (escalating by default)" - {

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

        "with no mount around it, a failed update is a notice and the follow goes on" in {
            for
                seen <- Promise.init[String, Any]
                src  <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                res  <- Failures.within(FailureFrame.Boundary(UI.boundary.onNotice(ex =>
                    seen.completeDiscard(Result.succeed(messageOf(ex)))
                ))) {
                    for
                        sig      <- src.dataSignal
                        _        <- src.set(QueryState.Failure(DefaultApolloException("update failure")))
                        surfaced <- seen.get
                        // An `Escalate` that finds no mount to fail is a notice: the follow is still alive.
                        _     <- src.set(QueryState.Success(99, fromCache = false))
                        after <- awaitSignal(sig)(_ == 99)
                    yield (surfaced, after)
                }
            yield assert(res == ("update failure", 99))
        }
    }
end DataSignalSpec
