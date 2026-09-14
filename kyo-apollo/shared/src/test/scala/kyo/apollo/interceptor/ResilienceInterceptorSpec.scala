package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.TestIds
import kyo.apollo.network.Uuid
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Tests the resilience interceptors on every platform: [[RetryOnErrorInterceptor]]
  * (schedule, jitter, interruption, transport-vs-GraphQL classification) and
  * [[AutoPersistedQueryInterceptor]] (hash-only probe → document fallback).
  *
  * A retry waits with `Async.sleep` in the consuming fiber, so the waits here run
  * on the clock of `Clock.withTimeControl` and are asserted to the virtual instant:
  * `awaitPendingSleepers` fences "the retry is waiting", and an `advance` that stops
  * one millisecond short of a deadline proves the retry has not run yet. No real
  * clock, no network. [[BatchingHttpInterceptor]] is covered by
  * `BatchingHttpInterceptorSpec`.
  */
class ResilienceInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures -------------------------------------------------------------

    /** The `{ "value": Int }` object shape `data` decodes from; `D` stays `Int`
      * via a transform of this derived object schema.
      */
    final case class ValueData(value: Int) derives Schema

    /** A query whose `data` is a single `{ "value": Int }` object. */
    final case class ValueQuery() extends Query[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    /** A terminal [[ApolloInterceptor]] that records every request it receives
      * together with the `Clock.now` it arrived at, and answers the n-th (1-based)
      * with `answer(n, requestUuid)`.
      */
    final class RecordingTerminal private (
        answer: (Int, Uuid) => Seq[ApolloResponse[Any]],
        received: AtomicRef[Chunk[(ApolloRequest[?], Instant)]]
    ) extends ApolloInterceptor:

        def requests(using Frame): Chunk[ApolloRequest[?]] < Sync = received.get.map(_.map(_._1))
        def arrivals(using Frame): Chunk[Instant] < Sync          = received.get.map(_.map(_._2))
        def calls(using Frame): Int < Sync                        = received.get.map(_.size)

        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.unwrap(Clock.now.map(now => received.updateAndGet(_.append((request, now)))).map { seen =>
                Stream.init(answer(seen.size, request.requestUuid).map(_.asInstanceOf[ApolloResponse[D]]))
            })
    end RecordingTerminal

    object RecordingTerminal:

        /** Answers call n with `script(n)`, repeating the last entry once the script is exhausted. */
        def scripted(script: (Uuid => ApolloResponse[Any])*)(using Frame): RecordingTerminal < Sync =
            init((n, id) => Seq(script(math.min(n, script.size) - 1)(id)))

        /** Answers every call with ALL of `makers` as one multi-emission stream — the
          * `@defer` / cache-and-network shape an interceptor must forward whole.
          */
        def multiEmit(makers: (Uuid => ApolloResponse[Any])*)(using Frame): RecordingTerminal < Sync =
            init((_, id) => makers.map(_(id)))

        private def init(answer: (Int, Uuid) => Seq[ApolloResponse[Any]])(using Frame): RecordingTerminal < Sync =
            AtomicRef.init(Chunk.empty[(ApolloRequest[?], Instant)]).map(new RecordingTerminal(answer, _))
    end RecordingTerminal

    private def data(value: Int): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, data = Present(value))
    private def networkFail: Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloNetworkException("boom"))
    private def httpFail(status: Int): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloHttpException(status, Nil, "http"))
    private def graphqlError: Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, error = Present(ApolloGraphQLException(Chunk(GraphQLError("bad field")))))
    private def apqError(message: String): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, error = Present(ApolloGraphQLException(Chunk(GraphQLError(message)))))

    private def proceed(interceptors: ApolloInterceptor*)(using Frame): ResponseStream[Int] =
        DefaultApolloInterceptorChain(Chunk.from(interceptors), 0).proceed(ApolloRequest(ValueQuery(), TestIds.requestUuid))

    /** Consume the first response on a fiber of its own, so the leaf can steer the
      * clock while that fiber waits out a retry.
      */
    private def consume(interceptor: ApolloInterceptor, terminal: RecordingTerminal)(using Frame) =
        Fiber.init(Scope.run(StreamProbe.first(proceed(interceptor, terminal))))

    private def after(duration: Duration): Instant = Instant.Epoch + duration

    // --- RetryOnErrorInterceptor ------------------------------------------------

    "RetryOnErrorInterceptor" - {

        "retries after exactly the scheduled delays" in {
            Clock.withTimeControl { control =>
                for
                    terminal <- RecordingTerminal.scripted(networkFail, networkFail, data(42))
                    fiber <- consume(
                        RetryOnErrorInterceptor(schedule = Schedule.fixed(1.second).take(2), jitter = 0.0),
                        terminal
                    )
                    _        <- control.awaitPendingSleepers(1) // attempt 1 failed; its retry waits
                    _        <- control.advance(999.millis)
                    at999ms  <- terminal.calls
                    _        <- control.advance(1.milli)
                    _        <- control.awaitPendingSleepers(1) // attempt 2 failed; the next retry waits
                    at1s     <- terminal.calls
                    _        <- control.advance(1.second)
                    response <- fiber.get
                    at2s     <- terminal.calls
                    arrivals <- terminal.arrivals
                yield
                    assert(at999ms == 1, s"no retry before the delay has passed, got $at999ms call(s)")
                    assert(at1s == 2, s"the retry runs once the delay has passed, got $at1s call(s)")
                    assert(at2s == 3)
                    assert(response.data == Present(42) && response.error == Absent)
                    assert(arrivals == Chunk(after(Duration.Zero), after(1.second), after(2.seconds)))
            }
        }

        "the default schedule retries twice, 1s then 2s apart, and then answers with the last failure" in {
            Clock.withTimeControl { control =>
                for
                    terminal <- RecordingTerminal.scripted(networkFail)
                    fiber    <- consume(RetryOnErrorInterceptor(jitter = 0.0), terminal)
                    _        <- control.awaitPendingSleepers(1)
                    _        <- control.advance(999.millis)
                    before1s <- terminal.calls
                    _        <- control.advance(1.milli)
                    _        <- control.awaitPendingSleepers(1)
                    _        <- control.advance(1999.millis)
                    before3s <- terminal.calls
                    _        <- control.advance(1.milli)
                    response <- fiber.get
                    arrivals <- terminal.arrivals
                yield
                    assert(before1s == 1 && before3s == 2, s"calls before the deadlines: $before1s, $before3s")
                    assert(response.error.exists(_.isInstanceOf[ApolloNetworkException]))
                    assert(arrivals == Chunk(after(Duration.Zero), after(1.second), after(3.seconds)))
            }
        }

        "an interrupted caller does not fire the pending retry" in {
            Clock.withTimeControl { control =>
                for
                    terminal <- RecordingTerminal.scripted(networkFail, data(42))
                    fiber <- consume(
                        RetryOnErrorInterceptor(schedule = Schedule.fixed(1.second).take(2), jitter = 0.0),
                        terminal
                    )
                    _ <- control.awaitPendingSleepers(1) // the retry waits in the caller's fiber
                    _ <- fiber.interrupt
                    // The retry would run in this fiber; once it has ended, nothing is left to run it.
                    result <- fiber.getResult
                    _      <- control.advance(10.seconds)
                    calls  <- terminal.calls
                yield
                    assert(!result.isSuccess, s"the caller was interrupted: $result")
                    assert(calls == 1, s"the pending retry must not run for an interrupted caller, got $calls call(s)")
            }
        }

        "jitter takes a share drawn from Random off each delay, and Random.withSeed reproduces it" in {
            // The run draws one `Random.nextDouble` per wait and nothing else, so the same
            // seed yields the same two shares here and inside the interceptor: each retry's
            // arrival is known to the nanosecond, and one millisecond short of it the
            // retry has not run.
            val schedule = Schedule.exponentialBackoff(1.second, 2.0, 30.seconds).take(2)
            def arrivalsUnder(seed: Int): (Chunk[Instant], Duration, Duration) < (Async & Scope) =
                Random.withSeed(seed)(Kyo.zip(Random.nextDouble, Random.nextDouble)).map { (r1, r2) =>
                    val wait1 = 1.second * (1.0 - 0.5 * r1)
                    val wait2 = 2.seconds * (1.0 - 0.5 * r2)
                    Clock.withTimeControl { control =>
                        Random.withSeed(seed) {
                            for
                                terminal <- RecordingTerminal.scripted(networkFail, networkFail, data(42))
                                fiber    <- consume(RetryOnErrorInterceptor(schedule = schedule, jitter = 0.5), terminal)
                                _        <- control.awaitPendingSleepers(1)
                                _        <- control.advance(wait1 - 1.milli)
                                early1   <- terminal.calls
                                _        <- control.advance(1.milli)
                                _        <- control.awaitPendingSleepers(1)
                                _        <- control.advance(wait2 - 1.milli)
                                early2   <- terminal.calls
                                _        <- control.advance(1.milli)
                                _        <- fiber.get
                                arrivals <- terminal.arrivals
                            yield
                                assert(early1 == 1 && early2 == 2, s"a retry ran before its jittered delay: $early1, $early2")
                                (arrivals, wait1, wait2)
                        }
                    }
                }
            for
                (arrivals, wait1, wait2) <- arrivalsUnder(7)
                (again, _, _)            <- arrivalsUnder(7)
                (otherSeed, _, _)        <- arrivalsUnder(8)
            yield
                assert(arrivals == Chunk(after(Duration.Zero), after(wait1), after(wait1 + wait2)))
                assert(wait1 >= 500.millis && wait1 <= 1.second, s"the first wait stays within [0.5s, 1s]: $wait1")
                assert(wait2 >= 1.second && wait2 <= 2.seconds, s"the second wait stays within [1s, 2s]: $wait2")
                assert(arrivals != Chunk(after(Duration.Zero), after(1.second), after(3.seconds)), "jitter was applied")
                assert(again == arrivals, "the same seed reproduces the same waits")
                assert(otherSeed != arrivals, "the waits come from Random: another seed gives other waits")
            end for
        }

        "a network failure is retried and then succeeds" in {
            for
                terminal <- RecordingTerminal.scripted(networkFail, networkFail, data(42))
                response <- StreamProbe.first(proceed(RetryOnErrorInterceptor(schedule = Schedule.repeat(2)), terminal))
                calls    <- terminal.calls
            yield
                assert(response.data == Present(42))
                assert(response.error == Absent)
                assert(calls == 3) // 1 initial + 2 retries
        }

        "a GraphQL error is not a transport fault and is passed through" in {
            for
                terminal <- RecordingTerminal.scripted(graphqlError, data(42))
                response <- StreamProbe.first(proceed(RetryOnErrorInterceptor(), terminal))
                calls    <- terminal.calls
            yield
                assert(response.errors.map(_.message) == Chunk("bad field"))
                assert(calls == 1) // never retried
        }

        "a 5xx is retried but a 4xx is not" in {
            val immediate = RetryOnErrorInterceptor(schedule = Schedule.repeat(2))
            for
                on5xx  <- RecordingTerminal.scripted(httpFail(503), data(7))
                on4xx  <- RecordingTerminal.scripted(httpFail(400), data(7))
                r5     <- StreamProbe.first(proceed(immediate, on5xx))
                r4     <- StreamProbe.first(proceed(immediate, on4xx))
                calls5 <- on5xx.calls
                calls4 <- on4xx.calls
            yield
                assert(r5.data == Present(7))
                assert(calls5 == 2)
                assert(r4.error.exists(_.isInstanceOf[ApolloHttpException]))
                assert(calls4 == 1) // 4xx not retried
            end for
        }

        "forwards every emission of a multi-emission response (no collapse)" in {
            // A happy-path @defer / CacheAndNetwork response emits more than once; the
            // retry decision rides only the first emission, the rest must pass through.
            for
                terminal  <- RecordingTerminal.multiEmit(data(1), data(2), data(3))
                responses <- StreamProbe.collect(proceed(RetryOnErrorInterceptor(), terminal))
                calls     <- terminal.calls
            yield
                assert(responses.map(_.data) == List(Present(1), Present(2), Present(3)))
                assert(calls == 1) // happy path — no retry, one round trip
        }
    }

    // --- AutoPersistedQueryInterceptor -----------------------------------------

    "AutoPersistedQueryInterceptor" - {

        "a registered-query hit sends only the hash, no document" in {
            for
                terminal <- RecordingTerminal.scripted(data(1))
                response <- StreamProbe.first(proceed(AutoPersistedQueryInterceptor(), terminal))
                requests <- terminal.requests
            yield
                assert(response.data == Present(1))
                assert(requests.size == 1)
                assert(requests.head.sendApqExtensions, "probe must carry the persistedQuery extension")
                assert(!requests.head.sendDocument, "probe must omit the document")
        }

        "PersistedQueryNotFound triggers a document-carrying resend" in {
            for
                terminal <- RecordingTerminal.scripted(apqError("PersistedQueryNotFound"), data(2))
                response <- StreamProbe.first(proceed(AutoPersistedQueryInterceptor(), terminal))
                requests <- terminal.requests
            yield
                assert(response.data == Present(2))
                assert(requests.size == 2)
                assert(!requests(0).sendDocument)                                 // first: hash only
                assert(requests(1).sendApqExtensions && requests(1).sendDocument) // second: hash + doc
        }

        "PersistedQueryNotSupported falls back to a plain document call" in {
            for
                terminal <- RecordingTerminal.scripted(apqError("PersistedQueryNotSupported"), data(3))
                response <- StreamProbe.first(proceed(AutoPersistedQueryInterceptor(), terminal))
                requests <- terminal.requests
            yield
                assert(response.data == Present(3))
                assert(requests.size == 2)
                assert(!requests(1).sendApqExtensions, "fallback drops APQ entirely")
                assert(requests(1).sendDocument)
        }

        "forwards every emission of an accepted (multi-emission) probe response" in {
            // An accepted probe may itself be an @defer stream; only the negotiation
            // rides the first response — later patches must not be dropped.
            for
                terminal  <- RecordingTerminal.multiEmit(data(1), data(2))
                responses <- StreamProbe.collect(proceed(AutoPersistedQueryInterceptor(), terminal))
                calls     <- terminal.calls
            yield
                assert(responses.map(_.data) == List(Present(1), Present(2)))
                assert(calls == 1) // registered hit — single round trip
        }
    }
end ResilienceInterceptorSpec
