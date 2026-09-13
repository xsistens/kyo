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
import kyo.apollo.network.Uuid
import kyo.apollo.network.ws.WsBackoff
import kyo.apollo.network.ws.WsScheduler
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap
import scala.concurrent.Future

/** Tests the Phase 07 resilience interceptors: [[RetryOnErrorInterceptor]]
  * (backoff + attempt cap, transport-vs-GraphQL classification) and
  * [[AutoPersistedQueryInterceptor]] (hash-only probe → document fallback). All
  * timing is deterministic via an auto-firing scheduler; no real clocks or
  * network. [[BatchingHttpInterceptor]] is covered by the shared
  * `BatchingHttpInterceptorSpec` under `Clock.withTimeControl`.
  */
class ResilienceInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

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

    /** A [[WsScheduler]] that records each requested delay and fires the task on
      * the next microtask, so an awaiting `Future` progresses without a real clock.
      */
    final class AutoScheduler extends WsScheduler:
        var delays: List[Long] = Nil
        def schedule(delayMillis: Long)(task: () => Unit): () => Unit =
            delays = delays :+ delayMillis
            Future.successful(()).foreach(_ => task())
            () => ()
        end schedule
    end AutoScheduler

    /** A terminal [[ApolloInterceptor]] that records every request it receives and
      * answers each `proceed` with the next scripted response (repeating the last
      * once the script is exhausted).
      */
    final class ScriptedApollo(script: List[Uuid => ApolloResponse[Any]]) extends ApolloInterceptor:
        var seen: List[ApolloRequest[?]] = Nil
        private var remaining            = script
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            seen = seen :+ request
            val make = remaining match
                case head :: tail => remaining = tail; head
                case Nil          => script.last
            Stream.init(Seq(make(request.requestUuid).asInstanceOf[ApolloResponse[D]]))
        end intercept
    end ScriptedApollo

    /** A terminal that answers each `proceed` with ALL scripted responses as one
      * multi-emission stream — the `@defer` / cache-and-network shape the resilience
      * interceptors must forward without collapsing to the first emission.
      */
    final class MultiEmitApollo(makers: List[Uuid => ApolloResponse[Any]]) extends ApolloInterceptor:
        var seen: List[ApolloRequest[?]] = Nil
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            seen = seen :+ request
            Stream.init(makers.map(m => m(request.requestUuid).asInstanceOf[ApolloResponse[D]]))
        end intercept
    end MultiEmitApollo

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

    private def retry(
        scheduler: AutoScheduler,
        script: List[Uuid => ApolloResponse[Any]],
        maxAttempts: Int = 3
    ): (ScriptedApollo, ResponseStream[Int]) =
        val terminal = ScriptedApollo(script)
        val interceptor = RetryOnErrorInterceptor(
            maxAttempts = maxAttempts,
            backoff = WsBackoff.exponential(baseMillis = 100L, factor = 2.0),
            jitterFactor = 0.0,
            scheduler = scheduler
        )
        val chain = DefaultApolloInterceptorChain(Chunk(interceptor, terminal), 0)
        (terminal, chain.proceed(ApolloRequest(ValueQuery())))
    end retry

    private def apqChain(
        script: List[Uuid => ApolloResponse[Any]]
    ): (ScriptedApollo, ResponseStream[Int]) =
        val terminal = ScriptedApollo(script)
        val chain =
            DefaultApolloInterceptorChain(Chunk(AutoPersistedQueryInterceptor(), terminal), 0)
        (terminal, chain.proceed(ApolloRequest(ValueQuery())))
    end apqChain

    "resilience interceptors" - {

        // --- RetryOnErrorInterceptor -------------------------------------------

        "retry: a network failure is retried and then succeeds" in {
            val scheduler          = AutoScheduler()
            val (terminal, stream) = retry(scheduler, List(networkFail, networkFail, data(42)))
            StreamProbe.first(stream).map { response =>
                assert(response.data == Present(42))
                assert(response.error == Absent)
                assert(terminal.seen.length == 3)            // 1 initial + 2 retries
                assert(scheduler.delays == List(100L, 200L)) // exponential, no jitter
            }
        }

        "retry: gives up after maxAttempts and returns the last failure" in {
            val scheduler          = AutoScheduler()
            val (terminal, stream) = retry(scheduler, List(networkFail), maxAttempts = 3)
            StreamProbe.first(stream).map { response =>
                assert(response.error.exists(_.isInstanceOf[ApolloNetworkException]))
                assert(terminal.seen.length == 3)
                assert(scheduler.delays == List(100L, 200L)) // two waits between three tries
            }
        }

        "retry: a GraphQL error is not a transport fault and is passed through" in {
            val scheduler          = AutoScheduler()
            val (terminal, stream) = retry(scheduler, List(graphqlError, data(42)))
            StreamProbe.first(stream).map { response =>
                assert(response.errors.map(_.message) == Chunk("bad field"))
                assert(terminal.seen.length == 1) // never retried
                assert(scheduler.delays == Nil)
            }
        }

        "retry: a 5xx is retried but a 4xx is not" in {
            val scheduler5xx         = AutoScheduler()
            val (seen5xx, stream5xx) = retry(scheduler5xx, List(httpFail(503), data(7)))
            val scheduler4xx         = AutoScheduler()
            val (seen4xx, stream4xx) = retry(scheduler4xx, List(httpFail(400), data(7)))
            for
                r5 <- StreamProbe.first(stream5xx)
                r4 <- StreamProbe.first(stream4xx)
            yield
                assert(r5.data == Present(7))
                assert(seen5xx.seen.length == 2)
                assert(r4.error.exists(_.isInstanceOf[ApolloHttpException]))
                assert(seen4xx.seen.length == 1) // 4xx not retried
            end for
        }

        "retry: forwards every emission of a multi-emission response (no collapse)" in {
            // A happy-path @defer / CacheAndNetwork response emits more than once; the
            // retry decision rides only the first emission, the rest must pass through.
            val terminal    = MultiEmitApollo(List(data(1), data(2), data(3)))
            val interceptor = RetryOnErrorInterceptor(scheduler = AutoScheduler())
            val chain       = DefaultApolloInterceptorChain(Chunk(interceptor, terminal), 0)
            StreamProbe.collect(chain.proceed(ApolloRequest(ValueQuery()))).map { responses =>
                assert(responses.map(_.data) == List(Present(1), Present(2), Present(3)))
                assert(terminal.seen.length == 1) // happy path — no retry, one round trip
            }
        }

        // --- AutoPersistedQueryInterceptor -------------------------------------

        "APQ: a registered-query hit sends only the hash, no document" in {
            val (terminal, stream) = apqChain(List(data(1)))
            StreamProbe.first(stream).map { response =>
                assert(response.data == Present(1))
                assert(terminal.seen.length == 1)
                val probe = terminal.seen.head
                assert(probe.sendApqExtensions, "probe must carry the persistedQuery extension")
                assert(!probe.sendDocument, "probe must omit the document")
            }
        }

        "APQ: PersistedQueryNotFound triggers a document-carrying resend" in {
            val (terminal, stream) = apqChain(List(apqError("PersistedQueryNotFound"), data(2)))
            StreamProbe.first(stream).map { response =>
                assert(response.data == Present(2))
                assert(terminal.seen.length == 2)
                val probe  = terminal.seen.head
                val resend = terminal.seen(1)
                assert(!probe.sendDocument)                             // first: hash only
                assert(resend.sendApqExtensions && resend.sendDocument) // second: hash + doc
            }
        }

        "APQ: PersistedQueryNotSupported falls back to a plain document call" in {
            val (terminal, stream) = apqChain(List(apqError("PersistedQueryNotSupported"), data(3)))
            StreamProbe.first(stream).map { response =>
                assert(response.data == Present(3))
                assert(terminal.seen.length == 2)
                val fallback = terminal.seen(1)
                assert(!fallback.sendApqExtensions, "fallback drops APQ entirely")
                assert(fallback.sendDocument)
            }
        }

        "APQ: forwards every emission of an accepted (multi-emission) probe response" in {
            // An accepted probe may itself be an @defer stream; only the negotiation
            // rides the first response — later patches must not be dropped.
            val terminal = MultiEmitApollo(List(data(1), data(2)))
            val chain    = DefaultApolloInterceptorChain(Chunk(AutoPersistedQueryInterceptor(), terminal), 0)
            StreamProbe.collect(chain.proceed(ApolloRequest(ValueQuery()))).map { responses =>
                assert(responses.map(_.data) == List(Present(1), Present(2)))
                assert(terminal.seen.length == 1) // registered hit — single round trip
            }
        }
    }
end ResilienceInterceptorSpec
