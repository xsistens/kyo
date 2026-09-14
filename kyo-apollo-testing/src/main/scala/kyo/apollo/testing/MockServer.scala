package kyo.apollo.testing

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine

/** An in-memory HTTP server that queues responses and records the requests it
  * received, usable over the HTTP transport via the injectable [[HttpEngine]]
  * seam (`ApolloClient.Config(url).httpEngine(mockServer)`). Mirrors apollo-kotlin's
  * `MockServer`: [[enqueue]] the responses a scenario will hand back in order,
  * run operations, then [[takeRequest]] / inspect [[requests]] to assert on what
  * the client actually sent.
  *
  * Built by promoting the document-routing engines (`RoutingEngine` /
  * `ScriptedEngine`) and the capturing/counting request-assertion machinery
  * catalogued in ADR §2b behind a queue + expectations façade. Responses are
  * served FIFO; when the queue is exhausted an optional [[default]] handler
  * answers (otherwise the round-trip panics with a `NoSuchElementException`, so a
  * missing `enqueue` surfaces loudly rather than hanging). For per-operation
  * routing that does not care about arrival order, prefer
  * [[MapTestNetworkTransport]] at the operation layer.
  *
  * A request is recorded, and a queued response taken, when [[execute]] runs —
  * never when its effect is merely built — so [[requestCount]] is the number of
  * round-trips that actually happened. The state lives in atomic cells, so
  * concurrent executions on the JVM or Native are all counted. Build one with
  * [[MockServer.init]].
  */
final class MockServer private (
    responses: AtomicRef[Chunk[MockServer.Responder]],
    fallback: AtomicRef[Maybe[MockServer.Responder]],
    received: AtomicRef[Chunk[HttpEngine.Request]],
    taken: AtomicInt
) extends HttpEngine:

    // --- scripting ------------------------------------------------------------

    /** Queue a fully-formed [[HttpEngine.Response]] as the next reply. */
    def enqueue(response: HttpEngine.Response)(using Frame): Unit < Sync =
        responses.updateAndGet(_.append(_ => response)).unit

    /** Queue a `(status, body)` reply as the next response (the common case). */
    def enqueue(body: String, status: HttpStatus)(using Frame): Unit < Sync =
        enqueue(HttpEngine.response(status, body))

    /** Queue a 200 reply carrying `body`. */
    def enqueue(body: String)(using Frame): Unit < Sync =
        enqueue(HttpEngine.response(HttpStatus.OK, body))

    /** Queue a reply computed from the request (route on `body`/`url`). */
    def enqueueWith(respond: HttpEngine.Request => HttpEngine.Response)(using Frame): Unit < Sync =
        responses.updateAndGet(_.append(request => respond(request))).unit

    /** Queue a simulated connection error (no response received) as the next
      * round-trip: the engine aborts with an `ApolloNetworkException` carrying `cause`,
      * which the transport folds into an `ApolloResponse.error` value.
      */
    def enqueueError(cause: Throwable)(using Frame): Unit < Sync =
        responses.updateAndGet(_.append(_ => Abort.fail(ApolloNetworkException(cause = cause)))).unit

    /** Install a fallback handler used once the enqueued responses run out
      * (instead of failing). Useful for "always answer X unless overridden".
      */
    def default(respond: HttpEngine.Request => HttpEngine.Response)(using Frame): Unit < Sync =
        fallback.set(Present(request => respond(request)))

    /** Install a fixed fallback response for every request past the queue. */
    def default(response: HttpEngine.Response)(using Frame): Unit < Sync =
        fallback.set(Present(_ => response))

    // --- assertions -----------------------------------------------------------

    /** Every request received, oldest first. */
    def requests(using Frame): Chunk[HttpEngine.Request] < Sync = received.get

    /** How many requests the server has received. */
    def requestCount(using Frame): Int < Sync = received.get.map(_.size)

    /** The next unread received request (advances an internal cursor), for
      * apollo-kotlin-style `takeRequest()` assertions. Panics with a
      * `NoSuchElementException` if none is left.
      */
    def takeRequest(using Frame): HttpEngine.Request < Sync =
        received.get.map { all =>
            taken.getAndUpdate(i => if i < all.size then i + 1 else i).map { i =>
                if i < all.size then all(i)
                else Abort.panic(new NoSuchElementException("MockServer.takeRequest: no more received requests"))
            }
        }

    /** True when every received request has been consumed via [[takeRequest]]. */
    def hasNoMoreRequests(using Frame): Boolean < Sync =
        received.get.map(all => taken.get.map(_ >= all.size))

    // --- engine ---------------------------------------------------------------

    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        received.updateAndGet(_.append(request)).andThen {
            responses.getAndUpdate(_.drop(1)).map { queued =>
                queued.headMaybe match
                    case Present(respond) => respond(request)
                    case Absent =>
                        fallback.get.map {
                            case Present(respond) => respond(request)
                            case Absent =>
                                Abort.panic(new NoSuchElementException(
                                    s"MockServer: no response enqueued for request to ${request.url.baseUrl}"
                                ))
                        }
            }
        }
    end execute
end MockServer

object MockServer:

    private type Responder = HttpEngine.Request => HttpEngine.Response < (Async & Abort[HttpEngineFailure])

    /** A server with an empty queue, no fallback and nothing received. */
    def init(using Frame): MockServer < Sync =
        for
            responses <- AtomicRef.init(Chunk.empty[Responder])
            fallback  <- AtomicRef.init(Maybe.empty[Responder])
            received  <- AtomicRef.init(Chunk.empty[HttpEngine.Request])
            taken     <- AtomicInt.init
        yield new MockServer(responses, fallback, received, taken)
end MockServer
