package kyo.apollo.testing

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import scala.collection.mutable

/** An in-memory HTTP server that queues responses and records the requests it
  * received, usable over the HTTP transport via the injectable [[HttpEngine]]
  * seam (`ApolloClient.builder().httpEngine(mockServer)`). Mirrors apollo-kotlin's
  * `MockServer`: [[enqueue]] the responses a scenario will hand back in order,
  * run operations, then [[takeRequest]] / inspect [[requests]] to assert on what
  * the client actually sent.
  *
  * Built by promoting the document-routing engines (`RoutingEngine` /
  * `ScriptedEngine`) and the capturing/counting request-assertion machinery
  * catalogued in ADR §2b behind a queue + expectations façade. Responses are
  * served FIFO; when the queue is exhausted an optional [[default]] handler
  * answers (otherwise [[execute]] fails, so a missing `enqueue` surfaces loudly
  * rather than hanging). For per-operation routing that does not care about
  * arrival order, prefer [[MapTestNetworkTransport]] at the operation layer.
  *
  * Single-consumer and single-threaded, matching the deterministic JS test
  * model: every request is appended to [[requests]] synchronously at execute
  * time (before any effectful response body resolves), so assertions never race
  * the reply.
  */
final class MockServer extends HttpEngine:

    private val responses                   = mutable.Queue.empty[HttpRequest => HttpResponse < (Async & Abort[HttpEngineFailure])]
    private var received: List[HttpRequest] = Nil
    private var takeCursor: Int             = 0
    private var fallback: Option[HttpRequest => HttpResponse < Async] = None

    // --- scripting ------------------------------------------------------------

    /** Queue a fully-formed [[HttpResponse]] as the next reply. */
    def enqueue(response: HttpResponse): MockServer =
        responses.enqueue(_ => response)
        this

    /** Queue a `(status, body)` reply as the next response (the common case). */
    def enqueue(body: String, status: Int): MockServer =
        enqueue(HttpResponse(status, Nil, body))

    /** Queue a 200 reply carrying `body`. */
    def enqueue(body: String): MockServer =
        enqueue(HttpResponse(200, Nil, body))

    /** Queue a reply computed from the request (route on `body`/`url`). */
    def enqueueWith(respond: HttpRequest => HttpResponse): MockServer =
        responses.enqueue(request => respond(request))
        this

    /** Queue a simulated connection error (no response received) as the next
      * round-trip: the engine aborts with an `ApolloNetworkException` carrying `cause`,
      * which the transport folds into an `ApolloResponse.error` value.
      */
    def enqueueError(cause: Throwable)(using Frame): MockServer =
        responses.enqueue(_ => Abort.fail(ApolloNetworkException(cause = cause)))
        this

    /** Install a fallback handler used once the enqueued responses run out
      * (instead of failing). Useful for "always answer X unless overridden".
      */
    def default(respond: HttpRequest => HttpResponse): MockServer =
        fallback = Some(request => respond(request))
        this

    /** Install a fixed fallback response for every request past the queue. */
    def default(response: HttpResponse): MockServer =
        fallback = Some(_ => response)
        this

    // --- assertions -----------------------------------------------------------

    /** Every request received, oldest first. */
    def requests: List[HttpRequest] = received

    /** How many requests the server has received. */
    def requestCount: Int = received.length

    /** The next unread received request (advances an internal cursor), for
      * apollo-kotlin-style `takeRequest()` assertions. Throws if none is left.
      */
    def takeRequest(): HttpRequest =
        if takeCursor >= received.length then
            throw new NoSuchElementException("MockServer.takeRequest(): no more received requests")
        val request = received(takeCursor)
        takeCursor += 1
        request
    end takeRequest

    /** True when every received request has been consumed via [[takeRequest]]. */
    def hasNoMoreRequests: Boolean = takeCursor >= received.length

    // --- engine ---------------------------------------------------------------

    def execute(request: HttpRequest)(using Frame): HttpResponse < (Async & Abort[HttpEngineFailure]) =
        received = received :+ request
        if responses.nonEmpty then responses.dequeue()(request)
        else
            fallback match
                case Some(respond) => respond(request)
                case None =>
                    throw new NoSuchElementException(
                        s"MockServer: no response enqueued for request to ${request.url}"
                    )
        end if
    end execute
end MockServer
