package kyo.apollo.testing

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import scala.concurrent.Future

/** The single, parameterized fake [[HttpEngine]] that collapses the ~11 inline
  * doubles the audit found across the suite (ADR §2b): the capturing engine
  * (records the request, returns a canned body), the counting engine (fixed
  * body, call count), the recording-with-callback engine, and the anonymous
  * canned / failing engines. Every request is recorded in [[requests]] (with a
  * [[calls]] count and [[lastRequest]] accessor), so the "capturing" and
  * "counting" flavors are just this one type read two ways; the response is
  * decided by the responder the factory installs.
  *
  * Construct via the [[TestHttpEngine$]] factories rather than the private
  * constructor:
  *
  *   - [[TestHttpEngine.returning]] — always the same `(status, body)` (static /
  *     counting).
  *   - [[TestHttpEngine.respondWith]] — a pure `HttpRequest => HttpResponse`
  *     (recording-with-callback, document routing).
  *   - [[TestHttpEngine.async]] — an effectful `HttpRequest => HttpResponse < Async`.
  *   - [[TestHttpEngine.failing]] — fails the async round-trip (a simulated
  *     connection error the transport folds into an `ApolloNetworkException`).
  *
  * For a reply parked on a gate (observe `Loading` before releasing the
  * response), use [[GatedHttpEngine]] instead — the one gate-on-a-`Promise`
  * engine promoted as-is from `KyoTestSupport`.
  */
final class TestHttpEngine private (responder: HttpRequest => HttpResponse < Async)
    extends HttpEngine:

    private var _requests: List[HttpRequest] = Nil

    /** Every request received, oldest first. */
    def requests: List[HttpRequest] = _requests

    /** How many times [[execute]] has been called. */
    def calls: Int = _requests.length

    /** The most recent request, if any. */
    def lastRequest: Option[HttpRequest] = _requests.lastOption

    def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
        _requests = _requests :+ request
        responder(request)
end TestHttpEngine

object TestHttpEngine:

    /** Answers every request with the same `(status, body)`. A non-2xx status is
      * folded by the transport into an `ApolloResponse.exception` value.
      */
    def returning(body: String, status: Int = 200): TestHttpEngine =
        new TestHttpEngine(_ => HttpResponse(status, Nil, body))

    /** Answers each request with `respond(request)` — the recording-with-callback
      * and document-routing flavors (route on `request.body`/`url`).
      */
    def respondWith(respond: HttpRequest => HttpResponse): TestHttpEngine =
        new TestHttpEngine(request => respond(request))

    /** Answers each request with an effectful `respond` (e.g. a deferred value). */
    def async(respond: HttpRequest => HttpResponse < Async): TestHttpEngine =
        new TestHttpEngine(respond)

    /** Fails the round-trip on the async channel with `cause` — a simulated
      * connection error (no response received). The transport folds it into an
      * `ApolloNetworkException` value, mirroring a real `fetch` rejection.
      */
    def failing(cause: Throwable)(using Frame): TestHttpEngine =
        new TestHttpEngine(_ => Async.fromFuture(Future.failed[HttpResponse](cause)))
end TestHttpEngine

/** An [[HttpEngine]] that parks every reply on a gate until [[release]] is
  * called, so a watcher's first (network) emission can be deferred
  * deterministically — a test observes `Loading` before the response, then
  * `Success` after. Promoted as-is from `KyoTestSupport.GatedEngine` (ADR §2a:
  * the only gate-on-a-`Promise` engine, essential for reactive `Loading` →
  * `Success` assertions), with the request-recording of [[TestHttpEngine]] added.
  */
final class GatedHttpEngine(responseBody: String, status: Int = 200) extends HttpEngine:
    private given AllowUnsafe = AllowUnsafe.embrace.danger
    private given Frame       = Frame.internal

    private val gate: Fiber.Promise[Unit, Any] =
        Sync.Unsafe.evalOrThrow(Fiber.Promise.init[Unit, Any])
    private val requestsRef = new AtomicReference[Vector[HttpRequest]](Vector.empty)

    /** Every request received, oldest first (recorded when parked, before release). */
    def requests: List[HttpRequest] = requestsRef.get().toList

    /** Release the gate so every parked (and future) reply resolves. Idempotent. */
    def release(): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(gate.unsafe.completeUnitDiscard())

    def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
        Sync.defer(discard(requestsRef.updateAndGet(_ :+ request))).andThen {
            gate.get.andThen(HttpResponse(status, Nil, responseBody))
        }
end GatedHttpEngine
