package kyo.apollo.testing

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine

/** The single, parameterized fake [[HttpEngine]] that collapses the ~11 inline
  * doubles the audit found across the suite (ADR §2b): the capturing engine
  * (records the request, returns a canned body), the counting engine (fixed
  * body, call count), the recording-with-callback engine, and the anonymous
  * canned / failing engines. Every request is recorded in [[requests]] (with a
  * [[calls]] count and [[lastRequest]] accessor), so the "capturing" and
  * "counting" flavors are just this one type read two ways; the response is
  * decided by the responder the factory installs.
  *
  * A request is recorded when [[execute]] runs, in the same effect that produces
  * the reply — an execution that is built but never run is not counted, and
  * concurrent executions are all counted. Construct via the [[TestHttpEngine$]]
  * factories rather than the private constructor:
  *
  *   - [[TestHttpEngine.returning]] — always the same `(status, body)` (static /
  *     counting).
  *   - [[TestHttpEngine.respondWith]] — a pure `HttpEngine.Request => HttpEngine.Response`
  *     (recording-with-callback, document routing).
  *   - [[TestHttpEngine.async]] — an effectful `HttpEngine.Request => HttpEngine.Response`, which may
  *     abort with an `HttpEngineFailure`.
  *   - [[TestHttpEngine.failing]] — aborts the round-trip with an
  *     `ApolloNetworkException` (a simulated connection error the transport folds
  *     into an `ApolloResponse.error` value).
  *
  * For a reply parked on a gate (observe `Loading` before releasing the
  * response), use [[GatedHttpEngine]] instead — the one gate-on-a-`Promise`
  * engine promoted as-is from `KyoTestSupport`.
  */
final class TestHttpEngine private (
    responder: HttpEngine.Request => HttpEngine.Response < (Async & Abort[HttpEngineFailure]),
    received: AtomicRef[Chunk[HttpEngine.Request]]
) extends HttpEngine:

    /** Every request executed, oldest first. */
    def requests(using Frame): Chunk[HttpEngine.Request] < Sync = received.get

    /** How many times [[execute]] has run. */
    def calls(using Frame): Int < Sync = received.get.map(_.size)

    /** The most recent request, if any. */
    def lastRequest(using Frame): Maybe[HttpEngine.Request] < Sync = received.get.map(_.lastMaybe)

    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        received.updateAndGet(_.append(request)).andThen(responder(request))
end TestHttpEngine

object TestHttpEngine:

    /** Answers every request with the same `(status, body)`. A non-2xx status is
      * folded by the transport into an `ApolloResponse.error` value.
      */
    def returning(body: String, status: HttpStatus = HttpStatus.OK)(using Frame): TestHttpEngine < Sync =
        init(_ => HttpEngine.response(status, body))

    /** Answers each request with `respond(request)` — the recording-with-callback
      * and document-routing flavors (route on `request.body`/`url`).
      */
    def respondWith(respond: HttpEngine.Request => HttpEngine.Response)(using Frame): TestHttpEngine < Sync =
        init(request => respond(request))

    /** Answers each request with an effectful `respond` (e.g. a deferred value). */
    def async(respond: HttpEngine.Request => HttpEngine.Response < (Async & Abort[HttpEngineFailure]))(using Frame): TestHttpEngine < Sync =
        init(respond)

    /** Aborts every round-trip with an `ApolloNetworkException` carrying `cause` — a
      * simulated connection error (no response received), as the production engines
      * report one. The transport folds it into an `ApolloResponse.error` value.
      */
    def failing(cause: Throwable)(using Frame): TestHttpEngine < Sync =
        init(_ => Abort.fail(ApolloNetworkException(cause = cause)))

    private def init(responder: HttpEngine.Request => HttpEngine.Response < (Async & Abort[HttpEngineFailure]))(using
        Frame
    ): TestHttpEngine < Sync =
        AtomicRef.init(Chunk.empty[HttpEngine.Request]).map(new TestHttpEngine(responder, _))
end TestHttpEngine

/** An [[HttpEngine]] that parks every reply on a gate until [[release]] is
  * called, so a watcher's first (network) emission can be deferred
  * deterministically — a test observes `Loading` before the response, then
  * `Success` after. Promoted as-is from `KyoTestSupport.GatedEngine` (ADR §2a:
  * the only gate-on-a-`Promise` engine, essential for reactive `Loading` →
  * `Success` assertions), with the request-recording of [[TestHttpEngine]] added.
  */
final class GatedHttpEngine(responseBody: String, status: HttpStatus = HttpStatus.OK) extends HttpEngine:
    private given Frame = Frame.internal
    private val unsafe  = AllowUnsafe.embrace.danger

    private val gate        = Fiber.Promise.Unsafe.init[Unit, Any]()(using unsafe).safe
    private val requestsRef = AtomicRef.Unsafe.init(Chunk.empty[HttpEngine.Request])(using unsafe).safe
    private val arrivedCh   = Channel.Unsafe.init[HttpEngine.Request](Int.MaxValue)(using summon[Frame], unsafe).safe

    /** Every request received, oldest first (recorded when parked, before release). */
    def requests: List[HttpEngine.Request] = requestsRef.unsafe.get()(using unsafe).toList

    /** The next request to reach the engine, in arrival order (each request is
      * handed out once) — the barrier a test waits on before asserting that a
      * reply is parked, instead of a pause.
      */
    def nextRequest(using Frame): HttpEngine.Request < Async =
        Abort.run[Closed](arrivedCh.take).map(_.getOrThrow)

    /** Release the gate so every parked (and future) reply resolves. Idempotent. */
    def release(): Unit =
        discard(gate.unsafe.completeUnitDiscard()(using unsafe))

    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
        requestsRef.getAndUpdate(_.append(request))
            .andThen(Abort.run[Closed](arrivedCh.offer(request)))
            .andThen(gate.get)
            .andThen(HttpEngine.response(status, responseBody))
end GatedHttpEngine
