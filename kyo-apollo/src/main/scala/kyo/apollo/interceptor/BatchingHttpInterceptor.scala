package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import kyo.apollo.network.ws.WsScheduler
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** An [[HttpInterceptor]] that coalesces several GraphQL POSTs fired within a
  * short window into a single batched HTTP request whose body is a JSON **array**
  * of the individual request bodies, then splits the array response back to each
  * caller.
  *
  * This is the standard Apollo query-batching transport: the server receives
  * `[ {query…}, {query…}, … ]` and answers with `[ {data…}, {data…}, … ]` in the
  * same order, so N operations cost one round trip. It lives at the HTTP layer —
  * below the operation/`ApolloRequest` envelope — because coalescing is purely
  * about the wire body; nothing above it needs to know a call was batched.
  *
  * Batching is opportunistic and time-boxed: the first request in an idle batch
  * arms a `batchIntervalMillis` timer on the injected [[WsScheduler]] (the same
  * timer seam the WebSocket layer uses); the batch flushes when the timer fires
  * or when it reaches `maxBatchSize`, whichever comes first. A batch of one is
  * sent as an ordinary request (unwrapped), so a lone call is never reshaped into
  * an array a non-batching server would reject. Requests without a body (a `Get`,
  * which carries the operation in its URL) cannot be batched and are forwarded
  * immediately and individually.
  *
  * All batched requests are sent with the first request's URL and headers — the
  * same limitation apollo-kotlin's `BatchingHttpInterceptor` has — so per-request
  * header overrides (e.g. distinct auth) are not preserved across a batch. Errors
  * are shared fairly: a non-2xx batched response is handed to every caller (each
  * transport maps it to an `ApolloHttpException` value), and a 2xx body that is
  * not an array of the expected length fails every caller's future (the transport
  * maps that to an `ApolloNetworkException` value) rather than silently dropping
  * responses.
  *
  * Safe without synchronization: Scala.js runs on a single-threaded event loop,
  * so the pending queue is only ever touched from one turn at a time.
  *
  * @param batchIntervalMillis how long the first request waits for companions
  * @param maxBatchSize        flush eagerly once this many requests are pending
  * @param scheduler           the timer the batching window is armed on
  */
final class BatchingHttpInterceptor(
    batchIntervalMillis: Long = 10L,
    maxBatchSize: Int = 10,
    scheduler: WsScheduler = WsScheduler.default
) extends HttpInterceptor:

    // Value-only import so it never clashes with `kyo.apollo.network.ExecutionContext`.
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    // Effect pivot (Schritt 2.2): the batching window's accumulate/flush machinery
    // stays Future/Promise/timer-based (single-threaded, proven, deterministic under
    // the fake scheduler); only the `HttpInterceptor` boundary is kyo. `intercept`
    // returns `< Async`, and where it drives the (kyo) `chain.proceed` it bridges
    // back to a `Future` via the local [[runToFuture]] to keep the queue logic
    // unchanged (the shared `KyoInterop` bridge is gone in Slice 2).

    /** One queued request awaiting its batch: the wire request, the chain that will
      * carry the (merged) request onward, and the promise its response fulfils.
      */
    final private case class Pending(
        request: HttpRequest,
        chain: HttpInterceptorChain,
        response: Promise[HttpResponse]
    )

    private var pending: List[Pending]   = Nil
    private var timer: Maybe[() => Unit] = Absent

    /** Discharge a kyo `HttpResponse < Async` into a `Future` for the Future-based
      * batching queue — a detached unscoped fiber whose completion is exposed as a
      * `Future`. The single Kyo→Future bridge this interceptor needs.
      */
    // These batch-flush helpers run detached (e.g. off the batching timer), so no
    // user `Frame` is in scope — and kyo bans auto-deriving one inside `package
    // kyo.*`. Supply an internal Frame for the effect calls below.
    private given Frame = Frame.internal

    private def runToFuture(eff: HttpResponse < Async): Future[HttpResponse] =
        import kyo.AllowUnsafe.embrace.danger
        Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(eff).map(_.toFuture))

    def intercept(
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < Async =
        // A bodiless request (GET) has nothing to place in a JSON body array; send it
        // through on its own.
        if request.body.isEmpty then chain.proceed(request)
        else
            // `Sync.defer` so the enqueue + timer-arm happen when the effect RUNS (not
            // when it is built), preserving the original per-execution batching window.
            Sync
                .defer {
                    val promise = Promise[HttpResponse]()
                    pending = pending :+ Pending(request, chain, promise)
                    if pending.length >= maxBatchSize then flush()
                    else if timer.isEmpty then
                        timer = Present(scheduler.schedule(batchIntervalMillis)(() => flush()))
                    promise
                }
                .map(promise => Async.fromFuture(promise.future))

    /** Send whatever is queued as one batch and reset for the next window. */
    private def flush(): Unit =
        timer.foreach(_())
        timer = Absent
        val batch = pending
        pending = Nil
        batch match
            case Nil        => ()
            case one :: Nil =>
                // A batch of one is just a normal request — do not wrap it in an array.
                runToFuture(one.chain.proceed(one.request)).onComplete(one.response.complete)
            case _ => sendBatch(batch)
        end match
    end flush

    /** POST the merged JSON array of every queued body, then fan the array response
      * back out. Uses the first request's chain/URL/headers to carry the batch.
      */
    private def sendBatch(batch: List[Pending]): Unit =
        val bodies  = batch.map(_.request.body.getOrElse("null"))
        val batched = batch.head.request.copy(body = Some(bodies.mkString("[", ",", "]")))
        runToFuture(batch.head.chain.proceed(batched)).onComplete {
            case Success(response) => split(batch, response)
            case Failure(cause)    => batch.foreach(_.response.failure(cause))
        }
    end sendBatch

    /** Split a batched `response` into one [[HttpResponse]] per caller, preserving
      * order. A non-2xx status is shared verbatim; a 2xx body that is not a JSON
      * array of exactly `batch.length` elements fails every caller.
      */
    private def split(batch: List[Pending], response: HttpResponse): Unit =
        if !response.isSuccessful then batch.foreach(_.response.success(response))
        else
            Try(JsonParser.parse(response.body)) match
                case Success(Json.JArr(items)) if items.length == batch.length =>
                    batch.zip(items).foreach { (item, json) =>
                        item.response.success(
                            HttpResponse(response.statusCode, response.headers, json.render)
                        )
                    }
                case _ =>
                    val cause = new RuntimeException(
                        s"Batched GraphQL response was not a JSON array of ${batch.length} " +
                            s"element(s): ${response.body}"
                    )
                    batch.foreach(_.response.failure(cause))
end BatchingHttpInterceptor
