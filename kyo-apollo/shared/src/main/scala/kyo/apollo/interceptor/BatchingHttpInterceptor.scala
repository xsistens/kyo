package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

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
  * opens a `batchInterval` window; the batch is dispatched when the window ends
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
  * not an array of the expected length fails every caller (the transport maps
  * that to an `ApolloNetworkException` value) rather than silently dropping
  * responses.
  *
  * Ownership and concurrency: the interceptor is created with
  * [[BatchingHttpInterceptor.init]] inside a `Scope` that owns its window fiber;
  * when that `Scope` ends the fiber stops, callers still waiting fail with a
  * [[kyo.Closed]] panic, and later calls fail the same way instead of hanging.
  * The pending queue is an `AtomicRef` updated by CAS — no locks, no second
  * runtime. A caller that is interrupted while waiting leaves the queue before
  * the batch is cut, so nothing is sent on its behalf; once its batch is in
  * flight the answer is simply discarded.
  *
  * The wire round trip always runs in a caller's fiber, never in the window
  * fiber: the window (or the size cap) only cuts the batch and hands it to its
  * first member, the *lead*, which sends under `Async.mask` and completes the
  * other members. Batches from consecutive windows therefore travel concurrently,
  * and an interrupt aimed at the lead cannot lose the batch: it is deferred past
  * the send, and if it lands before the send starts, the remaining members are
  * re-queued at the front and a new window opens.
  */
final class BatchingHttpInterceptor private (
    batchInterval: Duration,
    maxBatchSize: Int,
    pending: AtomicRef[Maybe[Chunk[BatchingHttpInterceptor.Pending]]],
    window: Channel[Unit],
    createdAt: Frame
) extends HttpInterceptor:
    import BatchingHttpInterceptor.*

    def intercept(
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < Async =
        // A bodiless request (GET) has nothing to place in a JSON body array; send it
        // through on its own.
        if request.body.isEmpty then chain.proceed(request)
        else
            for
                turn    <- Promise.init[Turn, Any]
                claimed <- AtomicBoolean.init
                me = Pending(request, chain, turn, claimed)
                queued <- pending.updateAndGet(_.map(_.append(me)))
                response <- queued match
                    case Absent => Abort.panic(closed)
                    case Present(batch) =>
                        kick(batch.length).andThen {
                            Sync.ensure(outcome => outcome.fold(())(_ => withdraw(me))) {
                                turn.get.map {
                                    case Turn.Done(response) => response
                                    case Turn.Lead(batch)    => Async.mask(claim(me).andThen(send(batch)))
                                }
                            }
                        }
            yield response

    /** Number of callers waiting for the next window; `0` once the `Scope` ended. */
    private[interceptor] def queued(using Frame): Int < Sync =
        pending.get.map(_.fold(0)(_.length))

    /** React to the queue having grown to `length`: the size cap dispatches at
      * once, the first entry opens a window, anything in between just waits.
      */
    private def kick(length: Int)(using Frame): Unit < Sync =
        if length >= maxBatchSize then dispatch
        else if length == 1 then open
        else ()

    /** Ask the window fiber for a window. `offer` on the capacity-1 channel is a
      * no-op while a request for a window is already parked, and `Closed` means
      * the `Scope` is ending — the teardown drain answers the callers then.
      */
    private def open(using Frame): Unit < Sync =
        Abort.run[Closed](window.offer(())).unit

    /** Cut whatever is queued as one batch (atomically) and hand it to its first
      * member, who becomes the lead and sends it. Nothing is sent from here.
      */
    private def dispatch(using Frame): Unit < Sync =
        pending.getAndUpdate(_.map(_ => Chunk.empty)).map {
            case Present(batch) if !batch.isEmpty =>
                batch(0).turn.completeDiscard(Result.succeed(Turn.Lead(batch)))
            case _ => ()
        }

    /** Finalizer of a caller that did not complete normally (interrupted, or its
      * send failed). Leaves the queue if still queued; and if the caller had been
      * made lead but never claimed the batch, gives the batch back to the queue so
      * the other members are not stranded.
      */
    private def withdraw(me: Pending)(using Frame): Unit < Sync =
        pending.getAndUpdate(_.map(_.filter(_ ne me))).andThen {
            me.turn.poll.map {
                case Present(Result.Success(Turn.Lead(batch))) =>
                    claim(me).map(won => if won then requeue(batch.filter(_ ne me)) else ())
                case _ => ()
            }
        }

    /** Take responsibility for sending the lead's batch: exactly one of the send
      * path and the finalizer wins.
      */
    private def claim(me: Pending)(using Frame): Boolean < Sync =
        me.claimed.compareAndSet(false, true)

    /** Put `rest` back at the front of the queue (they are older than anything
      * queued since) and react as if they had just arrived — unless the `Scope`
      * has ended, in which case they are answered like every other stranded caller.
      */
    private def requeue(rest: Chunk[Pending])(using Frame): Unit < Sync =
        if rest.isEmpty then ()
        else
            pending.updateAndGet(_.map(rest.concat(_))).map {
                case Present(all) => kick(all.length)
                case Absent       => fail(rest, closed)
            }

    /** The lead's send: one unwrapped request for a batch of one, otherwise the
      * JSON array of every body on the lead's chain/URL/headers. Every other
      * member is completed with its share; the lead's own share is returned.
      */
    private def send(batch: Chunk[Pending])(using Frame): HttpResponse < Async =
        val lead = batch(0)
        val wire =
            if batch.length == 1 then lead.request
            else lead.request.copy(body = Some(batch.map(_.request.body.getOrElse("null")).mkString("[", ",", "]")))
        Abort.run[Throwable](lead.chain.proceed(wire)).map { outcome =>
            val shares = outcome.fold(
                response => if batch.length == 1 then Chunk(Result.succeed(response)) else split(batch.length, response),
                cause => Chunk.from(Seq.fill(batch.length)(Result.panic(cause))),
                cause => Chunk.from(Seq.fill(batch.length)(Result.panic(cause)))
            )
            Kyo.foreachDiscard(batch.drop(1).zip(shares.drop(1))) { (member, share) =>
                member.turn.completeDiscard(share.map(Turn.Done(_)))
            }.andThen(Abort.get(shares(0)))
        }
    end send

    /** Split a batched `response` into one share per caller, preserving order. A
      * non-2xx status is shared verbatim; a 2xx body that is not a JSON array of
      * exactly `n` elements fails every caller.
      */
    private def split(n: Int, response: HttpResponse): Chunk[Result[Nothing, HttpResponse]] =
        if !response.isSuccessful then Chunk.from(Seq.fill(n)(Result.succeed(response)))
        else
            Result.catching[Throwable](JsonParser.parse(response.body)) match
                case Result.Success(Json.JArr(items)) if items.length == n =>
                    items.map(json => Result.succeed(HttpResponse(response.statusCode, response.headers, json.render)))
                case _ =>
                    val cause = new RuntimeException(
                        s"Batched GraphQL response was not a JSON array of $n element(s): ${response.body}"
                    )
                    Chunk.from(Seq.fill(n)(Result.panic(cause)))

    /** Answer every member of `batch` with a `cause` panic. */
    private def fail(batch: Chunk[Pending], cause: Throwable)(using Frame): Unit < Sync =
        Kyo.foreachDiscard(batch)(_.turn.completeDiscard(Result.panic(cause)))

    private def closed(using Frame): Closed =
        Closed("BatchingHttpInterceptor", createdAt, "the Scope that created the interceptor has ended")

end BatchingHttpInterceptor

object BatchingHttpInterceptor:

    /** What a queued caller is told once its batch is cut: either it is the lead
      * and must send `batch`, or the lead has sent and this is its share.
      */
    private enum Turn:
        case Lead(batch: Chunk[Pending])
        case Done(response: HttpResponse)

    /** One queued request awaiting its batch: the wire request, the chain that
      * will carry the (merged) request onward, the promise it waits on, and the
      * claim flag that decides between sending the batch and giving it back.
      */
    final private case class Pending(
        request: HttpRequest,
        chain: HttpInterceptorChain,
        turn: Promise[Turn, Any],
        claimed: AtomicBoolean
    )

    /** Create the interceptor and start its window fiber in the current `Scope`.
      * When the `Scope` ends the fiber is interrupted, waiting callers fail with a
      * [[kyo.Closed]] panic, and later calls fail the same way.
      *
      * @param batchInterval how long the first request waits for companions
      * @param maxBatchSize  dispatch eagerly once this many requests are pending
      */
    def init(
        batchInterval: Duration = 10.millis,
        maxBatchSize: Int = 10
    )(using frame: Frame): BatchingHttpInterceptor < (Sync & Scope) =
        for
            pending <- AtomicRef.init(Maybe(Chunk.empty[Pending]))
            window  <- Channel.init[Unit](1)
            self = new BatchingHttpInterceptor(batchInterval, maxBatchSize, pending, window, frame)
            // Registered before the fiber so it runs after the fiber is interrupted:
            // nothing dispatches any more, so whoever is still queued is answered here.
            _ <- Scope.ensure(pending.getAndSet(Absent).map(_.fold(())(self.fail(_, self.closed))))
            _ <- Fiber.init(
                Abort.run[Closed](Loop.forever(window.take.andThen(Async.sleep(batchInterval)).andThen(self.dispatch))).unit
            )
        yield self

end BatchingHttpInterceptor
