package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

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
  * an array a non-batching server would reject. Only JSON bodies are batched: a
  * request without a body (a `GET`, which carries the operation in its URL) and a
  * multipart upload are forwarded immediately and individually.
  *
  * All batched requests are sent with the first request's URL and headers — the
  * same limitation apollo-kotlin's `BatchingHttpInterceptor` has — so per-request
  * header overrides (e.g. distinct auth) are not preserved across a batch. Errors
  * are shared fairly: a non-2xx batched response is handed to every caller (each
  * transport maps it to an `ApolloHttpException` value), the engine's
  * `HttpEngineFailure` fails every caller with that same failure, and a 2xx body
  * that is not an array of the expected length fails every caller with an
  * `ApolloNetworkException` (no response of its own arrived) rather than silently
  * dropping responses.
  *
  * Ownership and concurrency: the interceptor is created with
  * [[BatchingHttpInterceptor.init]] inside a `Scope` that owns its window fiber;
  * when that `Scope` ends the fiber stops, callers still waiting fail with a
  * [[kyo.Closed]] panic, and later calls fail the same way instead of hanging.
  * The pending queue is an `AtomicRef` updated by CAS — no locks, no second
  * runtime. A caller that is interrupted while waiting leaves the queue, so a
  * batch cut after that sends nothing on its behalf; once its batch is cut the
  * answer is simply discarded. Each caller's `turn` promise is the single point
  * where the cut and the caller's leaving meet: whichever settles it first wins,
  * so a batch whose first member has just left passes to the next member.
  *
  * The wire round trip always runs in a caller's fiber, never in the window
  * fiber: the window (or the size cap) only cuts the batch and hands it to its
  * first member, the *lead*, which sends under `Async.uninterruptible` and completes the
  * other members. Batches from consecutive windows therefore travel concurrently,
  * and an interrupt aimed at the lead cannot lose the batch: it is deferred past
  * the send, and if it lands before the send starts, the remaining members still
  * waiting are re-queued at the front and a new window opens. Joining the queue
  * and requesting the window for it are one step, so a caller interrupted as it
  * joins never leaves the callers queued behind it without a window.
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
        request: HttpEngine.Request,
        chain: HttpInterceptorChain
    )(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        request.fields.body match
            // Only a JSON document fits in a JSON body array; a bodiless GET and a
            // multipart upload go through on their own.
            case HttpRequestBody.Text(json) => enqueue(request, json, chain)
            case _                          => chain.proceed(request)

    /** Queue `request` (whose JSON body is `json`) for the next batch and wait for its turn. */
    private def enqueue(
        request: HttpEngine.Request,
        json: String,
        chain: HttpInterceptorChain
    )(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        for
            // Interruptible on purpose: this caller is its only waiter, so an interrupt that settles it is this caller
            // leaving, and `dispatch` passes over it exactly as over a turn `withdraw` settled.
            turn    <- Promise.init[Turn, Abort[HttpEngineFailure]]
            claimed <- AtomicBoolean.init
            me = Pending(request, json, chain, turn, claimed)
            // The finalizer is in place before `me` is queued: an interrupt landing between the two would otherwise
            // leave `me` in the queue with nothing to take it out. `withdraw` of a caller that never got queued is a no-op.
            // The type argument is load-bearing: inferred from `()`, `fold` types as Unit and the withdraw is built and
            // discarded unrun.
            response <- Sync.ensure(outcome => outcome.fold[Unit < Sync](())(_ => withdraw(me))) {
                join(1)(_.append(me)).map {
                    case Absent         => Abort.panic(closed)
                    case Present(batch) =>
                        kick(batch.length).andThen {
                            turn.get.map {
                                case Turn.Done(response) => response
                                case Turn.Lead(batch)    => Async.uninterruptible(claim(me).andThen(send(batch)))
                            }
                        }
                }
            }
        yield response
    end enqueue

    /** Number of callers waiting for the next window; `0` once the `Scope` ended. */
    private[interceptor] def queued(using Frame): Int < Sync =
        pending.get.map(_.fold(0)(_.length))

    /** Add `count` members to the queue with `add` and, when the queue was empty
      * before, ask the window fiber for a window, as one step. Returns the queue
      * after, or `Absent` once the `Scope` has ended.
      *
      * A non-empty queue always has a window coming: only a join that finds the
      * queue empty asks for one, so a later joiner relies on the earlier one's
      * request. `offer` on the capacity-1 channel is a no-op while a request is
      * already parked, and `Closed` means the `Scope` is ending, when the
      * teardown drain answers the callers.
      */
    private def join(count: Int)(add: Chunk[Pending] => Chunk[Pending])(using Frame): Maybe[Chunk[Pending]] < Sync =
        // Unsafe: the update and the window request have to be one step no interrupt can split. A caller interrupted
        // between them leaves the queue again, but whoever joined in the meantime found it non-empty and asked for no
        // window, so nothing would ever cut their batch.
        Sync.Unsafe.defer {
            val queue = pending.unsafe.updateAndGet(_.map(add))
            queue match
                case Present(all) if all.length == count => discard(window.unsafe.offer(()))
                case _                                   => ()
            queue
        }

    /** React to the queue having grown to `length`: the size cap dispatches at
      * once, anything below it waits for the window [[join]] asked for.
      */
    private def kick(length: Int)(using Frame): Unit < Sync =
        if length >= maxBatchSize then dispatch else ()

    /** Cut whatever is queued as one batch (atomically) and hand it to its lead.
      * Nothing is sent from here.
      *
      * A member's `turn` decides whether it leads: the lead is the first member
      * whose turn this call completes with `Lead`. A member whose turn is already
      * settled has left (its finalizer withdrew it, or the interrupt that ended
      * its wait settled the turn), so it is dropped and the batch passes to the
      * next member. When no member's turn is still open, nothing is sent.
      */
    private def dispatch(using Frame): Unit < Sync =
        pending.getAndUpdate(_.map(_ => Chunk.empty)).map {
            case Present(batch) => lead(batch)
            case Absent         => ()
        }

    /** Make the first member of `rest` whose turn is still open the lead of the members from it on. */
    private def lead(rest: Chunk[Pending])(using Frame): Unit < Sync =
        if rest.isEmpty then ()
        else
            rest(0).turn.complete(Result.succeed(Turn.Lead(rest))).map { led =>
                if led then () else lead(rest.drop(1))
            }

    /** Finalizer of a caller that did not complete normally (interrupted, or its
      * send failed). Leaves the queue if still queued, then settles its own turn
      * as withdrawn. The turn is the one point both sides decide on: if this
      * settles it, no dispatch can make the caller lead any more; if it was
      * already settled with `Lead` but the caller never claimed the batch, the
      * batch goes back to the queue so the other members are not stranded. Any
      * other settled turn (an answer, or the interrupt that ended the wait) means
      * the caller is out already.
      */
    private def withdraw(me: Pending)(using Frame): Unit < Sync =
        pending.getAndUpdate(_.map(_.filter(_ ne me))).andThen {
            me.turn.complete(Withdrawn.result).map { left =>
                if left then ()
                else
                    me.turn.poll.map {
                        case Present(Result.Success(Turn.Lead(batch))) =>
                            claim(me).map(won => if won then requeue(batch.filter(_ ne me)) else ())
                        case _ => ()
                    }
            }
        }

    /** Take responsibility for sending the lead's batch: exactly one of the send
      * path and the finalizer wins.
      */
    private def claim(me: Pending)(using Frame): Boolean < Sync =
        me.claimed.compareAndSet(false, true)

    /** Put the members of `rest` still waiting back at the front of the queue
      * (they are older than anything queued since) and react as if they had just
      * arrived, a window included when the queue was empty: the one that covered
      * them may have ended already. A member that has left meanwhile is dropped,
      * so nothing is sent on its behalf. Once the `Scope` has ended they are
      * answered like every other stranded caller.
      */
    private def requeue(rest: Chunk[Pending])(using Frame): Unit < Sync =
        Kyo.filter(rest)(_.turn.done.map(!_)).map { waiting =>
            if waiting.isEmpty then ()
            else
                join(waiting.length)(waiting.concat(_)).map {
                    case Present(all) => kick(all.length)
                    case Absent       => fail(waiting, closed)
                }
        }

    /** The lead's send: one unwrapped request for a batch of one, otherwise the
      * JSON array of every body on the lead's chain/URL/headers. Every other
      * member is completed with its share; the lead's own share is returned.
      */
    private def send(batch: Chunk[Pending])(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        val lead = batch(0)
        val wire =
            if batch.length == 1 then lead.request
            else
                val array = HttpRequestBody.Text(batch.map(_.json).mkString("[", ",", "]"))
                lead.request.copy(fields = lead.request.fields.update("body", array))
        Abort.run[HttpEngineFailure](lead.chain.proceed(wire)).map { outcome =>
            val shares: Chunk[Result[HttpEngineFailure, HttpEngine.Response]] = outcome.fold(
                response => if batch.length == 1 then Chunk(Result.succeed(response)) else split(batch.length, response),
                failure => Chunk.from(Seq.fill(batch.length)(Result.fail(failure))),
                cause => Chunk.from(Seq.fill(batch.length)(Result.panic(cause)))
            )
            Kyo.foreachDiscard(batch.drop(1).zip(shares.drop(1))) { (member, share) =>
                member.turn.completeDiscard(share.map(Turn.Done(_)))
            }.andThen(Abort.get(shares(0)))
        }
    end send

    /** Split a batched `response` into one share per caller, preserving order. A
      * non-2xx status is shared verbatim; a 2xx body that is not a JSON array of
      * exactly `n` elements fails every caller: none of them received its response.
      */
    private def split(n: Int, response: HttpEngine.Response)(using
        Frame
    ): Chunk[Result[HttpEngineFailure, HttpEngine.Response]] =
        if !response.status.isSuccess then Chunk.from(Seq.fill(n)(Result.succeed(response)))
        else
            JsonParser.parse(response.fields.body) match
                case Result.Success(Json.JArr(items)) if items.length == n =>
                    items.map(json => Result.succeed(HttpEngine.response(response.status, json.render, response.headers)))
                case _ =>
                    val failure = ApolloNetworkException(
                        s"Batched GraphQL response was not a JSON array of $n element(s): ${response.fields.body}"
                    )
                    Chunk.from(Seq.fill(n)(Result.fail(failure)))

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
        case Done(response: HttpEngine.Response)

    /** The outcome a caller's finalizer settles its own turn with, so that no
      * dispatch can make it lead after it left. Nobody waits on that turn any
      * more, so no caller ever receives it.
      */
    private object Withdrawn extends Exception("batch member withdrawn", null, false, false):
        val result: Result.Panic = Result.Panic(this)

    /** One queued request awaiting its batch: the wire request and its JSON body, the
      * chain that will carry the (merged) request onward, the promise it waits on, and
      * the claim flag that decides between sending the batch and giving it back.
      */
    final private case class Pending(
        request: HttpEngine.Request,
        json: String,
        chain: HttpInterceptorChain,
        turn: Promise[Turn, Abort[HttpEngineFailure]],
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
            _ <- Scope.ensure(pending.getAndSet(Absent).map(_.fold[Unit < Sync](())(self.fail(_, self.closed))))
            _ <- Fiber.init(
                Abort.run[Closed](Loop.forever(window.take.andThen(Async.sleep(batchInterval)).andThen(self.dispatch))).unit
            )
        yield self

end BatchingHttpInterceptor
