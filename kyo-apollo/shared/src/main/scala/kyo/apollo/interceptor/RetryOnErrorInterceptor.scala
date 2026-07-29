package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.api.Subscription
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ws.WsBackoff
import kyo.apollo.network.ws.WsScheduler
import kyo.apollo.runtime.ResponseStream
import scala.concurrent.Promise
import scala.util.Random

/** An [[ApolloInterceptor]] that transparently re-runs an operation when it comes
  * back as a **transport failure**, backing off between attempts.
  *
  * The retry decision is made on `ApolloResponse.error` — a network drop or a
  * 5xx HTTP status — never on GraphQL `errors`. A partial-data response with
  * GraphQL errors is a legitimate server answer, not a transient fault, so it is
  * passed straight through (per the Task 1 reuse doc §3.5: "Retry classifies on
  * `ApolloResponse.error` (transport) vs GraphQL errors in `data` — only the
  * former is retried"). The classifier is injectable via `retryWhen`; the default
  * [[RetryOnErrorInterceptor.transportErrors]] retries [[ApolloNetworkException]]
  * and 5xx [[ApolloHttpException]]s.
  *
  * Backoff reuses the existing [[WsBackoff]] curve (exponential, 1s/2s/4s… by
  * default) and the existing [[WsScheduler]] timer seam — the same two pieces the
  * WebSocket reconnection path uses — rather than introducing a parallel timer or
  * backoff type. A configurable `jitterFactor` spreads the scheduled delay across
  * `[base·(1−jitterFactor), base]` so a fleet of clients retrying together does
  * not thunder; set it to `0` for exact, test-friendly delays. Randomness is
  * injectable (`random`) so tests are deterministic.
  *
  * The retry decision rides an attempt's **first** emission — a transport failure
  * there means the operation never produced a usable response. Every emission is
  * otherwise forwarded unchanged: an `@defer` operation streams incrementally and a
  * `CacheAndNetwork` query emits the cache hit then the network result, so collapsing
  * to the first emission would silently drop the deferred patches / network refresh.
  * A [[Subscription]] is a live stream and is passed through without any retry wrapping.
  *
  * Mirrors apollo-kotlin's `RetryOnErrorInterceptor`.
  *
  * @param maxAttempts  total attempts including the first (must be ≥ 1)
  * @param backoff      the delay curve consulted with the 1-based attempt number
  * @param jitterFactor fraction of the backoff delay that is randomized (0 = off)
  * @param random       source of `[0,1)` randomness for the jitter (injectable)
  * @param scheduler    the timer the inter-attempt delay is armed on
  * @param retryWhen    which transport exceptions are retryable
  */
final class RetryOnErrorInterceptor(
    maxAttempts: Int = 3,
    backoff: WsBackoff = WsBackoff.exponential(),
    jitterFactor: Double = 0.5,
    random: () => Double = () => Random.nextDouble(),
    scheduler: WsScheduler = WsScheduler.default,
    retryWhen: ApolloException => Boolean = RetryOnErrorInterceptor.transportErrors
) extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        request.operation match
            // A subscription is a live stream, not a single request/response — never
            // wrap it in the retry machinery.
            case _: Subscription[?] => chain.proceed(request)
            case _                  => attempt(request, chain, 1)

    /** Run attempt number `n` (1-based). On a retryable transport failure in the
      * attempt's first emission — attempts remaining — wait out the backoff and
      * re-run; otherwise forward the first response and every later emission
      * untouched (see the class doc on why the tail must not be collapsed).
      */
    private def attempt[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain,
        n: Int
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap {
            chain.proceed(request).splitAt(1).map { case (head, rest) =>
                if head.isEmpty then rest
                else
                    val first = head.head
                    // `hasTransportError` keeps `retryWhen` seeing only transport failures,
                    // as its contract promises: a response carrying the server's own
                    // GraphQL errors is an answer, not a connection worth retrying.
                    first.error match
                        case Present(cause)
                            if first.hasTransportError && n < maxAttempts && retryWhen(cause) =>
                            Stream.unwrap(
                                delay(jittered(backoff.delayMillis(n)))
                                    .andThen(attempt(request, chain, n + 1))
                            )
                        case _ => Stream.init(Seq(first)).concat(rest)
                    end match
            }
        }

    /** Apply `jitterFactor` to `base`: shrink it by up to `jitterFactor` of itself,
      * choosing the shrink amount from `random()`. `jitterFactor = 0` returns `base`
      * unchanged (deterministic); `1` yields `[0, base]`.
      */
    private def jittered(base: Long): Long =
        if jitterFactor <= 0.0 then base
        else
            val floor = base.toDouble * (1.0 - jitterFactor)
            val span  = base.toDouble * jitterFactor
            (floor + span * random()).toLong

    /** An effect that completes after `millis` on the injected [[WsScheduler]]; an
      * already-nonpositive delay completes immediately without arming a timer. The
      * timer stays on the (deterministically test-injectable) [[WsScheduler]] seam
      * rather than `Async.sleep`, so the retry tests fire it by hand; the armed
      * `Future` is bridged into the effect via `Async.fromFuture`. `Sync.defer`
      * arms the timer when the effect *runs*, not when it is built.
      */
    private def delay(millis: Long)(using Frame): Unit < Async =
        if millis <= 0L then Sync.defer(())
        else
            Sync
                .defer {
                    val armed = Promise[Unit]()
                    discard(scheduler.schedule(millis)(() => discard(armed.trySuccess(()))))
                    armed.future
                }
                .map(Async.fromFuture(_))
end RetryOnErrorInterceptor

object RetryOnErrorInterceptor:

    /** The default retry classifier: a connection-level failure is always
      * transient, and a 5xx is a server-side transient; everything else
      * (4xx, parse errors, cache misses) is not retried.
      */
    val transportErrors: ApolloException => Boolean =
        case _: ApolloNetworkException => true
        case e: ApolloHttpException    => e.statusCode >= 500
        case _                         => false
    end transportErrors
end RetryOnErrorInterceptor
