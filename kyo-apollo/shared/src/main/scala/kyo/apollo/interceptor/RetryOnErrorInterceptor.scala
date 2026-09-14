package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.api.Subscription
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** An [[ApolloInterceptor]] that transparently re-runs an operation when it comes
  * back as a **transport failure**, waiting between attempts as a [[kyo.Schedule]]
  * prescribes.
  *
  * The retry decision is made on `ApolloResponse.error` — a network drop or a
  * 5xx HTTP status — never on GraphQL `errors`. A partial-data response with
  * GraphQL errors is a legitimate server answer, not a transient fault, so it is
  * passed straight through. The classifier is injectable via `retryWhen`; the
  * default [[RetryOnErrorInterceptor.transportErrors]] retries
  * [[ApolloNetworkException]] and 5xx [[ApolloHttpException]]s.
  *
  * Each retry waits for the schedule's next delay with `Async.sleep` in the fiber
  * that consumes the response stream, so the wait reads the ambient `Clock`
  * (`Clock.withTimeControl` steers it exactly) and ends with that fiber: a caller
  * interrupted while it waits never starts the retry. When the schedule has no
  * delay left, the last failure is the answer.
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
  * @param schedule  the delays between attempts; the number of delays it yields is
  *                  the number of retries. The default waits 1s, then 2s (three
  *                  attempts in all)
  * @param jitter    the fraction of each delay drawn from kyo's `Random` and taken
  *                  off it: a delay `d` becomes a value in `[d·(1−jitter), d]`, so
  *                  clients retrying together do not arrive together and the
  *                  scheduled delay stays a ceiling. `0` waits the scheduled delays
  *                  exactly; `Random.withSeed` makes the drawn delays reproducible
  * @param retryWhen which transport exceptions are retryable
  */
final class RetryOnErrorInterceptor(
    schedule: Schedule = RetryOnErrorInterceptor.defaultSchedule,
    jitter: Double = 0.5,
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
            case _                  => attempt(request, chain, schedule)

    /** Run one attempt with `remaining` as the rest of the schedule. On a retryable
      * transport failure in the attempt's first emission, wait out the schedule's
      * next delay and re-run with what is left of it; without a next delay, or on
      * any other first emission, forward the first response and every later
      * emission untouched (see the class doc on why the tail must not be collapsed).
      */
    private def attempt[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain,
        remaining: Schedule
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap {
            chain.proceed(request).splitAt(1).map { case (head, rest) =>
                if head.isEmpty then rest
                else
                    val first  = head.head
                    val answer = Stream.init(Seq(first)).concat(rest)
                    // `hasTransportError` keeps `retryWhen` seeing only transport failures,
                    // as its contract promises: a response carrying the server's own
                    // GraphQL errors is an answer, not a connection worth retrying.
                    if !(first.hasTransportError && first.error.exists(retryWhen)) then answer
                    else
                        Stream.unwrap(Clock.now.map(remaining.next(_)).map {
                            case Present((delay, next)) =>
                                jittered(delay).map(Async.sleep(_)).andThen(attempt(request, chain, next))
                            case Absent => answer
                        })
                    end if
            }
        }

    /** Take a random share of up to `jitter` off `delay`, drawn from kyo's `Random`. */
    private def jittered(delay: Duration)(using Frame): Duration < Sync =
        if jitter <= 0.0 then delay
        else Random.nextDouble.map(r => delay * (1.0 - jitter * r))
end RetryOnErrorInterceptor

object RetryOnErrorInterceptor:

    /** Two retries, 1s then 2s apart: kyo's `Schedule.exponentialBackoff` (each
      * delay capped at 30s) limited to two delays.
      */
    val defaultSchedule: Schedule = Schedule.exponentialBackoff(1.second, 2.0, 30.seconds).take(2)

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
