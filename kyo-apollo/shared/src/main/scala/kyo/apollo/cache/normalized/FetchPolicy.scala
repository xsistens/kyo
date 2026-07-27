package kyo.apollo.cache.normalized

import kyo.apollo.network.ExecutionContext

/** Where an operation may draw its data from, and in what order — the cache /
  * network strategy the [[CacheInterceptor]] executes.
  *
  * A `FetchPolicy` is itself an [[ExecutionContext.Element]] keyed by its own
  * companion, so it rides the existing per-request context bag rather than
  * widening [[kyo.apollo.network.ApolloRequest]]: `ApolloCall.fetchPolicy(p)` attaches
  * it, and the interceptor reads it back with `context.get(FetchPolicy)`. When no
  * policy is attached the interceptor uses [[FetchPolicy.Default]] (`CacheFirst`),
  * matching apollo-kotlin.
  *
  * The emission contract each case produces (see [[CacheInterceptor]]):
  *
  *   - `CacheFirst`     — cache hit → one cache response; on miss, fall through to
  *                        the network (one response, written back).
  *   - `NetworkOnly`    — the network only (one response), always written back;
  *                        the cache is never read.
  *   - `CacheOnly`      — the cache only (one response); a miss surfaces as an
  *                        `ApolloResponse.exception` value, never the network.
  *   - `NetworkFirst`   — the network (written back); on a network error, fall
  *                        back to a cache read, else re-emit the network error.
  *   - `CacheAndNetwork`— a cache response (when hit) **then** a network response,
  *                        in that order — two emissions over one stream.
  *   - `NoCache`        — the network only (one response), and the cache is neither
  *                        read nor written — a pure passthrough (react `no-cache`).
  *   - `Standby`        — never fetches: a cache hit is served, a miss emits
  *                        **nothing** (no value, no error), so the operation is
  *                        "parked" and only reacts to external cache writes
  *                        (react `standby`, for conditional/disabled queries).
  */
enum FetchPolicy extends ExecutionContext.Element derives CanEqual:
    case CacheFirst, NetworkOnly, CacheOnly, NetworkFirst, CacheAndNetwork, NoCache, Standby

    /** Every case is keyed by the [[FetchPolicy]] companion in the context bag. */
    override def key: ExecutionContext.Key[FetchPolicy] = FetchPolicy
end FetchPolicy

object FetchPolicy extends ExecutionContext.Key[FetchPolicy]:
    /** The policy applied when a call pins none — cache-first, like apollo-kotlin. */
    val Default: FetchPolicy = CacheFirst
