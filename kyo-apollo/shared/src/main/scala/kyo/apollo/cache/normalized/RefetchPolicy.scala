package kyo.apollo.cache.normalized

import kyo.apollo.network.ExecutionContext

/** How a watcher re-produces its result when one of its dependent records
  * changes — the re-read / re-fetch strategy that mirrors [[FetchPolicy]] but
  * governs the *update* leg of a `watch()` rather than the initial fetch.
  *
  * Like [[FetchPolicy]], a `RefetchPolicy` is an [[ExecutionContext.Element]]
  * keyed by its own companion, so it rides the existing per-request context bag
  * rather than widening [[kyo.apollo.network.ApolloRequest]]:
  * `ApolloCall.refetchPolicy(p)` attaches it and the watcher reads it back with
  * `context.get(RefetchPolicy)`. When none is attached the watcher uses
  * [[RefetchPolicy.Default]] (`CacheOnly`) — the cheap, network-free re-read that
  * makes reactivity zero-cost, matching apollo-kotlin's default watch behaviour.
  *
  * The two update strategies (see the `watch()` extension in [[Watcher]]):
  *
  *   - `CacheOnly` (default) — re-read the operation from the store through the
  *     same [[kyo.apollo.cache.normalized.internal.CacheBatchReader]] the initial read
  *     used, and emit the fresh value. No network, no write-back.
  *   - `NetworkOnly` — re-run the operation over the network (which writes the
  *     response back into the store), and emit the networked value. Use when a
  *     dependent-key change should trigger a genuine refetch (e.g. a list whose
  *     membership only the server can recompute).
  */
enum RefetchPolicy extends ExecutionContext.Element derives CanEqual:
    case CacheOnly, NetworkOnly

    /** Every case is keyed by the [[RefetchPolicy]] companion in the context bag. */
    override def key: ExecutionContext.Key[RefetchPolicy] = RefetchPolicy
end RefetchPolicy

object RefetchPolicy extends ExecutionContext.Key[RefetchPolicy]:
    /** The policy applied when a watch pins none — a network-free cache re-read. */
    val Default: RefetchPolicy = CacheOnly
