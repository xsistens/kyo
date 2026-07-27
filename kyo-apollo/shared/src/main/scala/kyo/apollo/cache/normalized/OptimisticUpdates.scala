package kyo.apollo.cache.normalized

import kyo.apollo.ApolloCall
import kyo.apollo.network.ExecutionContext
import kyo.apollo.network.Uuid

/** Per-call carrier for a mutation's optimistic data, riding the request's
  * [[ExecutionContext]] exactly like [[FetchPolicy]] — so no new
  * [[kyo.apollo.network.ApolloRequest]] field is introduced.
  *
  * `ApolloCall.optimisticUpdates(data)` attaches it; the [[CacheInterceptor]]
  * reads it back with `context.get(OptimisticData)`, overlays `data` into the
  * store before the network call, and rolls the layer back — merging server truth
  * on success, reverting cleanly on failure — keyed by [[mutationId]].
  *
  * `data` is held as `Any` because an [[ExecutionContext.Element]] cannot be typed
  * per call site; the interceptor casts it back to the operation's data type — the
  * same erasure apollo-kotlin's optimistic path relies on.
  *
  * @param data       the optimistic value to overlay (the operation's data type)
  * @param mutationId the id the optimistic layer is stacked and rolled back under
  */
final case class OptimisticData(data: Any, mutationId: String) extends ExecutionContext.Element:
    override def key: ExecutionContext.Key[OptimisticData] = OptimisticData

object OptimisticData extends ExecutionContext.Key[OptimisticData]

extension [D](call: ApolloCall[D])
    /** Attach optimistic `data` to this mutation: the [[CacheInterceptor]] overlays
      * it into the store *before* the network call — so watchers immediately see the
      * optimistic state — then, when the network replies, drops the optimistic layer
      * and merges the real result (on success) or reverts cleanly (on failure).
      *
      * Each call tags its layer with a fresh mutation id, so concurrent optimistic
      * mutations stack independently and roll back independently. Rides the
      * request's [[ExecutionContext]] like `fetchPolicy`, returning a sibling call;
      * inert (plain metadata) without a normalized cache installed. Mirrors
      * apollo-kotlin's `ApolloCall.optimisticUpdates(data)`.
      */
    def optimisticUpdates(data: D): ApolloCall[D] =
        call.withRequest(
            call.apolloRequest.newBuilder
                .addExecutionContext(ExecutionContext.Empty + OptimisticData(data, Uuid.random().value))
                .build()
        )
end extension
