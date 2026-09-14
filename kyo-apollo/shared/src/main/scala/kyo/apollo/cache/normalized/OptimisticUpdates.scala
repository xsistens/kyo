package kyo.apollo.cache.normalized

import kyo.apollo.ApolloCall

extension [D](call: ApolloCall[D])
    /** Attach optimistic `data` to this mutation: the [[CacheInterceptor]] overlays
      * it into the store *before* the network call — so watchers immediately see the
      * optimistic state — then, when the network replies, drops the optimistic layer
      * and merges the real result (on success) or reverts cleanly (on failure).
      *
      * The data rides the request as its typed `optimisticData` field, returning a
      * sibling call; inert without a normalized cache installed. Each execution of
      * the call tags its layer with a mutation id minted when that execution starts,
      * so two executions of one call — a retry, two concurrent runs — stack two
      * layers and roll back independently. Mirrors apollo-kotlin's
      * `ApolloCall.optimisticUpdates(data)`.
      */
    def optimisticUpdates(data: D): ApolloCall[D] =
        call.withRequest(_.optimisticData(data))
end extension
