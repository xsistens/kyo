package kyo.apollo

import kyo.*
import kyo.apollo.cache.normalized.apolloStore
import kyo.apollo.exception.ApolloException

/** react-apollo's imperative store operations on the client (`refetchQueries` and
  * `resetStore`), built on the per-client [[ActiveQueryRegistry]] the kyo-ui query
  * binding populates.
  *
  * Note the division of labor: for the common "refetch after a mutation" case you
  * usually need **none of this** — a write to a shared normalized record already
  * re-emits every dependent watcher (and `apolloStore.updateOperation` splices a
  * new entity into a cached list without a round-trip). These helpers exist for
  * the cases the cache genuinely cannot serve: forcing a real **network** refetch
  * of server-computed results, and a post-login "everything fresh" reset.
  */
extension (client: ApolloClient)

    /** Force a NETWORK refetch of the named live queries — or of **every** active
      * query when `names` is empty (react's `client.refetchQueries()`). Each match
      * re-runs its operation `NetworkOnly`, rewriting the store (which re-emits its
      * `state`). Names are GraphQL operation names (e.g. `"GetTodos"`).
      */
    def refetchQueries(names: String*)(using Frame): Unit < (Async & Abort[ApolloException]) =
        client.activeQueries.selected(names.toSet).map(Kyo.foreachDiscard(_)(refetch => refetch))

    /** react-apollo's `client.resetStore`: clear the whole normalized cache, run any
      * registered reset hooks, then refetch every active query from the network so
      * the UI rebuilds from fresh server data. (`clearAll` alone does not publish, so
      * without this the watchers would not re-emit.)
      */
    def resetStore(using Frame): Unit < (Async & Abort[ApolloException]) =
        client.apolloStore.clearAll
            .andThen(client.activeQueries.resetHookEffects.map(Kyo.foreachDiscard(_)(hook => hook)))
            .andThen(client.activeQueries.selected(Set.empty).map(Kyo.foreachDiscard(_)(refetch => refetch)))

end extension
