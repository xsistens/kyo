package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException

/** Per-client registry of the **live queries** (`useQuery` watchers). Populated by
  * the kyo-ui binding when a live query is bound within a `Scope` (and its entry
  * removed on `Scope` teardown), and drained by `client.refetchQueries` /
  * `client.resetStore`.
  *
  * This is the seam React's imperative `refetchQueries` / `resetStore` need: the
  * normalized cache already re-emits every watcher of a shared record on write
  * (so most "refetch after mutate" cases need no API at all), but there is
  * otherwise **no way to enumerate active operations** to force a genuine NETWORK
  * refetch — the case that matters for server-computed results (a new entity a
  * list doesn't yet reference, server-side sorting/pagination) and for a
  * post-login "everything fresh" reset.
  *
  * Single-threaded (Scala.js) mutable state, the same idiom `QueryHandle`'s poll
  * cell uses; a plain `LinkedHashMap` keeps registration order stable.
  */
final class ActiveQueryRegistry:

    private var seq: Long = 0L
    private val queries =
        scala.collection.mutable.LinkedHashMap
            .empty[Long, (String, Unit < (Async & Abort[ApolloException]))]
    private val resetHooks =
        scala.collection.mutable.LinkedHashMap.empty[Long, Unit < Async]

    /** Register a live query by its operation name plus a NetworkOnly refetch effect
      * (typically `handle.refetch.unit`). Returns a dispose thunk to call on `Scope`
      * teardown.
      */
    def register(
        operationName: String,
        refetch: Unit < (Async & Abort[ApolloException])
    ): () => Unit =
        val id = seq
        seq += 1
        queries.update(id, (operationName, refetch))
        () =>
            val _ = queries.remove(id)
    end register

    /** Register an `onResetStore` hook fired by [[resetStore]]. Returns a dispose thunk. */
    def registerResetHook(hook: Unit < Async): () => Unit =
        val id = seq
        seq += 1
        resetHooks.update(id, hook)
        () =>
            val _ = resetHooks.remove(id)
    end registerResetHook

    /** Refetch effects for the named operations — or **all** live queries when
      * `names` is empty (react's `refetchQueries()` with no filter).
      */
    private[apollo] def selected(names: Set[String]): List[Unit < (Async & Abort[ApolloException])] =
        queries.valuesIterator.collect {
            case (name, refetch) if names.isEmpty || names.contains(name) => refetch
        }.toList

    private[apollo] def resetHookEffects: List[Unit < Async] = resetHooks.valuesIterator.toList
end ActiveQueryRegistry
