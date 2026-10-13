package kyo.apollo.devtools

import kyo.apollo.json.Json
import kyo.discard

/** In-memory record of the operations a client has run, in the shape the Apollo
  * Client Devtools Queries/Mutations tabs reflect off an Apollo Client —
  * `client.getObservableQueries("active")` and `client.queryManager.mutationStore`.
  *
  * kyo-apollo has no operation-history store of its own ([[kyo.apollo.ActiveQueryRegistry]]
  * tracks only live watchers, with no per-op status/variables/error), so this is
  * the substrate the devtools shim reads. It is fed by [[DevtoolsInterceptor]], a
  * head interceptor that records each operation as it flows through the chain.
  *
  * Queries are keyed by operation name (latest state wins), so the Queries tab
  * shows each distinct query with its current network status — a deliberate,
  * robust simplification over modelling exact watcher activation/teardown, and a
  * superset of live watchers (one-shot queries are surfaced too). Mutations are a
  * bounded most-recent history keyed by request id.
  *
  * The store keeps the variables it is given as they are. [[DevtoolsInterceptor]]
  * hands it a mutation's variables with every value redacted unless it was built
  * with a `redact` that keeps them.
  */
final class DevtoolsOperationStore(val maxMutations: Int = 25):
    import DevtoolsOperationStore.*

    private val queries   = scala.collection.mutable.LinkedHashMap.empty[String, QueryRecord]
    private val mutations = scala.collection.mutable.LinkedHashMap.empty[String, MutationRecord]

    /** Record (or refresh) a query's current state, keyed by operation name. */
    def upsertQuery(
        name: String,
        document: String,
        variables: Json,
        networkStatus: Int,
        error: Option[String],
        data: Option[Json]
    ): Unit = synchronized {
        queries.update(name, QueryRecord(name, document, variables, networkStatus, error, data))
    }

    /** Begin recording a mutation (loading), keyed by request id, evicting the
      * oldest entry once the bounded history is full.
      */
    def startMutation(id: String, name: String, document: String, variables: Json): Unit =
        synchronized {
            mutations.update(id, MutationRecord(name, document, variables, loading = true, error = None))
            while mutations.size > maxMutations do discard(mutations.remove(mutations.head._1))
        }

    /** Settle a previously-started mutation (`loading = false`, capturing an error). */
    def settleMutation(id: String, error: Option[String]): Unit = synchronized {
        mutations.get(id).foreach(m => mutations.update(id, m.copy(loading = false, error = error)))
    }

    /** The current queries, in insertion order. */
    def queriesSnapshot: List[QueryRecord] = synchronized(queries.values.toList)

    /** The recent mutation history, oldest first. */
    def mutationsSnapshot: List[MutationRecord] = synchronized(mutations.values.toList)

    /** Drop all recorded operations. */
    def clear(): Unit = synchronized {
        queries.clear()
        mutations.clear()
    }
end DevtoolsOperationStore

object DevtoolsOperationStore:

    /** A query's reflected state: its document (SDL, parsed to a graphql-js AST by
      * the shim), variables, Apollo `NetworkStatus` (1 = loading, 7 = ready, 8 =
      * error), an optional error message, and its latest data (the panel's cached
      * result, `getCacheDiff().result`).
      */
    final case class QueryRecord(
        name: String,
        document: String,
        variables: Json,
        networkStatus: Int,
        error: Option[String],
        data: Option[Json]
    )

    /** A mutation's reflected state for the `mutationStore`. */
    final case class MutationRecord(
        name: String,
        document: String,
        variables: Json,
        loading: Boolean,
        error: Option[String]
    )
end DevtoolsOperationStore
