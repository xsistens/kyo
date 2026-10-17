package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloExecuteFailure
import scala.collection.immutable.VectorMap

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
  * The state lives in kyo atomics, so the registry behaves the same on every
  * platform kyo-apollo builds for: ids come from an [[AtomicLong]] and the
  * entries sit in an [[AtomicRef]] holding an immutable `VectorMap` keyed by id,
  * which keeps registration order (refetch order = registration order) while
  * removing an entry stays O(log n) — a `Chunk` with `filterNot` turns a `Scope`
  * teardown of many entries quadratic. Registration is a `Scope.acquireRelease`
  * pair: the enclosing `Scope` owns the entry and its release removes exactly
  * that entry, never a neighbour.
  */
final class ActiveQueryRegistry private (
    seq: AtomicLong,
    queries: AtomicRef[VectorMap[Long, ActiveQueryRegistry.Entry]],
    resetHooks: AtomicRef[VectorMap[Long, ActiveQueryRegistry.ResetHook]]
):
    import ActiveQueryRegistry.*

    /** Register a live query by its operation name plus a NetworkOnly refetch effect
      * (typically `handle.refetch.unit`) for the lifetime of the enclosing `Scope`.
      * The `Scope`'s teardown removes exactly this entry. A refetch executes the
      * query, so its row is the execute failures.
      */
    def register(
        operationName: String,
        refetch: Unit < (Async & Abort[ApolloExecuteFailure])
    )(using Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            seq.incrementAndGet.map { id =>
                queries.updateAndGet(_.updated(id, Entry(id, operationName, refetch))).andThen(id)
            }
        )(id => queries.updateAndGet(_.removed(id))).unit

    /** Register an `onResetStore` hook fired by `resetStore` for the lifetime of the
      * enclosing `Scope`.
      */
    def registerResetHook(hook: Unit < Async)(using Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            seq.incrementAndGet.map { id =>
                resetHooks.updateAndGet(_.updated(id, ResetHook(id, hook))).andThen(id)
            }
        )(id => resetHooks.updateAndGet(_.removed(id))).unit

    /** Refetch effects for the named operations — or **all** live queries when
      * `names` is empty (react's `refetchQueries()` with no filter). Works on one
      * snapshot of the registry.
      */
    private[apollo] def selected(names: Set[String])(using Frame): Chunk[Unit < (Async & Abort[ApolloExecuteFailure])] < Sync =
        queries.use { qs =>
            Chunk.from(qs.values.collect {
                case Entry(_, name, refetch) if names.isEmpty || names.contains(name) => refetch
            })
        }

    /** Snapshot of the registered `onResetStore` hooks, in registration order. */
    private[apollo] def resetHookEffects(using Frame): Chunk[Unit < Async] < Sync =
        resetHooks.use(hs => Chunk.from(hs.values.map(_.hook)))

    /** Snapshot of the live-query entries, in registration order. */
    private[apollo] def entries(using Frame): Chunk[Entry] < Sync = queries.use(qs => Chunk.from(qs.values))
end ActiveQueryRegistry

object ActiveQueryRegistry:

    final private[apollo] case class Entry(
        id: Long,
        name: String,
        refetch: Unit < (Async & Abort[ApolloExecuteFailure])
    )

    final private[apollo] case class ResetHook(id: Long, hook: Unit < Async)

    /** An empty registry. */
    def init(using Frame): ActiveQueryRegistry < Sync = Sync.Unsafe.defer(Unsafe.init())

    object Unsafe:
        /** An empty registry, for construction outside an effect (the client's pure
          * constructor).
          */
        def init()(using AllowUnsafe): ActiveQueryRegistry =
            new ActiveQueryRegistry(
                AtomicLong.Unsafe.init(0L).safe,
                AtomicRef.Unsafe.init(VectorMap.empty[Long, Entry]).safe,
                AtomicRef.Unsafe.init(VectorMap.empty[Long, ResetHook]).safe
            )
    end Unsafe
end ActiveQueryRegistry
