package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloExecuteFailure
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Per-client registry of the **live queries**: every watcher a client has open on an
  * operation, whichever kyo-ui entry point opened it. Each registers once for the
  * lifetime of its `Scope` and is removed on teardown; `client.refetchQueries` and
  * `client.resetStore` drain it.
  *
  * A watcher only ever learns of what is written into its client's cache. This is
  * the seam for the cases where the SERVER knows more than the cache can: data
  * changed by someone else with no feed to bring it, results the server computes (a
  * new entity a list does not reference yet, server-side sorting, counts,
  * pagination), a conflict, and a post-login "everything fresh" reset. A change the
  * client makes itself needs none of it when the mutation's payload carries the
  * changed fields, because the normalized cache already re-emits every watcher of a
  * shared record.
  *
  * An entry yields its CURRENT [[ActiveQueryRegistry.Target]] each time the registry
  * is drained: `Absent` while the watcher is paused (a `skip`, live variables at
  * `Absent`, a lazy query not loaded yet), so a paused watcher takes no part, as in
  * react-apollo, where a standby query is left out. Entries whose targets share a
  * [[ActiveQueryRegistry.QueryKey]] are one query: they are refetched with ONE
  * request, and the cache hands the answer to all of them.
  *
  * The state lives in kyo atomics, so the registry behaves the same on every
  * platform kyo-apollo builds for: ids come from an [[AtomicLong]] and the
  * entries sit in an [[AtomicRef]] holding an immutable `VectorMap` keyed by id,
  * which keeps registration order (refetch order = registration order) while
  * removing an entry stays O(log n). Registration is a `Scope.acquireRelease`
  * pair: the enclosing `Scope` owns the entry and its release removes exactly
  * that entry, never a neighbour.
  */
final class ActiveQueryRegistry private (
    seq: AtomicLong,
    queries: AtomicRef[VectorMap[Long, ActiveQueryRegistry.Entry]],
    resetHooks: AtomicRef[VectorMap[Long, ActiveQueryRegistry.ResetHook]]
):
    import ActiveQueryRegistry.*

    /** Register a live watcher for the lifetime of the enclosing `Scope`. `target` is
      * read each time the registry is drained and yields what the watcher would
      * refetch now, or `Absent` while it is paused. The `Scope`'s teardown removes
      * exactly this entry.
      */
    def register(target: Maybe[Target] < Sync)(using Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            seq.incrementAndGet.map { id =>
                queries.updateAndGet(_.updated(id, Entry(id, target))).andThen(id)
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

    /** One refetch per query among the live targets named `names`, or among all of them
      * when `names` is empty (react's `refetchQueries()` with no filter), in
      * registration order. Every target of that query runs its `begin`, then the
      * query is fetched, then every target runs its `end`, whether the fetch
      * succeeded or failed. The fetch is sent once when every target follows the
      * cache; a target that does not runs its own fetch, whose write-back reaches the
      * ones that do. Works on one snapshot of the registry.
      */
    private[apollo] def refetchPlan(names: Set[String])(using
        Frame
    ): Chunk[Unit < (Async & Abort[ApolloExecuteFailure])] < Sync =
        live.map { targets =>
            val chosen = targets.filter(t => names.isEmpty || names.contains(t.key.name))
            val byKey  = chosen.foldLeft(VectorMap.empty[QueryKey, Chunk[Target]]) { (acc, t) =>
                acc.updated(t.key, acc.getOrElse(t.key, Chunk.empty) :+ t)
            }
            Chunk.from(byKey.values.map { same =>
                val own     = same.filterNot(_.followsCache)
                val fetches = if own.isEmpty then Chunk(same.head) else own
                Kyo.foreachDiscard(same)(_.begin).andThen(
                    Sync.ensure(Kyo.foreachDiscard(same)(_.end))(Kyo.foreachDiscard(fetches)(_.fetch))
                )
            })
        }

    /** The targets of the entries that are live now, in registration order. */
    private[apollo] def live(using Frame): Chunk[Target] < Sync =
        entries.map(es => Kyo.foreach(es)(_.target).map(_.collect { case Present(t) => t }))

    /** Snapshot of the registered `onResetStore` hooks, in registration order. */
    private[apollo] def resetHookEffects(using Frame): Chunk[Unit < Async] < Sync =
        resetHooks.use(hs => Chunk.from(hs.values.map(_.hook)))

    /** Snapshot of the entries, paused ones included, in registration order. */
    private[apollo] def entries(using Frame): Chunk[Entry] < Sync = queries.use(qs => Chunk.from(qs.values))
end ActiveQueryRegistry

object ActiveQueryRegistry:

    /** What makes two watchers one query: the operation's name, its document (two
      * selections can share a name) and its variables.
      */
    final case class QueryKey(name: String, document: String, variables: Json) derives CanEqual

    /** What a live watcher refetches now.
      *
      * @param key          the query it watches
      * @param fetch        the network fetch of that query. A refetch executes the query,
      *                     so its row is the execute failures.
      * @param begin        run before the fetch of its query, e.g. to show a refetch in progress
      * @param end          run after that fetch, whether it succeeded or failed
      * @param followsCache whether the watcher re-emits on a cache write: then any fetch of
      *                     its query reaches it, and one fetch serves every such watcher. A
      *                     watcher that does not (a lazy query's state) needs its own fetch.
      */
    final case class Target(
        key: QueryKey,
        fetch: Unit < (Async & Abort[ApolloExecuteFailure]),
        begin: Unit < Sync,
        end: Unit < Sync,
        followsCache: Boolean
    )

    final private[apollo] case class Entry(id: Long, target: Maybe[Target] < Sync)

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
