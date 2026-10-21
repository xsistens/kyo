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
  * An entry takes part only while it is ACTIVE, read each time the registry is
  * drained: a watcher paused by a `skip`, or a lazy query not loaded yet, is left
  * out, as in react-apollo, where a standby query is. A paused watcher that a drain
  * leaves out is told so (its target's `missed`), so it can catch up when it
  * resumes instead of showing what it held before. Entries whose targets share a
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

    /** Register a live watcher for the lifetime of the enclosing `Scope`. `active` is
      * read each time the registry is drained: while it is `false` the watcher is
      * paused and takes no part. The `Scope`'s teardown removes exactly this entry.
      */
    def register(target: Target, active: Boolean < Sync)(using Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            seq.incrementAndGet.map { id =>
                queries.updateAndGet(_.updated(id, Entry(id, target, active))).andThen(id)
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
      * ones that do. A paused entry the names would have chosen runs its target's
      * `missed` now, as the plan is made. Works on one snapshot of the registry.
      */
    private[apollo] def refetchPlan(names: Set[String])(using
        Frame
    ): Chunk[Unit < (Async & Abort[ApolloExecuteFailure])] < Sync =
        entries.map { es =>
            Kyo.foreach(es.filter(e => names.isEmpty || names.contains(e.target.key.name)))(e =>
                e.active.map(on => (e.target, on))
            )
        }.map { named =>
            Kyo.foreachDiscard(named.collect { case (t, false) => t })(_.missed).andThen(named.collect { case (t, true) => t })
        }.map { chosen =>
            val byKey = chosen.foldLeft(VectorMap.empty[QueryKey, Chunk[Target]]) { (acc, t) =>
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

    /** The targets of the entries that are active now, in registration order. */
    private[apollo] def live(using Frame): Chunk[Target] < Sync =
        entries.map(es => Kyo.foreach(es)(e => e.active.map(on => (e.target, on))).map(_.collect { case (t, true) => t }))

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
      * @param missed       run when a refetch or reset leaves the watcher out because it is
      *                     paused, so it can fetch again when it resumes
      */
    final case class Target(
        key: QueryKey,
        fetch: Unit < (Async & Abort[ApolloExecuteFailure]),
        begin: Unit < Sync,
        end: Unit < Sync,
        followsCache: Boolean,
        missed: Unit < Sync
    )

    final private[apollo] case class Entry(id: Long, target: Target, active: Boolean < Sync)

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
