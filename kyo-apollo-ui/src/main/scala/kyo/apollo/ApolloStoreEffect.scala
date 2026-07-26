package kyo.apollo

import kyo.*
import kyo.apollo.api.Operation
import kyo.apollo.cache.normalized.ApolloStore
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.exception.CacheMissException

/** The **effect view** of the normalized [[kyo.apollo.cache.normalized.ApolloStore]].
  *
  * `core`'s `ApolloStore` is a synchronous, imperative data structure: reads
  * return bare values and *throw* [[kyo.apollo.exception.CacheMissException]] on a
  * miss, writes return a bare `Set[String]`. That shape is deliberate — the cache
  * interceptor drives it in tight synchronous loops on the hot read path and must
  * stay Kyo-free (the same `core`-stays-Kyo-free boundary the `.data`/`.response`
  * extensions honor).
  *
  * App code, however, touches the store from effect context (`client.apolloStore`
  * inside a `for` over `Async`). These extensions are the module boundary that
  * gives it Kyo shapes: reads suspend in `Sync` and surface a miss on
  * `Abort[CacheMissException]` (via [[kyo.Abort.catching]]) instead of a throw;
  * writes/updates/invalidation suspend in `Sync`. The `read`/`write`/`update`
  * pairs are overloaded for whole operations ([[kyo.apollo.api.Operation]]) and for
  * single fragments ([[kyo.apollo.cache.normalized.api.Fragment]] + [[CacheKey]]).
  *
  * Names deliberately differ from the underlying members (`read` vs
  * `readOperation`, `evictKey` vs `evict`, …) so the effectful extension is never
  * shadowed by the synchronous member it wraps.
  */
extension (store: ApolloStore)

    /** Reassemble `operation`'s typed data from the cache, surfacing a
      * [[CacheMissException]] on `Abort` instead of throwing.
      */
    def read[D](operation: Operation[D])(using Frame): D < (Sync & Abort[CacheMissException]) =
        Abort.catching[CacheMissException](store.readOperation(operation))

    /** Reassemble `fragment`'s typed data rooted at `cacheKey`, surfacing a
      * [[CacheMissException]] on `Abort` instead of throwing.
      */
    def read[D](fragment: Fragment[D], cacheKey: CacheKey)(using
        Frame
    ): D < (Sync & Abort[CacheMissException]) =
        Abort.catching[CacheMissException](store.readFragment(fragment, cacheKey))

    /** Normalize and merge `operation`'s `data` into the cache, yielding the changed
      * record keys (which re-emit every dependent watcher).
      */
    def write[D](
        operation: Operation[D],
        data: D,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    )(using Frame): Set[String] < Sync =
        Sync.defer(store.writeOperation(operation, data, cacheHeaders))

    /** Normalize and merge `fragment`'s `data` into the record at `cacheKey`,
      * yielding the changed record keys.
      */
    def write[D](
        fragment: Fragment[D],
        cacheKey: CacheKey,
        data: D,
        cacheHeaders: CacheHeaders
    )(using Frame): Set[String] < Sync =
        Sync.defer(store.writeFragment(fragment, cacheKey, data, cacheHeaders))

    /** Read `operation`'s cached data, apply `f`, and write it back (a cache miss is
      * a no-op yielding the empty set). Yields the changed record keys.
      */
    def update[D](operation: Operation[D])(f: D => D)(using Frame): Set[String] < Sync =
        Sync.defer(store.updateOperation(operation)(f))

    /** Read the fragment at `cacheKey`, apply `f`, and write it back (a cache miss is
      * a no-op). Yields the changed record keys.
      */
    def update[D](fragment: Fragment[D], cacheKey: CacheKey)(f: D => D)(using
        Frame
    ): Set[String] < Sync =
        Sync.defer(store.updateFragment(fragment, cacheKey)(f))

    /** Evict the record at `cacheKey` (optionally cascading through its references),
      * yielding the removed keys (which re-emit dependent watchers).
      */
    def evictKey(cacheKey: CacheKey, cascade: Boolean = false)(using Frame): Set[String] < Sync =
        Sync.defer(store.evict(cacheKey, cascade))

    /** Reclaim every record unreachable from a root, yielding the removed keys. */
    def collectGarbage(using Frame): Set[String] < Sync =
        Sync.defer(store.garbageCollect())

    /** Drop every record from the backing cache. */
    def clear(using Frame): Unit < Sync =
        Sync.defer(store.clearAll())
end extension
