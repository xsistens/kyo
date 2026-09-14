package kyo.apollo.cache.normalized

import kyo.Maybe
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Record

/** A pass-through [[NormalizedCache]] that forwards every call to a wrapped
  * `delegate`, so a persistence backend can be layered on without touching core.
  *
  * This is the seam the Phase 07 task calls a "`SqlNormalizedCache`-style"
  * decorator: a browser IndexedDB / `localStorage` (or any out-of-process) store
  * plugs in by extending this class over an in-memory [[MemoryCache]] and
  * overriding only the operations it wants to persist — typically [[merge]]
  * (write-through to durable storage) and [[loadRecord]] (read-through on a
  * miss) — while inheriting correct forwarding for everything else. Because the
  * [[ApolloStore]] talks only to the `NormalizedCache` contract, dropping such a
  * backend in requires no core change; the dependency stays cache → store, never
  * the reverse. Mirrors apollo-kotlin's `NormalizedCache` chaining (`chain`/
  * `nextCache`), where a `SqlNormalizedCache` wraps a `MemoryCache`.
  *
  * Every method is `override`-able; the defaults are exact delegations, including
  * the two [[merge]] overloads (plain and [[RecordMerger]]-aware), [[loadRecords]]
  * batch loads, and the whole-store [[allRecords]] snapshot garbage collection
  * sweeps over — so a subclass that forgets to override one still behaves like its
  * delegate rather than silently dropping data.
  *
  * @param delegate the wrapped backend calls fall through to
  */
abstract class NormalizedCacheDecorator(protected val delegate: NormalizedCache)
    extends NormalizedCache:

    def loadRecord(key: CacheKey): Maybe[Record] = delegate.loadRecord(key)

    override def loadRecords(keys: Iterable[CacheKey]): Map[CacheKey, Record] =
        delegate.loadRecords(keys)

    override def allRecords(): Map[CacheKey, Record] = delegate.allRecords()

    def merge(records: Iterable[Record], cacheHeaders: CacheHeaders): Set[CacheKey] =
        delegate.merge(records, cacheHeaders)

    override def merge(
        records: Iterable[Record],
        cacheHeaders: CacheHeaders,
        recordMerger: RecordMerger
    ): Set[CacheKey] =
        delegate.merge(records, cacheHeaders, recordMerger)

    def remove(key: CacheKey): Boolean = delegate.remove(key)

    def clearAll(): Unit = delegate.clearAll()
end NormalizedCacheDecorator
