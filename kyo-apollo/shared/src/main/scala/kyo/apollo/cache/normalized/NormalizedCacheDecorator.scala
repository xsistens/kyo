package kyo.apollo.cache.normalized

import kyo.<
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Record

/** A pass-through [[NormalizedCache]] that forwards every call to a wrapped
  * `delegate`, so a persistence backend can be layered on without touching core.
  *
  * A browser IndexedDB / `localStorage` (or any out-of-process) store plugs in by
  * extending this class over an in-memory [[MemoryCache]] and overriding only the
  * primitives it wants to intercept — typically [[transact]] (write-through to
  * durable storage) and [[read]] (read-through on a miss, by wrapping the
  * [[RecordLoader]] it hands on) — while inheriting correct forwarding for the rest.
  * Because [[NormalizedCache.loadRecord]], [[NormalizedCache.loadRecords]] and
  * [[NormalizedCache.merge]] are expressed through [[read]] and [[transact]], an
  * override of those two reaches every read and write the [[ApolloStore]] makes.
  * Mirrors apollo-kotlin's `NormalizedCache` chaining (`chain`/`nextCache`), where a
  * `SqlNormalizedCache` wraps a `MemoryCache`.
  *
  * @param delegate the wrapped backend calls fall through to
  */
abstract class NormalizedCacheDecorator(protected val delegate: NormalizedCache)
    extends NormalizedCache:

    def read[A](f: RecordLoader => A)(using Frame): A < Sync = delegate.read(f)

    def transact[A](
        f: RecordLoader => (Chunk[Record], A),
        cacheHeaders: CacheHeaders,
        merger: RecordMerger
    )(using Frame): (Set[CacheKey], A) < Sync =
        delegate.transact(f, cacheHeaders, merger)

    def remove(keys: Chunk[CacheKey])(using Frame): Set[CacheKey] < Sync = delegate.remove(keys)

    def clearAll(using Frame): Unit < Sync = delegate.clearAll

    def allRecords(using Frame): Map[CacheKey, Record] < Sync = delegate.allRecords

    override def sizeLimit: Maybe[Int] = delegate.sizeLimit
end NormalizedCacheDecorator
