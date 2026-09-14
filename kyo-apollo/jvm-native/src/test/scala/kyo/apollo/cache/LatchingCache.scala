package kyo.apollo.cache

import kyo.*
import kyo.apollo.cache.normalized.NormalizedCache
import kyo.apollo.cache.normalized.NormalizedCacheDecorator
import kyo.apollo.cache.normalized.RecordLoader
import kyo.apollo.cache.normalized.RecordMerger
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Record

/** A cache that parks every read and transaction at the batch that loads `gateKey`:
  * it releases `reached` and then waits for `gate` before handing the batch to the
  * delegate's loader. A test awaits `reached` to know a store operation is suspended
  * in the middle of its load, runs a competing operation on its own fiber, and
  * releases `gate` — an interleaving forced by barriers, not by timing.
  *
  * The loader a backend hands out is a plain function, so the wait blocks the
  * loading thread (JVM/Native only). Once `gate` is released every later load passes
  * straight through, which is what lets a transaction that has to run again after a
  * lost compare-and-set finish.
  */
final class LatchingCache(delegate: NormalizedCache, gateKey: CacheKey, gate: Latch, reached: Latch)
    extends NormalizedCacheDecorator(delegate):

    private def gated(loader: RecordLoader)(using Frame): RecordLoader =
        keys =>
            if keys.contains(gateKey) then
                given AllowUnsafe = AllowUnsafe.embrace.danger
                reached.unsafe.release()
                discard(gate.unsafe.await().block(new Clock.Deadline.Unsafe(Absent, Clock.live.unsafe)))
            end if
            loader.load(keys)

    override def read[A](f: RecordLoader => A)(using Frame): A < Sync =
        delegate.read(loader => f(gated(loader)))

    override def transact[A](
        f: RecordLoader => (Chunk[Record], A),
        cacheHeaders: CacheHeaders,
        merger: RecordMerger
    )(using Frame): (Set[CacheKey], A) < Sync =
        delegate.transact(loader => f(gated(loader)), cacheHeaders, merger)
end LatchingCache
