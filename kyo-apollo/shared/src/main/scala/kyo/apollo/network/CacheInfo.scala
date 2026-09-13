package kyo.apollo.network

import kyo.Maybe
import kyo.Present
import kyo.apollo.exception.CacheMissException

/** Per-response metadata describing how the normalized cache participated in
  * producing an [[ApolloResponse]].
  *
  * The cache interceptor (Phase 04 Task 6) stamps this onto every response it
  * emits so a caller can tell a cache-served value from a networked one, and a
  * cache hit from a miss — without inspecting `data`/`exception` heuristically.
  * It lives in `kyo.apollo.network` alongside [[ApolloResponse]] (rather than in the
  * cache package) so the response model keeps depending only on `network` and
  * the leaf `exception` package, never on the cache layer: the dependency runs
  * cache → network, not the reverse. Mirrors apollo-kotlin's `CacheInfo`, pared
  * down to the flags this phase needs.
  *
  * @param fromCache           whether this response was served from the cache
  *                            (as opposed to the network)
  * @param isCacheHit          whether a cache read satisfied the operation
  * @param cacheMissException  the miss that occurred, when a cache read failed
  * @param dependentKeys       the record keys this response is tied to in the
  *                            store: for a cache hit, the keys the read touched
  *                            (root plus every reference/redirect target); for
  *                            a networked response, the keys its write-back
  *                            changed. A Phase 05 watcher intersects these
  *                            against a write's changed keys to decide whether
  *                            to re-emit; empty for a miss.
  * @param generation          for a cache hit, the store generation the read was
  *                            current at (sampled before the records were
  *                            loaded); `0` when no store read stands behind this
  *                            response (a networked response, a miss). A watcher
  *                            adopts `dependentKeys` together with it and re-reads
  *                            if the store has already moved past it.
  */
final case class CacheInfo(
    fromCache: Boolean,
    isCacheHit: Boolean,
    cacheMissException: Maybe[CacheMissException] = Maybe.empty,
    dependentKeys: Set[String] = Set.empty,
    generation: Long = 0L
)

object CacheInfo:
    /** A response assembled from the cache that satisfied the whole operation. */
    val hit: CacheInfo = CacheInfo(fromCache = true, isCacheHit = true)

    /** A cache hit carrying the `dependentKeys` the read touched (see
      * [[CacheInfo.dependentKeys]]) and the store `generation` it was read at
      * (see [[CacheInfo.generation]]).
      */
    def hit(dependentKeys: Set[String], generation: Long = 0L): CacheInfo =
        CacheInfo(fromCache = true, isCacheHit = true, dependentKeys = dependentKeys, generation = generation)

    /** A response served from the network (a cache write-back may have run). */
    val network: CacheInfo = CacheInfo(fromCache = false, isCacheHit = false)

    /** A network response whose write-back changed `dependentKeys` — the record
      * keys the response materialized in the store. A watcher uses them as its
      * fallback watch set when the post-write re-read cannot be satisfied.
      */
    def network(dependentKeys: Set[String]): CacheInfo =
        CacheInfo(fromCache = false, isCacheHit = false, dependentKeys = dependentKeys)

    /** A response representing a cache read that missed, carrying the `miss`. */
    def miss(miss: CacheMissException): CacheInfo =
        CacheInfo(fromCache = true, isCacheHit = false, cacheMissException = Present(miss))
end CacheInfo
