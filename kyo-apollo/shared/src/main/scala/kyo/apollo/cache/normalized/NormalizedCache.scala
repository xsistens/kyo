package kyo.apollo.cache.normalized

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.Record

/** A pluggable backend that stores normalized [[Record]]s keyed by cache key.
  *
  * This is the storage seam Phase 04 writes records into and reads them back
  * out of: [[kyo.apollo.cache.normalized.internal.Normalizer]] produces records, the
  * store merges and holds them, and
  * [[kyo.apollo.cache.normalized.internal.CacheBatchReader]] resolves them by key via
  * [[loadRecord]]. Concrete backends (an in-memory map today, a persistent store
  * later) implement the same contract, so the [[ApolloStore]] coordinator is
  * agnostic to where records live. Mirrors apollo-kotlin's `NormalizedCache`.
  *
  * Records are merged, never blindly replaced: [[merge]] unions an incoming
  * record's fields onto whatever is already stored and reports which keys
  * actually changed, so a re-fetch that returns identical data touches nothing.
  */
trait NormalizedCache:

    /** The record stored under `key`, or `Absent` if absent (or, for a time-based
      * store, expired).
      */
    def loadRecord(key: String): Maybe[Record]

    /** The records present among `keys`, keyed by cache key. Absent keys are
      * simply omitted. Overridable for stores that can batch the lookup.
      */
    def loadRecords(keys: Iterable[String]): Map[String, Record] =
        keys.iterator.flatMap(key => loadRecord(key).map(key -> _).toOption).toMap

    /** Every record currently held, keyed by [[Record.key]] — the whole-store
      * snapshot garbage collection sweeps over.
      *
      * [[ApolloStore.garbageCollect]] needs the full universe of stored keys to
      * compute the *complement* of the reachable set (unreachable = all − reachable)
      * without perturbing per-record bookkeeping (an LRU backend's recency, a
      * time-based backend's expiry). Returning a materialised snapshot lets the
      * sweep mark reachability against a stable view and remove the rest. Backends
      * that cannot cheaply enumerate (a remote store) may override to page, but the
      * in-memory and decorator backends return their map directly.
      */
    def allRecords(): Map[String, Record]

    /** Merge `records` into the store and return the set of record keys whose
      * stored value changed (new records always count; a re-write of identical
      * fields counts for nothing). `cacheHeaders` may carry write hints such as
      * [[CacheHeaders.Date]] (expiry stamp) or [[CacheHeaders.DoNotStore]].
      *
      * The returned changed-key set is what [[ApolloStore.publish]] hands to
      * watchers (Phase 05) so only affected reads re-run.
      */
    def merge(records: Iterable[Record], cacheHeaders: CacheHeaders = CacheHeaders.None): Set[String]

    /** Merge `records` using an explicit [[RecordMerger]] — the policy-aware write
      * path the [[ApolloStore]] uses so a per-field merge (e.g. connection edge
      * unioning) is honoured. The default implementation ignores `recordMerger` and
      * falls back to the plain [[merge]] above, so backends that predate field
      * policies keep working; a backend that supports custom merging (see
      * [[MemoryCache]]) overrides this to route through `recordMerger`.
      */
    def merge(
        records: Iterable[Record],
        cacheHeaders: CacheHeaders,
        @annotation.unused recordMerger: RecordMerger
    ): Set[String] =
        merge(records, cacheHeaders)

    /** Remove the record stored under `key`. Returns `true` if a record was
      * present and removed, `false` if there was nothing to remove.
      */
    def remove(key: String): Boolean

    /** Drop every record from the store. */
    def clearAll(): Unit

    /** The maximum number of records this backend retains before eviction, when it
      * is bounded — `None` for an unbounded store. Surfaced for diagnostics (e.g. the
      * devtools memory view); backends with a size cap override this.
      */
    def sizeLimit: Option[Int] = None
end NormalizedCache

object NormalizedCache:

    /** Merge `incoming` onto an optional `existing` record of the same key,
      * returning the merged record and the set of field keys whose value changed.
      *
      * This is the default record-merge policy shared by every backend: fields
      * union with the incoming value winning on conflict, metadata likewise. A
      * brand-new record reports all of its own fields as changed; a re-merge of
      * identical fields reports none. Field values are compared structurally, so
      * an unchanged re-fetch produces an empty changed set even though a fresh
      * expiry stamp may still be written into metadata. Mirrors apollo-kotlin's
      * `RecordMerger` default (`DefaultRecordMerger`).
      *
      * @param existing the record currently stored under this key, if any
      * @param incoming the record being written
      * @return the merged record, and the field keys that changed
      */
    def mergeRecords(existing: Maybe[Record], incoming: Record): (Record, Set[String]) =
        existing match
            case Absent => (incoming, incoming.fieldKeys)
            case Present(old) =>
                val changedFields = incoming.fields.iterator.collect {
                    case (fieldKey, value) if !old.fields.get(fieldKey).contains(value) => fieldKey
                }.toSet
                val merged = Record(
                    incoming.key,
                    old.fields ++ incoming.fields,
                    old.metadata ++ incoming.metadata
                )
                (merged, changedFields)
end NormalizedCache
