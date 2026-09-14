package kyo.apollo.cache.normalized

import kyo.<
import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.FieldKey
import kyo.apollo.cache.normalized.api.Record

/** A pluggable backend that stores normalized [[Record]]s keyed by cache key.
  *
  * This is the storage seam the store writes records into and reads them back out
  * of: [[kyo.apollo.cache.normalized.internal.Normalizer]] produces records, the
  * backend merges and holds them, and
  * [[kyo.apollo.cache.normalized.internal.CacheBatchReader]] resolves them through a
  * [[RecordLoader]]. Concrete backends (an in-memory map today, a persistent store
  * later) implement the same contract, so the [[ApolloStore]] coordinator is
  * agnostic to where records live. Mirrors apollo-kotlin's `NormalizedCache`.
  *
  * The contract is two primitives, not a set of single-record calls whose
  * consistency is left implicit:
  *
  *   - [[read]] hands its function ONE loader over ONE consistent state of the
  *     store. Every record a read loads — however many batches it takes — comes
  *     from that state, so a read sees the store either before or after a
  *     concurrent write, never a mixture of both.
  *   - [[transact]] is an atomic read-modify-write: its function loads what it
  *     needs, returns the records to merge, and the backend commits them against
  *     exactly the state that function saw. Under contention the function is run
  *     again on the newer state, so it must be pure.
  *
  * [[loadRecord]], [[loadRecords]] and [[merge]] are conveniences expressed through
  * those two, so a decorating backend overrides [[read]] and [[transact]] and
  * reaches every read and write the store makes.
  *
  * Records are merged, never blindly replaced: a commit unions an incoming record's
  * fields onto whatever is already stored (through the [[RecordMerger]] the store
  * hands in) and reports which keys actually changed, so a re-fetch that returns
  * identical data touches nothing.
  */
trait NormalizedCache:

    /** Run `f` against one loader over one consistent state of the store.
      *
      * `load` is the batch seam a persistent backend needs: a reader asks for all
      * the keys of one level of a selection tree at once. A record that is absent
      * (or, for a time-based backend, expired) is simply missing from the returned
      * map. `f` runs once; anything it throws propagates out of the effect.
      */
    def read[A](f: RecordLoader => A)(using Frame): A < Sync

    /** Atomically load, compute and merge: `f` receives a loader over the current
      * state and returns the records to merge plus a result of its own; the records
      * are merged through `merger` onto exactly that state, and the keys whose
      * stored value changed are returned together with `f`'s result.
      *
      * `f` must be PURE: when another write commits first, the backend runs it again
      * on the newer state. `cacheHeaders` carries write hints such as
      * [[CacheHeaders.Date]] (expiry stamp) or [[CacheHeaders.DoNotStore]] (commit
      * nothing).
      */
    def transact[A](
        f: RecordLoader => (Chunk[Record], A),
        cacheHeaders: CacheHeaders,
        merger: RecordMerger
    )(using Frame): (Set[CacheKey], A) < Sync

    /** Remove the records stored under `keys`, returning the keys that were present
      * and are now gone. Absent keys are skipped.
      */
    def remove(keys: Chunk[CacheKey])(using Frame): Set[CacheKey] < Sync

    /** Drop every record from the store. */
    def clearAll(using Frame): Unit < Sync

    /** Every record currently held, keyed by [[Record.key]] — the whole-store
      * snapshot garbage collection and the devtools dump sweep over, taken without
      * perturbing per-record bookkeeping (an LRU backend's recency, a time-based
      * backend's expiry).
      */
    def allRecords(using Frame): Map[CacheKey, Record] < Sync

    /** The maximum number of records this backend retains before eviction, when it
      * is bounded — `Absent` for an unbounded store. Surfaced for diagnostics (e.g. the
      * devtools memory view); backends with a size cap override this.
      */
    def sizeLimit: Maybe[Int] = Absent

    /** The record stored under `key`, or `Absent` — one [[read]] of one key. */
    final def loadRecord(key: CacheKey)(using Frame): Maybe[Record] < Sync =
        read(loader => Maybe.fromOption(loader.load(Chunk(key)).get(key)))

    /** The records present among `keys`, keyed by cache key — one [[read]] of one
      * batch. Absent keys are omitted.
      */
    final def loadRecords(keys: Chunk[CacheKey])(using Frame): Map[CacheKey, Record] < Sync =
        read(_.load(keys))

    /** Merge `records` into the store and return the keys whose stored value
      * changed (new records always count; a re-write of identical fields counts for
      * nothing) — a [[transact]] that loads nothing.
      */
    final def merge(
        records: Chunk[Record],
        cacheHeaders: CacheHeaders = CacheHeaders.None,
        merger: RecordMerger = RecordMerger.default
    )(using Frame): Set[CacheKey] < Sync =
        transact(_ => (records, ()), cacheHeaders, merger).map(_._1)
end NormalizedCache

/** A batch lookup over one consistent state of a [[NormalizedCache]] — what
  * [[NormalizedCache.read]] and [[NormalizedCache.transact]] hand their function.
  * Pure over that state: loading the same keys twice returns the same records.
  */
trait RecordLoader:
    /** The records present among `keys`, keyed by cache key; absent keys are
      * omitted.
      */
    def load(keys: Chunk[CacheKey]): Map[CacheKey, Record]
end RecordLoader

object RecordLoader:
    /** A loader over a fixed map of records. */
    def apply(records: Map[CacheKey, Record]): RecordLoader =
        keys => keys.iterator.flatMap(key => records.get(key).map(key -> _)).toMap
end RecordLoader

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
    def mergeRecords(existing: Maybe[Record], incoming: Record): (Record, Set[FieldKey]) =
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
