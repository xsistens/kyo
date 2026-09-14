package kyo.apollo.cache.normalized

import kyo.<
import kyo.Absent
import kyo.AllowUnsafe
import kyo.AtomicRef
import kyo.Chunk
import kyo.Frame
import kyo.Loop
import kyo.Maybe
import kyo.Present
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.FieldKey
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.json.Json
import scala.collection.immutable.TreeMap

/** An in-memory [[NormalizedCache]] holding one immutable state behind an
  * [[AtomicRef]], with optional LRU eviction and time-based expiration.
  *
  * This is the default cache: fast, process-local, and lost on restart. Three
  * bounds keep it from growing without limit or serving stale data, all off by
  * default:
  *
  *   - **`maxSize`** — the maximum number of records retained. On overflow the
  *     least-recently-used records are evicted. "Use" means a record a [[read]]
  *     loaded or a write merged, so hot records survive and cold ones are dropped
  *     first.
  *   - **`expireAfterMillis`** — a per-**record** time-to-live. Each written
  *     record is stamped with the epoch-millis it was received (from
  *     [[CacheHeaders.Date]] when present, otherwise the store's clock), and a
  *     record older than the TTL reads back as a miss.
  *   - **`maxAge`** — a per-**field** time-to-live. Each written field is
  *     stamped with its own received date, and a field older than `maxAge` reads
  *     back as absent (a per-field cache miss); a record all of whose fields have
  *     expired reads back as a whole miss. This mirrors apollo-kotlin's field
  *     `date` map, where individual fields expire independently of the record.
  *
  * Concurrency: a [[read]] takes one state and loads every record from it, so a
  * read never observes half of a concurrent write; reads never wait for writers. A
  * write ([[transact]], [[removeExpiredRecords]]) computes the next
  * state from the current one and commits it with a compare-and-set, running again
  * on the newer state if another write committed first. Recency and the physical
  * removal of records a read found expired are recorded after the read in one
  * update against the then-current state, so they never roll back a write that
  * landed during the read.
  *
  * The clock is injectable via `nowMillis` so expiration is deterministic under
  * test. Mirrors apollo-kotlin's `MemoryCache`.
  *
  * @param maxSize           the maximum record count before LRU eviction
  *                          (default unbounded)
  * @param expireAfterMillis a record's time-to-live in millis; negative disables
  *                          per-record expiration (the default)
  * @param maxAge            a field's time-to-live in millis; negative disables
  *                          per-field expiration (the default)
  * @param nowMillis         the clock used for stamping and expiry checks
  */
final class MemoryCache(
    maxSize: Int = Int.MaxValue,
    expireAfterMillis: Long = -1L,
    maxAge: Long = -1L,
    nowMillis: () => Long = () => System.currentTimeMillis()
) extends NormalizedCache:
    import MemoryCache.*

    require(maxSize >= 1, s"maxSize must be >= 1, was $maxSize")

    /** The LRU record cap, or `Absent` when effectively unbounded (`Int.MaxValue`). */
    override def sizeLimit: Maybe[Int] = if maxSize == Int.MaxValue then Absent else Present(maxSize)

    private val state: AtomicRef[State] =
        AtomicRef.Unsafe.init(State.empty)(using AllowUnsafe.embrace.danger).safe

    def read[A](f: RecordLoader => A)(using Frame): A < Sync =
        state.get.map { snapshot =>
            val now     = nowMillis()
            val used    = Chunk.newBuilder[CacheKey]
            val expired = Chunk.newBuilder[(CacheKey, Entry)]
            val loader: RecordLoader = keys =>
                keys.foldLeft(Map.empty[CacheKey, Record]) { (loaded, key) =>
                    snapshot.entries.get(key) match
                        case None => loaded
                        case Some(entry) =>
                            live(entry.record, now) match
                                case Present(record) =>
                                    used += key
                                    loaded.updated(key, record)
                                case Absent =>
                                    expired += (key -> entry)
                                    loaded
                }
            val result      = f(loader)
            val usedKeys    = used.result()
            val expiredSeen = expired.result()
            if usedKeys.isEmpty && expiredSeen.isEmpty then result
            else state.updateAndGet(_.afterRead(usedKeys, expiredSeen)).andThen(result)
        }

    def transact[A](
        f: RecordState => (RecordChanges, A),
        cacheHeaders: CacheHeaders,
        merger: RecordMerger
    )(using Frame): (Set[CacheKey], A) < Sync =
        val doNotStore = cacheHeaders.headerValue(CacheHeaders.DoNotStore).contains("true")
        modify { before =>
            val now          = nowMillis()
            val (changes, a) = f(before.asRecordState(live(_, now)))
            if doNotStore || (changes.merge.isEmpty && changes.remove.isEmpty) then (before, (Set.empty[CacheKey], a))
            else
                val removed = changes.remove.filter(before.entries.contains)
                val date    = writeDate(cacheHeaders, now)
                val (merged, changed) = changes.merge.foldLeft((before.removeAll(removed), removed)) {
                    case ((s, changedKeys), incoming) =>
                        val existing                = Maybe.fromOption(s.entries.get(incoming.key)).map(_.record)
                        val stamped                 = stamp(incoming, existing, date)
                        val (record, changedFields) = merger.merge(existing, stamped)
                        (s.put(record), if changedFields.nonEmpty then changedKeys + incoming.key else changedKeys)
                }
                (merged.evictTo(maxSize), (changed, a))
            end if
        }
    end transact

    def clearAll(using Frame): Unit < Sync = state.set(State.empty)

    def allRecords(using Frame): Map[CacheKey, Record] < Sync =
        state.get.map(_.records)

    /** Proactively drop every record and field that has outlived its TTL, in a
      * single commit, returning the keys of records removed *whole* (either the
      * record itself expired per [[expireAfterMillis]], or every one of its fields
      * expired per [[maxAge]]). Records that merely lose *some* expired fields are
      * trimmed in place and are not reported.
      *
      * The eager counterpart to the lazy expiry a [[read]] applies to the records
      * it loads: call it to reclaim memory (and to give
      * [[ApolloStore.garbageCollect]] a stale-free universe to sweep) without
      * touching every key by hand. A no-op returning the empty set when neither
      * TTL is configured.
      */
    def removeExpiredRecords(using Frame): Set[CacheKey] < Sync =
        if expireAfterMillis < 0 && maxAge < 0 then Set.empty[CacheKey]
        else
            modify { before =>
                val now = nowMillis()
                before.entries.foldLeft((before, Set.empty[CacheKey])) { case ((s, removed), (key, entry)) =>
                    live(entry.record, now) match
                        case Absent                                      => (s.removeAll(Set(key)), removed + key)
                        case Present(trimmed) if trimmed eq entry.record => (s, removed)
                        case Present(trimmed)                            => (s.replace(key, trimmed), removed)
                }
            }

    /** Commit `f`'s next state with a compare-and-set against the state `f` was given,
      * running `f` again on the newer state when another write committed first.
      */
    private def modify[B](f: State => (State, B))(using Frame): B < Sync =
        Loop.foreach {
            state.get.map { before =>
                val (after, result) = f(before)
                if after eq before then Loop.done(result)
                else state.compareAndSet(before, after).map(committed => if committed then Loop.done(result) else Loop.continue)
            }
        }

    /** The epoch-millis to stamp a write with: the [[CacheHeaders.Date]] header
      * when supplied, else `now`.
      */
    private def writeDate(cacheHeaders: CacheHeaders, now: Long): Long =
        cacheHeaders
            .headerValue(CacheHeaders.Date)
            .flatMap(v => Maybe.fromOption(v.toLongOption))
            .getOrElse(now)

    /** Stamp a record with its received date(s) for whichever expirations are
      * enabled: a single record-level date ([[expireAfterMillis]]) and/or a
      * per-field date map ([[maxAge]]). A no-op (leaving metadata untouched) when
      * neither is enabled, so records round-trip exactly.
      *
      * The per-field map merges the incoming fields' fresh dates onto `existing`'s
      * prior field dates, so a field left untouched by this write keeps its
      * original date (and expires on its own original schedule) rather than being
      * refreshed by a merge that never mentioned it.
      */
    private def stamp(record: Record, existing: Maybe[Record], date: Long): Record =
        if expireAfterMillis < 0 && maxAge < 0 then record
        else
            val recordDate =
                if expireAfterMillis >= 0 then Map(DateMetaKey -> Json.JInt(date)) else Map.empty[String, Json]
            val fieldDateMap =
                if maxAge < 0 then Map.empty[String, Json]
                else
                    val incomingDates = record.fields.keysIterator.map(dateSlot(_) -> Json.JInt(date)).toMap
                    val priorDates    = existing.map(fieldDates).getOrElse(Map.empty)
                    Map(FieldDatesMetaKey -> Json.JObj(priorDates ++ incomingDates))
            record.copy(metadata = record.metadata ++ recordDate ++ fieldDateMap)

    /** `record` as a read at `now` sees it: `Absent` when the record has outlived its
      * record-level TTL or every one of its fields has outlived [[maxAge]]; otherwise
      * the record without its expired fields (the very same instance when nothing
      * expired).
      */
    private def live(record: Record, now: Long): Maybe[Record] =
        if isExpired(record, now) then Absent else expireFields(record, now)

    /** Whether `record` has outlived its record-level TTL, per its stamped
      * received date.
      */
    private def isExpired(record: Record, now: Long): Boolean =
        expireAfterMillis >= 0 &&
            record.metadata.get(DateMetaKey).flatMap(Json.integral(_).toOption).exists(date =>
                now - date > expireAfterMillis
            )

    /** The slot a field's received date occupies in the per-field date map. The
      * map is JSON metadata, so it is keyed by the field key's string form — this
      * is the one place a [[FieldKey]] is lowered to that form, and nothing reads a
      * slot back into a key: expiry looks slots up from the record's own keys.
      */
    private def dateSlot(field: FieldKey): String = field.render

    /** The per-field received dates stamped on `record`, or empty if none. */
    private def fieldDates(record: Record): Map[String, Json] =
        record.metadata.get(FieldDatesMetaKey) match
            case Some(Json.JObj(dates)) => dates
            case _                      => Map.empty

    /** Drop `record`'s fields that have outlived [[maxAge]], returning the trimmed
      * record — or `Absent` if every field expired (a whole-record miss). Returns the
      * record unchanged when per-field expiration is disabled or nothing expired.
      */
    private def expireFields(record: Record, now: Long): Maybe[Record] =
        if maxAge < 0 then Present(record)
        else
            val dates = fieldDates(record)
            val expiredFields = record.fields.keySet.filter { field =>
                dates.get(dateSlot(field)).flatMap(Json.integral(_).toOption).exists(date => now - date > maxAge)
            }
            if expiredFields.isEmpty then Present(record)
            else
                val liveFields = record.fields -- expiredFields
                if liveFields.isEmpty then Absent
                else
                    val liveDates = dates -- expiredFields.map(dateSlot)
                    val metadata =
                        if liveDates.isEmpty then record.metadata - FieldDatesMetaKey
                        else record.metadata + (FieldDatesMetaKey -> Json.JObj(liveDates))
                    Present(record.copy(fields = liveFields, metadata = metadata))
                end if
            end if
end MemoryCache

object MemoryCache:
    /** Metadata key under which a record's received epoch-millis is stamped. */
    private[normalized] val DateMetaKey: String = "apollo-memory-cache-date"

    /** Metadata key under which a record's per-field received epoch-millis map
      * (field key → date) is stamped, for per-field [[MemoryCache.maxAge]] expiry.
      */
    private[normalized] val FieldDatesMetaKey: String = "apollo-memory-cache-field-dates"

    /** A stored record and the recency tick of its last use. */
    final private case class Entry(record: Record, lastUsed: Long)

    /** The whole cache as one immutable value: the records by key, the same keys
      * ordered by last use (the head is the LRU eviction candidate), and the next
      * recency tick.
      */
    final private case class State(entries: Map[CacheKey, Entry], recency: TreeMap[Long, CacheKey], tick: Long):

        /** The live records among `keys`, as `live` sees each stored one. */
        def load(keys: Chunk[CacheKey], live: Record => Maybe[Record]): Map[CacheKey, Record] =
            keys.foldLeft(Map.empty[CacheKey, Record]) { (loaded, key) =>
                entries.get(key).flatMap(entry => live(entry.record).toOption) match
                    case Some(record) => loaded.updated(key, record)
                    case None         => loaded
            }

        /** Every stored record, expired ones included. */
        def records: Map[CacheKey, Record] = entries.map((key, entry) => key -> entry.record)

        /** This state as a transaction sees it: loads through `live`, and every record
          * as stored — built only if the transaction asks for it.
          */
        def asRecordState(live: Record => Maybe[Record]): RecordState =
            new RecordState:
                def load(keys: Chunk[CacheKey]): Map[CacheKey, Record] = State.this.load(keys, live)
                lazy val allRecords: Map[CacheKey, Record]             = records

        /** Store `record` under its key as the most recently used entry. */
        def put(record: Record): State =
            val key     = record.key
            val trimmed = entries.get(key).fold(recency)(old => recency - old.lastUsed)
            State(entries.updated(key, Entry(record, tick)), trimmed.updated(tick, key), tick + 1)
        end put

        /** Replace the record under `key` without touching its recency. */
        def replace(key: CacheKey, record: Record): State =
            entries.get(key).fold(this)(entry => copy(entries = entries.updated(key, entry.copy(record = record))))

        def removeAll(keys: Set[CacheKey]): State =
            keys.foldLeft(this) { (s, key) =>
                s.entries.get(key).fold(s)(entry => State(s.entries - key, s.recency - entry.lastUsed, s.tick))
            }

        /** Evict least-recently-used entries until at most `maxSize` remain. */
        def evictTo(maxSize: Int): State =
            if entries.size <= maxSize then this
            else
                val evicted = recency.valuesIterator.take(entries.size - maxSize).toSet
                removeAll(evicted)

        /** Record what a read did, against the state as it is NOW: every key it used
          * becomes most recently used, and every entry it found expired is dropped —
          * unless a write replaced that entry since, in which case the write wins.
          */
        def afterRead(used: Chunk[CacheKey], expired: Chunk[(CacheKey, Entry)]): State =
            val pruned = expired.foldLeft(this) { case (s, (key, seen)) =>
                s.entries.get(key) match
                    case Some(current) if current eq seen => s.removeAll(Set(key))
                    case _                                => s
            }
            used.foldLeft(pruned) { (s, key) =>
                s.entries.get(key).fold(s)(entry => s.put(entry.record))
            }
        end afterRead
    end State

    private object State:
        val empty: State = State(Map.empty, TreeMap.empty, 0L)
end MemoryCache
