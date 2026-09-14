package kyo.apollo.cache.normalized

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.json.Json
import kyo.discard
import scala.collection.mutable

/** An in-memory [[NormalizedCache]] backed by a single map, with optional
  * LRU eviction and time-based expiration.
  *
  * This is the default cache: fast, process-local, and lost on restart. Three
  * bounds keep it from growing without limit or serving stale data, all off by
  * default:
  *
  *   - **`maxSize`** — the maximum number of records retained. On overflow the
  *     least-recently-used records are evicted. "Use" means a successful
  *     [[loadRecord]] or a [[merge]] that writes the record, so hot records
  *     survive and cold ones are dropped first.
  *   - **`expireAfterMillis`** — a per-**record** time-to-live. Each written
  *     record is stamped with the epoch-millis it was received (from
  *     [[CacheHeaders.Date]] when present, otherwise the store's clock), and a
  *     record older than the TTL reads back as a miss and is dropped.
  *   - **`maxAge`** — a per-**field** time-to-live. Each written field is
  *     stamped with its own received date, and a field older than `maxAge` reads
  *     back as absent (a per-field cache miss); a record all of whose fields have
  *     expired reads back as a whole miss. This mirrors apollo-kotlin's field
  *     `date` map, where individual fields expire independently of the record.
  *
  * The clock is injectable via `nowMillis` so expiration is deterministic under
  * test. All mutating operations synchronize on the instance, so a single cache
  * is safe to share across threads on the JVM (a no-op cost on Scala.js).
  * Mirrors apollo-kotlin's `MemoryCache`.
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

    require(maxSize >= 1, s"maxSize must be >= 1, was $maxSize")

    /** The LRU record cap, or `Absent` when effectively unbounded (`Int.MaxValue`). */
    override def sizeLimit: Maybe[Int] = if maxSize == Int.MaxValue then Absent else Present(maxSize)

    /** Records keyed by [[Record.key]], in least- to most-recently-used order:
      * the head is the LRU eviction candidate, the tail the freshest. Order is
      * maintained by re-inserting a key on each use.
      */
    private val entries = mutable.LinkedHashMap.empty[String, Record]

    def loadRecord(key: String): Maybe[Record] = synchronized {
        entries.get(key) match
            case Some(record) if isExpired(record) =>
                discard(entries.remove(key))
                Absent
            case Some(record) =>
                expireFields(record) match
                    case Absent =>
                        discard(entries.remove(key))
                        Absent
                    case Present(live) =>
                        touch(key, live)
                        Present(live)
            case None => Absent
    }

    def merge(records: Iterable[Record], cacheHeaders: CacheHeaders): Set[String] =
        merge(records, cacheHeaders, RecordMerger.default)

    override def merge(
        records: Iterable[Record],
        cacheHeaders: CacheHeaders,
        recordMerger: RecordMerger
    ): Set[String] = synchronized {
        if cacheHeaders.headerValue(CacheHeaders.DoNotStore).contains("true") then Set.empty
        else
            val changedKeys = records.foldLeft(Set.empty[String]) { (changed, incoming) =>
                val existing                = Maybe.fromOption(entries.get(incoming.key))
                val stamped                 = stamp(incoming, existing, cacheHeaders)
                val (merged, changedFields) = recordMerger.merge(existing, stamped)
                touch(incoming.key, merged)
                if changedFields.nonEmpty then changed + incoming.key else changed
            }
            evictIfNeeded()
            changedKeys
    }

    def remove(key: String): Boolean = synchronized {
        entries.remove(key).isDefined
    }

    def clearAll(): Unit = synchronized {
        entries.clear()
    }

    def allRecords(): Map[String, Record] = synchronized {
        entries.toMap
    }

    /** Proactively drop every record and field that has outlived its TTL, in a
      * single sweep, returning the keys of records removed *whole* (either the
      * record itself expired per [[expireAfterMillis]], or every one of its fields
      * expired per [[maxAge]]). Records that merely lose *some* expired fields are
      * trimmed in place and are not reported.
      *
      * The eager counterpart to the lazy expiry that [[loadRecord]] performs on a
      * per-key basis: call it to reclaim memory (and to give
      * [[ApolloStore.garbageCollect]] a stale-free universe to sweep) without
      * touching every key by hand. A no-op returning the empty set when neither
      * TTL is configured.
      */
    def removeExpiredRecords(): Set[String] = synchronized {
        if expireAfterMillis < 0 && maxAge < 0 then Set.empty
        else
            var removed = Set.empty[String]
            entries.keys.toList.foreach { key =>
                entries.get(key).foreach { record =>
                    if isExpired(record) then
                        discard(entries.remove(key))
                        removed += key
                    else
                        expireFields(record) match
                            case Absent =>
                                discard(entries.remove(key))
                                removed += key
                            case Present(trimmed) =>
                                entries.update(key, trimmed)
                }
            }
            removed
    }

    /** Move `key` to the most-recently-used end by re-inserting it. */
    private def touch(key: String, record: Record): Unit =
        discard(entries.remove(key))
        discard(entries.put(key, record))

    /** Evict least-recently-used records until the store is within `maxSize`. */
    private def evictIfNeeded(): Unit =
        while entries.size > maxSize && entries.nonEmpty do discard(entries.remove(entries.head._1))

    /** The epoch-millis to stamp a write with: the [[CacheHeaders.Date]] header
      * when supplied, else the store's clock.
      */
    private def writeDate(cacheHeaders: CacheHeaders): Long =
        cacheHeaders
            .headerValue(CacheHeaders.Date)
            .flatMap(v => Maybe.fromOption(v.toLongOption))
            .getOrElse(nowMillis())

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
    private def stamp(record: Record, existing: Maybe[Record], cacheHeaders: CacheHeaders): Record =
        if expireAfterMillis < 0 && maxAge < 0 then record
        else
            val date     = writeDate(cacheHeaders)
            var metadata = record.metadata
            if expireAfterMillis >= 0 then
                metadata = metadata + (MemoryCache.DateMetaKey -> Json.JNum(date.toDouble))
            if maxAge >= 0 then
                val incomingDates = record.fields.keysIterator.map(_ -> Json.JNum(date.toDouble)).toMap
                val priorDates    = existing.map(fieldDates).getOrElse(Map.empty)
                metadata =
                    metadata + (MemoryCache.FieldDatesMetaKey -> Json.JObj(priorDates ++ incomingDates))
            end if
            record.copy(metadata = metadata)

    /** Whether `record` has outlived its record-level TTL, per its stamped
      * received date.
      */
    private def isExpired(record: Record): Boolean =
        expireAfterMillis >= 0 && (record.metadata.get(MemoryCache.DateMetaKey) match
            case Some(Json.JNum(date)) => nowMillis() - date.toLong > expireAfterMillis
            case _                     => false)

    /** The per-field received dates stamped on `record`, or empty if none. */
    private def fieldDates(record: Record): Map[String, Json] =
        record.metadata.get(MemoryCache.FieldDatesMetaKey) match
            case Some(Json.JObj(dates)) => dates
            case _                      => Map.empty

    /** Drop `record`'s fields that have outlived [[maxAge]], returning the trimmed
      * record — or `None` if every field expired (a whole-record miss). Returns the
      * record unchanged when per-field expiration is disabled or nothing expired.
      */
    private def expireFields(record: Record): Maybe[Record] =
        if maxAge < 0 then Present(record)
        else
            val dates = fieldDates(record)
            val now   = nowMillis()
            val expiredFields = record.fields.keySet.filter { field =>
                dates.get(field) match
                    case Some(Json.JNum(date)) => now - date.toLong > maxAge
                    case _                     => false
            }
            if expiredFields.isEmpty then Present(record)
            else
                val liveFields = record.fields -- expiredFields
                if liveFields.isEmpty then Absent
                else
                    val liveDates = dates -- expiredFields
                    val metadata =
                        if liveDates.isEmpty then record.metadata - MemoryCache.FieldDatesMetaKey
                        else record.metadata + (MemoryCache.FieldDatesMetaKey -> Json.JObj(liveDates))
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
end MemoryCache
