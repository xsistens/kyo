package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit tests for the Phase 04 store backends: the [[NormalizedCache]] merge
  * policy, the [[MemoryCache]] backend (LRU + expiration), and the
  * [[ApolloStore]] coordinator's `writeOperation` → `readOperation` round-trip
  * and `publish` hook.
  */
class StoreSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def jstr(s: String): Json = Json.JStr(s)

    private def rec(key: CacheKey, fields: (FieldKey, RecordValue)*): Record =
        Record(key, fields.toMap)

    /** Bare-typename record keys for the backend-level cases, where the key's shape is irrelevant. */
    private def key(name: String): CacheKey = CacheKey(name, "")

    private def scalar(s: String): RecordValue = RecordValue.Scalar(jstr(s))

    // --- ApolloStore: typed round-trip + publish ------------------------------

    final case class Country(__typename: String, code: String, name: String) derives Schema
    final case class CountriesData(countries: List[Country]) derives Schema

    /** A query for `{ countries { __typename code name } }`. */
    final case class CountriesQuery() extends Query[CountriesData]:
        def name                              = "Countries"
        def document                          = "query Countries { countries { __typename code name } }"
        def dataSchema: Schema[CountriesData] = summon[Schema[CountriesData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(
                    CompiledField(
                        "countries",
                        CompiledListType(CompiledNamedType("Country")),
                        selections = Chunk(
                            CompiledField("__typename", CompiledNamedType("String")),
                            CompiledField("code", CompiledNamedType("String")),
                            CompiledField("name", CompiledNamedType("String"))
                        )
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CountriesQuery

    private def store(): ApolloStore =
        new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("code")))

    private val sampleData = CountriesData(
        List(Country("Country", "DE", "Germany"), Country("Country", "FR", "France"))
    )

    "NormalizedCache.mergeRecords / MemoryCache / ApolloStore" - {

        // --- NormalizedCache.mergeRecords -----------------------------------------

        "mergeRecords on an absent existing record reports all fields changed" in {
            val incoming          = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"), fk("title") -> scalar("Dune"))
            val (merged, changed) = NormalizedCache.mergeRecords(Absent, incoming)
            assert(merged == incoming)
            assert(changed == Set("id", "title"))
        }

        "mergeRecords unions fields, later value winning, reporting only the delta" in {
            val existing          = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"), fk("title") -> scalar("Old"))
            val incoming          = rec(CacheKey("Book", "1"), fk("title") -> scalar("New"), fk("author") -> scalar("A"))
            val (merged, changed) = NormalizedCache.mergeRecords(Present(existing), incoming)
            assert(
                merged.fields == Map(fk("id") -> scalar("1"), fk("title") -> scalar("New"), fk("author") -> scalar("A"))
            )
            assert(changed == Set("title", "author"))
        }

        "mergeRecords reports nothing changed when identical fields are re-written" in {
            val existing = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"))
            val (_, changed) =
                NormalizedCache.mergeRecords(Present(existing), rec(CacheKey("Book", "1"), fk("id") -> scalar("1")))
            assert(changed == Set.empty[CacheKey])
        }

        // --- MemoryCache: basic merge / load / remove / clear ---------------------

        "merge stores a record and reports its key as changed; loadRecord returns it" in {
            val cache  = MemoryCache()
            val record = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"))
            assert(cache.merge(List(record)) == Set(CacheKey("Book", "1")))
            assert(cache.loadRecord(CacheKey("Book", "1")) == Present(record))
        }

        "re-merging identical data reports no changed keys" in {
            val cache  = MemoryCache()
            val record = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"))
            cache.merge(List(record))
            assert(cache.merge(List(record)) == Set.empty[CacheKey])
        }

        "loadRecords returns only the present subset" in {
            val cache = MemoryCache()
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1")), rec(key("B"), fk("y") -> scalar("2"))))
            assert(cache.loadRecords(List(key("A"), key("missing"))).keySet == Set(key("A")))
        }

        "remove deletes a present record and reports absence otherwise" in {
            val cache = MemoryCache()
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1"))))
            assert(cache.remove(key("A")) == true)
            assert(cache.remove(key("A")) == false)
            assert(cache.loadRecord(key("A")) == Absent)
        }

        "clearAll empties the store" in {
            val cache = MemoryCache()
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1")), rec(key("B"), fk("y") -> scalar("2"))))
            cache.clearAll()
            assert(cache.loadRecord(key("A")) == Absent)
            assert(cache.loadRecord(key("B")) == Absent)
        }

        "DoNotStore header makes merge a no-op" in {
            val cache = MemoryCache()
            val changed = cache.merge(
                List(rec(key("A"), fk("x") -> scalar("1"))),
                CacheHeaders.of(CacheHeaders.DoNotStore -> "true")
            )
            assert(changed == Set.empty[CacheKey])
            assert(cache.loadRecord(key("A")) == Absent)
        }

        // --- MemoryCache: LRU eviction --------------------------------------------

        "exceeding maxSize evicts the least-recently-used record" in {
            val cache = MemoryCache(maxSize = 2)
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1"))))
            cache.merge(List(rec(key("B"), fk("x") -> scalar("2"))))
            cache.merge(List(rec(key("C"), fk("x") -> scalar("3")))) // evicts A (oldest)
            assert(cache.loadRecord(key("A")) == Absent)
            assert(cache.loadRecord(key("B")).isDefined)
            assert(cache.loadRecord(key("C")).isDefined)
        }

        "a load refreshes recency so a later write evicts a colder record" in {
            val cache = MemoryCache(maxSize = 2)
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1"))))
            cache.merge(List(rec(key("B"), fk("x") -> scalar("2"))))
            cache.loadRecord(key("A")) // A is now most-recently-used
            cache.merge(List(rec(key("C"), fk("x") -> scalar("3")))) // evicts B, not A
            assert(cache.loadRecord(key("A")).isDefined)
            assert(cache.loadRecord(key("B")) == Absent)
            assert(cache.loadRecord(key("C")).isDefined)
        }

        // --- MemoryCache: expiration ----------------------------------------------

        "a record older than the TTL reads back as a miss" in {
            var now   = 1000L
            val cache = MemoryCache(expireAfterMillis = 100L, nowMillis = () => now)
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1"))))
            now = 1050L // within TTL
            assert(cache.loadRecord(key("A")).isDefined)
            now = 1200L // past TTL (200 > 100)
            assert(cache.loadRecord(key("A")) == Absent)
        }

        "the Date cache header overrides the clock for the expiry stamp" in {
            var now   = 5000L
            val cache = MemoryCache(expireAfterMillis = 100L, nowMillis = () => now)
            // Stamp the record as received at t=1000 even though the clock reads 5000.
            cache.merge(List(rec(key("A"), fk("x") -> scalar("1"))), CacheHeaders.of(CacheHeaders.Date -> "1000"))
            assert(cache.loadRecord(key("A")) == Absent) // 5000 - 1000 = 4000 > 100
        }

        "writeOperation normalizes into id-keyed records and reports the changed keys" in {
            val s       = store()
            val changed = s.writeOperation(CountriesQuery(), sampleData)
            assert(changed == Set(CacheKey.QueryRoot, CacheKey("Country", "DE"), CacheKey("Country", "FR")))
            assert(
                s.cache.loadRecord(CacheKey("Country", "DE")).flatMap(_.get(fk("name"))) == Present(
                    RecordValue.Scalar(jstr("Germany"))
                )
            )
        }

        "writeOperation then readOperation returns typed data equal to the original" in {
            val s = store()
            s.writeOperation(CountriesQuery(), sampleData)
            assert(s.readOperation(CountriesQuery()) == sampleData)
        }

        "readOperation on an empty store raises CacheMissException" in {
            val s = store()
            val _ = intercept[CacheMissException](s.readOperation(CountriesQuery()))
        }

        "readOperation on a partial store (a record removed) raises CacheMissException" in {
            val s = store()
            s.writeOperation(CountriesQuery(), sampleData)
            s.cache.remove(CacheKey("Country", "FR"))
            val _ = intercept[CacheMissException](s.readOperation(CountriesQuery()))
        }

        "publish notifies a registered listener with the changed keys of a write" in {
            val s    = store()
            var seen = Set.empty[CacheKey]
            s.addChangedKeysListener(keys => seen = keys)
            s.writeOperation(CountriesQuery(), sampleData)
            assert(seen == Set(CacheKey.QueryRoot, CacheKey("Country", "DE"), CacheKey("Country", "FR")))
        }

        "a second identical writeOperation publishes nothing (no changed keys)" in {
            val s = store()
            s.writeOperation(CountriesQuery(), sampleData)
            var seen = Option.empty[Set[CacheKey]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            val changed = s.writeOperation(CountriesQuery(), sampleData)
            assert(changed == Set.empty[CacheKey])
            assert(seen == None) // publish is a no-op on an empty changed set
        }

        "remove deletes a present record and publishes its key" in {
            val s = store()
            s.writeOperation(CountriesQuery(), sampleData)
            var seen = Option.empty[Set[CacheKey]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            assert(s.remove(CacheKey("Country", "FR")) == true)
            assert(seen == Some(Set(CacheKey("Country", "FR"))))
            assert(s.cache.loadRecord(CacheKey("Country", "FR")) == Absent)
        }

        "remove of an absent record publishes nothing" in {
            val s    = store()
            var seen = Option.empty[Set[CacheKey]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            assert(s.remove(CacheKey("Country", "XX")) == false)
            assert(seen == None)
        }

        "remove takes the record's CacheKey" in {
            val s = store()
            s.writeOperation(CountriesQuery(), sampleData)
            assert(s.remove(CacheKey("Country", "DE")) == true)
            assert(s.cache.loadRecord(CacheKey("Country", "DE")) == Absent)
        }

        "the cancel thunk from addChangedKeysListener unsubscribes the listener" in {
            val s      = store()
            var calls  = 0
            val handle = s.addChangedKeysListener(_ => calls += 1)
            s.writeOperation(CountriesQuery(), sampleData)
            handle()
            s.remove(CacheKey("Country", "DE"))
            assert(calls == 1) // only the write before close was delivered
        }

        "removeChangedKeysListener unsubscribes by callback identity" in {
            val s                               = store()
            var calls                           = 0
            val listener: Set[CacheKey] => Unit = _ => calls += 1
            s.addChangedKeysListener(listener)
            s.removeChangedKeysListener(listener)
            s.writeOperation(CountriesQuery(), sampleData)
            assert(calls == 0)
        }

        "manual publish reaches listeners for external invalidation" in {
            val s    = store()
            var seen = Option.empty[Set[CacheKey]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            s.publish(Set(CacheKey("User", "1")))
            assert(seen == Some(Set(CacheKey("User", "1"))))
        }
    }
end StoreSpec
