package kyo.apollo.cache

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit tests for the store backends: the [[NormalizedCache]] merge policy, the
  * [[MemoryCache]] backend (LRU + expiration), and the [[ApolloStore]]
  * coordinator's `writeOperation` → `readOperation` round-trip and `publish` hook.
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
            for
                changed <- cache.merge(Chunk(record))
                loaded  <- cache.loadRecord(CacheKey("Book", "1"))
            yield
                assert(changed == Set(CacheKey("Book", "1")))
                assert(loaded == Present(record))
            end for
        }

        "re-merging identical data reports no changed keys" in {
            val cache  = MemoryCache()
            val record = rec(CacheKey("Book", "1"), fk("id") -> scalar("1"))
            for
                _       <- cache.merge(Chunk(record))
                changed <- cache.merge(Chunk(record))
            yield assert(changed == Set.empty[CacheKey])
            end for
        }

        "loadRecords returns only the present subset" in {
            val cache = MemoryCache()
            for
                _      <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1")), rec(key("B"), fk("y") -> scalar("2"))))
                loaded <- cache.loadRecords(Chunk(key("A"), key("missing")))
            yield assert(loaded.keySet == Set(key("A")))
            end for
        }

        "remove deletes a present record and reports absence otherwise" in {
            val cache = MemoryCache()
            for
                _      <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1"))))
                first  <- cache.remove(Chunk(key("A")))
                second <- cache.remove(Chunk(key("A")))
                loaded <- cache.loadRecord(key("A"))
            yield
                assert(first == Set(key("A")))
                assert(second == Set.empty[CacheKey])
                assert(loaded == Absent)
            end for
        }

        "clearAll empties the store" in {
            val cache = MemoryCache()
            for
                _ <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1")), rec(key("B"), fk("y") -> scalar("2"))))
                _ <- cache.clearAll
                a <- cache.loadRecord(key("A"))
                b <- cache.loadRecord(key("B"))
            yield
                assert(a == Absent)
                assert(b == Absent)
            end for
        }

        "DoNotStore header makes merge a no-op" in {
            val cache = MemoryCache()
            for
                changed <- cache.merge(
                    Chunk(rec(key("A"), fk("x") -> scalar("1"))),
                    CacheHeaders.of(CacheHeaders.DoNotStore -> "true")
                )
                loaded <- cache.loadRecord(key("A"))
            yield
                assert(changed == Set.empty[CacheKey])
                assert(loaded == Absent)
            end for
        }

        // --- MemoryCache: LRU eviction --------------------------------------------

        "exceeding maxSize evicts the least-recently-used record" in {
            val cache = MemoryCache(maxSize = 2)
            for
                _ <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1"))))
                _ <- cache.merge(Chunk(rec(key("B"), fk("x") -> scalar("2"))))
                _ <- cache.merge(Chunk(rec(key("C"), fk("x") -> scalar("3")))) // evicts A (oldest)
                a <- cache.loadRecord(key("A"))
                b <- cache.loadRecord(key("B"))
                c <- cache.loadRecord(key("C"))
            yield
                assert(a == Absent)
                assert(b.isDefined)
                assert(c.isDefined)
            end for
        }

        "a load refreshes recency so a later write evicts a colder record" in {
            val cache = MemoryCache(maxSize = 2)
            for
                _ <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1"))))
                _ <- cache.merge(Chunk(rec(key("B"), fk("x") -> scalar("2"))))
                _ <- cache.loadRecord(key("A"))                                // A is now most-recently-used
                _ <- cache.merge(Chunk(rec(key("C"), fk("x") -> scalar("3")))) // evicts B, not A
                a <- cache.loadRecord(key("A"))
                b <- cache.loadRecord(key("B"))
                c <- cache.loadRecord(key("C"))
            yield
                assert(a.isDefined)
                assert(b == Absent)
                assert(c.isDefined)
            end for
        }

        // --- MemoryCache: expiration ----------------------------------------------

        "a record older than the TTL reads back as a miss" in {
            var now   = 1000L
            val cache = MemoryCache(expireAfterMillis = 100L, nowMillis = () => now)
            for
                _      <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1"))))
                _      <- Sync.defer { now = 1050L } // within TTL
                within <- cache.loadRecord(key("A"))
                _      <- Sync.defer { now = 1200L } // past TTL (200 > 100)
                past   <- cache.loadRecord(key("A"))
            yield
                assert(within.isDefined)
                assert(past == Absent)
            end for
        }

        "the Date cache header overrides the clock for the expiry stamp" in {
            val now   = 5000L
            val cache = MemoryCache(expireAfterMillis = 100L, nowMillis = () => now)
            // Stamp the record as received at t=1000 even though the clock reads 5000.
            for
                _      <- cache.merge(Chunk(rec(key("A"), fk("x") -> scalar("1"))), CacheHeaders.of(CacheHeaders.Date -> "1000"))
                loaded <- cache.loadRecord(key("A"))
            yield assert(loaded == Absent) // 5000 - 1000 = 4000 > 100
            end for
        }

        "writeOperation normalizes into id-keyed records and reports the changed keys" in {
            val s = store()
            for
                changed <- s.writeOperation(CountriesQuery(), sampleData)
                de      <- s.cache.loadRecord(CacheKey("Country", "DE"))
            yield
                assert(changed == Set(CacheKey.QueryRoot, CacheKey("Country", "DE"), CacheKey("Country", "FR")))
                assert(de.flatMap(_.get(fk("name"))) == Present(RecordValue.Scalar(jstr("Germany"))))
            end for
        }

        "writeOperation then readOperation returns typed data equal to the original" in {
            val s = store()
            for
                _    <- s.writeOperation(CountriesQuery(), sampleData)
                read <- s.readOperation(CountriesQuery())
            yield assert(read == sampleData)
            end for
        }

        "readOperation on an empty store raises CacheMissException" in {
            val s = store()
            Abort.run[CacheMissException](s.readOperation(CountriesQuery())).map(result => assert(result.isFailure))
        }

        "readOperation on a partial store (a record removed) raises CacheMissException" in {
            val s = store()
            for
                _      <- s.writeOperation(CountriesQuery(), sampleData)
                _      <- s.cache.remove(Chunk(CacheKey("Country", "FR")))
                result <- Abort.run[CacheMissException](s.readOperation(CountriesQuery()))
            yield assert(result.isFailure)
            end for
        }

        "publish notifies a registered listener with the changed keys of a write" in {
            val s    = store()
            var seen = Set.empty[CacheKey]
            for
                _ <- s.addChangedKeysListener(keys => seen = keys)
                _ <- s.writeOperation(CountriesQuery(), sampleData)
            yield assert(seen == Set(CacheKey.QueryRoot, CacheKey("Country", "DE"), CacheKey("Country", "FR")))
            end for
        }

        "a second identical writeOperation publishes nothing (no changed keys)" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _       <- s.writeOperation(CountriesQuery(), sampleData)
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                changed <- s.writeOperation(CountriesQuery(), sampleData)
            yield
                assert(changed == Set.empty[CacheKey])
                assert(seen == Absent) // publish is a no-op on an empty changed set
            end for
        }

        "remove deletes a present record and publishes its key" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _       <- s.writeOperation(CountriesQuery(), sampleData)
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                removed <- s.remove(CacheKey("Country", "FR"))
                loaded  <- s.cache.loadRecord(CacheKey("Country", "FR"))
            yield
                assert(removed == true)
                assert(seen == Present(Set(CacheKey("Country", "FR"))))
                assert(loaded == Absent)
            end for
        }

        "remove of an absent record publishes nothing" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                removed <- s.remove(CacheKey("Country", "XX"))
            yield
                assert(removed == false)
                assert(seen == Absent)
            end for
        }

        "remove takes the record's CacheKey" in {
            val s = store()
            for
                _       <- s.writeOperation(CountriesQuery(), sampleData)
                removed <- s.remove(CacheKey("Country", "DE"))
                loaded  <- s.cache.loadRecord(CacheKey("Country", "DE"))
            yield
                assert(removed == true)
                assert(loaded == Absent)
            end for
        }

        "a listener registered in a Scope stops receiving once that Scope closes" in {
            val s     = store()
            var calls = 0
            for
                _ <- Scope.run(s.addChangedKeysListener(_ => calls += 1).andThen(s.writeOperation(CountriesQuery(), sampleData)))
                _ <- s.remove(CacheKey("Country", "DE"))
            yield assert(calls == 1) // only the write before the Scope closed was delivered
            end for
        }

        "removeChangedKeysListener unsubscribes by callback identity" in {
            val s                                      = store()
            var calls                                  = 0
            val listener: Set[CacheKey] => Unit < Sync = _ => calls += 1
            for
                _ <- s.addChangedKeysListener(listener)
                _ <- s.removeChangedKeysListener(listener)
                _ <- s.writeOperation(CountriesQuery(), sampleData)
            yield assert(calls == 0)
            end for
        }

        "manual publish reaches listeners for external invalidation" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _ <- s.addChangedKeysListener(keys => seen = Present(keys))
                _ <- s.publish(Set(CacheKey("User", "1")))
            yield assert(seen == Present(Set(CacheKey("User", "1"))))
            end for
        }

        "a cache read has no synchronous form" in {
            typeCheckFailure("""
                val cache                  = kyo.apollo.cache.normalized.MemoryCache()
                val loaded: kyo.Maybe[kyo.apollo.cache.normalized.api.Record] =
                    cache.loadRecord(kyo.apollo.cache.normalized.api.CacheKey("A", "1"))(using kyo.Frame.internal)
            """)("Required: kyo.Maybe[kyo.apollo.cache.normalized.api.Record]")
            typeCheckFailure("""
                val store = new kyo.apollo.cache.normalized.ApolloStore(kyo.apollo.cache.normalized.MemoryCache())
                val generation: Long = store.currentGeneration(using kyo.Frame.internal)
            """)("Required: Long")
        }
    }
end StoreSpec
