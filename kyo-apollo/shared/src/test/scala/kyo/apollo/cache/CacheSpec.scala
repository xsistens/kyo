package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap

/** Phase 04 acceptance suite for the normalized cache.
  *
  * Where the sibling specs test each component in isolation on flat fixtures (a
  * single `book`, or a flat `countries` list), this suite drives a deliberately
  * *deep* domain — `library { id name books { id title author { id name } } }`,
  * an object holding a list of objects each holding a nested object, every level
  * id-keyed — end-to-end through the public [[ApolloStore]] and [[ApolloClient]].
  * It maps one section to each bullet of the "write tests" task:
  *
  *   1. normalization of nested objects *and* lists into linked records,
  *   2. cache-key generation with and without ids,
  *   3. denormalization round-trip (`writeOperation` → `readOperation` == input),
  *   4. `CacheMissException` on a partial store (a record deep in the graph gone),
  *   5. each [[FetchPolicy]] emitting the expected response sequence against a
  *      fake network transport.
  *
  * Per-component edge cases (merge policy, LRU/TTL, inline fragments, redirects,
  * argument-aware keys) live in [[NormalizerSpec]], [[CacheBatchReaderSpec]],
  * [[CacheKeyGenerationSpec]], [[RecordModelSpec]], [[StoreSpec]] and
  * [[CacheInterceptorSpec]] and are not repeated here.
  */
class CacheSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- The deep domain: Library → [Book] → Author ---------------------------

    final case class Author(__typename: String, id: String, name: String) derives Schema
    final case class Book(__typename: String, id: String, title: String, author: Author)
        derives Schema
    final case class Library(__typename: String, id: String, name: String, books: List[Book])
        derives Schema
    final case class LibraryData(library: Library) derives Schema

    private def leaf(name: String): CompiledField =
        CompiledField(name, CompiledNamedType("String"))

    private def obj(
        name: String,
        typeName: String,
        selections: List[CompiledSelection]
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), selections = selections)

    /** `query { library { __typename id name books { __typename id title author { __typename id name } } } }` */
    final case class LibraryQuery() extends Query[LibraryData]:
        def name = "LibraryQuery"
        def document =
            "query LibraryQuery { library { __typename id name books { __typename id title author { __typename id name } } } }"
        def dataSchema: Schema[LibraryData] = summon[Schema[LibraryData]]
        def rootField: CompiledField =
            obj(
                "data",
                "Query",
                List(
                    obj(
                        "library",
                        "Library",
                        List(
                            leaf("__typename"),
                            leaf("id"),
                            leaf("name"),
                            CompiledField(
                                "books",
                                CompiledListType(CompiledNamedType("Book")),
                                selections = List(
                                    leaf("__typename"),
                                    leaf("id"),
                                    leaf("title"),
                                    obj("author", "Author", List(leaf("__typename"), leaf("id"), leaf("name")))
                                )
                            )
                        )
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end LibraryQuery

    private val sampleLibrary = LibraryData(
        Library(
            "Library",
            "l1",
            "Central",
            List(
                Book("Book", "b1", "Dune", Author("Author", "a1", "Herbert")),
                Book("Book", "b2", "Hyperion", Author("Author", "a2", "Simmons"))
            )
        )
    )

    /** The same value the [[LibraryQuery]] would decode from the network. */
    private val libraryBody =
        """{"data":{"library":{"__typename":"Library","id":"l1","name":"Central","books":[""" +
            """{"__typename":"Book","id":"b1","title":"Dune","author":{"__typename":"Author","id":"a1","name":"Herbert"}},""" +
            """{"__typename":"Book","id":"b2","title":"Hyperion","author":{"__typename":"Author","id":"a2","name":"Simmons"}}""" +
            """]}}}"""

    /** All records the deep response normalizes into, one per id-bearing object. */
    private val allKeys =
        Set("QUERY_ROOT", "Library:l1", "Book:b1", "Book:b2", "Author:a1", "Author:a2")

    private def store(): ApolloStore = new ApolloStore(MemoryCache())

    // An id-less object falls back to a stable response-path key through the same
    // default generator — the counterpart of the id branch.
    final case class Stats(label: String) derives Schema
    final case class StatsData(stats: Stats) derives Schema

    final case class StatsQuery() extends Query[StatsData]:
        def name                          = "StatsQuery"; def document = "query StatsQuery { stats { label } }"
        def dataSchema: Schema[StatsData] = summon[Schema[StatsData]]
        def rootField: CompiledField =
            obj("data", "Query", List(obj("stats", "Stats", List(leaf("label")))))
        def variables: Json = Json.JObj(VectorMap.empty)
    end StatsQuery

    /** A fake engine that counts calls and returns the deep body; `status` is
      * mutable so a test can flip the network to an error mid-run.
      */
    final private class CountingEngine(var status: Int = 200)
        extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(request: kyo.apollo.network.http.HttpRequest)(using
            Frame
        ): kyo.apollo.network.http.HttpResponse < Async =
            calls += 1
            kyo.apollo.network.http.HttpResponse(status, Nil, libraryBody)
        end execute
    end CountingEngine

    private def cachedClient(engine: CountingEngine): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache()) // default id generator keys the deep graph
            .build()

    private def call(client: ApolloClient) = client.query(LibraryQuery())

    private def collectAll(
        c: kyo.apollo.runtime.ApolloCall[LibraryData]
    ): List[ApolloResponse[LibraryData]] < (Async & Scope) =
        StreamProbe.collect(c.stream)

    "normalized cache" - {

        // --- 1. Normalization: nested objects AND lists → linked records --------

        "normalization links nested objects and list elements into separate records" in {
            val records = store().normalize(LibraryQuery(), sampleLibrary)

            // One record per object at every level — nothing inlined across an entity.
            assert(records.keySet == allKeys)

            // Root → Library by reference.
            assert(
                records("QUERY_ROOT").get("library") ==
                    Present(RecordValue.Reference(CacheReference("Library:l1")))
            )
            // Library → a *list of references* to the per-book records (not inlined).
            assert(
                records("Library:l1").get("books") ==
                    Present(
                        RecordValue.RList(
                            Chunk(
                                RecordValue.Reference(CacheReference("Book:b1")),
                                RecordValue.Reference(CacheReference("Book:b2"))
                            )
                        )
                    )
            )
            // Each Book → its Author by reference (the third level of linking).
            assert(
                records("Book:b1").get("author") == Present(
                    RecordValue.Reference(CacheReference("Author:a1"))
                )
            )
            assert(
                records("Book:b2").get("author") == Present(
                    RecordValue.Reference(CacheReference("Author:a2"))
                )
            )
            // Leaf scalars stay inline on their own record.
            assert(records("Author:a1").get("name") == Present(RecordValue.Scalar(Json.JStr("Herbert"))))
        }

        // --- 2. Cache-key generation with and without ids -----------------------

        "with ids: every object is keyed Typename:id via the default id generator" in {
            val changed = store().writeOperation(LibraryQuery(), sampleLibrary)
            assert(changed == allKeys)
        }

        "without ids: an object with no id falls back to a response-path key" in {
            val changed = store().writeOperation(StatsQuery(), StatsData(Stats("ok")))
            assert(changed == Set("QUERY_ROOT", "QUERY_ROOT.stats"))
        }

        // --- 3. Denormalization round-trip: write then read returns equal data --

        "writeOperation then readOperation returns typed data equal to the deep input" in {
            val s = store()
            s.writeOperation(LibraryQuery(), sampleLibrary)
            assert(s.readOperation(LibraryQuery()) == sampleLibrary)
        }

        // --- 4. CacheMissException on a partial store ---------------------------

        "readOperation raises CacheMissException when a record deep in the graph is gone" in {
            val s = store()
            s.writeOperation(LibraryQuery(), sampleLibrary)
            s.cache.remove("Author:a1") // a leaf entity two levels down
            val miss = intercept[CacheMissException](s.readOperation(LibraryQuery()))
            assert(miss.key == "Author:a1")
        }

        "readOperation on an empty store raises CacheMissException at the root" in {
            val miss = intercept[CacheMissException](store().readOperation(LibraryQuery()))
            assert(miss.key == "QUERY_ROOT")
        }

        // --- 5. Each FetchPolicy's emission sequence over a fake transport -------

        "CacheFirst: network on the cold call, cache hit on the second (one fetch)" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
                r2 <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
            yield
                assert(r1.data == Present(sampleLibrary))
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(r2.data == Present(sampleLibrary)) // reassembled from linked records
                assert(r2.cacheInfo.map(_.isCacheHit) == Present(true))
                assert(engine.calls == 1)
            end for
        }

        "NetworkOnly: always fetches and writes back (proven by a follow-up CacheOnly hit)" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                r2 <- call(client).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(engine.calls == 1)
                assert(r2.data == Present(sampleLibrary)) // NetworkOnly wrote the deep graph back
                assert(r2.cacheInfo.map(_.isCacheHit) == Present(true))
            end for
        }

        "CacheOnly: an empty store emits a CacheMissException value and never hits the network" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            call(client).fetchPolicy(FetchPolicy.CacheOnly).execute.map { r =>
                assert(engine.calls == 0)
                assert(r.data == Absent)
                assert(r.exception.exists(_.isInstanceOf[CacheMissException]))
            }
        }

        "CacheAndNetwork: emits the cache response then the network response" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                _         <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute // populate
                emissions <- collectAll(call(client).fetchPolicy(FetchPolicy.CacheAndNetwork))
            yield
                assert(emissions.length == 2)
                assert(emissions(0).cacheInfo.map(_.isCacheHit) == Present(true))
                assert(emissions(1).cacheInfo.map(_.fromCache) == Present(false))
                assert(emissions.map(_.data) == List(Present(sampleLibrary), Present(sampleLibrary)))
                assert(engine.calls == 2) // one populate + one network leg
            end for
        }

        "NetworkFirst: serves the network on success, falls back to cache on error" in {
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                r1 <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute // success, writes back
                _  <- Sync.defer { engine.status = 500 }
                r2 <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute // errors, cache fallback
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(r2.data == Present(sampleLibrary))
                assert(r2.cacheInfo.exists(_.isCacheHit))
                assert(engine.calls == 2)
            end for
        }
    }
end CacheSpec
