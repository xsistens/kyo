package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
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
        selections: Chunk[CompiledSelection]
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), selections = selections)

    /** `query { library { __typename id name books { __typename id title author { __typename id name } } } }` */
    final case class LibraryQuery() extends Query.Normalizable[LibraryData]:
        def name = "LibraryQuery"
        def document =
            "query LibraryQuery { library { __typename id name books { __typename id title author { __typename id name } } } }"
        val dataCodec: JsonCodec[LibraryData] = JsonCodec.fromSchema[LibraryData]
        def rootField: CompiledField =
            obj(
                "data",
                "Query",
                Chunk(
                    obj(
                        "library",
                        "Library",
                        Chunk(
                            leaf("__typename"),
                            leaf("id"),
                            leaf("name"),
                            CompiledField(
                                "books",
                                CompiledListType(CompiledNamedType("Book")),
                                selections = Chunk(
                                    leaf("__typename"),
                                    leaf("id"),
                                    leaf("title"),
                                    obj("author", "Author", Chunk(leaf("__typename"), leaf("id"), leaf("name")))
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
        Set(
            CacheKey.QueryRoot,
            CacheKey("Library", "l1"),
            CacheKey("Book", "b1"),
            CacheKey("Book", "b2"),
            CacheKey("Author", "a1"),
            CacheKey("Author", "a2")
        )

    private def store(): ApolloStore = new ApolloStore(MemoryCache())

    // An id-less object falls back to a stable response-path key through the same
    // default generator — the counterpart of the id branch.
    final case class Stats(label: String) derives Schema
    final case class StatsData(stats: Stats) derives Schema

    final case class StatsQuery() extends Query.Normalizable[StatsData]:
        def name                            = "StatsQuery"; def document = "query StatsQuery { stats { label } }"
        val dataCodec: JsonCodec[StatsData] = JsonCodec.fromSchema[StatsData]
        def rootField: CompiledField =
            obj("data", "Query", Chunk(obj("stats", "Stats", Chunk(leaf("label")))))
        def variables: Json = Json.JObj(VectorMap.empty)
    end StatsQuery

    // An IDENTIFIED parent holding an id-less child list, reached by two operations that
    // select different subfields of it — the F-18 shape. Two plain queries rather than a
    // query and a subscription: what matters is only that the two response paths differ
    // (`QUERY_ROOT.album...` vs `QUERY_ROOT.featured...`), which is what used to give the
    // same logical field two records. Both subfields are SERVER fields on purpose — a
    // `@client` field is never normalized and reads back as null, so it would make the
    // regression pass for the wrong reason.

    final case class CoverWide(url: String, alt: String) derives Schema
    final case class AlbumWide(__typename: String, id: String, covers: List[CoverWide]) derives Schema
    final case class WideData(album: AlbumWide) derives Schema

    final case class CoverNarrow(url: String) derives Schema
    final case class AlbumNarrow(__typename: String, id: String, covers: List[CoverNarrow])
        derives Schema
    final case class FeaturedData(featured: AlbumNarrow) derives Schema

    private def coversField(subfields: Chunk[CompiledSelection]): CompiledField =
        CompiledField("covers", CompiledListType(CompiledNamedType("Image")), selections = subfields)

    final case class WideQuery() extends Query.Normalizable[WideData]:
        def name = "WideQuery"
        def document =
            "query WideQuery { album { __typename id covers { url alt } } }"
        val dataCodec: JsonCodec[WideData] = JsonCodec.fromSchema[WideData]
        def rootField: CompiledField =
            obj(
                "data",
                "Query",
                Chunk(obj(
                    "album",
                    "Album",
                    Chunk(leaf("__typename"), leaf("id"), coversField(Chunk(leaf("url"), leaf("alt"))))
                ))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end WideQuery

    final case class FeaturedQuery() extends Query.Normalizable[FeaturedData]:
        def name = "FeaturedQuery"
        def document =
            "query FeaturedQuery { featured { __typename id covers { url } } }"
        val dataCodec: JsonCodec[FeaturedData] = JsonCodec.fromSchema[FeaturedData]
        def rootField: CompiledField =
            obj(
                "data",
                "Query",
                Chunk(obj(
                    "featured",
                    "Album",
                    Chunk(leaf("__typename"), leaf("id"), coversField(Chunk(leaf("url"))))
                ))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end FeaturedQuery

    private val wideAlbum =
        WideData(AlbumWide("Album", "1", List(CoverWide("u1", "A"), CoverWide("u2", "B"))))

    private val narrowAlbum =
        FeaturedData(AlbumNarrow("Album", "1", List(CoverNarrow("u1"), CoverNarrow("u2"))))

    /** A fake engine that counts calls and returns the deep body; `status` is
      * mutable so a test can flip the network to an error mid-run.
      */
    final private class CountingEngine(var status: Int = 200)
        extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(request: kyo.apollo.network.http.HttpEngine.Request)(using
            Frame
        ): kyo.apollo.network.http.HttpEngine.Response < Async =
            calls += 1
            kyo.apollo.network.http.HttpEngine.response(HttpStatus(status), libraryBody)
        end execute
    end CountingEngine

    private def cachedClient(engine: CountingEngine)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(
            ApolloClient.Config("https://example.com/graphql")
                .httpEngine(engine)
                .normalizedCache(MemoryCache()) // default id generator keys the deep graph
        )

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
                records(CacheKey.QueryRoot).get(fk("library")) ==
                    Present(RecordValue.Reference(CacheReference(CacheKey("Library", "l1"))))
            )
            // Library → a *list of references* to the per-book records (not inlined).
            assert(
                records(CacheKey("Library", "l1")).get(fk("books")) ==
                    Present(
                        RecordValue.RList(
                            Chunk(
                                RecordValue.Reference(CacheReference(CacheKey("Book", "b1"))),
                                RecordValue.Reference(CacheReference(CacheKey("Book", "b2")))
                            )
                        )
                    )
            )
            // Each Book → its Author by reference (the third level of linking).
            assert(
                records(CacheKey("Book", "b1")).get(fk("author")) == Present(
                    RecordValue.Reference(CacheReference(CacheKey("Author", "a1")))
                )
            )
            assert(
                records(CacheKey("Book", "b2")).get(fk("author")) == Present(
                    RecordValue.Reference(CacheReference(CacheKey("Author", "a2")))
                )
            )
            // Leaf scalars stay inline on their own record.
            assert(records(CacheKey("Author", "a1")).get(fk("name")) == Present(RecordValue.Scalar(Json.JStr("Herbert"))))
        }

        // --- 2. Cache-key generation with and without ids -----------------------

        "with ids: every object is keyed Typename:id via the default id generator" in {
            store().writeOperation(LibraryQuery(), sampleLibrary).map(changed => assert(changed == allKeys))
        }

        "without ids: an object with no id falls back to a response-path key" in {
            store().writeOperation(StatsQuery(), StatsData(Stats("ok"))).map { changed =>
                assert(changed == Set(CacheKey.QueryRoot, pathKey("QUERY_ROOT", "stats")))
            }
        }

        // --- 3. Denormalization round-trip: write then read returns equal data --

        "writeOperation then readOperation returns typed data equal to the deep input" in {
            val s = store()
            for
                _    <- s.writeOperation(LibraryQuery(), sampleLibrary)
                read <- s.readOperation(LibraryQuery())
            yield assert(read == sampleLibrary)
            end for
        }

        // --- 4. CacheMissException on a partial store ---------------------------

        "readOperation raises CacheMissException when a record deep in the graph is gone" in {
            val s = store()
            for
                _    <- s.writeOperation(LibraryQuery(), sampleLibrary)
                _    <- s.cache.remove(Chunk(CacheKey("Author", "a1"))) // a leaf entity two levels down
                read <- Abort.run[CacheMissException](s.readOperation(LibraryQuery()))
            yield assert(read.failure.map(_.key) == Present(CacheKey("Author", "a1").render))
            end for
        }

        "readOperation on an empty store raises CacheMissException at the root" in {
            Abort.run[CacheMissException](store().readOperation(LibraryQuery())).map { read =>
                assert(read.failure.map(_.key) == Present(CacheKey.QueryRoot.render))
            }
        }

        // --- 6. Two operations converging on one entity's id-less children -------

        "two operations writing the same entity share its id-less children, and neither loses a field" in {
            // GAPS.md F-18. The two operations reach Album:1 by different response paths and
            // select different subfields of its id-less `covers`. Keyed by the response path,
            // the narrower write repointed `Album:1.covers` at records that had never carried
            // `alt`, and the wider read then missed on data nobody had contradicted.
            val s = store()
            for
                _     <- s.writeOperation(WideQuery(), wideAlbum)
                _     <- s.writeOperation(FeaturedQuery(), narrowAlbum)
                read  <- s.readOperation(WideQuery())
                slot0 <- s.cache.loadRecord(pathKey("Album:1", "covers", "0"))
                all   <- s.cache.allRecords
            yield
                assert(read == wideAlbum)
                assert(slot0.map(_.fieldKeys) == Present(Set(fk("__typename"), fk("url"), fk("alt"))))
                // Two records for two covers — not four. Before the fix each writer minted its own
                // pair under its own response path, and the parent pointed at whichever came last.
                assert(
                    all.keySet.filter(_.render.contains("covers")) ==
                        Set(pathKey("Album:1", "covers", "0"), pathKey("Album:1", "covers", "1"))
                )
            end for
        }

        "a reordered id-less list merges positionally, keeping the previous occupant's fields" in {
            // The hazard the fix trades the hard failure for, pinned rather than left implicit:
            // an id-less element is addressed by its INDEX under the parent, so a shorter or
            // reordered write merges into the slot the previous element occupied. apollo-kotlin
            // behaves the same way; the remedy is to give the element type a cache identity.
            val s = store()
            for
                _ <- s.writeOperation(WideQuery(), wideAlbum) // covers = [c1(alt=A), c2(alt=B)]
                _ <- s.writeOperation(
                    FeaturedQuery(),
                    narrowAlbum.copy(featured = // covers = [c2] only
                        narrowAlbum.featured.copy(covers = List(CoverNarrow("u2")))
                    )
                )
                slot0 <- s.cache.loadRecord(pathKey("Album:1", "covers", "0"))
            yield
                // Slot 0 now holds c2's url beside c1's alt. Structurally valid, semantically wrong.
                assert(slot0.map(_.get(fk("url"))) == Present(Present(RecordValue.Scalar(Json.JStr("u2")))))
                assert(slot0.map(_.get(fk("alt"))) == Present(Present(RecordValue.Scalar(Json.JStr("A")))))
            end for
        }

        // --- 7. The positional-merge diagnostic ---------------------------------

        "the diagnostic reports a positional merge that CONTRADICTS a stored field" in {
            val seen = scala.collection.mutable.ListBuffer.empty[String]
            val s    = new ApolloStore(MemoryCache(), diagnostics = CacheDiagnostics.to(seen.append(_)))
            for
                _ <- s.writeOperation(WideQuery(), wideAlbum)
                _ <- s.writeOperation(
                    FeaturedQuery(),
                    narrowAlbum.copy(featured = narrowAlbum.featured.copy(covers = List(CoverNarrow("u2"))))
                )
            yield
                assert(seen.size == 1, s"expected one warning, got: $seen")
                assert(seen.head.contains("Album:1.covers.0"))
                assert(seen.head.contains("'url'"))
                assert(seen.head.contains("'Image'"), s"names the type to give an identity to: ${seen.head}")
            end for
        }

        "the diagnostic stays silent when a disjoint write merely EXTENDS the record" in {
            // The false-positive guard, and the more important of the two: this is the
            // ordinary overlapping-selection case, which must not be reported, or the
            // diagnostic gets muted and then protects nobody.
            val seen = scala.collection.mutable.ListBuffer.empty[String]
            val s    = new ApolloStore(MemoryCache(), diagnostics = CacheDiagnostics.to(seen.append(_)))
            for
                _ <- s.writeOperation(WideQuery(), wideAlbum)
                _ <- s.writeOperation(FeaturedQuery(), narrowAlbum)
            yield assert(seen.isEmpty, s"expected silence, got: $seen")
            end for
        }

        "the diagnostic stays silent for an identity-keyed record" in {
            // Only positionally-addressed records carry the hazard: an entity is the same
            // object whichever write reaches it, so a changed field there is just news.
            val seen = scala.collection.mutable.ListBuffer.empty[String]
            val s    = new ApolloStore(MemoryCache(), diagnostics = CacheDiagnostics.to(seen.append(_)))
            for
                _ <- s.writeOperation(LibraryQuery(), sampleLibrary)
                _ <- s.writeOperation(
                    LibraryQuery(),
                    sampleLibrary.copy(library = sampleLibrary.library.copy(name = "Branch"))
                )
            yield assert(seen.isEmpty, s"expected silence, got: $seen")
            end for
        }

        // --- 5. Each FetchPolicy's emission sequence over a fake transport -------

        "CacheFirst: network on the cold call, cache hit on the second (one fetch)" in {
            val engine = CountingEngine()
            for
                client <- cachedClient(engine)
                r1     <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
                r2     <- call(client).fetchPolicy(FetchPolicy.CacheFirst).execute
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
            for
                client <- cachedClient(engine)
                r1     <- call(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                r2     <- call(client).fetchPolicy(FetchPolicy.CacheOnly).execute
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(engine.calls == 1)
                assert(r2.data == Present(sampleLibrary)) // NetworkOnly wrote the deep graph back
                assert(r2.cacheInfo.map(_.isCacheHit) == Present(true))
            end for
        }

        "CacheOnly: an empty store emits a CacheMissException value and never hits the network" in {
            val engine = CountingEngine()
            cachedClient(engine).map { client =>
                call(client).fetchPolicy(FetchPolicy.CacheOnly).execute.map { r =>
                    assert(engine.calls == 0)
                    assert(r.data == Absent)
                    assert(r.error.exists(_.isInstanceOf[CacheMissException]))
                }
            }
        }

        "CacheAndNetwork: emits the cache response then the network response" in {
            val engine = CountingEngine()
            for
                client    <- cachedClient(engine)
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
            for
                client <- cachedClient(engine)
                r1     <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute // success, writes back
                _      <- Sync.defer { engine.status = 500 }
                r2     <- call(client).fetchPolicy(FetchPolicy.NetworkFirst).execute // errors, cache fallback
            yield
                assert(r1.cacheInfo.map(_.fromCache) == Present(false))
                assert(r2.data == Present(sampleLibrary))
                assert(r2.cacheInfo.exists(_.isCacheHit))
                assert(engine.calls == 2)
            end for
        }
    }
end CacheSpec
