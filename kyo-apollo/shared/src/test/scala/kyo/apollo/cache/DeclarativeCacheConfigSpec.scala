package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.internal.Normalizer
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit + end-to-end tests for Phase 07 declarative cache configuration:
  * [[TypePolicy]] / [[TypePolicyCacheKeyGenerator]] (custom key fields),
  * [[FieldKey]] `keyArgs` filtering, [[FieldPolicy]] read redirects and custom
  * merges, and [[ConnectionFieldPolicy]] paginated-list merging through the
  * [[ApolloStore]].
  */
class DeclarativeCacheConfigSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def jstr(s: String): Json = Json.JStr(s)
    private def jnum(n: Double): Json = Json.JNum(n)

    // --- TypePolicyCacheKeyGenerator ------------------------------------------

    private def ctx(path: List[String] = Nil): CacheKeyGeneratorContext =
        CacheKeyGeneratorContext(CompiledField("f", CompiledNamedType("X")), Map.empty, path)

    // --- FieldKey keyArgs filtering -------------------------------------------

    private def feedField(args: List[CompiledArgument]): CompiledField =
        CompiledField("feed", CompiledNamedType("FeedConnection"), arguments = args)

    // --- ConnectionFieldPolicy.unionByReference -------------------------------

    private def ref(key: String): RecordValue = RecordValue.reference(CacheKey(key))

    // --- TypePolicy through the Normalizer ------------------------------------

    final private case class KeyedQuery(selections: List[CompiledSelection]) extends Query[Int]:
        def name                    = "Q"; def document = "query Q { ... }"
        def dataSchema: Schema[Int] = summon[Schema[Int]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = selections)
        def variables: Json = Json.JObj(VectorMap.empty)
    end KeyedQuery

    // --- ConnectionFieldPolicy end to end through ApolloStore -----------------

    // `__typename` named verbatim so kyo-schema encodes the response key the
    // Normalizer/CacheBatchReader match on.
    final case class Post(__typename: String, id: String, title: String) derives Schema
    final case class PostEdge(__typename: String, cursor: String, node: Post) derives Schema
    final case class PageInfo(endCursor: String, hasNextPage: Boolean) derives Schema
    final case class Feed(__typename: String, edges: List[PostEdge], pageInfo: PageInfo)
        derives Schema
    final case class FeedData(feed: Feed) derives Schema

    /** `{ feed(first: 2, after: $after) { __typename edges { __typename cursor
      * node { __typename id title } } pageInfo { endCursor hasNextPage } } }`.
      * `after` rides as a literal so each page is a distinct request; the
      * connection policy's `keyArgs` collapses both onto one cache slot.
      */
    final case class FeedQuery(after: Option[String]) extends Query[FeedData]:
        def name                         = "Feed"
        def document                     = "query Feed { feed { ... } }"
        def dataSchema: Schema[FeedData] = summon[Schema[FeedData]]
        def rootField: CompiledField =
            val args = CompiledArgument.literal("first", jnum(2)) ::
                after.map(c => CompiledArgument.literal("after", jstr(c))).toList
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(
                    CompiledField(
                        "feed",
                        CompiledNamedType("FeedConnection"),
                        arguments = args,
                        selections = List(
                            CompiledField("__typename", CompiledNamedType("String")),
                            CompiledField(
                                "edges",
                                CompiledListType(CompiledNamedType("PostEdge")),
                                selections = List(
                                    CompiledField("__typename", CompiledNamedType("String")),
                                    CompiledField("cursor", CompiledNamedType("String")),
                                    CompiledField(
                                        "node",
                                        CompiledNamedType("Post"),
                                        selections = List(
                                            CompiledField("__typename", CompiledNamedType("String")),
                                            CompiledField("id", CompiledNamedType("String")),
                                            CompiledField("title", CompiledNamedType("String"))
                                        )
                                    )
                                )
                            ),
                            CompiledField(
                                "pageInfo",
                                CompiledNamedType("PageInfo"),
                                selections = List(
                                    CompiledField("endCursor", CompiledNamedType("String")),
                                    CompiledField("hasNextPage", CompiledNamedType("Boolean"))
                                )
                            )
                        )
                    )
                )
            )
        end rootField
        def variables: Json = Json.JObj(VectorMap.empty)
    end FeedQuery

    private def connectionStore(): ApolloStore =
        new ApolloStore(
            MemoryCache(),
            // Edges keyed by cursor so successive pages do not collide on a positional
            // key; nodes fall through to the default id-based key.
            cacheKeyGenerator = TypePolicyCacheKeyGenerator.of(TypePolicy("PostEdge", List("cursor"))),
            fieldPolicies = FieldPolicies.fromList(ConnectionFieldPolicy("Query", "feed"))
        )

    private def page(cursors: List[(String, String)], endCursor: String, hasNext: Boolean): FeedData =
        FeedData(
            Feed(
                "PostConnection",
                cursors.map { case (cursor, id) =>
                    PostEdge("PostEdge", cursor, Post("Post", id, s"Title-$id"))
                },
                PageInfo(endCursor, hasNext)
            )
        )

    "TypePolicy / FieldKey keyArgs / FieldPolicy / ConnectionFieldPolicy" - {

        "a TypePolicy keys objects of its type by the configured field" in {
            val gen = TypePolicyCacheKeyGenerator.of(TypePolicy("Country", List("code")))
            val obj = Map("__typename" -> jstr("Country"), "code" -> jstr("DE"))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("Country", "DE")))
        }

        "multiple key fields are joined in declaration order with a + separator" in {
            val gen = TypePolicyCacheKeyGenerator.of(TypePolicy("Book", List("isbn", "edition")))
            val obj = Map("__typename" -> jstr("Book"), "isbn" -> jstr("111"), "edition" -> jnum(2))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("Book:111+2")))
        }

        "a type without a policy falls back to the default id-based key" in {
            val gen = TypePolicyCacheKeyGenerator.of(TypePolicy("Country", List("code")))
            val obj = Map("__typename" -> jstr("User"), "id" -> jstr("u1"))
            assert(gen.cacheKeyForObject(obj, ctx()) == Present(CacheKey("User", "u1")))
        }

        "a policied type missing a key field falls back to its response path" in {
            val gen = TypePolicyCacheKeyGenerator.of(TypePolicy("Country", List("code")))
            val obj = Map("__typename" -> jstr("Country")) // no `code`
            assert(
                gen.cacheKeyForObject(obj, ctx(path = List("QUERY_ROOT", "country"))) ==
                    Present(CacheKey("QUERY_ROOT.country"))
            )
        }

        "TypePolicy requires at least one key field" in {
            val _ = intercept[IllegalArgumentException](TypePolicy("Country", Nil))
        }

        "keyArgs restricts a field key to the named arguments" in {
            val field = feedField(
                List(
                    CompiledArgument.literal("category", jstr("tech")),
                    CompiledArgument.literal("first", jnum(10)),
                    CompiledArgument.literal("after", jstr("cursor"))
                )
            )
            assert(FieldKey(field, Map.empty, List("category")) == "feed({\"category\":\"tech\"})")
        }

        "empty keyArgs collapses a field to its bare name regardless of arguments" in {
            val field = feedField(List(CompiledArgument.literal("first", jnum(10))))
            assert(FieldKey(field, Map.empty, Nil) == "feed")
        }

        "without keyArgs a field key includes all of its arguments" in {
            val field = feedField(List(CompiledArgument.literal("first", jnum(10))))
            assert(FieldKey(field, Map.empty) == "feed({\"first\":10})")
        }

        "unionByReference appends new edge references, de-duplicating by key" in {
            val existing = RecordValue.RList(Chunk(ref("E:1"), ref("E:2")))
            val incoming = RecordValue.RList(Chunk(ref("E:2"), ref("E:3")))
            assert(
                ConnectionFieldPolicy.unionByReference(Some(existing), incoming) ==
                    RecordValue.RList(Chunk(ref("E:1"), ref("E:2"), ref("E:3")))
            )
        }

        "unionByReference on a first write keeps the incoming list" in {
            val incoming = RecordValue.RList(Chunk(ref("E:1")))
            assert(ConnectionFieldPolicy.unionByReference(None, incoming) == incoming)
        }

        "an empty registry is identity: full field key, no redirect, no merge" in {
            val policies = FieldPolicies.empty
            assert(policies.isEmpty)
            val field = feedField(List(CompiledArgument.literal("first", jnum(1))))
            assert(policies.fieldKey(field, Map.empty) == "feed({\"first\":1})")
            assert(policies.readRedirect(field, Map.empty) == Absent)
            assert(policies.fieldMerge("feed") == Absent)
        }

        "a FieldPolicy read resolver is surfaced by the registry" in {
            val policies = FieldPolicies.of(
                FieldPolicy("Query", "book", read = Some(_ => Present(CacheKey("Book", "42"))))
            )
            val field = CompiledField("book", CompiledNamedType("Book"))
            assert(policies.readRedirect(field, Map.empty) == Present(CacheKey("Book", "42")))
        }

        "RecordMerger.fieldPolicies unions a policied field and reports it changed" in {
            val policies = FieldPolicies.of(
                FieldPolicy("Feed", "edges", merge = Some(ConnectionFieldPolicy.unionByReference))
            )
            val merger            = RecordMerger.fieldPolicies(policies)
            val existing          = Record("Feed:1", Map("edges" -> RecordValue.RList(Chunk(ref("E:1")))))
            val incoming          = Record("Feed:1", Map("edges" -> RecordValue.RList(Chunk(ref("E:2")))))
            val (merged, changed) = merger.merge(Present(existing), incoming)
            assert(merged.get("edges") == Present(RecordValue.RList(Chunk(ref("E:1"), ref("E:2")))))
            assert(changed == Set("edges"))
        }

        "RecordMerger.fieldPolicies falls back to the default for unpolicied fields" in {
            val merger = RecordMerger.fieldPolicies(FieldPolicies.empty)
            assert(merger eq RecordMerger.default)
        }

        "the Normalizer keys an object by its TypePolicy fields end to end" in {
            val selections = List(
                CompiledField(
                    "country",
                    CompiledNamedType("Country"),
                    selections = List(
                        CompiledField("__typename", CompiledNamedType("String")),
                        CompiledField("code", CompiledNamedType("String")),
                        CompiledField("name", CompiledNamedType("String"))
                    )
                )
            )
            val data = Map(
                "country" -> Json.JObj(
                    Map("__typename" -> jstr("Country"), "code" -> jstr("FR"), "name" -> jstr("France"))
                )
            )
            val records = Normalizer.normalize(
                KeyedQuery(selections),
                data,
                cacheKeyGenerator = TypePolicyCacheKeyGenerator.of(TypePolicy("Country", List("code")))
            )
            assert(records.keySet == Set("QUERY_ROOT", "Country:FR"))
            assert(records("Country:FR").get("name") == Present(RecordValue.Scalar(jstr("France"))))
        }

        "ConnectionFieldPolicy merges paginated pages into one logical list" in {
            val store = connectionStore()
            store.writeOperation(
                FeedQuery(None),
                page(List("c1" -> "1", "c2" -> "2"), "c2", hasNext = true)
            )
            store.writeOperation(
                FeedQuery(Some("c2")),
                page(List("c3" -> "3", "c4" -> "4"), "c4", hasNext = false)
            )

            // Both pages collapsed onto the single connection field key `feed`.
            assert(store.cache.loadRecord("QUERY_ROOT").map(_.fieldKeys) == Present(Set("feed")))

            val merged = store.readOperation(FeedQuery(None))
            assert(merged.feed.edges.map(_.cursor) == List("c1", "c2", "c3", "c4"))
            assert(merged.feed.edges.map(_.node.id) == List("1", "2", "3", "4"))
            // The latest page's pageInfo wins.
            assert(merged.feed.pageInfo == PageInfo("c4", hasNextPage = false))
        }

        "re-fetching an overlapping page does not duplicate edges" in {
            val store = connectionStore()
            store.writeOperation(
                FeedQuery(None),
                page(List("c1" -> "1", "c2" -> "2"), "c2", hasNext = true)
            )
            // Page 2 overlaps c2, then adds c3.
            store.writeOperation(
                FeedQuery(Some("c1")),
                page(List("c2" -> "2", "c3" -> "3"), "c3", hasNext = false)
            )

            val merged = store.readOperation(FeedQuery(None))
            assert(merged.feed.edges.map(_.cursor) == List("c1", "c2", "c3"))
        }

        "without a ConnectionFieldPolicy pages are stored under distinct keys and do not merge" in {
            // A store keyed the same way but with no field policies: each argument set
            // occupies its own connection slot, so pages never combine.
            val store = new ApolloStore(
                MemoryCache(),
                cacheKeyGenerator = TypePolicyCacheKeyGenerator.of(TypePolicy("PostEdge", List("cursor")))
            )
            store.writeOperation(
                FeedQuery(None),
                page(List("c1" -> "1", "c2" -> "2"), "c2", hasNext = true)
            )
            store.writeOperation(
                FeedQuery(Some("c2")),
                page(List("c3" -> "3", "c4" -> "4"), "c4", hasNext = false)
            )

            // Two distinct field keys on the root — the pages are isolated.
            assert(store.cache.loadRecord("QUERY_ROOT").map(_.fieldKeys.size) == Present(2))
            assert(store.readOperation(FeedQuery(None)).feed.edges.map(_.cursor) == List("c1", "c2"))
            assert(
                store.readOperation(FeedQuery(Some("c2"))).feed.edges.map(_.cursor) == List("c3", "c4")
            )
        }
    }
end DeclarativeCacheConfigSpec
