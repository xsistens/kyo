package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.internal.Normalizer
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit tests for Phase 04 normalization ([[Normalizer]]): walking an operation's
  * selection tree over a response `data` map to produce flat, linked
  * [[Record]]s.
  */
class NormalizerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- Minimal operations for the root key ----------------------------------

    /** A query whose root selections are supplied per-test; its `data` codec is
      * never exercised here (normalization only reads `rootField`/`variables`).
      */
    final case class TestQuery(selections: Chunk[CompiledSelection]) extends Query.Normalizable[Int]:
        def name                      = "Q"; def document = "query Q { ... }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = selections)
        def variables: Json = Json.JObj(VectorMap.empty)
    end TestQuery

    // --- Selection-set builders -----------------------------------------------

    private def leaf(name: String, typeName: String = "String"): CompiledField =
        CompiledField(name, CompiledNamedType(typeName))

    private def obj(
        name: String,
        typeName: String,
        selections: Chunk[CompiledSelection],
        args: Chunk[CompiledArgument] = Chunk.empty
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), arguments = args, selections = selections)

    /** Wrap a named field type as a list, preserving the leaf name for keying. */
    private def listOf(field: CompiledField): CompiledField =
        field.copy(fieldType = CompiledListType(field.fieldType))

    private def jstr(s: String): Json = Json.JStr(s)
    private def jnum(n: Double): Json = Json.JNum(n)

    /** The root record's expected fields: the test's own plus the `__typename`
      * the Normalizer stamps onto the root (so type-scoped field policies can be
      * resolved for root fields at merge time).
      */
    private def rootFields(fields: (FieldKey, RecordValue)*): Map[FieldKey, RecordValue] =
        Map(FieldKey.Typename -> RecordValue.Scalar(jstr("Query"))) ++ fields

    private def normalize(
        selections: Chunk[CompiledSelection],
        data: Map[String, Json],
        variables: Map[String, Json] = Map.empty,
        generator: CacheKeyGenerator = CacheKeyGenerator.default
    ): Map[CacheKey, Record] =
        Normalizer.normalize(TestQuery(selections), data, variables, generator)

    "Normalizer" - {

        // --- Scalars at the root --------------------------------------------------

        "scalar fields stay inline on the root record" in {
            val records = normalize(
                selections = Chunk(leaf("hello")),
                data = Map("hello" -> jstr("world"))
            )
            assert(records.keySet == Set(CacheKey.QueryRoot))
            assert(
                records(CacheKey.QueryRoot).fields ==
                    rootFields(fk("hello") -> RecordValue.Scalar(jstr("world")))
            )
        }

        "a null scalar is stored as RecordValue.Null" in {
            val records = normalize(
                selections = Chunk(leaf("hello")),
                data = Map("hello" -> Json.JNull)
            )
            assert(records(CacheKey.QueryRoot).fields == rootFields(fk("hello") -> RecordValue.Null))
        }

        "a scalar list stays inline as a list of scalars" in {
            val records = normalize(
                selections = Chunk(listOf(leaf("tags"))),
                data = Map("tags" -> Json.JArr(Chunk(jstr("a"), jstr("b"))))
            )
            assert(
                records(CacheKey.QueryRoot).fields ==
                    rootFields(
                        fk("tags") -> RecordValue.RList(
                            Chunk(RecordValue.Scalar(jstr("a")), RecordValue.Scalar(jstr("b")))
                        )
                    )
            )
        }

        // --- Nested objects → separate records ------------------------------------

        "a nested object with an id becomes a separate record linked by reference" in {
            val selections = Chunk(
                obj("book", "Book", Chunk(leaf("__typename"), leaf("id"), leaf("title")))
            )
            val data = Map(
                "book" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("42"), "title" -> jstr("Dune"))
                )
            )
            val records = normalize(selections, data)

            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Book", "42")))
            // Parent holds only a reference to the child.
            assert(
                records(CacheKey.QueryRoot).fields ==
                    rootFields(fk("book") -> RecordValue.Reference(CacheReference(CacheKey("Book", "42"))))
            )
            // Child record holds the object's own scalar fields.
            assert(
                records(CacheKey("Book", "42")).fields ==
                    Map(
                        fk("__typename") -> RecordValue.Scalar(jstr("Book")),
                        fk("id")         -> RecordValue.Scalar(jstr("42")),
                        fk("title")      -> RecordValue.Scalar(jstr("Dune"))
                    )
            )
        }

        "an id-less nested object is keyed by its response path" in {
            val selections = Chunk(obj("stats", "Stats", Chunk(leaf("views", "Int"))))
            val data       = Map("stats" -> Json.JObj(Map("views" -> jnum(10))))
            val records    = normalize(selections, data)

            assert(records.keySet == Set(CacheKey.QueryRoot, pathKey("QUERY_ROOT", "stats")))
            assert(
                records(CacheKey.QueryRoot).fields ==
                    rootFields(fk("stats") -> RecordValue.Reference(CacheReference(pathKey("QUERY_ROOT", "stats"))))
            )
            // The static-typename stamp is stored even on a path-keyed record, so the
            // read side's implicit `__typename` selection is satisfied.
            assert(
                records(pathKey("QUERY_ROOT", "stats")).fields ==
                    Map(
                        fk("views")      -> RecordValue.Scalar(jnum(10)),
                        fk("__typename") -> RecordValue.Scalar(jstr("Stats"))
                    )
            )
        }

        // --- Lists of objects → lists of references -------------------------------

        "a list of objects becomes a list of references to per-element records" in {
            val country =
                obj("countries", "Country", Chunk(leaf("__typename"), leaf("code"), leaf("name")))
            val selections = Chunk(listOf(country))
            val data = Map(
                "countries" -> Json.JArr(
                    Chunk(
                        Json.JObj(
                            Map("__typename" -> jstr("Country"), "code" -> jstr("DE"), "name" -> jstr("Germany"))
                        ),
                        Json.JObj(
                            Map("__typename" -> jstr("Country"), "code" -> jstr("FR"), "name" -> jstr("France"))
                        )
                    )
                )
            )
            // key countries by `code`
            val records = normalize(selections, data, generator = IdCacheKeyGenerator(List("code")))

            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Country", "DE"), CacheKey("Country", "FR")))
            assert(
                records(CacheKey.QueryRoot).fields ==
                    rootFields(
                        fk("countries") -> RecordValue.RList(
                            Chunk(
                                RecordValue.Reference(CacheReference(CacheKey("Country", "DE"))),
                                RecordValue.Reference(CacheReference(CacheKey("Country", "FR")))
                            )
                        )
                    )
            )
            assert(records(CacheKey("Country", "DE")).get(fk("name")) == Present(RecordValue.Scalar(jstr("Germany"))))
            assert(records(CacheKey("Country", "FR")).get(fk("name")) == Present(RecordValue.Scalar(jstr("France"))))
        }

        "an id-less list of objects keys each element by its indexed path" in {
            val item = obj("items", "Item", Chunk(leaf("label")))
            val records = normalize(
                selections = Chunk(listOf(item)),
                data = Map(
                    "items" -> Json.JArr(
                        Chunk(Json.JObj(Map("label" -> jstr("x"))), Json.JObj(Map("label" -> jstr("y"))))
                    )
                )
            )
            assert(records.keySet == Set(
                CacheKey.QueryRoot,
                pathKey("QUERY_ROOT", "items", "0"),
                pathKey("QUERY_ROOT", "items", "1")
            ))
            assert(records(pathKey("QUERY_ROOT", "items", "0")).get(fk("label")) == Present(RecordValue.Scalar(jstr("x"))))
            assert(records(pathKey("QUERY_ROOT", "items", "1")).get(fk("label")) == Present(RecordValue.Scalar(jstr("y"))))
        }

        // --- Where an id-less object's path is rooted -----------------------------

        "an id-less child of an identified parent is keyed under the parent, not the operation root" in {
            // The whole of F-18: were the key `QUERY_ROOT.album.cover`, a second operation
            // reaching the same album by another route would mint a SECOND record for one
            // logical field, and the narrower of the two writers would win the parent's
            // pointer while missing the other's fields.
            val cover = obj("cover", "Image", Chunk(leaf("url")))
            val album = obj("album", "Album", Chunk(leaf("__typename"), leaf("id"), cover))
            val records = normalize(
                selections = Chunk(album),
                data = Map(
                    "album" -> Json.JObj(Map(
                        "__typename" -> jstr("Album"),
                        "id"         -> jstr("1"),
                        "cover"      -> Json.JObj(Map("url" -> jstr("u")))
                    ))
                )
            )
            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Album", "1"), pathKey("Album:1", "cover")))
            assert(records(CacheKey(
                "Album",
                "1"
            )).get(fk("cover")) == Present(RecordValue.Reference(CacheReference(pathKey("Album:1", "cover")))))
        }

        "an id-less list under an identified parent indexes below the parent's key" in {
            val images = listOf(obj("images", "Image", Chunk(leaf("url"))))
            val album  = obj("album", "Album", Chunk(leaf("__typename"), leaf("id"), images))
            val records = normalize(
                selections = Chunk(album),
                data = Map(
                    "album" -> Json.JObj(Map(
                        "__typename" -> jstr("Album"),
                        "id"         -> jstr("1"),
                        "images" -> Json.JArr(
                            Chunk(Json.JObj(Map("url" -> jstr("a"))), Json.JObj(Map("url" -> jstr("b"))))
                        )
                    ))
                )
            )
            assert(records.keySet == Set(
                CacheKey.QueryRoot,
                CacheKey("Album", "1"),
                pathKey("Album:1", "images", "0"),
                pathKey("Album:1", "images", "1")
            ))
        }

        "a path-keyed object's own children keep their flat path key" in {
            // The idempotence half of the rule, and the reason rerooting changed no existing
            // expectation: with no identity anywhere, restarting the path at the child's own
            // path key renders the identical string.
            val breakdown = obj("breakdown", "Breakdown", Chunk(leaf("views")))
            val stats     = obj("stats", "Stats", Chunk(breakdown))
            val records = normalize(
                selections = Chunk(stats),
                data = Map(
                    "stats" -> Json.JObj(Map("breakdown" -> Json.JObj(Map("views" -> jnum(3)))))
                )
            )
            assert(records.keySet == Set(
                CacheKey.QueryRoot,
                pathKey("QUERY_ROOT", "stats"),
                pathKey("QUERY_ROOT", "stats", "breakdown")
            ))
        }

        // --- Deduplication / merge within one response ----------------------------

        "the same entity referenced twice merges into one record with unioned fields" in {
            // Two sibling fields select the same Book:1, each requesting a different scalar.
            val selections = Chunk(
                obj("primary", "Book", Chunk(leaf("__typename"), leaf("id"), leaf("title"))),
                obj("secondary", "Book", Chunk(leaf("__typename"), leaf("id"), leaf("author")))
            )
            val data = Map(
                "primary" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("1"), "title" -> jstr("A"))
                ),
                "secondary" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("1"), "author" -> jstr("B"))
                )
            )
            val records = normalize(selections, data)

            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Book", "1")))
            // Merged record carries fields from both occurrences.
            assert(
                records(CacheKey("Book", "1")).fields ==
                    Map(
                        fk("__typename") -> RecordValue.Scalar(jstr("Book")),
                        fk("id")         -> RecordValue.Scalar(jstr("1")),
                        fk("title")      -> RecordValue.Scalar(jstr("A")),
                        fk("author")     -> RecordValue.Scalar(jstr("B"))
                    )
            )
            assert(records(CacheKey.QueryRoot).fields.keySet == Set("__typename", "primary", "secondary"))
        }

        // --- Argument-aware field keys --------------------------------------------

        "arguments distinguish two selections of the same field" in {
            val userById = (id: Int) =>
                obj(
                    "user",
                    "User",
                    Chunk(leaf("__typename"), leaf("id"), leaf("name")),
                    args = Chunk(CompiledArgument.literal("id", jnum(id)))
                )
            // Same response name `user`, different args → distinct field keys on the root.
            val selections =
                Chunk(userById(1).copy(alias = Present("u1")), userById(2).copy(alias = Present("u2")))
            val data = Map(
                "u1" -> Json.JObj(
                    Map("__typename" -> jstr("User"), "id" -> jstr("1"), "name" -> jstr("Ann"))
                ),
                "u2" -> Json.JObj(
                    Map("__typename" -> jstr("User"), "id" -> jstr("2"), "name" -> jstr("Bo"))
                )
            )
            val records = normalize(selections, data)

            assert(
                records(CacheKey.QueryRoot).fields.keySet ==
                    Set("__typename", "user({\"id\":1})", "user({\"id\":2})")
            )
            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("User", "1"), CacheKey("User", "2")))
        }

        // --- __typename handling --------------------------------------------------

        "__typename is collected implicitly even when not explicitly selected" in {
            // The selection set does NOT list __typename, but the server sends it.
            val selections = Chunk(obj("book", "Book", Chunk(leaf("id"), leaf("title"))))
            val data = Map(
                "book" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("7"), "title" -> jstr("T"))
                )
            )
            val records = normalize(selections, data)

            // The implicit __typename let the id key be computed, and it is stored.
            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Book", "7")))
            assert(records(CacheKey("Book", "7")).get(fk("__typename")) == Present(RecordValue.Scalar(jstr("Book"))))
        }

        "without __typename in the response the static field type still keys the entity" in {
            // `addTypename` semantics at the store boundary: an object arriving
            // without `__typename` (e.g. a `mapInto` write-back re-encode) is stamped
            // with the field's static leaf type, so an id-carrying object is keyed as
            // an entity — never re-keyed under the writing operation's root path.
            val selections = Chunk(obj("book", "Book", Chunk(leaf("id"), leaf("title"))))
            val data       = Map("book" -> Json.JObj(Map("id" -> jstr("7"), "title" -> jstr("T"))))
            val records    = normalize(selections, data)
            assert(records.keySet == Set(CacheKey.QueryRoot, CacheKey("Book", "7")))
            assert(records(CacheKey("Book", "7")).get(fk("__typename")) == Present(RecordValue.Scalar(jstr("Book"))))
        }

        // --- Inline fragments -----------------------------------------------------

        "a matching inline fragment's fields are spliced into the object record" in {
            val fragment = CompiledFragment(
                typeCondition = "Book",
                possibleTypes = Chunk("Book"),
                selections = Chunk(leaf("title"))
            )
            val selections = Chunk(
                obj("node", "Node", Chunk(leaf("__typename"), leaf("id"), fragment))
            )
            val data = Map(
                "node" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("9"), "title" -> jstr("Frag"))
                )
            )
            val records = normalize(selections, data)
            assert(records(CacheKey("Book", "9")).get(fk("title")) == Present(RecordValue.Scalar(jstr("Frag"))))
        }

        "a non-matching inline fragment contributes no fields" in {
            val fragment = CompiledFragment(
                typeCondition = "Magazine",
                possibleTypes = Chunk("Magazine"),
                selections = Chunk(leaf("issue"))
            )
            val selections = Chunk(
                obj("node", "Node", Chunk(leaf("__typename"), leaf("id"), fragment))
            )
            val data = Map(
                "node" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("9"), "issue" -> jstr("should-be-ignored"))
                )
            )
            val records = normalize(selections, data)
            assert(records(CacheKey("Book", "9")).fieldKeys == Set("__typename", "id"))
        }

        // --- Absent fields --------------------------------------------------------

        "a selected field missing from the response is simply not stored" in {
            val selections = Chunk(leaf("present"), leaf("absent"))
            val records    = normalize(selections, Map("present" -> jstr("here")))
            assert(records(CacheKey.QueryRoot).fieldKeys == Set("__typename", "present"))
        }
    }
end NormalizerSpec
