package kyo.apollo.cache

import kyo.Chunk
import kyo.Maybe
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.internal.CacheBatchReader
import kyo.apollo.cache.normalized.internal.Normalizer
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit tests for Phase 04 denormalization ([[CacheBatchReader]]): walking an
  * operation's selection tree back over stored [[Record]]s to reassemble a
  * response `data` map and decode it into typed data, with a
  * [[CacheMissException]] on any gap.
  */
class CacheBatchReaderSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- Minimal operations (root key + selection tree) -----------------------

    /** A query whose root selections are supplied per-test; `data` is never
      * decoded through `dataSchema` in the Json-level tests, so `Int` is fine.
      */
    final case class TestQuery(selections: List[CompiledSelection]) extends Query[Int]:
        def name                    = "Q"; def document = "query Q { ... }"
        def dataSchema: Schema[Int] = summon[Schema[Int]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = selections)
        def variables: Json = Json.JObj(VectorMap.empty)
    end TestQuery

    // --- Selection-set builders (mirroring NormalizerSpec) --------------------

    private def leaf(name: String, typeName: String = "String"): CompiledField =
        CompiledField(name, CompiledNamedType(typeName))

    private def obj(
        name: String,
        typeName: String,
        selections: List[CompiledSelection],
        args: List[CompiledArgument] = Nil
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), arguments = args, selections = selections)

    private def listOf(field: CompiledField): CompiledField =
        field.copy(fieldType = CompiledListType(field.fieldType))

    private def jstr(s: String): Json = Json.JStr(s)
    private def jnum(n: Double): Json = Json.JNum(n)

    /** Assemble the response `data` map from `records`, starting at `QUERY_ROOT`. */
    private def toData(
        selections: List[CompiledSelection],
        records: Map[String, Record],
        variables: Map[String, Json] = Map.empty,
        resolver: CacheKeyResolver = CacheKeyResolver.default
    ): Json.JObj =
        new CacheBatchReader(k => Maybe.fromOption(records.get(k)), variables, "QUERY_ROOT", resolver)
            .toData(TestQuery(selections).rootField)

    // --- Typed round-trip through the operation's Adapter ---------------------

    final case class Book(id: String, title: String) derives Schema
    final case class BookData(book: Book) derives Schema

    /** A query for `{ book { __typename id title } }` returning [[BookData]]. The
      * stored `__typename` is an unknown field to `Book` and is ignored on decode.
      */
    final case class BookQuery() extends Query[BookData]:
        def name                         = "BookQuery"; def document = "query BookQuery { book { __typename id title } }"
        def dataSchema: Schema[BookData] = summon[Schema[BookData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(obj("book", "Book", List(leaf("__typename"), leaf("id"), leaf("title"))))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end BookQuery

    "CacheBatchReader" - {

        // --- Scalars at the root --------------------------------------------------

        "scalar fields on the root record read back into the data map" in {
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map("hello" -> RecordValue.Scalar(jstr("world")))
                )
            )
            assert(toData(List(leaf("hello")), records) == Json.JObj(Map("hello" -> jstr("world"))))
        }

        "a null field reads back as JSON null" in {
            val records =
                Map("QUERY_ROOT" -> Record(CacheKey.QueryRoot, Map("hello" -> RecordValue.Null)))
            assert(toData(List(leaf("hello")), records) == Json.JObj(Map("hello" -> Json.JNull)))
        }

        "a scalar list reads back element-wise, order preserved" in {
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map(
                        "tags" -> RecordValue.RList(
                            Chunk(RecordValue.Scalar(jstr("a")), RecordValue.Scalar(jstr("b")))
                        )
                    )
                )
            )
            assert(
                toData(List(listOf(leaf("tags"))), records) ==
                    Json.JObj(Map("tags" -> Json.JArr(Chunk(jstr("a"), jstr("b")))))
            )
        }

        // --- References → nested objects ------------------------------------------

        "a reference is resolved into its nested object" in {
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map("book" -> RecordValue.reference(CacheKey("Book", "42")))
                ),
                "Book:42" -> Record(
                    CacheKey("Book", "42"),
                    Map(
                        "__typename" -> RecordValue.Scalar(jstr("Book")),
                        "id"         -> RecordValue.Scalar(jstr("42")),
                        "title"      -> RecordValue.Scalar(jstr("Dune"))
                    )
                )
            )
            val selections =
                List(obj("book", "Book", List(leaf("__typename"), leaf("id"), leaf("title"))))
            assert(
                toData(selections, records) ==
                    Json.JObj(
                        Map(
                            "book" -> Json.JObj(
                                Map("__typename" -> jstr("Book"), "id" -> jstr("42"), "title" -> jstr("Dune"))
                            )
                        )
                    )
            )
        }

        "a list of references is resolved into a list of nested objects" in {
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map(
                        "countries" -> RecordValue.RList(
                            Chunk(
                                RecordValue.reference(CacheKey("Country", "DE")),
                                RecordValue.reference(CacheKey("Country", "FR"))
                            )
                        )
                    )
                ),
                "Country:DE" -> Record(
                    CacheKey("Country", "DE"),
                    Map("code" -> RecordValue.Scalar(jstr("DE")))
                ),
                "Country:FR" -> Record(
                    CacheKey("Country", "FR"),
                    Map("code" -> RecordValue.Scalar(jstr("FR")))
                )
            )
            val selections = List(listOf(obj("countries", "Country", List(leaf("code")))))
            assert(
                toData(selections, records) ==
                    Json.JObj(
                        Map(
                            "countries" -> Json.JArr(
                                Chunk(
                                    Json.JObj(Map("code" -> jstr("DE"))),
                                    Json.JObj(Map("code" -> jstr("FR")))
                                )
                            )
                        )
                    )
            )
        }

        // --- Inline fragments (resolved via the stored __typename) ----------------

        "a matching inline fragment's fields are read when the record's __typename matches" in {
            val fragment = CompiledFragment("Book", List("Book"), List(leaf("title")))
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map("node" -> RecordValue.reference(CacheKey("Book", "9")))
                ),
                "Book:9" -> Record(
                    CacheKey("Book", "9"),
                    Map(
                        "__typename" -> RecordValue.Scalar(jstr("Book")),
                        "id"         -> RecordValue.Scalar(jstr("9")),
                        "title"      -> RecordValue.Scalar(jstr("Frag"))
                    )
                )
            )
            val selections = List(obj("node", "Node", List(leaf("__typename"), leaf("id"), fragment)))
            val node       = toData(selections, records).fields("node").asInstanceOf[Json.JObj]
            assert(node.fields.get("title") == Some(jstr("Frag")))
        }

        // --- Cache misses ---------------------------------------------------------

        "a selected field absent from its record raises CacheMissException(key, field)" in {
            val records = Map(
                "QUERY_ROOT" -> Record(CacheKey.QueryRoot, Map("present" -> RecordValue.Scalar(jstr("x"))))
            )
            val miss = intercept[CacheMissException] {
                toData(List(leaf("present"), leaf("absent")), records)
            }
            assert(miss.key == "QUERY_ROOT")
            assert(miss.fieldName == Some("absent"))
        }

        "a reference to an absent record raises CacheMissException on the missing record" in {
            // Parent points at Book:42 but the store never got that record (partial store).
            val records = Map(
                "QUERY_ROOT" -> Record(
                    CacheKey.QueryRoot,
                    Map("book" -> RecordValue.reference(CacheKey("Book", "42")))
                )
            )
            val selections = List(obj("book", "Book", List(leaf("id"))))
            val miss       = intercept[CacheMissException](toData(selections, records))
            assert(miss.key == "Book:42")
            assert(miss.fieldName == Some("book"))
        }

        "an absent root record raises a whole-record CacheMissException" in {
            val miss = intercept[CacheMissException](toData(List(leaf("hello")), Map.empty))
            assert(miss.key == "QUERY_ROOT")
            assert(miss.fieldName == None)
        }

        // --- Cache redirects ------------------------------------------------------

        "a CacheKeyResolver redirect reads a field from another record" in {
            // book(id: "42") is served from the Book:42 record even though the root has
            // no `book(...)` field of its own.
            val records = Map(
                "QUERY_ROOT" -> Record(CacheKey.QueryRoot, Map.empty),
                "Book:42" -> Record(
                    CacheKey("Book", "42"),
                    Map("id" -> RecordValue.Scalar(jstr("42")), "title" -> RecordValue.Scalar(jstr("Dune")))
                )
            )
            val bookById =
                obj(
                    "book",
                    "Book",
                    List(leaf("id"), leaf("title")),
                    args = List(CompiledArgument.literal("id", jstr("42")))
                )
            val data = toData(List(bookById), records, resolver = CacheKeyResolver.byIdArgument())
            val book = data.fields(bookById.responseName).asInstanceOf[Json.JObj]
            assert(book.fields("title") == jstr("Dune"))
        }

        "normalize then read returns typed data equal to the original response" in {
            val op = BookQuery()
            val data = Map(
                "book" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("42"), "title" -> jstr("Dune"))
                )
            )
            val records = Normalizer.normalize(op, data)
            // Round-trip: the linked records reassemble into the same typed value.
            assert(
                CacheBatchReader.read(op, k => Maybe.fromOption(records.get(k))) == BookData(
                    Book("42", "Dune")
                )
            )
        }

        "reading a partial store through the typed path raises CacheMissException" in {
            val op = BookQuery()
            val data = Map(
                "book" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("42"), "title" -> jstr("Dune"))
                )
            )
            // Drop the Book record so the reference dangles.
            val partial = Normalizer.normalize(op, data) - "Book:42"
            val _ = intercept[CacheMissException](
                CacheBatchReader.read(op, k => Maybe.fromOption(partial.get(k)))
            )
        }

        "readWithDependentKeys captures the root and every reference target visited" in {
            val op = BookQuery()
            val data = Map(
                "book" -> Json.JObj(
                    Map("__typename" -> jstr("Book"), "id" -> jstr("42"), "title" -> jstr("Dune"))
                )
            )
            val records = Normalizer.normalize(op, data)
            val (typed, keys) =
                CacheBatchReader.readWithDependentKeys(op, k => Maybe.fromOption(records.get(k)))
            // Data path is identical to `read`; keys are the root plus the linked Book.
            assert(typed == BookData(Book("42", "Dune")))
            assert(keys == Set("QUERY_ROOT", "Book:42"))
        }

        "normalize then toData reproduces the original data map (Json round-trip)" in {
            val selections = List(
                listOf(obj("countries", "Country", List(leaf("__typename"), leaf("code"), leaf("name"))))
            )
            val op = TestQuery(selections)
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
            val records =
                Normalizer.normalize(op, data, cacheKeyGenerator = IdCacheKeyGenerator(List("code")))
            assert(toData(selections, records) == Json.JObj(data))
        }
    }
end CacheBatchReaderSpec
