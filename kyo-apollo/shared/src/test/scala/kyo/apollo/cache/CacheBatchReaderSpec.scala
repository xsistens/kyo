package kyo.apollo.cache

import kyo.Chunk
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.RecordLoader
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
    final case class TestQuery(selections: Chunk[CompiledSelection]) extends Query[Int]:
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
        selections: Chunk[CompiledSelection],
        args: Chunk[CompiledArgument] = Chunk.empty
    ): CompiledField =
        CompiledField(name, CompiledNamedType(typeName), arguments = args, selections = selections)

    private def listOf(field: CompiledField): CompiledField =
        field.copy(fieldType = CompiledListType(field.fieldType))

    private def jstr(s: String): Json = Json.JStr(s)
    private def jnum(n: Double): Json = Json.JNum(n)

    /** Assemble the response `data` map from `records`, starting at `QUERY_ROOT`. */
    private def toData(
        selections: Chunk[CompiledSelection],
        records: Map[CacheKey, Record],
        variables: Map[String, Json] = Map.empty,
        resolver: CacheKeyResolver = CacheKeyResolver.default
    ): Json.JObj =
        new CacheBatchReader(RecordLoader(records), variables, CacheKey.QueryRoot, resolver)
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
                selections = Chunk(obj("book", "Book", Chunk(leaf("__typename"), leaf("id"), leaf("title"))))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end BookQuery

    "CacheBatchReader" - {

        // --- Scalars at the root --------------------------------------------------

        "scalar fields on the root record read back into the data map" in {
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("hello") -> RecordValue.Scalar(jstr("world")))
                )
            )
            assert(toData(Chunk(leaf("hello")), records) == Json.JObj(Map("hello" -> jstr("world"))))
        }

        "a null field reads back as JSON null" in {
            val records =
                Map(CacheKey.QueryRoot -> Record(CacheKey.QueryRoot, Map(fk("hello") -> RecordValue.Null)))
            assert(toData(Chunk(leaf("hello")), records) == Json.JObj(Map("hello" -> Json.JNull)))
        }

        "a scalar list reads back element-wise, order preserved" in {
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(
                        fk("tags") -> RecordValue.RList(
                            Chunk(RecordValue.Scalar(jstr("a")), RecordValue.Scalar(jstr("b")))
                        )
                    )
                )
            )
            assert(
                toData(Chunk(listOf(leaf("tags"))), records) ==
                    Json.JObj(Map("tags" -> Json.JArr(Chunk(jstr("a"), jstr("b")))))
            )
        }

        // --- References → nested objects ------------------------------------------

        "a reference is resolved into its nested object" in {
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("book") -> RecordValue.reference(CacheKey("Book", "42")))
                ),
                CacheKey("Book", "42") -> Record(
                    CacheKey("Book", "42"),
                    Map(
                        fk("__typename") -> RecordValue.Scalar(jstr("Book")),
                        fk("id")         -> RecordValue.Scalar(jstr("42")),
                        fk("title")      -> RecordValue.Scalar(jstr("Dune"))
                    )
                )
            )
            val selections =
                Chunk(obj("book", "Book", Chunk(leaf("__typename"), leaf("id"), leaf("title"))))
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
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(
                        fk("countries") -> RecordValue.RList(
                            Chunk(
                                RecordValue.reference(CacheKey("Country", "DE")),
                                RecordValue.reference(CacheKey("Country", "FR"))
                            )
                        )
                    )
                ),
                CacheKey("Country", "DE") -> Record(
                    CacheKey("Country", "DE"),
                    Map(fk("code") -> RecordValue.Scalar(jstr("DE")))
                ),
                CacheKey("Country", "FR") -> Record(
                    CacheKey("Country", "FR"),
                    Map(fk("code") -> RecordValue.Scalar(jstr("FR")))
                )
            )
            val selections = Chunk(listOf(obj("countries", "Country", Chunk(leaf("code")))))
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
            val fragment = CompiledFragment("Book", Chunk("Book"), Chunk(leaf("title")))
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("node") -> RecordValue.reference(CacheKey("Book", "9")))
                ),
                CacheKey("Book", "9") -> Record(
                    CacheKey("Book", "9"),
                    Map(
                        fk("__typename") -> RecordValue.Scalar(jstr("Book")),
                        fk("id")         -> RecordValue.Scalar(jstr("9")),
                        fk("title")      -> RecordValue.Scalar(jstr("Frag"))
                    )
                )
            )
            val selections = Chunk(obj("node", "Node", Chunk(leaf("__typename"), leaf("id"), fragment)))
            val node       = toData(selections, records).fields("node").asInstanceOf[Json.JObj]
            assert(node.fields.get("title") == Some(jstr("Frag")))
        }

        // --- Cache misses ---------------------------------------------------------

        "a selected field absent from its record raises CacheMissException(key, field)" in {
            val records = Map(
                CacheKey.QueryRoot -> Record(CacheKey.QueryRoot, Map(fk("present") -> RecordValue.Scalar(jstr("x"))))
            )
            val miss = intercept[CacheMissException] {
                toData(Chunk(leaf("present"), leaf("absent")), records)
            }
            assert(miss.key == CacheKey.QueryRoot)
            assert(miss.fieldName == Some("absent"))
        }

        "a reference to an absent record raises CacheMissException on the missing record" in {
            // Parent points at Book:42 but the store never got that record (partial store).
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("book") -> RecordValue.reference(CacheKey("Book", "42")))
                )
            )
            val selections = Chunk(obj("book", "Book", Chunk(leaf("id"))))
            val miss       = intercept[CacheMissException](toData(selections, records))
            assert(miss.key == CacheKey("Book", "42"))
            assert(miss.fieldName == Some("book"))
        }

        "an absent root record raises a whole-record CacheMissException" in {
            val miss = intercept[CacheMissException](toData(Chunk(leaf("hello")), Map.empty))
            assert(miss.key == CacheKey.QueryRoot)
            assert(miss.fieldName == None)
        }

        // --- Cache redirects ------------------------------------------------------

        "a CacheKeyResolver redirect reads a field from another record" in {
            // book(id: "42") is served from the Book:42 record even though the root has
            // no `book(...)` field of its own.
            val records = Map(
                CacheKey.QueryRoot -> Record(CacheKey.QueryRoot, Map.empty),
                CacheKey("Book", "42") -> Record(
                    CacheKey("Book", "42"),
                    Map(fk("id") -> RecordValue.Scalar(jstr("42")), fk("title") -> RecordValue.Scalar(jstr("Dune")))
                )
            )
            val bookById =
                obj(
                    "book",
                    "Book",
                    Chunk(leaf("id"), leaf("title")),
                    args = Chunk(CompiledArgument.literal("id", jstr("42")))
                )
            val data = toData(Chunk(bookById), records, resolver = CacheKeyResolver.byIdArgument())
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
                CacheBatchReader.read(op, RecordLoader(records)) == BookData(
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
            val partial = Normalizer.normalize(op, data) - CacheKey("Book", "42")
            val _ = intercept[CacheMissException](
                CacheBatchReader.read(op, RecordLoader(partial))
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
                CacheBatchReader.readWithDependentKeys(op, RecordLoader(records))
            // Data path is identical to `read`; keys are the root plus the linked Book.
            assert(typed == BookData(Book("42", "Dune")))
            assert(keys == Set(CacheKey.QueryRoot, CacheKey("Book", "42")))
        }

        "normalize then toData reproduces the original data map (Json round-trip)" in {
            val selections = Chunk(
                listOf(obj("countries", "Country", Chunk(leaf("__typename"), leaf("code"), leaf("name"))))
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

        // --- Batching: one load per level of the selection tree ------------------

        "a list of a hundred references is one load, not a hundred" in {
            // P2-26: the reader used to resolve every reference with its own lookup. The
            // root is level one, the hundred items level two — two loads in total.
            val itemKeys = Chunk.from(1 to 100).map(i => CacheKey("Item", i.toString))
            val records = itemKeys.map(k => k -> Record(k, Map(fk("id") -> RecordValue.Scalar(jstr(k.render))))).toMap +
                (CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("items") -> RecordValue.RList(itemKeys.map(RecordValue.reference)))
                ))
            val loader     = CountingLoader(records)
            val selections = Chunk(listOf(obj("items", "Item", Chunk(leaf("id")))))
            val data       = new CacheBatchReader(loader, Map.empty, CacheKey.QueryRoot).toData(TestQuery(selections).rootField)
            assert(data.fields("items").asInstanceOf[Json.JArr].items.size == 100)
            assert(loader.batches == Chunk(1, 100), s"one load per level, got batch sizes ${loader.batches}")
        }

        "each further level of references costs exactly one more load" in {
            // A hundred items, each pointing at its own author: three levels, three loads,
            // for 201 records.
            val itemKeys   = Chunk.from(1 to 100).map(i => CacheKey("Item", i.toString))
            val authorKeys = Chunk.from(1 to 100).map(i => CacheKey("Author", i.toString))
            val items = itemKeys.zip(authorKeys).map((item, author) =>
                item -> Record(item, Map(fk("author") -> RecordValue.reference(author)))
            )
            val authors = authorKeys.map(k => k -> Record(k, Map(fk("name") -> RecordValue.Scalar(jstr(k.render)))))
            val records = (items ++ authors).toMap +
                (CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(fk("items") -> RecordValue.RList(itemKeys.map(RecordValue.reference)))
                ))
            val loader     = CountingLoader(records)
            val selections = Chunk(listOf(obj("items", "Item", Chunk(obj("author", "Author", Chunk(leaf("name")))))))
            val reader     = new CacheBatchReader(loader, Map.empty, CacheKey.QueryRoot)
            val _          = reader.toData(TestQuery(selections).rootField)
            assert(loader.batches == Chunk(1, 100, 100), s"one load per level, got batch sizes ${loader.batches}")
            assert(reader.dependentKeys == Set(CacheKey.QueryRoot) ++ itemKeys ++ authorKeys)
        }

        "a key reached along several paths is loaded once" in {
            // Two list entries and a second field all name Book:1: one level-two load of one key.
            val records = Map(
                CacheKey.QueryRoot -> Record(
                    CacheKey.QueryRoot,
                    Map(
                        fk("books")    -> RecordValue.RList(Chunk.fill(2)(RecordValue.reference(CacheKey("Book", "1")))),
                        fk("featured") -> RecordValue.reference(CacheKey("Book", "1"))
                    )
                ),
                CacheKey("Book", "1") -> Record(CacheKey("Book", "1"), Map(fk("title") -> RecordValue.Scalar(jstr("Dune"))))
            )
            val loader = CountingLoader(records)
            val selections =
                Chunk(listOf(obj("books", "Book", Chunk(leaf("title")))), obj("featured", "Book", Chunk(leaf("title"))))
            val _ = new CacheBatchReader(loader, Map.empty, CacheKey.QueryRoot).toData(TestQuery(selections).rootField)
            assert(loader.batches == Chunk(1, 1))
        }
    }

    /** A loader over fixed records that remembers the size of every batch it served. */
    final private class CountingLoader(records: Map[CacheKey, Record]) extends RecordLoader:
        private var sizes       = Chunk.empty[Int]
        def batches: Chunk[Int] = sizes
        def load(keys: Chunk[CacheKey]): Map[CacheKey, Record] =
            sizes = sizes.append(keys.size)
            RecordLoader(records).load(keys)
    end CountingLoader
end CacheBatchReaderSpec
