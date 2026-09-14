package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Unit tests for the Phase 04 record model: [[Record]] / [[RecordValue]],
  * [[CacheKey]], [[CacheReference]], and [[FieldKey]].
  */
class RecordModelSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- Minimal operations, one per kind, for CacheKey.rootKey ---------------

    final case class RootQuery() extends Query[Int]:
        def name                     = "Q"; def document = "query Q { x }"
        def dataSchema: Schema[Int]  = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end RootQuery

    final case class RootMutation() extends Mutation[Int]:
        def name                     = "M"; def document = "mutation M { x }"
        def dataSchema: Schema[Int]  = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end RootMutation

    final case class RootSubscription() extends Subscription[Int]:
        def name                     = "S"; def document = "subscription S { x }"
        def dataSchema: Schema[Int]  = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Subscription"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end RootSubscription

    // --- FieldKey -------------------------------------------------------------

    private def field(name: String, args: Chunk[CompiledArgument]): CompiledField =
        CompiledField(name, CompiledNamedType("X"), arguments = args)

    "record model" - {

        // --- CacheKey -------------------------------------------------------------

        "rootKey chooses the well-known key per operation kind" in {
            assert(CacheKey.rootKey(RootQuery()) == CacheKey.QueryRoot)
            assert(CacheKey.rootKey(RootMutation()) == CacheKey.MutationRoot)
            assert(CacheKey.rootKey(RootSubscription()) == CacheKey.SubscriptionRoot)
        }

        "root keys have the expected string values" in {
            assert(CacheKey.QueryRoot.render == "QUERY_ROOT")
            assert(CacheKey.MutationRoot.render == "MUTATION_ROOT")
            assert(CacheKey.SubscriptionRoot.render == "SUBSCRIPTION_ROOT")
        }

        "typename/id key renders as Typename:id and toString is the raw key" in {
            val k = CacheKey("Country", "DE")
            assert(k.render == "Country:DE")
            assert(k.toString == "Country:DE")
            assert(CacheKey("PlaybackState", "").render == "PlaybackState")
        }

        // --- opaque keys ------------------------------------------------------------

        "a record key and a field key cannot stand in for each other, nor can a string" in {
            // The well-typed shape compiles — so the failures below are the key types, not the fixture.
            typeCheck("Record(CacheKey.QueryRoot, Map(FieldKey.Typename -> RecordValue.Null))")
            typeCheckFailure("""Record("countries", Map("QUERY_ROOT" -> RecordValue.Null))""")(
                "Required: kyo.apollo.cache.normalized.api.CacheKey"
            )
            typeCheckFailure("Record(fk(\"countries\"), Map(CacheKey.QueryRoot -> RecordValue.Null))")(
                "Required: kyo.apollo.cache.normalized.api.CacheKey"
            )
            val rec = Record(CacheKey.QueryRoot, Map(FieldKey.Typename -> RecordValue.Null))
            typeCheck("rec.get(FieldKey.Typename)")
            typeCheckFailure("rec.get(rec.key)")("Required: kyo.apollo.cache.normalized.api.FieldKey")
        }

        "scalarString is the one scalar-to-id rendering the key generators share" in {
            assert(CacheKey.scalarString(Json.JStr("DE")) == Present("DE"))
            assert(CacheKey.scalarString(Json.JInt(9007199254740993L)) == Present("9007199254740993"))
            assert(CacheKey.scalarString(Json.JBool(true)) == Present("true"))
            assert(CacheKey.scalarString(Json.JNull) == Absent)
            assert(CacheKey.scalarString(Json.JObj(Map("a" -> Json.JInt(1)))) == Absent)
        }

        // --- CacheReference -------------------------------------------------------

        "CacheReference points at a CacheKey" in {
            val key = CacheKey("Book", "42")
            val ref = CacheReference(key)
            assert(ref.key == key)
            assert(ref.toString == "CacheReference(Book:42)")
        }

        // --- RecordValue ----------------------------------------------------------

        "RecordValue.scalar wraps JSON scalars and maps null to Null" in {
            assert(RecordValue.scalar(Json.JStr("hi")) == RecordValue.Scalar(Json.JStr("hi")))
            assert(RecordValue.scalar(Json.JNum(3)) == RecordValue.Scalar(Json.JNum(3)))
            assert(RecordValue.scalar(Json.JBool(true)) == RecordValue.Scalar(Json.JBool(true)))
            assert(RecordValue.scalar(Json.JNull) == RecordValue.Null)
        }

        "RecordValue.scalar rejects composite JSON" in {
            intercept[IllegalArgumentException] {
                RecordValue.scalar(Json.JObj(Map("a" -> Json.JNum(1))))
            }
            val _ = intercept[IllegalArgumentException] {
                RecordValue.scalar(Json.JArr(Chunk(Json.JNum(1))))
            }
        }

        // --- Record ---------------------------------------------------------------

        "Record.references collects references from fields and nested lists" in {
            val refA = CacheReference(CacheKey("Country", "DE"))
            val refB = CacheReference(CacheKey("Country", "FR"))
            val record = Record(
                CacheKey.QueryRoot,
                Map(
                    fk("name") -> RecordValue.scalar(Json.JStr("root")),
                    fk("countries") -> RecordValue.RList(
                        Chunk(RecordValue.Reference(refA), RecordValue.Reference(refB))
                    ),
                    fk("featured") -> RecordValue.Reference(refA)
                )
            )
            assert(record.references == Set(refA, refB))
            assert(record.fieldKeys == Set(fk("name"), fk("countries"), fk("featured")))
            assert(record.get(fk("featured")) == Present(RecordValue.Reference(refA)))
            assert(record.get(fk("missing")) == Absent)
            assert(record.metadata == Map.empty[String, Json])
        }

        "FieldKey with no arguments is just the field name" in {
            assert(FieldKey(field("countries", Chunk.empty)).render == "countries")
        }

        "FieldKey uses the schema name, not the alias" in {
            val aliased = CompiledField(
                "country",
                CompiledNamedType("Country"),
                alias = Present("de")
            )
            assert(FieldKey(aliased).render == "country")
        }

        "distinct literal argument values yield distinct field keys" in {
            val one = field("user", Chunk(CompiledArgument.literal("id", Json.JNum(1))))
            val two = field("user", Chunk(CompiledArgument.literal("id", Json.JNum(2))))
            assert(FieldKey(one).render == """user({"id":1})""")
            assert(FieldKey(two).render == """user({"id":2})""")
            assert(FieldKey(one) != FieldKey(two))
        }

        "FieldKey resolves variable arguments against the variables map" in {
            val f = field("user", Chunk(CompiledArgument.variable("id", "userId")))
            assert(FieldKey(f, Map("userId" -> Json.JNum(7))).render == """user({"id":7})""")
        }

        "FieldKey is canonical: argument order does not change the key" in {
            val ab = field(
                "search",
                Chunk(
                    CompiledArgument.literal("a", Json.JNum(1)),
                    CompiledArgument.literal("b", Json.JNum(2))
                )
            )
            val ba = field(
                "search",
                Chunk(
                    CompiledArgument.literal("b", Json.JNum(2)),
                    CompiledArgument.literal("a", Json.JNum(1))
                )
            )
            assert(FieldKey(ab) == FieldKey(ba))
            assert(FieldKey(ab).render == """search({"a":1,"b":2})""")
        }

        "FieldKey sorts nested object argument keys recursively" in {
            val f = field(
                "search",
                Chunk(
                    CompiledArgument.literal(
                        "filter",
                        Json.JObj(Map("z" -> Json.JNum(1), "a" -> Json.JStr("x")))
                    )
                )
            )
            assert(FieldKey(f).render == """search({"filter":{"a":"x","z":1}})""")
        }
    }
end RecordModelSpec
