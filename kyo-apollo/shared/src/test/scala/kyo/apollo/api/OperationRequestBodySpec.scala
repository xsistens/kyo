package kyo.apollo.api

import kyo.Schema
import kyo.apollo.api.JsonCodec
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap

/** Tests that [[OperationRequestBody]] composes a deterministic wire body and
  * that [[Operation.variables]] carries the operation's variable object (with
  * custom-scalar variables encoded through their `Schema`).
  */
class OperationRequestBodySpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A `DateTime`-like custom scalar used to prove variables are encoded through
      * their `Schema` (a string transform) rather than as a raw value.
      */
    final case class Stamp(iso: String)
    given Schema[Stamp] = Schema.stringSchema.transform[Stamp](Stamp.apply)(_.iso)

    /** Minimal hand-written query with an `Int` variable. */
    final case class MiniQuery(limit: Int) extends Query.Normalizable[Int]:
        def name: String              = "Mini"
        def document: String          = "query Mini($limit: Int!) { x }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json = Json.JObj(
            VectorMap("limit" -> SchemaJson.encode(limit))
        )
    end MiniQuery

    /** Query carrying a custom-scalar variable alongside a built-in one. */
    final case class SearchQuery(limit: Int, after: Stamp) extends Query.Normalizable[Int]:
        def name: String              = "Search"
        def document: String          = "query Search($limit: Int!, $after: DateTime) { y }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json = Json.JObj(
            VectorMap(
                "limit" -> SchemaJson.encode(limit),
                "after" -> SchemaJson.encode(after)
            )
        )
    end SearchQuery

    "OperationRequestBody" - {

        "request body matches expected JSON with deterministic field order" in {
            assert(
                OperationRequestBody.render(MiniQuery(5)) ==
                    """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":5}}"""
            )
        }

        "extensions are omitted entirely when empty" in {
            val body = OperationRequestBody.apply(MiniQuery(1))
            body match
                case Json.JObj(fields) => assert(!fields.contains("extensions"))
                case other             => fail(s"expected an object, got ${other.render}")
        }

        "extensions are appended last when present" in {
            assert(
                OperationRequestBody.render(
                    MiniQuery(1),
                    extensions = Map("tracing" -> Json.JBool(true))
                ) ==
                    """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":1},"extensions":{"tracing":true}}"""
            )
        }

        "custom-scalar variables are encoded through their Schema" in {
            assert(
                OperationRequestBody.render(SearchQuery(3, Stamp("2026-07-10"))) ==
                    """{"query":"query Search($limit: Int!, $after: DateTime) { y }","operationName":"Search","variables":{"limit":3,"after":"2026-07-10"}}"""
            )
        }

        "variables carries the operation's variable object" in {
            assert(MiniQuery(9).variables.render == """{"limit":9}""")
        }
    }
end OperationRequestBodySpec
