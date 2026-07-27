package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import scala.collection.immutable.VectorMap

/** Tests [[GraphQLResponse.parse]] and [[GraphQLError.parse]] across the
  * success, partial-error, and error-only payload shapes, plus the robustness
  * contract (never throws on a shape-valid envelope; top-level `extensions`
  * passthrough; mixed string/int error paths and source locations).
  */
class GraphQLResponseSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Hero(name: String) derives Schema

    /** A minimal operation whose only job here is to carry the `Hero` data
      * `Schema` into [[GraphQLResponse.parse]] (the envelope parsing is what's
      * under test).
      */
    private val heroOp: Query[Hero] = new Query[Hero]:
        def name: String             = "Hero"
        def document: String         = "query Hero { name }"
        def dataSchema: Schema[Hero] = summon[Schema[Hero]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)

    private def parse(text: String): GraphQLResponse[Hero] =
        GraphQLResponse.parse(JsonParser.parse(text), heroOp)

    "GraphQLResponse.parse / GraphQLError.parse" - {

        "success payload → data present, no errors, no extensions" in {
            val response = parse("""{ "data": { "name": "Luke" } }""")
            assert(response.data == Present(Hero("Luke")))
            assert(response.errors == Chunk.empty)
            assert(response.extensions == Map.empty[String, Json])
        }

        "partial-error payload → both data and errors populated" in {
            val response = parse(
                """{
          |  "data": { "name": "Luke" },
          |  "errors": [
          |    {
          |      "message": "name partially unavailable",
          |      "path": ["hero", 0, "name"],
          |      "locations": [{ "line": 2, "column": 5 }],
          |      "extensions": { "code": "UNAVAILABLE" }
          |    }
          |  ]
          |}""".stripMargin
            )
            assert(response.data == Present(Hero("Luke")))
            assert(response.errors.length == 1)
            val error = response.errors.head
            assert(error.message == "name partially unavailable")
            // Mixed field-name/list-index path is preserved as String | Int segments.
            assert(error.path == Chunk[String | Int]("hero", 0, "name"))
            assert(error.locations == Chunk(GraphQLError.Location(2, 5)))
            assert(error.extensions == Map[String, Json]("code" -> Json.JStr("UNAVAILABLE")))
        }

        "error-only payload with data: null → no data, errors present, no throw" in {
            val response = parse(
                """{ "data": null, "errors": [{ "message": "boom" }] }"""
            )
            assert(response.data == Absent)
            assert(response.errors.map(_.message) == Chunk("boom"))
        }

        "absent data → None without throwing" in {
            val response = parse("""{ "errors": [{ "message": "boom" }] }""")
            assert(response.data == Absent)
            assert(response.errors.length == 1)
        }

        "top-level extensions are passed through" in {
            val response = parse(
                """{ "data": { "name": "Luke" }, "extensions": { "cost": 3 } }"""
            )
            assert(response.extensions == Map[String, Json]("cost" -> Json.JNum(3.0)))
        }

        "a non-object envelope throws GraphQLResponseException" in {
            val _ = intercept[GraphQLResponseException](parse("""[1, 2, 3]"""))
        }

        "GraphQLError.parse tolerates a missing message and malformed segments" in {
            val error = GraphQLError.parse(
                JsonParser.parse(
                    """{ "path": ["ok", true, 2], "locations": [{ "line": 1 }, { "line": 1, "column": 4 }] }"""
                )
            )
            assert(error.message == "")
            // `true` is not a valid path segment and is skipped; strings and ints stay.
            assert(error.path == Chunk[String | Int]("ok", 2))
            // The first (column-less) location is skipped; the well-formed one survives.
            assert(error.locations == Chunk(GraphQLError.Location(1, 4)))
        }
    }
end GraphQLResponseSpec
