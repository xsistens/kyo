package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Present
import kyo.Result
import kyo.Schema
import kyo.apollo.exception.ApolloParseException
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

    final case class Session(token: String, expiresIn: Int) derives Schema

    private val sessionOp: Query[Session] = new Query[Session]:
        def name: String                = "Session"
        def document: String            = "query Session { token expiresIn }"
        def dataSchema: Schema[Session] = summon[Schema[Session]]
        def rootField: CompiledField    = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json             = Json.JObj(VectorMap.empty)

    /** `data` with a credential beside the field that fails to decode, and a second marker in that field. */
    private val sessionData: Json =
        Json.JObj(VectorMap("token" -> Json.JStr("secret-token-123"), "expiresIn" -> Json.JStr("secret-expiry-456")))

    private val sessionEnvelope: Json = Json.JObj(VectorMap("data" -> sessionData))

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

    /** Same operation, but its codec has a defect: decoding throws a non-decode exception. */
    private val defectiveOp: Query[Hero] = new Query[Hero]:
        def name: String             = "Hero"
        def document: String         = "query Hero { name }"
        def dataSchema: Schema[Hero] = summon[Schema[Hero]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
        override def dataCodec: JsonCodec[Hero] = new JsonCodec[Hero]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Hero] = throw new ClassCastException("defective codec")
            def encode(value: Hero): Json                                           = Json.JNull

    private def parseResult(text: String): Result[ApolloParseException, GraphQLResponse[Hero]] =
        JsonParser.parse(text).flatMap(GraphQLResponse.parse(_, heroOp))

    private def parse(text: String): GraphQLResponse[Hero] = parseResult(text).getOrThrow

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
            assert(response.extensions == Map[String, Json]("cost" -> Json.JInt(3)))
        }

        "a non-object envelope is a parse failure value carrying what was read" in {
            parseResult("""[1, 2, 3]""") match
                case Result.Failure(e) =>
                    assert(e.expected == "a GraphQL response object for operation 'Hero'")
                    assert(e.actual == JsonParser.parse("""[1, 2, 3]""").getOrThrow)
                case other => fail(s"expected a parse failure, got $other")
        }

        "a non-object errors entry is a parse failure value" in {
            parseResult("""{ "data": null, "errors": ["boom"] }""") match
                case Result.Failure(e) =>
                    assert(e.expected == "a GraphQL error object")
                    assert(e.actual == Json.JStr("boom"))
                case other => fail(s"expected a parse failure, got $other")
        }

        "data the schema rejects is a parse failure value with the decode error as cause" in {
            parseResult("""{ "data": { "name": 42 } }""") match
                case Result.Failure(e) =>
                    assert(e.expected.contains("Hero"))
                    assert(e.getCause.isInstanceOf[kyo.DecodeException])
                case other => fail(s"expected a parse failure, got $other")
        }

        "a data decode failure's message names the operation and JSON types, never a value of the data" in {
            // `sessionEnvelope` is declared away from this call: a development-mode
            // `getMessage` quotes the source around the construction site.
            GraphQLResponse.parse(sessionEnvelope, sessionOp) match
                case Result.Failure(e) =>
                    Chunk(e.getMessage, e.message, e.toString).foreach { text =>
                        assert(!text.contains("secret-token-123"), text)
                        assert(!text.contains("secret-expiry-456"), text)
                    }
                    assert(e.message == "Expected data matching operation 'Session' but got an object", e.message)
                    assert(e.getMessage.contains("TypeMismatchException"), e.getMessage)
                    assert(e.getCause.isInstanceOf[kyo.DecodeException])
                    assert(e.actual == sessionData)
                case other => fail(s"expected a parse failure, got $other")
            end match
        }

        "a codec defect is a panic, not a parse failure" in {
            GraphQLResponse.parse(JsonParser.parse("""{ "data": { "name": "Luke" } }""").getOrThrow, defectiveOp) match
                case Result.Panic(e) => assert(e.isInstanceOf[ClassCastException])
                case other           => fail(s"expected a panic, got $other")
        }

        "GraphQLError.parse tolerates a missing message and malformed segments" in {
            val error = GraphQLError.parse(
                JsonParser.parse(
                    """{ "path": ["ok", true, 2], "locations": [{ "line": 1 }, { "line": 1, "column": 4 }] }"""
                ).getOrThrow
            ).getOrThrow
            assert(error.message == "")
            // `true` is not a valid path segment and is skipped; strings and ints stay.
            assert(error.path == Chunk[String | Int]("ok", 2))
            // The first (column-less) location is skipped; the well-formed one survives.
            assert(error.locations == Chunk(GraphQLError.Location(1, 4)))
        }
    }
end GraphQLResponseSpec
