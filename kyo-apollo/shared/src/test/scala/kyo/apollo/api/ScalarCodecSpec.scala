package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.apollo.ClientField
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json

/** The leaf codecs decode to a `Result`: a response value of the wrong JSON type is an
  * `ApolloParseException` failure naming the type expected and the JSON type found,
  * never the value. Every decode below returns its failure; a codec that raised
  * instead would abort the leaf.
  */
class ScalarCodecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // The marked values live in `ScalarLeak`, away from these lines: a development-mode
    // `getMessage` quotes the source around the failure's frame, which is a call below.
    import ScalarLeak.*

    /** The failure `result` holds, after checking that no text of it, or of an apollo
      * parse exception it carries as cause, holds a marked value. A schema-backed leaf's
      * cause is kyo-schema's own `DecodeException`, whose message quotes the value it
      * rejected; the parse exception's text names only that cause's class.
      */
    private def failureOf[A](result: Result[ApolloParseException, A])(using Frame, kyo.test.AssertScope): ApolloParseException =
        result match
            case Result.Failure(e) =>
                val cause = Maybe(e.getCause).collect { case c: ApolloParseException => c }
                val texts = Chunk(e.message, e.getMessage, e.toString) ++
                    cause.fold(Chunk.empty[String])(c => Chunk(c.message, c.getMessage, c.toString))
                texts.foreach(text => markers.foreach(marker => assert(!text.contains(marker), text)))
                e
            case other => fail(s"expected a parse failure value, got $other")

    sealed trait Counter
    given TypeName[Counter] = TypeName("Counter")

    "ScalarCodec" - {

        "decode returns a Result, not the value" in {
            typeCheck(
                "val r: kyo.Result[kyo.apollo.exception.ApolloParseException, Int] = ScalarCodec.int.decode(kyo.apollo.json.Json.JInt(1L))"
            )
            typeCheckFailure("val n: Int = ScalarCodec.int.decode(kyo.apollo.json.Json.JInt(1L))")("Required: Int")
        }

        "a value of the wrong JSON type is a failure naming the GraphQL type and the JSON type found, never the value" in {
            val cases: Chunk[(Result[ApolloParseException, Any], String)] = Chunk(
                ScalarCodec.int.decode(text)                                       -> "Expected a GraphQL Int but got a string",
                ScalarCodec.int.decode(tooLarge)                                   -> "Expected a GraphQL Int but got a number",
                ScalarCodec.double.decode(text)                                    -> "Expected a GraphQL Float but got a string",
                ScalarCodec.string.decode(number)                                  -> "Expected a GraphQL String but got a number",
                ScalarCodec.id.decode(row)                                         -> "Expected a GraphQL String but got an object",
                ScalarCodec.boolean.decode(text)                                   -> "Expected a GraphQL Boolean but got a string",
                ScalarCodec.maybe(ScalarCodec.int).decode(text)                    -> "Expected a GraphQL Int but got a string",
                ScalarCodec.chunk(ScalarCodec.string).decode(row)                  -> "Expected a GraphQL list but got an object",
                ScalarCodec.chunk(ScalarCodec.maybe(ScalarCodec.int)).decode(list) -> "Expected a GraphQL Int but got a string",
                ScalarCodec.upload.decode(
                    text
                ) -> "Expected a GraphQL Upload, which is input-only and never in a response, but got a string",
                ScalarCodec.fromSchema[Long].decode(text) -> "Expected a value of Long but got a string"
            )
            cases.foreach { (result, expected) =>
                val failure = failureOf(result)
                assert(failure.message == expected, failure.message)
            }
        }

        "values of the right JSON type decode, through maybe and chunk" in {
            assert(ScalarCodec.int.decode(Json.JNum(1.0)) == Result.succeed(1))
            assert(ScalarCodec.double.decode(Json.JInt(2L)) == Result.succeed(2.0))
            assert(
                ScalarCodec.chunk(ScalarCodec.maybe(ScalarCodec.int)).decode(Json.JArr(Chunk(Json.JInt(1L), Json.JNull))) ==
                    Result.succeed(Chunk(Present(1), Absent))
            )
            assert(ScalarCodec.fromSchema[Long].decode(Json.JInt(Long.MaxValue)) == Result.succeed(Long.MaxValue))
        }

        "a wrong scalar in a response" - {

            val count = SelectionBuilder.scalar[RootQuery, (count: Int), Int]("count", CompiledNamedType("Int").notNull, ScalarCodec.int)
            val tags = SelectionBuilder.scalar[RootQuery, (tags: Chunk[String]), Chunk[String]](
                "tags",
                CompiledNamedType("String").notNull.list.notNull,
                ScalarCodec.chunk(ScalarCodec.string)
            )

            "fails the operation's decode with a value whose cause names the types" in {
                val query    = (count ~ tags).toQuery("Counts")
                val response = Json.JObj(Map("data" -> Json.JObj(Map("count" -> text, "tags" -> Json.JArr(Chunk.empty)))))
                val failure  = failureOf(GraphQLResponse.parse(response, query))
                assert(failure.message == "Expected data matching operation 'Counts' but got an object", failure.message)
                failure.getCause match
                    case cause: ApolloParseException =>
                        assert(cause.message == "Expected a GraphQL Int but got a string", cause.message)
                    case other => fail(s"expected the leaf failure as cause, got $other")
                end match
            }

            "fails a list of the wrong element type at the first wrong element" in {
                val response = Json.JObj(Map("count" -> Json.JInt(1L), "tags" -> Json.JArr(Chunk(Json.JStr("a"), number))))
                assert(failureOf((count ~ tags).decode(response)).message == "Expected a GraphQL String but got a number")
            }

            "fails a @client field's schema decode the same way" in {
                val clicks  = ClientField.create[Counter, Int]("clicks", default = 0)
                val failure = failureOf(clicks.select.decode(Json.JObj(Map("clicks" -> text))))
                assert(failure.message == "Expected a value of Int but got a string", failure.message)
                assert(clicks.select.decode(Json.JObj(Map("clicks" -> Json.JNull))) == Result.succeed(Tuple1(0)))
            }
        }
    }
end ScalarCodecSpec

/** Response values that must never reach a leaf failure's text. */
private object ScalarLeak:
    val markers: Chunk[String] = Chunk("secret-scalar-4711", "47114711471", "8642")

    val text: Json     = Json.JStr("secret-scalar-4711")
    val tooLarge: Json = Json.JInt(47114711471L)
    val number: Json   = Json.JInt(8642L)
    val row: Json      = Json.JObj(Map("token" -> Json.JStr("secret-scalar-4711")))
    val list: Json     = Json.JArr(Chunk(Json.JInt(1L), Json.JNull, Json.JStr("secret-scalar-4711")))
end ScalarLeak
