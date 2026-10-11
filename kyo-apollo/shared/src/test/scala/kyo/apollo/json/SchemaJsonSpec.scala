package kyo.apollo.json

import kyo.Chunk
import kyo.DecodeException
import kyo.Result
import kyo.Schema
import kyo.Span
import kyo.Structure
import kyo.apollo.exception.ApolloParseException
import scala.collection.immutable.VectorMap

/** The JSON layer converts structurally between typed values, [[Json]] and
  * `Structure.Value`, and keeps every number's precision: a `Long` past 2^53 and a
  * `BigDecimal` with more digits than a `Double` survive encode → store → decode,
  * and parse → render writes the text it read.
  */
class SchemaJsonSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    case class Money(amount: BigDecimal, id: Long) derives Schema

    case class Timed(at: java.time.Instant, span: java.time.Duration, byId: Map[Int, String], byName: Map[String, Int])
        derives Schema

    private val preciseAmount = BigDecimal("0.1000000000000000055511151231257827")
    private val money         = Money(preciseAmount, Long.MaxValue)

    /** Every JSON case, nested, with a field order that is not alphabetical. */
    private val everyCase: Json = Json.JObj(
        VectorMap(
            "z"      -> Json.JNull,
            "bool"   -> Json.JBool(true),
            "int"    -> Json.JInt(9007199254740993L),
            "dec"    -> Json.JDec(preciseAmount),
            "big"    -> Json.JDec(BigDecimal("123456789012345678901234567890")),
            "num"    -> Json.JNum(1.5),
            "str"    -> Json.JStr("a \"quoted\"\nline"),
            "arr"    -> Json.JArr(Chunk(Json.JInt(-1), Json.JNum(-2.25), Json.JStr("x"))),
            "nested" -> Json.JObj(VectorMap("b" -> Json.JInt(Long.MinValue), "a" -> Json.JArr(Chunk.empty)))
        )
    )

    "SchemaJson" - {

        "a Long.MaxValue id and a 34-digit BigDecimal survive encode -> decode" in {
            assert(SchemaJson.decode[Money](SchemaJson.encode(money)) == Result.succeed(money))
        }

        "encode keeps the exact number cases and renders the exact digits" in {
            val encoded = SchemaJson.encode(money)
            assert(encoded == Json.JObj(VectorMap("amount" -> Json.JDec(preciseAmount), "id" -> Json.JInt(Long.MaxValue))))
            assert(encoded.render == """{"amount":0.1000000000000000055511151231257827,"id":9223372036854775807}""")
        }

        "encode -> store as text -> parse -> decode is bit-exact for 2^53 + 1" in {
            val m      = Money(preciseAmount, 9007199254740993L)
            val stored = SchemaJson.encode(m).render
            assert(JsonParser.parse(stored).flatMap(SchemaJson.decode[Money]) == Result.succeed(m))
        }

        "decode of the wrong shape is a failure value carrying kyo's DecodeException" in {
            SchemaJson.decode[Money](Json.JStr("x")) match
                case Result.Failure(e: ApolloParseException) =>
                    assert(e.actual == Json.JStr("x"))
                    assert(e.expected.contains("Money"))
                    assert(e.getCause.isInstanceOf[DecodeException])
                case other => fail(s"expected a parse failure, got $other")
        }

        "decodeString parses and decodes, and fails as a value on malformed text" in {
            assert(SchemaJson.decodeString[Money]("""{"amount":1,"id":2}""") == Result.succeed(Money(BigDecimal(1), 2L)))
            assert(SchemaJson.decodeString[Money]("""{"amount":""").isFailure)
        }

        "instants, durations and maps are written the way kyo-schema's JSON writer writes them" in {
            val timed = Timed(
                java.time.Instant.parse("2026-09-14T10:15:30.123456789Z"),
                java.time.Duration.ofMillis(1500),
                Map(1   -> "one"),
                Map("k" -> 2)
            )
            val encoded = SchemaJson.encode(timed)
            assert(encoded.render == kyo.Json.encode(timed))
            assert(SchemaJson.decode[Timed](encoded) == Result.succeed(timed))
        }
    }

    "JsonParser" - {

        "an integer past 2^53 parses to JInt and renders the same text" in {
            assert(JsonParser.parse("9007199254740993") == Result.succeed(Json.JInt(9007199254740993L)))
            assert(JsonParser.parse("9007199254740993").map(_.render) == Result.succeed("9007199254740993"))
        }

        "a decimal a Double holds is JNum; one it does not hold, or an integer past Long, is JDec" in {
            assert(JsonParser.parse("1.5") == Result.succeed(Json.JNum(1.5)))
            assert(JsonParser.parse("0.1000000000000000055511151231257827") == Result.succeed(Json.JDec(preciseAmount)))
            assert(JsonParser.parse("0.1000000000000000055511151231257827").map(_.render) ==
                Result.succeed("0.1000000000000000055511151231257827"))
            assert(JsonParser.parse("123456789012345678901234567890").map(_.render) ==
                Result.succeed("123456789012345678901234567890"))
        }

        "text that is not JSON is a failure value, never a throw" in {
            Chunk("{", "boom", "-", "1.2.3").foreach { text =>
                JsonParser.parse(text) match
                    case Result.Failure(e: ApolloParseException) =>
                        assert(e.expected == "a JSON document")
                        assert(e.actual == Json.JStr(text))
                    case other => fail(s"expected a parse failure for '$text', got $other")
            }
            succeed
        }

        "fromStructure(toStructure(j)) == j for every JSON case, keeping field order" in {
            val back = JsonParser.fromStructure(JsonParser.toStructure(everyCase))
            assert(back == everyCase)
            assert(back.render == everyCase.render)
            assert(JsonParser.parse(everyCase.render) == Result.succeed(everyCase))
        }

        "toStructure(fromStructure(v)) == v for every value a JSON parse produces" in {
            val value = kyo.Json.decode[Structure.Value](everyCase.render).getOrThrow
            assert(JsonParser.toStructure(JsonParser.fromStructure(value)) == value)
        }

        "an upload placeholder maps to null, as the renderer writes it" in {
            val upload = Json.JUpload(kyo.apollo.Upload(Span.from(Array[Byte](1, 2)), "f.bin", "application/octet-stream"))
            assert(JsonParser.toStructure(upload) == Structure.Value.Null)
        }

        "values outside JSON map to kyo's JSON spelling" in {
            assert(
                JsonParser.fromStructure(Structure.Value.VariantCase(
                    "Circle",
                    Structure.Value.Record(Chunk("r" -> Structure.Value.Integer(1)))
                )) ==
                    Json.JObj(VectorMap("Circle" -> Json.JObj(VectorMap("r" -> Json.JInt(1)))))
            )
            assert(
                JsonParser.fromStructure(Structure.Value.MapEntries(Chunk(Structure.Value.Integer(1) -> Structure.Value.Str("one")))) ==
                    Json.JArr(Chunk(Json.JObj(VectorMap("key" -> Json.JInt(1), "value" -> Json.JStr("one")))))
            )
            assert(JsonParser.fromStructure(Structure.Value.Bytes(Span.from(Array[Byte](1, 2, 3)))) == Json.JStr("AQID"))
            assert(JsonParser.fromStructure(Structure.Value.Decimal(Double.NaN)) == Json.JStr("NaN"))
        }
    }
end SchemaJsonSpec
