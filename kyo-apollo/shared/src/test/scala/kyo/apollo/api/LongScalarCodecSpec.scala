package kyo.apollo.api

import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser

/** Guards the builtin-schema custom-scalar path (codegen `Long=Long` mapping):
  * `ScalarCodec.fromSchema[Long]` must decode a JSON *number* as produced by a
  * Caliban gateway's `Long` scalar, in the REAL Scala.js runtime too.
  *
  * A wire integer parses to [[Json.JInt]] and decodes structurally, so every `Long`
  * survives parse → codec → render byte-identically — including values past the
  * 2^53 range a `Double` holds exactly (snowflake ids).
  */
class LongScalarCodecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val codec = ScalarCodec.fromSchema[Long]

    private def roundTrip(text: String): (Long, String) =
        val decoded = codec.decode(JsonParser.parse(text).getOrThrow).getOrThrow
        (decoded, codec.encode(decoded).render)

    "Long scalar codec (fromSchema[Long])" - {

        "decodes a small JSON number (action-log id shape)" in {
            assert(codec.decode(JsonParser.parse("42").getOrThrow).getOrThrow == 42L)
        }

        "decodes a millisecond-timestamp magnitude without scientific notation" in {
            assert(roundTrip("1700000000000") == (1700000000000L, "1700000000000"))
        }

        "2^53 + 1 survives parse -> codec -> render byte-identically" in {
            assert(JsonParser.parse("9007199254740993").getOrThrow == Json.JInt(9007199254740993L))
            assert(roundTrip("9007199254740993") == (9007199254740993L, "9007199254740993"))
        }

        "Long.MaxValue and Long.MinValue survive parse -> codec -> render byte-identically" in {
            assert(roundTrip("9223372036854775807") == (Long.MaxValue, "9223372036854775807"))
            assert(roundTrip("-9223372036854775808") == (Long.MinValue, "-9223372036854775808"))
        }

        "encode emits a JSON integer that decodes back" in {
            val encoded = codec.encode(123456789012L)
            assert(encoded == Json.JInt(123456789012L))
            assert(codec.decode(encoded).getOrThrow == 123456789012L)
        }
    }
end LongScalarCodecSpec
