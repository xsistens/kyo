package kyo.apollo.api

import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser

/** Guards the builtin-schema custom-scalar path (codegen `Long=Long` mapping):
  * `ScalarCodec.fromSchema[Long]` must decode a JSON *number* as produced by a
  * Caliban gateway's `Long` scalar. The decode round-trips through
  * `Json.render` → `kyo.Json.decode`, so this pins two properties in the REAL
  * Scala.js runtime (doubles under the hood):
  *
  *   - integral doubles render as plain digits (`1.7e12` → `"1700000000000"`,
  *     never scientific notation, which `decode[Long]` would reject);
  *   - magnitudes through the 2^53 double-exact range survive the parse →
  *     render → decode pipeline losslessly.
  */
class LongScalarCodecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val codec = ScalarCodec.fromSchema[Long]

    "Long scalar codec (fromSchema[Long])" - {

        "decodes a small JSON number (action-log id shape)" in {
            assert(codec.decode(JsonParser.parse("42")) == 42L)
        }

        "decodes a millisecond-timestamp magnitude without scientific notation" in {
            assert(codec.decode(JsonParser.parse("1700000000000")) == 1700000000000L)
        }

        "round-trips the largest double-exact integer (2^53)" in {
            assert(codec.decode(JsonParser.parse("9007199254740992")) == 9007199254740992L)
        }

        "encode emits a JSON number that decodes back" in {
            val encoded = codec.encode(123456789012L)
            assert(encoded == Json.JNum(123456789012L.toDouble))
            assert(codec.decode(encoded) == 123456789012L)
        }
    }
end LongScalarCodecSpec
