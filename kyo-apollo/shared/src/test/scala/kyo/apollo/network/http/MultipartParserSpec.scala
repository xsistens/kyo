package kyo.apollo.network.http

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.json.Json

/** Unit tests for [[MultipartParser]]: boundary extraction, and splitting a
  * `multipart/mixed` incremental-delivery body into parts both when fully buffered
  * and when it arrives split across stream chunks (mid-delimiter).
  */
class MultipartParserSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val boundary = "-"

    /** A two-part incremental body: an initial payload then one `@defer` patch. */
    private val body =
        s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"data":{"a":1},"hasNext":true}""" + s"\r\n--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"incremental":[{"data":{"b":2},"path":[]}],"hasNext":false}""" +
            s"\r\n--$boundary--\r\n"

    private def field(part: MultipartPart, key: String): Option[Json] =
        part.json match
            case Json.JObj(fields) => fields.get(key)
            case _                 => None

    "MultipartParser.boundaryOf" - {
        "parses the boundary parameter, stripping quotes" in {
            assert(
                MultipartParser.boundaryOf("multipart/mixed; boundary=\"-\"; deferSpec=20220824") == "-"
            )
            assert(
                MultipartParser.boundaryOf(
                    "multipart/mixed; boundary=gc0p4Jq0M2Yt08jU534c0p"
                ) == "gc0p4Jq0M2Yt08jU534c0p"
            )
        }
        "defaults to `-` when no boundary parameter is present" in {
            assert(MultipartParser.boundaryOf("multipart/mixed") == "-")
        }
    }

    "MultipartParser.parts (buffered)" - {
        "splits a whole body into its parts, dropping the terminator" in {
            val parts = MultipartParser.parts(boundary, body)
            assert(parts.size == 2)
            assert(field(parts(0), "data").isDefined)
            assert(field(parts(1), "incremental").isDefined)
        }
    }

    "MultipartParser.parts (streamed)" - {
        "emits parts even when a chunk boundary falls mid-delimiter" in {
            // Cut the body in two at an awkward offset (inside the second delimiter).
            val cut    = body.indexOf(s"--$boundary", 10) + 3
            val chunks = Stream.init(Seq(body.substring(0, cut), body.substring(cut)))
            StreamProbe.collect(MultipartParser.parts(boundary, chunks)).map { parts =>
                assert(parts.size == 2)
                assert(field(parts(0), "data").isDefined)
                assert(field(parts(1), "incremental").isDefined)
            }
        }
    }
end MultipartParserSpec
