package kyo.apollo.network.http

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json

/** Unit tests for [[MultipartParser]]: boundary extraction, and splitting a
  * `multipart/mixed` incremental-delivery body into parts both when fully buffered
  * and when it arrives split across stream chunks (mid-delimiter). The body's end is
  * a step of its own: a last part without a terminator is delivered, a cut one is
  * [[MultipartPart.Malformed]], and a malformed part does not end the stream.
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

    private def field(part: MultipartPart, key: String): Maybe[Json] =
        part match
            case MultipartPart.Payload(Json.JObj(fields)) => Maybe.fromOption(fields.get(key))
            case _                                        => Absent

    private def isPayload(part: MultipartPart): Boolean = part match
        case MultipartPart.Payload(_) => true
        case _                        => false

    private def isMalformed(part: MultipartPart): Boolean = part match
        case MultipartPart.Malformed(_: ApolloParseException) => true
        case _                                                => false

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

        "delivers the last part when the body ends without a terminator" in {
            val parts = MultipartParser.parts("b", "--b\r\n\r\n{\"data\":1,\"hasNext\":false}")
            assert(parts.size == 1)
            assert(field(parts(0), "hasNext") == Present(Json.JBool(false)))
        }

        "text without any delimiter is Malformed, not an empty delivery" in {
            val parts = MultipartParser.parts("b", "{\"data\":1}")
            assert(parts.size == 1 && isMalformed(parts(0)))
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

        "delivers the last part when the body ends without a terminator" in {
            val chunks = Stream.init(Seq("--b\r\n\r\n{\"data\":1,", "\"hasNext\":false}"), chunkSize = 1)
            StreamProbe.collect(MultipartParser.parts("b", chunks)).map { parts =>
                assert(parts.size == 1, s"got $parts")
                assert(field(parts(0), "hasNext") == Present(Json.JBool(false)))
            }
        }

        "a body cut mid-part yields Malformed, not silence" in {
            val chunks = Stream.init(Seq("--b\r\n\r\n{\"data\":1,\"hasNext\":true}\r\n", "--b\r\n\r\n{\"data\":1,\"has"))
            StreamProbe.collect(MultipartParser.parts("b", chunks)).map { parts =>
                assert(parts.size == 2, s"got $parts")
                assert(isPayload(parts(0)))
                assert(isMalformed(parts(1)))
            }
        }

        "a malformed part does not end the stream" in {
            val chunks = Stream.init(
                Seq(
                    "--b\r\n\r\n{\"data\":1}\r\n",
                    "--b\r\n\r\n{not json}\r\n",
                    "--b\r\n\r\n{\"data\":2}\r\n--b--\r\n"
                ),
                chunkSize = 1
            )
            StreamProbe.collect(MultipartParser.parts("b", chunks)).map { parts =>
                assert(parts.size == 3, s"got $parts")
                assert(isPayload(parts(0)) && isMalformed(parts(1)) && isPayload(parts(2)))
                assert(field(parts(2), "data") == Present(Json.JInt(2)))
            }
        }

        "the closing terminator alone, or an empty body, yields no part" in {
            val terminatorOnly = Stream.init(Seq("--b\r\n\r\n{\"data\":1}\r\n--b", "--\r\n"), chunkSize = 1)
            val empty          = Stream.init(Seq.empty[String])
            for
                afterTerminator <- StreamProbe.collect(MultipartParser.parts("b", terminatorOnly))
                nothing         <- StreamProbe.collect(MultipartParser.parts("b", empty))
            yield
                assert(afterTerminator.size == 1 && isPayload(afterTerminator(0)))
                assert(nothing.isEmpty)
            end for
        }
    }
end MultipartParserSpec
