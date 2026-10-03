package kyo.apollo.network.http

import kyo.*
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import scala.annotation.tailrec

/** One part of a `multipart/mixed` incremental-delivery body. */
enum MultipartPart:
    /** A part whose body is a JSON document. */
    case Payload(json: Json)

    /** A part whose body is not a JSON document: corrupted on the wire, or cut
      * because the body ended in the middle of it.
      */
    case Malformed(error: ApolloParseException)
end MultipartPart

/** Splits a `multipart/mixed` incremental-delivery (`@defer`) body into its JSON
  * parts. Parts are delimited by `--<boundary>`; each part is `CRLF`-separated
  * MIME headers, then a blank line, then the JSON body. Works both on a
  * fully-buffered body and incrementally over a stream of text chunks (emitting a
  * part as soon as the *following* delimiter confirms it complete, and the last one
  * when the body ends).
  *
  * A part that does not parse is a [[MultipartPart.Malformed]] value in its place;
  * the parts after it are still delivered.
  */
object MultipartParser:

    private val maxShownText = 80

    /** The `boundary` parameter of a `multipart/mixed; boundary=…` Content-Type
      * (quotes stripped); defaults to `-` (graphql-js / Apollo's default).
      */
    def boundaryOf(contentType: String): String =
        contentType
            .split(";")
            .iterator
            .map(_.trim)
            .collectFirst {
                case p if p.toLowerCase.startsWith("boundary=") =>
                    p.drop("boundary=".length).trim.stripPrefix("\"").stripSuffix("\"")
            }
            .filter(_.nonEmpty)
            .getOrElse("-")

    /** Split a fully-buffered body into its parts, including a last part that no
      * terminator follows.
      */
    def parts(boundary: String, body: String)(using Frame): Chunk[MultipartPart] =
        val delim            = s"--$boundary"
        val (complete, rest) = extractComplete(body, delim)
        complete.concat(flush(rest, delim))
    end parts

    /** Split a stream of text chunks into parts as each completes. The text after
      * the last delimiter is carried to the next chunk in a cell created per
      * materialization, and flushed when the chunk stream ends. The chunk stream's
      * effects — including an engine's failure row — pass through unchanged.
      */
    def parts[S](
        boundary: String,
        chunks: Stream[String, S]
    )(using Frame): Stream[MultipartPart, S & Sync] =
        val delim = s"--$boundary"
        Stream.unwrap {
            AtomicRef.init("").map { carried =>
                chunks
                    .mapChunk { texts =>
                        carried.get.map { pending =>
                            val (complete, rest) = extractComplete(pending + texts.mkString, delim)
                            carried.set(rest).andThen(complete)
                        }
                    }
                    .concat(Stream.unwrap(carried.get.map(rest => Stream.init(flush(rest, delim)))))
            }
        }
    end parts

    /** Cut every *complete* part (one with a following delimiter) out of `text`.
      * Returns those parts and the rest: from the last delimiter on, or all of `text`
      * when it holds no delimiter yet.
      */
    private def extractComplete(text: String, delim: String)(using Frame): (Chunk[MultipartPart], String) =
        @tailrec def loop(start: Int, out: Chunk[MultipartPart]): (Chunk[MultipartPart], String) =
            val bodyStart = start + delim.length
            val next      = text.indexOf(delim, bodyStart)
            if next < 0 then (out, text.substring(start))
            else loop(next, parsePart(text.substring(bodyStart, next)).fold(out)(out.append))
        end loop
        val first = text.indexOf(delim)
        if first < 0 then (Chunk.empty, text) else loop(first, Chunk.empty)
    end extractComplete

    /** The parts left in `rest` when the body ends: none for an empty rest or the
      * closing terminator, the last part when no terminator followed it (`Malformed`
      * when it was cut). Text with no delimiter at all is not a multipart body.
      */
    private def flush(rest: String, delim: String)(using Frame): Chunk[MultipartPart] =
        if rest.startsWith(delim) then parsePart(rest.substring(delim.length)).toChunk
        else if rest.trim.isEmpty then Chunk.empty
        else
            Chunk(MultipartPart.Malformed(
                ApolloParseException(Json.JStr(rest.take(maxShownText)), s"a multipart/mixed body delimited by $delim")
            ))

    /** Parse one delimited segment (headers + blank line + JSON body) into a part;
      * `Absent` for the closing `--` terminator or an empty/header-only segment.
      */
    private def parsePart(segment: String)(using Frame): Maybe[MultipartPart] =
        val trimmed = segment.trim
        if trimmed.isEmpty || trimmed.startsWith("--") then Absent
        else
            val body = stripHeaders(segment).trim
            if body.isEmpty then Absent
            else Present(JsonParser.parse(body).foldOrThrow(MultipartPart.Payload(_), MultipartPart.Malformed(_)))
        end if
    end parsePart

    /** Drop the part's MIME headers (up to the first blank line); if there is none,
      * the whole segment is treated as the body.
      */
    private def stripHeaders(segment: String): String =
        val crlf = segment.indexOf("\r\n\r\n")
        if crlf >= 0 then segment.substring(crlf + 4)
        else
            val lf = segment.indexOf("\n\n")
            if lf >= 0 then segment.substring(lf + 2) else segment
        end if
    end stripHeaders
end MultipartParser
