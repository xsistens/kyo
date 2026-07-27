package kyo.apollo.network.http

import kyo.*
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser

/** One part of a `multipart/mixed` incremental-delivery body: its parsed JSON. */
final case class MultipartPart(json: Json)

/** Splits a `multipart/mixed` incremental-delivery (`@defer`) body into its JSON
  * parts. Parts are delimited by `--<boundary>`; each part is `CRLF`-separated
  * MIME headers, then a blank line, then the JSON body. Works both on a
  * fully-buffered body and incrementally over a stream of text chunks (emitting a
  * part as soon as the *following* delimiter confirms it complete).
  */
object MultipartParser:

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

    /** Split a fully-buffered body into its parts. */
    def parts(boundary: String, body: String): List[MultipartPart] =
        val buffer = new StringBuilder(body)
        extractComplete(buffer, s"--$boundary").toList

    /** Split a stream of text chunks into parts as each completes. A `StringBuilder`
      * buffer accumulates across chunks (safe: single-threaded JS, single-use
      * stream) and is created per materialization.
      */
    def parts(
        boundary: String,
        chunks: Stream[String, Async & Scope]
    )(using Frame): Stream[MultipartPart, Async & Scope] =
        val delim = s"--$boundary"
        Stream.unwrap {
            Sync.defer {
                val buffer = new StringBuilder
                chunks.mapChunkPure { texts =>
                    texts.foreach(buffer.append)
                    extractComplete(buffer, delim)
                }
            }
        }
    end parts

    /** Cut every *complete* part (one with a following delimiter) out of `buffer`,
      * leaving the incomplete tail (from the last delimiter on) in place.
      */
    private def extractComplete(buffer: StringBuilder, delim: String): Seq[MultipartPart] =
        val s    = buffer.toString
        val out  = List.newBuilder[MultipartPart]
        var idx  = s.indexOf(delim)
        var done = idx < 0
        while !done do
            val bodyStart = idx + delim.length
            val nextIdx   = s.indexOf(delim, bodyStart)
            if nextIdx < 0 then done = true
            else
                parsePart(s.substring(bodyStart, nextIdx)).foreach(out += _)
                idx = nextIdx
            end if
        end while
        val remaining = if idx >= 0 then s.substring(idx) else s
        buffer.clear()
        buffer.append(remaining)
        out.result()
    end extractComplete

    /** Parse one delimited segment (headers + blank line + JSON body) into a part;
      * `None` for the closing `--` terminator or an empty/header-only segment.
      */
    private def parsePart(segment: String): Option[MultipartPart] =
        val trimmed = segment.trim
        if trimmed.isEmpty || trimmed.startsWith("--") then None
        else
            val body = stripHeaders(segment).trim
            if body.isEmpty then None else Some(MultipartPart(JsonParser.parse(body)))
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
