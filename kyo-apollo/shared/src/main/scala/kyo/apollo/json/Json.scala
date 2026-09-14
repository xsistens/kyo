package kyo.apollo.json

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.Upload

/** The JSON AST the client's response, variables and cache layers share.
  *
  * Numbers keep the precision the wire carries: an integer that fits a `Long` is a
  * [[JInt]], a decimal a `Double` represents exactly is a [[JNum]], and anything
  * else (an integer beyond `Long`, a decimal with more digits than a `Double`
  * holds) is a [[JDec]]. [[JsonParser]] chooses the case, and [[render]] writes each
  * back in the form it was read.
  */
enum Json derives CanEqual:
    case JNull
    case JBool(value: Boolean)

    /** An integer in `Long` range: ids, counters, timestamps. */
    case JInt(value: Long)

    /** A number a `Double` does not hold exactly: money, integers beyond `Long`. */
    case JDec(value: BigDecimal)

    /** A decimal in `Double` range. */
    case JNum(value: Double)
    case JStr(value: String)
    case JArr(items: Chunk[Json])
    case JObj(fields: Map[String, Json])

    /** A file-upload placeholder — an [[kyo.apollo.Upload]] carried inside a
      * request's `variables` for the graphql-multipart-request-spec. It is NOT real
      * JSON: the request composer walks the variables, replaces each `JUpload` with
      * `null` in the `operations` payload and carries the upload as a separate form
      * part. It only ever appears on the request/variables path, never in response
      * `data`; the exhaustive renderers therefore emit `null` for it defensively.
      */
    case JUpload(upload: Upload)

    /** Render this value as compact, valid JSON text. */
    def render: String = this match
        case JNull       => "null"
        case JBool(v)    => if v then "true" else "false"
        case JInt(v)     => v.toString
        case JDec(v)     => v.toString
        case JNum(v)     => Json.renderNumber(v)
        case JStr(v)     => Json.renderString(v)
        case JArr(items) => items.map(_.render).mkString("[", ",", "]")
        case JObj(fields) =>
            fields
                .map((k, v) => s"${Json.renderString(k)}:${v.render}")
                .mkString("{", ",", "}")
        case JUpload(_) => "null"
end Json

object Json:

    /** The exact integer a JSON number denotes, or `Absent` when `json` is not a
      * number, or its value is fractional or outside `Long` range.
      */
    def integral(json: Json): Maybe[Long] = json match
        case JInt(v)                                => Present(v)
        case JNum(v) if v.isWhole && inLongRange(v) => Present(v.toLong)
        case JDec(v) if v.isValidLong               => Present(v.toLong)
        case _                                      => Absent

    private def inLongRange(v: Double): Boolean =
        v >= -9.223372036854775808e18 && v < 9.223372036854775808e18

    /** Render a double, collapsing integral values in `Long` range to avoid a trailing `.0`. */
    private def renderNumber(v: Double): String =
        if v.isWhole && inLongRange(v) then v.toLong.toString
        else v.toString

    /** Render a string as a JSON string literal with the required escaping. */
    private def renderString(s: String): String =
        val sb = StringBuilder("\"")
        s.foreach {
            case '"'          => sb ++= "\\\""
            case '\\'         => sb ++= "\\\\"
            case '\b'         => sb ++= "\\b"
            case '\f'         => sb ++= "\\f"
            case '\n'         => sb ++= "\\n"
            case '\r'         => sb ++= "\\r"
            case '\t'         => sb ++= "\\t"
            case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
            case c            => sb += c
        }
        sb += '"'
        sb.result()
    end renderString
end Json
