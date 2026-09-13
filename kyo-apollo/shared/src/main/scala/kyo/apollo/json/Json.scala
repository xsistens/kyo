package kyo.apollo.json

import kyo.Chunk
import kyo.apollo.Upload

/** A minimal internal JSON AST.
  *
  * This is deliberately dependency-free: `core` stays clean and self-contained
  * while the type-safe primitives take shape. Phase 02 replaces/extends this
  * with a richer model, so keep the surface area small and obvious.
  */
enum Json derives CanEqual:
    case JNull
    case JBool(value: Boolean)
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

    /** Render a number, collapsing integral values to avoid a trailing `.0`. */
    private def renderNumber(v: Double): String =
        if v.isWhole && !v.isInfinite && !v.isNaN then v.toLong.toString
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
