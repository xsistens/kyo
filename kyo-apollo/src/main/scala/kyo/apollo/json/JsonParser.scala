package kyo.apollo.json

import kyo.Chunk
import scala.collection.immutable.VectorMap
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Parses JSON text into the internal [[Json]] AST.
  *
  * Rather than hand-roll a lexer, this delegates to the platform's native
  * `JSON.parse` (fast and spec-correct) and then converts the resulting
  * `js.Any` graph into the typed [[Json]] model.
  */
object JsonParser:

    /** Parse `input` into a [[Json]] value, throwing on malformed input. */
    def parse(input: String): Json =
        fromJsAny(JSON.parse(input))

    private def fromJsAny(value: js.Any): Json =
        js.typeOf(value) match
            case "boolean" => Json.JBool(value.asInstanceOf[Boolean])
            case "number"  => Json.JNum(value.asInstanceOf[Double])
            case "string"  => Json.JStr(value.asInstanceOf[String])
            case "object" =>
                if value == null then Json.JNull
                else if js.Array.isArray(value) then
                    Json.JArr(Chunk.from(value.asInstanceOf[js.Array[js.Any]].map(fromJsAny)))
                else
                    val dict = value.asInstanceOf[js.Dictionary[js.Any]]
                    // Preserve the source document's field order (JS objects and
                    // `JSON.parse` iterate string keys in insertion order) so that a
                    // parse → render round-trip is byte-stable and request bodies stay
                    // deterministic. A plain `Map` would drop that order.
                    Json.JObj(VectorMap.from(dict.toList.map((k, v) => k -> fromJsAny(v))))
            case _ => Json.JNull
end JsonParser
