package kyo.apollo.json

import kyo.Chunk
import kyo.Frame
import kyo.Result
import kyo.Structure
import kyo.apollo.exception.ApolloParseException
import scala.collection.immutable.VectorMap
// NB: no `import kyo.*` — a wildcard import would bring `kyo.Json` (the schema
// format) into scope and shadow the same-package `kyo.apollo.json.Json` AST for an
// unqualified `Json`. We reference apollo's `Json` unqualified and kyo's format as
// the fully-qualified `kyo.Json`, mirroring SchemaJson's disambiguation.

/** Parses JSON text into the [[Json]] AST, and maps between [[Json]] and kyo-schema's
  * `Structure.Value` in both directions.
  *
  * Portable across every platform: text decodes through kyo-schema-json's identity
  * `Schema[Structure.Value]` — a shape-preserving parse of arbitrary JSON that reuses
  * kyo's UTF-8 `JsonReader` — and the resulting tree maps onto [[Json]].
  *
  * A derived `Schema[Json]` would be wrong here: kyo-schema encodes a sum type
  * as a tagged-union wrapper (`{"JStr":{"value":"x"}}`) and refuses plain JSON.
  * `Structure.Value` is the universal any-shape type, so its identity Schema
  * reads whatever shape the wire carries — exactly what a GraphQL response needs.
  */
object JsonParser:

    private val maxShownInput = 80

    /** Parse `input` into a [[Json]] value. Text that is not a JSON document is a
      * failure whose `actual` is a prefix of the text.
      *
      * kyo's reader reports some malformed numbers as a `NumberFormatException` panic
      * rather than a `DecodeException`; the input is untrusted wire text, so both are
      * the same parse failure here.
      */
    def parse(input: String)(using Frame): Result[ApolloParseException, Json] =
        kyo.Json.decode[Structure.Value](input) match
            case Result.Success(value)          => Result.succeed(fromStructure(value))
            case Result.Failure(cause)          => Result.fail(notJson(input, cause))
            case Result.Panic(cause: Exception) => Result.fail(notJson(input, cause))
            case panic @ Result.Panic(_)        => panic

    private def notJson(input: String, cause: Throwable)(using Frame): ApolloParseException =
        ApolloParseException(Json.JStr(input.take(maxShownInput)), "a JSON document", cause)

    /** Map a `Structure.Value` onto [[Json]] the way kyo-schema writes that value as JSON
      * (`SchemaSerializer.writeStructureValue` through `JsonWriter`), without the text:
      * a string-keyed map is an object, any other map an array of `{key, value}`
      * objects, a variant the `{name: payload}` wrapper, bytes Base64, instants and
      * durations their ISO-8601 strings, and a non-finite double its quoted name.
      * On values a JSON parse produces, this is the inverse of [[toStructure]].
      */
    private[apollo] def fromStructure(value: Structure.Value): Json = value match
        case Structure.Value.Null       => Json.JNull
        case Structure.Value.Bool(b)    => Json.JBool(b)
        case Structure.Value.Integer(n) => Json.JInt(n)
        case Structure.Value.Decimal(d) =>
            if d.isNaN then Json.JStr("NaN")
            else if d.isPosInfinity then Json.JStr("Infinity")
            else if d.isNegInfinity then Json.JStr("-Infinity")
            else Json.JNum(d)
        case Structure.Value.BigNum(d)      => Json.JDec(d)
        case Structure.Value.Str(s)         => Json.JStr(s)
        case Structure.Value.Sequence(es)   => Json.JArr(es.map(fromStructure))
        case Structure.Value.Record(fields) => Json.JObj(VectorMap.from(fields.iterator.map((k, v) => k -> fromStructure(v))))
        case Structure.Value.MapEntries(entries) =>
            val stringKeyed = entries.forall {
                case (Structure.Value.Str(_), _) => true
                case _                           => false
            }
            if stringKeyed then
                Json.JObj(VectorMap.from(entries.collect { case (Structure.Value.Str(k), v) => k -> fromStructure(v) }))
            else
                Json.JArr(entries.map((k, v) => Json.JObj(VectorMap("key" -> fromStructure(k), "value" -> fromStructure(v)))))
            end if
        case Structure.Value.VariantCase(name, payload) => Json.JObj(VectorMap(name -> fromStructure(payload)))
        case Structure.Value.Bytes(bytes) =>
            Json.JStr(java.util.Base64.getEncoder.encodeToString(bytes.toArray))
        case Structure.Value.Instant(instant)   => Json.JStr(instant.toString)
        case Structure.Value.Duration(duration) => Json.JStr(duration.toString)

    /** Map [[Json]] onto `Structure.Value`, keeping each number's case and each
      * object's field order. A [[Json.JUpload]] is `Null`, as the renderer writes it.
      */
    private[apollo] def toStructure(json: Json): Structure.Value = json match
        case Json.JNull        => Structure.Value.Null
        case Json.JBool(b)     => Structure.Value.Bool(b)
        case Json.JInt(n)      => Structure.Value.Integer(n)
        case Json.JDec(d)      => Structure.Value.BigNum(d)
        case Json.JNum(d)      => Structure.Value.Decimal(d)
        case Json.JStr(s)      => Structure.Value.Str(s)
        case Json.JArr(items)  => Structure.Value.Sequence(items.map(toStructure))
        case Json.JObj(fields) => Structure.Value.Record(Chunk.from(fields.iterator.map((k, v) => k -> toStructure(v))))
        case Json.JUpload(_)   => Structure.Value.Null
end JsonParser
