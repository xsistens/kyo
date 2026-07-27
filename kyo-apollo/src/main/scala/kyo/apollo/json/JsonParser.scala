package kyo.apollo.json

import kyo.Frame
import kyo.Structure
import scala.collection.immutable.VectorMap
// NB: no `import kyo.*` — a wildcard import would bring `kyo.Json` (the schema
// format) into scope and shadow the same-package `kyo.apollo.json.Json` AST for an
// unqualified `Json`. We reference apollo's `Json` unqualified and kyo's format as
// the fully-qualified `kyo.Json`, mirroring SchemaJson's disambiguation.

/** Parses JSON text into the internal [[Json]] AST.
  *
  * Portable across every platform: decodes through kyo-schema-json's identity
  * `Schema[Structure.Value]` — a shape-preserving parse of arbitrary JSON that
  * reuses kyo's hand-rolled UTF-8 `JsonReader` — and maps the resulting
  * `Structure.Value` tree onto the [[Json]] model. This replaces the former
  * `js.JSON.parse` path, so the module builds on JVM and Native as well as
  * JS/Wasm.
  *
  * A derived `Schema[Json]` would be wrong here: kyo-schema encodes a sum type
  * as a tagged-union wrapper (`{"JStr":{"value":"x"}}`) and refuses plain JSON.
  * `Structure.Value` is the universal any-shape type, so its identity Schema
  * reads whatever shape the wire carries — exactly what a GraphQL response needs.
  */
object JsonParser:

    // kyo forbids auto-deriving `Frame` inside `package kyo.*`. This parse is
    // deliberately effect-neutral, so supply an internal Frame for kyo.Json's
    // decode-trace metadata rather than threading a `using Frame`.
    private given Frame = Frame.internal

    /** Parse `input` into a [[Json]] value, throwing on malformed input. */
    def parse(input: String): Json =
        fromStructure(kyo.Json.decode[Structure.Value](input).getOrThrow)

    private def fromStructure(value: Structure.Value): Json = value match
        case Structure.Value.Null           => Json.JNull
        case Structure.Value.Bool(b)        => Json.JBool(b)
        case Structure.Value.Integer(n)     => Json.JNum(n.toDouble)
        case Structure.Value.Decimal(d)     => Json.JNum(d)
        case Structure.Value.BigNum(d)      => Json.JNum(d.toDouble)
        case Structure.Value.Str(s)         => Json.JStr(s)
        case Structure.Value.Sequence(es)   => Json.JArr(es.map(fromStructure))
        case Structure.Value.Record(fields) =>
            // Preserve source field order (a VectorMap) so parse -> render is
            // byte-stable and request bodies stay deterministic, matching the
            // former JSON.parse insertion-order behaviour.
            Json.JObj(VectorMap.from(fields.map((k, v) => k -> fromStructure(v))))
        // VariantCase / MapEntries / Bytes / Instant / Duration never arise from a
        // JSON parse (readStructure materializes objects as Record, arrays as
        // Sequence, scalars unwrapped); fold defensively to null as before.
        case _ => Json.JNull
end JsonParser
