package kyo.apollo.api

import kyo.Frame
import kyo.Schema
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson

/** A bidirectional codec between a typed `data` payload `D` and the slim [[Json]]
  * AST — the single seam response decoding and cache (de)normalization go
  * through.
  *
  * It generalizes the previous `Operation.dataSchema: Schema[D]` contract so a
  * `data` payload can be produced two ways:
  *   - [[fromSchema]] wraps a kyo-schema `Schema[D]` — how schema-derived
  *     operations (generated or hand-written `derives Schema` case classes) supply
  *     their codec; and
  *   - a [[SelectionBuilder]] supplies its own *structural* codec directly, so the
  *     inline-query path needs no `Schema` for the result named tuple at all
  *     (avoiding the Scala.js linker issues sum-type derivation hits at kyo RC5).
  *
  * The contract is bidirectional because the normalized cache both *encodes* a
  * `data` value into records ([[kyo.apollo.cache.normalized.ApolloStore]]
  * normalize/write) and *decodes* records back into `D` (denormalized reads) —
  * a decode-only function would break cache participation.
  */
trait JsonCodec[D]:
    def decode(json: Json): D
    def encode(value: D): Json

object JsonCodec:

    /** Adapt a kyo-schema `Schema[D]` into a [[JsonCodec]] via [[SchemaJson]]. A
      * mismatch throws the `ApolloParseException`, which the response parse re-wraps
      * with its own frame; `decode` has no frame to thread.
      */
    def fromSchema[D](using schema: Schema[D]): JsonCodec[D] =
        new JsonCodec[D]:
            def decode(json: Json): D  = SchemaJson.decode[D](json)(using schema, Frame.internal).getOrThrow
            def encode(value: D): Json = SchemaJson.encode[D](value)
end JsonCodec
