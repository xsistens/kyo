package kyo.apollo.api

import kyo.Frame
import kyo.Result
import kyo.Schema
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson

/** Decodes a typed `data` payload `D` from the slim [[Json]] AST — what response
  * decoding ([[GraphQLResponse.parse]]) and a denormalized cache read need.
  *
  * A payload of the wrong shape is an [[ApolloParseException]] failure built with
  * the caller's frame. Anything a decoder throws is a defect: the callers run it
  * where a throw becomes a panic, never a failure.
  */
trait JsonDecoder[D]:
    def decode(json: Json)(using Frame): Result[ApolloParseException, D]

/** A bidirectional codec between a typed `data` payload `D` and the slim [[Json]]
  * AST — a [[JsonDecoder]] that also encodes, which is what writing into the
  * normalized cache needs: the store *encodes* a `data` value into records
  * ([[kyo.apollo.cache.normalized.ApolloStore]] normalize/write) and *decodes*
  * records back into `D`.
  *
  * A `data` payload's codec comes from one of two places:
  *   - [[fromSchema]] wraps a kyo-schema `Schema[D]` — how an operation over a
  *     `derives Schema` case class supplies it; and
  *   - a [[SelectionBuilder]] supplies its own *structural* codec, so the
  *     inline-query path needs no `Schema` for its named-tuple result (avoiding
  *     the Scala.js linker issues sum-type derivation hits at kyo RC5).
  */
trait JsonCodec[D] extends JsonDecoder[D]:
    def encode(value: D): Json

object JsonCodec:

    /** The codec of an operation or fragment over a `derives Schema` type: decodes and
      * encodes through kyo-schema ([[SchemaJson]]). A payload the schema rejects is an
      * `ApolloParseException` failure carrying kyo's `DecodeException` as cause.
      */
    def fromSchema[D](using schema: Schema[D]): JsonCodec[D] =
        new JsonCodec[D]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, D] = SchemaJson.decode[D](json)
            def encode(value: D): Json                                           = SchemaJson.encode[D](value)
end JsonCodec
