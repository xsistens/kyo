package kyo.apollo.json

import kyo.*
import kyo.apollo.exception.ApolloParseException

/** Bridge between kyo-schema's typed codecs (`derives Schema`) and apollo's [[Json]]
  * AST — the generic value model the cache/normalizer walk.
  *
  * The conversion is structural in both directions: a typed value becomes a
  * `Structure.Value` tree (`Structure.encode`) that maps onto [[Json]]
  * ([[JsonParser.fromStructure]]), and a [[Json]] payload maps onto a
  * `Structure.Value` tree ([[JsonParser.toStructure]]) that kyo-schema decodes
  * (`Structure.decode`). No JSON text is rendered or re-parsed, so integers and
  * decimals keep their precision and a decode costs one tree walk.
  *
  * This module lives in `package kyo.apollo.json` next to the [[Json]] AST, so an
  * unqualified `Json` here is apollo's AST (the same-package type wins over the
  * `import kyo.*` wildcard); kyo-schema's default JSON format is referenced
  * explicitly as `kyo.Json`.
  */
object SchemaJson:

    /** Encode a typed value into the [[kyo.apollo.json.Json]] AST the normalizer
      * walks — the write-side inverse of [[decode]].
      */
    def encode[A](value: A)(using Schema[A]): kyo.apollo.json.Json =
        // `Structure.encode` takes a Frame it does not use: encoding cannot fail.
        JsonParser.fromStructure(Structure.encode[A](value)(using summon[Schema[A]], Frame.internal))

    /** Decode a typed value from a [[kyo.apollo.json.Json]] payload sub-tree. A payload
      * that does not match the schema is a failure carrying kyo's `DecodeException` as
      * its cause.
      */
    def decode[A](payload: kyo.apollo.json.Json)(using Schema[A], Frame): Result[ApolloParseException, A] =
        Structure
            .decode[A](JsonParser.toStructure(payload))
            .mapFailure(e => ApolloParseException(payload, s"a value of ${summon[Schema[A]].structure.name}", e))

    /** Decode a typed value directly from a wire JSON string. */
    def decodeString[A](wire: String)(using Schema[A], Frame): Result[ApolloParseException, A] =
        JsonParser.parse(wire).flatMap(decode[A])
end SchemaJson
