package kyo.apollo.json

import kyo.*

/** Bridge between kyo-schema's typed JSON codecs (`derives Schema` +
  * `kyo.Json.encode`/`kyo.Json.decode`) and apollo's slim [[Json]] AST — the
  * generic value model the cache/normalizer walk.
  *
  * This module lives in `package kyo.apollo.json` next to the [[Json]] AST, so an
  * unqualified `Json` here is apollo's AST (the same-package type wins over the
  * `import kyo.*` wildcard); kyo-schema's default JSON format is referenced
  * explicitly as `kyo.Json`. `import kyo.*` brings in `Schema` and that format.
  *
  * This replaces the hand-rolled token-codec layer: encoding a typed value goes
  * value → kyo wire string → [[JsonParser]] → AST, and decoding goes AST → wire
  * string → `kyo.Json.decode`, keeping the JSON API effect-neutral (decode is a
  * pure `Result`, surfaced via `getOrThrow` into the existing exception path).
  */
object SchemaJson:

    // kyo forbids auto-deriving `Frame` inside `package kyo.*`. This codec is
    // deliberately effect-neutral (see above), so it supplies an internal Frame
    // for `kyo.Json`'s decode-trace metadata rather than threading a `using Frame`.
    private given Frame = Frame.internal

    /** Encode a typed value into the [[kyo.apollo.json.Json]] AST the normalizer
      * walks — the write-side inverse of [[decode]].
      */
    def encode[A](value: A)(using Schema[A]): kyo.apollo.json.Json =
        JsonParser.parse(kyo.Json.encode[A](value))

    /** Decode a typed value from an [[kyo.apollo.json.Json]] payload sub-tree,
      * throwing the kyo `DecodeException` on failure so callers route it through
      * the existing parse-exception path.
      */
    def decode[A](payload: kyo.apollo.json.Json)(using Schema[A]): A =
        kyo.Json.decode[A](payload.render).getOrThrow

    /** Decode a typed value directly from a wire JSON string. */
    def decodeString[A](wire: String)(using Schema[A]): A =
        kyo.Json.decode[A](wire).getOrThrow
end SchemaJson
