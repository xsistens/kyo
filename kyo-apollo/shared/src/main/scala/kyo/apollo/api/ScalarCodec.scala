package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.Schema
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson

/** A leaf (scalar or enum) codec operating directly on the slim [[Json]] AST.
  *
  * This is the value-level building block a [[SelectionBuilder]] field carries:
  * it turns a single response value into its Scala type and back. Leaves
  * deliberately reuse the existing, linker-safe machinery — plain `Json` pattern
  * matches for the built-in scalars, and [[SchemaJson]] (which wraps kyo-schema's
  * `Schema`) via [[ScalarCodec.fromSchema]] for enums and mapped custom scalars.
  * Crucially, nothing on this path introduces a new `derives`, so it stays clear
  * of the Scala.js linker issues sum-type derivation hits at kyo RC5.
  */
final case class ScalarCodec[V](decode: Json => V, encode: V => Json)

object ScalarCodec:

    val string: ScalarCodec[String] = ScalarCodec(
        {
            case Json.JStr(value) => value
            case other            => throw ScalarDecodeException("String", other)
        },
        value => Json.JStr(value)
    )

    /** GraphQL `ID` is serialized as a string on the wire. */
    val id: ScalarCodec[String] = string

    val int: ScalarCodec[Int] = ScalarCodec(
        {
            case Json.JNum(value) => value.toInt
            case other            => throw ScalarDecodeException("Int", other)
        },
        value => Json.JNum(value.toDouble)
    )

    val double: ScalarCodec[Double] = ScalarCodec(
        {
            case Json.JNum(value) => value
            case other            => throw ScalarDecodeException("Float", other)
        },
        value => Json.JNum(value)
    )

    val boolean: ScalarCodec[Boolean] = ScalarCodec(
        {
            case Json.JBool(value) => value
            case other             => throw ScalarDecodeException("Boolean", other)
        },
        value => Json.JBool(value)
    )

    /** Lift a codec over a nullable field: JSON `null` ⇄ `Absent`. */
    def maybe[V](inner: ScalarCodec[V]): ScalarCodec[Maybe[V]] = ScalarCodec(
        {
            case Json.JNull => Absent
            case other      => Present(inner.decode(other))
        },
        {
            case Absent         => Json.JNull
            case Present(value) => inner.encode(value)
        }
    )

    /** Lift a codec over a list field: a JSON array ⇄ `Chunk`. */
    def chunk[V](inner: ScalarCodec[V]): ScalarCodec[Chunk[V]] = ScalarCodec(
        {
            case Json.JArr(items) => items.map(inner.decode)
            case other            => throw ScalarDecodeException("List", other)
        },
        values => Json.JArr(values.map(inner.encode))
    )

    /** Reuse a kyo-schema `Schema` (e.g. a generated enum's string-transform
      * `given`, or a mapped custom scalar) as a leaf codec, via [[SchemaJson]].
      */
    def fromSchema[V](using Schema[V]): ScalarCodec[V] =
        ScalarCodec(SchemaJson.decode[V], SchemaJson.encode[V])

    /** The `Upload` scalar codec: encodes an [[kyo.apollo.Upload]] to a
      * [[Json.JUpload]] placeholder that the request composer lifts into a
      * `multipart/form-data` file part. Input-only — decoding is unsupported (an
      * `Upload` never appears in a response `data`).
      */
    val upload: ScalarCodec[kyo.apollo.Upload] = ScalarCodec(
        json => throw ScalarDecodeException("Upload", json),
        value => Json.JUpload(value)
    )
end ScalarCodec

/** Raised when a leaf value is not the JSON shape its [[ScalarCodec]] expects. */
final class ScalarDecodeException(expected: String, got: Json)
    extends RuntimeException(s"Expected a GraphQL $expected but got: ${got.render}")
