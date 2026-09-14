package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.Schema
import kyo.apollo.exception.ApolloParseException
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
  *
  * A value of the wrong JSON type decodes to an [[ApolloParseException]] failure
  * built with the caller's frame, naming the type expected and the JSON type found,
  * never the value; a codec raises nothing on that path.
  */
trait ScalarCodec[V] extends JsonCodec[V]

object ScalarCodec:

    val string: ScalarCodec[String] =
        new ScalarCodec[String]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, String] =
                json match
                    case Json.JStr(value) => Result.succeed(value)
                    case other            => mismatch(other, "String")
            def encode(value: String): Json = Json.JStr(value)

    /** GraphQL `ID` is serialized as a string on the wire. */
    val id: ScalarCodec[String] = string

    /** GraphQL `Int`: a JSON number with an exact 32-bit value (`1.0` included, `1.5`
      * and `2^31` rejected, never truncated).
      */
    val int: ScalarCodec[Int] =
        new ScalarCodec[Int]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Int] =
                Json.integral(json) match
                    case Present(value) if value.isValidInt => Result.succeed(value.toInt)
                    case _                                  => mismatch(json, "Int")
            def encode(value: Int): Json = Json.JInt(value.toLong)

    /** GraphQL `Float`: any JSON number, including an integer literal such as `1`. */
    val double: ScalarCodec[Double] =
        new ScalarCodec[Double]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Double] =
                json match
                    case Json.JNum(value) => Result.succeed(value)
                    case Json.JInt(value) => Result.succeed(value.toDouble)
                    case Json.JDec(value) => Result.succeed(value.toDouble)
                    case other            => mismatch(other, "Float")
            def encode(value: Double): Json = Json.JNum(value)

    val boolean: ScalarCodec[Boolean] =
        new ScalarCodec[Boolean]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Boolean] =
                json match
                    case Json.JBool(value) => Result.succeed(value)
                    case other             => mismatch(other, "Boolean")
            def encode(value: Boolean): Json = Json.JBool(value)

    /** Lift a codec over a nullable field: JSON `null` ⇄ `Absent`. */
    def maybe[V](inner: ScalarCodec[V]): ScalarCodec[Maybe[V]] =
        new ScalarCodec[Maybe[V]]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Maybe[V]] =
                json match
                    case Json.JNull => Result.succeed(Absent)
                    case other      => inner.decode(other).map(Present(_))
            def encode(value: Maybe[V]): Json =
                value match
                    case Absent         => Json.JNull
                    case Present(value) => inner.encode(value)

    /** Lift a codec over a list field: a JSON array ⇄ `Chunk`. The first element of
      * the wrong type fails the whole list.
      */
    def chunk[V](inner: ScalarCodec[V]): ScalarCodec[Chunk[V]] =
        new ScalarCodec[Chunk[V]]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Chunk[V]] =
                json match
                    case Json.JArr(items) => Result.collect(items.map(inner.decode(_))).map(Chunk.from)
                    case other            => mismatch(other, "list")
            def encode(values: Chunk[V]): Json = Json.JArr(values.map(inner.encode))

    /** Reuse a kyo-schema `Schema` (e.g. a generated enum's string-transform
      * `given`, or a mapped custom scalar) as a leaf codec, via [[SchemaJson]].
      * A value the schema rejects is the `ApolloParseException` failure
      * [[SchemaJson.decode]] builds with the caller's frame.
      */
    def fromSchema[V](using Schema[V]): ScalarCodec[V] =
        new ScalarCodec[V]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, V] = SchemaJson.decode[V](json)
            def encode(value: V): Json                                           = SchemaJson.encode[V](value)

    /** The `Upload` scalar codec: encodes an [[kyo.apollo.Upload]] to a
      * [[Json.JUpload]] placeholder that the request composer lifts into a
      * `multipart/form-data` file part. Input-only — an `Upload` never appears in a
      * response `data`, so every decode is a failure.
      */
    val upload: ScalarCodec[kyo.apollo.Upload] =
        new ScalarCodec[kyo.apollo.Upload]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, kyo.apollo.Upload] =
                mismatch(json, "Upload, which is input-only and never in a response,")
            def encode(value: kyo.apollo.Upload): Json = Json.JUpload(value)

    /** The failure of a leaf decode: the GraphQL type expected and the JSON type of
      * `json`, never its value.
      */
    private def mismatch[V](json: Json, graphQLType: String)(using Frame): Result[ApolloParseException, V] =
        Result.fail(ApolloParseException(json, s"a GraphQL $graphQLType"))
end ScalarCodec
