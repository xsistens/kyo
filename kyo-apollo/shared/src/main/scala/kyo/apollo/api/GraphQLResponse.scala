package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.json.Json

/** A parsed GraphQL response for an operation with `data` payload type `D`.
  *
  * All three fields can be populated simultaneously: GraphQL permits a partial
  * `data` result alongside a non-empty `errors` list, plus top-level
  * `extensions` metadata. `extensions` is passed through as raw JSON (added in
  * Task 6 with a default so Phase 01 `GraphQLResponse(data, errors)`
  * constructions keep compiling). Build one from wire JSON via
  * [[GraphQLResponse.parse]].
  */
final case class GraphQLResponse[D](
    data: Maybe[D],
    errors: Chunk[GraphQLError],
    extensions: Map[String, Json] = Map.empty
)

object GraphQLResponse:

    /** Parse a raw response envelope `{ data?, errors?, extensions? }` using
      * `operation`'s kyo-schema data codec.
      *
      * Robustness contract (never throws on a *shape-valid* envelope):
      *   - `data` absent or JSON `null` → `None` (request/field errors return no
      *     data); a present non-null `data` is decoded with `operation.dataSchema`.
      *   - `errors` absent or empty → `Nil`; otherwise each entry is parsed by
      *     [[GraphQLError.parse]].
      *   - `extensions` absent → empty map; a present object is passed through.
      *
      * The partial-data case (both `data` and `errors` present) is fully
      * represented — both fields are populated. Only a fundamentally malformed
      * envelope (a non-object top level) throws.
      *
      * A present, non-null `data` value is decoded through the operation's
      * [[Operation.dataCodec]] (schema-backed or a [[SelectionBuilder]]'s structural
      * codec); a schema decode failure throws the kyo `DecodeException`, which the
      * transport catches as an `ApolloParseException` — the same exception path the
      * legacy adapter took.
      */
    def parse[D](
        json: Json,
        operation: Operation[D]
    ): GraphQLResponse[D] =
        json match
            case Json.JObj(fields) =>
                val data = fields.get("data") match
                    case None | Some(Json.JNull) => Absent
                    case Some(payload)           => Present(operation.dataCodec.decode(payload))
                val errors = fields.get("errors") match
                    case Some(Json.JArr(items)) => items.map(GraphQLError.parse)
                    case _                      => Chunk.empty
                val extensions = fields.get("extensions") match
                    case Some(Json.JObj(ext)) => ext
                    case _                    => Map.empty
                GraphQLResponse(data, errors, extensions)
            case other =>
                throw GraphQLResponseException(
                    s"Expected a GraphQL response object but got: ${other.render}"
                )
end GraphQLResponse

/** Raised when a response (or one of its `errors` entries) is not the JSON
  * object the parser requires. Never raised merely because `data` is absent or
  * `null` — that is a valid GraphQL response.
  */
final class GraphQLResponseException(message: String) extends RuntimeException(message)
