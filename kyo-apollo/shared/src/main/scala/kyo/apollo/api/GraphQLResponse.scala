package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.DecodeException
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.apollo.exception.ApolloParseException
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
      * `operation`'s data codec.
      *
      *   - `data` absent or JSON `null` → `Absent` (request/field errors return no
      *     data); a present non-null `data` is decoded with [[Operation.dataCodec]].
      *   - `errors` absent or empty → empty; otherwise each entry is parsed by
      *     [[GraphQLError.parse]].
      *   - `extensions` absent → empty map; a present object is passed through.
      *
      * The partial-data case (both `data` and `errors` present) is fully
      * represented. A server that sent the wrong shape — a non-object envelope, a
      * non-object error entry, or `data` the codec rejects — is a
      * `Result.Failure(ApolloParseException)`. Any other exception from the codec
      * is a defect, not a wire failure, and stays a `Result.Panic`.
      */
    def parse[D](
        json: Json,
        operation: Operation[D]
    )(using Frame): Result[ApolloParseException, GraphQLResponse[D]] =
        json match
            case Json.JObj(fields) =>
                val data: Result[ApolloParseException, Maybe[D]] = fields.get("data") match
                    case None | Some(Json.JNull) => Result.succeed(Absent)
                    case Some(payload)           => decodeData(payload, operation).map(Present(_))
                val errors: Result[ApolloParseException, Chunk[GraphQLError]] = fields.get("errors") match
                    case Some(Json.JArr(items)) => Result.collect(items.map(GraphQLError.parse)).map(Chunk.from)
                    case _                      => Result.succeed(Chunk.empty)
                val extensions = fields.get("extensions") match
                    case Some(Json.JObj(ext)) => ext
                    case _                    => Map.empty
                data.flatMap(d => errors.map(es => GraphQLResponse(d, es, extensions)))
            case other =>
                Result.fail(ApolloParseException(other, "a GraphQL response object"))

    /** Decode `payload` with the operation's codec; the codecs' shape errors are the wire failures.
      * A schema codec's own `ApolloParseException` is replaced by this one, keeping its cause.
      */
    private def decodeData[D](payload: Json, operation: Operation[D])(using Frame): Result[ApolloParseException, D] =
        Result
            .catching[ApolloParseException | DecodeException | SelectionDecodeException | ScalarDecodeException](
                operation.dataCodec.decode(payload)
            )
            .mapFailure { e =>
                val cause: Throwable = e match
                    case parse: ApolloParseException => Maybe(parse.getCause).getOrElse(parse)
                    case other                       => other
                ApolloParseException(payload, s"the data of operation '${operation.name}'", cause)
            }
end GraphQLResponse
