package kyo.apollo.api

import kyo.Chunk
import kyo.Frame
import kyo.Result
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json

/** A single error entry from a GraphQL response's `errors` array.
  *
  * Mirrors the GraphQL spec's error shape:
  *   - `message` — human-readable description (always present on the wire).
  *   - `locations` — source positions in the request document the error relates
  *     to; empty when the server omits them.
  *   - `path` — locates the response field the error applies to. Segments mix
  *     field-name strings and list indices (ints), so this is a
  *     `List[String | Int]` rather than the Phase 01 `List[String]` (which could
  *     not represent an index into a list field).
  *   - `extensions` — server-specific metadata as raw JSON.
  *
  * `locations` is appended last with a default so Phase 01 positional
  * constructions `GraphQLError(message, path, extensions)` keep compiling until
  * Task 7 regenerates the demo. Parse wire JSON via [[GraphQLError.parse]].
  */
final case class GraphQLError(
    message: String,
    path: Chunk[String | Int] = Chunk.empty,
    extensions: Map[String, Json] = Map.empty,
    locations: Chunk[GraphQLError.Location] = Chunk.empty
)

object GraphQLError:

    /** A source position (1-based line/column) inside the request document. */
    final case class Location(line: Int, column: Int)

    /** Parse a single `errors` entry from its raw [[Json]].
      *
      * Tolerant by design: a missing `message` yields the empty string, and
      * missing/malformed `locations`, `path`, or `extensions` degrade to their
      * empty values rather than throwing. Individual malformed `locations`/`path`
      * segments are skipped so one bad entry does not sink the whole error. A
      * non-object entry is a genuinely broken payload: `Result.Failure(ApolloParseException)`.
      */
    def parse(json: Json)(using Frame): Result[ApolloParseException, GraphQLError] = json match
        case Json.JObj(fields) =>
            val message = fields.get("message") match
                case Some(Json.JStr(m)) => m
                case _                  => ""
            val locations = fields.get("locations") match
                case Some(Json.JArr(items)) => items.flatMap(parseLocation)
                case _                      => Chunk.empty
            val path = fields.get("path") match
                case Some(Json.JArr(items)) => items.flatMap(parsePathSegment)
                case _                      => Chunk.empty
            val extensions = fields.get("extensions") match
                case Some(Json.JObj(ext)) => ext
                case _                    => Map.empty
            Result.succeed(GraphQLError(message, path, extensions, locations))
        case other =>
            Result.fail(ApolloParseException(other, "a GraphQL error object"))

    private def parseLocation(json: Json): Option[Location] = json match
        case Json.JObj(fields) =>
            (fields.get("line").flatMap(exactInt), fields.get("column").flatMap(exactInt)) match
                case (Some(line), Some(column)) => Some(Location(line, column))
                case _                          => None
        case _ => None

    private def parsePathSegment(json: Json): Option[String | Int] = json match
        case Json.JStr(name) => Some(name)
        case other           => exactInt(other)

    /** A JSON number with an exact `Int` value. */
    private def exactInt(json: Json): Option[Int] =
        Json.integral(json).filter(_.isValidInt).map(_.toInt).toOption
end GraphQLError
