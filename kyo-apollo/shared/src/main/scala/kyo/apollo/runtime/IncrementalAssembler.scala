package kyo.apollo.runtime

import kyo.*
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.json.JsonPath
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.http.MultipartPart

/** Folds a stream of `multipart/mixed` incremental-delivery parts into a stream of
  * [[ApolloResponse]] — one per part that changes the data, each carrying the
  * *accumulated* (progressively-fuller) response. The part that ends the delivery
  * (`hasNext: false` after `hasNext: true`) always yields exactly one response with
  * `complete = true`: the one its change produces, or — when it changes nothing —
  * the data last emitted, now complete.
  *
  * The initial part seeds `data`; each `@defer` patch is spliced at its `path`
  * into the retained JSON tree (both the `deferSpec=20220824` `incremental: [{data,
  * path}]` shape and the older single `{data, path}` shape), which is then
  * re-decoded through [[GraphQLResponse.parse]] — deferred fields being `Maybe`,
  * the partial tree decodes at every stage.
  *
  * The delivery is an explicit fold over one cell: a pure step turns the accumulated
  * state and a part into the next state or a parse failure. A part that does not
  * fit — a malformed part, an incremental payload without a valid `path`, a path the
  * accumulated tree does not have — ends the delivery with an `ApolloParseException`
  * response (`complete = false`), and the parts after it are ignored. A stream that
  * ends while the last payload still announced `hasNext: true` ends with a
  * truncation error, read from the same cell. Failures stay values; a decoder
  * defect panics.
  */
object IncrementalAssembler:

    /** The accumulated delivery. `awaitingMore` is the last `hasNext` the wire sent
      * (a server that never sends it leaves it false — no false alarm); `failed` is
      * set once a part did not fit.
      */
    final private case class Acc(
        data: Json,
        errors: Chunk[Json],
        extensions: Map[String, Json],
        awaitingMore: Boolean,
        failed: Boolean
    )

    private object Acc:
        val empty: Acc = Acc(Json.JObj(Map.empty), Chunk.empty, Map.empty, awaitingMore = false, failed = false)

    /** One applied part: the next state and whether it is worth a response (it
      * changed the data or errors, or it completes the delivery), or the parse
      * failure that ends the delivery.
      */
    private type Applied = Result[ApolloParseException, (Acc, Boolean)]

    /** Fold `parts` into responses; the part stream's effects (e.g. the engine's
      * failure row for a body that drops) pass through.
      */
    def stream[D, S](
        request: ApolloRequest[D],
        parts: Stream[MultipartPart, S]
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): Stream[ApolloResponse[D], S & Sync] =
        Stream.unwrap {
            AtomicRef.init(Acc.empty).map { cell =>
                parts
                    .mapChunk(chunk => Kyo.foreach(chunk)(part => step(request, cell, part)).map(_.flattenChunk))
                    // kyo-http maps a mid-body drop to a clean end of stream, so `hasNext` is the
                    // only way to tell an incomplete delivery from a complete one.
                    .concat(Stream.unwrap(cell.get.map { acc =>
                        if acc.awaitingMore && !acc.failed then Stream.init(Seq(truncated(request)))
                        else Stream.empty[ApolloResponse[D]]
                    }))
            }
        }

    private def step[D](request: ApolloRequest[D], cell: AtomicRef[Acc], part: MultipartPart)(using
        Frame
    ): Chunk[ApolloResponse[D]] < Sync =
        cell.get.map { acc =>
            if acc.failed then Chunk.empty[ApolloResponse[D]]
            else
                applyPart(acc, part) match
                    case Result.Success((next, changed)) =>
                        cell.set(next).andThen(if changed then response(request, next).map(Chunk(_)) else Chunk.empty[ApolloResponse[D]])
                    case Result.Failure(error) =>
                        cell.set(acc.copy(failed = true)).andThen(Chunk(failure(request, error)))
                    case Result.Panic(cause) => Abort.panic(cause)
        }

    /** Apply one part to the accumulated state. */
    private def applyPart(acc: Acc, part: MultipartPart)(using Frame): Applied =
        part match
            case MultipartPart.Malformed(error) => Result.fail(error)
            case MultipartPart.Payload(Json.JObj(fields)) =>
                val awaitingMore = fields.get("hasNext") match
                    case Some(Json.JBool(more)) => more
                    case _                      => acc.awaitingMore
                val extensions = fields.get("extensions") match
                    case Some(Json.JObj(ext)) => acc.extensions ++ ext
                    case _                    => acc.extensions
                val announced = acc.copy(awaitingMore = awaitingMore, extensions = extensions)
                val patched: Applied =
                    fields.get("incremental") match
                        case Some(Json.JArr(items)) =>
                            items.foldLeft[Applied](Result.succeed((announced, false))) { (applied, item) =>
                                applied.flatMap((next, changed) => applyIncremental(next, changed, item))
                            }
                        case Some(other) => Result.fail(ApolloParseException(other, "an incremental array"))
                        case None        => applyPayload(announced, fields)
                // The part that ends the delivery is always worth a response: a terminal part
                // that changes nothing still has to tell the consumer the data is complete.
                val completes = acc.awaitingMore && !awaitingMore
                patched.map((next, changed) => withErrors(next, changed || completes, fields.get("errors")))
            case MultipartPart.Payload(other) =>
                Result.fail(ApolloParseException(other, "an incremental delivery payload object"))

    /** The initial payload `{data}` seeds the tree; the older single-patch shape
      * `{data, path}` splices at `path`.
      */
    private def applyPayload(acc: Acc, fields: Map[String, Json])(using Frame): Applied =
        (fields.get("data"), fields.get("path")) match
            case (Some(data), Some(path)) => pathOf(path).flatMap(segments => spliceData(acc, path, segments, data))
            case (Some(data), None)       => Result.succeed((acc.copy(data = data), true))
            case (None, _)                => Result.succeed((acc, false))

    /** Apply one item of an `incremental` array: `@defer` items carry `data` (an
      * object merged at `path`), `@stream` items carry `items` (list elements placed
      * at `path`, whose final segment is the start index), and either may carry
      * `errors`. The `path` is required: without it there is nowhere to put the
      * payload, and merging it at the root would overwrite unrelated fields.
      */
    private def applyIncremental(acc: Acc, changed: Boolean, item: Json)(using Frame): Applied =
        item match
            case Json.JObj(inc) =>
                inc.get("path") match
                    case None => Result.fail(ApolloParseException(item, "an incremental payload with a path"))
                    case Some(path) =>
                        pathOf(path).flatMap { segments =>
                            val withData: Applied =
                                inc.get("data") match
                                    case Some(data) => spliceData(acc, path, segments, data)
                                    case None       => Result.succeed((acc, changed))
                            withData
                                .flatMap { (next, nextChanged) =>
                                    inc.get("items") match
                                        case Some(Json.JArr(items)) => spliceItems(next, path, segments, items)
                                        case Some(other) => Result.fail(ApolloParseException(other, "an array of streamed items"))
                                        case None        => Result.succeed((next, nextChanged))
                                }
                                .map((next, nextChanged) => withErrors(next, nextChanged, inc.get("errors")))
                        }
            case other => Result.fail(ApolloParseException(other, "an incremental payload object"))

    private def pathOf(path: Json)(using Frame): Result[ApolloParseException, Chunk[String | Int]] =
        JsonPath.parse(path) match
            case Present(segments) => Result.succeed(segments)
            case Absent            => Result.fail(ApolloParseException(path, "a response path of field names and list indices"))

    private def spliceData(acc: Acc, path: Json, segments: Chunk[String | Int], data: Json)(using Frame): Applied =
        JsonPath.splice(acc.data, segments, data) match
            case Present(tree) => Result.succeed((acc.copy(data = tree), true))
            case Absent =>
                Result.fail(ApolloParseException(path, s"${shown(segments)} to be a path in the accumulated response"))

    private def spliceItems(acc: Acc, path: Json, segments: Chunk[String | Int], items: Chunk[Json])(using Frame): Applied =
        JsonPath.spliceItems(acc.data, segments, items) match
            case Present(tree) => Result.succeed((acc.copy(data = tree), true))
            case Absent =>
                Result.fail(ApolloParseException(
                    path,
                    s"${shown(segments)} to be the path of a list in the accumulated response, ending at its size"
                ))

    /** A parsed response path for a message: response keys and list indices, which come
      * from the operation's selections, never from its data.
      */
    private def shown(segments: Chunk[String | Int]): String =
        if segments.isEmpty then "the root path" else segments.mkString(".")

    /** Accumulate a part's `errors`; an errors-only item (a deferred or streamed field
      * that resolved to an error) counts as a change so it surfaces even on the
      * terminal part.
      */
    private def withErrors(acc: Acc, changed: Boolean, errors: Option[Json]): (Acc, Boolean) =
        errors match
            case Some(Json.JArr(es)) => (acc.copy(errors = acc.errors.concat(es)), true)
            case _                   => (acc, changed)

    private def response[D](request: ApolloRequest[D], acc: Acc)(using Frame): ApolloResponse[D] < Sync =
        val envelope =
            Map("data" -> acc.data)
                ++ (if acc.errors.isEmpty then Map.empty[String, Json] else Map("errors" -> Json.JArr(acc.errors)))
                ++ (if acc.extensions.isEmpty then Map.empty[String, Json] else Map("extensions" -> Json.JObj(acc.extensions)))
        GraphQLResponse.parse(Json.JObj(envelope), request.operation) match
            case Result.Success(gql) =>
                ApolloResponse
                    .fromGraphQLResponse(request.requestUuid, gql, request.executionContext)
                    // Still growing while the wire has announced more payloads.
                    .copy(complete = !acc.awaitingMore)
            case Result.Failure(parseFailure) =>
                ApolloResponse
                    .fromException(request.requestUuid, parseFailure, request.executionContext)
                    .copy(complete = !acc.awaitingMore)
            case Result.Panic(cause) => Abort.panic(cause)
        end match
    end response

    private def failure[D](request: ApolloRequest[D], error: ApolloException): ApolloResponse[D] =
        ApolloResponse.fromException(request.requestUuid, error, request.executionContext).copy(complete = false)

    private def truncated[D](request: ApolloRequest[D])(using Frame): ApolloResponse[D] =
        failure(
            request,
            ApolloNetworkException(message =
                "Incremental delivery stream ended before its final payload (hasNext=false): the response was truncated"
            )
        )
end IncrementalAssembler
