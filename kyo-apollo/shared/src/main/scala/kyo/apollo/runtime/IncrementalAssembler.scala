package kyo.apollo.runtime

import kyo.*
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.json.Json
import kyo.apollo.json.JsonPath
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.http.MultipartPart

/** Folds a stream of `multipart/mixed` incremental-delivery parts into a stream of
  * [[ApolloResponse]] — one per part that changes the data, each carrying the
  * *accumulated* (progressively-fuller) response.
  *
  * The initial part seeds `data`; each `@defer` patch is spliced at its `path`
  * into the retained JSON tree (both the `deferSpec=20220824` `incremental: [{data,
  * path}]` shape and the older single `{data, path}` shape), which is then
  * re-decoded through [[GraphQLResponse.parse]] — deferred fields being `Maybe`,
  * the partial tree decodes at every stage. Failures stay values (a part whose
  * accumulated tree does not decode becomes an `ApolloParseException` response, not a
  * throw); a decoder defect panics.
  */
object IncrementalAssembler:

    def stream[D](
        request: ApolloRequest[D],
        parts: Stream[MultipartPart, Async & Scope]
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap {
            Sync.defer {
                var data: Json                    = Json.JObj(Map.empty)
                var errors: List[Json]            = Nil
                var extensions: Map[String, Json] = Map.empty
                // Each incremental payload carries `hasNext`; the terminal one is the
                // only `false`. `awaitingMore` stays true after a `hasNext: true` part,
                // so a stream that ends while it is still true was truncated (kyo-http
                // maps a mid-body drop to a clean EOF, so this protocol signal is the
                // only way apollo can tell an incomplete delivery from a complete one).
                // A server that never sends `hasNext` leaves it false — no false alarm.
                var awaitingMore = false

                // Apply one part to the accumulator; return whether it changed anything
                // worth emitting a response for.
                def applyPart(part: Json): Boolean = part match
                    case Json.JObj(fields) =>
                        fields.get("hasNext") match
                            case Some(Json.JBool(b)) => awaitingMore = b
                            case _                   => ()
                        var changed = false
                        fields.get("extensions") match
                            case Some(Json.JObj(ext)) => extensions = extensions ++ ext
                            case _                    => ()
                        fields.get("incremental") match
                            case Some(Json.JArr(items)) =>
                                items.foreach {
                                    case Json.JObj(inc) =>
                                        val path = inc.get("path").map(JsonPath.parse).getOrElse(Nil)
                                        // `@defer` patches carry `data` (an object merged at `path`);
                                        // `@stream` patches carry `items` (list entries appended at
                                        // `path`, whose final segment is the start index).
                                        inc.get("data").foreach { d =>
                                            data = JsonPath.splice(data, path, d)
                                            changed = true
                                        }
                                        inc.get("items") match
                                            case Some(Json.JArr(newItems)) =>
                                                data = JsonPath.spliceItems(data, path, newItems.toList)
                                                changed = true
                                            case _ => ()
                                        end match
                                        // An incremental entry may carry `errors` with no data/items (a
                                        // deferred/streamed field that resolved to an error); mark it
                                        // changed so it surfaces even if it rides the terminal part.
                                        inc.get("errors") match
                                            case Some(Json.JArr(es)) => errors = errors ++ es.toList; changed = true
                                            case _                   => ()
                                    case _ => ()
                                }
                            case _ =>
                                fields.get("data").foreach { d =>
                                    fields.get("path").map(JsonPath.parse) match
                                        case Some(path) => data = JsonPath.splice(data, path, d)
                                        case None       => data = d
                                    changed = true
                                }
                        end match
                        fields.get("errors") match
                            case Some(Json.JArr(es)) => errors = errors ++ es.toList; changed = true
                            case _                   => ()
                        changed
                    case _ => false

                // The accumulated state is read when `response()` is called, so each result
                // is computed eagerly right after its part is applied; only a decoder
                // defect is a suspended panic.
                def response(): ApolloResponse[D] < Sync =
                    val env = Map.newBuilder[String, Json]
                    env += "data"                                   -> data
                    if errors.nonEmpty then env += "errors"         -> Json.JArr(Chunk.from(errors))
                    if extensions.nonEmpty then env += "extensions" -> Json.JObj(extensions)
                    GraphQLResponse.parse(Json.JObj(env.result()), request.operation) match
                        case Result.Success(gql) =>
                            ApolloResponse
                                .fromGraphQLResponse(request.requestUuid, gql, request.executionContext)
                                // Still growing while the wire has announced more payloads.
                                .copy(complete = !awaitingMore)
                        case Result.Failure(parseFailure) =>
                            ApolloResponse.fromException(request.requestUuid, parseFailure, request.executionContext)
                        case Result.Panic(cause) => Abort.panic(cause)
                    end match
                end response

                val mapped =
                    parts.mapChunk { partChunk =>
                        Kyo.collectAll(partChunk.toList.flatMap(part => if applyPart(part.json) then Seq(response()) else Nil))
                    }
                // If the stream ended while a `hasNext: true` was still outstanding, the
                // incremental delivery was truncated — surface a terminal exception value
                // rather than presenting the partial data as a complete response.
                mapped.concat(Stream.unwrap(Sync.defer {
                    if awaitingMore then
                        Stream.init(Seq(ApolloResponse.fromException(
                            request.requestUuid,
                            ApolloNetworkException(message =
                                "Incremental delivery stream ended before its final payload (hasNext=false): the response was truncated"
                            ),
                            request.executionContext
                        )))
                    else Stream.empty[ApolloResponse[D]]
                }))
            }
        }
end IncrementalAssembler
