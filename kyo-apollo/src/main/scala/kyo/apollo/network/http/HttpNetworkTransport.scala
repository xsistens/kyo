package kyo.apollo.network.http

import kyo.*
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.JsonParser
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.IncrementalAssembler
import kyo.apollo.runtime.ResponseStream
import scala.util.control.NonFatal

/** The terminal HTTP transport: takes an [[ApolloRequest]], sends it over an
  * [[HttpEngine]], and returns a fully-decoded [[ApolloResponse]].
  *
  * It is the single place where failures become values (Phase 03's contract):
  * every network drop, non-2xx status, or unparseable body is caught here and
  * folded into `ApolloResponse.exception`, so the returned `Future` **never
  * fails** for those conditions.
  *
  *   - connection error / `fetch` rejection → [[ApolloNetworkException]]
  *   - HTTP status outside 2xx             → [[ApolloHttpException]]
  *   - body not a valid GraphQL envelope   → [[ApolloParseException]]
  *
  * Serialization stays delegated: [[HttpRequestComposer]] owns request-body
  * composition and [[GraphQLResponse.parse]] owns response decoding — the
  * transport only wires them to the engine and maps errors.
  *
  * @param serverUrl the GraphQL endpoint every request is sent to
  * @param engine    the wire round-trip (defaults to the real [[FetchHttpEngine]];
  *                  tests inject a fake)
  * @param composer  the [[ApolloRequest]] → [[HttpRequest]] lowering
  */
final class HttpNetworkTransport(
    serverUrl: String,
    engine: HttpEngine = FetchHttpEngine(),
    composer: HttpRequestComposer = HttpRequestComposer()
):

    /** Execute `request`, yielding its [[ApolloResponse]]. Never raises for
      * network/HTTP/parse conditions — those arrive in `exception`.
      *
      * Effect pivot (Slice 2): the engine + HTTP interceptors are kyo (`< Async`)
      * and this returns the response as a Kyo value directly (the Slice 1
      * `KyoInterop`/`Future` bridge is gone). A connection error surfaces on the
      * async Throwable channel and is folded to an `ApolloNetworkException` value.
      */
    def execute[D](request: ApolloRequest[D])(using Frame): ApolloResponse[D] < Async =
        val httpRequest = composer.compose(serverUrl, request)
        // `Abort.run[Throwable]` captures BOTH an abort-failure AND a panic as a
        // `Result` — critical because a `fetch` rejection arrives via
        // `Async.fromFuture` on the PANIC channel, which a plain `Abort.recover`
        // would re-raise (letting a real network drop escape uncaught). Folding
        // both keeps the "failures are values" contract for the production engine.
        Abort
            .run[Throwable] {
                engine.execute(httpRequest).map(httpResponse => decode(request, httpResponse))
            }
            .map {
                case Result.Success(decoded) => decoded
                case Result.Failure(cause) =>
                    failure(request, ApolloNetworkException(cause = cause))
                case Result.Panic(cause) =>
                    failure(request, ApolloNetworkException(cause = cause))
            }
    end execute

    /** Execute `request` as an incremental-delivery (`@defer`) operation, yielding a
      * [[ResponseStream]] of progressively-fuller [[ApolloResponse]]s. The engine
      * reads a `multipart/mixed` body (streamed off the `fetch` reader when the
      * production engine, buffered otherwise); the parts are split
      * ([[MultipartParser]]) and folded ([[IncrementalAssembler]]) into one response
      * per patch. A server that ignores `@defer` (non-multipart Content-Type) or a
      * non-2xx / network failure collapses to a single response, keeping the
      * "failures are values" contract.
      */
    def executeStreaming[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        val httpRequest = composer.compose(serverUrl, request)
        Stream.unwrap {
            Abort.run[Throwable](engine.executeStreaming(httpRequest)).map {
                case Result.Success(resp) if !resp.isSuccessful =>
                    Stream.init(
                        Seq(
                            failure(
                                request,
                                ApolloHttpException(
                                    statusCode = resp.statusCode,
                                    headers = resp.headers,
                                    message = s"HTTP request failed with status ${resp.statusCode}"
                                )
                            )
                        )
                    )
                case Result.Success(resp) =>
                    resp.header("Content-Type") match
                        case Some(ct) if isMultipart(ct) =>
                            val boundary = MultipartParser.boundaryOf(ct)
                            val partStream = resp.body match
                                case HttpStreamBody.Chunked(chunks) => MultipartParser.parts(boundary, chunks)
                                case HttpStreamBody.Buffered(text) =>
                                    Stream.init(MultipartParser.parts(boundary, text))
                            IncrementalAssembler.stream(request, partStream)
                        case _ =>
                            // The server ignored @defer (a plain JSON reply): one response.
                            resp.body match
                                case HttpStreamBody.Buffered(text) =>
                                    Stream.init(Seq(decodeSingle(request, text)))
                                case HttpStreamBody.Chunked(chunks) =>
                                    Stream.unwrap(
                                        chunks.run.map(cs => Stream.init(Seq(decodeSingle(request, cs.mkString))))
                                    )
                case Result.Failure(cause) =>
                    Stream.init(Seq(failure(request, ApolloNetworkException(cause = cause))))
                case Result.Panic(cause) =>
                    Stream.init(Seq(failure(request, ApolloNetworkException(cause = cause))))
            }
        }
    end executeStreaming

    private def isMultipart(contentType: String): Boolean =
        contentType.toLowerCase.contains("multipart/mixed")

    /** Turn a received [[HttpResponse]] into an [[ApolloResponse]]: reject non-2xx
      * statuses, otherwise parse the body (catching parse failures as values).
      */
    private def decode[D](
        request: ApolloRequest[D],
        httpResponse: HttpResponse
    ): ApolloResponse[D] =
        if !httpResponse.isSuccessful then
            failure(
                request,
                ApolloHttpException(
                    statusCode = httpResponse.statusCode,
                    headers = httpResponse.headers,
                    message = s"HTTP request failed with status ${httpResponse.statusCode}"
                )
            )
        else decodeSingle(request, httpResponse.body)

    /** Parse a single GraphQL response envelope, folding a parse failure to a value. */
    private def decodeSingle[D](
        request: ApolloRequest[D],
        body: String
    ): ApolloResponse[D] =
        try
            val json     = JsonParser.parse(body)
            val response = GraphQLResponse.parse(json, request.operation)
            ApolloResponse.fromGraphQLResponse(
                request.requestUuid,
                response,
                request.executionContext
            )
        catch
            case NonFatal(cause) =>
                failure(request, ApolloParseException(cause = cause))

    private def failure[D](
        request: ApolloRequest[D],
        exception: kyo.apollo.exception.ApolloException
    ): ApolloResponse[D] =
        ApolloResponse.fromException(
            request.requestUuid,
            exception,
            request.executionContext
        )
end HttpNetworkTransport
