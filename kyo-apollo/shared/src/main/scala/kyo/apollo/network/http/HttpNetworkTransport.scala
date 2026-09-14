package kyo.apollo.network.http

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.IncrementalAssembler
import kyo.apollo.runtime.ResponseStream

/** The terminal HTTP transport: takes an [[ApolloRequest]], sends it over an
  * [[HttpEngine]], and returns a fully-decoded [[ApolloResponse]].
  *
  * It is the single place where failures become values (Phase 03's contract):
  * every network drop, non-2xx status, or unparseable body is caught here and
  * folded into `ApolloResponse.error`, so the returned `Future` **never
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
    engine: HttpEngine = HttpEngine.default(),
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
        foldStreamFailure(request) {
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
                                        Stream.unwrap(decodeSingle(request, text).map(r => Stream.init(Seq(r))))
                                    case HttpStreamBody.Chunked(chunks) =>
                                        Stream.unwrap(
                                            chunks.run
                                                .map(cs => decodeSingle(request, cs.mkString))
                                                .map(r => Stream.init(Seq(r)))
                                        )
                    case Result.Failure(cause) =>
                        Stream.init(Seq(failure(request, ApolloNetworkException(cause = cause))))
                    case Result.Panic(cause) =>
                        Stream.init(Seq(failure(request, ApolloNetworkException(cause = cause))))
                }
            }
        }
    end executeStreaming

    /** Fold any failure raised *while the body stream is being consumed* into a
      * terminal [[ApolloNetworkException]] response appended after whatever was already
      * emitted — the streaming counterpart of [[execute]]'s `Abort.run`. The initial
      * round-trip failure is already a value (see the `Result.Failure`/`Panic` arms
      * above); this covers a live body that drops mid-stream (the JVM/Native engine
      * re-raises such a drop through its body stream), keeping "failures are values"
      * without buffering — chunks emitted before the drop still reach the caller.
      */
    private def foldStreamFailure[D](request: ApolloRequest[D])(stream: ResponseStream[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        Stream {
            Abort.run[Throwable](stream.emit).map {
                case Result.Success(_) => ()
                case Result.Failure(cause) =>
                    Emit.value(Chunk(failure(request, ApolloNetworkException(cause = cause))))
                case Result.Panic(cause) =>
                    Emit.value(Chunk(failure(request, ApolloNetworkException(cause = cause))))
            }
        }

    private def isMultipart(contentType: String): Boolean =
        contentType.toLowerCase.contains("multipart/mixed")

    /** Turn a received [[HttpResponse]] into an [[ApolloResponse]]: reject non-2xx
      * statuses, otherwise parse the body (catching parse failures as values).
      */
    private def decode[D](
        request: ApolloRequest[D],
        httpResponse: HttpResponse
    )(using Frame): ApolloResponse[D] < Sync =
        if httpResponse.isSuccessful then decodeSingle(request, httpResponse.body)
        else
            // GraphQL-over-HTTP: `application/graphql-response+json` means the body IS a
            // well-formed GraphQL response whatever the status — a 4xx carrying the
            // server's typed `errors` is far more useful than an opaque status. Only a
            // body that fails to parse (or a legacy `application/json` server, where a
            // non-2xx says nothing about the body) degrades to the status exception.
            val graphqlBody: Maybe[ApolloResponse[D]] < Sync =
                if isGraphQLResponse(httpResponse.header("Content-Type").getOrElse("")) then
                    decodeGraphQLResponse(request, httpResponse.body)
                else Absent
            graphqlBody.map(_.getOrElse(
                failure(
                    request,
                    ApolloHttpException(
                        statusCode = httpResponse.statusCode,
                        headers = httpResponse.headers,
                        message = s"HTTP request failed with status ${httpResponse.statusCode}"
                    )
                )
            ))
        end if
    end decode

    private def isGraphQLResponse(contentType: String): Boolean =
        contentType.toLowerCase.contains("application/graphql-response+json")

    /** Decode `body` as a GraphQL envelope, or [[Absent]] if it is not one. Unlike
      * [[decodeSingle]] a parse failure is not an [[ApolloParseException]] here: on a
      * non-2xx it just means the body was never a GraphQL response, and the caller
      * falls back to the HTTP status. A decoder defect still panics.
      */
    private def decodeGraphQLResponse[D](
        request: ApolloRequest[D],
        body: String
    )(using Frame): Maybe[ApolloResponse[D]] < Sync =
        parseBody(request, body) match
            case Result.Success(response) =>
                Present(ApolloResponse.fromGraphQLResponse(request.requestUuid, response, request.executionContext))
            case Result.Failure(_)   => Absent
            case Result.Panic(cause) => Abort.panic(cause)

    /** Parse a single GraphQL response envelope: a parse failure becomes an
      * [[ApolloParseException]] value, a decoder defect panics.
      */
    private def decodeSingle[D](
        request: ApolloRequest[D],
        body: String
    )(using Frame): ApolloResponse[D] < Sync =
        parseBody(request, body) match
            case Result.Success(response) =>
                ApolloResponse.fromGraphQLResponse(request.requestUuid, response, request.executionContext)
            case Result.Failure(parseFailure) => failure(request, parseFailure)
            case Result.Panic(cause)          => Abort.panic(cause)

    /** Read `body` as JSON and then as a GraphQL envelope of the request's operation.
      * Text that is not JSON and an envelope of the wrong shape are failures; anything
      * else the decoders throw is a `Result.Panic`.
      */
    private def parseBody[D](
        request: ApolloRequest[D],
        body: String
    )(using Frame): Result[ApolloParseException, GraphQLResponse[D]] =
        Result
            .catching[DecodeException](JsonParser.parse(body))
            .mapFailure(e => ApolloParseException(Json.JStr(body), "a JSON document", e))
            .flatMap(json => GraphQLResponse.parse(json, request.operation))

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
