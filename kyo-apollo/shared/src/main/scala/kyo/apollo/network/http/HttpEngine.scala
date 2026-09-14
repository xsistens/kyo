package kyo.apollo.network.http

import kyo.*
import kyo.apollo.exception.HttpEngineFailure

/** The seam between [[HttpNetworkTransport]] and the actual wire.
  *
  * The transport owns GraphQL concerns (composing the request, decoding the
  * envelope, mapping failures to `ApolloResponse.error`); an `HttpEngine`
  * owns only the raw round-trip. Splitting it out lets the platform engine hit the
  * network in production while unit tests inject a deterministic fake with no
  * server.
  *
  * The engine speaks kyo-http's types: an [[HttpEngine.Request]] is a
  * `kyo.HttpRequest` whose one field is the [[HttpRequestBody]], an
  * [[HttpEngine.Response]] a `kyo.HttpResponse` whose one field is the body text.
  * The seam exists for the transport choice on JS: kyo-http's only JS transport is
  * bound to Node raw sockets (`node:net`/`node:tls`) and is not browser-capable, so
  * the JS/Wasm engine sends through the browser/Node `fetch`, while the JVM/Native
  * engine delegates to `kyo.HttpClient`.
  */
trait HttpEngine:

    /** Send `request` and complete with its buffered [[HttpEngine.Response]]. The
      * effect aborts with an [[HttpEngineFailure]] only when no response was received
      * (the connection failed, the request could not be sent); a non-2xx status still
      * completes successfully — status interpretation is the transport's job, not
      * the engine's. Interrupting the effect cancels the request. Anything else an
      * engine raises is a defect and stays a panic.
      */
    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure])

    /** Send `request` and complete with a possibly-streamed [[HttpEngine.StreamResponse]]
      * — the incremental-delivery (`@defer`) round-trip, with the same failure row as
      * [[execute]]; a body that drops mid-stream aborts the chunk stream with it. The
      * request lives until the `Scope` closes. The default buffers via [[execute]]
      * (so every non-streaming engine keeps working unchanged); the production
      * engines override it to read a `multipart/mixed` body as a live chunk stream.
      */
    def executeStreaming(request: HttpEngine.Request)(using
        Frame
    ): HttpEngine.StreamResponse < (Async & Scope & Abort[HttpEngineFailure]) =
        execute(request).map(r => HttpEngine.streamResponse(r.status, HttpStreamBody.Buffered(r.fields.body), r.headers))
end HttpEngine

object HttpEngine:

    /** The platform's production engine, resolved per-platform via
      * [[HttpEnginePlatform]]: the browser/Node `fetch`-backed `FetchHttpEngine` on
      * JS/Wasm, a `kyo-http`-backed `HttpClientEngine` on JVM/Native.
      */
    def default(): HttpEngine = HttpEnginePlatform.default()

    /** A request with `method`, `url`, `headers` and `body`. */
    def request(method: HttpMethod, url: HttpUrl, headers: HttpHeaders, body: HttpRequestBody): Request =
        HttpRequest(method, url, headers, Record.empty).addField("body", body)

    /** A buffered response with `status`, `body` text and `headers`. */
    def response(status: HttpStatus, body: String, headers: HttpHeaders = HttpHeaders.empty): Response =
        HttpResponse(status, headers, Record.empty).addField("body", body)

    /** A possibly-streamed response with `status`, `body` and `headers`. */
    def streamResponse(status: HttpStatus, body: HttpStreamBody, headers: HttpHeaders = HttpHeaders.empty): StreamResponse =
        HttpResponse(status, headers, Record.empty).addField("body", body)

    /** What an engine sends: kyo-http's request carrying the [[HttpRequestBody]] as its
      * `body` field.
      */
    type Request = HttpRequest["body" ~ HttpRequestBody]

    /** What [[HttpEngine.execute]] returns: kyo-http's response with the body read as
      * text, the shape `kyo.HttpClient` produces for a `bodyText` response.
      */
    type Response = HttpResponse["body" ~ String]

    /** What [[HttpEngine.executeStreaming]] returns: kyo-http's response with a
      * buffered or chunked [[HttpStreamBody]].
      */
    type StreamResponse = HttpResponse["body" ~ HttpStreamBody]
end HttpEngine
