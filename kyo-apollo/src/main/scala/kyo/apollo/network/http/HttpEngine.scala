package kyo.apollo.network.http

import kyo.*

/** The seam between [[HttpNetworkTransport]] and the actual wire.
  *
  * The transport owns GraphQL concerns (composing the request, decoding the
  * envelope, mapping failures to `ApolloResponse.exception`); an `HttpEngine`
  * owns only the raw round-trip `HttpRequest => HttpResponse < Async`. Splitting
  * it out lets [[FetchHttpEngine]] hit the network in production while unit
  * tests inject a deterministic fake with no server.
  *
  * Effect pivot (Schritt 2.2): the round-trip is a kyo `Async` effect rather
  * than a `Future`. The engine seam stays — kyo-http was rejected: it is
  * cross-published for Scala.js, but its only JS transport engine is bound to
  * Node raw sockets (`node:net`/`node:tls`), so it is not browser-capable.
  * `FetchHttpEngine` keeps using the browser/Node `fetch`, bridged into `Async`.
  */
trait HttpEngine:

    /** Send `request` and complete with the raw [[HttpResponse]]. The effect fails
      * (on the async Throwable channel) only when no response was received
      * (connection error); a non-2xx status still completes successfully — status
      * interpretation is the transport's job, not the engine's.
      */
    def execute(request: HttpRequest)(using Frame): HttpResponse < Async

    /** Send `request` and complete with a possibly-streamed [[HttpStreamResponse]] —
      * the incremental-delivery (`@defer`) round-trip. The default buffers via
      * [[execute]] (so every non-streaming engine keeps working unchanged); only
      * [[FetchHttpEngine]] overrides it to read a `multipart/mixed` body as a live
      * chunk stream.
      */
    def executeStreaming(request: HttpRequest)(using Frame): HttpStreamResponse < (Async & Scope) =
        execute(request).map(r =>
            HttpStreamResponse(r.statusCode, r.headers, HttpStreamBody.Buffered(r.body))
        )
end HttpEngine
