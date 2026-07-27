package kyo.apollo.network.http

import kyo.{HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod

/** The JVM/Native production [[HttpEngine]]: issues requests through kyo-http's
  * `Async` HTTP client (over kyo-net sockets), the counterpart to JS/Wasm's
  * `FetchHttpEngine`.
  *
  * kyo-http's body methods fail with `HttpStatusException` on non-2xx, but apollo's
  * contract is that a non-2xx status still *completes* (the transport interprets
  * status), so the `*Response` variants are used with `failOnError = false`. A
  * genuine transport failure (`Abort[HttpException]`) is re-raised as a panic, which
  * `HttpNetworkTransport` folds into an `ApolloNetworkException` value.
  *
  * Buffered only for now — the incremental `@defer` (`multipart/mixed`) path inherits
  * [[HttpEngine.executeStreaming]]'s buffered default; true streaming over kyo-http
  * is a follow-up.
  *
  * `import kyo.*` is deliberately avoided: kyo-http also defines `HttpRequest` /
  * `HttpResponse`, which a wildcard import would shadow over apollo's same-named,
  * same-package types.
  */
final class HttpClientEngine extends HttpEngine:

    def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
        val headers = request.headers.map(h => h.name -> h.value)
        val call =
            request.method match
                case HttpMethod.Get =>
                    HttpClient.getTextResponse(request.url, headers, failOnError = false)
                case HttpMethod.Post =>
                    HttpClient.postTextResponse(request.url, request.body.getOrElse(""), headers, failOnError = false)
        Abort.run[HttpException](call).map {
            case Result.Success(resp) =>
                val hs = List.newBuilder[HttpHeader]
                resp.headers.foreach((n, v) => hs += HttpHeader(n, v))
                HttpResponse(resp.status.code, hs.result(), resp.fields.body)
            case Result.Failure(e) => Sync.defer(throw e)
            case Result.Panic(e)   => Sync.defer(throw e)
        }
    end execute
end HttpClientEngine
