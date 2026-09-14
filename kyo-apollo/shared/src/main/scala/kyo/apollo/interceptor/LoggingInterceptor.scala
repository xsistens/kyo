package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** A concrete [[HttpInterceptor]] that logs each request as it goes out and its
  * response as it comes back, without altering either — the request is passed
  * through the chain untouched and the response is returned verbatim.
  *
  * Lines go to the ambient [[kyo.Log]], so the application's logger and level decide
  * where they land and which are kept: the request line and the response status at
  * `debug`, the headers and the request body at `trace`. The value of every header
  * named in `redact` (compared case-insensitively) is written as `<redacted>`.
  * Logging is part of the effect: a request is logged when the interceptor runs, and
  * a request that is built but never sent logs nothing. Mirrors the intent of
  * apollo-kotlin's `LoggingInterceptor`.
  *
  * @param redact header names whose values never reach the log (defaults to
  *               [[LoggingInterceptor.sensitiveHeaders]])
  */
final class LoggingInterceptor(redact: Set[String] = LoggingInterceptor.sensitiveHeaders) extends HttpInterceptor:

    def intercept(
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < (Async & Abort[HttpEngineFailure]) =
        for
            _        <- Log.debug(s"--> ${request.method} ${request.url}")
            _        <- logHeaders(request.headers)
            _        <- request.body.fold(Kyo.unit)(body => Log.trace(s"    $body"))
            response <- chain.proceed(request)
            _        <- Log.debug(s"<-- ${response.statusCode} (${request.method} ${request.url})")
            _        <- logHeaders(response.headers)
        yield response
    end intercept

    private def logHeaders(headers: List[HttpHeader])(using Frame): Unit < Sync =
        Kyo.foreachDiscard(headers)(h => Log.trace(s"    ${h.name}: ${shown(h)}"))

    private def shown(header: HttpHeader): String =
        if redact.exists(_.equalsIgnoreCase(header.name)) then "<redacted>" else header.value
end LoggingInterceptor

object LoggingInterceptor:

    /** The headers that carry credentials or session state. */
    val sensitiveHeaders: Set[String] = Set("Authorization", "Cookie", "Set-Cookie", "Proxy-Authorization")
end LoggingInterceptor
