package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

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
        request: HttpEngine.Request,
        chain: HttpInterceptorChain
    )(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        for
            _ <- Log.debug(s"--> ${request.method.name} ${request.url.full}")
            _ <- logHeaders(request.headers)
            _ <- request.fields.body match
                case HttpRequestBody.Text(body) => Log.trace(s"    $body")
                case _                          => Kyo.unit
            response <- chain.proceed(request)
            _        <- Log.debug(s"<-- ${response.status.code} (${request.method.name} ${request.url.full})")
            _        <- logHeaders(response.headers)
        yield response
    end intercept

    private def logHeaders(headers: HttpHeaders)(using Frame): Unit < Sync =
        headers.foldLeft(Kyo.unit: Unit < Sync)((logged, name, value) =>
            logged.andThen(Log.trace(s"    $name: ${shown(name, value)}"))
        )

    private def shown(name: String, value: String): String =
        if redact.exists(_.equalsIgnoreCase(name)) then "<redacted>" else value
end LoggingInterceptor

object LoggingInterceptor:

    /** The headers that carry credentials or session state. */
    val sensitiveHeaders: Set[String] = Set("Authorization", "Cookie", "Set-Cookie", "Proxy-Authorization")
end LoggingInterceptor
