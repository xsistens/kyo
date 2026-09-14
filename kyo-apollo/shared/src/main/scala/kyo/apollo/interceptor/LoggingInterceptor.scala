package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** A concrete [[HttpInterceptor]] that logs each request as it goes out and its
  * response as it comes back, without altering either — the request is passed
  * through the chain untouched and the response is returned verbatim.
  *
  * The log sink is injectable (defaulting to `println`) so tests can capture the
  * lines and applications can route them to their own logger. Mirrors the intent
  * of apollo-kotlin's `LoggingInterceptor`.
  *
  * @param log where log lines are written (defaults to standard out)
  */
final class LoggingInterceptor(log: String => Unit = line => println(line)) extends HttpInterceptor:

    def intercept(
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < (Async & Abort[HttpEngineFailure]) =
        log(s"--> ${request.method} ${request.url}")
        request.headers.foreach(h => log(s"    ${h.name}: ${h.value}"))
        request.body.foreach(body => log(s"    $body"))
        chain.proceed(request).map { response =>
            log(s"<-- ${response.statusCode} (${request.method} ${request.url})")
            response
        }
    end intercept
end LoggingInterceptor
