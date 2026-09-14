package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** Intercepts at the **HTTP layer** — the wire level, after an operation has
  * been lowered to an [[HttpRequest]] but before (and after) it reaches the
  * network. This is the place for concerns that are purely about HTTP:
  * attaching headers, authorization, request/response logging, retries.
  *
  * An interceptor either forwards the (possibly modified) request down the
  * [[HttpInterceptorChain]] via `chain.proceed(request)` and observes the
  * resulting [[HttpResponse]], or short-circuits by returning a response
  * without proceeding. Interceptors are invoked in registration order, each
  * wrapping the rest of the chain; the chain terminates at the
  * [[kyo.apollo.network.http.HttpEngine]].
  *
  * Mirrors apollo-kotlin's `HttpInterceptor` (its `suspend intercept` collapses
  * to a `Future` here — Scala.js has no coroutines). See [[LoggingInterceptor]]
  * and [[AuthorizationHeaderInterceptor]] for concrete examples.
  */
trait HttpInterceptor:

    /** Handle `request`, delegating to `chain.proceed` to continue toward the
      * network, and complete with the (observed or produced) [[HttpResponse]]. The
      * row is the engine's: an interceptor may pass on, recover or raise an
      * [[HttpEngineFailure]].
      */
    def intercept(
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < (Async & Abort[HttpEngineFailure])
end HttpInterceptor
