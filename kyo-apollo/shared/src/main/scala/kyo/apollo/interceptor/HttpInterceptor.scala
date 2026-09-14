package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine

/** Intercepts at the **HTTP layer** — the wire level, after an operation has
  * been lowered to an [[HttpEngine.Request]] but before (and after) it reaches the
  * network. This is the place for concerns that are purely about HTTP:
  * attaching headers, authorization, request/response logging, retries.
  *
  * An interceptor either forwards the (possibly modified) request down the
  * [[HttpInterceptorChain]] via `chain.proceed(request)` and observes the
  * resulting [[HttpEngine.Response]], or short-circuits by returning a response
  * without proceeding. Interceptors are invoked in registration order, each
  * wrapping the rest of the chain; the chain terminates at the
  * [[kyo.apollo.network.http.HttpEngine]].
  *
  * Mirrors apollo-kotlin's `HttpInterceptor` (its `suspend intercept` is an
  * `Async` effect here). See [[LoggingInterceptor]] and
  * [[AuthorizationHeaderInterceptor]] for concrete examples.
  */
trait HttpInterceptor:

    /** Handle `request`, delegating to `chain.proceed` to continue toward the
      * network, and complete with the (observed or produced) [[HttpEngine.Response]].
      * The row is the engine's: an interceptor may pass on, recover or raise an
      * [[HttpEngineFailure]].
      */
    def intercept(
        request: HttpEngine.Request,
        chain: HttpInterceptorChain
    )(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure])
end HttpInterceptor
