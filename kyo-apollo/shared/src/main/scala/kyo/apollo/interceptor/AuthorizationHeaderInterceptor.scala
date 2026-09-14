package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine

/** A concrete [[HttpInterceptor]] that attaches an authorization header to every
  * outgoing request before it continues down the chain.
  *
  * The header is appended (preserving any already present), so the composed
  * default headers and per-request headers survive. `headerName` defaults to the
  * conventional `Authorization`; `value` is the full header value the caller
  * supplies (e.g. `"Bearer <token>"`). This is the canonical example of the
  * "auth" concern the HTTP layer owns.
  *
  * @param value      the header value to send (e.g. `"Bearer eyJ..."`)
  * @param headerName the header name (defaults to `"Authorization"`)
  */
final class AuthorizationHeaderInterceptor(
    value: String,
    headerName: String = "Authorization"
) extends HttpInterceptor:

    def intercept(
        request: HttpEngine.Request,
        chain: HttpInterceptorChain
    )(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        chain.proceed(request.addHeader(headerName, value))
end AuthorizationHeaderInterceptor
