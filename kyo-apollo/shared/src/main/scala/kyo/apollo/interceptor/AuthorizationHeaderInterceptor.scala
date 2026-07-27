package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

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
        request: HttpRequest,
        chain: HttpInterceptorChain
    )(using Frame): HttpResponse < Async =
        val authorized =
            request.copy(headers = request.headers :+ HttpHeader(headerName, value))
        chain.proceed(authorized)
    end intercept
end AuthorizationHeaderInterceptor
