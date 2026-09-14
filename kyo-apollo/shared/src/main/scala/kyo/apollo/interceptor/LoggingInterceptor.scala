package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.devtools.DevtoolsInterceptor
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

/** A concrete [[HttpInterceptor]] that logs each request as it goes out and its
  * response as it comes back, without altering either — the request is passed
  * through the chain untouched and the response is returned verbatim.
  *
  * Lines go to the ambient [[kyo.Log]], so the application's logger and level decide
  * where they land and which are kept: the request line and the response status at
  * `debug`, the headers and the request body at `trace`. Logging is part of the
  * effect: a request is logged when the interceptor runs, and a request that is built
  * but never sent logs nothing. Mirrors the intent of apollo-kotlin's
  * `LoggingInterceptor`.
  *
  * What never reaches the log:
  *   - the value of every header named in `redact` (compared case-insensitively),
  *     written as `<redacted>`;
  *   - the value of every operation variable: a JSON body is logged with its
  *     `variables` passed through [[DevtoolsInterceptor.redactValues]] (names, nesting
  *     and `null`s stay, every value becomes `"<redacted>"`), each element of a
  *     batched array alike. The interceptor sees the wire body, not the operation, so
  *     it cannot tell a mutation's credentials from a query's arguments and redacts
  *     both;
  *   - a body that is not JSON (a multipart upload, anything unparseable), which is
  *     logged as its kind and length only;
  *   - the URL's query string, where a `GET` carries its variables: the request and
  *     status lines name the URL without it.
  *
  * WARNING: a literal written into the operation document (`login(password: "…")`
  * instead of a variable) is part of `query` and is logged as written.
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
            _        <- Log.debug(s"--> ${request.method.name} ${request.url.baseUrl}")
            _        <- logHeaders(request.headers)
            _        <- shownBody(request.fields.body).fold(Kyo.unit)(body => Log.trace(s"    $body"))
            response <- chain.proceed(request)
            _        <- Log.debug(s"<-- ${response.status.code} (${request.method.name} ${request.url.baseUrl})")
            _        <- logHeaders(response.headers)
        yield response
    end intercept

    private def logHeaders(headers: HttpHeaders)(using Frame): Unit < Sync =
        headers.foldLeft(Kyo.unit: Unit < Sync)((logged, name, value) =>
            logged.andThen(Log.trace(s"    $name: ${shown(name, value)}"))
        )

    private def shown(name: String, value: String): String =
        if redact.exists(_.equalsIgnoreCase(name)) then "<redacted>" else value

    /** The body as it may be logged: JSON with its variables redacted, anything else
      * as its kind and length; nothing for an empty body.
      */
    private def shownBody(body: HttpRequestBody)(using Frame): Maybe[String] =
        body match
            case HttpRequestBody.Empty => Absent
            case HttpRequestBody.Text(text) =>
                JsonParser.parse(text) match
                    case Result.Success(json) => Present(LoggingInterceptor.withoutVariableValues(json).render)
                    case _                    => Present(s"text body, ${text.length} characters, not JSON")
            case HttpRequestBody.Multipart(parts) =>
                Present(s"multipart/form-data body, ${parts.length} parts, ${parts.foldLeft(0L)(_ + _.data.size)} bytes")
end LoggingInterceptor

object LoggingInterceptor:

    /** The headers that carry credentials or session state. */
    val sensitiveHeaders: Set[String] = Set("Authorization", "Cookie", "Set-Cookie", "Proxy-Authorization")

    /** `json` (a request body, or a batch of them) with the value of every operation's
      * `variables` redacted by [[DevtoolsInterceptor.redactValues]].
      */
    private def withoutVariableValues(json: Json): Json =
        json match
            case Json.JObj(fields) =>
                Json.JObj(fields.map((name, value) =>
                    if name == "variables" then name -> DevtoolsInterceptor.redactValues(value) else name -> value
                ))
            case Json.JArr(items) => Json.JArr(items.map(withoutVariableValues))
            case other            => other
end LoggingInterceptor
