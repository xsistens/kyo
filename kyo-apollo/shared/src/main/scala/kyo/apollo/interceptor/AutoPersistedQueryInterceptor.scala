package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.api.Subscription
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** An [[ApolloInterceptor]] implementing Automatic Persisted Queries (APQ).
  *
  * APQ trades a small negotiation for much smaller request bodies: the client
  * first sends only the SHA-256 hash of the operation document; if the server
  * already has that query registered it answers directly, and the full document
  * never crosses the wire. If the server has not seen the query it replies with a
  * `PersistedQueryNotFound` error, and the client retries **once** sending the
  * hash *and* the document, which registers it for every subsequent call.
  *
  * This interceptor expresses that entirely through the two APQ flags already on
  * [[ApolloRequest]] — the wire mechanics (adding the `persistedQuery` extension,
  * omitting `query`) live in [[kyo.apollo.network.http.HttpRequestComposer]], which
  * hashes the document with the existing [[kyo.apollo.network.http.Sha256]] (the Task
  * 1 reuse doc §2/§4: "APQ reuses it; do not add a second implementation"):
  *   - **hash-only probe** — `sendApqExtensions = true`, `sendDocument = false`.
  *   - **register fallback** — `sendApqExtensions = true`, `sendDocument = true`,
  *     sent only after a `PersistedQueryNotFound`.
  *
  * If the server reports `PersistedQueryNotSupported`, APQ is abandoned for that
  * call and the plain document is sent (`sendApqExtensions = false`,
  * `sendDocument = true`). Transport failures and ordinary GraphQL errors are not
  * APQ signals, so they pass straight through the first response. A
  * [[Subscription]] is never persisted — it flows through untouched.
  *
  * The negotiation errors ride the response's GraphQL `errors` (an HTTP 200 whose
  * body carries an error, per the APQ protocol), matched by message or by the
  * `extensions.code`. Mirrors apollo-kotlin's `AutoPersistedQueryInterceptor`.
  */
final class AutoPersistedQueryInterceptor extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        request.operation match
            case _: Subscription[?] => chain.proceed(request)
            case _ =>
                val probe = request.copy(sendApqExtensions = true, sendDocument = false)
                Stream.init(negotiate(request, probe, chain).map(Seq(_)))

    /** The APQ negotiation as a single-response effect: send the hash-only probe,
      * then re-send with the document (register), fall back to a plain call, or
      * accept the probe response as-is.
      */
    private def negotiate[D](
        request: ApolloRequest[D],
        probe: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ApolloResponse[D] < (Async & Scope) =
        chain.proceed(probe).take(1).run.map(_.head).flatMap { response =>
            if hasError(response, AutoPersistedQueryInterceptor.NotFound) then
                // Server does not know this query yet — resend to register it.
                chain
                    .proceed(request.copy(sendApqExtensions = true, sendDocument = true))
                    .take(1)
                    .run
                    .map(_.head)
            else if hasError(response, AutoPersistedQueryInterceptor.NotSupported) then
                // Server has APQ off — fall back to a plain, document-carrying call.
                chain
                    .proceed(request.copy(sendApqExtensions = false, sendDocument = true))
                    .take(1)
                    .run
                    .map(_.head)
            else response
        }

    /** True when `response` carries a GraphQL error matching `signal`, either by
      * its `message` or its `extensions.code` (both forms appear in the wild).
      */
    private def hasError(
        response: ApolloResponse[?],
        signal: AutoPersistedQueryInterceptor.Signal
    ): Boolean =
        response.errors.exists { error =>
            error.message == signal.message ||
            error.extensions.get("code").contains(Json.JStr(signal.code))
        }
end AutoPersistedQueryInterceptor

object AutoPersistedQueryInterceptor:

    /** One APQ negotiation error, in both the `message` and `extensions.code`
      * spellings a server may use.
      */
    final private case class Signal(message: String, code: String)

    private val NotFound =
        Signal("PersistedQueryNotFound", "PERSISTED_QUERY_NOT_FOUND")

    private val NotSupported =
        Signal("PersistedQueryNotSupported", "PERSISTED_QUERY_NOT_SUPPORTED")
end AutoPersistedQueryInterceptor
