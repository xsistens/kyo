package kyo.apollo.network

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.json.Json

/** The transport-level result of executing an [[ApolloRequest]].
  *
  * Wraps [[GraphQLResponse]]'s `data` and `extensions` **under the same names**
  * so there is no parallel response model, and adds transport metadata: the
  * correlating [[requestUuid]], the [[executionContext]], and — crucially — a
  * single [[error]] channel.
  *
  * ==One error channel==
  *
  * Every way an operation can fall short of a clean result lands in [[error]]:
  * a network drop, a non-2xx status, a malformed body, a cache miss, and the
  * server's own GraphQL `errors` (as an [[ApolloGraphQLException]]). There is
  * no second field to also consult, so "did this succeed" is one question with
  * one answer, and a response cannot represent the contradictory state of
  * carrying a transport failure and server-side errors at once. This is Apollo
  * Client 4's consolidation; the typed GraphQL errors remain reachable through
  * the [[errors]] projection or by matching [[ApolloGraphQLException]] directly.
  *
  * Failures stay values, not throws: a failed call surfaces as
  * `error = Present(...)` rather than escaping the public API.
  * [[GraphQLResponse.parse]] remains the single response decoder; construct the
  * server-answered case with [[ApolloResponse.fromGraphQLResponse]] rather than
  * re-walking the JSON.
  *
  * @param requestUuid       the id of the [[ApolloRequest]] that produced this
  * @param data              the decoded payload, absent on a failed call and on
  *                          a response whose errors left no usable result
  * @param error             the single failure channel, absent on a clean call
  * @param extensions        top-level response `extensions`, passed through raw
  * @param executionContext  metadata accumulated across the interceptor chain
  * @param cacheInfo         how the normalized cache produced this response, when
  *                          it flowed through the cache interceptor (Phase 04);
  *                          absent for a plain networked response
  * @param complete          false while an incremental delivery (`@defer` /
  *                          `@stream`) still has payloads outstanding (the wire
  *                          `hasNext: true`) — Apollo Client 4's
  *                          `dataState: "streaming"`. Ordinary single-part
  *                          responses are always complete, so nothing changes
  *                          for the ~90 % of operations that never stream.
  */
final case class ApolloResponse[D](
    requestUuid: Uuid,
    data: Maybe[D] = Maybe.empty,
    error: Maybe[ApolloException] = Maybe.empty,
    extensions: Map[String, Json] = Map.empty,
    executionContext: ExecutionContext = ExecutionContext.Empty,
    cacheInfo: Maybe[CacheInfo] = Maybe.empty,
    complete: Boolean = true
):

    /** True when the call did not fully succeed — i.e. [[error]] is present. */
    def hasErrors: Boolean = error.isDefined

    /** True when [[error]] is a transport-level failure: the call never produced a
      * GraphQL result at all (network drop, non-2xx, unparseable body, cache miss).
      *
      * False both for a clean response and for one whose only failure was the
      * server answering with `errors` — that response still carries a usable
      * result, possibly with partial data, and is normalized and cached like any
      * other. Consolidating the two former fields into one [[error]] made this
      * distinction implicit; naming it keeps the call sites that depend on it
      * (cache write-back, retry) reading as what they mean.
      */
    def hasTransportError: Boolean = error match
        case Present(_: ApolloGraphQLException) => false
        case Present(_)                         => true
        case Absent                             => false

    /** The server's typed GraphQL `errors`, or empty.
      *
      * A projection of [[error]], not a second source of truth: non-empty exactly
      * when the failure was the GraphQL layer answering with `errors`, empty for
      * every transport failure and for a clean response. Note that data can be
      * present alongside these — that is GraphQL's partial-data case.
      */
    def errors: Chunk[GraphQLError] = error match
        case Present(e: ApolloGraphQLException) => e.errors
        case _                                  => Chunk.empty
end ApolloResponse

object ApolloResponse:

    /** Lift a decoded [[GraphQLResponse]] into an [[ApolloResponse]], copying
      * `data` and `extensions` verbatim and attaching transport metadata.
      *
      * A non-empty `errors` list becomes an [[ApolloGraphQLException]] on
      * [[ApolloResponse.error]] — including when `data` is also present, which is
      * the partial-data case: both fields are populated and neither is lost.
      */
    def fromGraphQLResponse[D](
        requestUuid: Uuid,
        response: GraphQLResponse[D],
        executionContext: ExecutionContext = ExecutionContext.Empty
    )(using Frame): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = requestUuid,
            data = response.data,
            error =
                if response.errors.isEmpty then Absent
                else Present(ApolloGraphQLException(response.errors)),
            extensions = response.extensions,
            executionContext = executionContext
        )

    /** Build a failure response carrying `error` and no data. Used by the terminal
      * transport to fold a caught [[ApolloException]] into a value.
      */
    def fromException[D](
        requestUuid: Uuid,
        exception: ApolloException,
        executionContext: ExecutionContext = ExecutionContext.Empty
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = requestUuid,
            data = Absent,
            error = Present(exception),
            executionContext = executionContext
        )
end ApolloResponse
