package kyo.apollo.network

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.json.Json

/** The transport-level result of executing an [[ApolloRequest]].
  *
  * Wraps the three [[GraphQLResponse]] fields **under the same names**
  * (`data` / `errors` / `extensions`) so there is no parallel response model,
  * and adds transport metadata: the correlating [[requestUuid]], the
  * [[executionContext]], and — crucially — [[exception]].
  *
  * Failures are values, not throws: a network drop, a non-2xx status, or a
  * malformed body all surface as `exception = Some(...)` with `data = None`
  * rather than escaping the public API. [[GraphQLResponse.parse]] remains the
  * single response decoder; construct the success case with
  * [[ApolloResponse.fromGraphQLResponse]] rather than re-walking the JSON.
  *
  * @param requestUuid       the id of the [[ApolloRequest]] that produced this
  * @param data              the decoded payload, `None` on a failed/errored call
  * @param errors            GraphQL `errors` entries (may accompany partial data)
  * @param extensions        top-level response `extensions`, passed through raw
  * @param executionContext  metadata accumulated across the interceptor chain
  * @param exception         a transport/parse failure, when one occurred
  * @param cacheInfo         how the normalized cache produced this response, when
  *                          it flowed through the cache interceptor (Phase 04);
  *                          `None` for a plain networked response
  */
final case class ApolloResponse[D](
    requestUuid: Uuid,
    data: Maybe[D] = Maybe.empty,
    errors: Chunk[GraphQLError] = Chunk.empty,
    extensions: Map[String, Json] = Map.empty,
    executionContext: ExecutionContext = ExecutionContext.Empty,
    exception: Maybe[ApolloException] = Maybe.empty,
    cacheInfo: Maybe[CacheInfo] = Maybe.empty
):

    /** True when this response carries either GraphQL `errors` or a transport /
      * parse [[exception]] — i.e. the call did not fully succeed.
      */
    def hasErrors: Boolean = errors.nonEmpty || exception.isDefined

    /** The transport/parse [[exception]] unwrapped, or `null` when the call
      * reached the server. The nullable analogue of [[exception]], mirroring
      * apollo-kotlin's `exceptionOrNull()` — lets Java/JS-style call sites test
      * `response.exceptionOrNull != null` without unwrapping the `Option`.
      */
    def exceptionOrNull: ApolloException = exception.getOrElse(null)

    /** The `data` payload, or throw if the call did not reach the server.
      *
      * Throws the transport/parse [[exception]] itself when one is present, so a
      * network drop or non-2xx status surfaces its original type. Otherwise
      * throws a [[DefaultApolloException]] when `data` is absent. GraphQL
      * `errors` are **not** consulted: a partial-data response is returned as-is
      * (use [[dataAssertNoErrors]] to reject partial data). Mirrors
      * apollo-kotlin's `dataOrThrow()`.
      */
    def dataOrThrow(): D =
        exception match
            case Present(e) => throw e
            case Absent =>
                data.getOrElse(
                    throw DefaultApolloException("The server did not return any data")
                )

    /** The `data` payload, or throw if the response carries **any** error.
      *
      * Stricter than [[dataOrThrow]]: throws the transport/parse [[exception]]
      * when present, then throws a [[DefaultApolloException]] if GraphQL `errors`
      * are present, and finally throws when `data` is absent. Only a fully clean
      * response yields data. Mirrors apollo-kotlin's `dataAssertNoErrors()`; the
      * thrown [[ApolloGraphQLException]] carries the typed errors and the
      * Apollo-JS-parity message (raw messages joined by newline, no prefix).
      */
    def dataAssertNoErrors(): D =
        exception match
            case Present(e) => throw e
            case Absent =>
                if errors.nonEmpty then throw ApolloGraphQLException(errors)
                else
                    data.getOrElse(
                        throw DefaultApolloException("The server did not return any data")
                    )
end ApolloResponse

object ApolloResponse:

    /** Lift a decoded [[GraphQLResponse]] into an [[ApolloResponse]], copying the
      * `data` / `errors` / `extensions` fields verbatim and attaching transport
      * metadata. `exception` is `None` — this is the success path.
      */
    def fromGraphQLResponse[D](
        requestUuid: Uuid,
        response: GraphQLResponse[D],
        executionContext: ExecutionContext = ExecutionContext.Empty
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = requestUuid,
            data = response.data,
            errors = response.errors,
            extensions = response.extensions,
            executionContext = executionContext,
            exception = Absent
        )

    /** Build a failure response carrying `exception` and no data. Used by the
      * terminal transport (Task 3) to fold a caught [[ApolloException]] into a
      * value.
      */
    def fromException[D](
        requestUuid: Uuid,
        exception: ApolloException,
        executionContext: ExecutionContext = ExecutionContext.Empty
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = requestUuid,
            data = Absent,
            executionContext = executionContext,
            exception = Present(exception)
        )
end ApolloResponse
