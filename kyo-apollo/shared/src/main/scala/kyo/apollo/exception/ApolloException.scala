package kyo.apollo.exception

import kyo.Chunk
import kyo.Frame
import kyo.KyoException
import kyo.apollo.api.GraphQLError
import kyo.apollo.json.Json
import kyo.apollo.network.HttpHeader

/** Root of the Apollo error hierarchy, the one sealed module exception.
  *
  * Every leaf takes `(using Frame)` and inherits `NoStackTrace` from [[kyo.KyoException]].
  * Leaves mix in the operation traits below whose rows they can appear on, so a row
  * names the precise failure set of its operation instead of this blanket base:
  *
  *   - [[ApolloExecuteFailure]] — executing an operation: transport, HTTP status,
  *     wire parse, GraphQL `errors`. These travel as `ApolloResponse.error` values.
  *   - [[ApolloParseFailure]] — decoding a GraphQL response envelope.
  *   - [[HttpEngineFailure]] — an HTTP round-trip that received no response.
  *   - [[CacheReadFailure]] — a normalized-cache read that could not be satisfied.
  *
  * [[ApolloConfigException]] mixes in none: it is a construction-time error.
  *
  * @param message the leaf's own message, as a UI shows it. `getMessage` adds the
  *                `KyoException` framing (creation site in development, the cause's
  *                detail) on top of it.
  */
sealed abstract class ApolloException(val message: String, cause: String | Throwable = "")(using Frame)
    extends KyoException(message, cause)

/** The failures of executing an operation (network, HTTP status, parse, GraphQL errors). */
sealed trait ApolloExecuteFailure extends ApolloException

/** The failures of decoding a GraphQL response envelope ([[kyo.apollo.api.GraphQLResponse.parse]]). */
sealed trait ApolloParseFailure extends ApolloException

/** The failures of an `HttpEngine` round-trip: no HTTP response was received. */
sealed trait HttpEngineFailure extends ApolloException

/** The failures of reading from the normalized cache. */
sealed trait CacheReadFailure extends ApolloException

/** No HTTP response was received at all: the connection failed, the request could
  * not be sent, or the body dropped. The underlying platform error is the `cause`.
  */
final class ApolloNetworkException(
    message: String = "Failed to execute GraphQL HTTP request",
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(message, cause)
    with ApolloExecuteFailure
    with HttpEngineFailure

/** An HTTP response arrived but its status was not in the 2xx range. Carries the
  * [[statusCode]] and the response [[headers]] so callers can inspect them (e.g.
  * a `Retry-After`), mirroring apollo-kotlin's `ApolloHttpException`.
  */
final class ApolloHttpException(
    val statusCode: Int,
    val headers: List[HttpHeader],
    message: String,
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(message, cause)
    with ApolloExecuteFailure

/** A response could not be decoded: `actual` is not the `expected` shape — a body
  * that is not JSON, an envelope that is not an object, or `data` that does not
  * match the operation. The decoder's own error, when there is one, is the `cause`.
  *
  * @param actual   the JSON that was read (for unparseable text, a prefix of it as a string)
  * @param expected what the decoder required, e.g. `"a GraphQL response object"`
  */
final class ApolloParseException(
    val actual: Json,
    val expected: String,
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(ApolloParseException.describe(actual, expected), cause)
    with ApolloExecuteFailure
    with ApolloParseFailure

object ApolloParseException:
    private val maxRendered = 200

    private def describe(actual: Json, expected: String): String =
        val rendered = actual.render
        val shown    = if rendered.length > maxRendered then rendered.take(maxRendered) + "…" else rendered
        s"Expected $expected but got: $shown"
    end describe
end ApolloParseException

/** A WebSocket connection carrying subscription operations was closed by the
  * server or the network. Carries the close [[code]] and optional [[reason]]
  * from the WebSocket close frame, mirroring apollo-kotlin's
  * `ApolloWebSocketClosedException`.
  */
final class ApolloWebSocketClosedException(
    val code: Int,
    val reason: Option[String] = None,
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(
        reason.fold(s"WebSocket closed with code $code")(r =>
            s"WebSocket closed with code $code: $r"
        ),
        cause
    ) with ApolloExecuteFailure

/** The GraphQL layer answered with `errors` — the server-side domain failure, as
  * opposed to the transport leaves above. Carries the typed
  * [[kyo.apollo.api.GraphQLError]]s so callers can inspect `extensions`/`path`/error
  * codes instead of parsing a string.
  *
  * The message is the raw error messages joined by `"\n"`, no prefix — apollo-client's
  * `CombinedGraphQLErrors` default formatter; the name follows apollo-kotlin's
  * `ApolloGraphQLException`.
  */
final class ApolloGraphQLException(
    val errors: Chunk[GraphQLError],
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(ApolloGraphQLException.messageFor(errors), cause)
    with ApolloExecuteFailure

object ApolloGraphQLException:
    private def messageFor(errors: Chunk[GraphQLError]): String =
        if errors.isEmpty then "GraphQL operation failed"
        else errors.map(_.message).mkString("\n")
end ApolloGraphQLException

/** Catch-all for an execution failure that maps onto none of the specific leaves. */
final class DefaultApolloException(
    message: String = "Apollo operation failed",
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(message, cause)
    with ApolloExecuteFailure

/** A client could not be constructed because its configuration was incomplete or
  * invalid — today, `serverUrl` was never set before build. A construction-time
  * failure: it carries no operation trait and appears on no operation's row.
  */
final class ApolloConfigException(
    message: String,
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(message, cause)

/** A denormalizing read could not be satisfied from the normalized cache: a
  * required record or one of its selected fields was absent. Carries the [[key]]
  * of the record being read and the [[fieldName]] that was missing (`None` when the
  * whole record was absent), mirroring apollo-kotlin's `CacheMissException(key, fieldName)`.
  *
  * Because it is a distinct leaf, the cache interceptor can tell a genuine cache
  * miss apart from a transport error: `CacheFirst`/`NetworkFirst` fall through to
  * the network on a miss, while `CacheOnly` surfaces it as an `ApolloResponse.error` value.
  *
  * @param key       the record key that was being read
  * @param fieldName the missing field, or `None` when the record itself was absent
  */
final class CacheMissException(
    val key: String,
    val fieldName: Option[String] = None,
    cause: String | Throwable = ""
)(using Frame) extends ApolloException(CacheMissException.describe(key, fieldName), cause)
    with CacheReadFailure

object CacheMissException:
    /** A miss for the whole record `key` (no such record in the cache). */
    def apply(key: String)(using Frame): CacheMissException = new CacheMissException(key, None)

    /** A miss for field `fieldName` of record `key`. */
    def apply(key: String, fieldName: String)(using Frame): CacheMissException =
        new CacheMissException(key, Some(fieldName))

    private def describe(key: String, fieldName: Option[String]): String =
        fieldName match
            case Some(f) => s"Object '$key' has no field named '$f' in the cache"
            case None    => s"Object '$key' not found in the cache"
end CacheMissException
