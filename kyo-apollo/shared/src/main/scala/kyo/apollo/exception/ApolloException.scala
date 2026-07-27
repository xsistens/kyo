package kyo.apollo.exception

import kyo.Chunk
import kyo.apollo.api.GraphQLError
import kyo.apollo.network.HttpHeader

/** Root of the Apollo error hierarchy.
  *
  * Defined here as the sealed base so that [[kyo.apollo.network.ApolloResponse]]
  * (Phase 03 Task 2) can name its `exception: Option[ApolloException]` payload
  * type. The concrete subtypes live alongside this base (the hierarchy is
  * `sealed`, so every subtype must share this file).
  *
  * The three transport subtypes below — [[ApolloNetworkException]],
  * [[ApolloHttpException]], [[ApolloParseException]] — were introduced with the
  * HTTP transport (Phase 03 Task 3), which folds each failure it catches into an
  * [[kyo.apollo.network.ApolloResponse.exception]] value. Task 4 rounds the
  * hierarchy out with [[ApolloWebSocketClosedException]] (Phase 06 stub) and a
  * catch-all [[DefaultApolloException]], completing the closed set of Apollo
  * failure values.
  *
  * Contract: these are **values**, carried inside `ApolloResponse.exception`;
  * the public API never throws them for network/HTTP/parse conditions.
  */
sealed abstract class ApolloException(
    message: String,
    cause: Throwable = null
) extends RuntimeException(message, cause)

/** The `fetch` promise rejected or the connection never completed — no HTTP
  * response was received at all. Wraps the underlying JS error as its `cause`.
  */
final class ApolloNetworkException(
    message: String = "Failed to execute GraphQL HTTP request",
    cause: Throwable = null
) extends ApolloException(message, cause)

/** An HTTP response arrived but its status was not in the 2xx range. Carries the
  * [[statusCode]] and the response [[headers]] so callers can inspect them (e.g.
  * a `Retry-After`), mirroring apollo-kotlin's `ApolloHttpException`.
  */
final class ApolloHttpException(
    val statusCode: Int,
    val headers: List[HttpHeader],
    message: String,
    cause: Throwable = null
) extends ApolloException(message, cause)

/** The HTTP response body could not be parsed into a GraphQL envelope — either
  * it was not valid JSON or not the object shape [[kyo.apollo.api.GraphQLResponse]]
  * requires. Wraps the thrown parse error as its `cause`.
  */
final class ApolloParseException(
    message: String = "Failed to parse GraphQL HTTP response body",
    cause: Throwable = null
) extends ApolloException(message, cause)

/** A WebSocket connection carrying subscription operations was closed by the
  * server or the network. Carries the close [[code]] and optional [[reason]]
  * from the WebSocket close frame, mirroring apollo-kotlin's
  * `ApolloWebSocketClosedException`.
  *
  * Stub for Phase 06 (subscriptions over WebSocket): the subtype exists now so
  * the hierarchy is complete and callers can pattern-match on it, but no
  * transport constructs it until the WebSocket layer lands.
  */
final class ApolloWebSocketClosedException(
    val code: Int,
    val reason: Option[String] = None,
    cause: Throwable = null
) extends ApolloException(
        reason.fold(s"WebSocket closed with code $code")(r =>
            s"WebSocket closed with code $code: $r"
        ),
        cause
    )

/** The GraphQL layer answered with `errors` and the operation yielded no usable
  * data — the server-side domain failure, as opposed to the transport subtypes
  * above. Carries the typed [[kyo.apollo.api.GraphQLError]]s so callers can
  * inspect `extensions`/`path`/error codes instead of parsing a string.
  *
  * '''Message contract (Apollo JS parity).''' `getMessage` is the raw error
  * messages joined by `"\n"` — no prefix — mirroring apollo-client's
  * `CombinedGraphQLErrors` default formatter
  * (`errors.map(e => e.message).join("\n")`). The '''name''' follows
  * apollo-kotlin's `ApolloGraphQLException`; the message deliberately does not
  * (kotlin carries a single error and prefixes `"GraphQL error: '…'"`, JS is
  * the behavioural reference of this port and UIs toast `getMessage`
  * verbatim). Raised by `ApolloEffect.resolve` (ErrorPolicy.None),
  * `QueryState`/`MutationState.fromResponse` when errors arrive without data,
  * and `ApolloResponse.dataAssertNoErrors`.
  *
  * Depends on `kyo.apollo.api` for the error type — same-module precedent as
  * the existing `network.HttpHeader` import above; `api` does not import this
  * package, so the layering stays acyclic.
  */
final class ApolloGraphQLException(
    val errors: Chunk[GraphQLError],
    cause: Throwable = null
) extends ApolloException(ApolloGraphQLException.messageFor(errors), cause)

object ApolloGraphQLException:
    private def messageFor(errors: Chunk[GraphQLError]): String =
        if errors.isEmpty then "GraphQL operation failed"
        else errors.map(_.message).mkString("\n")
end ApolloGraphQLException

/** Catch-all for any Apollo failure that does not map onto one of the specific
  * subtypes above. This is the fallthrough case referenced by the Task 1 design
  * (§3): anything the transport or a future layer cannot classify becomes a
  * [[DefaultApolloException]] value rather than an escaping throw.
  */
final class DefaultApolloException(
    message: String = "Apollo operation failed",
    cause: Throwable = null
) extends ApolloException(message, cause)

/** A client could not be constructed because its configuration was incomplete or
  * invalid — today, `serverUrl` was never set before build. Unlike the transport
  * subtypes, this is a construction-time failure: it is the value the effectful
  * `ApolloClient.builder` entry points (`ApolloClientResource.init` / `.layer`)
  * raise on Kyo's `Abort` channel instead of the throwing `Builder.build()`.
  */
final class ApolloConfigException(
    message: String,
    cause: Throwable = null
) extends ApolloException(message, cause)

/** A denormalizing read could not be satisfied from the normalized cache: a
  * required record or one of its selected fields was absent (Phase 04). Carries
  * the [[key]] of the record being read and the [[fieldName]] that was missing
  * (`None` when the whole record was absent), mirroring apollo-kotlin's
  * `CacheMissException(key, fieldName)`.
  *
  * Because it is a distinct subtype, the cache interceptor can tell a genuine
  * cache miss apart from a transport error: `CacheFirst`/`NetworkFirst` fall
  * through to the network on a miss, while `CacheOnly` surfaces it as an
  * `ApolloResponse.exception` value (consistent with the Phase 03 value
  * contract). Lives in this file because [[ApolloException]] is `sealed`, and
  * deliberately depends on no cache types so `kyo.apollo.exception` stays leaf-level.
  *
  * @param key       the record key that was being read
  * @param fieldName the missing field, or `None` when the record itself was absent
  */
final class CacheMissException(
    val key: String,
    val fieldName: Option[String] = None,
    cause: Throwable = null
) extends ApolloException(CacheMissException.describe(key, fieldName), cause)

object CacheMissException:
    /** A miss for the whole record `key` (no such record in the cache). */
    def apply(key: String): CacheMissException = new CacheMissException(key, None)

    /** A miss for field `fieldName` of record `key`. */
    def apply(key: String, fieldName: String): CacheMissException =
        new CacheMissException(key, Some(fieldName))

    private def describe(key: String, fieldName: Option[String]): String =
        fieldName match
            case Some(f) => s"Object '$key' has no field named '$f' in the cache"
            case None    => s"Object '$key' not found in the cache"
end CacheMissException
