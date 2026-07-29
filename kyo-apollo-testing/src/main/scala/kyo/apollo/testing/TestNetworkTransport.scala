package kyo.apollo.testing

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.Operation
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.Uuid
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.runtime.ResponseStream
import scala.collection.mutable

/** Register canned [[ApolloResponse]]s per operation for deterministic tests —
  * the operation-layer analogue of [[MockServer]] (which scripts at the HTTP
  * layer). A `TestNetworkTransport` is a **terminal** [[ApolloInterceptor]]: it
  * answers each request from its script and never calls `chain.proceed`, so no
  * real network/socket is touched and any cache interceptor installed *above* it
  * still normalizes the canned response exactly as it would a networked one.
  *
  * Promoted from `ResilienceInterceptorSpec.ScriptedApollo` + the canned
  * `ApolloResponse` builders (ADR §5). Two flavors:
  *
  *   - [[QueueTestNetworkTransport]] — FIFO scripted responses (generalizes
  *     `ScriptedApollo`): the next `enqueue`d response answers the next request,
  *     regardless of which operation it is.
  *   - [[MapTestNetworkTransport]] — a per-operation registry (generalizes the
  *     document-routing `RoutingEngine`): responses keyed by operation, so
  *     arrival order does not matter.
  *
  * Wire one into a client with the [[TestApolloClient]] builders. Build canned
  * responses with [[TestResponses]].
  */
sealed abstract class TestNetworkTransport extends ApolloInterceptor:

    private var _seen: List[ApolloRequest[?]] = Nil

    /** Every request this transport answered, oldest first. */
    final def requests: List[ApolloRequest[?]] = _seen

    /** The operation names answered, oldest first — the common assertion. */
    final def operationNames: List[String] = _seen.map(_.operation.name)

    /** Pick the response for `request`. Implemented per flavor; receives the
      * request's [[Uuid]] already available via `request.requestUuid`.
      */
    protected def responseFor[D](request: ApolloRequest[D]): ApolloResponse[D]

    final def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        _seen = _seen :+ request
        Stream.init(Seq(responseFor(request)))
    end intercept
end TestNetworkTransport

/** FIFO scripted responses: the next [[enqueue]]d builder answers the next
  * request. Throws (loudly) when the script is exhausted, so a missing `enqueue`
  * is a visible test failure rather than a hang. Generalizes `ScriptedApollo`.
  */
final class QueueTestNetworkTransport extends TestNetworkTransport:

    private val script = mutable.Queue.empty[Uuid => ApolloResponse[Any]]

    /** Queue a response built from the answered request's [[Uuid]] (use the
      * [[TestResponses]] builders).
      */
    def enqueue(response: Uuid => ApolloResponse[Any]): QueueTestNetworkTransport =
        script.enqueue(response)
        this

    /** Queue a plain success payload as the next response. */
    def enqueueData[D](data: D): QueueTestNetworkTransport =
        enqueue(TestResponses.data(data))

    /** Queue a fully-formed response, re-stamped with the answered request's id. */
    def enqueueResponse(response: ApolloResponse[Any]): QueueTestNetworkTransport =
        enqueue(uuid => response.copy(requestUuid = uuid))

    protected def responseFor[D](request: ApolloRequest[D]): ApolloResponse[D] =
        if script.isEmpty then
            throw new NoSuchElementException(
                s"QueueTestNetworkTransport: no response enqueued for operation '${request.operation.name}'"
            )
        end if
        script.dequeue()(request.requestUuid).asInstanceOf[ApolloResponse[D]]
    end responseFor
end QueueTestNetworkTransport

/** A per-operation registry keyed by the operation's document text: [[register]]
  * a response for an operation, and every request for that operation is answered
  * with it (order-independent). Generalizes the document-routing `RoutingEngine`.
  */
final class MapTestNetworkTransport extends TestNetworkTransport:

    private val registry = mutable.LinkedHashMap.empty[String, Uuid => ApolloResponse[Any]]

    /** Register a response builder for `operation`, keyed by its document. */
    def register[D](
        operation: Operation[D],
        response: Uuid => ApolloResponse[Any]
    ): MapTestNetworkTransport =
        registry(operation.document) = response
        this
    end register

    /** Register a plain success payload for `operation`. */
    def registerData[D](operation: Operation[D], data: D): MapTestNetworkTransport =
        register(operation, TestResponses.data(data))

    protected def responseFor[D](request: ApolloRequest[D]): ApolloResponse[D] =
        registry.get(request.operation.document) match
            case Some(build) => build(request.requestUuid).asInstanceOf[ApolloResponse[D]]
            case None =>
                throw new NoSuchElementException(
                    s"MapTestNetworkTransport: no response registered for operation '${request.operation.name}'"
                )
end MapTestNetworkTransport

/** Canned [[ApolloResponse]] builders — the `data` / `networkError` /
  * `httpError` / `graphqlError` factories promoted from
  * `ResilienceInterceptorSpec` (ADR §5). Each yields a `Uuid => ApolloResponse`
  * so the transport stamps the answered request's id.
  */
object TestResponses:

    /** A clean success carrying `payload`. */
    def data[D](payload: D): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, data = Present(payload))

    /** A transport failure (a connection error). */
    def networkError(message: String = "network error"): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloNetworkException(message))

    /** A non-2xx HTTP failure. */
    def httpError(status: Int): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloHttpException(status, Nil, s"HTTP $status"))

    /** A response carrying a single GraphQL `errors` entry (not a transport fault). */
    def graphqlError(message: String): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, error = Present(ApolloGraphQLException(Chunk(GraphQLError(message)))))

    /** A response carrying an arbitrary transport [[ApolloException]]. */
    def exception(cause: ApolloException): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, cause)
end TestResponses

/** The `ApolloClient` test builders (ADR §5) — the `cacheless` / `cached`
  * factories generalizing `KyoTestSupport.cacheless`/`cached` and the ~9
  * `cachedClient`/`clientFor` copies scattered across the specs. Each builds a
  * ready-to-run client against a fake [[HttpEngine]] or a [[TestNetworkTransport]],
  * with an optional normalized cache keyed by `id`.
  */
object TestApolloClient:

    /** A stable non-routable server URL; a fake engine/transport answers, so no
      * request ever leaves the process.
      */
    val DefaultServerUrl: String = "https://example.test/graphql"

    /** A client with no cache over `engine` — `.execute()` runs the engine through
      * the HTTP transport straight through.
      */
    def cacheless(engine: HttpEngine, serverUrl: String = DefaultServerUrl): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl(serverUrl)
            .httpEngine(engine)
            .build()

    /** A client with a normalized cache (keyed by `keyFields`, default `id`) over
      * `engine`, so a query read and a store write share a record — required by
      * `watch()` / `watchSignal`.
      */
    def cached(
        engine: HttpEngine,
        keyFields: List[String] = List("id"),
        serverUrl: String = DefaultServerUrl
    ): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl(serverUrl)
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(keyFields))
            .build()

    /** A cacheless client answered at the operation layer by `transport` (added as
      * the terminal interceptor, so the real network is never reached).
      */
    def withTransport(
        transport: TestNetworkTransport,
        serverUrl: String = DefaultServerUrl
    ): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl(serverUrl)
            .addInterceptor(transport)
            .build()

    /** A client with a normalized cache whose network tier is `transport`: the
      * cache interceptor runs above the terminal transport, normalizing each
      * canned response exactly as it would a networked one.
      */
    def cachedWithTransport(
        transport: TestNetworkTransport,
        keyFields: List[String] = List("id"),
        serverUrl: String = DefaultServerUrl
    ): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl(serverUrl)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(keyFields))
            .addInterceptor(transport)
            .build()
end TestApolloClient
