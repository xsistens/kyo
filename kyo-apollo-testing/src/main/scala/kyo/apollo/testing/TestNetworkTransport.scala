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
  * A request is recorded, and its response picked, when the stream it returns is
  * consumed — a stream that is built and dropped is not counted, and a stream
  * consumed twice is counted twice. Wire one into a client with the
  * [[TestApolloClient]] builders. Build canned responses with [[TestResponses]].
  */
sealed abstract class TestNetworkTransport private[testing] (seen: AtomicRef[Chunk[ApolloRequest[?]]])
    extends ApolloInterceptor:

    /** Every request this transport answered, oldest first. */
    final def requests(using Frame): Chunk[ApolloRequest[?]] < Sync = seen.get

    /** The operation names answered, oldest first — the common assertion. */
    final def operationNames(using Frame): Chunk[String] < Sync = seen.get.map(_.map(_.operation.name))

    /** Pick the response for `request`. Implemented per flavor; receives the
      * request's [[Uuid]] already available via `request.requestUuid`.
      */
    protected def responseFor[D](request: ApolloRequest[D])(using Frame): ApolloResponse[D] < Sync

    final def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap(
            seen.updateAndGet(_.append(request)).andThen(responseFor(request)).map(response => Stream.init(Chunk(response)))
        )
    end intercept
end TestNetworkTransport

/** FIFO scripted responses: the next [[enqueue]]d builder answers the next
  * request. Panics (loudly) when the script is exhausted, so a missing `enqueue`
  * is a visible test failure rather than a hang. Generalizes `ScriptedApollo`.
  * Build one with [[QueueTestNetworkTransport.init]].
  */
final class QueueTestNetworkTransport private (
    seen: AtomicRef[Chunk[ApolloRequest[?]]],
    script: AtomicRef[Chunk[Uuid => ApolloResponse[Any]]]
) extends TestNetworkTransport(seen):

    /** Queue a response built from the answered request's [[Uuid]] (use the
      * [[TestResponses]] builders).
      */
    def enqueue(response: Uuid => ApolloResponse[Any])(using Frame): Unit < Sync =
        script.updateAndGet(_.append(response)).unit

    /** Queue a plain success payload as the next response. */
    def enqueueData[D](data: D)(using Frame): Unit < Sync =
        enqueue(TestResponses.data(data))

    /** Queue a fully-formed response, re-stamped with the answered request's id. */
    def enqueueResponse(response: ApolloResponse[Any])(using Frame): Unit < Sync =
        enqueue(uuid => response.copy(requestUuid = uuid))

    protected def responseFor[D](request: ApolloRequest[D])(using Frame): ApolloResponse[D] < Sync =
        script.getAndUpdate(_.drop(1)).map { queued =>
            queued.headMaybe match
                case Present(build) => build(request.requestUuid).asInstanceOf[ApolloResponse[D]]
                case Absent =>
                    Abort.panic(new NoSuchElementException(
                        s"QueueTestNetworkTransport: no response enqueued for operation '${request.operation.name}'"
                    ))
        }
    end responseFor
end QueueTestNetworkTransport

object QueueTestNetworkTransport:

    /** A transport with an empty script that has answered nothing. */
    def init(using Frame): QueueTestNetworkTransport < Sync =
        for
            seen   <- AtomicRef.init(Chunk.empty[ApolloRequest[?]])
            script <- AtomicRef.init(Chunk.empty[Uuid => ApolloResponse[Any]])
        yield new QueueTestNetworkTransport(seen, script)
end QueueTestNetworkTransport

/** A per-operation registry keyed by the operation's document text: [[register]]
  * a response for an operation, and every request for that operation is answered
  * with it (order-independent). Generalizes the document-routing `RoutingEngine`.
  * Build one with [[MapTestNetworkTransport.init]].
  */
final class MapTestNetworkTransport private (
    seen: AtomicRef[Chunk[ApolloRequest[?]]],
    registry: AtomicRef[Map[String, Uuid => ApolloResponse[Any]]]
) extends TestNetworkTransport(seen):

    /** Register a response builder for `operation`, keyed by its document. */
    def register[D](
        operation: Operation[D],
        response: Uuid => ApolloResponse[Any]
    )(using Frame): Unit < Sync =
        registry.updateAndGet(_.updated(operation.document, response)).unit

    /** Register a plain success payload for `operation`. */
    def registerData[D](operation: Operation[D], data: D)(using Frame): Unit < Sync =
        register(operation, TestResponses.data(data))

    protected def responseFor[D](request: ApolloRequest[D])(using Frame): ApolloResponse[D] < Sync =
        registry.get.map { routes =>
            routes.get(request.operation.document) match
                case Some(build) => build(request.requestUuid).asInstanceOf[ApolloResponse[D]]
                case None =>
                    Abort.panic(new NoSuchElementException(
                        s"MapTestNetworkTransport: no response registered for operation '${request.operation.name}'"
                    ))
        }
end MapTestNetworkTransport

object MapTestNetworkTransport:

    /** A transport with no registered responses that has answered nothing. */
    def init(using Frame): MapTestNetworkTransport < Sync =
        for
            seen     <- AtomicRef.init(Chunk.empty[ApolloRequest[?]])
            registry <- AtomicRef.init(Map.empty[String, Uuid => ApolloResponse[Any]])
        yield new MapTestNetworkTransport(seen, registry)
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
    def networkError(message: String = "network error")(using Frame): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloNetworkException(message))

    /** A non-2xx HTTP failure. */
    def httpError(status: Int)(using Frame): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, ApolloHttpException(status, HttpHeaders.empty, s"HTTP $status"))

    /** A response carrying a single GraphQL `errors` entry (not a transport fault). */
    def graphqlError(message: String)(using Frame): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse[Any](uuid, error = Present(ApolloGraphQLException(Chunk(GraphQLError(message)))))

    /** A response carrying an arbitrary transport [[ApolloException]]. */
    def exception(cause: ApolloException): Uuid => ApolloResponse[Any] =
        uuid => ApolloResponse.fromException(uuid, cause)
end TestResponses

/** The `ApolloClient` test factories (ADR §5) — the `cacheless` / `cached`
  * factories generalizing `KyoTestSupport.cacheless`/`cached` and the ~9
  * `cachedClient`/`clientFor` copies scattered across the specs. Each creates a
  * ready-to-run client against a fake [[HttpEngine]] or a [[TestNetworkTransport]],
  * with an optional normalized cache keyed by `id`, owned by the enclosing `Scope`
  * ([[ApolloClient.init]]) — a test leaf's `Scope` closes it.
  */
object TestApolloClient:

    /** A stable non-routable server URL; a fake engine/transport answers, so no
      * request ever leaves the process.
      */
    val DefaultServerUrl: String = "https://example.test/graphql"

    /** A client with no cache over `engine` — `.execute()` runs the engine through
      * the HTTP transport straight through.
      */
    def cacheless(engine: HttpEngine, serverUrl: String = DefaultServerUrl)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(ApolloClient.Config(serverUrl).httpEngine(engine))

    /** A client with a normalized cache (keyed by `keyFields`, default `id`) over
      * `engine`, so a query read and a store write share a record — required by
      * `watch()` / `watchSignal`. Each evaluation gets a fresh cache.
      */
    def cached(
        engine: HttpEngine,
        keyFields: List[String] = List("id"),
        serverUrl: String = DefaultServerUrl
    )(using Frame): ApolloClient < (Sync & Scope) =
        Sync.defer(
            ApolloClient.Config(serverUrl).httpEngine(engine).normalizedCache(MemoryCache(), IdCacheKeyGenerator(keyFields))
        ).map(ApolloClient.init)

    /** A cacheless client answered at the operation layer by `transport` (added as
      * the terminal interceptor, so the real network is never reached).
      */
    def withTransport(
        transport: TestNetworkTransport,
        serverUrl: String = DefaultServerUrl
    )(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(ApolloClient.Config(serverUrl).addInterceptor(transport))

    /** A client with a normalized cache whose network tier is `transport`: the
      * cache interceptor runs above the terminal transport, normalizing each
      * canned response exactly as it would a networked one. Each evaluation gets a
      * fresh cache.
      */
    def cachedWithTransport(
        transport: TestNetworkTransport,
        keyFields: List[String] = List("id"),
        serverUrl: String = DefaultServerUrl
    )(using Frame): ApolloClient < (Sync & Scope) =
        Sync.defer(
            ApolloClient.Config(serverUrl).normalizedCache(MemoryCache(), IdCacheKeyGenerator(keyFields)).addInterceptor(transport)
        ).map(ApolloClient.init)
end TestApolloClient
