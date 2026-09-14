package kyo.apollo

import kyo.*
import kyo.apollo.api.Mutation
import kyo.apollo.api.Operation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.exception.ApolloException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.interceptor.HttpInterceptor
import kyo.apollo.interceptor.HttpInterceptorChain
import kyo.apollo.interceptor.NetworkInterceptor
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ExecutionContext
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpNetworkTransport
import kyo.apollo.network.ws.GraphQLWsProtocol
import kyo.apollo.network.ws.WebSocketEngine
import kyo.apollo.network.ws.WebSocketNetworkTransport
import kyo.apollo.network.ws.WsProtocol
import kyo.apollo.runtime.ResponseStream

/** The top-level facade over the whole Phase 03 stack: it wires the two-tier
  * interceptor chain to the HTTP transport once, then hands out an
  * [[ApolloCall]] per operation.
  *
  * ==Lifecycle==
  *
  * A client is a resource: it owns the shared subscription socket. It is created
  * from an immutable [[ApolloClient.Config]] by [[ApolloClient.init]], which ties it
  * to the enclosing `Scope` — the Scope's end [[close]]s it. [[ApolloClient.use]]
  * brackets a block instead, and [[ApolloClient.initUnscoped]] /
  * [[ApolloClient.Unsafe.init]] leave closing to the caller.
  *
  * ==What it assembles==
  *
  * At creation it composes, in order:
  *
  *   1. the **HTTP tier** — the configured [[HttpInterceptor]]s presented as a
  *      single [[kyo.apollo.network.http.HttpEngine]] via
  *      [[HttpInterceptorChain.asEngine]], wrapping the real
  *      [[kyo.apollo.network.http.FetchHttpEngine]] (or an injected one for tests);
  *   1. the **transports** — an [[HttpNetworkTransport]] bound to `serverUrl` for
  *      queries/mutations, and a
  *      [[kyo.apollo.network.ws.WebSocketNetworkTransport]] bound to
  *      `webSocketServerUrl` for subscriptions; both are the single place
  *      network/HTTP/parse/socket failures become `ApolloResponse.error`
  *      values;
  *   1. the **Apollo tier** — the configured [[ApolloInterceptor]]s followed by
  *      a terminal [[NetworkInterceptor]] that routes each operation to the right
  *      transport (queries/mutations over HTTP, subscriptions over WebSocket).
  *
  * ==What it hands out==
  *
  * [[query]] / [[mutation]] / [[subscription]] seed an [[ApolloRequest]] with the
  * client defaults (headers, HTTP method, custom-scalar registry) and wrap it in
  * an [[ApolloCall]]. The call carries fluent per-request overrides
  * (`.addHttpHeader`, `.httpMethod`, …) and is executed stream-first via
  * `.toFlow` or as a single response via `.execute()` — both run the exact same
  * interceptor chain, so a plain query, a cache-then-network read, and a
  * long-lived subscription stream all share one path. Nothing runs until the
  * call is executed (the chain is cold).
  *
  * Mirrors apollo-kotlin's `ApolloClient`; its `ApolloClient.Builder` is the
  * [[ApolloClient.Config]] value.
  *
  * @param activeQueries registry of the live `useQuery` watchers, populated by the
  *                      kyo-ui binding (each live query registers on setup and
  *                      de-registers on `Scope` teardown); the seam for imperative
  *                      `refetchQueries` / `resetStore`
  */
final class ApolloClient private (
    config: ApolloClient.Config,
    httpEngine: HttpEngine,
    webSocketTransport: WebSocketNetworkTransport,
    private[apollo] val activeQueries: ActiveQueryRegistry
):

    // The terminal transport, built once: the HTTP interceptor stack presented as
    // a single engine (identity when there are none), wrapped by the network
    // transport that folds failures into `ApolloResponse.error` values.
    private val transport: HttpNetworkTransport =
        HttpNetworkTransport(
            config.serverUrl,
            HttpInterceptorChain.asEngine(config.httpInterceptors.toList, httpEngine)
        )

    // The Apollo-tier chain: user interceptors first, then the terminal
    // NetworkInterceptor that routes queries/mutations to the HTTP transport and
    // subscriptions to the WebSocket transport. Built once; a fresh (cheap,
    // immutable) cursor over it is created per execution.
    private val apolloInterceptors: Chunk[ApolloInterceptor] =
        config.interceptors.append(NetworkInterceptor(transport, Some(webSocketTransport)))

    /** Prepare a call for `query` — the read path. Nothing runs until the returned
      * [[ApolloCall]] is executed.
      */
    def query[D](query: Query[D]): ApolloCall[D] = call(query)

    /** Prepare a call for `mutation` — the write path. Nothing runs until the
      * returned [[ApolloCall]] is executed.
      */
    def mutation[D](mutation: Mutation[D]): ApolloCall[D] = call(mutation)

    /** Prepare a call for `subscription` — the long-lived streaming path. The
      * returned [[ApolloCall]]'s `toFlow` is a cold, multi-emission stream of
      * subscription events, routed through the exact same interceptor chain as
      * queries (so a `CacheInterceptor`, if installed, normalizes each streamed
      * event into the store and triggers watchers) and terminating at the
      * [[kyo.apollo.network.ws.WebSocketNetworkTransport]] instead of HTTP. Nothing
      * touches the socket until the flow is collected/subscribed. Mirrors
      * apollo-kotlin's `ApolloClient.subscription`.
      */
    def subscription[D](subscription: Subscription[D]): ApolloCall[D] =
        call(subscription)

    private def call[D](operation: Operation[D]): ApolloCall[D] =
        val builder = ApolloRequest.builder(operation).httpHeaders(config.httpHeaders.toList)
        new ApolloCall(this, config.httpMethod.fold(builder)(builder.httpMethod))

    /** Run one execution of `request` through the full Apollo interceptor chain,
      * returning its stream of responses. Invoked by [[ApolloCall.stream]]; a new
      * immutable chain cursor is used each time.
      *
      * The execution starts when the stream is consumed: the request is built there
      * — minting its `requestUuid` unless the builder pins one — so every
      * consumption is a request of its own. The chain walk is DEFERRED into the
      * stream body too: interceptors' `intercept` methods (and anything they invoke
      * while assembling their streams — cache reads, devtools bookkeeping, the
      * transport's engine call) run only when the stream is consumed, never when
      * the effect value is constructed. This is what makes `ApolloCall.stream` (and
      * everything built on it — `.data`, a handle's held `refetch`) genuinely cold.
      */
    private[apollo] def executeAsStream[D](
        request: ApolloRequest.Builder[D]
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap(
            Sync.defer(request.build.map(DefaultApolloInterceptorChain(apolloInterceptors, 0).proceed(_)))
        )

    /** Run an already-built `request` through the full Apollo interceptor chain — for
      * a caller that owns the execution's id (a `watch()` stamps it onto its cache
      * re-reads). Deferred into the stream body exactly like the builder overload.
      */
    private[apollo] def executeAsStream[D](
        request: ApolloRequest[D]
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap(
            Sync.defer(DefaultApolloInterceptorChain(apolloInterceptors, 0).proceed(request))
        )

    /** The Apollo-tier interceptors configured on this client (the internal
      * terminal network step excluded). Exposed package-privately so the cache
      * layer's `normalizedStore` accessor can locate an installed
      * `kyo.apollo.cache.normalized.CacheInterceptor` and hand its store back — without
      * the core client type ever depending on the cache package. The dependency
      * still runs cache → client, mirroring apollo-kotlin's `apolloStore`.
      */
    private[apollo] def registeredInterceptors: Chunk[ApolloInterceptor] = config.interceptors

    /** Close the client: let the subscriptions active at the call finish for up to
      * `gracePeriod`, then close the shared subscription socket. Subscriptions still
      * active then receive a terminal `ApolloWebSocketClosedException` value.
      * Returns once the socket is closed and the transport's fibers have exited. The
      * HTTP engine holds nothing disposable, so queries and mutations keep working;
      * a subscription started afterwards yields the closed value. Mirrors
      * apollo-kotlin's `ApolloClient.close()`.
      */
    def close(gracePeriod: Duration)(using Frame): Unit < Async = webSocketTransport.close(gracePeriod)

    /** [[close]] with a 30-second grace period — what the end of an [[ApolloClient.init]] Scope runs. */
    def close(using Frame): Unit < Async = close(30.seconds)

    /** [[close]] without waiting for live subscriptions. */
    def closeNow(using Frame): Unit < Async = close(Duration.Zero)
end ApolloClient

/** A prepared, not-yet-executed GraphQL operation carrying the
  * [[ApolloRequest.Builder]] its executions are built from and a back-reference to
  * the [[ApolloClient]] that runs it.
  *
  * It is the concrete implementation of the [[kyo.apollo.runtime.ApolloCall]]
  * contract (Task 6): [[stream]] runs the client's interceptor chain, and the
  * inherited `execute` is "take the first / only emission" over that stream.
  * The fluent setters (`.addHttpHeader`, `.httpMethod`, …) each return a **new**
  * `ApolloCall` over an updated builder, so a call is a cheap immutable value and
  * building it never mutates the client.
  *
  * A call holds no request id. Every consumption of [[stream]] is an execution of
  * its own and mints its own `requestUuid`, so one call value can be retried or run
  * concurrently without two executions sharing an identity.
  *
  * @param client  the client whose chain executes this call
  * @param request the builder each execution's request is built from
  */
final class ApolloCall[D] private[apollo] (
    client: ApolloClient,
    request: ApolloRequest.Builder[D]
) extends runtime.ApolloCall[D]:

    /** The stream-first primitive: run one execution of this call through the
      * client's full interceptor chain. Cold — nothing happens, and no id is minted,
      * until it is consumed.
      */
    def stream(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] = client.executeAsStream(request)

    /** The client that runs this call. Exposed package-privately for the cache
      * layer's `watch()` extension, which needs the store (via
      * `client.apolloStore`) and the interceptor chain (to re-fetch over the
      * network).
      */
    private[apollo] def apolloClient: ApolloClient = client

    /** The builder behind this call. Exposed for `watch()`, whose re-reads and
      * re-fetches operate on this operation and its execution context (fetch /
      * refetch policy), and which builds its own requests from it.
      */
    private[apollo] def requestBuilder: ApolloRequest.Builder[D] = request

    /** Append one header for this call, on top of the client defaults. */
    def addHttpHeader(name: String, value: String): ApolloCall[D] =
        withRequest(_.addHttpHeader(name, value))

    /** Replace this call's headers wholesale (dropping the client defaults). */
    def httpHeaders(headers: List[HttpHeader]): ApolloCall[D] =
        withRequest(_.httpHeaders(headers))

    /** Pin the HTTP method for this call, overriding the client default. */
    def httpMethod(method: HttpMethod): ApolloCall[D] =
        withRequest(_.httpMethod(method))

    /** Pin the cache [[FetchPolicy]] for this call. The policy rides the request's
      * [[ExecutionContext]] (no new request field), where the
      * [[kyo.apollo.cache.normalized.CacheInterceptor]] reads it back; without a cache
      * installed it is inert metadata.
      */
    def fetchPolicy(policy: FetchPolicy): ApolloCall[D] =
        withRequest(_.addExecutionContext(ExecutionContext.Empty + policy))

    /** Pin the [[ErrorPolicy]] for this call — how `.data` treats GraphQL `errors`
      * (react-apollo `errorPolicy`). Rides the request's [[ExecutionContext]] like
      * [[fetchPolicy]]; the `.data` effect reads it back. `Ignore`/`All` make `.data`
      * return the `data` despite GraphQL errors; `None` (default) raises them. Has no
      * effect on `.response`, which always exposes `data` + `errors`.
      */
    def errorPolicy(policy: ErrorPolicy): ApolloCall[D] =
        withRequest(_.addExecutionContext(ExecutionContext.Empty + policy))

    /** Build a sibling call over an updated builder. Package-private so the cache
      * layer's `refetchPolicy(...)` / `optimisticUpdates(...)` extensions can extend
      * the request the same way `fetchPolicy` does.
      */
    private[apollo] def withRequest(update: ApolloRequest.Builder[D] => ApolloRequest.Builder[D]): ApolloCall[D] =
        new ApolloCall(client, update(request))
end ApolloCall

object ApolloClient:

    /** Everything an [[ApolloClient]] is created from: an immutable value, so deriving
      * a variant (`config.addInterceptor(…)`) never changes the original, and each
      * client created from it owns its own socket, transports and query registry. The
      * interceptor and engine instances a `Config` holds are shared by every client
      * created from it. `serverUrl` is the one required field; the fluent methods are
      * `copy` shorthands.
      *
      * Mirrors apollo-kotlin's `ApolloClient.Builder`.
      *
      * @param serverUrl                  the GraphQL endpoint every operation is sent to
      * @param httpHeaders                headers applied to every request (per-call
      *                                   headers are appended on top)
      * @param httpInterceptors           HTTP-tier interceptors, in order
      * @param interceptors               Apollo-tier interceptors, in order (the terminal
      *                                   network step is appended internally)
      * @param httpMethod                 the method used when a call does not pin one
      *                                   (`Absent` falls back to the composer default, POST)
      * @param httpEngine                 the wire round-trip (defaults to the platform's
      *                                   real engine; injectable for tests / custom transports)
      * @param webSocketServerUrl         the `ws(s)://` endpoint subscriptions connect to
      *                                   (defaults to `serverUrl`, matching apollo-kotlin)
      * @param wsProtocol                 the subscription wire protocol (defaults to the
      *                                   modern `graphql-transport-ws`)
      * @param webSocketReopenWhen        decides, from a drop's exception and 1-based
      *                                   attempt number, whether to transparently reopen a
      *                                   dropped subscription socket and resubscribe
      *                                   (defaults to never — opt-in, like apollo-kotlin)
      * @param webSocketBackoff           the delays before successive reopen attempts
      * @param webSocketConnectionPayload optional `connection_init` payload (e.g. auth)
      * @param webSocketEngine            the socket round-trip (defaults to the platform's
      *                                   real engine; injectable for tests)
      * @param webSocketConnectTimeout    how long the default socket engine may take to
      *                                   open a socket (ignored with an injected engine)
      * @param subscriptionBufferSize     how many responses a subscription buffers before
      *                                   its consumer's pace holds the socket read back
      */
    final case class Config(
        serverUrl: String,
        httpHeaders: Chunk[HttpHeader] = Chunk.empty,
        httpInterceptors: Chunk[HttpInterceptor] = Chunk.empty,
        interceptors: Chunk[ApolloInterceptor] = Chunk.empty,
        httpMethod: Maybe[HttpMethod] = Absent,
        httpEngine: Maybe[HttpEngine] = Absent,
        webSocketServerUrl: Maybe[String] = Absent,
        wsProtocol: WsProtocol = GraphQLWsProtocol,
        webSocketReopenWhen: (ApolloException, Long) => Boolean = WebSocketNetworkTransport.reconnectNever,
        webSocketBackoff: Schedule = WebSocketNetworkTransport.defaultBackoff,
        webSocketConnectionPayload: Maybe[Json] = Absent,
        webSocketEngine: Maybe[WebSocketEngine] = Absent,
        webSocketConnectTimeout: Duration = WebSocketEngine.defaultConnectTimeout,
        subscriptionBufferSize: Int = WebSocketNetworkTransport.defaultSubscriptionBufferSize
    ) derives CanEqual:

        /** Append one default header applied to every request. */
        def addHttpHeader(name: String, value: String): Config =
            copy(httpHeaders = httpHeaders.append(HttpHeader(name, value)))

        /** Append one HTTP-tier interceptor. */
        def addHttpInterceptor(interceptor: HttpInterceptor): Config =
            copy(httpInterceptors = httpInterceptors.append(interceptor))

        /** Append one Apollo-tier interceptor. */
        def addInterceptor(interceptor: ApolloInterceptor): Config =
            copy(interceptors = interceptors.append(interceptor))

        /** Prepend one Apollo-tier interceptor, so it runs *first* — wrapping the
          * rest of the chain (cache included) and observing the final, post-cache
          * responses. The position a devtools/observability interceptor needs.
          */
        def prependInterceptor(interceptor: ApolloInterceptor): Config =
            copy(interceptors = Chunk(interceptor).concat(interceptors))

        /** Set the default HTTP method (`Get` or `Post`) for every operation. */
        def httpMethod(method: HttpMethod): Config = copy(httpMethod = Present(method))

        /** Inject the wire engine — primarily for tests (a deterministic fake with
          * no server) or a custom transport.
          */
        def httpEngine(engine: HttpEngine): Config = copy(httpEngine = Present(engine))

        /** The `ws(s)://` endpoint subscriptions connect to. Most servers expose
          * subscriptions on a distinct URL, so set this whenever subscriptions are used.
          */
        def webSocketServerUrl(url: String): Config = copy(webSocketServerUrl = Present(url))

        /** The subscription wire protocol; pass
          * [[kyo.apollo.network.ws.SubscriptionWsProtocol]] for the legacy
          * `subscriptions-transport-ws` servers.
          */
        def wsProtocol(protocol: WsProtocol): Config = copy(wsProtocol = protocol)

        /** Opt into automatic reconnection: on an abnormal subscription-socket drop,
          * `predicate(exception, attempt)` decides whether to reopen the socket,
          * re-run `connection_init`, and resubscribe every active subscription. Pass
          * [[kyo.apollo.network.ws.WebSocketNetworkTransport.reconnectAlways]] to always
          * reconnect. Mirrors apollo-kotlin's `webSocketReopenWhen`.
          */
        def webSocketReopenWhen(predicate: (ApolloException, Long) => Boolean): Config =
            copy(webSocketReopenWhen = predicate)

        /** The delays before successive reopen attempts. */
        def webSocketBackoff(schedule: Schedule): Config = copy(webSocketBackoff = schedule)

        /** The `connection_init` payload sent during the WebSocket handshake —
          * typically auth the server validates before it acknowledges the connection.
          */
        def webSocketConnectionPayload(payload: Json): Config = copy(webSocketConnectionPayload = Present(payload))

        /** Inject the WebSocket engine — primarily for tests (a scripted in-memory
          * socket with no server).
          */
        def webSocketEngine(engine: WebSocketEngine): Config = copy(webSocketEngine = Present(engine))

        /** How long the default socket engine may take to open a socket. */
        def webSocketConnectTimeout(timeout: Duration): Config = copy(webSocketConnectTimeout = timeout)

        /** How many responses a subscription buffers before its consumer's pace holds the socket read back. */
        def subscriptionBufferSize(size: Int): Config = copy(subscriptionBufferSize = size)
    end Config

    /** Create a client for `config`, owned by the enclosing `Scope`: the Scope's end
      * [[ApolloClient.close]]s it with the default grace period. The primary way to
      * create a client.
      */
    def init(config: Config)(using Frame): ApolloClient < (Sync & Scope) =
        initWith(config)(identity)

    /** [[init]], handing the client to `f`. */
    def initWith(config: Config)[B, S](f: ApolloClient => B < S)(using Frame): B < (S & Sync & Scope) =
        Sync.Unsafe.defer {
            val client = construct(config)
            Scope.ensure(client.close).andThen(f(client))
        }

    /** Run `f` with a client for `config` and close the client when `f` ends. */
    def use(config: Config)[B, S](f: ApolloClient => B < S)(using Frame): B < (S & Async) =
        Scope.run(initWith(config)(f))

    /** Create a client no `Scope` owns: the caller must [[ApolloClient.close]] it. */
    def initUnscoped(config: Config)(using Frame): ApolloClient < Sync =
        initUnscopedWith(config)(identity)

    /** [[initUnscoped]], handing the client to `f`. */
    def initUnscopedWith(config: Config)[B, S](f: ApolloClient => B < S)(using Frame): B < (S & Sync) =
        Sync.Unsafe.defer(f(Unsafe.init(config)))

    /** WARNING: Low-level API meant for integrations, libraries, and performance-sensitive code. See AllowUnsafe for more details. */
    object Unsafe:
        /** Create a client no `Scope` owns: the caller must [[ApolloClient.close]] it. */
        def init(config: Config)(using AllowUnsafe): ApolloClient = construct(config)
    end Unsafe

    private def construct(config: Config)(using AllowUnsafe): ApolloClient =
        new ApolloClient(
            config = config,
            httpEngine = config.httpEngine.getOrElse(HttpEngine.default()),
            webSocketTransport = new WebSocketNetworkTransport(
                serverUrl = config.webSocketServerUrl.getOrElse(config.serverUrl),
                protocol = config.wsProtocol,
                engine = config.webSocketEngine.getOrElse(WebSocketEngine.default(config.webSocketConnectTimeout)),
                connectionPayload = config.webSocketConnectionPayload.toOption,
                reconnectWhen = config.webSocketReopenWhen,
                backoff = config.webSocketBackoff,
                subscriptionBufferSize = config.subscriptionBufferSize
            ),
            activeQueries = ActiveQueryRegistry.Unsafe.init()
        )
end ApolloClient
