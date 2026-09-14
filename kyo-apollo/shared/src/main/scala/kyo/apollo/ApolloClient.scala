package kyo.apollo

import kyo.*
import kyo.apollo.api.Mutation
import kyo.apollo.api.Operation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.exception.ApolloConfigException
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
  * ==What it assembles==
  *
  * A client is immutable and built through [[ApolloClient.builder]]. At build
  * time it composes, in order:
  *
  *   1. the **HTTP tier** — the registered [[HttpInterceptor]]s presented as a
  *      single [[kyo.apollo.network.http.HttpEngine]] via
  *      [[HttpInterceptorChain.asEngine]], wrapping the real
  *      [[kyo.apollo.network.http.FetchHttpEngine]] (or an injected one for tests);
  *   1. the **transports** — an [[HttpNetworkTransport]] bound to `serverUrl` for
  *      queries/mutations, and a
  *      [[kyo.apollo.network.ws.WebSocketNetworkTransport]] bound to
  *      `webSocketServerUrl` for subscriptions; both are the single place
  *      network/HTTP/parse/socket failures become `ApolloResponse.error`
  *      values;
  *   1. the **Apollo tier** — the registered [[ApolloInterceptor]]s followed by
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
  * Mirrors apollo-kotlin's `ApolloClient` / `ApolloClient.Builder`.
  *
  * @param serverUrl            the GraphQL endpoint every operation is sent to
  * @param defaultHttpHeaders   headers applied to every request (per-call
  *                             headers are appended on top)
  * @param httpInterceptors     HTTP-tier interceptors, in registration order
  * @param interceptors         Apollo-tier interceptors, in registration order
  *                             (the terminal network step is appended internally)
  * @param defaultHttpMethod    the method used when a call does not pin one
  *                             (`None` falls back to the composer default, POST)
  * @param httpEngine           the wire round-trip (defaults to the real fetch
  *                             engine; injectable for tests / custom transports)
  * @param webSocketServerUrl   the `ws(s)://` endpoint subscriptions connect to
  *                             (defaults to `serverUrl`, matching apollo-kotlin)
  * @param wsProtocol           the subscription wire protocol (defaults to the
  *                             modern `graphql-transport-ws`)
  * @param webSocketReopenWhen  decides, from a drop's exception and 1-based
  *                             attempt number, whether to transparently reopen a
  *                             dropped subscription socket and resubscribe
  *                             (defaults to never — opt-in, like apollo-kotlin)
  * @param webSocketConnectionPayload optional `connection_init` payload (e.g. auth)
  * @param webSocketEngine      the socket round-trip (defaults to the real JS
  *                             engine; injectable for tests)
  */
final class ApolloClient private (
    serverUrl: String,
    defaultHttpHeaders: List[HttpHeader],
    httpInterceptors: List[HttpInterceptor],
    interceptors: List[ApolloInterceptor],
    defaultHttpMethod: Option[HttpMethod],
    httpEngine: HttpEngine,
    webSocketServerUrl: String,
    wsProtocol: WsProtocol,
    webSocketReopenWhen: (ApolloException, Long) => Boolean,
    webSocketConnectionPayload: Option[Json],
    webSocketEngine: WebSocketEngine
):

    // The terminal transport, built once: the HTTP interceptor stack presented as
    // a single engine (identity when there are none), wrapped by the network
    // transport that folds failures into `ApolloResponse.error` values.
    private val transport: HttpNetworkTransport =
        HttpNetworkTransport(
            serverUrl,
            HttpInterceptorChain.asEngine(httpInterceptors, httpEngine)
        )

    // The terminal transport for subscriptions: one shared, lazily-opened socket
    // multiplexing every subscription. Built eagerly (it holds nothing and opens
    // no socket until the first subscription is collected), so a query-only client
    // pays nothing for it. Torn down by `close()`.
    private val webSocketTransport: WebSocketNetworkTransport =
        new WebSocketNetworkTransport(
            serverUrl = webSocketServerUrl,
            protocol = wsProtocol,
            engine = webSocketEngine,
            connectionPayload = webSocketConnectionPayload,
            reconnectWhen = webSocketReopenWhen
        )

    // The Apollo-tier chain: user interceptors first, then the terminal
    // NetworkInterceptor that routes queries/mutations to the HTTP transport and
    // subscriptions to the WebSocket transport. Built once; a fresh (cheap,
    // immutable) cursor over it is created per execution.
    private val apolloInterceptors: Chunk[ApolloInterceptor] =
        Chunk.from(interceptors :+ NetworkInterceptor(transport, Some(webSocketTransport)))

    /** Registry of the live `useQuery` watchers, populated by the kyo-ui binding
      * (each live query registers on setup and de-registers on `Scope` teardown).
      * The seam for imperative `refetchQueries` / `resetStore`.
      */
    private[apollo] val activeQueries: ActiveQueryRegistry =
        ActiveQueryRegistry.Unsafe.init()(using AllowUnsafe.embrace.danger)

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
        val builder = ApolloRequest.builder(operation).httpHeaders(defaultHttpHeaders)
        new ApolloCall(this, defaultHttpMethod.fold(builder)(builder.httpMethod))

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

    /** The Apollo-tier interceptors registered on this client (the internal
      * terminal network step excluded). Exposed package-privately so the cache
      * layer's `normalizedStore` accessor can locate an installed
      * `kyo.apollo.cache.normalized.CacheInterceptor` and hand its store back — without
      * the core client type ever depending on the cache package. The dependency
      * still runs cache → client, mirroring apollo-kotlin's `apolloStore`.
      */
    private[apollo] def registeredInterceptors: List[ApolloInterceptor] = interceptors

    /** Release transport resources: closes the shared subscription socket (if one
      * is open) and tears down the WebSocket transport, delivering a terminal
      * `ApolloWebSocketClosedException` value to any active subscribers, and returns
      * once the socket is closed. The HTTP fetch engine holds nothing disposable, so
      * queries/mutations are unaffected; a subscription started afterwards yields
      * the closed value. Mirrors apollo-kotlin's `ApolloClient.close()`.
      */
    def close(using Frame): Unit < Async = webSocketTransport.closeNow

    /** The same as [[close]]: it returns once the subscription socket is gone. */
    def closeAndAwait(using Frame): Unit < Async = webSocketTransport.closeNow
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

    /** Start a fresh [[Builder]]. `serverUrl` is required before `build()`. */
    def builder(): Builder = new Builder

    /** Fluent builder for an immutable [[ApolloClient]].
      *
      * Mirrors apollo-kotlin's `ApolloClient.Builder`: `serverUrl` is mandatory,
      * everything else is optional and defaulted. Both list-valued knobs offer a
      * whole-list setter and an `add*` accumulator; interceptors apply in the
      * order they were added. Fields are named with a leading underscore so the
      * accessor generated for each does not collide with its same-named setter.
      */
    final class Builder:
        private var _serverUrl: Option[String]               = None
        private var _httpHeaders: List[HttpHeader]           = Nil
        private var _httpInterceptors: List[HttpInterceptor] = Nil
        private var _interceptors: List[ApolloInterceptor]   = Nil
        private var _httpMethod: Option[HttpMethod]          = None
        private var _httpEngine: Option[HttpEngine]          = None
        private var _webSocketServerUrl: Option[String]      = None
        private var _wsProtocol: WsProtocol                  = GraphQLWsProtocol
        private var _webSocketReopenWhen: (ApolloException, Long) => Boolean =
            WebSocketNetworkTransport.reconnectNever
        private var _webSocketConnectionPayload: Option[Json] = None
        private var _webSocketEngine: Option[WebSocketEngine] = None

        /** The GraphQL endpoint URL (required). */
        def serverUrl(value: String): this.type =
            _serverUrl = Some(value)
            this

        /** Replace the default headers applied to every request. */
        def httpHeaders(value: List[HttpHeader]): this.type =
            _httpHeaders = value
            this

        /** Append one default header applied to every request. */
        def addHttpHeader(name: String, value: String): this.type =
            _httpHeaders = _httpHeaders :+ HttpHeader(name, value)
            this

        /** Replace the HTTP-tier interceptor list. */
        def httpInterceptors(value: List[HttpInterceptor]): this.type =
            _httpInterceptors = value
            this

        /** Append one HTTP-tier interceptor. */
        def addHttpInterceptor(value: HttpInterceptor): this.type =
            _httpInterceptors = _httpInterceptors :+ value
            this

        /** Replace the Apollo-tier interceptor list. */
        def interceptors(value: List[ApolloInterceptor]): this.type =
            _interceptors = value
            this

        /** Append one Apollo-tier interceptor. */
        def addInterceptor(value: ApolloInterceptor): this.type =
            _interceptors = _interceptors :+ value
            this

        /** Prepend one Apollo-tier interceptor, so it runs *first* — wrapping the
          * rest of the chain (cache included) and observing the final, post-cache
          * responses. The position a devtools/observability interceptor needs.
          */
        def prependInterceptor(value: ApolloInterceptor): this.type =
            _interceptors = value :: _interceptors
            this

        /** Set the default HTTP method (`Get` or `Post`) for every operation. */
        def httpMethod(value: HttpMethod): this.type =
            _httpMethod = Some(value)
            this

        /** Inject the wire engine — primarily for tests (a deterministic fake with
          * no server) or a custom transport. Defaults to the real fetch engine.
          */
        def httpEngine(value: HttpEngine): this.type =
            _httpEngine = Some(value)
            this

        /** The `ws(s)://` endpoint subscriptions connect to. Defaults to `serverUrl`
          * when unset (matching apollo-kotlin); most servers expose subscriptions on
          * a distinct URL, so set this whenever subscriptions are used.
          */
        def webSocketServerUrl(value: String): this.type =
            _webSocketServerUrl = Some(value)
            this

        /** The subscription wire protocol. Defaults to the modern
          * `graphql-transport-ws`; pass
          * [[kyo.apollo.network.ws.SubscriptionWsProtocol]] for the legacy
          * `subscriptions-transport-ws` servers.
          */
        def wsProtocol(value: WsProtocol): this.type =
            _wsProtocol = value
            this

        /** Opt into automatic reconnection: on an abnormal subscription-socket drop,
          * `predicate(exception, attempt)` decides whether to reopen the socket,
          * re-run `connection_init`, and resubscribe every active subscription. The
          * default never reopens. Pass
          * [[kyo.apollo.network.ws.WebSocketNetworkTransport.reconnectAlways]] to always
          * reconnect. Mirrors apollo-kotlin's `webSocketReopenWhen`.
          */
        def webSocketReopenWhen(
            predicate: (ApolloException, Long) => Boolean
        ): this.type =
            _webSocketReopenWhen = predicate
            this
        end webSocketReopenWhen

        /** The optional `connection_init` payload sent during the WebSocket
          * handshake — typically auth headers/tokens the server validates before it
          * acknowledges the connection.
          */
        def webSocketConnectionPayload(value: Json): this.type =
            _webSocketConnectionPayload = Some(value)
            this

        /** Inject the WebSocket engine — primarily for tests (a scripted in-memory
          * socket with no server). Defaults to the real JS engine.
          */
        def webSocketEngine(value: WebSocketEngine): this.type =
            _webSocketEngine = Some(value)
            this

        /** Validate the config and build the immutable client without throwing:
          * `Left(ApolloConfigException)` when `serverUrl` was never set, `Right`
          * otherwise. The non-throwing core the effectful entry points
          * (`ApolloClientResource.init` / `.layer`, in the kyo-ui binding) project onto
          * Kyo's `Abort` channel; prefer those over [[build]] in effect code.
          */
        def buildResult()(using Frame): Result[ApolloConfigException, ApolloClient] =
            _serverUrl match
                case None =>
                    Result.fail(
                        ApolloConfigException(
                            "ApolloClient requires a serverUrl; call .serverUrl(...) before build()."
                        )
                    )
                case Some(serverUrl) =>
                    Result.succeed(
                        new ApolloClient(
                            serverUrl = serverUrl,
                            defaultHttpHeaders = _httpHeaders,
                            httpInterceptors = _httpInterceptors,
                            interceptors = _interceptors,
                            defaultHttpMethod = _httpMethod,
                            httpEngine = _httpEngine.getOrElse(HttpEngine.default()),
                            webSocketServerUrl = _webSocketServerUrl.getOrElse(serverUrl),
                            wsProtocol = _wsProtocol,
                            webSocketReopenWhen = _webSocketReopenWhen,
                            webSocketConnectionPayload = _webSocketConnectionPayload,
                            webSocketEngine = _webSocketEngine.getOrElse(WebSocketEngine.default())
                        )
                    )

        /** Build the immutable client, throwing [[ApolloConfigException]] if `serverUrl`
          * was never set. A thin convenience over [[buildResult]] for apollo-kotlin
          * parity and non-effect (test) call sites; effect code should use
          * `ApolloClientResource.init` / `.layer`, which raise the same failure on
          * `Abort` instead.
          */
        def build()(using Frame): ApolloClient = buildResult().getOrThrow
    end Builder
end ApolloClient
