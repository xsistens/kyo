package kyo.apollo

import kyo.*
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.interceptor.AuthorizationHeaderInterceptor
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.ws.FakeWebSocketConnection
import kyo.apollo.network.ws.FakeWebSocketEngine
import kyo.apollo.network.ws.FreshWebSocketEngine
import kyo.apollo.network.ws.SubscriptionWsProtocol
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WebSocketEngine
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Tests the top-level [[ApolloClient]] facade end to end through a fake
  * [[HttpEngine]] (no network): typed query/mutation execution, client defaults
  * and per-call fluent overrides reaching the composed [[HttpRequest]], both
  * interceptor tiers running, failures arriving as values, and the lifecycle: a
  * client created by `init` is closed by its `Scope`, `closeNow` does not wait for
  * live subscriptions while `close(gracePeriod)` does, and a `Config` is a value.
  * On kyo-test each leaf body IS the effect and owns a `Scope`, which closes the
  * clients a leaf creates; subscription frames are scripted with `Sync.defer`, and
  * each step waits for the frame the transport sends in reply.
  */
class ApolloClientSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape each operation's `data` decodes from;
      * the operation's `D` stays `Int` by transforming this derived object schema.
      */
    final case class ValueData(value: Int) derives Schema
    private def valueSchema: Schema[Int] =
        summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)

    final case class ValueQuery() extends Query.Normalizable[Int]:
        def name: String              = "Value"
        def document: String          = "query Value { value }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema(using valueSchema)
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json           = Json.JObj(VectorMap.empty)
    end ValueQuery

    final case class BumpMutation() extends Mutation.Normalizable[Int]:
        def name: String              = "Bump"
        def document: String          = "mutation Bump { value }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema(using valueSchema)
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json           = Json.JObj(VectorMap.empty)
    end BumpMutation

    final case class ValueSubscription() extends Subscription.Normalizable[Int]:
        def name: String              = "Value"
        def document: String          = "subscription Value { value }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema(using valueSchema)
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Subscription"))
        def variables: Json           = Json.JObj(VectorMap.empty)
    end ValueSubscription

    /** A fake engine that records the last request it saw and returns a canned
      * body/status without any network.
      */
    final private class CapturingEngine(body: String, status: HttpStatus = HttpStatus.OK) extends HttpEngine:
        var lastRequest: Maybe[HttpEngine.Request] = Absent
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            lastRequest = Present(request)
            HttpEngine.response(status, body)
    end CapturingEngine

    private val baseConfig = ApolloClient.Config("https://example.com/graphql")

    /** A client over `engine`, owned by the leaf's `Scope`. */
    private def clientReturning(
        engine: HttpEngine,
        configure: ApolloClient.Config => ApolloClient.Config = identity
    )(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(configure(baseConfig.httpEngine(engine)))

    private def ack: String = """{"type":"connection_ack"}"""
    private def next(id: String, value: Int): String =
        s"""{"id":"$id","type":"next","payload":{"data":{"value":$value}}}"""

    /** A config whose subscriptions run over a scripted in-memory socket. */
    private def wsConfig(engine: WebSocketEngine): ApolloClient.Config =
        baseConfig.httpEngine(CapturingEngine("""{"data":{"value":0}}""")).webSocketEngine(engine)

    /** Answer the handshake on `conn` and wait until the subscription's start frame went out. */
    private def established(conn: FakeWebSocketConnection)(using Frame): Unit < Async =
        conn.awaitSent(_ == """{"type":"connection_init"}""")
            .andThen(Sync.defer(conn.server(ack)))
            .andThen(conn.awaitSent(_.contains("\"type\":\"subscribe\"")))
            .unit

    private def closedValue(response: ApolloResponse[Int]): Boolean =
        response.error.exists(_.isInstanceOf[ApolloWebSocketClosedException])

    "ApolloClient" - {

        "query executes through the full stack and returns typed data" in {
            val engine = CapturingEngine("""{"data":{"value":42}}""")
            for
                client   <- clientReturning(engine)
                response <- client.query(ValueQuery()).execute
            yield
                assert(response.data == Present(42))
                assert(response.error == Absent)
                // Default method is POST with a JSON body carrying the document.
                assert(engine.lastRequest.map(_.method) == Present(HttpMethod.POST))
                assert(engine.lastRequest.flatMap(_.fields.body.text).exists(_.contains("query Value")))
            end for
        }

        "mutation executes and returns typed data" in {
            val engine = CapturingEngine("""{"data":{"value":7}}""")
            for
                client   <- clientReturning(engine)
                response <- client.mutation(BumpMutation()).execute
            yield
                assert(response.data == Present(7))
                assert(engine.lastRequest.flatMap(_.fields.body.text).exists(_.contains("mutation Bump")))
            end for
        }

        "client default headers and per-call headers both reach the wire" in {
            val engine = CapturingEngine("""{"data":{"value":1}}""")
            for
                client <- clientReturning(engine, _.addHttpHeader("X-Client", "base"))
                _      <- client.query(ValueQuery()).addHttpHeader("X-Call", "extra").execute
            yield
                val headers = engine.lastRequest.map(_.headers).getOrElse(HttpHeaders.empty)
                assert(
                    headers.getAll("X-Client").contains("base"),
                    s"client default missing: $headers"
                )
                assert(
                    headers.getAll("X-Call").contains("extra"),
                    s"per-call header missing: $headers"
                )
            end for
        }

        "per-call .httpMethod(Get) overrides the client default POST" in {
            val engine = CapturingEngine("""{"data":{"value":9}}""")
            for
                client   <- clientReturning(engine)
                response <- client.query(ValueQuery()).httpMethod(HttpMethod.GET).execute
            yield
                assert(response.data == Present(9))
                assert(engine.lastRequest.map(_.method) == Present(HttpMethod.GET))
                assert(engine.lastRequest.flatMap(_.fields.body.text) == Absent) // GET has no body
                assert(
                    engine.lastRequest.exists(_.url.full.contains("query=")),
                    "GET should encode query params"
                )
            end for
        }

        "the configured default httpMethod(Get) applies to every call" in {
            val engine = CapturingEngine("""{"data":{"value":3}}""")
            for
                client <- clientReturning(engine, _.httpMethod(HttpMethod.GET))
                _      <- client.query(ValueQuery()).execute
            yield assert(engine.lastRequest.map(_.method) == Present(HttpMethod.GET))
            end for
        }

        "HTTP-tier interceptors run before the engine" in {
            val engine = CapturingEngine("""{"data":{"value":1}}""")
            for
                client <- clientReturning(
                    engine,
                    _.addHttpInterceptor(new AuthorizationHeaderInterceptor("secret-token"))
                )
                _ <- client.query(ValueQuery()).execute
            yield
                val headers = engine.lastRequest.map(_.headers).getOrElse(HttpHeaders.empty)
                assert(
                    headers.getAll("Authorization").contains("secret-token"),
                    s"auth header missing: $headers"
                )
            end for
        }

        "Apollo-tier interceptors run in registration order around the network" in {
            var order = List.empty[String]
            val tap: ApolloInterceptor = new ApolloInterceptor:
                def intercept[D](
                    request: ApolloRequest[D],
                    chain: ApolloInterceptorChain
                )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
                    order = order :+ "before"
                    chain.proceed(request).mapPure { r =>
                        order = order :+ "after"; r
                    }
                end intercept
            val engine = CapturingEngine("""{"data":{"value":1}}""")
            for
                client   <- clientReturning(engine, _.addInterceptor(tap))
                response <- client.query(ValueQuery()).execute
            yield
                assert(response.data == Present(1))
                assert(order == List("before", "after"))
            end for
        }

        "stream emits exactly one response for a plain query" in {
            val engine = CapturingEngine("""{"data":{"value":5}}""")
            for
                client    <- clientReturning(engine)
                emissions <- StreamProbe.collect(client.query(ValueQuery()).stream)
            yield assert(emissions.map(_.data) == List(Present(5)))
            end for
        }

        "an HTTP error status arrives as a value, not a thrown exception" in {
            val engine = CapturingEngine("""{"errors":[]}""", status = HttpStatus(500))
            for
                client   <- clientReturning(engine)
                response <- client.query(ValueQuery()).execute
            yield
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[kyo.apollo.exception.ApolloHttpException]))
                assert(response.hasErrors)
            end for
        }

        "a Config without a serverUrl does not compile" in {
            typeCheck("""ApolloClient.Config("https://example.com/graphql")""")
            typeCheckFailure("""ApolloClient.Config()""")("serverUrl")
        }

        "close leaves the query path usable (no active subscription socket)" in {
            val engine = CapturingEngine("""{"data":{"value":11}}""")
            for
                client   <- clientReturning(engine)
                _        <- client.close
                response <- client.query(ValueQuery()).execute
            yield assert(response.data == Present(11))
            end for
        }

        // --- lifecycle ------------------------------------------------------------

        "leaving the client's Scope closes its socket without a close call" in {
            val conn = new FakeWebSocketConnection
            for
                value <- Scope.run {
                    for
                        client <- ApolloClient.init(wsConfig(FakeWebSocketEngine(conn)))
                        pull   <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                        _      <- established(conn)
                        _      <- Sync.defer(conn.server(next("0", 7)))
                        first  <- pull.next
                    yield
                        assert(conn.closedWith == None, "the socket closed while its client's Scope was open")
                        first.data
                }
            yield
                assert(value == Present(7))
                assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }

        "use closes the client when its block ends" in {
            val conn = new FakeWebSocketConnection
            for
                value <- ApolloClient.use(wsConfig(FakeWebSocketEngine(conn))) { client =>
                    for
                        pull  <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                        _     <- established(conn)
                        _     <- Sync.defer(conn.server(next("0", 4)))
                        first <- pull.next
                    yield first.data
                }
            yield
                assert(value == Present(4))
                assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }

        "closeNow does not wait for an open subscription; close(gracePeriod) returns once the grace period is over" in Clock
            .withTimeControl { control =>
                val immediateConn = new FakeWebSocketConnection
                val patientConn   = new FakeWebSocketConnection
                for
                    immediate <- ApolloClient.init(wsConfig(FakeWebSocketEngine(immediateConn)))
                    _         <- StreamProbe.Pull.open(immediate.subscription(ValueSubscription()).stream)
                    _         <- established(immediateConn)
                    _         <- immediate.closeNow // the clock never moves
                    _ = assert(immediateConn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                    patient <- ApolloClient.init(wsConfig(FakeWebSocketEngine(patientConn)))
                    pull    <- StreamProbe.Pull.open(patient.subscription(ValueSubscription()).stream)
                    _       <- established(patientConn)
                    closer  <- Fiber.init(patient.close(10.seconds))
                    _       <- control.awaitPendingSleepers(1) // the grace period is running
                    early   <- closer.done
                    _ = assert(!early, "close(10.seconds) returned before its grace period was over")
                    _ = assert(patientConn.closedWith.isEmpty, "the socket closed during the grace period")
                    _     <- control.advance(10.seconds, Duration.Zero)
                    _     <- closer.get
                    ended <- pull.next
                yield
                    assert(patientConn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                    assert(closedValue(ended), s"got $ended")
                end for
            }

        "a Config is a value: a derived Config leaves the original unchanged, and each client owns its socket" in {
            val http   = CapturingEngine("""{"data":{"value":1}}""")
            val socket = FreshWebSocketEngine()
            val base   = baseConfig.httpEngine(http).webSocketEngine(socket)
            val tagged = base.addHttpHeader("X-Extra", "1")
            for
                plain <- ApolloClient.init(base)
                other <- ApolloClient.init(base)
                _     <- plain.query(ValueQuery()).execute
                plainHeaders = http.lastRequest.map(_.headers).getOrElse(HttpHeaders.empty)
                taggedClient <- ApolloClient.init(tagged)
                _            <- taggedClient.query(ValueQuery()).execute
                taggedHeaders = http.lastRequest.map(_.headers).getOrElse(HttpHeaders.empty)
                // Closing one client leaves a sibling from the same Config untouched.
                _         <- plain.closeNow
                afterward <- Scope.run(StreamProbe.collect(plain.subscription(ValueSubscription()).stream))
                _         <- StreamProbe.Pull.open(other.subscription(ValueSubscription()).stream)
                conn      <- socket.nextConnection
                init      <- conn.nextSent
            yield
                assert(base.httpHeaders.isEmpty)
                assert(
                    !plainHeaders.getAll("X-Extra").contains("1"),
                    s"the variant's header reached the original's client: $plainHeaders"
                )
                assert(taggedHeaders.getAll("X-Extra").contains("1"))
                assert(afterward.size == 1 && closedValue(afterward.head), s"got $afterward")
                assert(init == """{"type":"connection_init"}""")
                assert(socket.conns.size == 1)
            end for
        }

        "a client from initUnscoped runs subscriptions until the caller closes it" in {
            val conn = new FakeWebSocketConnection
            for
                client <- ApolloClient.initUnscoped(wsConfig(FakeWebSocketEngine(conn)))
                pull   <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                _      <- established(conn)
                _      <- Sync.defer(conn.server(next("0", 3)))
                first  <- pull.next
                _      <- client.closeNow
                ended  <- pull.next
            yield
                assert(first.data == Present(3))
                assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                assert(closedValue(ended), s"got $ended")
            end for
        }

        // --- subscriptions ------------------------------------------------------

        "a subscription streams typed events over the WebSocket transport" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            for
                client <- ApolloClient.init(
                    baseConfig.webSocketServerUrl("wss://example.com/subscriptions").webSocketEngine(engine)
                )
                pull  <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                first <- conn.nextSent
                // Opened at the ws URL with the modern subprotocol; sent connection_init.
                _ = assert(
                    engine.opens == List(("wss://example.com/subscriptions", Some("graphql-transport-ws")))
                )
                _ = assert(first == """{"type":"connection_init"}""")
                _ <- Sync.defer(conn.server(ack))
                _ <- conn.awaitSent(_.contains("\"type\":\"subscribe\""))
                _ <- Sync.defer { conn.server(next("0", 7)); conn.server(next("0", 8)) }
                a <- pull.next
                b <- pull.next
            yield assert(List(a, b).flatMap(_.data) == List(7, 8))
            end for
        }

        "wsProtocol(SubscriptionWsProtocol) negotiates the legacy subprotocol" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            for
                client <- ApolloClient.init(
                    baseConfig
                        .webSocketServerUrl("wss://example.com/subscriptions")
                        .wsProtocol(SubscriptionWsProtocol)
                        .webSocketEngine(engine)
                )
                _ <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                _ <- conn.nextSent // connection_init: the socket is open
            yield assert(engine.opens.map(_._2) == List(Some("graphql-ws")))
            end for
        }

        "webSocketServerUrl defaults to serverUrl when unset" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            for
                client <- ApolloClient.init(ApolloClient.Config("wss://example.com/graphql").webSocketEngine(engine))
                _      <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                _      <- conn.nextSent // connection_init: the socket is open
            yield assert(engine.opens.map(_._1) == List("wss://example.com/graphql"))
            end for
        }

        "a query routes over HTTP and never opens the subscription socket" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            for
                client <- ApolloClient.init(
                    baseConfig.httpEngine(CapturingEngine("""{"data":{"value":5}}""")).webSocketEngine(engine)
                )
                response <- client.query(ValueQuery()).execute
            yield
                assert(response.data == Present(5))
                assert(engine.opens == Nil) // no subscription socket for a query
            end for
        }

        "closeNow closes the shared subscription socket cleanly" in {
            val conn = new FakeWebSocketConnection
            for
                client <- ApolloClient.init(wsConfig(FakeWebSocketEngine(conn)))
                _      <- StreamProbe.Pull.open(client.subscription(ValueSubscription()).stream)
                _      <- established(conn)
                _ = assert(conn.closedWith == None) // still open while streaming
                _ <- client.closeNow // returns once the socket is closed
            yield assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }
    }
end ApolloClientSpec
