package kyo.apollo

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.interceptor.AuthorizationHeaderInterceptor
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import kyo.apollo.network.ws.FakeWebSocketConnection
import kyo.apollo.network.ws.FakeWebSocketEngine
import kyo.apollo.network.ws.SubscriptionWsProtocol
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Tests the top-level [[ApolloClient]] facade end to end through a fake
  * [[HttpEngine]] (no network): typed query/mutation execution, client defaults
  * and per-call fluent overrides reaching the composed [[HttpRequest]], both
  * interceptor tiers running, failures arriving as values, and `close()`. On
  * kyo-test each leaf body IS the effect; subscription frames are scripted with
  * `Sync.defer` and a short `Async.sleep` (`settle`) between them.
  */
class ApolloClientSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape each operation's `data` decodes from;
      * the operation's `D` stays `Int` by transforming this derived object schema.
      */
    final case class ValueData(value: Int) derives Schema
    private def valueSchema: Schema[Int] =
        summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)

    final case class ValueQuery() extends Query[Int]:
        def name: String             = "Value"
        def document: String         = "query Value { value }"
        def dataSchema: Schema[Int]  = valueSchema
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    final case class BumpMutation() extends Mutation[Int]:
        def name: String             = "Bump"
        def document: String         = "mutation Bump { value }"
        def dataSchema: Schema[Int]  = valueSchema
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end BumpMutation

    final case class ValueSubscription() extends Subscription[Int]:
        def name: String             = "Value"
        def document: String         = "subscription Value { value }"
        def dataSchema: Schema[Int]  = valueSchema
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Subscription"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueSubscription

    /** A fake engine that records the last request it saw and returns a canned
      * body/status without any network.
      */
    final private class CapturingEngine(body: String, status: Int = 200) extends HttpEngine:
        var lastRequest: Option[HttpRequest] = None
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            lastRequest = Some(request)
            HttpResponse(status, Nil, body)
    end CapturingEngine

    private def clientReturning(
        engine: HttpEngine,
        build: ApolloClient.Builder => ApolloClient.Builder = identity
    ): ApolloClient =
        build(
            ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(engine)
        ).build()

    /** Yield to kyo's scheduler so the subscription's background fiber dispatches
      * the frames scripted just before — the effect-native form of the old
      * macrotask `flush`.
      */
    private def settle(using Frame): Unit < Async = Async.sleep(30L.millis)

    private def ack: String = """{"type":"connection_ack"}"""
    private def next(id: String, value: Int): String =
        s"""{"id":"$id","type":"next","payload":{"data":{"value":$value}}}"""

    /** Build a client whose subscriptions run over a scripted in-memory socket. */
    private def wsClient(
        conn: FakeWebSocketConnection,
        build: ApolloClient.Builder => ApolloClient.Builder = identity
    ): ApolloClient =
        build(
            ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(CapturingEngine("""{"data":{"value":0}}"""))
                .webSocketEngine(FakeWebSocketEngine(conn))
        ).build()

    "ApolloClient" - {

        "query executes through the full stack and returns typed data" in {
            val engine = CapturingEngine("""{"data":{"value":42}}""")
            val client = clientReturning(engine)
            client.query(ValueQuery()).execute.map { response =>
                assert(response.data == Present(42))
                assert(response.exception == Absent)
                // Default method is POST with a JSON body carrying the document.
                assert(engine.lastRequest.map(_.method) == Some(HttpMethod.Post))
                assert(engine.lastRequest.flatMap(_.body).exists(_.contains("query Value")))
            }
        }

        "mutation executes and returns typed data" in {
            val engine = CapturingEngine("""{"data":{"value":7}}""")
            val client = clientReturning(engine)
            client.mutation(BumpMutation()).execute.map { response =>
                assert(response.data == Present(7))
                assert(engine.lastRequest.flatMap(_.body).exists(_.contains("mutation Bump")))
            }
        }

        "client default headers and per-call headers both reach the wire" in {
            val engine = CapturingEngine("""{"data":{"value":1}}""")
            val client = clientReturning(engine, _.addHttpHeader("X-Client", "base"))
            client.query(ValueQuery()).addHttpHeader("X-Call", "extra").execute.map { _ =>
                val headers = engine.lastRequest.map(_.headers).getOrElse(Nil)
                assert(
                    headers.contains(HttpHeader("X-Client", "base")),
                    s"client default missing: $headers"
                )
                assert(
                    headers.contains(HttpHeader("X-Call", "extra")),
                    s"per-call header missing: $headers"
                )
            }
        }

        "per-call .httpMethod(Get) overrides the client default POST" in {
            val engine = CapturingEngine("""{"data":{"value":9}}""")
            val client = clientReturning(engine)
            client.query(ValueQuery()).httpMethod(HttpMethod.Get).execute.map { response =>
                assert(response.data == Present(9))
                assert(engine.lastRequest.map(_.method) == Some(HttpMethod.Get))
                assert(engine.lastRequest.flatMap(_.body) == None) // GET has no body
                assert(
                    engine.lastRequest.exists(_.url.contains("query=")),
                    "GET should encode query params"
                )
            }
        }

        "the builder default httpMethod(Get) applies to every call" in {
            val engine = CapturingEngine("""{"data":{"value":3}}""")
            val client = clientReturning(engine, _.httpMethod(HttpMethod.Get))
            client.query(ValueQuery()).execute.map { _ =>
                assert(engine.lastRequest.map(_.method) == Some(HttpMethod.Get))
            }
        }

        "HTTP-tier interceptors run before the engine" in {
            val engine = CapturingEngine("""{"data":{"value":1}}""")
            val client = clientReturning(
                engine,
                _.addHttpInterceptor(new AuthorizationHeaderInterceptor("secret-token"))
            )
            client.query(ValueQuery()).execute.map { _ =>
                val headers = engine.lastRequest.map(_.headers).getOrElse(Nil)
                assert(
                    headers.contains(HttpHeader("Authorization", "secret-token")),
                    s"auth header missing: $headers"
                )
            }
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
            val client = clientReturning(engine, _.addInterceptor(tap))
            client.query(ValueQuery()).execute.map { response =>
                assert(response.data == Present(1))
                assert(order == List("before", "after"))
            }
        }

        "stream emits exactly one response for a plain query" in {
            val engine = CapturingEngine("""{"data":{"value":5}}""")
            val client = clientReturning(engine)
            StreamProbe.collect(client.query(ValueQuery()).stream).map { emissions =>
                assert(emissions.map(_.data) == List(Present(5)))
            }
        }

        "an HTTP error status arrives as a value, not a thrown exception" in {
            val engine = CapturingEngine("""{"errors":[]}""", status = 500)
            val client = clientReturning(engine)
            client.query(ValueQuery()).execute.map { response =>
                assert(response.data == Absent)
                assert(response.exception.exists(_.isInstanceOf[kyo.apollo.exception.ApolloHttpException]))
                assert(response.hasErrors)
            }
        }

        "buildResult() without a serverUrl yields a config failure (no throw)" in {
            ApolloClient.builder().buildResult() match
                case Result.Failure(ex) =>
                    assert(ex.isInstanceOf[kyo.apollo.exception.ApolloConfigException])
                    assert(ex.getMessage.contains("serverUrl"))
                case other => assert(false, s"expected a Failure, got $other")
        }

        "build() without a serverUrl throws ApolloConfigException" in {
            val _ = intercept[kyo.apollo.exception.ApolloConfigException] {
                ApolloClient.builder().build()
            }
        }

        "close() leaves the query path usable (no active subscription socket)" in {
            val engine = CapturingEngine("""{"data":{"value":11}}""")
            val client = clientReturning(engine)
            client.close()
            client.query(ValueQuery()).execute.map { response =>
                assert(response.data == Present(11))
            }
        }

        // --- subscriptions ------------------------------------------------------

        "a subscription streams typed events over the WebSocket transport" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .webSocketServerUrl("wss://example.com/subscriptions")
                .webSocketEngine(engine).build()
            var seen = List.empty[Int]
            for
                _ <- StreamProbe.drain(client.subscription(ValueSubscription()).stream)(r =>
                    r.data.foreach(v => seen = seen :+ v)
                )
                _ <- settle
                // Opened at the ws URL with the modern subprotocol; sent connection_init.
                _ = assert(
                    engine.opens == List(("wss://example.com/subscriptions", Some("graphql-transport-ws")))
                )
                _ = assert(conn.sent.head == """{"type":"connection_init"}""")
                _ <- Sync.defer(conn.server(ack))
                _ <- settle
                _ <- Sync.defer { conn.server(next("0", 7)); conn.server(next("0", 8)) }
                _ <- settle
                _ <- Sync.defer(client.close())
                _ <- settle
            yield assert(seen == List(7, 8))
            end for
        }

        "wsProtocol(SubscriptionWsProtocol) negotiates the legacy subprotocol" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .webSocketServerUrl("wss://example.com/subscriptions")
                .wsProtocol(SubscriptionWsProtocol)
                .webSocketEngine(engine).build()
            for
                _ <- StreamProbe.drain(client.subscription(ValueSubscription()).stream)(_ => ())
                _ <- settle
                _ <- Sync.defer(client.close())
                _ <- settle
            yield assert(engine.opens.map(_._2) == List(Some("graphql-ws")))
            end for
        }

        "webSocketServerUrl defaults to serverUrl when unset" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            val client = ApolloClient
                .builder()
                .serverUrl("wss://example.com/graphql")
                .webSocketEngine(engine).build()
            for
                _ <- StreamProbe.drain(client.subscription(ValueSubscription()).stream)(_ => ())
                _ <- settle
                _ <- Sync.defer(client.close())
                _ <- settle
            yield assert(engine.opens.map(_._1) == List("wss://example.com/graphql"))
            end for
        }

        "a query routes over HTTP and never opens the subscription socket" in {
            val conn   = new FakeWebSocketConnection
            val engine = FakeWebSocketEngine(conn)
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(CapturingEngine("""{"data":{"value":5}}"""))
                .webSocketEngine(engine).build()
            client.query(ValueQuery()).execute.map { response =>
                assert(response.data == Present(5))
                assert(engine.opens == Nil) // no subscription socket for a query
            }
        }

        "close() closes the shared subscription socket cleanly" in {
            val conn   = new FakeWebSocketConnection
            val client = wsClient(conn)
            for
                _ <- StreamProbe.drain(client.subscription(ValueSubscription()).stream)(_ => ())
                _ <- settle
                _ <- Sync.defer(conn.server(ack))
                _ <- settle
                _ = assert(conn.closedWith == None) // still open while streaming
                _ <- Sync.defer(client.close())
                _ <- settle // close() is async (enqueues Shutdown); let the owner fiber process it
            yield assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }
    }
end ApolloClientSpec
