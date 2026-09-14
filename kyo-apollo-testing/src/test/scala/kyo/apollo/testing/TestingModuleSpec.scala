package kyo.apollo.testing

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.Uuid
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.runtime.ResponseStream

/** Validates the promoted `kyo.apollo.testing` doubles behave as the specs that once
  * declared them inline relied on: the unified [[TestHttpEngine]], the
  * [[MockServer]], the operation-layer [[QueueTestNetworkTransport]] /
  * [[MapTestNetworkTransport]], the [[TestApolloClient]] builders (incl. cache
  * integration), the scripted [[MockWebSocketServer]], and [[StreamProbe]] — all
  * driven through the real `ApolloClient` interceptor chain with no network.
  *
  * On kyo-test each leaf body IS the effect the runner discharges.
  */
class TestingModuleSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Yield to kyo's scheduler so the WebSocket transport's background fibers
      * dispatch the frames scripted just before — the effect-native `flush`.
      */
    private def settle(using Frame): Unit < Async = Async.sleep(30L.millis)

    "kyo.apollo.testing" - {

        // --- TestHttpEngine + cacheless client ---------------------------------

        "TestHttpEngine.returning answers a query and records the request" in {
            for
                engine <- TestHttpEngine.returning("""{"data":{"value":7}}""")
                client = TestApolloClient.cacheless(engine)
                response <- client.query(Fixtures.ValueQuery()).execute
                calls    <- engine.calls
                last     <- engine.lastRequest
            yield
                assert(response.data == Present(7))
                assert(calls == 1)
                assert(last.exists(_.body.exists(_.contains("Value"))))
            end for
        }

        "TestHttpEngine.failing surfaces as an ApolloResponse.error value" in {
            for
                engine <- TestHttpEngine.failing(new RuntimeException("down"))
                client = TestApolloClient.cacheless(engine)
                response <- client.query(Fixtures.ValueQuery()).execute
            yield
                assert(response.data == Absent)
                assert(response.error.isDefined)
            end for
        }

        // --- the doubles count what ran, not what was built -------------------

        "a round-trip that is built but never run is neither counted nor answered" in {
            val request = HttpRequest(HttpMethod.Post, TestApolloClient.DefaultServerUrl, Nil, Some("{}"))
            for
                engine    <- TestHttpEngine.returning("{}")
                server    <- MockServer.init
                transport <- QueueTestNetworkTransport.init
                _         <- server.enqueue("""{"data":{"value":1}}""")
                _         <- transport.enqueueData(1)
                _ = discard(engine.execute(request))
                _ = discard(server.execute(request))
                _ = discard(transport.intercept(ApolloRequest(Fixtures.ValueQuery(), requestId), InertChain))
                calls    <- engine.calls
                received <- server.requestCount
                answered <- transport.requests
                // The queued answers are still there for the executions that do run.
                client = TestApolloClient.cacheless(server)
                first <- client.query(Fixtures.ValueQuery()).execute
                seen  <- transport.intercept(ApolloRequest(Fixtures.ValueQuery(), requestId), InertChain).run
            yield
                assert(calls == 0, s"TestHttpEngine counted a request that never ran: $calls")
                assert(received == 0, s"MockServer counted a request that never ran: $received")
                assert(answered.isEmpty, s"the transport recorded a request whose stream was never consumed: $answered")
                assert(first.data == Present(1))
                assert(seen.map(_.data) == Chunk(Present(1)))
            end for
        }

        "concurrent round-trips are all counted" in {
            val request = HttpRequest(HttpMethod.Post, TestApolloClient.DefaultServerUrl, Nil, Some("{}"))
            for
                engine <- TestHttpEngine.returning("{}")
                start  <- Latch.init(1)
                fibers <- Kyo.fill(64)(Fiber.init(start.await.andThen(engine.execute(request))))
                _      <- start.release
                _      <- Kyo.foreachDiscard(fibers)(_.get)
                calls  <- engine.calls
            yield assert(calls == 64)
            end for
        }

        // --- MockServer --------------------------------------------------------

        "MockServer serves enqueued responses FIFO and records requests" in {
            for
                server <- MockServer.init
                _      <- server.enqueue("""{"data":{"value":1}}""")
                _      <- server.enqueue("""{"data":{"value":2}}""")
                client = TestApolloClient.cacheless(server)
                first  <- client.query(Fixtures.ValueQuery()).execute
                second <- client.query(Fixtures.ValueQuery()).execute
                count  <- server.requestCount
                taken  <- server.takeRequest
                more   <- server.hasNoMoreRequests
            yield
                assert(first.data == Present(1))
                assert(second.data == Present(2))
                assert(count == 2)
                assert(taken.body.exists(_.contains("Value")))
                assert(!more)
            end for
        }

        // --- QueueTestNetworkTransport -----------------------------------------

        "QueueTestNetworkTransport answers operation-layer, in order" in {
            for
                transport <- QueueTestNetworkTransport.init
                _         <- transport.enqueueData(41)
                _         <- transport.enqueueData(42)
                client = TestApolloClient.withTransport(transport)
                a     <- client.query(Fixtures.ValueQuery()).execute
                b     <- client.query(Fixtures.ValueQuery()).execute
                names <- transport.operationNames
            yield
                assert(a.data == Present(41))
                assert(b.data == Present(42))
                assert(names == Chunk("Value", "Value"))
            end for
        }

        // --- MapTestNetworkTransport -------------------------------------------

        "MapTestNetworkTransport routes each operation to its registered response" in {
            for
                transport <- MapTestNetworkTransport.init
                _         <- transport.registerData(Fixtures.ValueQuery(), 99)
                _         <- transport.registerData(Fixtures.CurrentUserQuery(), Fixtures.userData("Alice"))
                client = TestApolloClient.withTransport(transport)
                value <- client.query(Fixtures.ValueQuery()).execute
                user  <- client.query(Fixtures.CurrentUserQuery()).execute
            yield
                assert(value.data == Present(99))
                assert(user.data.exists(_.user.name == "Alice"))
            end for
        }

        // --- TestApolloClient.cached (cache integration) -----------------------

        "cached: a CacheFirst re-read is served from the cache, not the engine" in {
            for
                engine <- TestHttpEngine.returning(Fixtures.body("Alice"))
                client = TestApolloClient.cached(engine)
                first  <- client.query(Fixtures.CurrentUserQuery()).execute
                second <- client.query(Fixtures.CurrentUserQuery()).execute
                calls  <- engine.calls
            yield
                assert(first.data.exists(_.user.name == "Alice"))
                assert(second.data.exists(_.user.name == "Alice"))
                assert(calls == 1) // second read hit the normalized cache
            end for
        }

        "cachedWithTransport wires a cache above a canned transport" in {
            for
                transport <- MapTestNetworkTransport.init
                _         <- transport.registerData(Fixtures.CurrentUserQuery(), Fixtures.userData("Bob"))
                client = TestApolloClient.cachedWithTransport(transport)
                _ <- client.query(Fixtures.CurrentUserQuery()).execute
                cached <- client
                    .query(Fixtures.CurrentUserQuery())
                    .fetchPolicy(FetchPolicy.CacheOnly)
                    .execute
            yield assert(cached.data.exists(_.user.name == "Bob"))
            end for
        }

        // --- GatedHttpEngine ---------------------------------------------------

        "GatedHttpEngine parks the reply until released" in {
            val engine = GatedHttpEngine("""{"data":{"value":5}}""")
            val client = TestApolloClient.cacheless(engine)
            for
                fiber <- Fiber.init(Scope.run(client.query(Fixtures.ValueQuery()).execute))
                _     <- settle
                // The request reached the engine and is now parked on the gate; the
                // result only arrives after release.
                _        <- Sync.defer(assert(engine.requests.nonEmpty))
                _        <- Sync.defer(engine.release())
                response <- fiber.get
            yield assert(response.data == Present(5))
            end for
        }

        // --- MockWebSocketServer (scripted subscription end-to-end) -------------

        "MockWebSocketServer drives a subscription: ack, events, complete" in {
            val ws = new MockWebSocketServer
            val client = kyo.apollo.ApolloClient
                .builder()
                .serverUrl("https://example.test/graphql")
                .webSocketServerUrl("wss://example.test/graphql")
                .webSocketEngine(ws)
                .build()
            var seen: List[Int] = Nil
            for
                _ <- StreamProbe.drain(client.subscription(Fixtures.ValueSubscription()).stream)((r: ApolloResponse[Int]) =>
                    r.data.foreach(v => seen = seen :+ v)
                )
                _ <- settle
                _ <- Sync.defer(ws.ack())
                _ <- settle
                _ <- Sync.defer { ws.next("0", 10); ws.next("0", 20) }
                _ <- settle
                _ <- Sync.defer(ws.complete("0"))
                _ <- settle
            yield
                assert(seen == List(10, 20))
                assert(ws.sent.exists(_.contains("connection_init")))
                assert(ws.opens.map(_._1) == List("wss://example.test/graphql"))
            end for
        }

        // --- StreamProbe + frame builders --------------------------------------

        "StreamProbe.collect / first read a finite stream" in {
            val stream = Stream.init(Seq(1, 2, 3))
            for
                all  <- StreamProbe.collect(stream)
                head <- StreamProbe.first(Stream.init(Seq(9)))
            yield
                assert(all == List(1, 2, 3))
                assert(head == 9)
            end for
        }

        "WsFrames build the two protocol wire forms" in {
            assert(
                WsFrames.modern
                    .next("0", 3) == """{"id":"0","type":"next","payload":{"data":{"value":3}}}"""
            )
            assert(
                WsFrames.legacy
                    .next("0", 3) == """{"id":"0","type":"data","payload":{"data":{"value":3}}}"""
            )
            assert(WsFrames.modern.keepAlive == """{"type":"ping"}""")
            assert(WsFrames.legacy.keepAlive == """{"type":"ka"}""")
        }
    }

    private val requestId = Uuid("00000000-0000-4000-8000-000000000001")

    /** A chain whose continuation answers nothing: a terminal transport never calls it. */
    private object InertChain extends ApolloInterceptorChain:
        def proceed[D](request: ApolloRequest[D])(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.empty[ApolloResponse[D]]
    end InertChain
end TestingModuleSpec
