package kyo.apollo.testing

import kyo.*
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.network.ApolloResponse

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
            val engine = TestHttpEngine.returning("""{"data":{"value":7}}""")
            val client = TestApolloClient.cacheless(engine)
            client.query(Fixtures.ValueQuery()).execute.map { response =>
                assert(response.data == Present(7))
                assert(engine.calls == 1)
                assert(engine.lastRequest.exists(_.body.exists(_.contains("Value"))))
            }
        }

        "TestHttpEngine.failing surfaces as an ApolloResponse.exception value" in {
            val engine = TestHttpEngine.failing(new RuntimeException("down"))
            val client = TestApolloClient.cacheless(engine)
            client.query(Fixtures.ValueQuery()).execute.map { response =>
                assert(response.data == Absent)
                assert(response.exception.isDefined)
            }
        }

        // --- MockServer --------------------------------------------------------

        "MockServer serves enqueued responses FIFO and records requests" in {
            val server = new MockServer
            server.enqueue("""{"data":{"value":1}}""").enqueue("""{"data":{"value":2}}""")
            val client = TestApolloClient.cacheless(server)
            for
                first  <- client.query(Fixtures.ValueQuery()).execute
                second <- client.query(Fixtures.ValueQuery()).execute
            yield
                assert(first.data == Present(1))
                assert(second.data == Present(2))
                assert(server.requestCount == 2)
                assert(server.takeRequest().body.exists(_.contains("Value")))
            end for
        }

        // --- QueueTestNetworkTransport -----------------------------------------

        "QueueTestNetworkTransport answers operation-layer, in order" in {
            val transport = new QueueTestNetworkTransport
            transport.enqueueData(41).enqueueData(42)
            val client = TestApolloClient.withTransport(transport)
            for
                a <- client.query(Fixtures.ValueQuery()).execute
                b <- client.query(Fixtures.ValueQuery()).execute
            yield
                assert(a.data == Present(41))
                assert(b.data == Present(42))
                assert(transport.operationNames == List("Value", "Value"))
            end for
        }

        // --- MapTestNetworkTransport -------------------------------------------

        "MapTestNetworkTransport routes each operation to its registered response" in {
            val transport = new MapTestNetworkTransport
            transport
                .registerData(Fixtures.ValueQuery(), 99)
                .registerData(Fixtures.CurrentUserQuery(), Fixtures.userData("Alice"))
            val client = TestApolloClient.withTransport(transport)
            for
                value <- client.query(Fixtures.ValueQuery()).execute
                user  <- client.query(Fixtures.CurrentUserQuery()).execute
            yield
                assert(value.data == Present(99))
                assert(user.data.exists(_.user.name == "Alice"))
            end for
        }

        // --- TestApolloClient.cached (cache integration) -----------------------

        "cached: a CacheFirst re-read is served from the cache, not the engine" in {
            val engine = TestHttpEngine.returning(Fixtures.body("Alice"))
            val client = TestApolloClient.cached(engine)
            for
                first  <- client.query(Fixtures.CurrentUserQuery()).execute
                second <- client.query(Fixtures.CurrentUserQuery()).execute
            yield
                assert(first.data.exists(_.user.name == "Alice"))
                assert(second.data.exists(_.user.name == "Alice"))
                assert(engine.calls == 1) // second read hit the normalized cache
            end for
        }

        "cachedWithTransport wires a cache above a canned transport" in {
            val transport = new MapTestNetworkTransport
            transport.registerData(Fixtures.CurrentUserQuery(), Fixtures.userData("Bob"))
            val client = TestApolloClient.cachedWithTransport(transport)
            for
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
                .webSocketScheduler(ws.scheduler)
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
end TestingModuleSpec
