package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ApolloResponse

/** The subscription scenario matrix, driven entirely through the shared scripted
  * [[FakeWebSocketConnection]] / [[FreshWebSocketEngine]] doubles and the
  * [[WsTestSupport]] fixture under `Clock.withTimeControl` — no real socket, no
  * server, no wall-clock timers.
  *
  * Complementary to [[WebSocketNetworkTransportSpec]]: this suite exercises the
  * legacy `subscriptions-transport-ws` protocol end-to-end (handshake,
  * `start`/`data`/`stop`, one-way `ka` keepalive), pushes multiplexing to three
  * concurrent subscriptions with interleaved routing, and drives
  * reconnect-then-resubscribe with two active subscriptions.
  */
class SubscriptionScenarioSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.request

    final private class Fixture(protocol: WsProtocol = GraphQLWsProtocol):
        val conn   = new FakeWebSocketConnection
        val engine = new FakeWebSocketEngine(conn)
        val transport = new WebSocketNetworkTransport(
            serverUrl = "wss://example.com/graphql",
            protocol = protocol,
            engine = engine
        )
    end Fixture

    private def settle(control: Clock.TimeControl)(using Frame): Unit < Async =
        control.advance(0.millis, 30.millis)

    private def wsClosed(r: ApolloResponse[Int], code: Int): Boolean =
        r.error.exists {
            case e: ApolloWebSocketClosedException => e.code == code
            case _                                 => false
        }

    "subscription scenarios" - {

        "handshake — legacy protocol negotiates the graphql-ws subprotocol and starts only after ack" in Clock
            .withTimeControl { control =>
                val f = new Fixture(SubscriptionWsProtocol)
                for
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                    _ <- settle(control)
                    _ = assert(f.engine.opens == List(("wss://example.com/graphql", Some("graphql-ws"))))
                    _ = assert(f.conn.sent == List("""{"type":"connection_init"}"""))
                    _ <- Sync.defer(f.conn.server(WsTestSupport.legacy.ack))
                    _ <- settle(control)
                yield assert(f.conn.sent.exists(_.contains("\"type\":\"start\"")))
                end for
            }

        "routing — interleaved Data frames reach only their owning subscriber (three-way)" in Clock
            .withTimeControl { control =>
                val f = new Fixture
                var a = List.empty[Int]
                var b = List.empty[Int]
                var c = List.empty[Int]
                for
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => a = a :+ v))
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => b = b :+ v))
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => c = c :+ v))
                    _ <- settle(control)
                    _ <- Sync.defer(f.conn.server(ack))
                    _ <- settle(control)
                    _ = assert(f.engine.opens.size == 1)
                    _ <- Sync.defer {
                        f.conn.server(next("1", 10))
                        f.conn.server(next("0", 20))
                        f.conn.server(next("2", 30))
                        f.conn.server(next("1", 11))
                        f.conn.server(next("0", 21))
                    }
                    _ <- settle(control)
                yield
                    assert(a == List(20, 21))
                    assert(b == List(10, 11))
                    assert(c == List(30))
                end for
            }

        "complete — ending the middle of three multiplexed subscriptions leaves the others streaming" in Clock
            .withTimeControl { control =>
                val f = new Fixture
                var a = List.empty[Int]
                var c = List.empty[Int]
                for
                    _     <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => a = a :+ v))
                    doneB <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                    _     <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => c = c :+ v))
                    _     <- settle(control)
                    _     <- Sync.defer(f.conn.server(ack))
                    _     <- settle(control)
                    _ <- Sync.defer {
                        f.conn.server(next("0", 1))
                        f.conn.server(next("1", 2))
                        f.conn.server(next("2", 3))
                        f.conn.server(complete("1"))
                        f.conn.server(next("0", 4))
                        f.conn.server(next("2", 5))
                    }
                    b <- doneB.get
                    _ <- settle(control)
                yield
                    assert(b.flatMap(_.data) == List(2))
                    assert(a == List(1, 4))
                    assert(c == List(3, 5))
                end for
            }

        "multiplex — three subscriptions share one socket with distinct operation ids" in Clock.withTimeControl {
            control =>
                val f = new Fixture
                for
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                    _ <- settle(control)
                    _ <- Sync.defer(f.conn.server(ack))
                    _ <- settle(control)
                    _ = assert(f.engine.opens.size == 1)
                    starts <- Sync.defer(f.conn.sent.filter(_.contains("\"type\":\"subscribe\"")))
                yield
                    assert(starts.size == 3)
                    assert(starts.exists(_.contains("\"id\":\"0\"")))
                    assert(starts.exists(_.contains("\"id\":\"1\"")))
                    assert(starts.exists(_.contains("\"id\":\"2\"")))
                end for
        }

        "keepalive — a legacy server 'ka' interleaved mid-stream draws no reply and passes the surrounding data through in order" in Clock
            .withTimeControl { control =>
                val f    = new Fixture(SubscriptionWsProtocol)
                var seen = List.empty[Int]
                for
                    _      <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => seen = seen :+ v))
                    _      <- settle(control)
                    _      <- Sync.defer(f.conn.server(WsTestSupport.legacy.ack))
                    _      <- settle(control)
                    before <- Sync.defer(f.conn.sent)
                    // A `ka` *between* two data frames, not in isolation: this is how a real
                    // server paces keepalives during an active subscription. Since the transport
                    // treats KeepAlive and Unknown identically (both no-ops), a bare idle `ka`
                    // asserts almost nothing; interleaving proves the frame is consumed
                    // transparently — the data on either side still arrives, in order.
                    _ <- Sync.defer {
                        f.conn.server(WsTestSupport.legacy.data("0", 7))
                        f.conn.server(WsTestSupport.legacy.ka)
                        f.conn.server(WsTestSupport.legacy.data("0", 8))
                    }
                    _ <- settle(control)
                    _ = assert(f.conn.sent == before) // neither the data nor the ka drew a client reply
                yield assert(seen == List(7, 8)) // the ka neither dropped nor reordered the surrounding data
                end for
            }

        "keepalive — a modern server ping is answered with a pong and streaming continues" in Clock.withTimeControl {
            control =>
                val f    = new Fixture
                var seen = List.empty[Int]
                for
                    _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => seen = seen :+ v))
                    _ <- settle(control)
                    _ <- Sync.defer(f.conn.server(ack))
                    _ <- settle(control)
                    _ <- Sync.defer(f.conn.server(WsTestSupport.modern.ping))
                    _ <- settle(control)
                    _ = assert(f.conn.sent.contains("""{"type":"pong"}"""))
                    _ <- Sync.defer(f.conn.server(next("0", 9)))
                    _ <- settle(control)
                yield assert(seen == List(9))
                end for
        }

        "reconnect — a scripted drop resubscribes both active subscriptions, which stream again" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                    backoff = WsBackoff.constant(1000)
                )
                var a = List.empty[ApolloResponse[Int]]
                var b = List.empty[ApolloResponse[Int]]
                for
                    _  <- StreamProbe.drain(transport.subscribe(request()))(r => a = a :+ r)
                    _  <- StreamProbe.drain(transport.subscribe(request()))(r => b = b :+ r)
                    _  <- settle(control)
                    c0 <- Sync.defer(engine.conns.head)
                    _  <- Sync.defer(c0.server(ack))
                    _  <- settle(control)
                    _  <- Sync.defer { c0.server(next("0", 1)); c0.server(next("1", 2)) }
                    _  <- settle(control)
                    _ = assert(a.flatMap(_.data) == List(1))
                    _ = assert(b.flatMap(_.data) == List(2))
                    _ <- Sync.defer(c0.drop(1011, "boom"))
                    _ <- settle(control)
                    _ = assert(a.exists(wsClosed(_, 1011)))
                    _ = assert(b.exists(wsClosed(_, 1011)))
                    _ <- control.advance(1.second)
                    _ <- settle(control)
                    _ = assert(engine.conns.size == 2)
                    c1 <- Sync.defer(engine.conns(1))
                    _ = assert(c1.sent.contains("""{"type":"connection_init"}"""))
                    _      <- Sync.defer(c1.server(ack))
                    _      <- settle(control)
                    starts <- Sync.defer(c1.sent.filter(_.contains("\"type\":\"subscribe\"")))
                    _ = assert(starts.exists(_.contains("\"id\":\"0\"")))
                    _ = assert(starts.exists(_.contains("\"id\":\"1\"")))
                    _ <- Sync.defer { c1.server(next("0", 3)); c1.server(next("1", 4)) }
                    _ <- settle(control)
                yield
                    assert(a.flatMap(_.data) == List(1, 3))
                    assert(b.flatMap(_.data) == List(2, 4))
                end for
            }
    }
end SubscriptionScenarioSpec
