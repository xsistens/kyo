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
  * reconnect-then-resubscribe with two active subscriptions. Every step waits on
  * a barrier: a frame the transport sent, a response a subscriber received, or a
  * timer armed on the controlled clock.
  */
class SubscriptionScenarioSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.request

    private val init = """{"type":"connection_init"}"""

    final private class Fixture(protocol: WsProtocol = GraphQLWsProtocol):
        val conn   = new FakeWebSocketConnection
        val engine = new FakeWebSocketEngine(conn)
        val transport = new WebSocketNetworkTransport(
            serverUrl = "wss://example.com/graphql",
            protocol = protocol,
            engine = engine
        )
    end Fixture

    private def startFrame(kind: String)(frame: String): Boolean = frame.contains(s"\"type\":\"$kind\"")

    /** Answer the handshake on `conn` with `ackFrame` and collect the next `count` start frames. */
    private def established(
        conn: FakeWebSocketConnection,
        count: Int,
        ackFrame: String = ack,
        kind: String = "subscribe"
    )(using Frame): Chunk[String] < Async =
        conn.awaitSent(_ == init)
            .andThen(Sync.defer(conn.server(ackFrame)))
            .andThen(Kyo.foreach(Chunk.from(1 to count))(_ => conn.awaitSent(startFrame(kind))))

    private def data(pull: StreamProbe.Pull[ApolloResponse[Int]], n: Int)(using Frame): Chunk[Int] < Async =
        Kyo.foreach(Chunk.from(1 to n))(_ => pull.next.map(_.data.getOrElse(-1)))

    private def wsClosed(r: ApolloResponse[Int], code: Int): Boolean =
        r.error.exists {
            case e: ApolloWebSocketClosedException => e.code == code
            case _                                 => false
        }

    "subscription scenarios" - {

        "handshake — legacy protocol negotiates the graphql-ws subprotocol and starts only after ack" in Clock
            .withTimeControl { _ =>
                val f = new Fixture(SubscriptionWsProtocol)
                for
                    _     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                    first <- f.conn.nextSent
                    _ = assert(f.engine.opens == List(("wss://example.com/graphql", Some("graphql-ws"))))
                    _     <- Sync.defer(f.conn.server(WsTestSupport.legacy.ack))
                    start <- f.conn.nextSent
                yield
                    assert(first == init)
                    assert(startFrame("start")(start))
                end for
            }

        "routing — interleaved Data frames reach only their owning subscriber (three-way)" in Clock
            .withTimeControl { _ =>
                val f  = new Fixture
                val sa = f.transport.subscribe(request())
                val sb = f.transport.subscribe(request())
                val sc = f.transport.subscribe(request())
                for
                    a <- StreamProbe.Pull.open(sa)
                    b <- StreamProbe.Pull.open(sb)
                    c <- StreamProbe.Pull.open(sc)
                    _ <- established(f.conn, 3)
                    _ = assert(f.engine.opens.size == 1)
                    _ <- Sync.defer {
                        f.conn.server(next("1", 10))
                        f.conn.server(next("0", 20))
                        f.conn.server(next("2", 30))
                        f.conn.server(next("1", 11))
                        f.conn.server(next("0", 21))
                    }
                    seenA <- data(a, 2)
                    seenB <- data(b, 2)
                    seenC <- data(c, 1)
                yield
                    assert(seenA == Chunk(20, 21))
                    assert(seenB == Chunk(10, 11))
                    assert(seenC == Chunk(30))
                end for
            }

        "complete — ending the middle of three multiplexed subscriptions leaves the others streaming" in Clock
            .withTimeControl { _ =>
                val f  = new Fixture
                val sa = f.transport.subscribe(request())
                val sb = f.transport.subscribe(request())
                val sc = f.transport.subscribe(request())
                for
                    a     <- StreamProbe.Pull.open(sa)
                    doneB <- Fiber.init(Scope.run(StreamProbe.collect(sb)))
                    c     <- StreamProbe.Pull.open(sc)
                    _     <- established(f.conn, 3)
                    _ <- Sync.defer {
                        f.conn.server(next("0", 1))
                        f.conn.server(next("1", 2))
                        f.conn.server(next("2", 3))
                        f.conn.server(complete("1"))
                        f.conn.server(next("0", 4))
                        f.conn.server(next("2", 5))
                    }
                    seenB <- doneB.get
                    seenA <- data(a, 2)
                    seenC <- data(c, 2)
                yield
                    assert(seenB.flatMap(_.data) == List(2))
                    assert(seenA == Chunk(1, 4))
                    assert(seenC == Chunk(3, 5))
                end for
            }

        "multiplex — three subscriptions share one socket with distinct operation ids" in Clock.withTimeControl { _ =>
            val f  = new Fixture
            val sa = f.transport.subscribe(request())
            val sb = f.transport.subscribe(request())
            val sc = f.transport.subscribe(request())
            for
                _      <- StreamProbe.Pull.open(sa)
                _      <- StreamProbe.Pull.open(sb)
                _      <- StreamProbe.Pull.open(sc)
                starts <- established(f.conn, 3)
            yield
                assert(f.engine.opens.size == 1)
                assert(starts.exists(_.contains("\"id\":\"0\"")))
                assert(starts.exists(_.contains("\"id\":\"1\"")))
                assert(starts.exists(_.contains("\"id\":\"2\"")))
            end for
        }

        "keepalive — a legacy server 'ka' interleaved mid-stream draws no reply and passes the surrounding data through in order" in Clock
            .withTimeControl { _ =>
                val f = new Fixture(SubscriptionWsProtocol)
                for
                    pull <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                    _    <- established(f.conn, 1, WsTestSupport.legacy.ack, kind = "start")
                    // A `ka` *between* two data frames, not in isolation: this is how a real
                    // server paces keepalives during an active subscription. Interleaving proves
                    // the frame is consumed transparently — the data on either side still
                    // arrives, in order.
                    _ <- Sync.defer {
                        f.conn.server(WsTestSupport.legacy.data("0", 7))
                        f.conn.server(WsTestSupport.legacy.ka)
                        f.conn.server(WsTestSupport.legacy.data("0", 8))
                    }
                    seen <- data(pull, 2)
                    // All three frames are processed by now: had any of them drawn a reply, it
                    // would go out before the stop frame this cancel sends.
                    _    <- pull.cancel
                    stop <- f.conn.nextSent
                yield
                    assert(seen == Chunk(7, 8)) // the ka neither dropped nor reordered the surrounding data
                    assert(stop == """{"id":"0","type":"stop"}""", s"expected only the stop frame, got $stop")
                end for
            }

        "keepalive — a modern server ping is answered with a pong and streaming continues" in Clock.withTimeControl { _ =>
            val f = new Fixture
            for
                pull  <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _     <- established(f.conn, 1)
                _     <- Sync.defer(f.conn.server(WsTestSupport.modern.ping))
                reply <- f.conn.nextSent
                _     <- Sync.defer(f.conn.server(next("0", 9)))
                event <- pull.next
            yield
                assert(reply == """{"type":"pong"}""")
                assert(event.data == Present(9))
            end for
        }

        "reconnect — a scripted drop resubscribes both active subscriptions, which stream again" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                    backoff = Schedule.fixed(1.second)
                )
                val sa = transport.subscribe(request())
                val sb = transport.subscribe(request())
                for
                    a      <- StreamProbe.Pull.open(sa)
                    b      <- StreamProbe.Pull.open(sb)
                    c0     <- engine.nextConnection
                    _      <- established(c0, 2)
                    _      <- Sync.defer { c0.server(next("0", 1)); c0.server(next("1", 2)) }
                    a1     <- a.next
                    b1     <- b.next
                    _      <- Sync.defer(c0.drop(1011, "boom"))
                    a2     <- a.next
                    b2     <- b.next
                    _      <- control.awaitPendingSleepers(1) // the reconnect backoff
                    _      <- control.advance(1.second, Duration.Zero)
                    c1     <- engine.nextConnection
                    starts <- established(c1, 2)
                    _      <- Sync.defer { c1.server(next("0", 3)); c1.server(next("1", 4)) }
                    a3     <- a.next
                    b3     <- b.next
                yield
                    assert(List(a1, a3).flatMap(_.data) == List(1, 3))
                    assert(List(b1, b3).flatMap(_.data) == List(2, 4))
                    assert(wsClosed(a2, 1011))
                    assert(wsClosed(b2, 1011))
                    assert(engine.conns.size == 2)
                    assert(starts.exists(_.contains("\"id\":\"0\"")))
                    assert(starts.exists(_.contains("\"id\":\"1\"")))
                end for
            }
    }
end SubscriptionScenarioSpec
