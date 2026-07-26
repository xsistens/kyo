package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ApolloResponse

/** The Task 8 subscription scenario matrix, driven entirely through the shared
  * scripted [[FakeWebSocketConnection]] / [[FreshWebSocketEngine]] /
  * [[ManualWsScheduler]] doubles and the [[WsTestSupport]] fixture — no real
  * socket, no server, no wall-clock timers.
  *
  * Each test maps to one of the six axes Task 8 calls out, and is deliberately
  * *complementary* to [[WebSocketNetworkTransportSpec]] rather than a restatement
  * of it: this suite exercises the **legacy** `subscriptions-transport-ws`
  * protocol end-to-end at the transport level (handshake, `start`/`data`/`stop`,
  * one-way `ka` keepalive), pushes multiplexing to **three** concurrent
  * subscriptions over one socket with interleaved routing, and drives
  * reconnect-then-resubscribe with **two** active subscriptions so both are
  * re-sent under their original ids after a scripted drop.
  *
  * On kyo-test each leaf body IS the effect: subscriptions are drained on forked
  * fibers via [[StreamProbe.drain]] (accumulating into `var`s), scripted frames
  * are pushed with `Sync.defer`, and a short `Async.sleep` (`settle`) lets kyo's
  * scheduler dispatch the transport's background fibers between frames — the
  * effect-native replacement for the old `Future`-chained macrotask `flush`.
  */
class SubscriptionScenarioSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.request

    // ---- fixtures -------------------------------------------------------------

    /** A transport over a single scripted socket, defaulting to the modern
      * protocol; pass [[SubscriptionWsProtocol]] to drive the legacy wire.
      */
    final private class Fixture(protocol: WsProtocol = GraphQLWsProtocol):
        val conn      = new FakeWebSocketConnection
        val engine    = new FakeWebSocketEngine(conn)
        val scheduler = new ManualWsScheduler
        val transport = new WebSocketNetworkTransport(
            serverUrl = "wss://example.com/graphql",
            protocol = protocol,
            engine = engine,
            scheduler = scheduler
        )
    end Fixture

    /** Yield to kyo's scheduler so the transport's background fibers dispatch the
      * frames scripted just before — the effect-native form of the old macrotask
      * `flush`. A single real `Async.sleep` lets all pending fibers run.
      */
    private def settle(using Frame): Unit < Async = Async.sleep(30L.millis)

    private def wsClosed(r: ApolloResponse[Int], code: Int): Boolean =
        r.exception.exists {
            case e: ApolloWebSocketClosedException => e.code == code
            case _                                 => false
        }

    "subscription scenarios" - {

        // ---- 1. init / ack handshake --------------------------------------------

        "handshake — legacy protocol negotiates the graphql-ws subprotocol and starts only after ack" in {
            val f = new Fixture(SubscriptionWsProtocol)
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle
                // Legacy subprotocol token is the bare "graphql-ws".
                _ = assert(f.engine.opens == List(("wss://example.com/graphql", Some("graphql-ws"))))
                // Only connection_init has gone out before the ack.
                _ = assert(f.conn.sent == List("""{"type":"connection_init"}"""))
                _ <- Sync.defer(f.conn.server(WsTestSupport.legacy.ack))
                _ <- settle
            // The legacy protocol starts operations with a `start` frame.
            yield assert(f.conn.sent.exists(_.contains("\"type\":\"start\"")))
            end for
        }

        // ---- 2. routing Data to the right subscriber ----------------------------

        "routing — interleaved Data frames reach only their owning subscriber (three-way)" in {
            val f = new Fixture
            var a = List.empty[Int]
            var b = List.empty[Int]
            var c = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => a = a :+ v)
                )
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => b = b :+ v)
                )
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => c = c :+ v)
                )
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ = assert(f.engine.opens.size == 1) // one shared socket
                // Deliver out of id order to prove per-id routing, not arrival order.
                _ <- Sync.defer {
                    f.conn.server(next("1", 10))
                    f.conn.server(next("0", 20))
                    f.conn.server(next("2", 30))
                    f.conn.server(next("1", 11))
                    f.conn.server(next("0", 21))
                }
                _ <- settle
            yield
                assert(a == List(20, 21))
                assert(b == List(10, 11))
                assert(c == List(30))
            end for
        }

        // ---- 3. Complete terminates one subscription only -----------------------

        "complete — ending the middle of three multiplexed subscriptions leaves the others streaming" in {
            val f = new Fixture
            var a = List.empty[Int]
            var c = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => a = a :+ v)
                )
                doneB <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => c = c :+ v)
                )
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer {
                    f.conn.server(next("0", 1))
                    f.conn.server(next("1", 2))
                    f.conn.server(next("2", 3))
                    f.conn.server(complete("1")) // ends the middle subscription only
                    f.conn.server(next("0", 4))
                    f.conn.server(next("2", 5))
                }
                b <- doneB.get
                _ <- settle
            yield
                assert(b.flatMap(_.data) == List(2)) // stopped at its complete
                assert(a == List(1, 4))              // unaffected
                assert(c == List(3, 5))              // unaffected
            end for
        }

        // ---- 4. multiplexing over one socket ------------------------------------

        "multiplex — three subscriptions share one socket with distinct operation ids" in {
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ = assert(f.engine.opens.size == 1) // exactly one socket opened
                starts <- Sync.defer(f.conn.sent.filter(_.contains("\"type\":\"subscribe\"")))
            yield
                assert(starts.size == 3)
                assert(starts.exists(_.contains("\"id\":\"0\"")))
                assert(starts.exists(_.contains("\"id\":\"1\"")))
                assert(starts.exists(_.contains("\"id\":\"2\"")))
            end for
        }

        // ---- 5. keepalive handling ----------------------------------------------

        "keepalive — a legacy server 'ka' draws no reply and the subscription keeps streaming" in {
            val f    = new Fixture(SubscriptionWsProtocol)
            var seen = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => seen = seen :+ v)
                )
                _      <- settle
                _      <- Sync.defer(f.conn.server(WsTestSupport.legacy.ack))
                _      <- settle
                before <- Sync.defer(f.conn.sent)
                _      <- Sync.defer(f.conn.server(WsTestSupport.legacy.ka))
                // Legacy `ka` is one-way — the client sends nothing back.
                _ = assert(f.conn.sent == before)
                // ...and the socket is still live: a data frame after the ka is delivered.
                _ <- Sync.defer(f.conn.server(WsTestSupport.legacy.data("0", 7)))
                _ <- settle
            yield assert(seen == List(7))
            end for
        }

        "keepalive — a modern server ping is answered with a pong and streaming continues" in {
            val f    = new Fixture
            var seen = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => seen = seen :+ v)
                )
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer(f.conn.server(WsTestSupport.modern.ping))
                _ = assert(f.conn.sent.contains("""{"type":"pong"}"""))
                _ <- Sync.defer(f.conn.server(next("0", 9)))
                _ <- settle
            yield assert(seen == List(9))
            end for
        }

        // ---- 6. reconnect then resubscribe after a scripted drop ----------------

        "reconnect — a scripted drop resubscribes both active subscriptions, which stream again" in {
            val engine    = new FreshWebSocketEngine
            val scheduler = new ManualWsScheduler
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                scheduler = scheduler,
                reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                backoff = WsBackoff.constant(1000)
            )
            var a = List.empty[ApolloResponse[Int]]
            var b = List.empty[ApolloResponse[Int]]
            for
                _  <- StreamProbe.drain(transport.subscribe(request()))(r => a = a :+ r)
                _  <- StreamProbe.drain(transport.subscribe(request()))(r => b = b :+ r)
                _  <- settle
                c0 <- Sync.defer(engine.conns.head)
                _  <- Sync.defer(c0.server(ack))
                _  <- settle
                _  <- Sync.defer { c0.server(next("0", 1)); c0.server(next("1", 2)) }
                _  <- settle
                _ = assert(a.flatMap(_.data) == List(1))
                _ = assert(b.flatMap(_.data) == List(2))
                // The established socket drops abnormally.
                _ <- Sync.defer(c0.drop(1011, "boom"))
                _ <- settle
                // Both subscribers see the drop as the resubscription-signal value...
                _ = assert(a.exists(wsClosed(_, 1011)))
                _ = assert(b.exists(wsClosed(_, 1011)))
                // ...and one reconnect is armed.
                _ = assert(scheduler.pending == 1)
                _ <- Sync.defer(scheduler.fireAll())
                _ <- settle
                _ = assert(engine.conns.size == 2)
                c1 <- Sync.defer(engine.conns(1))
                _ = assert(c1.sent.contains("""{"type":"connection_init"}"""))
                _ <- Sync.defer(c1.server(ack))
                _ <- settle
                // Both subscriptions are resubscribed under their original ids...
                starts <- Sync.defer(c1.sent.filter(_.contains("\"type\":\"subscribe\"")))
                _ = assert(starts.exists(_.contains("\"id\":\"0\"")))
                _ = assert(starts.exists(_.contains("\"id\":\"1\"")))
                // ...and both resume streaming on the reopened socket.
                _ <- Sync.defer { c1.server(next("0", 3)); c1.server(next("1", 4)) }
                _ <- settle
            yield
                assert(a.flatMap(_.data) == List(1, 3))
                assert(b.flatMap(_.data) == List(2, 4))
            end for
        }
    }
end SubscriptionScenarioSpec
