package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ApolloResponse

/** Drives [[WebSocketNetworkTransport]] against the shared scripted in-memory
  * [[FakeWebSocketConnection]] and a manual [[ManualWsScheduler]] — no real
  * socket, no server, no wall-clock timers. The socket doubles and the
  * `{ "value": Int }` subscription fixture live in [[FakeWebSocket]] /
  * [[WsTestSupport]] so this spec, the scenario matrix, and the client/cache
  * specs all drive one implementation.
  *
  * Covers the Task 4 contract: the init/ack handshake, per-id routing of
  * `Data`, `Complete` terminating one subscription while another keeps
  * streaming, multiplexing two subscriptions over one socket, keepalive/ping
  * handling, the ack timeout as a value, cancel sending the protocol stop, and
  * the idle close after the last subscription ends. Also covers the Task 5
  * reconnection contract: a dropped socket reopens, re-runs `connection_init`,
  * and resubscribes active subscriptions (surfacing the drop as a value first);
  * a reconnection that fails to re-establish is itself retried; and with
  * reconnection disabled a drop terminates the subscription.
  *
  * On kyo-test each leaf body IS the effect: subscriptions are drained on forked
  * fibers via [[StreamProbe.drain]] / [[StreamProbe.collect]], scripted frames
  * are pushed with `Sync.defer`, and a short `Async.sleep` (`settle`) lets kyo's
  * scheduler dispatch the transport's background fibers between frames.
  */
class WebSocketNetworkTransportSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.request

    // ---- fixtures -------------------------------------------------------------

    final private class Fixture:
        val conn      = new FakeWebSocketConnection
        val engine    = new FakeWebSocketEngine(conn)
        val scheduler = new ManualWsScheduler
        val transport = new WebSocketNetworkTransport(
            serverUrl = "wss://example.com/graphql",
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

    "WebSocketNetworkTransport" - {

        "opens with the protocol subprotocol and sends connection_init" in {
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle
            yield
                assert(f.engine.opens == List(("wss://example.com/graphql", Some("graphql-transport-ws"))))
                assert(f.conn.sent.head == """{"type":"connection_init"}""")
            end for
        }

        "start is sent only after connection_ack" in {
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle
                // Before ack, only connection_init has gone out.
                _ = assert(f.conn.sent == List("""{"type":"connection_init"}"""))
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
            yield assert(f.conn.sent.exists(_.contains("\"type\":\"subscribe\"")))
            end for
        }

        "routes Data to the subscribing stream as typed responses" in {
            val f    = new Fixture
            var seen = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => seen = seen :+ v)
                )
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer { f.conn.server(next("0", 11)); f.conn.server(next("0", 22)) }
                _ <- settle
            yield assert(seen == List(11, 22))
            end for
        }

        "Complete ends one subscription without affecting the other (multiplexing)" in {
            val f = new Fixture
            var b = List.empty[Int]
            for
                doneA <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r =>
                    r.data.foreach(v => b = b :+ v)
                )
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                // One socket carries both; ids 0 and 1 route independently.
                _ = assert(f.engine.opens.size == 1)
                _ <- Sync.defer {
                    f.conn.server(next("0", 1))
                    f.conn.server(next("1", 2))
                    f.conn.server(complete("0")) // ends A only
                    f.conn.server(next("1", 3))  // B still streams
                }
                a <- doneA.get
                _ <- settle
            yield
                assert(a.flatMap(_.data) == List(1))
                assert(b == List(2, 3))
            end for
        }

        "a server ping is answered with a pong" in {
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer(f.conn.server("""{"type":"ping"}"""))
            yield assert(f.conn.sent.contains("""{"type":"pong"}"""))
            end for
        }

        "cancelling a subscription sends the protocol stop for its id" in {
            val f = new Fixture
            for
                cancelable <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _          <- settle
                _          <- Sync.defer(f.conn.server(ack))
                _          <- settle
                _          <- cancelable.interrupt
                _          <- settle
            // Modern protocol: stop == complete for the op id.
            yield assert(f.conn.sent.contains("""{"id":"0","type":"complete"}"""))
            end for
        }

        "the socket is closed after the last subscription ends (idle timeout)" in {
            val f = new Fixture
            for
                _ <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer(f.conn.server(complete("0"))) // last (only) subscription ends
                _ = assert(f.scheduler.pending == 1) // idle timer armed
                _ <- Sync.defer(f.scheduler.fireAll())
                _ <- settle
            yield assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }

        "an ack timeout surfaces as an ApolloNetworkException value" in {
            val f    = new Fixture
            var seen = List.empty[ApolloResponse[Int]]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => seen = seen :+ r)
                _ <- settle
                // No ack delivered; fire the armed ack timer.
                _ <- Sync.defer(f.scheduler.fireAll())
                _ <- settle
            yield
                assert(seen.size == 1)
                assert(seen.head.exception.exists(_.isInstanceOf[ApolloNetworkException]))
            end for
        }

        "a socket drop surfaces as an ApolloWebSocketClosedException value" in {
            val f    = new Fixture
            var seen = List.empty[ApolloResponse[Int]]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => seen = seen :+ r)
                _ <- settle
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle
                _ <- Sync.defer(f.conn.drop(1011, "server error"))
                _ <- settle
            yield assert(seen.exists(_.exception.exists {
                case e: ApolloWebSocketClosedException => e.code == 1011
                case _                                 => false
            }))
            end for
        }

        "a dropped socket reconnects, re-runs connection_init, and resubscribes active subscriptions" in {
            val engine    = new FreshWebSocketEngine
            val scheduler = new ManualWsScheduler
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                scheduler = scheduler,
                reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                backoff = WsBackoff.constant(1000)
            )
            var seen = List.empty[ApolloResponse[Int]]
            for
                _  <- StreamProbe.drain(transport.subscribe(request()))(r => seen = seen :+ r)
                _  <- settle
                c0 <- Sync.defer(engine.conns.head)
                _  <- Sync.defer(c0.server(ack))
                _  <- settle
                _  <- Sync.defer(c0.server(next("0", 1)))
                _  <- settle
                _ = assert(seen.flatMap(_.data) == List(1))
                // The established socket drops abnormally.
                _ <- Sync.defer(c0.drop(1011, "boom"))
                _ <- settle
                // The drop surfaced as a WS-closed value (the resubscription signal)...
                _ = assert(seen.exists(wsClosed(_, 1011)))
                // ...and a reconnect is armed on the scheduler (nothing else pending).
                _ = assert(scheduler.pending == 1)
                _ <- Sync.defer(scheduler.fireAll())
                _ <- settle
                // A second socket was opened and re-initialised.
                _ = assert(engine.conns.size == 2)
                c1 <- Sync.defer(engine.conns(1))
                _ = assert(c1.sent.contains("""{"type":"connection_init"}"""))
                _ <- Sync.defer(c1.server(ack))
                _ <- settle
                // The subscription was resubscribed under its original id...
                _ = assert(c1.sent.exists(_.contains("\"type\":\"subscribe\"")))
                // ...and streams again on the reopened socket.
                _ <- Sync.defer(c1.server(next("0", 2)))
                _ <- settle
            yield assert(seen.flatMap(_.data) == List(1, 2))
            end for
        }

        "a reconnection that fails to re-establish is itself retried" in {
            val engine    = new FreshWebSocketEngine
            val scheduler = new ManualWsScheduler
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                scheduler = scheduler,
                reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                backoff = WsBackoff.constant(1000)
            )
            for
                _  <- StreamProbe.drain(transport.subscribe(request()))(_ => ())
                _  <- settle
                c0 <- Sync.defer(engine.conns.head)
                _  <- Sync.defer(c0.server(ack))
                _  <- settle
                _  <- Sync.defer(c0.drop(1011, "boom")) // established socket drops
                _  <- settle
                _  <- Sync.defer(scheduler.fireAll())   // fire backoff → reconnect attempt 1 opens c1
                _  <- settle
                _ = assert(engine.conns.size == 2)
                _ <- Sync.defer(engine.conns(1).drop(1012, "again")) // reconnect drops before its ack
                _ <- settle
                // Attempt 1 failed to re-establish → another reconnect is armed.
                _ = assert(scheduler.pending == 1)
                _ <- Sync.defer(scheduler.fireAll()) // fire backoff → reconnect attempt 2 opens c2
                _ <- settle
            yield assert(engine.conns.size == 3)
            end for
        }

        "with reconnection disabled a socket drop terminates the subscription with a WS-closed value" in {
            val engine    = new FreshWebSocketEngine
            val scheduler = new ManualWsScheduler
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                scheduler = scheduler,
                reconnectWhen = WebSocketNetworkTransport.reconnectNever
            )
            for
                done <- Fiber.init(Scope.run(StreamProbe.collect(transport.subscribe(request()))))
                _    <- settle
                c0   <- Sync.defer(engine.conns.head)
                _    <- Sync.defer(c0.server(ack))
                _    <- settle
                _    <- Sync.defer(c0.drop(1011, "boom"))
                // collect completes when the subscription terminates.
                seen <- done.get
            yield
                assert(seen.exists(wsClosed(_, 1011)))
                assert(engine.conns.size == 1) // no reopen
                assert(scheduler.pending == 0) // no reconnect armed
            end for
        }
    }
end WebSocketNetworkTransportSpec
