package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ApolloResponse

/** Drives [[WebSocketNetworkTransport]] against the shared scripted in-memory
  * [[FakeWebSocketConnection]] and a `Clock.withTimeControl` clock — no real
  * socket, no server, no wall-clock timers. The socket doubles and the
  * `{ "value": Int }` subscription fixture live in [[FakeWebSocket]] /
  * [[WsTestSupport]] so this spec, the scenario matrix, and the client/cache
  * specs all drive one implementation.
  *
  * Covers the handshake, per-id routing, `Complete` terminating one subscription
  * while another streams, multiplexing over one socket, keepalive/ping handling,
  * the ack timeout as a value, cancel sending the protocol stop, and the idle
  * close after the last subscription ends. Also the reconnection contract: a
  * dropped socket reopens, re-runs `connection_init`, and resubscribes active
  * subscriptions (surfacing the drop as a value first); a reconnection that fails
  * to re-establish is itself retried; and with reconnection disabled a drop
  * terminates the subscription.
  *
  * On kyo-test each leaf body IS the effect: the whole test runs inside
  * `Clock.withTimeControl`, so the transport's `Clock`-driven ack / idle / backoff
  * timers fire deterministically via `control.advance`. `settle` advances virtual
  * time by zero with a small real wall delay, yielding to kyo's scheduler so the
  * owner fiber dispatches the frames scripted just before.
  */
class WebSocketNetworkTransportSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.request

    final private class Fixture:
        val conn   = new FakeWebSocketConnection
        val engine = new FakeWebSocketEngine(conn)
        val transport = new WebSocketNetworkTransport(
            serverUrl = "wss://example.com/graphql",
            engine = engine
        )
    end Fixture

    /** Yield to kyo's scheduler (a small real wall delay, no virtual advance) so the
      * transport's owner fiber dispatches the frames scripted just before.
      */
    private def settle(control: Clock.TimeControl)(using Frame): Unit < Async =
        control.advance(0.millis, 30.millis)

    private def wsClosed(r: ApolloResponse[Int], code: Int): Boolean =
        r.error.exists {
            case e: ApolloWebSocketClosedException => e.code == code
            case _                                 => false
        }

    "WebSocketNetworkTransport" - {

        "opens with the protocol subprotocol and sends connection_init" in Clock.withTimeControl { control =>
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle(control)
            yield
                assert(f.engine.opens == List(("wss://example.com/graphql", Some("graphql-transport-ws"))))
                assert(f.conn.sent.head == """{"type":"connection_init"}""")
            end for
        }

        "start is sent only after connection_ack" in Clock.withTimeControl { control =>
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle(control)
                _ = assert(f.conn.sent == List("""{"type":"connection_init"}"""))
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle(control)
            yield assert(f.conn.sent.exists(_.contains("\"type\":\"subscribe\"")))
            end for
        }

        "routes Data to the subscribing stream as typed responses" in Clock.withTimeControl { control =>
            val f    = new Fixture
            var seen = List.empty[Int]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => seen = seen :+ v))
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle(control)
                _ <- Sync.defer { f.conn.server(next("0", 11)); f.conn.server(next("0", 22)) }
                _ <- settle(control)
            yield assert(seen == List(11, 22))
            end for
        }

        "Complete ends one subscription without affecting the other (multiplexing)" in Clock.withTimeControl {
            control =>
                val f = new Fixture
                var b = List.empty[Int]
                for
                    doneA <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                    _     <- StreamProbe.drain(f.transport.subscribe(request()))(r => r.data.foreach(v => b = b :+ v))
                    _     <- settle(control)
                    _     <- Sync.defer(f.conn.server(ack))
                    _     <- settle(control)
                    _ = assert(f.engine.opens.size == 1)
                    _ <- Sync.defer {
                        f.conn.server(next("0", 1))
                        f.conn.server(next("1", 2))
                        f.conn.server(complete("0"))
                        f.conn.server(next("1", 3))
                    }
                    a <- doneA.get
                    _ <- settle(control)
                yield
                    assert(a.flatMap(_.data) == List(1))
                    assert(b == List(2, 3))
                end for
        }

        "a next, complete and the socket's clean close in one burst still deliver both frames, then end cleanly" in Clock
            .withTimeControl { control =>
                val f = new Fixture
                for
                    doneA <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                    _     <- settle(control)
                    _     <- Sync.defer(f.conn.server(ack))
                    _     <- settle(control)
                    // The ordinary end of a finite subscription: the server's last `next`,
                    // its `complete` and its close frame arrive back-to-back, before the
                    // transport's drain has taken any of them. Both frames must still
                    // reach the subscriber, and the socket's clean close must not turn
                    // into an ApolloWebSocketClosedException value after the `complete`.
                    _ <- Sync.defer {
                        f.conn.server(next("0", 1))
                        f.conn.server(complete("0"))
                        f.conn.serverClose()
                    }
                    a <- doneA.get
                yield
                    assert(a.flatMap(_.data) == List(1))
                    assert(a.forall(_.error.isEmpty), s"expected a clean end after complete, got $a")
                end for
            }

        "a server ping is answered with a pong" in Clock.withTimeControl { control =>
            val f = new Fixture
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server("""{"type":"ping"}"""))
                _ <- settle(control)
            yield assert(f.conn.sent.contains("""{"type":"pong"}"""))
            end for
        }

        "cancelling a subscription sends the protocol stop for its id" in Clock.withTimeControl { control =>
            val f = new Fixture
            for
                cancelable <- StreamProbe.drain(f.transport.subscribe(request()))(_ => ())
                _          <- settle(control)
                _          <- Sync.defer(f.conn.server(ack))
                _          <- settle(control)
                _          <- cancelable.interrupt
                _          <- settle(control)
            yield assert(f.conn.sent.contains("""{"id":"0","type":"complete"}"""))
            end for
        }

        "the socket is closed after the last subscription ends (idle timeout)" in Clock.withTimeControl { control =>
            val f = new Fixture
            for
                _ <- Fiber.init(Scope.run(StreamProbe.collect(f.transport.subscribe(request()))))
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server(complete("0"))) // last (only) subscription ends
                _ <- settle(control)
                _ = assert(f.conn.closedWith.isEmpty) // idle timer armed but not yet fired
                _ <- control.advance(60.seconds) // fire the idle timeout
                _ <- settle(control)
            yield assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }

        "an ack timeout surfaces as an ApolloNetworkException value" in Clock.withTimeControl { control =>
            val f    = new Fixture
            var seen = List.empty[ApolloResponse[Int]]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => seen = seen :+ r)
                _ <- settle(control)
                _ <- control.advance(10.seconds) // no ack delivered; fire the ack timer
                _ <- settle(control)
            yield
                assert(seen.size == 1)
                assert(seen.head.error.exists(_.isInstanceOf[ApolloNetworkException]))
            end for
        }

        "a socket drop surfaces as an ApolloWebSocketClosedException value" in Clock.withTimeControl { control =>
            val f    = new Fixture
            var seen = List.empty[ApolloResponse[Int]]
            for
                _ <- StreamProbe.drain(f.transport.subscribe(request()))(r => seen = seen :+ r)
                _ <- settle(control)
                _ <- Sync.defer(f.conn.server(ack))
                _ <- settle(control)
                _ <- Sync.defer(f.conn.drop(1011, "server error"))
                _ <- settle(control)
            yield assert(seen.exists(wsClosed(_, 1011)))
            end for
        }

        "a dropped socket reconnects, re-runs connection_init, and resubscribes active subscriptions" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                    backoff = Schedule.fixed(1.second)
                )
                var seen = List.empty[ApolloResponse[Int]]
                for
                    _  <- StreamProbe.drain(transport.subscribe(request()))(r => seen = seen :+ r)
                    _  <- settle(control)
                    c0 <- Sync.defer(engine.conns.head)
                    _  <- Sync.defer(c0.server(ack))
                    _  <- settle(control)
                    _  <- Sync.defer(c0.server(next("0", 1)))
                    _  <- settle(control)
                    _ = assert(seen.flatMap(_.data) == List(1))
                    _ <- Sync.defer(c0.drop(1011, "boom"))
                    _ <- settle(control)
                    _ = assert(seen.exists(wsClosed(_, 1011)))
                    _ = assert(engine.conns.size == 1) // no reopen until backoff elapses
                    _ <- control.advance(1.second) // fire the reconnect backoff
                    _ <- settle(control)
                    _ = assert(engine.conns.size == 2)
                    c1 <- Sync.defer(engine.conns(1))
                    _ = assert(c1.sent.contains("""{"type":"connection_init"}"""))
                    _ <- Sync.defer(c1.server(ack))
                    _ <- settle(control)
                    _ = assert(c1.sent.exists(_.contains("\"type\":\"subscribe\"")))
                    _ <- Sync.defer(c1.server(next("0", 2)))
                    _ <- settle(control)
                yield assert(seen.flatMap(_.data) == List(1, 2))
                end for
            }

        "a reconnection that fails to re-establish is itself retried" in Clock.withTimeControl { control =>
            val engine = new FreshWebSocketEngine
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                backoff = Schedule.fixed(1.second)
            )
            for
                _  <- StreamProbe.drain(transport.subscribe(request()))(_ => ())
                _  <- settle(control)
                c0 <- Sync.defer(engine.conns.head)
                _  <- Sync.defer(c0.server(ack))
                _  <- settle(control)
                _  <- Sync.defer(c0.drop(1011, "boom"))
                _  <- settle(control)
                _  <- control.advance(1.second) // backoff -> reconnect attempt 1 opens c1
                _  <- settle(control)
                _ = assert(engine.conns.size == 2)
                _ <- Sync.defer(engine.conns(1).drop(1012, "again")) // reconnect drops before its ack
                _ <- settle(control)
                _ <- control.advance(1.second)                       // backoff -> reconnect attempt 2 opens c2
                _ <- settle(control)
            yield assert(engine.conns.size == 3)
            end for
        }

        "a reconnect schedule with no delay left terminates the subscription with the drop" in Clock.withTimeControl {
            control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                    backoff = Schedule.fixed(1.second).take(1)
                )
                for
                    done <- Fiber.init(Scope.run(StreamProbe.collect(transport.subscribe(request()))))
                    _    <- settle(control)
                    c0   <- Sync.defer(engine.conns.head)
                    _    <- Sync.defer(c0.server(ack))
                    _    <- settle(control)
                    _    <- Sync.defer(c0.drop(1011, "boom"))
                    _    <- settle(control)
                    _    <- control.advance(1.second) // the schedule's one delay -> attempt 1 opens c1
                    _    <- settle(control)
                    _ = assert(engine.conns.size == 2)
                    _    <- Sync.defer(engine.conns(1).drop(1012, "again")) // drops before its ack; no delay left
                    seen <- done.get
                    _    <- control.advance(5.seconds)
                    _    <- settle(control)
                yield
                    assert(seen.exists(wsClosed(_, 1012)), s"the subscription ends with the last drop: $seen")
                    assert(engine.conns.size == 2, "no attempt beyond the schedule")
                end for
        }

        "the reconnect schedule starts over once a reconnection is acknowledged" in Clock.withTimeControl { control =>
            val engine = new FreshWebSocketEngine
            val transport = new WebSocketNetworkTransport(
                serverUrl = "wss://example.com/graphql",
                engine = engine,
                reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                backoff = Schedule.exponential(1.second, 2.0)
            )
            for
                _  <- StreamProbe.drain(transport.subscribe(request()))(_ => ())
                _  <- settle(control)
                c0 <- Sync.defer(engine.conns.head)
                _  <- Sync.defer(c0.server(ack))
                _  <- settle(control)
                _  <- Sync.defer(c0.drop(1011, "boom"))
                _  <- settle(control)
                _  <- control.advance(1.second) // the first delay -> attempt 1 opens c1
                _  <- settle(control)
                _ = assert(engine.conns.size == 2)
                c1 <- Sync.defer(engine.conns(1))
                _  <- Sync.defer(c1.server(ack)) // re-established
                _  <- settle(control)
                _  <- Sync.defer(c1.drop(1011, "boom again"))
                _  <- settle(control)
                _  <- control.advance(999.millis)
                _  <- settle(control)
                _ = assert(engine.conns.size == 2, "no reopen before the first delay has passed")
                _ <- control.advance(1.milli) // 1s again, not the 2s a continued schedule would wait
                _ <- settle(control)
            yield assert(engine.conns.size == 3, s"the schedule must start over after an ack, got ${engine.conns.size} sockets")
            end for
        }

        "with reconnection disabled a socket drop terminates the subscription with a WS-closed value" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectNever
                )
                for
                    done <- Fiber.init(Scope.run(StreamProbe.collect(transport.subscribe(request()))))
                    _    <- settle(control)
                    c0   <- Sync.defer(engine.conns.head)
                    _    <- Sync.defer(c0.server(ack))
                    _    <- settle(control)
                    _    <- Sync.defer(c0.drop(1011, "boom"))
                    seen <- done.get
                    _    <- control.advance(5.seconds) // prove no reconnect is armed
                    _    <- settle(control)
                yield
                    assert(seen.exists(wsClosed(_, 1011)))
                    assert(engine.conns.size == 1) // no reopen
                end for
            }

        "cancelling the last subscription during a reconnect backoff resets to idle, not stranded" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                val transport = new WebSocketNetworkTransport(
                    serverUrl = "wss://example.com/graphql",
                    engine = engine,
                    reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
                    backoff = Schedule.fixed(1.second)
                )
                var seen = List.empty[ApolloResponse[Int]]
                for
                    sub <- StreamProbe.drain(transport.subscribe(request()))(_ => ())
                    _   <- settle(control)
                    c0  <- Sync.defer(engine.conns.head)
                    _   <- Sync.defer(c0.server(ack))
                    _   <- settle(control)
                    _   <- Sync.defer(c0.drop(1011, "boom")) // abnormal drop -> reconnect backoff armed
                    _   <- settle(control)
                    _ = assert(engine.conns.size == 1) // backoff pending, not yet reopened
                    // Cancel the only subscription WHILE the backoff is armed.
                    _ <- sub.interrupt
                    _ <- settle(control)
                    // The reconnect existed only to resubscribe the now-gone route, so
                    // advancing past the backoff must NOT reopen — the machine reset to idle.
                    _ <- control.advance(1.second)
                    _ <- settle(control)
                    _ = assert(engine.conns.size == 1)
                    // The regression: a fresh subscribe must open a NEW socket. On the
                    // stranded machine (reconnecting stuck true) onRegister would merely
                    // store the route and wait on an ack that never comes — conns stays 1.
                    _ <- StreamProbe.drain(transport.subscribe(request()))(r => seen = seen :+ r)
                    _ <- settle(control)
                    _ = assert(engine.conns.size == 2)
                    c1 <- Sync.defer(engine.conns(1))
                    _  <- Sync.defer(c1.server(ack))
                    _  <- settle(control)
                    _  <- Sync.defer(c1.server(next("1", 7))) // second subscription's id is "1"
                    _  <- settle(control)
                yield assert(seen.flatMap(_.data) == List(7))
                end for
            }
    }
end WebSocketNetworkTransportSpec
