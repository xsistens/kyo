package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.cache.LogProbe
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
  * the ack timeout as a value (also for a socket that never opens), cancel sending
  * the protocol stop, and the idle close after the last subscription ends. The
  * reconnection contract: a dropped socket reopens, re-runs `connection_init`, and
  * resubscribes active subscriptions (surfacing the drop as a value first), never
  * while the previous socket is still open; a reconnection that fails to
  * re-establish is itself retried; and with reconnection disabled a drop terminates
  * the subscription. The lifecycle: `close` waits for the socket's release and lets
  * live subscriptions finish for its grace period, and a closed transport answers a
  * subscription with a closed value rather than hanging. Unroutable frames are
  * logged, and a slow consumer holds the socket read back without losing a frame.
  *
  * Every wait is a barrier: a frame the transport sent (`nextSent` / `awaitSent`),
  * a response the subscriber received, a connection the engine opened or released,
  * a sleeper registered on the controlled clock (`awaitPendingSleepers`) before the
  * clock is advanced. Nothing waits on the wall clock.
  */
class WebSocketNetworkTransportSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    import WsTestSupport.modern.ack
    import WsTestSupport.modern.complete
    import WsTestSupport.modern.next
    import WsTestSupport.modern.ping
    import WsTestSupport.request

    private val url  = "wss://example.com/graphql"
    private val init = """{"type":"connection_init"}"""
    private val pong = """{"type":"pong"}"""

    final private case class Fixture(conn: FakeWebSocketConnection, engine: FakeWebSocketEngine, transport: WebSocketNetworkTransport)

    /** A transport over one scripted connection, owned by the enclosing `Scope`. */
    private def fixture(releaseDelay: Duration = Duration.Zero, bufferSize: Int = 256)(using Frame): Fixture < (Sync & Scope) =
        val conn   = new FakeWebSocketConnection
        val engine = new FakeWebSocketEngine(conn, releaseDelay)
        WebSocketNetworkTransport.init(serverUrl = url, engine = engine, subscriptionBufferSize = bufferSize)
            .map(Fixture(conn, engine, _))
    end fixture

    private def reconnecting(engine: WebSocketEngine, backoff: Schedule)(using Frame): WebSocketNetworkTransport < (Sync & Scope) =
        WebSocketNetworkTransport.init(
            serverUrl = url,
            engine = engine,
            reconnectWhen = WebSocketNetworkTransport.reconnectAlways,
            backoff = backoff
        )

    private def isSubscribe(id: String)(frame: String): Boolean =
        frame.contains("\"type\":\"subscribe\"") && frame.contains(s"\"id\":\"$id\"")

    /** Answer the handshake on `conn` and wait until the start frames of `ids` went out. */
    private def established(conn: FakeWebSocketConnection, ids: String*)(using Frame): Unit < Async =
        conn.awaitSent(_ == init)
            .andThen(Sync.defer(conn.server(ack)))
            .andThen(Kyo.foreachDiscard(Chunk.from(ids))(_ => conn.awaitSent(_.contains("\"type\":\"subscribe\""))))

    /** Advance the controlled clock by `d` once a timer is armed on it. */
    private def fire(control: Clock.TimeControl, d: Duration)(using Frame): Unit < Async =
        control.awaitPendingSleepers(1).andThen(control.advance(d, Duration.Zero))

    private def wsClosed(r: ApolloResponse[Int], code: Int): Boolean =
        r.error.exists {
            case e: ApolloWebSocketClosedException => e.code == code
            case _                                 => false
        }

    private def networkFailure(r: ApolloResponse[Int], text: String): Boolean =
        r.error.exists {
            case e: ApolloNetworkException => e.message.contains(text)
            case _                         => false
        }

    "WebSocketNetworkTransport" - {

        "opens with the protocol subprotocol and sends connection_init" in Clock.withTimeControl { _ =>
            for
                f     <- fixture()
                _     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                first <- f.conn.nextSent
            yield
                assert(f.engine.opens == List((url, Some("graphql-transport-ws"))))
                assert(first == init)
            end for
        }

        "start is sent only after connection_ack" in Clock.withTimeControl { _ =>
            for
                f     <- fixture()
                _     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                first <- f.conn.nextSent
                // A ping before the ack is answered in frame order: had the start frame
                // gone out before the ack, it would sit between the init and the pong.
                _         <- Sync.defer(f.conn.server(ping))
                beforeAck <- f.conn.nextSent
                _         <- Sync.defer(f.conn.server(ack))
                afterAck  <- f.conn.nextSent
            yield
                assert(first == init)
                assert(beforeAck == pong)
                assert(isSubscribe("0")(afterAck))
            end for
        }

        "routes Data to the subscribing stream as typed responses" in Clock.withTimeControl { _ =>
            for
                f      <- fixture()
                pull   <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _      <- established(f.conn, "0")
                _      <- Sync.defer { f.conn.server(next("0", 11)); f.conn.server(next("0", 22)) }
                first  <- pull.next
                second <- pull.next
            yield assert(List(first, second).flatMap(_.data) == List(11, 22))
            end for
        }

        "Complete ends one subscription without affecting the other (multiplexing)" in Clock.withTimeControl { _ =>
            fixture().map { f =>
                // Both ids are fixed here, in call order, before any fiber runs.
                val a = f.transport.subscribe(request())
                val b = f.transport.subscribe(request())
                for
                    doneA <- Fiber.init(Scope.run(StreamProbe.collect(a)))
                    pullB <- StreamProbe.Pull.open(b)
                    _     <- established(f.conn, "0", "1")
                    _ <- Sync.defer {
                        f.conn.server(next("0", 1))
                        f.conn.server(next("1", 2))
                        f.conn.server(complete("0"))
                        f.conn.server(next("1", 3))
                    }
                    seenA <- doneA.get
                    b1    <- pullB.next
                    b2    <- pullB.next
                yield
                    assert(f.engine.opens.size == 1)
                    assert(seenA.flatMap(_.data) == List(1))
                    assert(List(b1, b2).flatMap(_.data) == List(2, 3))
                end for
            }
        }

        "a next, complete and the socket's clean close in one burst still deliver both frames, then end cleanly" in Clock
            .withTimeControl { _ =>
                fixture().map { f =>
                    val a = f.transport.subscribe(request())
                    for
                        doneA <- Fiber.init(Scope.run(StreamProbe.collect(a)))
                        _     <- established(f.conn, "0")
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
                        seen <- doneA.get
                    yield
                        assert(seen.flatMap(_.data) == List(1))
                        assert(seen.forall(_.error.isEmpty), s"expected a clean end after complete, got $seen")
                    end for
                }
            }

        "a server ping is answered with a pong" in Clock.withTimeControl { _ =>
            for
                f     <- fixture()
                _     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _     <- established(f.conn, "0")
                _     <- Sync.defer(f.conn.server(ping))
                reply <- f.conn.nextSent
            yield assert(reply == pong)
            end for
        }

        "cancelling a subscription sends the protocol stop for its id" in Clock.withTimeControl { _ =>
            fixture().map { f =>
                val subscription = f.transport.subscribe(request())
                for
                    consumer <- Fiber.init(Scope.run(subscription.discard))
                    _        <- established(f.conn, "0")
                    _        <- consumer.interrupt
                    stop     <- f.conn.nextSent
                yield assert(stop == """{"id":"0","type":"complete"}""")
                end for
            }
        }

        "the socket is closed after the last subscription ends (idle timeout)" in Clock.withTimeControl { control =>
            fixture().map { f =>
                val subscription = f.transport.subscribe(request())
                for
                    done <- Fiber.init(Scope.run(StreamProbe.collect(subscription)))
                    _    <- established(f.conn, "0")
                    _    <- Sync.defer(f.conn.server(complete("0"))) // last (only) subscription ends
                    _    <- done.get
                    _    <- control.awaitPendingSleepers(1)          // the idle timer is armed ...
                    _ = assert(f.conn.closedWith.isEmpty) // ... and has not fired
                    _ <- control.advance(60.seconds, Duration.Zero)
                    _ <- f.conn.awaitReleased
                yield assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                end for
            }
        }

        "an ack timeout surfaces as an ApolloNetworkException value that counts the frames received" in Clock.withTimeControl {
            control =>
                for
                    f        <- fixture()
                    pull     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                    _        <- f.conn.awaitSent(_ == init)
                    _        <- fire(control, 10.seconds) // no ack delivered; the ack timer fires
                    response <- pull.next
                yield assert(networkFailure(response, "0 frames received"), s"got $response")
                end for
        }

        "a handshake that never completes is ended by the ack timer" in Clock.withTimeControl { control =>
            val engine = new HangingWebSocketEngine
            for
                transport <- WebSocketNetworkTransport.init(serverUrl = url, engine = engine)
                pull      <- StreamProbe.Pull.open(transport.subscribe(request()))
                _         <- engine.awaitOpenStarted
                _         <- fire(control, 10.seconds)
                response  <- pull.next
            yield assert(networkFailure(response, "the socket did not open"), s"got $response")
            end for
        }

        "a socket drop surfaces as an ApolloWebSocketClosedException value" in Clock.withTimeControl { _ =>
            for
                f        <- fixture()
                pull     <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _        <- established(f.conn, "0")
                _        <- Sync.defer(f.conn.drop(1011, "server error"))
                response <- pull.next
            yield assert(wsClosed(response, 1011))
            end for
        }

        "a dropped socket reconnects, re-runs connection_init, and resubscribes active subscriptions" in Clock
            .withTimeControl { control =>
                val engine = new FreshWebSocketEngine
                for
                    transport <- reconnecting(engine, Schedule.fixed(1.second))
                    pull      <- StreamProbe.Pull.open(transport.subscribe(request()))
                    c0        <- engine.nextConnection
                    _         <- established(c0, "0")
                    _         <- Sync.defer(c0.server(next("0", 1)))
                    first     <- pull.next
                    _         <- Sync.defer(c0.drop(1011, "boom"))
                    dropped   <- pull.next
                    _         <- control.awaitPendingSleepers(1) // the reconnect backoff is armed
                    _ = assert(engine.conns.size == 1) // no reopen until backoff elapses
                    _      <- control.advance(1.second, Duration.Zero)
                    c1     <- engine.nextConnection
                    _      <- established(c1, "0")
                    _      <- Sync.defer(c1.server(next("0", 2)))
                    second <- pull.next
                yield
                    assert(first.data == Present(1))
                    assert(wsClosed(dropped, 1011))
                    assert(second.data == Present(2))
                end for
            }

        "a reconnect opens its socket only after the dropped one is released" in Clock.withTimeControl { control =>
            // Releasing a connection's Scope takes 5s here; the backoff is 1s.
            val engine = new FreshWebSocketEngine(releaseDelay = 5.seconds)
            for
                transport <- reconnecting(engine, Schedule.fixed(1.second))
                _         <- StreamProbe.Pull.open(transport.subscribe(request()))
                c0        <- engine.nextConnection
                _         <- established(c0, "0")
                _         <- Sync.defer(c0.drop(1011, "boom"))
                // Step the clock a second at a time; each step waits for whatever timer is
                // armed next (the release, the backoff, then the new socket's ack timer).
                _   <- Kyo.foreachDiscard(Chunk.from(1 to 10))(_ => fire(control, 1.second))
                c1  <- engine.nextConnection
                max <- engine.maxConcurrentlyOpen
                // Close inside the controlled clock: releasing c1 takes 5s of it, which the
                // Scope's close after the block could not advance.
                _      <- established(c1, "0")
                closer <- Fiber.init(transport.closeNow)
                _      <- fire(control, 5.seconds)
                _      <- closer.get
            yield assert(max == 1, s"$max sockets were open at the same time")
            end for
        }

        "a reconnection that fails to re-establish is itself retried" in Clock.withTimeControl { control =>
            val engine = new FreshWebSocketEngine
            for
                transport <- reconnecting(engine, Schedule.fixed(1.second))
                pull      <- StreamProbe.Pull.open(transport.subscribe(request()))
                c0        <- engine.nextConnection
                _         <- established(c0, "0")
                _         <- Sync.defer(c0.drop(1011, "boom"))
                _         <- pull.next                          // the drop value
                _         <- fire(control, 1.second)            // backoff -> reconnect attempt 1 opens c1
                c1        <- engine.nextConnection
                _         <- c1.awaitSent(_ == init)
                _         <- Sync.defer(c1.drop(1012, "again")) // reconnect drops before its ack
                _         <- c1.awaitReleased                   // the failed attempt is torn down, its ack timer stopped
                _         <- fire(control, 1.second)            // backoff -> reconnect attempt 2 opens c2
                _         <- engine.nextConnection
            yield assert(engine.conns.size == 3)
            end for
        }

        "a reconnect schedule with no delay left terminates the subscription with the drop" in Clock.withTimeControl {
            control =>
                val engine = new FreshWebSocketEngine
                reconnecting(engine, Schedule.fixed(1.second).take(1)).map { transport =>
                    val subscription = transport.subscribe(request())
                    for
                        done <- Fiber.init(Scope.run(StreamProbe.collect(subscription)))
                        c0   <- engine.nextConnection
                        _    <- established(c0, "0")
                        _    <- Sync.defer(c0.drop(1011, "boom"))
                        _    <- fire(control, 1.second)            // the schedule's one delay -> attempt 1 opens c1
                        c1   <- engine.nextConnection
                        _    <- c1.awaitSent(_ == init)
                        _    <- Sync.defer(c1.drop(1012, "again")) // drops before its ack; no delay left
                        seen <- done.get
                    yield
                        assert(seen.exists(wsClosed(_, 1011)), s"the first drop surfaces: $seen")
                        assert(seen.last.error.exists(_.isInstanceOf[ApolloWebSocketClosedException]))
                        assert(wsClosed(seen.last, 1012), s"the subscription ends with the last drop: $seen")
                        assert(engine.conns.size == 2, "no attempt beyond the schedule")
                    end for
                }
        }

        "the reconnect schedule starts over once a reconnection is acknowledged" in Clock.withTimeControl { control =>
            val engine = new FreshWebSocketEngine
            for
                transport <- reconnecting(engine, Schedule.exponential(1.second, 2.0))
                pull      <- StreamProbe.Pull.open(transport.subscribe(request()))
                c0        <- engine.nextConnection
                _         <- established(c0, "0")
                _         <- Sync.defer(c0.drop(1011, "boom"))
                _         <- pull.next
                _         <- fire(control, 1.second) // the first delay -> attempt 1 opens c1
                c1        <- engine.nextConnection
                _         <- established(c1, "0")    // re-established
                _         <- Sync.defer(c1.drop(1011, "boom again"))
                _         <- pull.next
                _         <- control.awaitPendingSleepers(1)
                _         <- control.advance(999.millis, Duration.Zero)
                _ = assert(engine.conns.size == 2, "no reopen before the first delay has passed")
                _ <- control.advance(1.milli, Duration.Zero) // 1s again, not the 2s a continued schedule would wait
                _ <- engine.nextConnection
            yield assert(engine.conns.size == 3)
            end for
        }

        "with reconnection disabled a socket drop terminates the subscription with a WS-closed value" in Clock
            .withTimeControl { _ =>
                val engine = new FreshWebSocketEngine
                for
                    transport <- WebSocketNetworkTransport.init(
                        serverUrl = url,
                        engine = engine,
                        reconnectWhen = WebSocketNetworkTransport.reconnectNever
                    )
                    done <- Fiber.init(Scope.run(StreamProbe.collect(transport.subscribe(request()))))
                    c0   <- engine.nextConnection
                    _    <- established(c0, "0")
                    _    <- Sync.defer(c0.drop(1011, "boom"))
                    seen <- done.get
                yield
                    assert(seen.exists(wsClosed(_, 1011)))
                    assert(engine.conns.size == 1)
                end for
            }

        "cancelling the last subscription during a reconnect backoff resets to idle, not stranded" in Clock
            .withTimeControl { _ =>
                val engine = new FreshWebSocketEngine
                reconnecting(engine, Schedule.fixed(1.second)).map { transport =>
                    val first  = transport.subscribe(request()) // id "0"
                    val second = transport.subscribe(request()) // id "1"
                    for
                        // Takes the drop value and ends: its Scope has sent the Cancel when `get` returns.
                        firstDone <- Fiber.init(Scope.run(first.take(1).run))
                        c0        <- engine.nextConnection
                        _         <- established(c0, "0")
                        _         <- Sync.defer(c0.drop(1011, "boom")) // abnormal drop -> reconnect backoff armed
                        dropped   <- firstDone.get
                        // The reconnect existed only to resubscribe the now-gone route, so the
                        // machine is idle again: a fresh subscription opens a NEW socket at once,
                        // without the clock moving. On a stranded machine it would wait for an
                        // ack of a socket that is never opened.
                        pull  <- StreamProbe.Pull.open(second)
                        c1    <- engine.nextConnection
                        _     <- established(c1, "1")
                        _     <- Sync.defer(c1.server(next("1", 7)))
                        event <- pull.next
                    yield
                        assert(dropped.exists(wsClosed(_, 1011)))
                        assert(event.data == Present(7))
                        assert(engine.conns.size == 2)
                    end for
                }
            }

        "a subscription after close yields a closed value instead of hanging" in Clock.withTimeControl { _ =>
            for
                f         <- fixture()
                before    <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _         <- established(f.conn, "0")
                _         <- f.transport.closeNow
                ended     <- before.next
                afterward <- Scope.run(StreamProbe.collect(f.transport.subscribe(request())))
            yield
                assert(wsClosed(ended, WebSocketConnection.NormalClosure))
                assert(afterward.size == 1)
                assert(wsClosed(afterward.head, WebSocketConnection.NormalClosure))
                assert(f.engine.opens.size == 1)
            end for
        }

        "a transport closed before its first subscription opens nothing and answers a subscription with a closed value" in Clock
            .withTimeControl { _ =>
                for
                    f         <- fixture()
                    _         <- f.transport.closeNow
                    afterward <- Scope.run(StreamProbe.collect(f.transport.subscribe(request())))
                yield
                    assert(afterward.size == 1)
                    assert(wsClosed(afterward.head, WebSocketConnection.NormalClosure))
                    assert(f.engine.opens.isEmpty)
                end for
            }

        "the Scope a transport is created in closes it: its socket is closed and a later subscription gets the closed value" in Clock
            .withTimeControl { _ =>
                for
                    f <- Scope.run {
                        for
                            f    <- fixture()
                            pull <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                            _    <- established(f.conn, "0")
                            _    <- Sync.defer(f.conn.server(next("0", 1)))
                            _    <- pull.next
                        yield f
                    }
                    afterward <- Scope.run(StreamProbe.collect(f.transport.subscribe(request())))
                yield
                    assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                    assert(afterward.size == 1)
                    assert(wsClosed(afterward.head, WebSocketConnection.NormalClosure))
                    assert(f.engine.opens.size == 1)
                end for
            }

        "close and subscribe racing never hang: the subscription gets exactly one closed value" in Clock.withTimeControl {
            _ =>
                Kyo.foreachDiscard(Chunk.from(1 to 50)) { _ =>
                    // One Scope per round, so each round's transport is gone before the next.
                    Scope.run {
                        for
                            f <- fixture()
                            subscription = f.transport.subscribe(request())
                            start      <- Latch.init(1)
                            subscriber <- Fiber.init(start.await.andThen(Scope.run(StreamProbe.collect(subscription))))
                            closer     <- Fiber.init(start.await.andThen(f.transport.closeNow))
                            _          <- start.release
                            seen       <- subscriber.get
                            _          <- closer.get
                        yield
                            assert(seen.size == 1, s"got $seen")
                            assert(wsClosed(seen.head, WebSocketConnection.NormalClosure), s"got $seen")
                        end for
                    }
                }
        }

        "close returns only after the connection's Scope is released" in Clock.withTimeControl { control =>
            // Releasing the connection's Scope takes 5s here.
            for
                f      <- fixture(releaseDelay = 5.seconds)
                _      <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _      <- established(f.conn, "0")
                closer <- Fiber.init(f.transport.closeNow)
                _      <- control.awaitPendingSleepers(1) // the release has begun
                early  <- closer.done
                _ = assert(!early, "close returned while the connection was still being released")
                _ = assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                _ <- control.advance(5.seconds, Duration.Zero)
                _ <- closer.get
                _ <- f.conn.awaitReleased
            yield succeed
            end for
        }

        "close lets an active subscription finish within its grace period" in Clock.withTimeControl { control =>
            fixture().map { f =>
                val subscription = f.transport.subscribe(request())
                for
                    done   <- Fiber.init(Scope.run(StreamProbe.collect(subscription)))
                    _      <- established(f.conn, "0")
                    closer <- Fiber.init(f.transport.close(10.seconds))
                    _      <- control.awaitPendingSleepers(1) // the grace period is running
                    _      <- Sync.defer { f.conn.server(next("0", 5)); f.conn.server(complete("0")) }
                    seen   <- done.get
                    _      <- closer.get                      // without the clock moving
                yield
                    assert(seen.flatMap(_.data) == List(5))
                    assert(seen.forall(_.error.isEmpty), s"the subscription completed on its own: $seen")
                    assert(f.conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
                end for
            }
        }

        "a subscription that outlives the grace period receives the closed value" in Clock.withTimeControl { control =>
            for
                f      <- fixture()
                pull   <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _      <- established(f.conn, "0")
                closer <- Fiber.init(f.transport.close(10.seconds))
                _      <- fire(control, 10.seconds)
                ended  <- pull.next
                _      <- closer.get
            yield assert(wsClosed(ended, WebSocketConnection.NormalClosure))
            end for
        }

        "an unknown frame is logged at warn without its content, and a frame for an unknown id at debug" in Clock
            .withTimeControl { _ =>
                for
                    probe <- LogProbe.init
                    _ <- probe.run {
                        for
                            // Created inside the probe: the owner fiber logs through the context it is started in.
                            f <- fixture()
                            _ <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                            _ <- established(f.conn, "0")
                            _ <- Sync.defer {
                                f.conn.server("""{"type":"data","id":"0","payload":{"token":"s3cr3t"}}""")
                                f.conn.server(next("7", 1))
                                f.conn.server(ping)
                            }
                            _ <- f.conn.awaitSent(_ == pong) // both frames before the ping were processed
                        yield ()
                    }
                    lines <- probe.lines
                yield
                    val warns  = lines.filter(_.level == Log.Level.warn).map(_.message)
                    val debugs = lines.filter(_.level == Log.Level.debug).map(_.message)
                    assert(
                        warns.exists(m => m.contains("type 'data'") && m.contains("graphql-transport-ws")),
                        s"warn lines: $warns"
                    )
                    assert(!lines.exists(_.message.contains("s3cr3t")), s"a log line carries the frame's content: $lines")
                    assert(debugs.exists(m => m.contains("'next'") && m.contains("'7'")), s"debug lines: $debugs")
                end for
            }

        "the owner runs in the context the transport was created in, not the first subscriber's" in Clock.withTimeControl { _ =>
            for
                probe <- LogProbe.init
                // Only the creation runs under the probe's Log; the subscription runs outside it.
                f <- probe.run(fixture())
                _ <- StreamProbe.Pull.open(f.transport.subscribe(request()))
                _ <- established(f.conn, "0")
                _ <- Sync.defer {
                    f.conn.server("""{"type":"data","id":"0","payload":{}}""")
                    f.conn.server(ping)
                }
                _     <- f.conn.awaitSent(_ == pong)
                lines <- probe.lines
            yield assert(lines.exists(_.level == Log.Level.warn), s"the owner's warn line did not reach the creator's Log: $lines")
            end for
        }

        "a slow subscriber holds the socket read back, and every frame arrives in order" in Clock.withTimeControl { _ =>
            val bufferSize = 2
            val frames     = 20
            fixture(bufferSize = bufferSize).map { f =>
                val subscription = f.transport.subscribe(request())
                for
                    gate     <- Latch.init(1)
                    received <- AtomicRef.init(Chunk.empty[Int])
                    consumer <- Fiber.init(Scope.run(subscription.take(frames).foreach { r =>
                        gate.await.andThen(received.getAndUpdate(_.append(r.data.getOrElse(-1))))
                    }))
                    _ <- established(f.conn, "0")
                    _ <- Sync.defer((1 to frames).foreach(i => f.conn.server(next("0", i))))
                    // With the consumer parked on its first response, at least this many data frames
                    // are taken: one in the consumer's hand, the buffer, one the owner holds, one the
                    // drain holds. The ack is the first frame read.
                    _         <- f.conn.awaitFramesRead(1 + 1 + bufferSize + 1 + 1)
                    readAhead <- f.conn.framesRead
                    _         <- gate.release
                    _         <- consumer.get
                    seen      <- received.get
                yield
                    // At most the consumer's chunk (the buffer plus one waiting put), the refilled
                    // buffer, the owner's frame and the drain's frame.
                    assert(readAhead - 1 <= 2 * bufferSize + 3, s"${readAhead - 1} data frames read ahead of the consumer")
                    assert(seen == Chunk.from(1 to frames))
                end for
            }
        }
    }
end WebSocketNetworkTransportSpec
