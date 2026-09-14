package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloWebSocketClosedException
// Aliased to `sjs`: `kyo.test.Test` inherits a `js` platform-filter extension
// method, and an inherited member outranks an explicit import, so a bare
// `js.Dynamic` would bind to that extension. The alias sidesteps the clash.
import scala.scalajs.js as sjs

/** Drives [[JsWebSocketConnection]]'s event bridge with a scripted in-memory fake
  * WebSocket — no real socket, no server. The production [[JsWebSocketEngine]]
  * only resolves and news-up a platform `WebSocket`; the behaviour worth testing
  * (handshake signal, buffered ordered delivery, clean-vs-abnormal termination,
  * send-after-close) lives in the connection, exercised here via
  * [[JsWebSocketConnection.openWith]] feeding it the same `addEventListener` events
  * a real socket would.
  */
class JsWebSocketConnectionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A fake platform socket: records listeners, sends, and the close call, and
      * lets the test fire open/message/error/close events at will.
      */
    final private class FakeSocket:
        private val listeners =
            scala.collection.mutable.Map.empty[String, sjs.Function1[sjs.Dynamic, Unit]]
        var sent: List[String]                = List.empty
        var closedWith: Option[(Int, String)] = None

        private val addFn: sjs.Function2[String, sjs.Function1[sjs.Dynamic, Unit], Unit] =
            (event, listener) => listeners(event) = listener
        private val sendFn: sjs.Function1[String, Unit] =
            data => sent = sent :+ data
        private val closeFn: sjs.Function2[Int, String, Unit] =
            (code, reason) => closedWith = Some((code, reason))

        val asJs: JsWebSocket = sjs.Dynamic
            .literal(addEventListener = addFn, send = sendFn, close = closeFn)
            .asInstanceOf[JsWebSocket]

        private def fire(event: String, payload: sjs.Dynamic): Unit =
            listeners.get(event).foreach(_(payload))

        def open(): Unit                = fire("open", sjs.Dynamic.literal())
        def message(text: String): Unit = fire("message", sjs.Dynamic.literal(data = text))
        def error(): Unit               = fire("error", sjs.Dynamic.literal())
        def closeEvent(code: Int, reason: String = ""): Unit =
            fire("close", sjs.Dynamic.literal(code = code, reason = reason))
    end FakeSocket

    /** Fork `openWith` (it suspends on the socket's `open` event), let it attach its
      * listeners, then fire `open` so it resolves to the live connection.
      */
    private def open(fake: FakeSocket)(using
        Frame
    ): JsWebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        for
            fiber <- Fiber.init(JsWebSocketConnection.openWith(fake.asJs))
            _     <- Async.sleep(10.millis)
            _     <- Sync.defer(fake.open())
            conn  <- fiber.get
        yield conn

    "JsWebSocketConnection" - {

        "a normal (1000) close makes the liveness signal succeed" in {
            val fake = new FakeSocket
            for
                conn   <- open(fake)
                watch  <- Fiber.init(Abort.run[ApolloWebSocketClosedException](conn.closed))
                _      <- Async.sleep(10.millis)
                _      <- Sync.defer(fake.closeEvent(WebSocketConnection.NormalClosure))
                result <- watch.get
            yield assert(result.isSuccess)
            end for
        }

        "an abnormal close fails the liveness signal with ApolloWebSocketClosedException" in {
            val fake = new FakeSocket
            for
                conn   <- open(fake)
                watch  <- Fiber.init(Abort.run[ApolloWebSocketClosedException](conn.closed))
                _      <- Async.sleep(10.millis)
                _      <- Sync.defer(fake.closeEvent(1011, "server error"))
                result <- watch.get
            yield result match
                case Result.Failure(e) => assert(e.code == 1011 && e.reason == Some("server error"))
                case other             => assert(false, s"expected failure, got $other")
            end for
        }

        "buffers frames received before the stream drains, then delivers them in order" in {
            val fake = new FakeSocket
            for
                conn    <- open(fake)
                _       <- Sync.defer { fake.message("a"); fake.message("b") }
                drained <- Fiber.init(conn.incoming.take(2).run.map(_.toList))
                seen    <- drained.get
            yield assert(seen == List("a", "b"))
            end for
        }

        "frames received in the same burst as the close are all delivered, then the stream ends" in {
            val fake = new FakeSocket
            for
                conn <- open(fake)
                // The ordinary end of a finite subscription: the server's last `next`, its
                // `complete` and its close frame land in ONE synchronous burst, so the
                // `message` and `close` listeners all run before any fiber drains — on JS's
                // single carrier there is no other interleaving point. A hard `close` of the
                // frame channel here hands both frames to the closer and `run` sees none.
                _ <- Sync.defer {
                    fake.message("next")
                    fake.message("complete")
                    fake.closeEvent(WebSocketConnection.NormalClosure)
                }
                seen   <- conn.incoming.run.map(_.toList)
                closed <- Abort.run[ApolloWebSocketClosedException](conn.closed)
            yield
                assert(seen == List("next", "complete"))
                assert(closed.isSuccess, s"expected a clean close, got $closed")
            end for
        }

        "send forwards text; a send after termination is dropped" in {
            val fake = new FakeSocket
            for
                conn <- open(fake)
                _    <- conn.send("hello")
                _    <- Sync.defer(fake.closeEvent(WebSocketConnection.NormalClosure))
                _    <- conn.send("after-close")
            yield assert(fake.sent == List("hello"))
            end for
        }

        "close forwards the code and reason to the socket" in {
            val fake = new FakeSocket
            for
                conn <- open(fake)
                _    <- conn.close(4000, "bye")
            yield assert(fake.closedWith == Some((4000, "bye")))
            end for
        }

        "a close before open fails the opened handshake" in {
            val fake = new FakeSocket
            for
                fiber  <- Fiber.init(JsWebSocketConnection.openWith(fake.asJs))
                _      <- Async.sleep(10.millis)
                _      <- Sync.defer(fake.closeEvent(4401, "unauthorized"))
                result <- fiber.getResult
            yield result match
                case Result.Failure(e: ApolloWebSocketClosedException) => assert(e.code == 4401)
                case other => assert(false, s"expected ApolloWebSocketClosedException, got $other")
            end for
        }

        "a socket that never fires open fails after the connect timeout, and its scope closes the socket" in Clock.withTimeControl {
            control =>
                val fake = new FakeSocket
                for
                    fiber  <- Fiber.init(Scope.run(Abort.run[ApolloException](JsWebSocketConnection.openWith(fake.asJs, 5.seconds))))
                    _      <- control.awaitPendingSleepers(1) // the connect timeout is armed
                    _      <- control.advance(5.seconds, Duration.Zero)
                    result <- fiber.get
                yield
                    result match
                        case Result.Failure(e: ApolloNetworkException) => assert(e.message.contains("did not open within"))
                        case other                                     => assert(false, s"expected ApolloNetworkException, got $other")
                    assert(fake.closedWith.isDefined, "the socket must be closed when its open times out")
                end for
        }
    }
end JsWebSocketConnectionSpec
