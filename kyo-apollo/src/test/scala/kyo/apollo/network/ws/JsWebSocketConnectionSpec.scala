package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloWebSocketClosedException
// Aliased to `sjs`: `kyo.test.Test` inherits a `js` platform-filter extension
// method (`.js`/`.notJs` test selectors), and an inherited member outranks an
// explicit import, so a bare `js.Dynamic` would bind to that extension instead
// of the `scala.scalajs.js` package. The alias sidesteps the clash.
import scala.scalajs.js as sjs

/** Drives [[JsWebSocketConnection]]'s event state machine with a scripted
  * in-memory fake WebSocket — no real socket, no server.
  *
  * The production [[JsWebSocketEngine]] only resolves and news-up a platform
  * `WebSocket`; all the behaviour worth testing (the handshake signal, buffering
  * frames until a sink attaches, ordered delivery, and the clean-vs-abnormal
  * termination that becomes the transport's liveness signal) lives in the
  * connection, so it is exercised here directly by feeding it the same
  * `addEventListener` events a real socket would. This mirrors the project
  * convention of testing the seam, not the JS-global engine (there is no
  * `FetchHttpEngineSpec` either).
  */
class JsWebSocketConnectionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A fake platform socket: records listeners, `send`s, and the close call, and
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

        def open(): Unit = fire("open", sjs.Dynamic.literal())
        def message(text: String): Unit =
            fire("message", sjs.Dynamic.literal(data = text))
        def error(): Unit = fire("error", sjs.Dynamic.literal())
        def closeEvent(code: Int, reason: String = ""): Unit =
            fire("close", sjs.Dynamic.literal(code = code, reason = reason))
    end FakeSocket

    private def connect(): (FakeSocket, JsWebSocketConnection) =
        val fake = new FakeSocket
        (fake, new JsWebSocketConnection(fake.asJs))

    "JsWebSocketConnection" - {

        "opened completes when the socket fires open" in {
            val (fake, conn) = connect()
            fake.open()
            Async.fromFuture(conn.opened).map(_ => assert(true))
        }

        "buffers frames received before a sink attaches, then delivers live" in {
            val (fake, conn) = connect()
            fake.open()
            fake.message("a")
            fake.message("b")
            var seen = List.empty[String]
            conn.incoming(m => seen = seen :+ m)
            fake.message("c")
            fake.closeEvent(WebSocketConnection.NormalClosure)
            Async.fromFuture(conn.closed).map(_ => assert(seen == List("a", "b", "c")))
        }

        "a normal (1000) close completes the liveness signal" in {
            val (fake, conn) = connect()
            fake.open()
            conn.incoming(_ => ())
            fake.closeEvent(WebSocketConnection.NormalClosure)
            Async.fromFuture(conn.closed).map(_ => assert(true))
        }

        "an abnormal close fails the liveness signal with ApolloWebSocketClosedException" in {
            val (fake, conn) = connect()
            fake.open()
            conn.incoming(_ => ())
            fake.closeEvent(1011, "server error")
            Abort.run[Throwable](Async.fromFuture(conn.closed)).map {
                case Result.Panic(e: ApolloWebSocketClosedException) =>
                    assert(e.code == 1011)
                    assert(e.reason == Some("server error"))
                case other => fail(s"expected ApolloWebSocketClosedException, got $other")
            }
        }

        "a close before open fails the opened handshake" in {
            val (fake, conn) = connect()
            fake.closeEvent(4401, "unauthorized")
            Abort.run[Throwable](Async.fromFuture(conn.opened)).map {
                case Result.Panic(e: ApolloWebSocketClosedException) => assert(e.code == 4401)
                case other                                           => fail(s"expected ApolloWebSocketClosedException, got $other")
            }
        }

        "send forwards text; a send after termination is dropped" in {
            val (fake, conn) = connect()
            fake.open()
            conn.send("hello")
            fake.closeEvent(WebSocketConnection.NormalClosure)
            conn.send("after-close")
            assert(fake.sent == List("hello"))
        }

        "close forwards the code and reason to the socket" in {
            val (fake, conn) = connect()
            fake.open()
            conn.close(4000, "bye")
            assert(fake.closedWith == Some((4000, "bye")))
        }

        "an error before open fails the handshake" in {
            val (fake, conn) = connect()
            fake.error()
            fake.closeEvent(1006)
            Abort.run[Throwable](Async.fromFuture(conn.opened)).map {
                case Result.Panic(_: ApolloWebSocketClosedException) => assert(true)
                case other                                           => fail(s"expected ApolloWebSocketClosedException, got $other")
            }
        }
    }
end JsWebSocketConnectionSpec
