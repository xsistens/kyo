package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloWebSocketClosedException

/** Drives the JVM/Native [[KyoHttpWebSocketEngine]] against an in-process kyo-http
  * server. The JS engine's counterpart is `JsWebSocketConnectionSpec` over a scripted
  * fake socket; here the real kyo-http socket is the only way to exercise the
  * engine's `ws.stream` → `incoming` bridge, in particular how the bridge ends.
  */
class KyoHttpWebSocketEngineSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // Starts a real server (an ephemeral listener fd the NIO transport defers closing,
    // as kyo-http's own suites note); run leaves sequentially and skip the socket leak check.
    override def config = super.config.sequential.leakCheckSockets(false)

    "KyoHttpWebSocketEngine" - {

        "a frame still buffered in the engine when the server closes is delivered, then the stream ends cleanly" in {
            // The end of a finite subscription: the server's last frames go out, then its
            // close frame. The engine's drain fiber pumps the frames into its channel and
            // settles `closed` when `ws.stream` ends; the consumer of `incoming` is a
            // separate fiber. Here it takes only the first frame, so the second is still
            // buffered in the engine's channel when the close lands — a hard `close` of that
            // channel would hand it to the closer. The server closes only after a receipt
            // round-trip: kyo-http's own WebSocket close paths (`outbound.closeDiscard` in
            // the server's `ws.close`, `inbound.closeDiscard` on the client's peer close)
            // drop frames that are still in flight when the close frame follows them
            // back-to-back, which is a kyo-http matter and not what this leaf tests.
            val handler = HttpHandler.webSocket("ws") { (_, ws) =>
                ws.put(HttpWebSocket.Payload.Text("next"))
                    .andThen(ws.put(HttpWebSocket.Payload.Text("complete")))
                    .andThen(ws.take())
                    .andThen(ws.close(WebSocketConnection.NormalClosure, "done"))
            }
            HttpServer.init(0, "127.0.0.1")(handler).map { server =>
                val engine = new KyoHttpWebSocketEngine
                for
                    conn  <- engine.open(s"ws://127.0.0.1:${server.port}/ws", None)
                    first <- conn.incoming.take(1).run.map(_.toList)
                    _     <- conn.send("received")
                    // Resume draining only once the socket has ended: by then the engine has
                    // settled `closed` and marked the channel's end behind the buffered frame.
                    closed <- Abort.run[ApolloWebSocketClosedException](conn.closed)
                    rest   <- conn.incoming.run.map(_.toList)
                yield
                    assert(closed.isSuccess, s"expected a clean close, got $closed")
                    assert(first == List("next"), s"got $first")
                    assert(rest == List("complete"), s"got $rest")
                end for
            }
        }
    }
end KyoHttpWebSocketEngineSpec
