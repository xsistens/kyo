package kyo.apollo.network.ws

import scala.concurrent.Future

/** A single live WebSocket, seen as a text channel.
  *
  * This is the raw-socket seam beneath the multiplexing
  * [[kyo.apollo.network.ws.WebSocketNetworkTransport]] (Task 4): it knows nothing of
  * GraphQL, operation ids, or the [[WsProtocol]] framing — it only carries
  * `String` frames both ways and reports when the socket closes. Everything
  * above it (handshake, multiplexing, routing, reconnection) is built on this
  * three-method contract, exactly as the HTTP layer is built on
  * [[kyo.apollo.network.http.HttpEngine]]. Splitting the platform socket out this way
  * lets [[JsWebSocketEngine]] open a real browser/Node socket in production while
  * the transport tests (Task 7) drive a scripted in-memory fake — the same
  * fake-engine pattern the Phase 03 HTTP tests use.
  *
  * The incoming side is a callback sink plus a liveness `Future` (the effect
  * pivot, Schritt 2.2, keeps this raw socket a plain single-threaded callback
  * machine behind the transport's Kyo `Stream` surface). [[incoming]] attaches
  * the transport's single sink; [[closed]] is the socket's liveness signal —
  *
  *   - it **completes** when the socket closes cleanly (a normal `1000` close),
  *     which the transport reads as an intentional shutdown, and
  *   - it **fails** with an [[kyo.apollo.exception.ApolloWebSocketClosedException]]
  *     (carrying the close `code`/`reason`) on any abnormal close or socket
  *     error, which the transport reads as a drop worth reconnecting (Task 5).
  *
  * Frames that arrive before the sink is attached are buffered and replayed in
  * order on attach, so no server message is lost in the gap between the socket
  * opening and the transport subscribing.
  */
trait WebSocketConnection:

    /** Send one text frame. A no-op once the socket has terminated (a late `stop`
      * racing a close must not throw), so callers need not guard every send.
      */
    def send(text: String): Unit

    /** Attach the single incoming-frame sink. Frames buffered before attach replay
      * in order. Single-consumer — the transport attaches exactly one.
      */
    def incoming(onText: String => Unit): Unit

    /** The socket's liveness signal: completes on a clean `1000` close, fails with
      * an [[kyo.apollo.exception.ApolloWebSocketClosedException]] on any abnormal close
      * or socket error. The transport reads this as its close/drop trigger.
      */
    def closed: Future[Unit]

    /** Close the socket with a WebSocket close `code` and optional `reason`.
      * Idempotent from the caller's view: closing an already-closing socket is
      * swallowed rather than thrown.
      */
    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    ): Unit
end WebSocketConnection

object WebSocketConnection:

    /** The WebSocket "normal closure" status code (RFC 6455 §7.4.1). A close with
      * this code is an intentional shutdown, so [[incoming]] *completes*; any other
      * code is treated as an abnormal drop and *fails* the stream.
      */
    val NormalClosure: Int = 1000
end WebSocketConnection

/** Opens [[WebSocketConnection]]s — the injectable factory the transport depends
  * on, mirroring [[kyo.apollo.network.http.HttpEngine]].
  *
  * Kept separate from the connection so production ([[JsWebSocketEngine]], which
  * constructs a real platform socket) and tests (a fake that hands back a
  * scripted connection) are swapped at this one seam. The transport never
  * constructs a socket directly; it asks an engine to [[open]] one.
  */
trait WebSocketEngine:

    /** Open a socket to `url`, negotiating the optional sub`protocol` token (the
      * [[WsProtocol.name]], e.g. `"graphql-transport-ws"`). The returned `Future`
      * resolves with the connection **once the socket is open** — so the transport
      * can immediately send `connection_init` — and fails if the socket errors or
      * closes before it ever opens.
      */
    def open(
        url: String,
        protocol: Option[String] = None
    ): Future[WebSocketConnection]
end WebSocketEngine
