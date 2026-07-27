package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloWebSocketClosedException

/** A single live WebSocket, seen as a text channel — the native-kyo seam beneath
  * the multiplexing [[kyo.apollo.network.ws.WebSocketNetworkTransport]].
  *
  * It knows nothing of GraphQL, operation ids, or the [[WsProtocol]] framing — it
  * only carries `String` frames both ways and reports when the socket closes.
  * Everything above it (handshake, multiplexing, routing, reconnection) is built
  * on this four-method contract, exactly as the HTTP layer is built on
  * [[kyo.apollo.network.http.HttpEngine]]. Splitting the platform socket out this
  * way lets a platform [[WebSocketEngine]] open a real browser/Node socket
  * ([[JsWebSocketEngine]]) or a kyo-http socket (JVM/Native) in production while
  * the transport tests drive a scripted in-memory fake.
  *
  * All four methods are kyo-effectful so the transport can drive the socket with
  * structured concurrency: [[incoming]] is a live `Stream` of text frames that
  * ends when the socket closes; [[closed]] is the liveness signal —
  *
  *   - it **succeeds** when the socket closes cleanly (a normal `1000` close),
  *     which the transport reads as an intentional shutdown, and
  *   - it **aborts** with an [[ApolloWebSocketClosedException]] (carrying the
  *     close `code`/`reason`) on any abnormal close or socket error, which the
  *     transport reads as a drop worth reconnecting.
  */
trait WebSocketConnection:

    /** Send one text frame. A no-op once the socket has terminated (a late stop
      * racing a close must not fail), so callers need not guard every send.
      */
    def send(text: String)(using Frame): Unit < Async

    /** The live stream of incoming text frames, ending (without error) when the
      * socket closes. Single-consumer — the transport forks exactly one drain.
      */
    def incoming(using Frame): Stream[String, Async]

    /** The socket's liveness signal: succeeds on a clean `1000` close, aborts with
      * an [[ApolloWebSocketClosedException]] on any abnormal close or socket error.
      * The transport reads this as its close/drop trigger.
      */
    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException])

    /** Close the socket with a WebSocket close `code` and optional `reason`.
      * Idempotent from the caller's view: closing an already-closing socket is
      * swallowed rather than failing.
      */
    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async
end WebSocketConnection

object WebSocketConnection:

    /** The WebSocket "normal closure" status code (RFC 6455 §7.4.1). A close with
      * this code is an intentional shutdown, so [[closed]] *succeeds*; any other
      * code is treated as an abnormal drop and *aborts*.
      */
    val NormalClosure: Int = 1000
end WebSocketConnection

/** Opens [[WebSocketConnection]]s — the injectable factory the transport depends
  * on, mirroring [[kyo.apollo.network.http.HttpEngine]].
  *
  * Kept separate from the connection so production and tests are swapped at this
  * one seam. The transport never constructs a socket directly; it asks an engine
  * to [[open]] one. The returned connection is `Scope`-managed: closing the scope
  * tears the socket down, so the transport opens each socket in a dedicated fiber
  * and interrupts it to discard the socket.
  */
trait WebSocketEngine:

    /** Open a socket to `url`, negotiating the optional sub`protocol` token (the
      * [[WsProtocol.name]], e.g. `"graphql-transport-ws"`). The effect succeeds
      * with the connection **once the socket is open** — so the transport can
      * immediately send `connection_init` — and aborts if the socket errors or
      * closes before it ever opens.
      */
    def open(
        url: String,
        protocol: Option[String] = None
    )(using Frame): WebSocketConnection < (Async & Scope & Abort[ApolloException])
end WebSocketEngine
