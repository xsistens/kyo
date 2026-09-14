package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloWebSocketClosedException
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.Try

/** The production [[WebSocketEngine]] on JS/Wasm: opens a real platform WebSocket
  * and bridges its browser-style callback events into the native-kyo seam.
  *
  * Resolves the constructor at first use, as apollo-kotlin's engine does: the
  * global `WebSocket` (browsers and Node 21+; this project runs Node 26),
  * otherwise the [`ws`](https://www.npmjs.com/package/ws) npm package `require`d
  * lazily. `ws` is therefore an optional dependency, only touched when the global
  * is undefined, so it is intentionally not declared in `build.sbt`.
  *
  * @param connectTimeout how long a socket may take to fire `open`; a platform
  *                       socket has no bound of its own for a server that accepts
  *                       the connection but never completes the upgrade
  */
final class JsWebSocketEngine(connectTimeout: Duration = JsWebSocketEngine.defaultConnectTimeout) extends WebSocketEngine:

    def open(
        url: String,
        protocol: Option[String] = None
    )(using Frame): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        JsWebSocketConnection.open(url, protocol, connectTimeout)
end JsWebSocketEngine

object JsWebSocketEngine:

    /** The default bound on opening a socket. */
    val defaultConnectTimeout: Duration = WebSocketEngine.defaultConnectTimeout

    /** Node's CommonJS `require`, faceted so the `ws` fallback can be pulled in
      * lazily. Only ever called when the global `WebSocket` is missing.
      */
    @js.native
    @JSGlobal("require")
    private[ws] def require(module: String): js.Dynamic = js.native

    /** The resolved WebSocket constructor — global first, `ws` package fallback. */
    private[ws] lazy val constructor: js.Dynamic =
        if js.typeOf(js.Dynamic.global.WebSocket) != "undefined" then js.Dynamic.global.WebSocket
        else require("ws")
end JsWebSocketEngine

/** Minimal facade over the browser/`ws` WebSocket surface this engine uses. */
@js.native
private[ws] trait JsWebSocket extends js.Object:
    def send(data: String): Unit               = js.native
    def close(code: Int, reason: String): Unit = js.native
    def addEventListener(
        `type`: String,
        listener: js.Function1[js.Dynamic, Unit]
    ): Unit = js.native
end JsWebSocket

/** A [[WebSocketConnection]] backed by a platform `socket`, wiring its
  * open/message/error/close events into the [[incoming]] channel and the [[closed]]
  * liveness promise. The impure event callbacks push into kyo primitives via their
  * `unsafe` handles; the effectful seam methods read those primitives.
  */
final private[ws] class JsWebSocketConnection private (
    socket: JsWebSocket,
    incomingCh: Channel[Maybe[String]],
    donePromise: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]]
) extends WebSocketConnection:

    @volatile private var terminated = false

    private[ws] def markTerminated(): Unit = terminated = true

    def send(text: String)(using Frame): Unit < Async =
        Sync.defer(if !terminated then socket.send(text) else ())

    def incoming(using Frame): Stream[String, Async] =
        WebSocketConnection.untilEnd(incomingCh)

    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException]) =
        donePromise.get

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async =
        // Closing an already-closing/closed socket can throw in some engines; the
        // contract says close is swallow-safe, so guard it.
        Sync.defer(discard(Try(socket.close(code, reason))))
end JsWebSocketConnection

private[ws] object JsWebSocketConnection:

    /** Open a socket and complete once it fires `open`; abort if it errors or
      * closes first, or does not open within `connectTimeout`. The connection's
      * `Scope` closes the socket on teardown.
      */
    def open(
        url: String,
        protocol: Option[String],
        connectTimeout: Duration
    )(using Frame): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer {
            protocol match
                case Some(p) => js.Dynamic.newInstance(JsWebSocketEngine.constructor)(url, p)
                case None    => js.Dynamic.newInstance(JsWebSocketEngine.constructor)(url)
        }.map(socket => openWith(socket.asInstanceOf[JsWebSocket], connectTimeout))

    /** Wire a connection over an already-constructed socket and await its `open`
      * event, for at most `connectTimeout` (then the open fails with an
      * [[ApolloNetworkException]] and the `Scope` closes the socket). Split out from
      * [[open]] so the connection state machine can be driven by a scripted fake
      * socket in tests without a real platform `WebSocket`.
      */
    private[ws] def openWith(
        socket: JsWebSocket,
        connectTimeout: Duration = JsWebSocketEngine.defaultConnectTimeout
    )(using Frame): JsWebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        for
            // Unbounded and never closed: frames are `Present`, the socket's end is one
            // `Absent` marker (see `WebSocketConnection.untilEnd`), so the channel simply
            // becomes unreachable with the connection.
            incomingCh <- Channel.initUnscoped[Maybe[String]](Int.MaxValue)
            done       <- Fiber.Promise.init[Unit, Abort[ApolloWebSocketClosedException]]
            opened     <- Fiber.Promise.init[Unit, Abort[ApolloWebSocketClosedException]]
            connection <- Sync.defer {
                given AllowUnsafe = AllowUnsafe.embrace.danger
                val conn          = new JsWebSocketConnection(socket, incomingCh, done)
                wire(socket, conn, incomingCh, done, opened)
                conn
            }
            _ <- Scope.ensure(connection.close())
            _ <- Abort.run[Timeout](Async.timeout(connectTimeout)(opened.get)).map {
                case Result.Success(_) => Kyo.unit
                case Result.Failure(_) => Abort.fail(ApolloNetworkException(s"The WebSocket did not open within ${connectTimeout.show}"))
                case Result.Panic(e)   => Abort.panic(e)
            }
        yield connection

    /** Attach the socket's event listeners, translating each event into an
      * `unsafe` push/complete on the connection's kyo primitives.
      */
    private def wire(
        socket: JsWebSocket,
        conn: JsWebSocketConnection,
        incomingCh: Channel[Maybe[String]],
        done: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]],
        opened: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]]
    )(using AllowUnsafe, Frame): Unit =
        def on(event: String)(handler: js.Dynamic => Unit): Unit =
            val listener: js.Function1[js.Dynamic, Unit] = (e: js.Dynamic) => handler(e)
            socket.addEventListener(event, listener)

        on("open")(_ => discard(opened.unsafe.completeUnitDiscard()))
        on("message")(e => discard(incomingCh.unsafe.offer(Present(messageText(e)))))
        on("error")(_ =>
            val ex = ApolloWebSocketClosedException(
                WebSocketConnection.NormalClosure + 6, // 1006 — abnormal, no close frame
                Some("WebSocket connection error")
            )
            terminate(conn, incomingCh, done, opened, Present(ex))
        )
        on("close")(e =>
            val code      = closeCode(e)
            val reasonOpt = Option(closeReason(e)).filter(_.nonEmpty)
            val failure =
                if code == WebSocketConnection.NormalClosure then Absent
                else Present(ApolloWebSocketClosedException(code, reasonOpt))
            terminate(conn, incomingCh, done, opened, failure)
        )
    end wire

    private def terminate(
        conn: JsWebSocketConnection,
        incomingCh: Channel[Maybe[String]],
        done: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]],
        opened: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]],
        failure: Maybe[ApolloWebSocketClosedException]
    )(using AllowUnsafe, Frame): Unit =
        conn.markTerminated()
        // A close/error arriving before `open` fails the handshake future.
        failure match
            case Absent =>
                discard(opened.unsafe.completeUnitDiscard())
                discard(done.unsafe.completeUnitDiscard())
            case Present(ex) =>
                discard(opened.unsafe.completeDiscard(Result.fail(ex)))
                discard(done.unsafe.completeDiscard(Result.fail(ex)))
        end match
        // The end-marker, never `close`: `on("message")` and this `on("close")` fire in
        // the same synchronous burst, before any fiber drains, and a `close` would hand
        // the frames still buffered to this closer instead of to `incoming`. An `error`
        // followed by `close` leaves a second marker behind the first; harmless.
        discard(incomingCh.unsafe.offer(Absent))
    end terminate

    private def messageText(event: js.Dynamic): String =
        val data = event.data
        if js.typeOf(data) == "string" then data.asInstanceOf[String]
        else data.applyDynamic("toString")().asInstanceOf[String]
    end messageText

    private def closeCode(event: js.Dynamic): Int =
        val code = event.code
        if js.typeOf(code) == "number" then code.asInstanceOf[Int]
        else WebSocketConnection.NormalClosure + 6 // 1006 when no code is supplied
    end closeCode

    private def closeReason(event: js.Dynamic): String =
        val reason = event.reason
        if js.typeOf(reason) == "string" then reason.asInstanceOf[String] else ""
end JsWebSocketConnection
