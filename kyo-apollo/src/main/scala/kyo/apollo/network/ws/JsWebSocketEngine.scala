package kyo.apollo.network.ws

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.discard
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.Try

/** The production [[WebSocketEngine]]: opens a real platform WebSocket.
  *
  * Resolves the constructor at first use, in the order apollo-kotlin's engine
  * resolves its platform socket:
  *
  *   1. the global `WebSocket` — present in browsers and in Node 21+ (this
  *      project runs on Node 26, so the global is the normal path); otherwise
  *   2. the [`ws`](https://www.npmjs.com/package/ws) npm package, `require`d
  *      lazily as a Node fallback for older runtimes.
  *
  * `ws` is therefore an **optional** dependency: it is only touched when the
  * global `WebSocket` is undefined, so it is intentionally *not* declared in
  * `build.sbt`. Deployments on a runtime without a global `WebSocket` must
  * `npm install ws` themselves; everyone on a modern runtime needs nothing. Both
  * candidates expose the same browser-shaped surface this engine relies on —
  * `new WebSocket(url, protocol)`, `send`, `close(code, reason)`, and
  * `addEventListener("open"|"message"|"error"|"close", …)` — so one code path
  * drives either (`ws` implements the `addEventListener`/`MessageEvent`/
  * `CloseEvent` DOM shim in addition to its native `EventEmitter` API).
  */
final class JsWebSocketEngine extends WebSocketEngine:

    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    def open(
        url: String,
        protocol: Option[String] = None
    ): Future[WebSocketConnection] =
        val socket = protocol match
            case Some(p) => js.Dynamic.newInstance(JsWebSocketEngine.constructor)(url, p)
            case None    => js.Dynamic.newInstance(JsWebSocketEngine.constructor)(url)
        val connection = new JsWebSocketConnection(socket.asInstanceOf[JsWebSocket])
        connection.opened.map(_ => connection)
    end open
end JsWebSocketEngine

object JsWebSocketEngine:

    /** Node's CommonJS `require`, faceted so the `ws` fallback can be pulled in
      * lazily. Only ever called when the global `WebSocket` is missing, i.e. under
      * an older Node runtime where `require` is guaranteed present.
      */
    @js.native
    @JSGlobal("require")
    private def require(module: String): js.Dynamic = js.native

    /** The resolved WebSocket constructor — global first, `ws` package fallback.
      * `lazy` so the lookup (and any `require("ws")`) happens on first open, not at
      * class-load, keeping construction side-effect-free until a socket is opened.
      */
    private lazy val constructor: js.Dynamic =
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
  * open/message/error/close events into the [[incoming]] `Flow` and the
  * [[opened]] handshake signal.
  *
  * State is a tiny single-threaded machine (JS has no real concurrency): frames
  * arriving before a sink attaches are buffered and flushed in order on attach;
  * the first terminal event (clean close, abnormal close, or error) wins and is
  * latched, so later events are ignored and the completion/failure is delivered
  * exactly once. Package-private so the Task 3 spec can drive it with a scripted
  * fake socket — the same seam the transport tests reuse.
  */
final private[ws] class JsWebSocketConnection(socket: JsWebSocket) extends WebSocketConnection:

    private val openedPromise               = Promise[Unit]()
    private val donePromise                 = Promise[Unit]()
    private val buffered                    = mutable.Queue.empty[String]
    private var sink: Maybe[String => Unit] = Absent
    private var errored                     = false
    private var terminated                  = false

    on("open")(_ => discard(openedPromise.trySuccess(())))
    on("message")(e => push(messageText(e)))
    on("error")(_ => onError())
    on("close")(e => onClose(closeCode(e), closeReason(e)))

    /** Completes when the socket fires `open`; fails if it errors or closes first.
      * The engine chains this into the `Future[WebSocketConnection]` it returns.
      */
    def opened: Future[Unit] = openedPromise.future

    def send(text: String): Unit = if !terminated then socket.send(text)

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    ): Unit =
        // Closing an already-closing/closed socket can throw in some engines; the
        // contract says close is swallow-safe, so guard it.
        discard(Try(socket.close(code, reason)))

    def incoming(onText: String => Unit): Unit = attach(onText)

    def closed: Future[Unit] = donePromise.future

    // ---- internals -----------------------------------------------------------

    private def attach(emit: String => Unit): Unit =
        while buffered.nonEmpty do emit(buffered.dequeue())
        sink = Present(emit)

    private def push(message: String): Unit = sink match
        case Present(emit) => emit(message)
        case Absent        => buffered.enqueue(message)

    private def onError(): Unit =
        errored = true
        // A socket that errors before opening never yields a connection; the
        // following `close` (browsers and `ws` both emit one) terminates `incoming`.
        discard(
            openedPromise.tryFailure(
                ApolloWebSocketClosedException(
                    WebSocketConnection.NormalClosure + 6, // 1006 — abnormal, no close frame
                    Some("WebSocket connection error")
                )
            )
        )
    end onError

    private def onClose(code: Int, reason: String): Unit =
        val reasonOpt = Option(reason).filter(_.nonEmpty)
        // A close arriving before `open` fails the handshake future.
        discard(openedPromise.tryFailure(ApolloWebSocketClosedException(code, reasonOpt)))
        if code == WebSocketConnection.NormalClosure && !errored then terminate(p => discard(p.trySuccess(())))
        else terminate(p => discard(p.tryFailure(ApolloWebSocketClosedException(code, reasonOpt))))
    end onClose

    private def terminate(complete: Promise[Unit] => Unit): Unit =
        if !terminated then
            terminated = true
            complete(donePromise)

    private def on(event: String)(handler: js.Dynamic => Unit): Unit =
        val listener: js.Function1[js.Dynamic, Unit] = (e: js.Dynamic) => handler(e)
        socket.addEventListener(event, listener)

    /** A text frame's payload. Text frames arrive as a JS string on both the
      * browser and `ws`; anything else (a binary `Buffer` under `ws`) is coerced
      * to its text form so the protocol layer always sees a `String`.
      */
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
