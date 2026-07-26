package kyo.apollo.testing

import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WebSocketEngine
import kyo.apollo.network.ws.WsScheduler
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.Promise

/** A scripted, in-memory WebSocket server for subscription tests, promoted from
  * `core`'s `FakeWebSocket.scala` (the connection/engine/scheduler triple) and
  * `WsTestSupport.scala` (the dual-protocol server-frame builders) — ADR §2a/§5.
  *
  * It plays the role of the server below the [[WebSocketConnection]] seam: a test
  * injects it as the client's `webSocketEngine` (and its [[scheduler]] as the
  * `webSocketScheduler`), then scripts the handshake and events by hand
  * ([[ack]] / [[next]] / [[complete]] / …) with no real socket and no wall clock.
  * Client → server frames are recorded in [[sent]] for assertion; frames pushed
  * before the transport attaches its sink are buffered and replayed in order.
  *
  * The frame builders come in two protocol families selected by [[protocol]] —
  * [[MockWebSocketServer.Modern]] (`graphql-transport-ws`: `connection_ack` /
  * `next` / `error` / `complete` / `ping`) and [[MockWebSocketServer.Legacy]]
  * (`subscriptions-transport-ws`: `connection_ack` / `data` / `error` /
  * `complete` / `ka`) — so one scenario can drive either wire protocol. For raw
  * control, [[push]] any frame text directly; for reconnect scenarios use
  * [[FreshWebSocketEngine]] (a brand-new socket per `open`).
  */
final class MockWebSocketServer(
    val protocol: MockWebSocketServer.Protocol = MockWebSocketServer.Modern
) extends WebSocketEngine:

    /** The single scripted connection this server hands back on every `open`. */
    val connection: FakeWebSocketConnection = new FakeWebSocketConnection

    /** The manual timer seam — inject as the client's `webSocketScheduler` and
      * fire the ack/idle/backoff timers by hand with [[ManualWsScheduler.fireAll]].
      */
    val scheduler: ManualWsScheduler = new ManualWsScheduler

    /** Each `open` call's `(url, protocol)`, in order. */
    var opens: List[(String, Option[String])] = List.empty

    def open(url: String, protocol: Option[String] = None): Future[WebSocketConnection] =
        opens = opens :+ (url, protocol)
        Future.successful(connection)

    /** The client → server frames the transport has sent, oldest first (handshake
      * `connection_init`, per-subscription `subscribe`/`start`, `complete`/`stop`).
      */
    def sent: List[String] = connection.sent

    /** Push a raw server frame to the transport (buffered until its sink attaches). */
    def push(frame: String): Unit = connection.server(frame)

    // --- protocol-aware server frames -----------------------------------------

    private def frames: WsFrames = protocol match
        case MockWebSocketServer.Modern => WsFrames.modern
        case MockWebSocketServer.Legacy => WsFrames.legacy

    /** Acknowledge the connection handshake (`connection_ack`). */
    def ack(): Unit = push(frames.ack)

    /** Emit a `{ value: n }` event for subscription `id` (`next`/`data`). */
    def next(id: String, value: Int): Unit = push(frames.next(id, value))

    /** Emit a protocol error for subscription `id`. */
    def error(id: String, message: String): Unit = push(frames.error(id, message))

    /** Complete subscription `id`. */
    def complete(id: String): Unit = push(frames.complete(id))

    /** Emit the protocol keep-alive (`ping` modern / `ka` legacy). */
    def keepAlive(): Unit = push(frames.keepAlive)

    /** Simulate an abnormal server-side drop (fails the liveness stream), so a
      * reconnect path can be exercised.
      */
    def drop(code: Int, reason: String): Unit = connection.drop(code, reason)
end MockWebSocketServer

object MockWebSocketServer:

    /** Which subscription wire protocol the server's frame builders emit. */
    sealed trait Protocol derives CanEqual

    /** `graphql-transport-ws` — `connection_ack` / `next` / `error` / `complete` /
      * `ping`.
      */
    case object Modern extends Protocol

    /** `subscriptions-transport-ws` — `connection_ack` / `data` / `error` /
      * `complete` / `ka`.
      */
    case object Legacy extends Protocol
end MockWebSocketServer

/** The dual-protocol server-frame builders promoted from `WsTestSupport`. A
  * [[WsFrames]] instance produces the `{ value: Int }` event shape for one
  * protocol; [[WsFrames.modern]] / [[WsFrames.legacy]] are the two families.
  */
trait WsFrames:
    def ack: String
    def next(id: String, value: Int): String
    def error(id: String, message: String): String
    def complete(id: String): String
    def keepAlive: String
end WsFrames

object WsFrames:

    /** `graphql-transport-ws` server frames. */
    val modern: WsFrames = new WsFrames:
        val ack: String = """{"type":"connection_ack"}"""
        def next(id: String, value: Int): String =
            s"""{"id":"$id","type":"next","payload":{"data":{"value":$value}}}"""
        def error(id: String, message: String): String =
            s"""{"id":"$id","type":"error","payload":[{"message":"$message"}]}"""
        def complete(id: String): String = s"""{"id":"$id","type":"complete"}"""
        val keepAlive: String            = """{"type":"ping"}"""

    /** `subscriptions-transport-ws` server frames. */
    val legacy: WsFrames = new WsFrames:
        val ack: String = """{"type":"connection_ack"}"""
        def next(id: String, value: Int): String =
            s"""{"id":"$id","type":"data","payload":{"data":{"value":$value}}}"""
        def error(id: String, message: String): String =
            s"""{"id":"$id","type":"error","payload":{"message":"$message"}}"""
        def complete(id: String): String = s"""{"id":"$id","type":"complete"}"""
        val keepAlive: String            = """{"type":"ka"}"""
end WsFrames

/** A [[WebSocketConnection]] the test scripts directly: it records sends and the
  * close call, buffers server frames until the single sink attaches, then pushes
  * them in order. Promoted verbatim from `core`'s `FakeWebSocket.scala`.
  */
final class FakeWebSocketConnection extends WebSocketConnection:
    var sent: List[String]                   = List.empty
    var closedWith: Option[(Int, String)]    = None
    private var sink: Option[String => Unit] = None
    private val buffer                       = mutable.Queue.empty[String]
    private val done                         = Promise[Unit]()

    def send(text: String): Unit = sent = sent :+ text

    def incoming(onText: String => Unit): Unit =
        while buffer.nonEmpty do onText(buffer.dequeue())
        sink = Some(onText)

    def closed: Future[Unit] = done.future

    def close(code: Int = WebSocketConnection.NormalClosure, reason: String = ""): Unit =
        if closedWith.isEmpty then
            closedWith = Some((code, reason))
            if code == WebSocketConnection.NormalClosure then done.trySuccess(()): Unit
            else
                done.tryFailure(
                    ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty))
                ): Unit
            end if

    /** Push a server frame to the sink (buffered until one attaches). */
    def server(text: String): Unit = sink match
        case Some(emit) => emit(text)
        case None       => buffer.enqueue(text)

    /** Simulate an abnormal server-side drop (fails the liveness stream). */
    def drop(code: Int, reason: String): Unit =
        done.tryFailure(
            ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty))
        ): Unit
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol.
  * Promoted verbatim from `core`'s `FakeWebSocket.scala` — the low-level form
  * beneath [[MockWebSocketServer]] for specs that want the raw connection.
  */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection) extends WebSocketEngine:
    var opens: List[(String, Option[String])] = List.empty
    def open(url: String, protocol: Option[String] = None): Future[WebSocketConnection] =
        opens = opens :+ (url, protocol)
        Future.successful(conn)
end FakeWebSocketEngine

/** An engine that hands back a **fresh** [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket instead of the already-
  * terminated first one. Every opened connection is retained in [[conns]] so a
  * test can drive whichever socket generation it is interested in. Promoted
  * verbatim from `core`'s `FakeWebSocket.scala`.
  */
final class FreshWebSocketEngine extends WebSocketEngine:
    var conns: Vector[FakeWebSocketConnection] = Vector.empty
    def open(url: String, protocol: Option[String] = None): Future[WebSocketConnection] =
        val conn = new FakeWebSocketConnection
        conns = conns :+ conn
        Future.successful(conn)
    end open
end FreshWebSocketEngine

/** A scheduler that records timers instead of arming real ones; the test fires
  * them by hand. Cancelling drops the pending timer. Promoted verbatim from
  * `core`'s `FakeWebSocket.scala`.
  */
final class ManualWsScheduler extends WsScheduler:
    private val tasks = mutable.Map.empty[Long, () => Unit]
    private var seq   = 0L
    def schedule(delayMillis: Long)(task: () => Unit): () => Unit =
        val id = seq
        seq += 1
        tasks(id) = task
        () =>
            tasks.remove(id): Unit
    end schedule
    def pending: Int = tasks.size
    def fireAll(): Unit =
        val snapshot = tasks.values.toList
        tasks.clear()
        snapshot.foreach(_())
    end fireAll
end ManualWsScheduler
