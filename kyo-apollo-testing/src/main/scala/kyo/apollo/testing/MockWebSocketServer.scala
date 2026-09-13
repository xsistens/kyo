package kyo.apollo.testing

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WebSocketEngine
import kyo.apollo.network.ws.WsScheduler
import scala.collection.mutable

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

    private val opensRef = new AtomicReference[Vector[(String, Option[String])]](Vector.empty)

    /** Each `open` call's `(url, protocol)`, in order. */
    def opens: List[(String, Option[String])] = opensRef.get().toList

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer { discard(opensRef.updateAndGet(_ :+ (url, protocol))); connection }

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

/** A [[WebSocketConnection]] the test scripts directly: it records effectful sends
  * and the close call, and pushes server frames into the incoming channel (buffered
  * until the transport drains it). Its `unsafe` primitives are created eagerly so
  * the plain `server` / `drop` driving methods can offer/complete synchronously.
  * Promoted from `core`'s `FakeWebSocket.scala` (native-kyo seam).
  */
final class FakeWebSocketConnection extends WebSocketConnection:
    private given AllowUnsafe = AllowUnsafe.embrace.danger
    private given Frame       = Frame.internal

    private val sentRef                                      = new AtomicReference[Vector[String]](Vector.empty)
    @volatile private var closedState: Option[(Int, String)] = None
    private val incomingCh: Channel[String] =
        Sync.Unsafe.evalOrThrow(Channel.initUnscoped[String](Int.MaxValue))
    private val donePromise: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]] =
        Sync.Unsafe.evalOrThrow(Fiber.Promise.init[Unit, Abort[ApolloWebSocketClosedException]])

    /** Every frame the transport has sent, in order. */
    def sent: List[String] = sentRef.get().toList

    /** The `(code, reason)` the transport closed with, if it has. */
    def closedWith: Option[(Int, String)] = closedState

    def send(text: String)(using Frame): Unit < Async =
        Sync.defer(discard(sentRef.updateAndGet(_ :+ text)))

    def incoming(using Frame): Stream[String, Async] = incomingCh.streamUntilClosed()

    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException]) = donePromise.get

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async =
        Sync.defer {
            given AllowUnsafe = AllowUnsafe.embrace.danger
            if closedState.isEmpty then
                closedState = Some((code, reason))
                if code == WebSocketConnection.NormalClosure then discard(donePromise.unsafe.completeUnitDiscard())
                else
                    discard(donePromise.unsafe.completeDiscard(
                        Result.fail(ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty)))
                    ))
                end if
                discard(incomingCh.unsafe.close())
            end if
        }

    /** Push a server frame to the incoming stream (buffered until it drains). */
    def server(text: String): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(incomingCh.unsafe.offer(text))

    /** Simulate an abnormal server-side drop (aborts the liveness signal). */
    def drop(code: Int, reason: String): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(donePromise.unsafe.completeDiscard(
            Result.fail(ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty)))
        ))
        discard(incomingCh.unsafe.close())
    end drop
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol.
  * Promoted from `core`'s `FakeWebSocket.scala` — the low-level form beneath
  * [[MockWebSocketServer]] for specs that want the raw connection.
  */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection) extends WebSocketEngine:
    private val opensRef                      = new AtomicReference[Vector[(String, Option[String])]](Vector.empty)
    def opens: List[(String, Option[String])] = opensRef.get().toList
    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer { discard(opensRef.updateAndGet(_ :+ (url, protocol))); conn }
end FakeWebSocketEngine

/** An engine that hands back a **fresh** [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket instead of the already-
  * terminated first one. Every opened connection is retained in [[conns]] so a
  * test can drive whichever socket generation it is interested in. Promoted from
  * `core`'s `FakeWebSocket.scala`.
  */
final class FreshWebSocketEngine extends WebSocketEngine:
    private val connsRef                       = new AtomicReference[Vector[FakeWebSocketConnection]](Vector.empty)
    def conns: Vector[FakeWebSocketConnection] = connsRef.get()
    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer {
            val c = new FakeWebSocketConnection
            discard(connsRef.updateAndGet(_ :+ c))
            c
        }
end FreshWebSocketEngine

/** A scheduler that records timers instead of arming real ones; the test fires
  * them by hand. Cancelling drops the pending timer. Retained for HTTP
  * interceptor specs that still inject a [[WsScheduler]]; the WebSocket transport
  * now drives its timers through `Clock` and is tested with `Clock.withTimeControl`.
  * Promoted from `core`'s `FakeWebSocket.scala`.
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
