package kyo.apollo.testing

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WebSocketEngine

/** A scripted, in-memory WebSocket server for subscription tests, promoted from
  * `core`'s `FakeWebSocket.scala` (the connection/engine pair) and
  * `WsTestSupport.scala` (the dual-protocol server-frame builders) — ADR §2a/§5.
  *
  * It plays the role of the server below the [[WebSocketConnection]] seam: a test
  * injects it as the client's `webSocketEngine`, then scripts the handshake and
  * events by hand ([[ack]] / [[next]] / [[complete]] / …) with no real socket; the
  * transport's ack, idle and reconnect timers run on the `Clock`, so a test inside
  * `Clock.withTimeControl` fires them with `advance`.
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

    private val opensRef =
        AtomicRef.Unsafe.init(Chunk.empty[(String, Option[String])])(using AllowUnsafe.embrace.danger).safe

    /** Each `open` call's `(url, protocol)`, in order. */
    def opens: List[(String, Option[String])] = opensRef.unsafe.get()(using AllowUnsafe.embrace.danger).toList

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        opensRef.getAndUpdate(_.append((url, protocol))).andThen(connection)

    /** The client → server frames the transport has sent, oldest first (handshake
      * `connection_init`, per-subscription `subscribe`/`start`, `complete`/`stop`).
      */
    def sent: List[String] = connection.sent

    /** Wait for the next client → server frame that satisfies `p` (skipping the
      * others) — the barrier a test waits on before scripting the server's reply.
      */
    def awaitSent(p: String => Boolean)(using Frame): String < Async = connection.awaitSent(p)

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
  * the plain `server` / `serverClose` / `drop` driving methods can offer/complete
  * synchronously. The incoming channel ends with the same `Absent` marker the
  * platform engines put (see [[WebSocketConnection.untilEnd]]), so frames scripted
  * right before a close are delivered exactly as a real socket would deliver them.
  * Promoted from `core`'s `FakeWebSocket.scala` (native-kyo seam).
  */
final class FakeWebSocketConnection extends WebSocketConnection:
    private given Frame = Frame.internal
    private val unsafe  = AllowUnsafe.embrace.danger

    private val sentRef     = AtomicRef.Unsafe.init(Chunk.empty[String])(using unsafe).safe
    private val sentCh      = Channel.Unsafe.init[String](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val closedRef   = AtomicRef.Unsafe.init(Maybe.empty[(Int, String)])(using unsafe).safe
    private val incomingCh  = Channel.Unsafe.init[Maybe[String]](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val donePromise = Fiber.Promise.Unsafe.init[Unit, Abort[ApolloWebSocketClosedException]]()(using unsafe).safe

    /** Every frame the transport has sent, in order. */
    def sent: List[String] = sentRef.unsafe.get()(using unsafe).toList

    /** The next frame the transport sends, in send order (each frame is handed out
      * once) — a barrier a test waits on instead of a pause.
      */
    def nextSent(using Frame): String < Async =
        Abort.run[Closed](sentCh.take).map(_.getOrThrow)

    /** Skip sent frames until one satisfies `p`, and return it. */
    def awaitSent(p: String => Boolean)(using Frame): String < Async =
        Loop.foreach(nextSent.map(frame => if p(frame) then Loop.done(frame) else Loop.continue))

    /** The `(code, reason)` the transport closed with, if it has. */
    def closedWith: Option[(Int, String)] = closedRef.unsafe.get()(using unsafe).toOption

    def send(text: String)(using Frame): Unit < Async =
        sentRef.getAndUpdate(_.append(text)).andThen(Abort.run[Closed](sentCh.offer(text)).unit)

    def incoming(using Frame): Stream[String, Async] = WebSocketConnection.untilEnd(incomingCh)

    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException]) = donePromise.get

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async =
        closedRef.compareAndSet(Absent, Present((code, reason))).map { first =>
            if !first then Kyo.unit
            else
                val settle =
                    if code == WebSocketConnection.NormalClosure then donePromise.completeUnitDiscard
                    else donePromise.completeDiscard(Result.fail(closedException(code, reason)))
                settle.andThen(Abort.run[Closed](incomingCh.offer(Absent)).unit)
        }

    /** Push a server frame to the incoming stream (buffered until it drains). */
    def server(text: String): Unit =
        discard(incomingCh.unsafe.offer(Present(text))(using unsafe, summon[Frame]))

    /** Simulate the server closing cleanly (a `1000` close frame): the liveness
      * signal succeeds and the incoming stream ends after the frames already pushed.
      */
    def serverClose(): Unit =
        discard(donePromise.unsafe.complete(Result.succeed(()))(using unsafe))
        discard(incomingCh.unsafe.offer(Absent)(using unsafe, summon[Frame]))
    end serverClose

    /** Simulate an abnormal server-side drop (aborts the liveness signal). */
    def drop(code: Int, reason: String): Unit =
        discard(donePromise.unsafe.complete(Result.fail(closedException(code, reason)))(using unsafe))
        discard(incomingCh.unsafe.offer(Absent)(using unsafe, summon[Frame]))
    end drop

    private def closedException(code: Int, reason: String): ApolloWebSocketClosedException =
        ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty))
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol.
  * Promoted from `core`'s `FakeWebSocket.scala` — the low-level form beneath
  * [[MockWebSocketServer]] for specs that want the raw connection.
  */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection) extends WebSocketEngine:
    private val opensRef =
        AtomicRef.Unsafe.init(Chunk.empty[(String, Option[String])])(using AllowUnsafe.embrace.danger).safe

    def opens: List[(String, Option[String])] = opensRef.unsafe.get()(using AllowUnsafe.embrace.danger).toList

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        opensRef.getAndUpdate(_.append((url, protocol))).andThen(conn)
end FakeWebSocketEngine

/** An engine that hands back a **fresh** [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket instead of the already-
  * terminated first one. Every opened connection is retained in [[conns]] so a
  * test can drive whichever socket generation it is interested in, and handed out
  * once through [[nextConnection]]. Promoted from `core`'s `FakeWebSocket.scala`.
  */
final class FreshWebSocketEngine extends WebSocketEngine:
    private given Frame = Frame.internal
    private val unsafe  = AllowUnsafe.embrace.danger

    private val connsRef = AtomicRef.Unsafe.init(Chunk.empty[FakeWebSocketConnection])(using unsafe).safe
    private val openedCh = Channel.Unsafe.init[FakeWebSocketConnection](Int.MaxValue)(using summon[Frame], unsafe).safe

    /** Every connection opened so far, in order. */
    def conns: Vector[FakeWebSocketConnection] = connsRef.unsafe.get()(using unsafe).toVector

    /** The next connection the transport opens (each connection is handed out once). */
    def nextConnection(using Frame): FakeWebSocketConnection < Async =
        Abort.run[Closed](openedCh.take).map(_.getOrThrow)

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer(new FakeWebSocketConnection).map { c =>
            connsRef.getAndUpdate(_.append(c)).andThen(Abort.run[Closed](openedCh.offer(c))).andThen(c)
        }
end FreshWebSocketEngine
