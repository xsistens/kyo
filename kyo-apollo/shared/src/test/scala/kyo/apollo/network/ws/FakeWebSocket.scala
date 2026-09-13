package kyo.apollo.network.ws

import java.util.concurrent.atomic.AtomicReference
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloWebSocketClosedException
import scala.collection.mutable

/** Shared, scripted WebSocket test doubles reused by the client/cache/transport
  * specs so each does not re-implement the same in-memory socket. A test drives a
  * [[FakeWebSocketConnection]] directly — pushing server frames, simulating drops —
  * and advances a `Clock.withTimeControl` clock to fire the transport's ack / idle
  * / reconnect timers, so the WebSocket layer is exercised deterministically with
  * no real socket, no server, and no wall-clock delays.
  */

/** A [[WebSocketConnection]] the test scripts directly: it records effectful sends
  * and the close call, and pushes server frames into the incoming channel (buffered
  * until the transport drains it). Its `unsafe` primitives are created eagerly so
  * the plain `server` / `serverClose` / `drop` driving methods can offer/complete
  * synchronously. The incoming channel ends with the same `Absent` marker the
  * platform engines put (see [[WebSocketConnection.untilEnd]]), so frames scripted
  * right before a close are delivered exactly as a real socket would deliver them.
  */
final class FakeWebSocketConnection extends WebSocketConnection:
    private given AllowUnsafe = AllowUnsafe.embrace.danger
    private given Frame       = Frame.internal

    private val sentRef                                      = new AtomicReference[Vector[String]](Vector.empty)
    @volatile private var closedState: Option[(Int, String)] = None
    private val incomingCh: Channel[Maybe[String]] =
        Sync.Unsafe.evalOrThrow(Channel.initUnscoped[Maybe[String]](Int.MaxValue))
    private val donePromise: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]] =
        Sync.Unsafe.evalOrThrow(Fiber.Promise.init[Unit, Abort[ApolloWebSocketClosedException]])

    /** Every frame the transport has sent, in order. */
    def sent: List[String] = sentRef.get().toList

    /** The `(code, reason)` the transport closed with, if it has. */
    def closedWith: Option[(Int, String)] = closedState

    def send(text: String)(using Frame): Unit < Async =
        Sync.defer(discard(sentRef.updateAndGet(_ :+ text)))

    def incoming(using Frame): Stream[String, Async] = WebSocketConnection.untilEnd(incomingCh)

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
                discard(incomingCh.unsafe.offer(Absent))
            end if
        }

    /** Push a server frame to the incoming stream (buffered until it drains). */
    def server(text: String): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(incomingCh.unsafe.offer(Present(text)))

    /** Simulate the server closing cleanly (a `1000` close frame): the liveness
      * signal succeeds and the incoming stream ends after the frames already pushed.
      */
    def serverClose(): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(donePromise.unsafe.completeUnitDiscard())
        discard(incomingCh.unsafe.offer(Absent))
    end serverClose

    /** Simulate an abnormal server-side drop (aborts the liveness signal). */
    def drop(code: Int, reason: String): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(donePromise.unsafe.completeDiscard(
            Result.fail(ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty)))
        ))
        discard(incomingCh.unsafe.offer(Absent))
    end drop
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol. */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection) extends WebSocketEngine:
    private val opensRef                      = new AtomicReference[Vector[(String, Option[String])]](Vector.empty)
    def opens: List[(String, Option[String])] = opensRef.get().toList
    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer { discard(opensRef.updateAndGet(_ :+ (url, protocol))); conn }
end FakeWebSocketEngine

/** An engine that hands back a fresh [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket. Every opened connection is
  * retained in [[conns]] so a test can drive whichever socket generation it wants.
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
  * them by hand. Cancelling drops the pending timer. Retained for the HTTP
  * interceptor specs that still inject a [[WsScheduler]]; the WebSocket transport
  * now drives its timers through `Clock` and is tested with `Clock.withTimeControl`.
  */
final class ManualWsScheduler extends WsScheduler:
    private val tasks = mutable.Map.empty[Long, () => Unit]
    private var seq   = 0L
    def schedule(delayMillis: Long)(task: () => Unit): () => Unit =
        val id = seq
        seq += 1
        tasks(id) = task
        () =>
            tasks.remove(id); ()
    end schedule
    def pending: Int = tasks.size
    def fireAll(): Unit =
        val snapshot = tasks.values.toList
        tasks.clear()
        snapshot.foreach(_())
    end fireAll
end ManualWsScheduler
