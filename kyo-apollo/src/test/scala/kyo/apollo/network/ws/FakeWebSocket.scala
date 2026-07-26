package kyo.apollo.network.ws

import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.discard
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.Promise

/** Shared, scripted WebSocket test doubles reused by the client/cache/transport
  * specs so each does not re-implement the same in-memory socket. A test drives
  * a [[FakeWebSocketConnection]] directly — pushing server frames, simulating
  * drops — and fires the [[ManualWsScheduler]]'s timers by hand, so the
  * WebSocket layer is exercised deterministically with no real socket, no
  * server, and no wall-clock delays.
  */

/** A [[WebSocketConnection]] the test scripts directly: it records sends and the
  * close call, buffers server frames until the single sink attaches, then pushes
  * them in order.
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
            if code == WebSocketConnection.NormalClosure then discard(done.trySuccess(()))
            else
                discard(done.tryFailure(
                    ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty))
                ))
            end if

    /** Push a server frame to the sink (buffered until one attaches). */
    def server(text: String): Unit = sink match
        case Some(emit) => emit(text)
        case None       => buffer.enqueue(text)

    /** Simulate an abnormal server-side drop (fails the liveness stream). */
    def drop(code: Int, reason: String): Unit =
        discard(done.tryFailure(
            ApolloWebSocketClosedException(code, Option(reason).filter(_.nonEmpty))
        ))
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol. */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection) extends WebSocketEngine:
    var opens: List[(String, Option[String])] = List.empty
    def open(url: String, protocol: Option[String]): Future[WebSocketConnection] =
        opens = opens :+ (url, protocol)
        Future.successful(conn)
end FakeWebSocketEngine

/** An engine that hands back a **fresh** [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket instead of the already-
  * terminated first one. Every opened connection is retained in [[conns]] so a
  * test can drive whichever socket generation it is interested in.
  */
final class FreshWebSocketEngine extends WebSocketEngine:
    var conns: Vector[FakeWebSocketConnection] = Vector.empty
    def open(url: String, protocol: Option[String]): Future[WebSocketConnection] =
        val conn = new FakeWebSocketConnection
        conns = conns :+ conn
        Future.successful(conn)
    end open
end FreshWebSocketEngine

/** A scheduler that records timers instead of arming real ones; the test fires
  * them by hand. Cancelling drops the pending timer.
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
