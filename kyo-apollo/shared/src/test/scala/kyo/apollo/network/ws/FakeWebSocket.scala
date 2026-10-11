package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloWebSocketClosedException

/** Shared, scripted WebSocket test doubles reused by the client/cache/transport
  * specs so each does not re-implement the same in-memory socket. A test drives a
  * [[FakeWebSocketConnection]] directly — pushing server frames, simulating drops —
  * and advances a `Clock.withTimeControl` clock to fire the transport's ack / idle
  * / reconnect timers, so the WebSocket layer is exercised deterministically with
  * no real socket, no server, and no wall-clock delays.
  *
  * Every observation a test waits for is a barrier, never a wall-clock pause: the
  * frames the transport sends arrive in order through [[FakeWebSocketConnection.nextSent]],
  * the frames it reads are counted through [[FakeWebSocketConnection.awaitFramesRead]],
  * a connection's `Scope` release completes [[FakeWebSocketConnection.awaitReleased]],
  * and each opened socket is handed out by [[FreshWebSocketEngine.nextConnection]].
  */

/** A [[WebSocketConnection]] the test scripts directly: it records effectful sends
  * and the close call, and pushes server frames into the incoming channel (buffered
  * until the transport drains it). Its primitives are created eagerly so the plain
  * `server` / `serverClose` / `drop` driving methods can push synchronously. The
  * incoming channel ends with the same `Absent` marker the platform engines put
  * (see [[WebSocketConnection.untilEnd]]), so frames scripted right before a close
  * are delivered exactly as a real socket would deliver them.
  */
final class FakeWebSocketConnection extends WebSocketConnection:
    import FakeWebSocketConnection.unsafe
    private given Frame = Frame.internal

    private val sentRef     = AtomicRef.Unsafe.init(Chunk.empty[String])(using unsafe).safe
    private val sentCh      = Channel.Unsafe.init[String](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val closedRef   = AtomicRef.Unsafe.init(Maybe.empty[(Int, String)])(using unsafe).safe
    private val readCount   = AtomicInt.Unsafe.init(0)(using unsafe).safe
    private val readCh      = Channel.Unsafe.init[Unit](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val releasedP   = Fiber.Promise.Unsafe.init[Unit, Any]()(using unsafe).safe
    private val incomingCh  = Channel.Unsafe.init[Maybe[String]](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val donePromise = Fiber.Promise.Unsafe.init[Unit, Abort[ApolloWebSocketClosedException]]()(using unsafe).safe

    /** Every frame the transport has sent so far, in order. */
    def sent: List[String] = sentRef.unsafe.get()(using unsafe).toList

    /** The next frame the transport sends, in send order (each frame is handed out once). */
    def nextSent(using Frame): String < Async =
        Abort.run[Closed](sentCh.take).map(_.getOrThrow)

    /** Skip sent frames until one satisfies `p`, and return it. */
    def awaitSent(p: String => Boolean)(using Frame): String < Async =
        Loop.foreach(nextSent.map(frame => if p(frame) then Loop.done(frame) else Loop.continue))

    /** The `(code, reason)` the transport closed with, if it has. */
    def closedWith: Option[(Int, String)] = closedRef.unsafe.get()(using unsafe).toOption

    /** How many frames the transport has taken from the socket so far. */
    def framesRead(using Frame): Int < Sync = readCount.get

    /** Wait until the transport has taken `n` more frames from the socket. */
    def awaitFramesRead(n: Int)(using Frame): Unit < Async =
        Kyo.foreachDiscard(Chunk.from(1 to n))(_ => Abort.run[Closed](readCh.take).unit)

    /** Wait until the `Scope` the engine opened this connection in has been released. */
    def awaitReleased(using Frame): Unit < Async = releasedP.get

    private[ws] def markReleased(using Frame): Unit < Sync = releasedP.completeUnitDiscard

    def send(text: String)(using Frame): Unit < Async =
        sentRef.getAndUpdate(_.append(text)).andThen(Abort.run[Closed](sentCh.offer(text)).unit)

    def incoming(using Frame): Stream[String, Async] =
        WebSocketConnection.untilEnd(incomingCh).map { text =>
            readCount.incrementAndGet.andThen(Abort.run[Closed](readCh.offer(()))).andThen(text)
        }

    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException]) = donePromise.get

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async =
        closedRef.compareAndSet(Absent, Present((code, reason))).map { first =>
            if !first then Kyo.unit
            else
                val endLiveness =
                    if code == WebSocketConnection.NormalClosure then donePromise.completeUnitDiscard
                    else donePromise.completeDiscard(Result.fail(closedException(code, reason)))
                endLiveness.andThen(Abort.run[Closed](incomingCh.offer(Absent)).unit)
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

object FakeWebSocketConnection:
    private[ws] val unsafe: AllowUnsafe = AllowUnsafe.embrace.danger

    /** Count `conn` as open in `open`'s `Scope` and release it with that `Scope`,
      * `releaseDelay` later on the opener's clock. `openNow`/`maxOpen` count the
      * connections whose `Scope` has not been released yet.
      *
      * The clock is captured at open: the finalizers of an interrupted fiber's
      * `Scope` run without that fiber's locals, so a bare `Async.sleep` there would
      * wait on the live clock instead of the test's controlled one.
      */
    private[ws] def openIn(
        conn: FakeWebSocketConnection,
        releaseDelay: Duration,
        openNow: AtomicInt,
        maxOpen: AtomicInt
    )(using Frame): Unit < (Sync & Scope) =
        Clock.use { clock =>
            openNow.incrementAndGet.map { n =>
                maxOpen.getAndUpdate(math.max(_, n)).andThen(
                    Scope.ensure(
                        Clock.let(clock)(Async.sleep(releaseDelay)).andThen(openNow.decrementAndGet).andThen(conn.markReleased)
                    )
                )
            }
        }
end FakeWebSocketConnection

/** An engine that always hands back `conn`, recording each open's url/protocol.
  * The connection's `Scope` release takes `releaseDelay` on the caller's clock.
  */
final class FakeWebSocketEngine(conn: FakeWebSocketConnection, releaseDelay: Duration = Duration.Zero) extends WebSocketEngine:
    import FakeWebSocketConnection.unsafe
    private given Frame = Frame.internal

    private val opensRef = AtomicRef.Unsafe.init(Chunk.empty[(String, Option[String])])(using unsafe).safe
    private val openNow  = AtomicInt.Unsafe.init(0)(using unsafe).safe
    private val maxOpen  = AtomicInt.Unsafe.init(0)(using unsafe).safe

    def opens: List[(String, Option[String])] = opensRef.unsafe.get()(using unsafe).toList

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        opensRef.getAndUpdate(_.append((url, protocol)))
            .andThen(FakeWebSocketConnection.openIn(conn, releaseDelay, openNow, maxOpen))
            .andThen(conn)
end FakeWebSocketEngine

/** An engine that hands back a fresh [[FakeWebSocketConnection]] per open, in
  * order, so a reconnect gets a brand-new socket. Every opened connection is
  * retained in [[conns]] and handed out once through [[nextConnection]]; the
  * engine counts how many connections were open at the same time. A connection's
  * `Scope` release takes `releaseDelay` on the caller's clock.
  */
final class FreshWebSocketEngine(releaseDelay: Duration = Duration.Zero) extends WebSocketEngine:
    import FakeWebSocketConnection.unsafe
    private given Frame = Frame.internal

    private val connsRef = AtomicRef.Unsafe.init(Chunk.empty[FakeWebSocketConnection])(using unsafe).safe
    private val openedCh = Channel.Unsafe.init[FakeWebSocketConnection](Int.MaxValue)(using summon[Frame], unsafe).safe
    private val openNow  = AtomicInt.Unsafe.init(0)(using unsafe).safe
    private val maxOpen  = AtomicInt.Unsafe.init(0)(using unsafe).safe

    /** Every connection opened so far, in order. */
    def conns: Vector[FakeWebSocketConnection] = connsRef.unsafe.get()(using unsafe).toVector

    /** The next connection the transport opens (each connection is handed out once). */
    def nextConnection(using Frame): FakeWebSocketConnection < Async =
        Abort.run[Closed](openedCh.take).map(_.getOrThrow)

    /** The most connections that were open (not yet released) at the same time. */
    def maxConcurrentlyOpen(using Frame): Int < Sync = maxOpen.get

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        Sync.defer(new FakeWebSocketConnection).map { conn =>
            connsRef.getAndUpdate(_.append(conn))
                .andThen(FakeWebSocketConnection.openIn(conn, releaseDelay, openNow, maxOpen))
                .andThen(Abort.run[Closed](openedCh.offer(conn)))
                .andThen(conn)
        }
end FreshWebSocketEngine

/** An engine whose open never completes — a server that accepts the TCP
  * connection but never finishes the WebSocket upgrade. [[awaitOpenStarted]]
  * completes once the transport has begun opening.
  */
final class HangingWebSocketEngine extends WebSocketEngine:
    import FakeWebSocketConnection.unsafe
    private given Frame = Frame.internal

    private val started = Fiber.Promise.Unsafe.init[Unit, Any]()(using unsafe).safe

    def awaitOpenStarted(using Frame): Unit < Async = started.get

    def open(url: String, protocol: Option[String])(using
        Frame
    ): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        started.completeUnitDiscard.andThen(Async.never)
end HangingWebSocketEngine
