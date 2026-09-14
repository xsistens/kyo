package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.api.OperationRequestBody
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** The terminal transport for **subscription** operations: multiplexes many
  * long-lived subscriptions over a single shared [[WebSocketConnection]].
  *
  * The WebSocket analog of [[kyo.apollo.network.http.HttpNetworkTransport]]. It
  * opens a socket lazily on the first subscription, performs the
  * `connection_init` / `connection_ack` handshake once, then routes every server
  * frame to the right subscriber by the operation id it assigned. Each
  * [[subscribe]] returns a cold, long-lived [[ResponseStream]]; nothing touches
  * the socket until it is collected, and cancelling a subscriber sends the
  * protocol `stop`/`complete` for its id.
  *
  * '''Concurrency model.''' All connection and routing state is one [[State]]
  * whose [[Phase]] is `Idle`, `Connecting`, `Open`, `Reconnecting` or `Closed`;
  * each phase carries exactly the socket, fibers and timers that exist in it, so
  * an open socket without a connection, a reconnect without a timer, or an
  * acknowledged handshake that still waits for its ack cannot be written down.
  * The state is changed only by one '''owner fiber''' that takes [[Msg]] commands
  * from a mailbox, one at a time. Every transition is an exhaustive `match` on the
  * phase; a transition that stops a fiber or closes a socket runs that effect in
  * the owner and waits for it, so the next phase never starts while the previous
  * one is still being torn down (a reconnect opens its socket only after the old
  * one is closed).
  *
  * '''Lifetime.''' [[WebSocketNetworkTransport.init]] forks the owner fiber in the
  * caller's `Scope` (the fiber inherits that caller's context, notably a
  * `Clock.withTimeControl` clock) and closes the transport when the `Scope` ends.
  * The owner stays idle, with no socket, until the first subscription arrives; a
  * transport built with the package-private constructor alone has no owner and
  * serves no subscription. The owner runs one nested `Scope` per active period —
  * from the first command to the return to `Idle` — and forks the connection
  * fiber and the timers there, so their finalizer entries are released with the
  * period. [[close]] lets live
  * subscriptions finish for the grace period, then waits until the socket is
  * closed, the connection fiber has released its `Scope` and the owner has
  * exited. A closed transport answers every later [[subscribe]] with an
  * [[ApolloWebSocketClosedException]] value.
  *
  * '''Generation fencing.''' Each opened socket gets a monotonic `generation`.
  * Connection-scoped commands carry the generation they were produced under and
  * are ignored once it no longer matches; timer commands carry a token of their
  * timer and are ignored once that timer is no longer part of the phase.
  *
  * '''Backpressure.''' Each subscription has a channel of
  * [[subscriptionBufferSize]] responses. The owner delivers into it and waits
  * while it is full; the socket drain holds one credit per frame and hands over
  * the next frame only once the owner has processed the previous one, so a
  * consumer that does not keep up stops the socket read instead of growing a
  * buffer. One socket carries every subscription, so a full subscriber holds the
  * others back too. Once [[close]] has begun, a delivery into a full channel is
  * dropped instead of waited for.
  *
  * '''Failures are values''': a socket drop or terminal server close surfaces as
  * `ApolloResponse.error` (an [[ApolloWebSocketClosedException]]) pushed to
  * every active subscriber; the stream itself does not fail. '''Reconnection''' is
  * opt-in via [[reconnectWhen]]: an established socket's abnormal drop surfaces to
  * every subscriber as the resubscription-signal value, then the transport
  * reopens after the next delay of its [[backoff]] schedule, re-runs the
  * handshake, and resubscribes every still-active subscription under its
  * original id. The schedule starts over once a reconnection is acknowledged;
  * when it has no delay left, the drop terminates the subscriptions as it would
  * without reconnection. A frame no subscription can take is logged, never
  * silently dropped: an unknown frame at `warn`, a frame for an unknown id at
  * `debug`.
  *
  * @param serverUrl              the `ws(s)://` endpoint subscriptions connect to
  * @param protocol               the wire protocol (defaults to the modern
  *                               `graphql-transport-ws`); its [[WsProtocol.name]] is
  *                               negotiated as the socket subprotocol
  * @param engine                 opens the platform socket (tests inject a fake)
  * @param connectionPayload      optional `connection_init` payload, e.g. auth headers
  * @param ackTimeoutMillis       how long connecting, the WebSocket handshake and
  *                               the `connection_ack` may take together
  * @param idleTimeoutMillis      how long to keep the socket open after the last
  *                               subscription ends
  * @param reconnectWhen          decides, from the drop's exception and the 1-based
  *                               attempt number, whether to reopen a dropped socket;
  *                               defaults to [[WebSocketNetworkTransport.reconnectNever]]
  * @param backoff                the delays before successive reconnection attempts;
  *                               defaults to 1s, doubling, each capped at 30s
  * @param subscriptionBufferSize how many responses a subscription buffers before
  *                               its consumer's pace holds the socket read back
  */
final class WebSocketNetworkTransport private[apollo] (
    serverUrl: String,
    protocol: WsProtocol = GraphQLWsProtocol,
    engine: WebSocketEngine = WebSocketEngine.default(),
    connectionPayload: Option[Json] = None,
    ackTimeoutMillis: Long = 10000L,
    idleTimeoutMillis: Long = 60000L,
    reconnectWhen: (ApolloException, Long) => Boolean = WebSocketNetworkTransport.reconnectNever,
    backoff: Schedule = WebSocketNetworkTransport.defaultBackoff,
    subscriptionBufferSize: Int = WebSocketNetworkTransport.defaultSubscriptionBufferSize
):
    import WebSocketNetworkTransport.*

    private given Frame = Frame.internal

    // ---- primitives (plain data structures, allocated with the transport) -----

    private val ids: AtomicLong.Unsafe                = AtomicLong.Unsafe.init(0L)(using AllowUnsafe.embrace.danger)
    private val timerTokens: AtomicLong               = AtomicLong.Unsafe.init(0L)(using AllowUnsafe.embrace.danger).safe
    private val stateRef: AtomicRef[State]            = AtomicRef.Unsafe.init(State.initial)(using AllowUnsafe.embrace.danger).safe
    private val ownerStarted: AtomicBoolean           = AtomicBoolean.Unsafe.init(false)(using AllowUnsafe.embrace.danger).safe
    private val ownerExited: Fiber.Promise[Unit, Any] = Fiber.Promise.Unsafe.init[Unit, Any]()(using AllowUnsafe.embrace.danger).safe
    private val closing: Fiber.Promise[Unit, Any]     = Fiber.Promise.Unsafe.init[Unit, Any]()(using AllowUnsafe.embrace.danger).safe
    private val mailbox: Channel[Msg] =
        Channel.Unsafe.init[Msg](Int.MaxValue, Access.MultiProducerSingleConsumer)(using summon[Frame], AllowUnsafe.embrace.danger).safe

    // ---- public API -----------------------------------------------------------

    /** A cold [[ResponseStream]] for `request`'s subscription. Consuming it (within
      * an `Async & Scope` context) registers the operation on the shared socket:
      * server frames are decoded and delivered into a per-subscription channel of
      * [[subscriptionBufferSize]] responses that drives the stream, and the
      * enclosing `Scope`'s teardown sends the protocol stop for this operation and
      * closes the channel. On a closed transport the stream is one response whose
      * error is an [[ApolloWebSocketClosedException]].
      */
    def subscribe[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        // The operation id is assigned eagerly, at the `subscribe` call, so it is a
        // stable property of this subscription fixed in call order — not a race at
        // consume time. Were it assigned lazily inside the per-consumer fiber below,
        // two subscriptions forked concurrently would grab ids in scheduler order
        // (fine on JS's single carrier, non-deterministic on JVM/Native), misrouting
        // every frame the fake servers script by a presumed id.
        val id = ids.getAndIncrement()(using AllowUnsafe.embrace.danger).toString
        Stream.unwrap {
            stateRef.get.map(_.phase).map {
                case Phase.Closed => closedStream(request)
                case _            => register(request, id)
            }
        }
    end subscribe

    /** Close the transport: let the subscriptions active at the call finish for up
      * to `gracePeriod`, then close the socket and cancel any pending reconnect.
      * Subscriptions still active then receive a terminal
      * [[ApolloWebSocketClosedException]] value. Returns once the socket's `close`
      * has completed, the connection fiber has released its `Scope` and the owner
      * fiber has exited. Every later [[subscribe]] yields the closed value.
      */
    def close(gracePeriod: Duration)(using Frame): Unit < Async =
        awaitRoutes(gracePeriod)
            .andThen(closing.completeUnitDiscard)
            .andThen(Async.mask(requestShutdown))
            .andThen(ownerExited.get)

    /** [[close]] with a 30-second grace period. */
    def close(using Frame): Unit < Async = close(30.seconds)

    /** [[close]] without waiting for live subscriptions. */
    def closeNow(using Frame): Unit < Async = close(Duration.Zero)

    // ---- subscription side ----------------------------------------------------

    private def register[D](request: ApolloRequest[D], id: String)(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] < (Async & Scope) =
        for
            channel <- Channel.init[ApolloResponse[D]](subscriptionBufferSize)
            ended   <- Fiber.Promise.init[Unit, Any]
            subscriber = Subscriber(
                id = id,
                startFrame = protocol.startOperation(id, OperationRequestBody(request.operation)),
                deliverData = payload => deliver(channel, decodeData(request, payload)),
                deliverErrors = payload => deliver(channel, errorResponse(request, payload)),
                deliverException = exception => deliver(channel, exceptionResponse(request, exception)),
                // Graceful end, not a hard close: a close hands the buffered responses to the
                // closer, dropping a final `next` that the consumer has not taken yet. The
                // channel ends once the consumer has drained it.
                end = Sync.Unsafe.defer(discard(channel.unsafe.closeAwaitEmpty())).andThen(ended.completeUnitDiscard),
                ended = ended
            )
            // The teardown finalizer is installed BEFORE the route is registered, so an
            // interrupt between the two leaves no route behind (the Cancel of an id the
            // owner never saw is a no-op). It closes the channel first, so an owner
            // waiting to deliver into this full channel is released before the Cancel.
            _ <- Scope.ensure(
                channel.closeDiscard
                    .andThen(ended.completeUnitDiscard)
                    .andThen(offer(Msg.Cancel(id)))
            )
            registered <- Abort.run[Closed](mailbox.offer(Msg.Register(subscriber)))
        yield registered match
            case Result.Success(_) => channel.streamUntilClosed()
            case _                 => closedStream(request)
    end register

    /** Deliver `response` into a subscription's channel, waiting while it is full —
      * unless [[close]] has begun, which a full channel no longer holds up.
      */
    private def deliver[D](channel: Channel[ApolloResponse[D]], response: ApolloResponse[D])(using Frame): Unit < Async =
        Abort.run[Closed](channel.offer(response)).map {
            case Result.Success(false) => Abort.run[Closed](Async.raceFirst(channel.put(response), closing.get)).unit
            case _                     => Kyo.unit // delivered, or the subscription has ended
        }

    private def closedStream[D](request: ApolloRequest[D])(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.init(Chunk(exceptionResponse(request, transportClosed)))

    // ---- lifecycle ------------------------------------------------------------

    /** Fork the owner fiber in the caller's `Scope`, once. The owner inherits the
      * caller's context and waits, idle, for the first subscription. The `Scope`
      * interrupts the owner when it ends, so whoever starts the transport in a
      * `Scope` registers [[close]] there afterwards, which runs first and lets the
      * owner exit on its own. A transport that [[close]] already claimed is not
      * started.
      */
    private[apollo] def start(using Frame): Unit < (Sync & Scope) =
        Sync.Unsafe.defer {
            if ownerStarted.unsafe.compareAndSet(false, true) then Fiber.init(runOwner).unit
            else Kyo.unit
        }

    /** Fork the owner fiber with no `Scope`, once, on a carrier with an empty context:
      * it runs on the default `Clock` and `Log`, not the caller's. Only [[close]] ends
      * it; an owner that is never closed stays parked on its mailbox. The start of a
      * transport created outside the effect system ([[kyo.apollo.ApolloClient.Unsafe.init]]).
      */
    private[apollo] def startDetached()(using AllowUnsafe): Unit =
        if ownerStarted.unsafe.compareAndSet(false, true) then discard(Fiber.Unsafe.init(runOwner))

    /** Hand the shutdown to the owner, or — on a transport whose owner never
      * started — close it right here: `Closed`, a closed mailbox, and no message a
      * later owner could read.
      */
    private def requestShutdown(using Frame): Unit < Async =
        ownerStarted.compareAndSet(false, true).map { claimed =>
            if claimed then
                stateRef.getAndUpdate(_.copy(phase = Phase.Closed))
                    .andThen(mailbox.closeDiscard)
                    .andThen(ownerExited.completeUnitDiscard)
            else offer(Msg.Shutdown)
        }

    /** Wait up to `gracePeriod` for the subscriptions active now to end. */
    private def awaitRoutes(gracePeriod: Duration)(using Frame): Unit < Async =
        if gracePeriod <= Duration.Zero then Kyo.unit
        else
            stateRef.get.map { s =>
                if s.routes.isEmpty then Kyo.unit
                else
                    Abort.run[Timeout](Async.timeout(gracePeriod)(Kyo.foreachDiscard(Chunk.from(s.routes.values.toSeq))(_.ended.get))).unit
            }

    // ---- owner fiber ----------------------------------------------------------

    /** The owner: active periods, each in its own `Scope`, until the phase is
      * `Closed`; then it answers the registrations still queued and exits.
      */
    private def runOwner(using Frame): Unit < Async =
        Sync.ensure(ownerExited.completeUnitDiscard) {
            Abort.run[Throwable] {
                Loop.foreach {
                    Scope.run(period).andThen(stateRef.get).map(_.phase match
                        case Phase.Closed => Loop.done
                        case _            => Loop.continue)
                }
            }.map {
                case Result.Success(_) => retire
                case Result.Failure(e) => ownerFailed(e)
                case Result.Panic(e)   => ownerFailed(e)
            }
        }

    /** One active period: commands until the phase is back to `Idle` (or `Closed`). */
    private def period(using Frame): Unit < (Async & Scope) =
        Loop.foreach {
            Abort.run[Closed](mailbox.take).map {
                case Result.Success(msg) =>
                    handle(msg).andThen(stateRef.get).map(_.phase match
                        case Phase.Idle | Phase.Closed => Loop.done
                        case _                         => Loop.continue)
                case Result.Failure(_) => stateRef.getAndUpdate(_.copy(phase = Phase.Closed)).andThen(Loop.done)
                case Result.Panic(e)   => Abort.panic(e)
            }
        }

    /** Close the mailbox and answer every registration that was still queued. */
    private def retire(using Frame): Unit < Async =
        mailbox.close.map {
            case Present(backlog) =>
                Kyo.foreachDiscard(Chunk.from(backlog)) {
                    case Msg.Register(sub) => sub.deliverException(transportClosed).andThen(sub.end)
                    case _                 => Kyo.unit
                }
            case Absent => Kyo.unit
        }

    private def ownerFailed(cause: Throwable)(using Frame): Unit < Async =
        Log.error("The WebSocket transport's owner fiber failed; the transport is closed", cause)
            .andThen(stateRef.get)
            .map(s => onShutdown(s, ApolloNetworkException("The WebSocket transport failed", cause)))
            .andThen(retire)

    private def handle(msg: Msg)(using Frame): Unit < (Async & Scope) =
        stateRef.get.map { s =>
            msg match
                case Msg.Register(sub)            => onRegister(s, sub)
                case Msg.Cancel(id)               => onCancel(s, id)
                case Msg.Frame(gen, text, credit) => onFrame(s, gen, text).andThen(Abort.run[Closed](credit.poll).unit)
                case Msg.Opened(gen, conn)        => onOpened(s, gen, conn)
                case Msg.OpenFailed(gen, cause)   => whenConnecting(s, gen)(handshakeFailure(s, _, cause))
                case Msg.SocketClosed(gen, cause) => onSocketClosed(s, gen, cause)
                case Msg.AckTimeout(gen)          => whenConnecting(s, gen)(c => handshakeFailure(s, c, ackTimeout(c, s.framesSeen)))
                case Msg.IdleTimeout(token)       => onIdleTimeout(s, token)
                case Msg.ReconnectTimeout(token)  => onReconnectTimeout(s, token)
                case Msg.Shutdown                 => onShutdown(s, normalClose)
        }

    private def whenConnecting(s: State, gen: Long)(f: Phase.Connecting => Unit < (Async & Scope))(using
        Frame
    ): Unit < (Async & Scope) =
        s.phase match
            case c: Phase.Connecting if c.gen == gen => f(c)
            case _                                   => Kyo.unit

    private def onRegister(s: State, sub: Subscriber)(using Frame): Unit < (Async & Scope) =
        val routes = s.routes.updated(sub.id, sub)
        s.phase match
            case Phase.Idle =>
                connect(s.copy(routes = routes), retry = Absent)
            case Phase.Open(gen, conn, socket, idleTimer) =>
                // A live socket: a pending idle close is off, and the start frame goes out now.
                stopTimer(idleTimer)
                    .andThen(stateRef.set(s.copy(phase = Phase.Open(gen, conn, socket, Absent), routes = routes)))
                    .andThen(conn.send(sub.startFrame))
            case _: Phase.Connecting | _: Phase.Reconnecting =>
                // The start frame is sent for every route once connection_ack lands.
                stateRef.set(s.copy(routes = routes))
            case Phase.Closed =>
                sub.deliverException(transportClosed).andThen(sub.end)
        end match
    end onRegister

    private def onCancel(s: State, id: String)(using Frame): Unit < (Async & Scope) =
        s.routes.get(id) match
            case None => Kyo.unit
            case Some(_) =>
                val sendStop = s.phase match
                    case Phase.Open(_, conn, _, _) => conn.send(protocol.stopOperation(id))
                    case _                         => Kyo.unit
                sendStop.andThen(removeRoute(s, id))

    /** Drop a route; when it was the last one, the phase decides what the socket
      * machinery that existed only to serve it becomes.
      */
    private def removeRoute(s: State, id: String)(using Frame): Unit < (Async & Scope) =
        val s1 = s.copy(routes = s.routes.removed(id))
        if s1.routes.nonEmpty then stateRef.set(s1)
        else
            s1.phase match
                case Phase.Open(gen, conn, socket, _) =>
                    // A live socket stays open for the idle window, then closes.
                    timer(idleTimeoutMillis.millis, Msg.IdleTimeout(_)).map { idleTimer =>
                        stateRef.set(s1.copy(phase = Phase.Open(gen, conn, socket, Present(idleTimer))))
                    }
                case phase @ (_: Phase.Connecting | _: Phase.Reconnecting) =>
                    // A handshake or a reconnect backoff that no route is waiting for anymore.
                    teardown(phase, WebSocketConnection.NormalClosure).andThen(stateRef.set(idle(s1)))
                case Phase.Idle | Phase.Closed =>
                    stateRef.set(s1)
        end if
    end removeRoute

    private def onFrame(s: State, gen: Long, text: String)(using Frame): Unit < (Async & Scope) =
        if gen != s.generation then Kyo.unit
        else
            val s1 = s.copy(framesSeen = s.framesSeen + 1)
            stateRef.set(s1).andThen(dispatch(s1, protocol.parse(text)))

    /** Route one decoded server frame to its handler. */
    private def dispatch(s: State, message: WsMessage)(using Frame): Unit < (Async & Scope) =
        message match
            case WsMessage.ConnectionAck(_) =>
                s.phase match
                    case c: Phase.Connecting => onAck(s, c)
                    case _                   => Log.debug(s"Ignored a connection_ack outside the handshake (${protocol.name})")
            case WsMessage.ConnectionError(payload) =>
                fail(
                    s,
                    ApolloNetworkException(
                        payload.fold("WebSocket connection rejected by server")(p => s"WebSocket connection rejected: ${p.render}")
                    )
                )
            case WsMessage.Ping(_) =>
                (protocol.pong(), connectionOf(s.phase)) match
                    case (Some(text), Present(conn)) => conn.send(text)
                    case _                           => Kyo.unit
            case WsMessage.Pong(_) | WsMessage.KeepAlive => Kyo.unit
            case WsMessage.Data(id, payload) =>
                s.routes.get(id) match
                    case Some(sub) => sub.deliverData(payload)
                    case None      => unroutable("next", id)
            case WsMessage.Error(id, payload) =>
                s.routes.get(id) match
                    case Some(sub) => sub.deliverErrors(payload).andThen(sub.end).andThen(removeRoute(s, id))
                    case None      => unroutable("error", id)
            case WsMessage.Complete(id) =>
                s.routes.get(id) match
                    case Some(sub) => sub.end.andThen(removeRoute(s, id))
                    case None      => unroutable("complete", id)
            case WsMessage.Unknown(raw) =>
                val shape = WsProtocol.asObject(raw) match
                    case Some(fields) => s"type '${WsProtocol.typeOf(fields)}'"
                    case None         => "not a JSON object"
                // The frame's type and size, never its text: a payload can carry subscription data.
                Log.warn(s"Dropped a WebSocket frame the ${protocol.name} protocol does not define ($shape, ${raw.length} chars)")

    private def unroutable(kind: String, id: String)(using Frame): Unit < Sync =
        Log.debug(s"Dropped a WebSocket '$kind' frame for id '$id', which has no active subscription")

    /** `connection_ack` arrived: the ack timer stops, the socket is open, and the
      * start frame goes out for every route — fresh subscriptions and a reconnect's
      * resubscriptions alike. The reconnect schedule starts over (no retry left in
      * the phase).
      */
    private def onAck(s: State, c: Phase.Connecting)(using Frame): Unit < (Async & Scope) =
        c.conn match
            case Absent => Kyo.unit // frames are handed over only after `Opened`
            case Present(conn) =>
                stopFiber(c.ackTimer)
                    .andThen(stateRef.set(s.copy(phase = Phase.Open(c.gen, conn, c.socket, Absent))))
                    .andThen(Kyo.foreachDiscard(Chunk.from(s.routes.values.toSeq))(sub => conn.send(sub.startFrame)))

    /** The socket closed. `cause` is `Absent` on a clean `1000` close, or the
      * drop's exception on an abnormal close. During the handshake it is a
      * [[handshakeFailure]]; an open socket's drop either reconnects (when
      * [[reconnectWhen]] agrees, subscriptions remain and the [[backoff]] schedule
      * has a delay left) or terminates them.
      */
    private def onSocketClosed(s: State, gen: Long, cause: Maybe[ApolloException])(using Frame): Unit < (Async & Scope) =
        if gen != s.generation then Kyo.unit
        else
            s.phase match
                case c: Phase.Connecting => handshakeFailure(s, c, cause.getOrElse(normalClose))
                case open: Phase.Open =>
                    cause match
                        case Present(drop) if s.routes.nonEmpty && reconnectWhen(drop, 1L) =>
                            Clock.now.map(now => backoff.next(now)).map {
                                case Present((delay, rest)) =>
                                    val s1 = idle(s).copy(routes = s.routes)
                                    teardown(open, WebSocketConnection.NormalClosure)
                                        .andThen(stateRef.set(s1))
                                        .andThen(Kyo.foreachDiscard(Chunk.from(s1.routes.values.toSeq))(_.deliverException(drop)))
                                        .andThen(scheduleReconnect(s1, ReconnectPlan(1L, rest), delay))
                                case Absent => fail(s, drop)
                            }
                        case _ => fail(s, cause.getOrElse(normalClose))
                case Phase.Idle | Phase.Reconnecting(_, _) | Phase.Closed => Kyo.unit

    private def onOpened(s: State, gen: Long, conn: WebSocketConnection)(using Frame): Unit < (Async & Scope) =
        s.phase match
            case c: Phase.Connecting if c.gen == gen && c.conn.isEmpty =>
                stateRef.set(s.copy(phase = c.copy(conn = Present(conn))))
                    .andThen(conn.send(protocol.connectionInit(connectionPayload)))
            case _ =>
                conn.close() // superseded before it opened — discard it

    private def onIdleTimeout(s: State, token: Long)(using Frame): Unit < (Async & Scope) =
        s.phase match
            case open @ Phase.Open(_, _, _, Present(idleTimer)) if idleTimer.token == token && s.routes.isEmpty =>
                teardown(open, WebSocketConnection.NormalClosure).andThen(stateRef.set(idle(s)))
            case _ => Kyo.unit

    private def onReconnectTimeout(s: State, token: Long)(using Frame): Unit < (Async & Scope) =
        s.phase match
            case Phase.Reconnecting(retry, timer) if timer.token == token => connect(s, Present(retry))
            case _                                                        => Kyo.unit

    private def onShutdown(s: State, exception: ApolloException)(using Frame): Unit < Async =
        stateRef.set(s.copy(phase = Phase.Closed))
            .andThen(teardown(s.phase, closeCodeOf(exception)))
            .andThen(endRoutes(s, exception))
            .andThen(stateRef.set(State(Phase.Closed, VectorMap.empty, s.generation + 1, 0L)))

    // ---- connection lifecycle -------------------------------------------------

    /** Arm the ack timer, then open a socket on a connection fiber. The timer covers
      * connecting, the WebSocket handshake and the `connection_ack` together.
      */
    private def connect(s: State, retry: Maybe[ReconnectPlan])(using Frame): Unit < (Async & Scope) =
        val gen = s.generation + 1
        for
            ackTimer <- Fiber.init(Async.sleep(ackTimeoutMillis.millis).andThen(offer(Msg.AckTimeout(gen))))
            socket   <- worker(openLoop(gen))
        yield stateRef.set(s.copy(phase = Phase.Connecting(gen, socket, ackTimer, Absent, retry), generation = gen, framesSeen = 0L))
        end for
    end connect

    /** The connection fiber's body: open the socket, report it, then drain its
      * frames. `Opened` is queued before the drain starts, so no frame of a socket
      * reaches the owner ahead of the socket itself.
      */
    private def openLoop(gen: Long)(using Frame): Unit < (Async & Scope) =
        Abort.run[Throwable](engine.open(serverUrl, Some(protocol.name))).map {
            case Result.Success(conn) =>
                Channel.init[Unit](1).map { credit =>
                    offer(Msg.Opened(gen, conn))
                        .andThen(Fiber.init(drainThenWatchClosed(gen, conn, credit)))
                        .andThen(Async.never)
                }
            case Result.Failure(cause) => offer(Msg.OpenFailed(gen, toApolloException(cause)))
            case Result.Panic(cause)   => offer(Msg.OpenFailed(gen, toApolloException(cause)))
        }

    /** Drain `incoming` to its end, THEN watch `closed` — in one fiber, so
      * `SocketClosed` is queued strictly after every `Frame` the socket delivered.
      * `incoming` ends exactly when the socket closes (the engines put an end-marker
      * after their last frame), so nothing is lost by waiting. Two independent
      * fibers would race: a `SocketClosed` that overtakes the last `next` +
      * `complete` terminates the routes and bumps the generation, fencing those
      * frames out as stale. Each frame first takes the connection's one credit,
      * which the owner returns after processing the frame.
      */
    private def drainThenWatchClosed(gen: Long, conn: WebSocketConnection, credit: Channel[Unit])(using Frame): Unit < Async =
        Abort.run[Closed](
            conn.incoming.foreach(text => credit.put(()).andThen(offer(Msg.Frame(gen, text, credit))))
        ).andThen(watchClosed(gen, conn))

    private def watchClosed(gen: Long, conn: WebSocketConnection)(using Frame): Unit < Async =
        Abort.run[ApolloWebSocketClosedException](conn.closed).map {
            case Result.Success(_)      => offer(Msg.SocketClosed(gen, Absent))
            case Result.Failure(closed) => offer(Msg.SocketClosed(gen, Present(closed)))
            case Result.Panic(cause)    => offer(Msg.SocketClosed(gen, Present(toApolloException(cause))))
        }

    // ---- failures and reconnection --------------------------------------------

    /** A failure before the handshake's ack: the socket is discarded, then a
      * reconnect attempt retries (or gives up) and a first handshake terminates its
      * pending subscriptions with the failure value.
      */
    private def handshakeFailure(s: State, c: Phase.Connecting, exception: ApolloException)(using Frame): Unit < (Async & Scope) =
        teardown(c, closeCodeOf(exception)).andThen {
            val s1 = idle(s).copy(routes = s.routes)
            c.retry match
                case Present(retry) if s1.routes.nonEmpty && reconnectWhen(exception, retry.attempt + 1) =>
                    Clock.now.map(now => retry.plan.next(now)).map {
                        case Present((delay, rest)) => scheduleReconnect(s1, ReconnectPlan(retry.attempt + 1, rest), delay)
                        case Absent                 => endRoutes(s1, exception).andThen(stateRef.set(s1.copy(routes = VectorMap.empty)))
                    }
                case _ => endRoutes(s1, exception).andThen(stateRef.set(s1.copy(routes = VectorMap.empty)))
            end match
        }

    private def scheduleReconnect(s: State, retry: ReconnectPlan, delay: Duration)(using Frame): Unit < (Async & Scope) =
        timer(delay, Msg.ReconnectTimeout(_)).map(t => stateRef.set(s.copy(phase = Phase.Reconnecting(retry, t))))

    /** Full terminal shutdown of the current phase: the socket closes with the
      * value's close code, every subscription receives the value and ends, and the
      * transport is `Idle`.
      */
    private def fail(s: State, exception: ApolloException)(using Frame): Unit < Async =
        teardown(s.phase, closeCodeOf(exception))
            .andThen(endRoutes(s, exception))
            .andThen(stateRef.set(idle(s)))

    private def endRoutes(s: State, exception: ApolloException)(using Frame): Unit < Async =
        Kyo.foreachDiscard(Chunk.from(s.routes.values.toSeq))(sub => sub.deliverException(exception).andThen(sub.end))

    /** Stop everything `phase` owns and wait for it: timers, the socket's close with
      * `code`, and the connection fiber's `Scope` release.
      */
    private def teardown(phase: Phase, code: Int)(using Frame): Unit < Async =
        phase match
            case Phase.Connecting(_, socket, ackTimer, conn, _) =>
                stopFiber(ackTimer).andThen(conn.fold(Kyo.unit)(_.close(code))).andThen(socket.stop)
            case Phase.Open(_, conn, socket, idleTimer) =>
                stopTimer(idleTimer).andThen(conn.close(code)).andThen(socket.stop)
            case Phase.Reconnecting(_, timer) =>
                stopFiber(timer.fiber)
            case Phase.Idle | Phase.Closed =>
                Kyo.unit

    /** `s` with no socket machinery: `Idle`, the next generation, no routes. */
    private def idle(s: State): State = State(Phase.Idle, VectorMap.empty, s.generation + 1, 0L)

    // ---- fibers ---------------------------------------------------------------

    /** Fork `body` on a connection fiber of the current period. Its `Scope` records
      * `released` first, so that finalizer runs last: once `released` is complete,
      * every resource the body acquired has been released.
      */
    private def worker(body: Unit < (Async & Scope))(using Frame): Worker < (Sync & Scope) =
        for
            entered  <- AtomicBoolean.init(false)
            released <- Fiber.Promise.init[Unit, Any]
            fiber <- Fiber.init(
                Scope.run(Scope.ensure(released.completeUnitDiscard).andThen(entered.set(true)).andThen(body))
            )
        yield Worker(fiber, entered, released)

    private def timer(delay: Duration, msg: Long => Msg)(using Frame): Timer < (Sync & Scope) =
        timerTokens.incrementAndGet.map { token =>
            Fiber.init(Async.sleep(delay).andThen(offer(msg(token)))).map(Timer(token, _))
        }

    private def stopTimer(timer: Maybe[Timer])(using Frame): Unit < Async =
        timer.fold(Kyo.unit)(t => stopFiber(t.fiber))

    private def offer(msg: Msg)(using Frame): Unit < Sync =
        Abort.run[Closed](mailbox.offer(msg)).unit

    private def ackTimeout(c: Phase.Connecting, framesSeen: Long)(using Frame): ApolloNetworkException =
        val detail = c.conn match
            case Absent => "the socket did not open (connecting or the WebSocket upgrade hung), 0 frames received"
            case Present(_) if framesSeen == 0 =>
                "0 frames received: the endpoint sent nothing back (is this a GraphQL WebSocket endpoint?)"
            case Present(_) =>
                s"$framesSeen frames received, none of them connection_ack (does the server speak ${protocol.name}?)"
        ApolloNetworkException(s"Timed out waiting for connection_ack after ${ackTimeoutMillis.millis.show}: $detail")
    end ackTimeout

    // ---- response decoding (pure; per-subscription D captured at subscribe) ----

    // Decoding runs on the owner fiber, which every subscription on the socket shares:
    // a decoder defect is folded into this subscription's value (with the defect as
    // `cause`) rather than raised, so it cannot stop the owner loop.
    private def decodeData[D](request: ApolloRequest[D], payload: Json): ApolloResponse[D] =
        GraphQLResponse.parse(payload, request.operation).fold(
            response => ApolloResponse.fromGraphQLResponse(request.requestUuid, response, request.executionContext),
            parseFailure => exceptionResponse(request, parseFailure),
            defect =>
                exceptionResponse(
                    request,
                    ApolloParseException(payload, s"a GraphQL response payload for operation '${request.operation.name}'", defect)
                )
        )

    private def errorResponse[D](request: ApolloRequest[D], payload: Json): ApolloResponse[D] =
        parseErrors(payload).fold(
            errors =>
                ApolloResponse(
                    requestUuid = request.requestUuid,
                    data = Absent,
                    error = Present(ApolloGraphQLException(errors)),
                    executionContext = request.executionContext
                ),
            parseFailure => exceptionResponse(request, parseFailure),
            defect =>
                exceptionResponse(
                    request,
                    ApolloParseException(payload, s"GraphQL error objects for operation '${request.operation.name}'", defect)
                )
        )

    private def parseErrors(payload: Json): Result[ApolloParseException, Chunk[GraphQLError]] = payload match
        case Json.JArr(items) => Result.collect(items.map(GraphQLError.parse)).map(Chunk.from)
        case obj: Json.JObj   => GraphQLError.parse(obj).map(Chunk(_))
        case _                => Result.succeed(Chunk.empty)

    private def exceptionResponse[D](request: ApolloRequest[D], exception: ApolloException): ApolloResponse[D] =
        ApolloResponse.fromException(request.requestUuid, exception, request.executionContext)
end WebSocketNetworkTransport

object WebSocketNetworkTransport:

    /** Never reopen a dropped socket: the drop terminates its subscriptions with
      * the [[ApolloWebSocketClosedException]] value. The default, matching
      * apollo-kotlin's opt-in `reopenWhen`.
      */
    val reconnectNever: (ApolloException, Long) => Boolean = (_, _) => false

    /** Always reopen on a drop, regardless of cause or attempt count (the
      * `backoff` schedule still paces the attempts, and ends them once it has no
      * delay left).
      */
    val reconnectAlways: (ApolloException, Long) => Boolean = (_, _) => true

    /** The default reconnection delays: 1s, doubling, each capped at 30s, without end. */
    val defaultBackoff: Schedule = Schedule.exponentialBackoff(1.second, 2.0, 30.seconds)

    /** The default number of responses a subscription buffers for its consumer. */
    val defaultSubscriptionBufferSize: Int = 256

    /** Create a transport owned by the enclosing `Scope`: its owner fiber is forked in
      * that `Scope`, idle until the first subscription, and the `Scope`'s end
      * [[WebSocketNetworkTransport.close]]s the transport with the default grace
      * period. Parameters as on [[WebSocketNetworkTransport]].
      */
    def init(
        serverUrl: String,
        protocol: WsProtocol = GraphQLWsProtocol,
        engine: WebSocketEngine = WebSocketEngine.default(),
        connectionPayload: Option[Json] = None,
        ackTimeoutMillis: Long = 10000L,
        idleTimeoutMillis: Long = 60000L,
        reconnectWhen: (ApolloException, Long) => Boolean = reconnectNever,
        backoff: Schedule = defaultBackoff,
        subscriptionBufferSize: Int = defaultSubscriptionBufferSize
    )(using Frame): WebSocketNetworkTransport < (Sync & Scope) =
        Sync.defer(
            new WebSocketNetworkTransport(
                serverUrl,
                protocol,
                engine,
                connectionPayload,
                ackTimeoutMillis,
                idleTimeoutMillis,
                reconnectWhen,
                backoff,
                subscriptionBufferSize
            )
        ).map(transport => transport.start.andThen(Scope.ensure(transport.close)).andThen(transport))

    private def normalClose(using Frame): ApolloWebSocketClosedException =
        ApolloWebSocketClosedException(WebSocketConnection.NormalClosure)

    private def transportClosed(using Frame): ApolloWebSocketClosedException =
        ApolloWebSocketClosedException(WebSocketConnection.NormalClosure, Some("the WebSocket transport is closed"))

    private def closeCodeOf(exception: ApolloException): Int = exception match
        case e: ApolloWebSocketClosedException => e.code
        case _                                 => WebSocketConnection.NormalClosure

    private def toApolloException(cause: Throwable)(using Frame): ApolloException = cause match
        case exception: ApolloException => exception
        case other                      => ApolloNetworkException(cause = other)

    private def connectionOf(phase: Phase): Maybe[WebSocketConnection] = phase match
        case c: Phase.Connecting => c.conn
        case o: Phase.Open       => Present(o.conn)
        case _                   => Absent

    /** Interrupt `fiber` and wait for its result. Enough for a fiber that holds no
      * resource (a timer); a connection fiber is stopped through its [[Worker]].
      */
    private def stopFiber(fiber: Fiber[Unit, Any])(using Frame): Unit < Async =
        fiber.interrupt.andThen(fiber.getResult).unit

    /** One live subscriber's per-`D` reactions, captured at [[subscribe]] time and
      * run by the owner fiber: each delivers into the subscription's channel.
      */
    final private case class Subscriber(
        id: String,
        startFrame: String,
        deliverData: Json => Unit < Async,
        deliverErrors: Json => Unit < Async,
        deliverException: ApolloException => Unit < Async,
        end: Unit < Async,
        ended: Fiber.Promise[Unit, Any]
    )

    /** A connection fiber whose teardown can be awaited. Interrupting a fiber and
      * awaiting its result returns as soon as the interrupt lands, before the
      * finalizers of its `Scope` have run; [[stop]] waits for `released`, the last of
      * them, when the body got as far as registering it.
      */
    final private case class Worker(fiber: Fiber[Unit, Any], entered: AtomicBoolean, released: Fiber.Promise[Unit, Any]):
        def stop(using Frame): Unit < Async =
            stopFiber(fiber).andThen(entered.get).map(if _ then released.get else Kyo.unit)

    /** A timer fiber and the token its command carries. */
    final private case class Timer(token: Long, fiber: Fiber[Unit, Any])

    /** A reconnection in progress: the attempt number and the rest of the schedule. */
    final private case class ReconnectPlan(attempt: Long, plan: Schedule)

    /** Where the connection is. Each case holds exactly what exists in it. */
    private enum Phase derives CanEqual:
        /** No socket. */
        case Idle

        /** A socket is being opened (`conn` absent) or has opened and waits for
          * `connection_ack` (`conn` present); `ackTimer` covers both. `retry` is
          * present when this is a reconnection attempt.
          */
        case Connecting(
            gen: Long,
            socket: Worker,
            ackTimer: Fiber[Unit, Any],
            conn: Maybe[WebSocketConnection],
            retry: Maybe[ReconnectPlan]
        )

        /** The handshake is acknowledged. `idleTimer` runs while no route is left. */
        case Open(gen: Long, conn: WebSocketConnection, socket: Worker, idleTimer: Maybe[Timer])

        /** A dropped socket waits for its next attempt. */
        case Reconnecting(retry: ReconnectPlan, timer: Timer)

        /** Terminal: [[close]] has run. */
        case Closed
    end Phase

    /** The whole transport state, changed only by the owner fiber. `framesSeen`
      * counts the current socket's frames, for the ack-timeout diagnosis.
      */
    final private case class State(
        phase: Phase,
        routes: VectorMap[String, Subscriber],
        generation: Long,
        framesSeen: Long
    )

    private object State:
        val initial: State = State(Phase.Idle, VectorMap.empty, 0L, 0L)

    /** The owner fiber's command mailbox alphabet. Connection-scoped commands carry
      * the `generation` of the socket that produced them, timer commands the token
      * of their timer.
      */
    private enum Msg derives CanEqual:
        case Register(sub: Subscriber)
        case Cancel(id: String)
        case Frame(gen: Long, text: String, credit: Channel[Unit])
        case Opened(gen: Long, conn: WebSocketConnection)
        case OpenFailed(gen: Long, cause: ApolloException)
        case SocketClosed(gen: Long, cause: Maybe[ApolloException])
        case AckTimeout(gen: Long)
        case IdleTimeout(token: Long)
        case ReconnectTimeout(token: Long)
        case Shutdown
    end Msg
end WebSocketNetworkTransport
