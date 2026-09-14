package kyo.apollo.network.ws

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
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
  * '''Concurrency model (native kyo).''' All connection + routing state lives in a
  * single [[State]] mutated only by one background '''owner fiber''' that consumes
  * a command [[Channel]] mailbox. Every external interaction ([[subscribe]],
  * [[close]]) and every socket/timer event enqueues a [[Msg]]; the owner fiber
  * processes them one at a time, so the state machine is serialized without locks
  * and is correct on JS, JVM and Native alike (unlike a bare event-loop
  * assumption). The [[State]] is held in an `AtomicReference` purely for cross-
  * carrier-thread visibility of the owner fiber's own writes.
  *
  * '''Generation fencing.''' Each opened socket gets a monotonic `generation`.
  * Connection-scoped commands ([[Msg.Frame]], [[Msg.SocketClosed]],
  * [[Msg.Opened]], [[Msg.OpenFailed]], [[Msg.AckTimeout]]) carry the generation
  * they were produced under and are ignored when it no longer matches, so a
  * superseded socket's late events cannot re-enter the machine. Discarding a
  * socket also interrupts its dedicated fiber (which holds the connection's
  * `Scope` plus the incoming-drain and close-watcher fibers), tearing the socket
  * and its producers down.
  *
  * '''Timers''' (ack, idle, reconnect backoff) are `Async.delay`-forked fibers
  * that enqueue a command when they elapse and are cancelled by interrupt. Being
  * `Clock`-driven they are deterministic under `Clock.withTimeControl` in tests.
  *
  * '''Failures are values''': a socket drop or terminal server close surfaces as
  * `ApolloResponse.error` (an [[ApolloWebSocketClosedException]]) pushed to
  * every active subscriber; the stream itself does not fail. '''Reconnection''' is
  * opt-in via [[reconnectWhen]]: an established socket's abnormal drop surfaces to
  * every subscriber as the resubscription-signal value, then the transport
  * reopens after a [[backoff]] delay, re-runs the handshake, and resubscribes
  * every still-active subscription under its original id.
  *
  * @param serverUrl         the `ws(s)://` endpoint subscriptions connect to
  * @param protocol          the wire protocol (defaults to the modern
  *                          `graphql-transport-ws`); its [[WsProtocol.name]] is
  *                          negotiated as the socket subprotocol
  * @param engine            opens the platform socket (tests inject a fake)
  * @param connectionPayload optional `connection_init` payload, e.g. auth headers
  * @param ackTimeoutMillis  how long to wait for `connection_ack` before failing
  *                          the handshake
  * @param idleTimeoutMillis how long to keep the socket open after the last
  *                          subscription ends
  * @param reconnectWhen     decides, from the drop's exception and the 1-based
  *                          attempt number, whether to reopen a dropped socket;
  *                          defaults to [[WebSocketNetworkTransport.reconnectNever]]
  * @param backoff           how long to wait before each reconnection attempt
  */
final class WebSocketNetworkTransport(
    serverUrl: String,
    protocol: WsProtocol = GraphQLWsProtocol,
    engine: WebSocketEngine = WebSocketEngine.default(),
    connectionPayload: Option[Json] = None,
    ackTimeoutMillis: Long = 10000L,
    idleTimeoutMillis: Long = 60000L,
    reconnectWhen: (ApolloException, Long) => Boolean = WebSocketNetworkTransport.reconnectNever,
    backoff: WsBackoff = WsBackoff.default
):
    import WebSocketNetworkTransport.*

    private given Frame              = Frame.internal
    private given AllowUnsafe        = AllowUnsafe.embrace.danger
    private given CanEqual[Msg, Msg] = CanEqual.derived

    // ---- owner fiber + mailbox (started eagerly, parked until first command) ---

    private val idCounter    = new AtomicLong(0L)
    private val stateRef     = new AtomicReference[State](State.initial)
    private val ownerStarted = new AtomicReference[Boolean](false)

    // The mailbox is a plain data structure (no clock dependency), so it is created
    // eagerly. The owner fiber, however, is forked lazily on first subscribe from
    // WITHIN the effect runtime — never via an unsafe eval at construction — so it
    // inherits the caller's context (notably a `Clock.withTimeControl` clock in
    // tests), which its ack/idle/backoff timer fibers then transitively inherit.
    private val mailbox: Channel[Msg] =
        Sync.Unsafe.evalOrThrow(Channel.initUnscoped[Msg](Int.MaxValue))

    // Completed by the owner fiber once a Shutdown has actually torn the socket down. `close()` only
    // ENQUEUES the shutdown, so without this a caller has no way to know when the socket is gone: the
    // Scope release returned while the owner fiber had not yet reached the command, which made "the
    // transport is closed when the block completes" a race the caller happened to win.
    private val shutdownDone: Fiber.Promise.Unsafe[Unit, Any] =
        Sync.Unsafe.evalOrThrow(Sync.Unsafe.defer(Fiber.Promise.Unsafe.init[Unit, Any]()))

    private def ensureStarted(using Frame): Unit < Async =
        Sync.defer(ownerStarted.compareAndSet(false, true)).map { firstStart =>
            val eff: Unit < Async =
                if firstStart then Fiber.initUnscoped(mailbox.streamUntilClosed().foreach(handle)).unit
                else ()
            eff
        }

    // ---- public API -----------------------------------------------------------

    /** A cold [[ResponseStream]] for `request`'s subscription. Consuming it (within
      * an `Async & Scope` context) registers the operation on the shared socket:
      * server frames are decoded and pushed into a per-subscription [[Channel]]
      * whose `streamUntilClosed` drives the stream, and the enclosing `Scope`'s
      * teardown sends the protocol stop for this operation and closes the channel.
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
        val id = idCounter.getAndIncrement().toString
        Stream.unwrap {
            ensureStarted.andThen(Channel.initUnscoped[ApolloResponse[D]](Int.MaxValue)).map { channel =>
                given AllowUnsafe = AllowUnsafe.embrace.danger
                val body          = OperationRequestBody(request.operation)
                val startFrame    = protocol.startOperation(id, body)
                val subscriber = Subscriber(
                    id = id,
                    startFrame = startFrame,
                    emitData = payload => discard(channel.unsafe.offer(decodeData(request, payload))),
                    emitErrorPayload = payload => discard(channel.unsafe.offer(errorResponse(request, payload))),
                    emitException = exception => discard(channel.unsafe.offer(exceptionResponse(request, exception))),
                    // Graceful close, not a hard `close`: a plain close hands the channel's
                    // backlog to the closer, dropping it for the consumer. On JS's single
                    // carrier the owner fiber runs the last `offer(response)` and this
                    // `terminate` (a server `complete` right after the last `next`) before
                    // the consumer drains, so a hard close would drop that final response.
                    // `closeAwaitEmpty` instead closes only once the consumer has drained
                    // the buffer, cooperating with `streamUntilClosed`'s own drain. One
                    // layer down, the engines' socket channels sidestep the same trap with
                    // an `Absent` end-marker (`WebSocketConnection.untilEnd`), so every
                    // frame the socket received before its close reaches `openLoop`'s drain.
                    terminate = () => discard(channel.unsafe.closeAwaitEmpty())
                )
                // Install the teardown finalizer BEFORE registering the route, then register.
                // Registering first would leave a window: an interrupt landing between the
                // Register offer and the ensure installation strands the route forever — the
                // owner adds it, but no Cancel is ever offered and the channel is never closed,
                // so the socket never idle-closes (routes never empties) and the ghost route is
                // resubscribed on every reconnect. With ensure first, an interrupt before the
                // Register makes the finalizer's Cancel a harmless no-op (onCancel ignores an
                // unknown id) and after it cleans the route up — cleanup is always paired.
                // Not unit-tested by design: reproducing the interrupt precisely in this window
                // (which has no natural suspension point) would require a production test seam,
                // and this ordering is correct by construction.
                Scope
                    .ensure(Sync.defer {
                        given AllowUnsafe = AllowUnsafe.embrace.danger
                        discard(mailbox.unsafe.offer(Msg.Cancel(id)))
                        discard(channel.unsafe.close())
                    })
                    .andThen(Sync.defer {
                        given AllowUnsafe = AllowUnsafe.embrace.danger
                        discard(mailbox.unsafe.offer(Msg.Register(subscriber)))
                    })
                    .andThen(channel.streamUntilClosed())
            }
        }
    end subscribe

    /** Tear the transport down: close the shared socket, cancel any pending
      * reconnect, and drop all state. Active subscribers receive the close as a
      * terminal [[ApolloWebSocketClosedException]] value.
      */
    def close(): Unit =
        given AllowUnsafe = AllowUnsafe.embrace.danger
        discard(mailbox.unsafe.offer(Msg.Shutdown))
    end close

    /** Enqueue the shutdown and wait for the owner fiber to have completed it: on return the socket is
      * closed and every route has seen its terminal value. A transport that was never started has no owner
      * fiber to run the command, so it completes the promise itself.
      */
    def closeAndAwait(using Frame): Unit < Async =
        Sync.defer {
            given AllowUnsafe = AllowUnsafe.embrace.danger
            discard(mailbox.unsafe.offer(Msg.Shutdown))
            if !ownerStarted.get() then shutdownDone.completeUnitDiscard()
        }.andThen(shutdownDone.safe.get)
    end closeAndAwait

    // ---- owner-fiber command handling (single-threaded by construction) --------

    private def handle(msg: Msg): Unit < Async =
        val s = stateRef.get()
        msg match
            case Msg.Register(sub)          => onRegister(s, sub)
            case Msg.Cancel(id)             => onCancel(s, id)
            case Msg.Frame(gen, text)       => if gen == s.generation then dispatch(s, protocol.parse(text)) else ()
            case Msg.Opened(gen, conn)      => onOpened(s, gen, conn)
            case Msg.OpenFailed(gen, cause) => if gen == s.generation then handshakeFailure(s, cause) else ()
            case Msg.SocketClosed(gen, cause) =>
                if gen == s.generation then onSocketClosed(s, cause) else ()
            case Msg.AckTimeout(gen) =>
                if gen == s.generation && s.awaitingAck then handshakeFailure(s, ackTimeoutException) else ()
            case Msg.IdleTimeout(token) =>
                if token == s.idleToken && s.routes.isEmpty then terminate(s, normalClose) else ()
            case Msg.DoReconnect(token) => if token == s.reconnectToken then doReconnect(s) else ()
            case Msg.Shutdown =>
                terminate(s, normalClose).andThen(closeMailbox).andThen(Sync.Unsafe.defer {
                    given AllowUnsafe = AllowUnsafe.embrace.danger
                    shutdownDone.completeUnitDiscard()
                })
        end match
    end handle

    // Closing the mailbox ends the owner fiber's `streamUntilClosed` loop; run only
    // after Shutdown's terminate so subscribers are notified first.
    private def closeMailbox: Unit < Async =
        Sync.defer(discard(mailbox.unsafe.close()))

    private def onRegister(s: State, sub: Subscriber): Unit < Async =
        val s1 = cancelIdleTimer(s).copy(routes = s.routes.updated(sub.id, sub))
        if s1.established then
            stateRef.set(s1)
            s1.conn.map(_.send(sub.startFrame)).getOrElse(())
        else if s1.awaitingAck || s1.reconnecting then
            // A handshake (or reconnect) is in flight; the start frame is sent for
            // every route once connection_ack lands.
            stateRef.set(s1)
        else
            // Idle: open the socket; the start frame is sent on ack.
            connect(s1)
        end if
    end onRegister

    private def onCancel(s: State, id: String): Unit < Async =
        s.routes.get(id) match
            case None => ()
            case Some(_) =>
                val sendStop: Unit < Async =
                    if s.established then sendVia(s.conn, protocol.stopOperation(id)) else ()
                sendStop.andThen(removeRoute(s, id))
    end onCancel

    /** Route one decoded server frame to its handler. */
    private def dispatch(s: State, message: WsMessage): Unit < Async = message match
        case WsMessage.ConnectionAck(_) => onAck(s)
        case WsMessage.ConnectionError(payload) =>
            terminate(
                s,
                ApolloNetworkException(
                    payload.fold("WebSocket connection rejected by server")(p =>
                        s"WebSocket connection rejected: ${p.render}"
                    )
                )
            )
        case WsMessage.Ping(_) =>
            protocol.pong() match
                case Some(text) => s.conn.map(_.send(text)).getOrElse(())
                case None       => ()
        case WsMessage.Pong(_) | WsMessage.KeepAlive => ()
        case WsMessage.Data(id, payload) =>
            s.routes.get(id).foreach(_.emitData(payload)); ()
        case WsMessage.Error(id, payload) =>
            s.routes.get(id) match
                case Some(sub) => sub.emitErrorPayload(payload); sub.terminate(); removeRoute(s, id)
                case None      => ()
        case WsMessage.Complete(id) =>
            s.routes.get(id) match
                case Some(sub) => sub.terminate(); removeRoute(s, id)
                case None      => ()
        case WsMessage.Unknown(_) => ()

    /** `connection_ack` arrived: cancel the ack timer, mark the socket established,
      * reset the reconnect counter, and (re)send the start frame for every route —
      * covering both freshly-pending subscriptions and a reconnect's resubscribe.
      */
    private def onAck(s: State): Unit < Async =
        val s1 = cancelAckTimer(s).copy(
            awaitingAck = false,
            established = true,
            reconnecting = false,
            reconnectAttempt = 0L
        )
        stateRef.set(s1)
        s1.conn match
            case Absent => ()
            case Present(conn) =>
                Kyo.foreachDiscard(s1.routes.values.toSeq)(sub => conn.send(sub.startFrame))
        end match
    end onAck

    /** The socket closed. `cause` is `Absent` on a clean `1000` close, or the
      * drop's exception on an abnormal close. A failure around the handshake (still
      * awaiting ack) becomes a [[handshakeFailure]]; an established socket's drop
      * either reconnects (when [[reconnectWhen]] agrees and subscriptions remain)
      * or terminates them.
      */
    private def onSocketClosed(s: State, cause: Maybe[ApolloException]): Unit < Async =
        if s.awaitingAck then handshakeFailure(s, cause.getOrElse(normalClose))
        else
            cause match
                case Present(drop) if s.routes.nonEmpty && reconnectWhen(drop, s.reconnectAttempt + 1) =>
                    val s1 = clearSocket(s)
                    stateRef.set(s1)
                    s1.routes.values.foreach(_.emitException(drop))
                    scheduleReconnect(s1)
                case _ =>
                    terminate(s, cause.getOrElse(normalClose))
    end onSocketClosed

    // ---- connection lifecycle -------------------------------------------------

    /** Open a socket in a dedicated fiber and drive its handshake. The fiber holds
      * the connection's `Scope` open (via `Async.never`) plus the drain-then-watch
      * fiber; interrupting it (on discard) tears both down.
      */
    private def connect(s: State): Unit < Async =
        val gen = s.generation + 1
        Fiber
            .initUnscoped(openLoop(gen))
            .map { fiber =>
                stateRef.set(s.copy(
                    generation = gen,
                    conn = Absent,
                    connFiber = Present(fiber.unsafeWiden),
                    awaitingAck = true,
                    established = false
                ))
            }
    end connect

    private def openLoop(gen: Long): Unit < Async =
        Scope.run {
            Abort.run[Throwable](engine.open(serverUrl, Some(protocol.name))).map {
                case Result.Success(conn) =>
                    // Drain `incoming` to its end, THEN watch `closed` — in one fiber, so
                    // `SocketClosed` is enqueued strictly after every `Frame` the socket
                    // delivered. `incoming` ends exactly when the socket closes (the
                    // engines put an end-marker after their last frame), so nothing is
                    // lost by waiting. Two independent fibers would race: a `SocketClosed`
                    // that overtakes the last `next` + `complete` terminates the routes
                    // and bumps the generation, fencing those frames out as stale.
                    Fiber.init(drainThenWatchClosed(gen, conn)).andThen {
                        offer(Msg.Opened(gen, conn)).andThen(Async.never)
                    }
                case Result.Failure(cause) => offer(Msg.OpenFailed(gen, toApolloException(cause)))
                case Result.Panic(cause)   => offer(Msg.OpenFailed(gen, toApolloException(cause)))
            }
        }

    private def drainThenWatchClosed(gen: Long, conn: WebSocketConnection): Unit < Async =
        conn.incoming.foreach(text => offer(Msg.Frame(gen, text))).andThen(watchClosed(gen, conn))

    private def watchClosed(gen: Long, conn: WebSocketConnection): Unit < Async =
        Abort.run[ApolloWebSocketClosedException](conn.closed).map {
            case Result.Success(_)      => offer(Msg.SocketClosed(gen, Absent))
            case Result.Failure(closed) => offer(Msg.SocketClosed(gen, Present(closed)))
            case Result.Panic(cause)    => offer(Msg.SocketClosed(gen, Present(toApolloException(cause))))
        }

    private def onOpened(s: State, gen: Long, conn: WebSocketConnection): Unit < Async =
        if gen != s.generation then conn.close() // superseded before it opened — discard it
        else
            conn.send(protocol.connectionInit(connectionPayload)).andThen {
                Fiber.initUnscoped(Async.delay(ackTimeoutMillis.millis)(offer(Msg.AckTimeout(gen)))).map { timer =>
                    stateRef.set(s.copy(conn = Present(conn), ackTimer = Present(timer.unsafeWiden)))
                }
            }
    end onOpened

    // ---- reconnection ---------------------------------------------------------

    private def scheduleReconnect(s: State): Unit < Async =
        val attempt = s.reconnectAttempt + 1
        val token   = s.reconnectToken + 1
        val s1      = cancelReconnectTimer(cancelIdleTimer(s))
        Fiber
            .initUnscoped(Async.delay(backoff.delayMillis(attempt).millis)(offer(Msg.DoReconnect(token))))
            .map { timer =>
                stateRef.set(s1.copy(
                    reconnectAttempt = attempt,
                    reconnectToken = token,
                    reconnecting = true,
                    reconnectTimer = Present(timer.unsafeWiden)
                ))
            }
    end scheduleReconnect

    private def doReconnect(s: State): Unit < Async =
        val s1 = s.copy(reconnectTimer = Absent)
        if s1.routes.isEmpty then
            stateRef.set(s1.copy(reconnecting = false))
            finishTeardown(s1)
        else connect(s1)
        end if
    end doReconnect

    /** A reconnect attempt failed to re-establish: retry (honoring [[reconnectWhen]]
      * for the next attempt) or give up and terminate every subscription.
      */
    private def reconnectFailed(s: State, cause: ApolloException): Unit < Async =
        if s.routes.isEmpty then finishTeardown(s)
        else if reconnectWhen(cause, s.reconnectAttempt + 1) then scheduleReconnect(s)
        else terminate(s, cause)

    // ---- teardown helpers -----------------------------------------------------

    /** A failure before/around a handshake ack. Discards the socket; if this was a
      * reconnect attempt, route to [[reconnectFailed]]; otherwise (initial
      * handshake) terminate the pending subscriptions with the failure value.
      */
    private def handshakeFailure(s: State, exception: ApolloException): Unit < Async =
        val s1 = discardActiveSocket(cancelAckTimer(s))
        if s1.reconnecting then reconnectFailed(s1, exception)
        else terminate(s1, exception)
    end handshakeFailure

    /** Full terminal shutdown: cancel timers, close + discard the socket, terminate
      * every subscription with the value, and reset to idle.
      */
    private def terminate(s: State, exception: ApolloException): Unit < Async =
        val closeConn: Unit < Async = s.conn match
            case Present(c) => c.close(closeCodeOf(exception))
            case Absent     => ()
        closeConn.andThen {
            interruptAll(s).map { _ =>
                s.routes.values.foreach { sub =>
                    sub.emitException(exception)
                    sub.terminate()
                }
                stateRef.set(State.initial.copy(generation = s.generation + 1))
            }
        }
    end terminate

    /** Interrupt + close the current socket (unresponsive handshake); the next
      * connect bumps the generation, fencing its late events.
      */
    private def discardActiveSocket(s: State): State =
        s.conn.foreach(c => Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(c.close()).unit))
        interruptFiber(s.connFiber)
        s.copy(conn = Absent, connFiber = Absent, generation = s.generation + 1, awaitingAck = false)
    end discardActiveSocket

    /** Drop the socket mechanics but keep [[State.routes]] for resubscribe; the
      * socket has already closed, so no explicit close is sent.
      */
    private def clearSocket(s: State): State =
        interruptFiber(s.connFiber)
        cancelAckTimer(s).copy(
            conn = Absent,
            connFiber = Absent,
            established = false,
            awaitingAck = false,
            generation = s.generation + 1
        )
    end clearSocket

    private def finishTeardown(s: State): Unit < Async =
        interruptAll(s).map(_ => stateRef.set(State.initial.copy(generation = s.generation + 1)))

    private def removeRoute(s: State, id: String): Unit < Async =
        val cur     = stateRef.get()
        val routes1 = cur.routes.removed(id)
        val s1      = cur.copy(routes = routes1)
        if routes1.nonEmpty then
            stateRef.set(s1)
            ()
        else
            s1.conn match
                case Present(_) =>
                    // A live socket: hold it open for the idle window, then close.
                    scheduleIdleClose(cancelReconnectTimer(s1))
                case Absent =>
                    // No live socket — a handshake is still opening or a reconnect
                    // backoff is armed, both existing only to (re)serve routes that are
                    // now gone. Reset straight to idle: finishTeardown interrupts the
                    // connect fiber and every timer (including the reconnect backoff)
                    // and clears `reconnecting`. Routing through scheduleIdleClose would
                    // instead cancel the reconnect timer but leave `reconnecting = true`
                    // with no timer, stranding the machine — the next subscribe would
                    // then wait forever on a connection_ack that never arrives.
                    finishTeardown(s1)
        end if
    end removeRoute

    private def scheduleIdleClose(s: State): Unit < Async =
        s.conn match
            case Absent => stateRef.set(s) // no live socket to idle-close
            case Present(_) =>
                val token = s.idleToken + 1
                val s1    = cancelIdleTimer(s)
                Fiber.initUnscoped(Async.delay(idleTimeoutMillis.millis)(offer(Msg.IdleTimeout(token)))).map { timer =>
                    stateRef.set(s1.copy(idleToken = token, idleTimer = Present(timer.unsafeWiden)))
                }
    end scheduleIdleClose

    // ---- timer cancellation (pure state updates; interrupt is fire-and-forget) -

    private def cancelAckTimer(s: State): State =
        interruptFiber(s.ackTimer); s.copy(ackTimer = Absent)

    private def cancelIdleTimer(s: State): State =
        interruptFiber(s.idleTimer); s.copy(idleTimer = Absent)

    private def cancelReconnectTimer(s: State): State =
        interruptFiber(s.reconnectTimer); s.copy(reconnectTimer = Absent)

    private def interruptAll(s: State): Unit < Async =
        interruptFiber(s.ackTimer)
        interruptFiber(s.idleTimer)
        interruptFiber(s.reconnectTimer)
        interruptFiber(s.connFiber)
        ()
    end interruptAll

    private def interruptFiber(fiber: Maybe[Fiber[Unit, Any]]): Unit =
        fiber.foreach(f => discard(Sync.Unsafe.evalOrThrow(f.interrupt)))

    /** Send `text` over `conn` if present; a no-op when absent. */
    private def sendVia(conn: Maybe[WebSocketConnection], text: String): Unit < Async =
        conn match
            case Present(c) => c.send(text)
            case Absent     => ()

    private def offer(msg: Msg): Unit < Async =
        Abort.run[Closed](mailbox.offer(msg)).unit

    // ---- response decoding (pure; per-subscription D captured at subscribe) ----

    // Decoding runs inside the owner fiber's routing closures, which every subscription
    // on the socket shares: a decoder defect is folded into this subscription's value
    // (with the defect as `cause`) rather than raised, so it cannot stop the owner loop.
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

    private def toApolloException(cause: Throwable): ApolloException = cause match
        case exception: ApolloException => exception
        case other                      => ApolloNetworkException(cause = other)
end WebSocketNetworkTransport

object WebSocketNetworkTransport:

    /** Never reopen a dropped socket: the drop terminates its subscriptions with
      * the [[ApolloWebSocketClosedException]] value. The default, matching
      * apollo-kotlin's opt-in `reopenWhen`.
      */
    val reconnectNever: (ApolloException, Long) => Boolean = (_, _) => false

    /** Always reopen on a drop, regardless of cause or attempt count (the
      * [[WsBackoff]] still paces the attempts).
      */
    val reconnectAlways: (ApolloException, Long) => Boolean = (_, _) => true

    private def normalClose(using Frame): ApolloWebSocketClosedException =
        ApolloWebSocketClosedException(WebSocketConnection.NormalClosure)

    private def ackTimeoutException(using Frame): ApolloNetworkException =
        ApolloNetworkException(s"Timed out waiting for connection_ack")

    private def closeCodeOf(exception: ApolloException): Int = exception match
        case e: ApolloWebSocketClosedException => e.code
        case _                                 => WebSocketConnection.NormalClosure

    /** One live subscriber's per-`D` reactions, captured at [[subscribe]] time and
      * invoked by the owner fiber. The closures push into the subscription's
      * channel via `channel.unsafe.*`; route bookkeeping stays owner-side.
      */
    final private class Subscriber(
        val id: String,
        val startFrame: String,
        val emitData: Json => Unit,
        val emitErrorPayload: Json => Unit,
        val emitException: ApolloException => Unit,
        val terminate: () => Unit
    )

    /** The owner fiber's command mailbox alphabet. Connection-scoped commands carry
      * the `generation` of the socket that produced them for fencing.
      */
    private enum Msg derives CanEqual:
        case Register(sub: Subscriber)
        case Cancel(id: String)
        case Frame(gen: Long, text: String)
        case Opened(gen: Long, conn: WebSocketConnection)
        case OpenFailed(gen: Long, cause: ApolloException)
        case SocketClosed(gen: Long, cause: Maybe[ApolloException])
        case AckTimeout(gen: Long)
        case IdleTimeout(token: Long)
        case DoReconnect(token: Long)
        case Shutdown
    end Msg

    /** The whole transport state, mutated only by the owner fiber. */
    final private case class State(
        generation: Long,
        routes: VectorMap[String, Subscriber],
        conn: Maybe[WebSocketConnection],
        connFiber: Maybe[Fiber[Unit, Any]],
        awaitingAck: Boolean,
        established: Boolean,
        reconnecting: Boolean,
        reconnectAttempt: Long,
        reconnectToken: Long,
        idleToken: Long,
        ackTimer: Maybe[Fiber[Unit, Any]],
        idleTimer: Maybe[Fiber[Unit, Any]],
        reconnectTimer: Maybe[Fiber[Unit, Any]]
    )

    private object State:
        val initial: State = State(
            generation = 0L,
            routes = VectorMap.empty,
            conn = Absent,
            connFiber = Absent,
            awaitingAck = false,
            established = false,
            reconnecting = false,
            reconnectAttempt = 0L,
            reconnectToken = 0L,
            idleToken = 0L,
            ackTimer = Absent,
            idleTimer = Absent,
            reconnectTimer = Absent
        )
    end State

    extension [A, S](fiber: Fiber[A, S])
        /** Widen a fiber's phantom parameters for uniform storage in [[State]]. */
        private def unsafeWiden: Fiber[Unit, Any] = fiber.asInstanceOf[Fiber[Unit, Any]]
end WebSocketNetworkTransport
