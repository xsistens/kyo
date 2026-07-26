package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.api.OperationRequestBody
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.control.NonFatal

/** The terminal transport for **subscription** operations: multiplexes many
  * long-lived subscriptions over a single shared [[WebSocketConnection]].
  *
  * This is the WebSocket analog of [[kyo.apollo.network.http.HttpNetworkTransport]].
  * Where the HTTP transport runs one request/one response, this one opens a
  * socket lazily on the first subscription, performs the `connection_init` /
  * `connection_ack` handshake once, then routes every server frame to the right
  * subscriber by the operation id it assigned. Each call to [[subscribe]]
  * returns a cold, long-lived `Flow[ApolloResponse[D]]`; nothing touches the
  * socket until that flow is collected/subscribed (matching the Phase 05
  * cold-stream contract), and cancelling a subscriber sends the protocol
  * `stop`/`complete` for its id.
  *
  * Lifecycle, top to bottom:
  *   - **Lazy open + handshake.** The first active subscription opens the socket
  *     through the [[WebSocketEngine]], sends [[WsProtocol.connectionInit]] (with
  *     the optional [[connectionPayload]] for auth), and waits for
  *     [[WsMessage.ConnectionAck]]. If no ack arrives within
  *     [[ackTimeoutMillis]], the handshake fails and every pending subscription
  *     receives that failure as a value.
  *   - **Multiplexing.** Every subscription gets a unique integer id;
  *     [[WsMessage.Data]] / [[WsMessage.Error]] / [[WsMessage.Complete]] frames
  *     are dispatched to the one subscriber that owns their id, so two
  *     subscriptions share one socket without interfering.
  *   - **Keepalive.** A server [[WsMessage.Ping]] is answered with
  *     [[WsProtocol.pong]] when the protocol defines one; a [[WsMessage.Pong]] or
  *     legacy [[WsMessage.KeepAlive]] is observed as a liveness signal only.
  *   - **Idle close.** When the last subscription ends, the socket is closed
  *     after [[idleTimeoutMillis]] of no subscriptions; a new subscription inside
  *     that window cancels the pending close and reuses the socket.
  *
  * **Failures are values** (the Phase 03 contract): a socket drop or a terminal
  * server close surfaces as `ApolloResponse.exception` — an
  * [[ApolloWebSocketClosedException]] — pushed to every active subscriber; the
  * `Flow` itself does not fail for those conditions.
  *
  * **Automatic reconnection** (Phase 06 Task 5) is layered on top: when an
  * *established* socket drops abnormally and [[reconnectWhen]] agrees, the drop
  * is surfaced to every active subscriber as an [[ApolloWebSocketClosedException]]
  * value (the *resubscription signal* — callers can refetch any state they may
  * have missed), and the transport transparently reopens the socket after a
  * [[backoff]] delay, re-runs the `connection_init` handshake, and resubscribes
  * every still-active subscription under its original operation id. The attempt
  * counter resets to zero once a reconnection re-acknowledges, so a socket that
  * flaps briefly does not inherit an old outage's backoff. When [[reconnectWhen]]
  * declines (or a reconnection cycle exhausts), the drop becomes *terminal*: each
  * subscription receives the `ApolloWebSocketClosedException` value and then
  * completes. A clean `1000` close (idle timeout, [[close]]) is always terminal
  * and never reconnects.
  *
  * Single-threaded by construction: JS has no real concurrency, so the routing
  * table and connection state are plain mutable fields mutated only from the
  * event loop. A monotonic `generation` counter fences a socket that has been
  * deliberately discarded (ack timeout, reconnect) so its late `close`/`message`
  * callbacks are ignored rather than re-entering the state machine.
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
  * @param scheduler         the timer seam for the ack/idle/backoff timeouts
  *                          (tests inject a manual one)
  * @param reconnectWhen     decides, from the drop's exception and the 1-based
  *                          attempt number, whether to reopen a dropped socket;
  *                          defaults to [[WebSocketNetworkTransport.reconnectNever]]
  *                          (opt-in, matching apollo-kotlin's `reopenWhen`)
  * @param backoff           how long to wait before each reconnection attempt
  */
final class WebSocketNetworkTransport(
    serverUrl: String,
    protocol: WsProtocol = GraphQLWsProtocol,
    engine: WebSocketEngine = JsWebSocketEngine(),
    connectionPayload: Option[Json] = None,
    ackTimeoutMillis: Long = 10000L,
    idleTimeoutMillis: Long = 60000L,
    scheduler: WsScheduler = WsScheduler.default,
    reconnectWhen: (ApolloException, Long) => Boolean = WebSocketNetworkTransport.reconnectNever,
    backoff: WsBackoff = WsBackoff.default
):

    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    /** One live subscriber's reaction to the frames it owns.
      *
      * `onMessage` handles the operation-scoped frames ([[WsMessage.Data]],
      * [[WsMessage.Error]], [[WsMessage.Complete]]); `onTerminate` delivers a
      * *terminal* connection-level failure (a drop we will not reconnect, an ack
      * timeout) as a value and ends the subscription; `onDrop` delivers a
      * *recoverable* drop as a value (the resubscription signal) **without** ending
      * the subscription, so streaming resumes after the reconnect; `resend`
      * re-issues this subscription's start frame on a freshly reopened socket.
      */
    final private class Subscriber(
        val onMessage: WsMessage => Unit,
        val onTerminate: ApolloException => Unit,
        val onDrop: ApolloException => Unit,
        val resend: WebSocketConnection => Unit
    )

    // ---- mutable connection + routing state (event-loop-confined) -------------

    /** Memoized handshake: `Some` once an open is in flight or established, so all
      * subscriptions share the one socket. Reset to `None` on any terminal close
      * so the next subscription reopens.
      */
    private var handshake: Maybe[Future[WebSocketConnection]]   = Absent
    private var active: Maybe[WebSocketConnection]              = Absent
    private var pendingAck: Maybe[Promise[WebSocketConnection]] = Absent
    private var ackTimer: Maybe[() => Unit]                     = Absent
    private var idleTimer: Maybe[() => Unit]                    = Absent
    private var reconnectTimer: Maybe[() => Unit]               = Absent

    /** `Some` while a reconnect cycle is in flight (backoff pending or attempt
      * running). It doubles as the shared [[handshake]] future during that window
      * so subscriptions started mid-reconnect wait for the reopen rather than
      * racing a second socket open; it resolves when the reconnect re-acknowledges.
      */
    private var reconnectPromise: Maybe[Promise[WebSocketConnection]] = Absent

    /** 1-based reconnection attempt count, driving [[backoff]]; reset to zero on a
      * successful re-acknowledgement and on any full teardown.
      */
    private var reconnectAttempt: Long = 0L

    /** Bumped whenever a socket is deliberately abandoned; async callbacks captured
      * against an older generation become no-ops, so a discarded socket's late
      * `close`/`message` never re-enters the state machine.
      */
    private var generation: Long = 0L

    /** id → subscriber, populated only once a subscription's start frame has been
      * sent (i.e. after the handshake). Entries survive a recoverable drop so the
      * subscription can be resubscribed on the reopened socket; server frames route
      * to a live operation by this id both before and after a reconnect.
      */
    private val routes       = mutable.Map.empty[String, Subscriber]
    private var nextId: Long = 0L

    /** A cold [[ResponseStream]] for `request`'s subscription. Consuming it (within
      * an `Async & Scope` context) registers the operation on the shared socket:
      * server frames are pushed into a per-subscription [[Channel]] whose
      * `streamUntilClosed` drives the stream, and the enclosing `Scope`'s teardown
      * sends the protocol stop for this operation, drops its routing entry, and
      * closes the channel (ending the stream). The subscription's own natural end
      * (server `complete`/`error`, terminal drop) closes the channel too.
      *
      * The transport's internal Future/Promise/callback state machine is unchanged;
      * this only bridges its `emit` callback boundary into a Kyo `Channel` (the
      * `channel.unsafe.offer` from the event-loop callback is the idiomatic interop
      * point, matching `kyo-ui`'s reactive bridge).
      */
    def subscribe[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        Stream.unwrap {
            Channel.initUnscoped[ApolloResponse[D]](Int.MaxValue).map { channel =>
                given AllowUnsafe = AllowUnsafe.embrace.danger
                val cancel = register(
                    request,
                    resp =>
                        val _ = channel.unsafe.offer(resp)
                    ,
                    () =>
                        val _ = channel.unsafe.close()
                )
                Scope
                    .ensure(Sync.defer {
                        given AllowUnsafe = AllowUnsafe.embrace.danger
                        cancel()
                        val _ = channel.unsafe.close()
                    })
                    .andThen(channel.streamUntilClosed())
            }
        }

    /** Tear the transport down: close the shared socket (if any), cancel any
      * pending reconnect, and drop all state. The [[kyo.apollo.ApolloClient.close]] seam
      * calls this. Active subscribers receive the close as a terminal
      * [[ApolloWebSocketClosedException]] value.
      */
    def close(): Unit =
        terminate(ApolloWebSocketClosedException(WebSocketConnection.NormalClosure))

    // ---- registration ---------------------------------------------------------

    /** Register one subscription and return its cancel thunk. `onFinish` fires once
      * when the subscription terminates for any reason (server `complete`/`error`,
      * socket drop, or caller cancel).
      */
    private def register[D](
        request: ApolloRequest[D],
        emit: ApolloResponse[D] => Unit,
        onFinish: () => Unit
    ): () => Unit =
        val id         = allocateId()
        var finished   = false
        var registered = false

        // The start frame is fixed for this operation, so precompute it once and
        // reuse it for both the initial send and any reconnection resubscribe.
        val body       = OperationRequestBody(request.operation)
        val startFrame = protocol.startOperation(id, body)

        def finish(): Unit =
            if !finished then
                finished = true
                if registered then discard(routes.remove(id))
                registered = false
                onFinish()
                if routes.isEmpty then
                    cancelReconnect()
                    scheduleIdleClose()

        val subscriber = new Subscriber(
            onMessage = {
                case WsMessage.Data(_, payload) => emit(decodeData(request, payload))
                case WsMessage.Error(_, payload) =>
                    emit(errorResponse(request, payload)); finish()
                case WsMessage.Complete(_) => finish()
                case _                     => ()
            },
            onTerminate = exception =>
                emit(exceptionResponse(request, exception));
                finish()
            ,
            // A recoverable drop is surfaced as a value but does not end the stream:
            // the transport resubscribes and data resumes on the reopened socket.
            onDrop = exception => emit(exceptionResponse(request, exception)),
            resend = connection => connection.send(startFrame)
        )

        // A fresh subscription cancels any pending idle close so the socket is reused.
        cancelIdleClose()

        ensureConnection().onComplete {
            case Success(connection) =>
                if !finished then
                    registered = true
                    routes(id) = subscriber
                    connection.send(startFrame)
            case Failure(cause) =>
                if !finished then subscriber.onTerminate(toApolloException(cause))
        }

        () =>
            if !finished then
                if registered then active.foreach(_.send(protocol.stopOperation(id)))
                finish()
    end register

    private def allocateId(): String =
        val id = nextId
        nextId += 1
        id.toString
    end allocateId

    // ---- shared connection + handshake ----------------------------------------

    /** The shared socket, opened + handshaken on first use and memoized after.
      * During a reconnect cycle the memoized future is the pending reconnect, so a
      * subscription started mid-outage waits for the reopen instead of racing it.
      */
    private def ensureConnection(): Future[WebSocketConnection] =
        handshake match
            case Present(future) => future
            case Absent =>
                val future = connect()
                handshake = Present(future)
                future

    /** Open a socket, wire up its incoming frames, send `connection_init`, and
      * complete once `connection_ack` arrives (or fail on ack timeout / open
      * failure). Manages only the socket mechanics + [[pendingAck]]; the caller
      * (initial [[ensureConnection]] or a [[reconnect]] attempt) owns what the
      * returned future's outcome means. Every async callback is fenced by the
      * [[generation]] captured here, so a socket abandoned before its callbacks run
      * cannot re-enter the state machine.
      */
    private def connect(): Future[WebSocketConnection] =
        val gen = generation
        val ack = Promise[WebSocketConnection]()
        pendingAck = Present(ack)
        engine.open(serverUrl, Some(protocol.name)).onComplete {
            case Success(connection) =>
                if gen == generation then
                    active = Present(connection)
                    // The single incoming sink; the `closed` future is the drop/close signal.
                    connection.incoming(text => if gen == generation then dispatch(protocol.parse(text)))
                    connection.closed.onComplete {
                        case Success(_) =>
                            if gen == generation then onSocketClosed(None)
                        case Failure(cause) =>
                            if gen == generation then onSocketClosed(Some(toApolloException(cause)))
                    }
                    connection.send(protocol.connectionInit(connectionPayload))
                    ackTimer = Present(
                        scheduler.schedule(ackTimeoutMillis)(() => if gen == generation then onAckTimeout())
                    )
                else connection.close() // superseded before it opened — discard it
            case Failure(cause) =>
                if gen == generation then handleHandshakeFailure(toApolloException(cause))
        }
        ack.future
    end connect

    /** Route one decoded server frame to its handler. */
    private def dispatch(message: WsMessage): Unit = message match
        case WsMessage.ConnectionAck(_) =>
            cancelAckTimer()
            for connection <- active; promise <- pendingAck do discard(promise.trySuccess(connection))
            pendingAck = Absent
        case WsMessage.ConnectionError(payload) =>
            val exception = ApolloNetworkException(
                payload.fold("WebSocket connection rejected by server")(p =>
                    s"WebSocket connection rejected: ${p.render}"
                )
            )
            // A server rejection is terminal — never reconnect through it.
            terminate(exception)
        case WsMessage.Ping(_) =>
            protocol.pong().foreach(text => active.foreach(_.send(text)))
        case WsMessage.Pong(_) | WsMessage.KeepAlive =>
            () // liveness signal only
        case data: WsMessage.Data     => routes.get(data.id).foreach(_.onMessage(data))
        case error: WsMessage.Error   => routes.get(error.id).foreach(_.onMessage(error))
        case done: WsMessage.Complete => routes.get(done.id).foreach(_.onMessage(done))
        case WsMessage.Unknown(_)     => () // stray/future frame — ignored, stays total

    /** The socket closed. `cause` is `None` on a clean `1000` close, or the drop's
      * [[ApolloException]] on an abnormal close. A failure *around the handshake*
      * (before ack) is delegated to [[handleHandshakeFailure]] so the awaiter
      * reacts; an *established* socket's abnormal drop either reconnects (when
      * [[reconnectWhen]] agrees and subscriptions remain) or terminates them.
      */
    private def onSocketClosed(cause: Option[ApolloException]): Unit =
        if pendingAck.isDefined then
            handleHandshakeFailure(
                cause.getOrElse(ApolloWebSocketClosedException(WebSocketConnection.NormalClosure))
            )
        else
            cause match
                case Some(drop) if routes.nonEmpty && reconnectWhen(drop, reconnectAttempt + 1) =>
                    clearSocket()
                    // The resubscription signal: every active subscriber sees the drop as a
                    // value, then the transport reopens and resubscribes them.
                    routes.values.toList.foreach(_.onDrop(drop))
                    scheduleReconnect()
                case _ =>
                    terminate(
                        cause.getOrElse(ApolloWebSocketClosedException(WebSocketConnection.NormalClosure))
                    )

    /** `connection_ack` never arrived: discard the unresponsive socket and fail the
      * handshake so the awaiter (a pending subscription, or a reconnect attempt)
      * reacts.
      */
    private def onAckTimeout(): Unit =
        ackTimer = Absent
        handleHandshakeFailure(
            ApolloNetworkException(
                s"Timed out after ${ackTimeoutMillis}ms waiting for connection_ack"
            )
        )
    end onAckTimeout

    // ---- reconnection ---------------------------------------------------------

    /** Arm the next reconnection attempt on the [[backoff]] delay. Reuses the
      * shared [[reconnectPromise]] across attempts so subscriptions awaiting the
      * reopen stay parked until it succeeds (or the cycle gives up).
      */
    private def scheduleReconnect(): Unit =
        reconnectAttempt += 1
        cancelReconnect()
        cancelIdleClose()
        val promise = reconnectPromise.getOrElse {
            val fresh = Promise[WebSocketConnection]()
            reconnectPromise = Present(fresh)
            fresh
        }
        handshake = Present(promise.future)
        reconnectTimer = Present(
            scheduler.schedule(backoff.delayMillis(reconnectAttempt))(() => reconnect(promise))
        )
    end scheduleReconnect

    /** Reopen the socket for a reconnection attempt. On success, reset the attempt
      * counter, resolve the shared promise (unparking mid-outage subscriptions),
      * and resubscribe every still-active subscription under its original id.
      */
    private def reconnect(promise: Promise[WebSocketConnection]): Unit =
        reconnectTimer = Absent
        if routes.isEmpty then
            reconnectPromise = Absent
            finishTeardown() // everyone cancelled during backoff — nothing to reopen
        else
            connect().onComplete {
                case Success(connection) =>
                    reconnectAttempt = 0
                    reconnectPromise = Absent
                    handshake = Present(Future.successful(connection))
                    discard(promise.trySuccess(connection))
                    routes.values.toList.foreach(_.resend(connection))
                case Failure(reason) =>
                    onReconnectFailed(toApolloException(reason))
            }
        end if
    end reconnect

    /** A reconnection attempt failed to re-establish. Retry (honoring
      * [[reconnectWhen]] for the next attempt) or give up and terminate every
      * subscription (which also fails the shared [[reconnectPromise]]).
      */
    private def onReconnectFailed(cause: ApolloException): Unit =
        if routes.isEmpty then
            reconnectPromise = Absent
            finishTeardown()
        else if reconnectWhen(cause, reconnectAttempt + 1) then scheduleReconnect()
        else terminate(cause)

    // ---- teardown helpers -----------------------------------------------------

    /** A failure before/around a handshake ack. Discards the socket without
      * re-entry and fails [[pendingAck]] so the awaiter reacts: a pending
      * subscription terminates (and the next reopens, since [[handshake]] is
      * cleared), while a reconnect attempt drops through to [[onReconnectFailed]]
      * (its promise, and thus [[handshake]], stays pending).
      */
    private def handleHandshakeFailure(exception: ApolloException): Unit =
        cancelAckTimer() // a discarded socket's ack timer must not linger
        discardActiveSocket()
        val awaiter = pendingAck
        pendingAck = Absent
        if reconnectPromise.isEmpty then handshake = Absent
        awaiter.foreach(a => discard(a.tryFailure(exception)))
    end handleHandshakeFailure

    /** Terminate every currently-routed subscription with a terminal value.
      * Snapshots the routes first because `onTerminate` removes entries as it runs.
      */
    private def terminateAll(exception: ApolloException): Unit =
        routes.values.toList.foreach(_.onTerminate(exception))

    /** Full terminal shutdown: fail any pending handshake/reconnect, discard the
      * socket, terminate every subscription with the value, and zero all state.
      */
    private def terminate(exception: ApolloException): Unit =
        pendingAck.foreach(p => discard(p.tryFailure(exception)))
        reconnectPromise.foreach(p => discard(p.tryFailure(exception)))
        discardActiveSocket()
        terminateAll(exception)
        finishTeardown()
    end terminate

    /** Bump the [[generation]] and close the current socket so its late callbacks
      * are fenced out. Leaves routing/reconnect bookkeeping to the caller.
      */
    private def discardActiveSocket(): Unit =
        val socket = active
        generation += 1
        active = Absent
        socket.foreach(_.close())
    end discardActiveSocket

    /** Drop the socket mechanics but keep [[routes]] and the reconnect bookkeeping. */
    private def clearSocket(): Unit =
        active = Absent
        handshake = Absent
        pendingAck = Absent
        cancelAckTimer()
    end clearSocket

    /** Reset every field back to the initial idle state. */
    private def finishTeardown(): Unit =
        generation += 1
        active = Absent
        handshake = Absent
        pendingAck = Absent
        reconnectAttempt = 0
        reconnectPromise = Absent
        cancelAckTimer()
        cancelReconnect()
        cancelIdleClose()
    end finishTeardown

    private def cancelAckTimer(): Unit =
        ackTimer.foreach(_())
        ackTimer = Absent

    private def cancelReconnect(): Unit =
        reconnectTimer.foreach(_())
        reconnectTimer = Absent

    // ---- idle close -----------------------------------------------------------

    private def scheduleIdleClose(): Unit =
        cancelIdleClose()
        idleTimer = Present(scheduler.schedule(idleTimeoutMillis) { () =>
            idleTimer = Absent
            if routes.isEmpty then
                terminate(ApolloWebSocketClosedException(WebSocketConnection.NormalClosure))
        })
    end scheduleIdleClose

    private def cancelIdleClose(): Unit =
        idleTimer.foreach(_())
        idleTimer = Absent

    // ---- response decoding ----------------------------------------------------

    /** Decode a [[WsMessage.Data]] payload (`{ data, errors, extensions }`) into a
      * typed [[ApolloResponse]], folding a parse failure into an exception value.
      */
    private def decodeData[D](
        request: ApolloRequest[D],
        payload: Json
    ): ApolloResponse[D] =
        try
            val response = GraphQLResponse.parse(
                payload,
                request.operation
            )
            ApolloResponse.fromGraphQLResponse(
                request.requestUuid,
                response,
                request.executionContext
            )
        catch
            case NonFatal(cause) =>
                ApolloResponse.fromException(
                    request.requestUuid,
                    ApolloParseException(cause = cause),
                    request.executionContext
                )

    /** Turn a [[WsMessage.Error]] payload into a response carrying the GraphQL
      * errors. The modern protocol sends an array of errors; the legacy protocol a
      * single error object — both are surfaced faithfully.
      */
    private def errorResponse[D](
        request: ApolloRequest[D],
        payload: Json
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = request.requestUuid,
            data = Absent,
            errors = parseErrors(payload),
            executionContext = request.executionContext
        )

    private def parseErrors(payload: Json): Chunk[GraphQLError] = payload match
        case Json.JArr(items) => items.map(GraphQLError.parse)
        case obj: Json.JObj   => Chunk(GraphQLError.parse(obj))
        case _                => Chunk.empty

    private def exceptionResponse[D](
        request: ApolloRequest[D],
        exception: ApolloException
    ): ApolloResponse[D] =
        ApolloResponse.fromException(
            request.requestUuid,
            exception,
            request.executionContext
        )

    private def toApolloException(cause: Throwable): ApolloException = cause match
        case exception: ApolloException => exception
        case other                      => ApolloNetworkException(cause = other)
end WebSocketNetworkTransport

object WebSocketNetworkTransport:

    /** Never reopen a dropped socket: the drop terminates its subscriptions with
      * the [[ApolloWebSocketClosedException]] value. The default, matching
      * apollo-kotlin's opt-in `reopenWhen`; the client `Builder` exposes a hook to
      * override it (Task 6).
      */
    val reconnectNever: (ApolloException, Long) => Boolean = (_, _) => false

    /** Always reopen on a drop, regardless of the cause or attempt count (the
      * [[WsBackoff]] still paces the attempts).
      */
    val reconnectAlways: (ApolloException, Long) => Boolean = (_, _) => true
end WebSocketNetworkTransport
