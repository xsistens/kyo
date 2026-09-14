package kyo.apollo.network.ws

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloWebSocketClosedException

/** The JVM/Native production [[WebSocketEngine]]: opens a socket through kyo-http's
  * `HttpClient.webSocket` and bridges its fiber/`Channel`-based [[HttpWebSocket]]
  * into apollo's `Stream`/`Channel` seam. The counterpart to JS/Wasm's
  * `JsWebSocketEngine`.
  *
  * kyo-http's `webSocket` scopes the socket to a callback, so a background fiber
  * holds it open: the fiber exposes the live socket via `opened`, drains inbound
  * frames into an intermediate `incoming` channel, and completes `closed` when the
  * socket ends (cleanly on a 1000 close, aborting otherwise). Interrupting that
  * fiber — which the apollo transport does to discard a socket — tears the kyo-http
  * connection down via its scope.
  *
  * @param connectTimeout how long connecting and the WebSocket upgrade may take
  *                       before the open fails; `HttpWebSocket.Config` sets no
  *                       bound of its own
  */
final class KyoHttpWebSocketEngine(connectTimeout: Duration = KyoHttpWebSocketEngine.defaultConnectTimeout)
    extends WebSocketEngine:
    def open(
        url: String,
        protocol: Option[String] = None
    )(using Frame): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        KyoHttpWebSocketConnection.open(url, protocol, connectTimeout)
end KyoHttpWebSocketEngine

object KyoHttpWebSocketEngine:
    /** The default bound on opening a socket. */
    val defaultConnectTimeout: Duration = WebSocketEngine.defaultConnectTimeout

final private[ws] class KyoHttpWebSocketConnection private (
    ws: HttpWebSocket,
    incomingCh: Channel[Maybe[String]],
    donePromise: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]]
) extends WebSocketConnection:

    def send(text: String)(using Frame): Unit < Async =
        // A send after the socket has closed is a no-op (Abort[Closed] swallowed).
        Abort.run[Closed](ws.put(HttpWebSocket.Payload.Text(text))).unit

    def incoming(using Frame): Stream[String, Async] =
        WebSocketConnection.untilEnd(incomingCh)

    def closed(using Frame): Unit < (Async & Abort[ApolloWebSocketClosedException]) =
        donePromise.get

    def close(
        code: Int = WebSocketConnection.NormalClosure,
        reason: String = ""
    )(using Frame): Unit < Async =
        ws.close(code, reason)
end KyoHttpWebSocketConnection

private[ws] object KyoHttpWebSocketConnection:

    def open(
        url: String,
        protocol: Option[String],
        connectTimeout: Duration
    )(using Frame): WebSocketConnection < (Async & Scope & Abort[ApolloException]) =
        for
            // Unbounded and never closed: frames are `Present`, the socket's end is one
            // `Absent` marker (see `WebSocketConnection.untilEnd`), so the channel simply
            // becomes unreachable with the connection.
            incomingCh <- Channel.initUnscoped[Maybe[String]](Int.MaxValue)
            done       <- Fiber.Promise.init[Unit, Abort[ApolloWebSocketClosedException]]
            opened     <- Fiber.Promise.init[HttpWebSocket, Abort[ApolloException]]
            // The connection lives on this scoped fiber; interrupting it (on discard)
            // closes the kyo-http socket via its scope.
            _ <- Fiber.init(runConnection(url, protocol, incomingCh, done, opened))
            // A server that accepts the connection but never completes the upgrade would
            // otherwise hold the open forever; the scope's end interrupts the connecting fiber.
            ws <- Abort.run[Timeout](Async.timeout(connectTimeout)(opened.get)).map {
                case Result.Success(ws) => ws
                case Result.Failure(_)  => Abort.fail(ApolloNetworkException(s"The WebSocket did not open within ${connectTimeout.show}"))
                case Result.Panic(e)    => Abort.panic(e)
            }
        yield new KyoHttpWebSocketConnection(ws, incomingCh, done)

    /** Connect, expose the live socket, drain frames, and settle `closed`. */
    private def runConnection(
        url: String,
        protocol: Option[String],
        incomingCh: Channel[Maybe[String]],
        done: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]],
        opened: Fiber.Promise[HttpWebSocket, Abort[ApolloException]]
    )(using Frame): Unit < (Async & Scope) =
        val config = HttpWebSocket.Config(subprotocols = protocol.toSeq)
        val connect =
            HttpClient.webSocket(url, HttpHeaders.empty, config) { ws =>
                opened.completeDiscard(Result.succeed(ws)).andThen(drain(ws, incomingCh, done))
            }
        Abort.run[Throwable](connect).map {
            case Result.Success(_) => ()
            case Result.Failure(e) => failHandshake(e, done, opened, incomingCh)
            case Result.Panic(e)   => failHandshake(e, done, opened, incomingCh)
        }
    end runConnection

    /** Pump inbound frames into `incomingCh`; when the socket ends, settle `closed`
      * from its close reason. A clean close always carries a reason — a peer close
      * frame or a locally-initiated close both set `closeReason` `Present` — so a
      * 1000 reason settles cleanly and any other code aborts with that code. The
      * *absence* of a reason means the socket ended with no close frame at all (a
      * TCP reset / EOF): an abnormal drop, settled as 1006 so the transport
      * reconnects, matching the JS engine's browser-1006 mapping. Treating that
      * `Absent` as a clean close would silently disable reconnection on JVM/Native.
      *
      * The channel ends with the `Absent` marker, never `close`: `ws.stream` ends and
      * this fiber settles `closed` in one go, while the transport's drain of
      * `incoming` is a separate fiber that may not have taken the last frames yet — a
      * `close` here would hand them to this closer and lose a `next` + `complete` sent
      * right before the server's close frame.
      */
    private def drain(
        ws: HttpWebSocket,
        incomingCh: Channel[Maybe[String]],
        done: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]]
    )(using Frame): Unit < Async =
        ws.stream.foreach {
            // Drop a frame if the intermediate channel has closed (Abort[Closed] swallowed).
            case HttpWebSocket.Payload.Text(t)   => Abort.run[Closed](incomingCh.put(Present(t))).unit
            case HttpWebSocket.Payload.Binary(_) => ()
        }.andThen {
            ws.closeReason.map { reason =>
                val settle =
                    reason match
                        case Present((code, why)) if code != WebSocketConnection.NormalClosure =>
                            done.completeDiscard(
                                Result.fail(ApolloWebSocketClosedException(code, Option(why).filter(_.nonEmpty)))
                            )
                        case Present(_) =>
                            done.completeUnitDiscard
                        case Absent =>
                            done.completeDiscard(
                                Result.fail(ApolloWebSocketClosedException(WebSocketConnection.NormalClosure + 6, None))
                            )
                settle.andThen(offerEnd(incomingCh))
            }
        }
    end drain

    private def failHandshake(
        cause: Throwable,
        done: Fiber.Promise[Unit, Abort[ApolloWebSocketClosedException]],
        opened: Fiber.Promise[HttpWebSocket, Abort[ApolloException]],
        incomingCh: Channel[Maybe[String]]
    )(using Frame): Unit < Async =
        val apollo = cause match
            case e: ApolloException => e
            case other              => ApolloNetworkException(cause = other)
        val wsClosed = ApolloWebSocketClosedException(WebSocketConnection.NormalClosure + 6, Some(apollo.getMessage))
        opened.completeDiscard(Result.fail(apollo))
            .andThen(done.completeDiscard(Result.fail(wsClosed)))
            .andThen(offerEnd(incomingCh))
    end failHandshake

    /** Put the end-of-stream marker (see [[drain]] for why a marker, not `close`). */
    private def offerEnd(incomingCh: Channel[Maybe[String]])(using Frame): Unit < Async =
        Abort.run[Closed](incomingCh.put(Absent)).unit
end KyoHttpWebSocketConnection
