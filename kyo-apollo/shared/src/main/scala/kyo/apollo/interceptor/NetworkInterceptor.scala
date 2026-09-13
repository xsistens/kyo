package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.api.Defer
import kyo.apollo.api.Subscription
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.http.HttpNetworkTransport
import kyo.apollo.network.ws.WebSocketNetworkTransport
import kyo.apollo.runtime.ResponseStream

/** The terminal [[ApolloInterceptor]]: it does not proceed down the chain but
  * instead hands the request to a transport, lifting the result into a [[Flow]].
  *
  * It routes by operation kind, mirroring apollo-kotlin's `NetworkInterceptor`:
  *   - a [[Subscription]] is a long-lived stream, so it goes to the
  *     [[WebSocketNetworkTransport]] whose `subscribe` returns the cold,
  *     multi-emission `Flow` of subscription events directly; and
  *   - every other operation (query / mutation) is a single request/response, so
  *     it goes to the [[HttpNetworkTransport]] and its `Future[ApolloResponse]` is
  *     lifted into a one-emission [[Flow]].
  *
  * Because both transports already fold every network/HTTP/parse failure into
  * `ApolloResponse.error` (Task 3's "failures are values" contract), this
  * interceptor never fails the stream for those conditions — the response, error
  * or not, simply arrives as a value. It must be placed **last** in an
  * [[ApolloInterceptorChain]]; it ignores the `chain` argument by design.
  *
  * @param transport          the terminal HTTP transport queries/mutations use
  * @param webSocketTransport the terminal WebSocket transport subscriptions use;
  *                           `None` when the client configured no subscription
  *                           endpoint, in which case a subscription surfaces a
  *                           clear [[ApolloNetworkException]] value rather than
  *                           throwing (failures stay values)
  */
final class NetworkInterceptor(
    transport: HttpNetworkTransport,
    webSocketTransport: Option[WebSocketNetworkTransport] = None
) extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        request.operation match
            case _: Subscription[?] =>
                webSocketTransport match
                    case Some(ws) => ws.subscribe(request)
                    case None =>
                        Stream.init(
                            Seq(
                                ApolloResponse.fromException(
                                    request.requestUuid,
                                    ApolloNetworkException(
                                        "No WebSocket transport is configured; set " +
                                            "ApolloClient.Builder.webSocketServerUrl to run subscriptions."
                                    ),
                                    request.executionContext
                                )
                            )
                        )
            case _ if Defer.has(request.operation.rootField) =>
                // An @defer operation streams incrementally: the transport reads a
                // multipart/mixed body and emits one response per patch.
                transport.executeStreaming(request)
            case _ =>
                // Lift the single transport response into a one-element stream. The
                // response is fetched when the stream is consumed (cold), matching the
                // old `Flow.fromFuture` laziness.
                Stream.init(transport.execute(request).map(Seq(_)))
end NetworkInterceptor
