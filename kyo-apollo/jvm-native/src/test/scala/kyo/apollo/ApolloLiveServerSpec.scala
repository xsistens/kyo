package kyo.apollo

import kyo.*
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloWebSocketClosedException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ws.KyoHttpWebSocketEngine
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WebSocketNetworkTransport
import kyo.apollo.network.ws.WsTestSupport
import scala.collection.immutable.VectorMap

/** End-to-end proof that apollo's production JVM/Native engines talk to a real
  * GraphQL server over live kyo-http sockets — on BOTH platforms, in-process, with
  * no caliban (which is JVM-only, so the caliban smoke covers only the JVM).
  *
  * The server is a minimal hand-rolled GraphQL endpoint built directly on kyo-http
  * (itself cross-published to Native): a `POST /graphql` returning `{ value: 42 }`
  * and a `graphql-transport-ws` WebSocket at `/graphql/ws` that acks the handshake
  * and streams three `next` events then `complete`. An [[ApolloClient]] on its
  * default per-platform engines drives both: [[kyo.apollo.network.http.HttpEngine]]
  * for the query, [[kyo.apollo.network.ws.WebSocketEngine]] for the subscription —
  * so the kyo-http-backed `HttpClientEngine` and `KyoHttpWebSocketEngine` run against
  * real sockets on Native, not just the in-memory fakes the rest of the suite uses.
  */
class ApolloLiveServerSpec extends kyo.test.Test[Any]:

    // Starts a real server (an ephemeral listener fd the NIO transport defers closing);
    // run leaves sequentially and skip the socket leak check, as the kyo-http suites do.
    override def config = super.config.sequential.leakCheckSockets(false)

    /** A `{ value: Int }` query mirroring [[WsTestSupport.ValueSubscription]]. */
    final case class ValueQuery() extends Query[Int]:
        def name: String = "Value"
        def document: String =
            "query Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[WsTestSupport.ValueData]].transform[Int](_.value)(WsTestSupport.ValueData.apply)
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"))
        def variables: Json = Json.JObj(VectorMap.empty)
    end ValueQuery

    private def extractId(msg: String): String =
        val marker = "\"id\":\""
        val i      = msg.indexOf(marker)
        if i < 0 then "0"
        else
            val start = i + marker.length
            msg.substring(start, msg.indexOf('"', start))
        end if
    end extractId

    /** Minimal `graphql-transport-ws` server: ack the init, answer a subscribe with
      * three `next` events then `complete`, and stop when the client completes.
      */
    private def serveWs(ws: HttpWebSocket)(using Frame): Unit < (Async & Abort[Closed]) =
        Loop.foreach {
            ws.take().map {
                case HttpWebSocket.Payload.Text(msg) =>
                    if msg.contains("connection_init") then
                        ws.put(HttpWebSocket.Payload.Text("""{"type":"connection_ack"}""")).andThen(Loop.continue)
                    else if msg.contains("\"subscribe\"") then
                        val id = extractId(msg)
                        // Fire the three events plus complete in a synchronous burst — the
                        // transport's subscriber channel drains gracefully (closeAwaitEmpty),
                        // so even a complete arriving in the same tick as the last next never
                        // drops it. No pacing needed.
                        Kyo.foreachDiscard(Seq(10, 20, 30))(v =>
                            ws.put(HttpWebSocket.Payload.Text(
                                s"""{"id":"$id","type":"next","payload":{"data":{"value":$v}}}"""
                            ))
                        ).andThen(
                            ws.put(HttpWebSocket.Payload.Text(s"""{"id":"$id","type":"complete"}"""))
                        ).andThen(Loop.continue)
                    else if msg.contains("\"complete\"") then Loop.done
                    else Loop.continue
                case HttpWebSocket.Payload.Binary(_) => Loop.continue
            }
        }

    "apollo drives a real query and subscription over live kyo-http sockets" in {
        val queryHandler =
            HttpRoute.postRaw("graphql").request(_.bodyText).response(_.bodyText).handler { _ =>
                HttpResponse.ok.addField("body", """{"data":{"value":42}}""").setHeader("Content-Type", "application/json")
            }
        val wsHandler =
            HttpHandler.webSocket("graphql/ws", HttpWebSocket.Config(subprotocols = Seq("graphql-transport-ws"))) {
                (_, ws) => serveWs(ws)
            }

        HttpServer.init(0, "127.0.0.1")(queryHandler, wsHandler).map { server =>
            val client = ApolloClient
                .builder()
                .serverUrl(s"http://127.0.0.1:${server.port}/graphql")
                .webSocketServerUrl(s"ws://127.0.0.1:${server.port}/graphql/ws")
                .build()
            for
                queryResp <- client.query(ValueQuery()).execute
                subResps  <- StreamProbe.collect(client.subscription(WsTestSupport.ValueSubscription()).stream.take(3))
            yield
                assert(queryResp.data == Present(42))
                val values = subResps.flatMap(r =>
                    r.data match
                        case Present(v) => List(v)
                        case Absent     => Nil
                )
                assert(values == List(10, 20, 30))
            end for
        }
    }

    "an abnormal socket drop (no close frame) surfaces as a 1006, not a clean 1000" in {
        // The server acks the handshake then holds the subscription open, sending
        // nothing. Force-closing it (closeNow, a raw TCP drop) leaves the client with
        // an EOF and NO WebSocket close frame — kyo-http reports closeReason Absent.
        // The engine must map that to 1006 (abnormal), the value the transport's
        // reconnect logic keys on; the pre-fix code read Absent as a clean 1000 close,
        // silently disabling reconnection on JVM/Native (a divergence from the JS
        // engine's browser-1006 mapping).
        Fiber.Promise.init[Unit, Any].map { gotInit =>
            val wsHandler =
                HttpHandler.webSocket("graphql/ws", HttpWebSocket.Config(subprotocols = Seq("graphql-transport-ws"))) {
                    (_, ws) =>
                        Loop.foreach {
                            ws.take().map {
                                case HttpWebSocket.Payload.Text(msg) =>
                                    if msg.contains("connection_init") then
                                        ws.put(HttpWebSocket.Payload.Text("""{"type":"connection_ack"}"""))
                                            .andThen(gotInit.completeUnitDiscard)
                                            .andThen(Loop.continue)
                                    else Loop.continue // ack, then hold open; never complete
                                case HttpWebSocket.Payload.Binary(_) => Loop.continue
                            }
                        }
                }
            HttpServer.init(0, "127.0.0.1")(wsHandler).map { server =>
                // reconnectWhen defaults to reconnectNever, so the drop terminates the
                // subscription with its close value — exactly the code under test.
                val transport = new WebSocketNetworkTransport(
                    serverUrl = s"ws://127.0.0.1:${server.port}/graphql/ws",
                    engine = new KyoHttpWebSocketEngine
                )
                val subscription = transport.subscribe(ApolloRequest(WsTestSupport.ValueSubscription()))
                for
                    collected <- Fiber.init(Scope.run(StreamProbe.collect(subscription)))
                    _         <- gotInit.get     // the server got the handshake — the socket is open
                    _         <- server.closeNow // raw TCP drop, no WS close frame
                    seen      <- collected.get   // the subscription terminates with the drop value
                yield assert(seen.exists(r =>
                    r.exception.exists {
                        case e: ApolloWebSocketClosedException => e.code == WebSocketConnection.NormalClosure + 6 // 1006
                        case _                                 => false
                    }
                ))
                end for
            }
        }
    }
end ApolloLiveServerSpec
