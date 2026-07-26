package kyo.apollo.network.ws

import kyo.Chunk
import kyo.apollo.json.Json

/** Unit tests for the two GraphQL-over-WebSocket wire protocols
  * ([[GraphQLWsProtocol]] and [[SubscriptionWsProtocol]]).
  *
  * Both are pure functions — argument → wire text (outgoing) and wire text →
  * [[WsMessage]] (incoming) — so they are tested here in isolation, without any
  * socket or transport. The multiplexing transport that drives them lands with
  * its own fake-connection tests in a later Phase 06 task.
  */
class WsProtocolSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val payload = Json.JObj(scala.collection.immutable.VectorMap("token" -> Json.JStr("abc")))
    private val requestBody = Json.JObj(
        scala.collection.immutable.VectorMap(
            "query"         -> Json.JStr("subscription S { ticks }"),
            "operationName" -> Json.JStr("S")
        )
    )

    "GraphQLWsProtocol & SubscriptionWsProtocol" - {

        // ---- graphql-transport-ws (modern) ---------------------------------------

        "modern: subprotocol token is graphql-transport-ws" in {
            assert(GraphQLWsProtocol.name == "graphql-transport-ws")
        }

        "modern: connection_init omits payload when None, includes it when Some" in {
            assert(GraphQLWsProtocol.connectionInit() == """{"type":"connection_init"}""")
            assert(
                GraphQLWsProtocol.connectionInit(Some(payload))
                    == """{"type":"connection_init","payload":{"token":"abc"}}"""
            )
        }

        "modern: start uses subscribe with id/type/payload in order" in {
            assert(
                GraphQLWsProtocol.startOperation("7", requestBody)
                    == """{"id":"7","type":"subscribe","payload":{"query":"subscription S { ticks }","operationName":"S"}}"""
            )
        }

        "modern: stop uses complete" in {
            assert(GraphQLWsProtocol.stopOperation("7") == """{"id":"7","type":"complete"}""")
        }

        "modern: pong is emitted, echoing an optional payload" in {
            assert(GraphQLWsProtocol.pong() == Some("""{"type":"pong"}"""))
            assert(
                GraphQLWsProtocol.pong(Some(payload))
                    == Some("""{"type":"pong","payload":{"token":"abc"}}""")
            )
        }

        "modern: parse decodes every server frame type" in {
            assert(
                GraphQLWsProtocol.parse("""{"type":"connection_ack"}""")
                    == WsMessage.ConnectionAck(None)
            )
            assert(
                GraphQLWsProtocol.parse("""{"type":"connection_ack","payload":{"token":"abc"}}""")
                    == WsMessage.ConnectionAck(Some(payload))
            )
            assert(
                GraphQLWsProtocol.parse("""{"id":"1","type":"next","payload":{"data":null}}""")
                    == WsMessage.Data(
                        "1",
                        Json.JObj(scala.collection.immutable.VectorMap("data" -> Json.JNull))
                    )
            )
            assert(
                GraphQLWsProtocol.parse("""{"id":"1","type":"error","payload":[{"message":"boom"}]}""")
                    == WsMessage.Error(
                        "1",
                        Json.JArr(
                            Chunk(Json.JObj(scala.collection.immutable.VectorMap("message" -> Json.JStr("boom"))))
                        )
                    )
            )
            assert(GraphQLWsProtocol.parse("""{"id":"1","type":"complete"}""") == WsMessage.Complete("1"))
            assert(GraphQLWsProtocol.parse("""{"type":"ping"}""") == WsMessage.Ping(None))
            assert(GraphQLWsProtocol.parse("""{"type":"pong"}""") == WsMessage.Pong(None))
        }

        "modern: ka is not a known modern frame" in {
            assert(GraphQLWsProtocol.parse("""{"type":"ka"}""") == WsMessage.Unknown("""{"type":"ka"}"""))
        }

        // ---- subscriptions-transport-ws (legacy) ---------------------------------

        "legacy: subprotocol token is graphql-ws" in {
            assert(SubscriptionWsProtocol.name == "graphql-ws")
        }

        "legacy: start uses start and stop uses stop" in {
            assert(
                SubscriptionWsProtocol.startOperation("2", requestBody)
                    == """{"id":"2","type":"start","payload":{"query":"subscription S { ticks }","operationName":"S"}}"""
            )
            assert(SubscriptionWsProtocol.stopOperation("2") == """{"id":"2","type":"stop"}""")
        }

        "legacy: no pong frame" in {
            assert(SubscriptionWsProtocol.pong() == None)
            assert(SubscriptionWsProtocol.pong(Some(payload)) == None)
        }

        "legacy: parse decodes data, ka, complete, error and connection frames" in {
            assert(
                SubscriptionWsProtocol.parse("""{"type":"connection_ack"}""") == WsMessage.ConnectionAck(
                    None
                )
            )
            assert(
                SubscriptionWsProtocol.parse("""{"type":"connection_error","payload":{"token":"abc"}}""")
                    == WsMessage.ConnectionError(Some(payload))
            )
            assert(SubscriptionWsProtocol.parse("""{"type":"ka"}""") == WsMessage.KeepAlive)
            assert(
                SubscriptionWsProtocol.parse("""{"id":"3","type":"data","payload":{"data":null}}""")
                    == WsMessage.Data(
                        "3",
                        Json.JObj(scala.collection.immutable.VectorMap("data" -> Json.JNull))
                    )
            )
            assert(
                SubscriptionWsProtocol.parse("""{"id":"3","type":"complete"}""") == WsMessage.Complete("3")
            )
        }

        "legacy: subscribe/next/ping are not legacy frames" in {
            assert(
                SubscriptionWsProtocol.parse("""{"id":"1","type":"next","payload":{}}""")
                    == WsMessage.Unknown("""{"id":"1","type":"next","payload":{}}""")
            )
        }

        // ---- shared robustness ----------------------------------------------------

        "both: malformed text parses to Unknown, never throws" in {
            assert(GraphQLWsProtocol.parse("not json") == WsMessage.Unknown("not json"))
            assert(SubscriptionWsProtocol.parse("[") == WsMessage.Unknown("["))
            // A non-object JSON top level is also Unknown.
            assert(GraphQLWsProtocol.parse("42") == WsMessage.Unknown("42"))
        }

        "both: an operation frame missing its id folds to Unknown" in {
            assert(
                GraphQLWsProtocol.parse("""{"type":"next","payload":{}}""") == WsMessage.Unknown(
                    """{"type":"next","payload":{}}"""
                )
            )
            assert(
                SubscriptionWsProtocol.parse("""{"type":"data","payload":{}}""") == WsMessage.Unknown(
                    """{"type":"data","payload":{}}"""
                )
            )
        }

        "both: an unknown type folds to Unknown" in {
            assert(
                GraphQLWsProtocol.parse("""{"type":"weird"}""") == WsMessage.Unknown("""{"type":"weird"}""")
            )
        }
    }
end WsProtocolSpec
