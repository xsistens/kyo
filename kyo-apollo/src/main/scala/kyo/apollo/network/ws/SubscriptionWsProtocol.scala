package kyo.apollo.network.ws

import kyo.apollo.json.Json

/** The legacy `subscriptions-transport-ws` protocol — apollo-kotlin's
  * `SubscriptionWsProtocol`, still spoken by older Apollo Server deployments.
  *
  * Wire shape (subprotocol token `graphql-ws` — note the historical clash with
  * the *modern* library's name; see [[WsProtocol.name]]):
  *   - client → `GQL_CONNECTION_INIT`   `{"type":"connection_init","payload"?:{…}}`
  *   - server → `GQL_CONNECTION_ACK`    `{"type":"connection_ack"}`
  *   - server → `GQL_CONNECTION_ERROR`  `{"type":"connection_error","payload":{…}}`
  *   - client → `GQL_START`             `{"id":"1","type":"start","payload":{query,…}}`
  *   - server → `GQL_DATA`              `{"id":"1","type":"data","payload":{data,errors}}`
  *   - server → `GQL_ERROR`             `{"id":"1","type":"error","payload":{…}}`
  *   - client → `GQL_STOP`              `{"id":"1","type":"stop"}`
  *   - server → `GQL_COMPLETE`          `{"id":"1","type":"complete"}`
  *   - server → `GQL_CONNECTION_KEEP_ALIVE` `{"type":"ka"}`
  *
  * Two structural differences from the modern protocol matter to the transport:
  * cancellation uses a distinct `stop` frame (not `complete`), and keep-alive is
  * a one-way server `ka` requiring no reply — so [[pong]] returns `None`.
  */
object SubscriptionWsProtocol extends WsProtocol:
    import WsProtocol.*

    /** Discriminators unique to this protocol (the shared ones live in
      * [[WsProtocol.Type]]).
      */
    private object Type:
        val Start     = "start"
        val Stop      = "stop"
        val Data      = "data"
        val KeepAlive = "ka"
    end Type

    val name: String = "graphql-ws"

    def connectionInit(payload: Option[Json] = None): String =
        message(
            required("type", Json.JStr(WsProtocol.Type.ConnectionInit)),
            "payload" -> payload
        )

    def startOperation(id: String, payload: Json): String =
        message(
            required("id", Json.JStr(id)),
            required("type", Json.JStr(Type.Start)),
            required("payload", payload)
        )

    def stopOperation(id: String): String =
        message(
            required("id", Json.JStr(id)),
            required("type", Json.JStr(Type.Stop))
        )

    /** The legacy protocol has no client `pong`; its server keep-alive (`ka`)
      * requires no response.
      */
    def pong(payload: Option[Json] = None): Option[String] = None

    def parse(text: String): WsMessage =
        asObject(text) match
            case None => WsMessage.Unknown(text)
            case Some(fields) =>
                val id      = idOf(fields)
                val payload = payloadOf(fields)
                typeOf(fields) match
                    case WsProtocol.Type.ConnectionAck   => WsMessage.ConnectionAck(payload)
                    case WsProtocol.Type.ConnectionError => WsMessage.ConnectionError(payload)
                    case Type.KeepAlive                  => WsMessage.KeepAlive
                    case Type.Data =>
                        id.fold(WsMessage.Unknown(text))(i => WsMessage.Data(i, payload.getOrElse(Json.JNull)))
                    case WsProtocol.Type.Error =>
                        id.fold(WsMessage.Unknown(text))(i => WsMessage.Error(i, payload.getOrElse(Json.JNull)))
                    case WsProtocol.Type.Complete =>
                        id.fold(WsMessage.Unknown(text))(WsMessage.Complete(_))
                    case _ => WsMessage.Unknown(text)
                end match
end SubscriptionWsProtocol
