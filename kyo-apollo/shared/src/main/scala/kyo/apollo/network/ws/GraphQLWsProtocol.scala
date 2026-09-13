package kyo.apollo.network.ws

import kyo.apollo.json.Json

/** The modern `graphql-transport-ws` protocol — the one implemented by the
  * `graphql-ws` library and the current apollo-kotlin default
  * (`GraphQLWsProtocol`).
  *
  * Wire shape (subprotocol token `graphql-transport-ws`):
  *   - client → `{"type":"connection_init","payload"?:{…}}`
  *   - server → `{"type":"connection_ack","payload"?:{…}}`
  *   - client → `{"id":"1","type":"subscribe","payload":{query,…}}`
  *   - server → `{"id":"1","type":"next","payload":{data,errors}}`
  *   - server → `{"id":"1","type":"error","payload":[{…error}]}` (an array)
  *   - either → `{"id":"1","type":"complete"}` (client cancel *and* server end)
  *   - either → `{"type":"ping","payload"?:{…}}` / `{"type":"pong","payload"?:{…}}`
  *
  * Cancellation and normal server completion share the `complete` frame, so
  * [[stopOperation]] emits `complete` — consistent with the Task 1 decision that
  * a subscriber's cancellation is a protocol `complete` for its id.
  */
object GraphQLWsProtocol extends WsProtocol:
    import WsProtocol.*

    /** Discriminators unique to this protocol (the shared ones live in
      * [[WsProtocol.Type]]).
      */
    private object Type:
        val Subscribe = "subscribe"
        val Next      = "next"
        val Ping      = "ping"
        val Pong      = "pong"
    end Type

    val name: String = "graphql-transport-ws"

    def connectionInit(payload: Option[Json] = None): String =
        message(
            required("type", Json.JStr(WsProtocol.Type.ConnectionInit)),
            "payload" -> payload
        )

    def startOperation(id: String, payload: Json): String =
        message(
            required("id", Json.JStr(id)),
            required("type", Json.JStr(Type.Subscribe)),
            required("payload", payload)
        )

    def stopOperation(id: String): String =
        message(
            required("id", Json.JStr(id)),
            required("type", Json.JStr(WsProtocol.Type.Complete))
        )

    def pong(payload: Option[Json] = None): Option[String] =
        Some(message(required("type", Json.JStr(Type.Pong)), "payload" -> payload))

    def parse(text: String): WsMessage =
        asObject(text) match
            case None => WsMessage.Unknown(text)
            case Some(fields) =>
                val id      = idOf(fields)
                val payload = payloadOf(fields)
                typeOf(fields) match
                    case WsProtocol.Type.ConnectionAck   => WsMessage.ConnectionAck(payload)
                    case WsProtocol.Type.ConnectionError => WsMessage.ConnectionError(payload)
                    case Type.Ping                       => WsMessage.Ping(payload)
                    case Type.Pong                       => WsMessage.Pong(payload)
                    case Type.Next =>
                        id.fold(WsMessage.Unknown(text))(i => WsMessage.Data(i, payload.getOrElse(Json.JNull)))
                    case WsProtocol.Type.Error =>
                        id.fold(WsMessage.Unknown(text))(i => WsMessage.Error(i, payload.getOrElse(Json.JNull)))
                    case WsProtocol.Type.Complete =>
                        id.fold(WsMessage.Unknown(text))(WsMessage.Complete(_))
                    case _ => WsMessage.Unknown(text)
                end match
end GraphQLWsProtocol
