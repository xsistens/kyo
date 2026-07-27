package kyo.apollo.network.ws

import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import scala.collection.immutable.VectorMap
import scala.util.Try

/** A single frame decoded from the server side of a GraphQL-over-WebSocket
  * conversation.
  *
  * Both wire protocols ([[GraphQLWsProtocol]] — modern `graphql-transport-ws`,
  * and [[SubscriptionWsProtocol]] — legacy `subscriptions-transport-ws`) parse
  * their otherwise-incompatible message shapes down onto this one sealed set, so
  * the [[kyo.apollo.network.ws.WebSocketNetworkTransport]] (Task 4) can route
  * messages protocol-agnostically. Only the frames the transport actually reacts
  * to are modelled; anything unrecognised folds into [[Unknown]] rather than
  * throwing, keeping the parse total (a stray or future message type never
  * breaks an active socket).
  *
  * Operation-scoped frames ([[Data]], [[Error]], [[Complete]]) carry the string
  * operation `id` the transport assigned in [[WsProtocol.startOperation]], so it
  * can dispatch each to the right subscriber `Flow`. Connection-scoped frames
  * ([[ConnectionAck]], [[ConnectionError]], [[Ping]], [[Pong]], [[KeepAlive]])
  * carry no id.
  *
  * Payloads stay as raw [[Json]] here — this layer does not know an operation's
  * `data` type. The transport decodes a [[Data]] payload into a
  * [[kyo.apollo.api.GraphQLResponse]] once it has matched the id back to its
  * operation and adapter.
  */
enum WsMessage derives CanEqual:
    /** Server accepted `connection_init`; the handshake is complete. `payload` is
      * the optional server acknowledgement object (present on the modern
      * protocol, absent on the legacy one).
      */
    case ConnectionAck(payload: Option[Json])

    /** A fatal connection-level error (legacy `connection_error`, or a modern
      * server rejecting the init). Not tied to any operation; the transport treats
      * it as a terminal handshake failure. `payload` is the server's error object
      * when supplied.
      */
    case ConnectionError(payload: Option[Json])

    /** A data payload for operation `id`. On the modern protocol this is the
      * `next` frame whose `payload` is a `{ data, errors }` GraphQL envelope; on
      * the legacy protocol it is the `data` frame with the same envelope shape.
      */
    case Data(id: String, payload: Json)

    /** An error terminating operation `id`. `payload` is the modern protocol's
      * array of GraphQL errors, or the legacy protocol's single error object —
      * left as raw [[Json]] so the transport can surface it faithfully.
      */
    case Error(id: String, payload: Json)

    /** Operation `id` completed normally; no further [[Data]] will arrive for it. */
    case Complete(id: String)

    /** A server-initiated `ping` (modern protocol only). The transport should
      * answer with [[WsProtocol.pong]]. `payload` is the optional ping body.
      */
    case Ping(payload: Option[Json])

    /** A server `pong`, i.e. the answer to a client ping. Informational — the
      * transport uses it only as a liveness signal.
      */
    case Pong(payload: Option[Json])

    /** The legacy protocol's server keep-alive (`ka`). Requires no response;
      * observed purely as a liveness signal.
      */
    case KeepAlive

    /** A message that did not match any known frame (unparseable text, a missing
      * required `id`, or an unrecognised `type`). Carries the raw wire text for
      * diagnostics. Never thrown — parsing stays total.
      */
    case Unknown(raw: String)
end WsMessage

/** Encodes the client half and decodes the server half of a
  * GraphQL-over-WebSocket wire protocol.
  *
  * This is the one seam that differs between the two subprotocols apollo-kotlin
  * supports. Everything above it (the multiplexing transport, the interceptor
  * chain, `Flow`) is protocol-agnostic: it speaks in operation ids and
  * [[WsMessage]] values, and delegates every byte on the wire to a `WsProtocol`.
  *
  * The trait is intentionally pure and stateless — each method maps arguments to
  * a wire `String` (outgoing) or a `String` to a [[WsMessage]] (incoming), with
  * no sockets, timers, or mutable connection state. That keeps both
  * implementations trivially unit-testable in isolation (see `WsProtocolSpec`),
  * and lets the transport own all lifecycle concerns. Outgoing helpers return
  * the exact text to hand to [[WebSocketConnection.send]].
  *
  * Mirrors apollo-kotlin's `WsProtocol` abstraction (`connectionInit` /
  * `startOperation` / `stopOperation` / `handleServerMessage`).
  */
trait WsProtocol:

    /** The WebSocket subprotocol token to negotiate in the `Sec-WebSocket-Protocol`
      * header when opening the socket — `"graphql-transport-ws"` for the modern
      * protocol, `"graphql-ws"` for the legacy one. The engine (Task 3) passes
      * this to the platform `WebSocket` constructor.
      *
      * Note the historical naming trap: the *legacy* protocol claims the plain
      * `"graphql-ws"` token even though the *modern* library is called `graphql-ws`.
      */
    def name: String

    /** The client's opening `connection_init` frame. `payload` carries optional
      * connection parameters (e.g. an auth token map); when `None` the `payload`
      * field is omitted entirely, as some servers reject a null one.
      */
    def connectionInit(payload: Option[Json] = None): String

    /** Start operation `id` with the already-composed GraphQL request `payload`
      * (`{ query, operationName, variables, extensions }`, produced by
      * [[kyo.apollo.api.OperationRequestBody]]). The protocol wraps it in its own
      * envelope (`subscribe` for modern, `start` for legacy).
      */
    def startOperation(id: String, payload: Json): String

    /** Stop operation `id` — the client's request to cancel a subscription
      * (modern `complete`, legacy `stop`). Sent when a subscriber cancels.
      */
    def stopOperation(id: String): String

    /** The client's answer to a server [[WsMessage.Ping]], echoing its `payload`.
      * Returns `None` on protocols without a pong frame (the legacy protocol),
      * where the server keep-alive requires no reply.
      */
    def pong(payload: Option[Json] = None): Option[String]

    /** Decode one raw server message into a [[WsMessage]]. Total: malformed text,
      * an unknown `type`, or a frame missing a required `id` all yield
      * [[WsMessage.Unknown]] rather than throwing.
      */
    def parse(text: String): WsMessage
end WsProtocol

object WsProtocol:

    /** Wire `type` discriminators shared by both protocols. The subprotocols
      * overlap heavily on token spelling (`connection_init`, `connection_ack`,
      * `complete`, `error`) while diverging on a handful (`subscribe`/`start`,
      * `next`/`data`, `ping`/`ka`), so centralising the common ones avoids typos.
      */
    private[ws] object Type:
        val ConnectionInit  = "connection_init"
        val ConnectionAck   = "connection_ack"
        val ConnectionError = "connection_error"
        val Error           = "error"
        val Complete        = "complete"
    end Type

    /** Serialise a JSON object message with deterministic field order.
      *
      * Backed by [[VectorMap]] so field insertion order is preserved, making the
      * emitted text stable and assertion-friendly — the same determinism contract
      * [[kyo.apollo.api.OperationRequestBody]] relies on. Entries whose value is `None`
      * are dropped, so callers can express an optional `payload` inline.
      */
    private[ws] def message(fields: (String, Option[Json])*): String =
        val present = fields.collect { case (k, Some(v)) => k -> v }
        Json.JObj(VectorMap.from(present)).render

    /** A required field, always emitted. */
    private[ws] def required(name: String, value: Json): (String, Option[Json]) =
        name -> Some(value)

    /** Parse `text` to a JSON object's fields, or `None` when it is not a JSON
      * object (malformed text or a non-object top level).
      */
    private[ws] def asObject(text: String): Option[Map[String, Json]] =
        Try(JsonParser.parse(text)).toOption.collect { case Json.JObj(fields) =>
            fields
        }

    /** The string `type` discriminator, or `""` when absent/non-string. */
    private[ws] def typeOf(fields: Map[String, Json]): String =
        fields.get("type").collect { case Json.JStr(t) => t }.getOrElse("")

    /** The operation `id`, or `None` when absent/non-string. */
    private[ws] def idOf(fields: Map[String, Json]): Option[String] =
        fields.get("id").collect { case Json.JStr(s) => s }

    /** The optional `payload`, absent or JSON `null` both mapping to `None`. */
    private[ws] def payloadOf(fields: Map[String, Json]): Option[Json] =
        fields.get("payload").filter(_ != Json.JNull)
end WsProtocol
