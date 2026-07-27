package kyo.apollo.network.ws

import kyo.Schema
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Subscription
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import scala.collection.immutable.VectorMap

/** Shared subscription fixture + scripted server-frame builders reused by the
  * WebSocket transport specs, so no spec re-declares the same
  * `{ "value": Int }` subscription or hand-writes the same wire text twice.
  *
  * The frame builders come in two families — [[modern]] (`graphql-transport-ws`:
  * `connection_ack` / `next` / `error` / `complete` / `ping`) and [[legacy]]
  * (`subscriptions-transport-ws`: `connection_ack` / `data` / `error` /
  * `complete` / `ka`) — so a scenario can drive either protocol through the same
  * [[FakeWebSocketConnection.server]] seam.
  */
object WsTestSupport:

    /** The `{ "value": Int }` object shape the subscription's `data` decodes
      * from; the operation's `D` stays `Int` by transforming this derived object
      * schema, so assertions read as plain integers.
      */
    final case class ValueData(value: Int) derives Schema

    /** A subscription whose `data` is a single `{ "value": Int }` object; the
      * emitted value is the streamed event so assertions read as plain integers.
      */
    final case class ValueSubscription() extends Subscription[Int]:
        def name: String     = "Value"
        def document: String = "subscription Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Subscription"))
        def variables: Json = Json.JObj(VectorMap.empty)
    end ValueSubscription

    def request(): ApolloRequest[Int] = ApolloRequest(ValueSubscription())

    /** Server-frame builders for the modern `graphql-transport-ws` protocol. */
    object modern:
        val ack: String = """{"type":"connection_ack"}"""
        def next(id: String, value: Int): String =
            s"""{"id":"$id","type":"next","payload":{"data":{"value":$value}}}"""
        def error(id: String, message: String): String =
            s"""{"id":"$id","type":"error","payload":[{"message":"$message"}]}"""
        def complete(id: String): String = s"""{"id":"$id","type":"complete"}"""
        val ping: String                 = """{"type":"ping"}"""
    end modern

    /** Server-frame builders for the legacy `subscriptions-transport-ws` protocol. */
    object legacy:
        val ack: String = """{"type":"connection_ack"}"""
        def data(id: String, value: Int): String =
            s"""{"id":"$id","type":"data","payload":{"data":{"value":$value}}}"""
        def error(id: String, message: String): String =
            s"""{"id":"$id","type":"error","payload":{"message":"$message"}}"""
        def complete(id: String): String = s"""{"id":"$id","type":"complete"}"""
        val ka: String                   = """{"type":"ka"}"""
    end legacy
end WsTestSupport
