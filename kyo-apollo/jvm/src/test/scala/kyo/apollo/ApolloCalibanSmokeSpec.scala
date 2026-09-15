package kyo.apollo

import caliban.RootResolver
import caliban.graphQL
import kyo.*
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.network.ws.WsTestSupport
import scala.collection.immutable.VectorMap

/** The JVM engines against a caliban server: an `ApolloClient` on its default engines runs a
  * query (`HttpClientEngine`) and a `graphql-transport-ws` subscription (`KyoHttpWebSocketEngine`)
  * against an in-process caliban server served over kyo-http (`Resolvers.run`).
  *
  * [[ApolloLiveServerSpec]] drives the same engines over live sockets on JVM and Native, but its
  * server is a hand-written stub that shares the client's reading of the protocol. This suite
  * checks the client against a server implementation it does not share code with. caliban runs
  * only on the JVM, hence the JVM row. [[ApolloE2ESpec]] repeats the check on every platform
  * against `kyo-apollo-itserver` when `APOLLO_IT_URL` is set.
  *
  * The schema mirrors the `{ value: Int }` fixtures: `query Value { value }` returns 42 and
  * `subscription Value { value }` streams 10, 20, 30.
  */
class ApolloCalibanSmokeSpec extends kyo.test.Test[Any]:

    // Starts a real server whose listener fd the NIO transport closes late; run leaves
    // sequentially and skip the socket leak check, as the kyo-caliban suites do.
    override def config = super.config.sequential.leakCheckSockets(false)

    case class CalibanQuery(value: Int) derives caliban.schema.Schema.SemiAuto
    case class CalibanMutation(noop: Int) derives caliban.schema.Schema.SemiAuto
    case class CalibanSubscription(value: zio.stream.ZStream[Any, Nothing, Int]) derives caliban.schema.Schema.SemiAuto

    private def api(values: Int*) =
        graphQL(RootResolver(
            CalibanQuery(42),
            CalibanMutation(0),
            CalibanSubscription(zio.stream.ZStream.fromIterable(values))
        ))

    /** A `{ value: Int }` query mirroring [[WsTestSupport.ValueSubscription]]. */
    final case class ValueQuery() extends Query.Normalizable[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        val dataCodec: JsonCodec[Int] =
            JsonCodec.fromSchema(using summon[Schema[WsTestSupport.ValueData]].transform[Int](_.value)(WsTestSupport.ValueData.apply))
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    "HttpClientEngine executes a query against a caliban server" in {
        for
            interpreter <- Resolvers.get(api())
            server      <- Resolvers.run(interpreter)
            client      <- ApolloClient.init(ApolloClient.Config(s"http://localhost:${server.port}/api/graphql"))
            response    <- client.query(ValueQuery()).execute
        yield assert(response.data == Present(42))
        end for
    }

    "KyoHttpWebSocketEngine streams a subscription from a caliban server" in {
        for
            interpreter <- Resolvers.get(api(10, 20, 30))
            server      <- Resolvers.run(interpreter)
            client <- ApolloClient.init(
                ApolloClient.Config(s"http://localhost:${server.port}/api/graphql")
                    .webSocketServerUrl(s"ws://localhost:${server.port}/api/graphql/ws")
            )
            responses <- StreamProbe.collect(client.subscription(WsTestSupport.ValueSubscription()).stream.take(3))
        yield
            val values = responses.flatMap(r =>
                r.data match
                    case Present(v) => List(v)
                    case Absent     => Nil
            )
            assert(values == List(10, 20, 30))
        end for
    }
end ApolloCalibanSmokeSpec
