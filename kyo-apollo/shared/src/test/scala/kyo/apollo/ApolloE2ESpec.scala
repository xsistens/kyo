package kyo.apollo

import kyo.{System as KyoSystem, *}
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.network.ws.WsTestSupport
import scala.collection.immutable.VectorMap

/** The one spec that runs on EVERY client platform (JVM/JS/Native/Wasm) against a
  * single shared caliban backend — the cross-platform interop proof the per-platform
  * hermetic specs cannot give. Each platform exercises its own production engines
  * (kyo-http on JVM/Native, `fetch`/`WebSocket` on JS/Wasm) over real sockets against
  * the same running `apolloit.ItServer`.
  *
  * Gated on the `APOLLO_IT_URL` env var (host:port) the orchestrator exports after
  * starting the server: absent — e.g. a plain `sbt test` — the leaf is `cancel`led
  * (reported Cancelled, not Passed) so a run without the backend never masquerades as
  * interop coverage. The env read is `kyo.System.env`, which is `process.env` on
  * JS/Wasm (Node) and the JVM/Native environment otherwise.
  */
class ApolloE2ESpec extends kyo.test.Test[Any]:

    // Connects to an external server; no in-process listener, but skip the client-socket
    // leak check to match the other live specs.
    override def config = super.config.leakCheckSockets(false)

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

    "apollo client drives a query and subscription against the shared caliban server" in {
        KyoSystem.env[String]("APOLLO_IT_URL").map {
            case Absent =>
                // Not running under the E2E orchestrator (scripts/apollo-e2e.sh). Report
                // this as Cancelled, not Passed: a plain `sbt test` must not show a green
                // leaf that never touched a live engine — that would read as cross-platform
                // interop coverage the run did not actually provide.
                cancel("APOLLO_IT_URL not set — run scripts/apollo-e2e.sh to exercise the live engines")
            case Present(base) =>
                val client = ApolloClient
                    .builder()
                    .serverUrl(s"http://$base/api/graphql")
                    .webSocketServerUrl(s"ws://$base/api/graphql/ws")
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
end ApolloE2ESpec
