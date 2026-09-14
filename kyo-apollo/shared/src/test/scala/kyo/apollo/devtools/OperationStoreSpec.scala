package kyo.apollo.devtools

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.TestIds
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Unit tests for the devtools operation-observability layer: the
  * [[DevtoolsOperationStore]] data model (enumerate, lifecycle, bounded history)
  * and the [[DevtoolsInterceptor]] recording queries/mutations as they flow
  * through the interceptor chain.
  */
class OperationStoreSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class ValueData(value: Int) derives Schema

    final case class ValueQuery() extends Query[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    final case class SetValueMutation() extends Mutation[Int]:
        def name: String     = "SetValue"
        def document: String = "mutation SetValue { setValue }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end SetValueMutation

    /** A terminal interceptor emitting one successful (data-absent) response. */
    private val okTerminal: ApolloInterceptor = new ApolloInterceptor:
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.init(Seq(ApolloResponse[D](request.requestUuid)))

    /** A terminal interceptor emitting one failed response carrying `message`. */
    private def failingTerminal(message: String): ApolloInterceptor = new ApolloInterceptor:
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.init(
                Seq(
                    ApolloResponse[D](
                        request.requestUuid,
                        error = Present(DefaultApolloException(message))
                    )
                )
            )

    private def chainOf(store: DevtoolsOperationStore, terminal: ApolloInterceptor) =
        DefaultApolloInterceptorChain(Chunk(new DevtoolsInterceptor(store), terminal), 0)

    "DevtoolsOperationStore model" - {

        "upsertQuery enumerates the query with its latest state" in {
            val store = new DevtoolsOperationStore()
            store.upsertQuery("Value", "query Value { value }", Json.JObj(VectorMap.empty), 7, None, None)
            val qs = store.queriesSnapshot
            assert(qs.size == 1 && qs.head.name == "Value" && qs.head.networkStatus == 7)
        }

        "upsertQuery on the same name replaces (latest wins)" in {
            val store = new DevtoolsOperationStore()
            store.upsertQuery("Value", "d", Json.JObj(VectorMap.empty), 1, None, None)
            store.upsertQuery("Value", "d", Json.JObj(VectorMap.empty), 7, None, None)
            assert(store.queriesSnapshot.size == 1 && store.queriesSnapshot.head.networkStatus == 7)
        }

        "startMutation then settleMutation flips loading and records the error" in {
            val store = new DevtoolsOperationStore()
            store.startMutation("id1", "SetValue", "d", Json.JObj(VectorMap.empty))
            assert(store.mutationsSnapshot.head.loading)
            store.settleMutation("id1", Some("boom"))
            val m = store.mutationsSnapshot.head
            assert(!m.loading && m.error == Some("boom"))
        }

        "mutation history is bounded to maxMutations (oldest evicted)" in {
            val store = new DevtoolsOperationStore(maxMutations = 2)
            for i <- 1 to 4 do store.startMutation(s"id$i", "SetValue", "d", Json.JObj(VectorMap.empty))
            val snap = store.mutationsSnapshot
            assert(snap.size == 2)
        }

        "clear empties both queries and mutations" in {
            val store = new DevtoolsOperationStore()
            store.upsertQuery("Value", "d", Json.JObj(VectorMap.empty), 7, None, None)
            store.startMutation("id1", "SetValue", "d", Json.JObj(VectorMap.empty))
            store.clear()
            assert(store.queriesSnapshot.isEmpty && store.mutationsSnapshot.isEmpty)
        }
    }

    "DevtoolsInterceptor recording" - {

        "a query flowing through is recorded ready (networkStatus 7) with its document" in {
            val store = new DevtoolsOperationStore()
            StreamProbe.first(chainOf(store, okTerminal).proceed(ApolloRequest(ValueQuery(), TestIds.requestUuid))).map { _ =>
                val qs = store.queriesSnapshot
                assert(qs.size == 1)
                assert(qs.head.name == "Value" && qs.head.document == "query Value { value }")
                assert(qs.head.networkStatus == 7 && qs.head.error.isEmpty)
            }
        }

        "a mutation flowing through is recorded settled (loading = false)" in {
            val store = new DevtoolsOperationStore()
            StreamProbe.first(chainOf(store, okTerminal).proceed(ApolloRequest(SetValueMutation(), TestIds.requestUuid))).map {
                _ =>
                    val ms = store.mutationsSnapshot
                    assert(ms.size == 1)
                    assert(ms.head.name == "SetValue" && !ms.head.loading && ms.head.error.isEmpty)
            }
        }

        "an errored query is recorded with networkStatus 8 and the error message" in {
            val store = new DevtoolsOperationStore()
            StreamProbe
                .first(chainOf(store, failingTerminal("nope")).proceed(ApolloRequest(ValueQuery(), TestIds.requestUuid)))
                .map { _ =>
                    val q = store.queriesSnapshot.head
                    assert(q.networkStatus == 8 && q.error == Some("nope"))
                }
        }
    }
end OperationStoreSpec
