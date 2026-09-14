package kyo.apollo.network

import kyo.{HttpMethod as _, *}
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap

/** Tests [[ApolloRequest]] as a value and its fluent [[ApolloRequest.Builder]]:
  * equality over equal arguments, id minting at build time (fresh per build,
  * reproducible under `Random.withSeed`), header accumulation, method/APQ
  * overrides, and execution context merging — all without duplicating any
  * serialization logic (the request is a pure envelope around the
  * [[kyo.apollo.api.Operation]]).
  */
class ApolloRequestSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Minimal hand-written query, mirroring the kyo-schema test operations. */
    final case class MiniQuery(limit: Int) extends Query.Normalizable[Int]:
        def name: String              = "Mini"
        def document: String          = "query Mini($limit: Int!) { x }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json           = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end MiniQuery

    private val id = TestIds.requestUuid

    "ApolloRequest" - {

        "defaults: POST-unspecified, no headers, document sent, APQ off, no optimistic data, empty context" in {
            val request = ApolloRequest(MiniQuery(1), id)
            assert(request.httpHeaders == Nil)
            assert(request.httpMethod == None)
            assert(request.sendApqExtensions == false)
            assert(request.sendDocument == true)
            assert(request.optimisticData == Absent)
            assert(request.executionContext.isEmpty)
        }

        "two requests built from equal arguments are equal" in {
            val context = ExecutionContext.Empty + ExecutionContextSpecKeys.Flag("a")
            assert(ApolloRequest(MiniQuery(1), id) == ApolloRequest(MiniQuery(1), id))
            assert(
                ApolloRequest(MiniQuery(1), id, executionContext = context) ==
                    ApolloRequest(MiniQuery(1), id, executionContext = ExecutionContext.Empty + ExecutionContextSpecKeys.Flag("a"))
            )
            assert(ApolloRequest(MiniQuery(1), id) != ApolloRequest(MiniQuery(1), TestIds.otherUuid))
        }

        "a builder without an id mints a fresh one on every build, and nothing else differs" in {
            val builder = ApolloRequest.builder(MiniQuery(1)).addHttpHeader("A", "1")
            Kyo.zip(builder.build, builder.build).map { (a, b) =>
                assert(a.requestUuid != b.requestUuid)
                assert(a.copy(requestUuid = b.requestUuid) == b)
            }
        }

        "Random.withSeed makes the ids a builder mints reproducible" in {
            val builder = ApolloRequest.builder(MiniQuery(1))
            Kyo.zip(Random.withSeed(42)(builder.build), Random.withSeed(42)(builder.build)).map { (a, b) =>
                assert(a == b)
            }
        }

        "builder chains headers, method, and APQ flags" in {
            ApolloRequest
                .builder(MiniQuery(5))
                .requestUuid(id)
                .httpMethod(HttpMethod.Get)
                .addHttpHeader("Authorization", "Bearer t")
                .addHttpHeader("X-Trace", "abc")
                .sendApqExtensions(true)
                .sendDocument(false)
                .build
                .map { request =>
                    assert(request.httpMethod == Some(HttpMethod.Get))
                    assert(
                        request.httpHeaders ==
                            List(HttpHeader("Authorization", "Bearer t"), HttpHeader("X-Trace", "abc"))
                    )
                    assert(request.sendApqExtensions == true)
                    assert(request.sendDocument == false)
                }
        }

        "a builder with a pinned id builds that id every time" in {
            val builder = ApolloRequest.builder(MiniQuery(1)).requestUuid(id)
            Kyo.zip(builder.build, builder.build).map { (a, b) =>
                assert(a.requestUuid == id)
                assert(a == b)
            }
        }

        "optimisticData rides the request typed as the operation's data" in {
            ApolloRequest.builder(MiniQuery(1)).optimisticData(7).build.map { request =>
                val data: Maybe[Int] = request.optimisticData
                assert(data == Present(7))
            }
        }

        "addExecutionContext merges without discarding prior elements" in {
            ApolloRequest
                .builder(MiniQuery(1))
                .addExecutionContext(ExecutionContext.Empty + ExecutionContextSpecKeys.Flag("a"))
                .addExecutionContext(ExecutionContext.Empty + ExecutionContextSpecKeys.Count(3))
                .build
                .map { request =>
                    assert(
                        request.executionContext.get(ExecutionContextSpecKeys.Flag) == Present(
                            ExecutionContextSpecKeys.Flag("a")
                        )
                    )
                    assert(
                        request.executionContext.get(ExecutionContextSpecKeys.Count) == Present(
                            ExecutionContextSpecKeys.Count(3)
                        )
                    )
                }
        }

        "newBuilder preserves existing values, the id included, and edits incrementally" in {
            val base = ApolloRequest(MiniQuery(1), id, httpHeaders = List(HttpHeader("A", "1")))
            base.newBuilder.addHttpHeader("B", "2").build.map { edited =>
                assert(edited.requestUuid == id)
                assert(edited.httpHeaders == List(HttpHeader("A", "1"), HttpHeader("B", "2")))
                // The original envelope is untouched (immutability).
                assert(base.httpHeaders == List(HttpHeader("A", "1")))
            }
        }
    }
end ApolloRequestSpec

/** Context elements used by [[ApolloRequestSpec]]'s merge test. */
object ExecutionContextSpecKeys:
    final case class Flag(v: String) extends ExecutionContext.Element:
        def key: ExecutionContext.Key[Flag] = Flag
    object Flag extends ExecutionContext.Key[Flag]

    final case class Count(v: Int) extends ExecutionContext.Element:
        def key: ExecutionContext.Key[Count] = Count
    object Count extends ExecutionContext.Key[Count]
end ExecutionContextSpecKeys
