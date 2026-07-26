package kyo.apollo.network

import kyo.Absent
import kyo.Present
import kyo.Schema
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap

/** Tests [[ApolloRequest]] defaults and its fluent [[ApolloRequest.Builder]]:
  * header accumulation, method/APQ overrides, and execution context merging —
  * all without duplicating any serialization logic (the request is a pure
  * envelope around the [[kyo.apollo.api.Operation]]).
  */
class ApolloRequestSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Minimal hand-written query, mirroring the kyo-schema test operations. */
    final case class MiniQuery(limit: Int) extends Query[Int]:
        def name: String             = "Mini"
        def document: String         = "query Mini($limit: Int!) { x }"
        def dataSchema: Schema[Int]  = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end MiniQuery

    "ApolloRequest" - {

        "defaults: POST-unspecified, no headers, document sent, APQ off, empty context" in {
            val request = ApolloRequest(MiniQuery(1))
            assert(request.httpHeaders == Nil)
            assert(request.httpMethod == None)
            assert(request.sendApqExtensions == false)
            assert(request.sendDocument == true)
            assert(request.executionContext.isEmpty)
        }

        "each request gets a distinct default requestUuid" in {
            assert(ApolloRequest(MiniQuery(1)).requestUuid != ApolloRequest(MiniQuery(1)).requestUuid)
        }

        "builder chains headers, method, and APQ flags" in {
            val request = ApolloRequest
                .builder(MiniQuery(5))
                .httpMethod(HttpMethod.Get)
                .addHttpHeader("Authorization", "Bearer t")
                .addHttpHeader("X-Trace", "abc")
                .sendApqExtensions(true)
                .sendDocument(false)
                .build()

            assert(request.httpMethod == Some(HttpMethod.Get))
            assert(
                request.httpHeaders ==
                    List(HttpHeader("Authorization", "Bearer t"), HttpHeader("X-Trace", "abc"))
            )
            assert(request.sendApqExtensions == true)
            assert(request.sendDocument == false)
        }

        "builder carries an explicit uuid" in {
            val id = Uuid.random()
            val request = ApolloRequest
                .builder(MiniQuery(1))
                .requestUuid(id)
                .build()

            assert(request.requestUuid == id)
        }

        "addExecutionContext merges without discarding prior elements" in {
            val request = ApolloRequest
                .builder(MiniQuery(1))
                .addExecutionContext(ExecutionContext.Empty + ExecutionContextSpecKeys.Flag("a"))
                .addExecutionContext(ExecutionContext.Empty + ExecutionContextSpecKeys.Count(3))
                .build()

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

        "newBuilder preserves existing values and edits incrementally" in {
            val base   = ApolloRequest.builder(MiniQuery(1)).addHttpHeader("A", "1").build()
            val edited = base.newBuilder.addHttpHeader("B", "2").build()
            assert(edited.httpHeaders == List(HttpHeader("A", "1"), HttpHeader("B", "2")))
            // The original envelope is untouched (immutability).
            assert(base.httpHeaders == List(HttpHeader("A", "1")))
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
