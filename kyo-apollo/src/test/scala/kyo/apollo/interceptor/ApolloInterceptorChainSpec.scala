package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpNetworkTransport
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Tests the operation-layer interceptor chain: terminal delegation to the
  * transport through [[NetworkInterceptor]], invocation order, response-stream
  * transformation, and the wiring-error guard when the chain is exhausted.
  */
class ApolloInterceptorChainSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape `data` decodes from; `D` stays `Int`
      * via a transform of this derived object schema.
      */
    final case class ValueData(value: Int) derives Schema

    /** A query whose `data` is a single `{ "value": Int }` object. */
    final case class ValueQuery() extends Query[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    /** A transport whose fake engine always returns `body` with a 200 status. */
    private def transportReturning(body: String): HttpNetworkTransport =
        val engine: HttpEngine = new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                HttpResponse(200, Nil, body)
        HttpNetworkTransport("https://example.com/graphql", engine)
    end transportReturning

    "ApolloInterceptorChain" - {

        "NetworkInterceptor terminates the chain, delegating to the transport" in {
            val chain = DefaultApolloInterceptorChain(
                Chunk(new NetworkInterceptor(transportReturning("""{"data":{"value":42}}"""))),
                0
            )
            StreamProbe.first(chain.proceed(ApolloRequest(ValueQuery()))).map { response =>
                assert(response.data == Present(42))
                assert(response.exception == Absent)
            }
        }

        "interceptors run in order and can transform the response stream" in {
            var order = List.empty[String]
            val tap: ApolloInterceptor = new ApolloInterceptor:
                def intercept[D](
                    request: ApolloRequest[D],
                    chain: ApolloInterceptorChain
                )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
                    order = order :+ "tap-before"
                    chain.proceed(request).mapPure { r =>
                        order = order :+ "tap-after"; r
                    }
                end intercept
            val terminal = new NetworkInterceptor(transportReturning("""{"data":{"value":1}}"""))
            val chain    = DefaultApolloInterceptorChain(Chunk(tap, terminal), 0)
            StreamProbe.first(chain.proceed(ApolloRequest(ValueQuery()))).map { response =>
                assert(response.data == Present(1))
                assert(order == List("tap-before", "tap-after"))
            }
        }

        "an interceptor may short-circuit without proceeding" in {
            val short: ApolloInterceptor = new ApolloInterceptor:
                def intercept[D](
                    request: ApolloRequest[D],
                    chain: ApolloInterceptorChain
                )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
                    Stream.init(Seq(ApolloResponse[D](request.requestUuid)))
            // No terminal needed: `short` never proceeds, so the chain never runs off end.
            val chain   = DefaultApolloInterceptorChain(Chunk(short), 0)
            val request = ApolloRequest(ValueQuery())
            StreamProbe.first(chain.proceed(request)).map { response =>
                assert(response.requestUuid == request.requestUuid)
                assert(response.data == Absent)
            }
        }

        "proceeding past the terminal interceptor is a wiring error" in {
            val chain = DefaultApolloInterceptorChain(Chunk.empty, 0)
            val _ = intercept[IllegalStateException] {
                chain.proceed(ApolloRequest(ValueQuery()))
            }
        }
    }
end ApolloInterceptorChainSpec
