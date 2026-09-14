package kyo.apollo.devtools

import kyo.{HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.TestIds
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** What the [[DevtoolsInterceptor]] hands the [[DevtoolsOperationStore]]: a
  * mutation's variables with every value redacted and the shape kept, a query's
  * variables as they are, and nothing at all until the operation actually runs.
  */
class DevtoolsInterceptorSpec extends kyo.test.Test[Any]:

    final case class LoginData(login: Boolean) derives Schema

    private val loginSchema: Schema[Boolean] = summon[Schema[LoginData]].transform[Boolean](_.login)(LoginData.apply)

    /** A login mutation whose input carries a credential and one value of every JSON
      * leaf kind.
      */
    final case class LoginMutation() extends Mutation[Boolean]:
        def name: String                = "Login"
        def document: String            = "mutation Login($input: LoginInput!) { login(input: $input) }"
        def dataSchema: Schema[Boolean] = loginSchema
        def rootField: CompiledField    = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json =
            Json.JObj(VectorMap(
                "input" -> Json.JObj(VectorMap(
                    "email"    -> Json.JStr("a@b.example"),
                    "password" -> Json.JStr("pw"),
                    "remember" -> Json.JBool(true),
                    "attempts" -> Json.JInt(3L),
                    "score"    -> Json.JNum(0.5),
                    "amount"   -> Json.JDec(BigDecimal("12345678901234567890.12")),
                    "scopes"   -> Json.JArr(Chunk(Json.JStr("read"), Json.JStr("write"))),
                    "otp"      -> Json.JNull
                ))
            ))
    end LoginMutation

    final case class SearchQuery() extends Query[Boolean]:
        def name: String                = "Search"
        def document: String            = "query Search($term: String!, $limit: Int!) { login }"
        def dataSchema: Schema[Boolean] = loginSchema
        def rootField: CompiledField    = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json =
            Json.JObj(VectorMap("term" -> Json.JStr("kyo"), "limit" -> Json.JInt(10L)))
    end SearchQuery

    private val redacted = Json.JStr("<redacted>")

    private val redactedLoginVariables: Json =
        Json.JObj(VectorMap(
            "input" -> Json.JObj(VectorMap(
                "email"    -> redacted,
                "password" -> redacted,
                "remember" -> redacted,
                "attempts" -> redacted,
                "score"    -> redacted,
                "amount"   -> redacted,
                "scopes"   -> Json.JArr(Chunk(redacted, redacted)),
                "otp"      -> Json.JNull
            ))
        ))

    /** A terminal interceptor emitting one successful response. */
    private val okTerminal: ApolloInterceptor = new ApolloInterceptor:
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            Stream.init(Seq(ApolloResponse[D](request.requestUuid)))

    private def chainOf(interceptor: DevtoolsInterceptor) =
        DefaultApolloInterceptorChain(Chunk(interceptor, okTerminal), 0)

    "the default redaction" - {

        "a mutation's variables reach the store with every value redacted and the shape kept" in {
            val store = new DevtoolsOperationStore()
            StreamProbe
                .first(chainOf(new DevtoolsInterceptor(store)).proceed(ApolloRequest(LoginMutation(), TestIds.requestUuid)))
                .map { _ =>
                    val recorded = store.mutationsSnapshot.head.variables
                    assert(recorded.render.contains("\"<redacted>\""))
                    assert(!recorded.render.contains("pw"))
                    assert(!recorded.render.contains("a@b.example"))
                    assert(recorded == redactedLoginVariables)
                }
        }

        "a query's variables reach the store unchanged" in {
            val store = new DevtoolsOperationStore()
            StreamProbe
                .first(chainOf(new DevtoolsInterceptor(store)).proceed(ApolloRequest(SearchQuery(), TestIds.requestUuid)))
                .map { _ =>
                    assert(store.queriesSnapshot.head.variables == SearchQuery().variables)
                }
        }

        "redactValues keeps names, lengths and nulls, and nothing else" in {
            assert(DevtoolsInterceptor.redactValues(LoginMutation().variables) == redactedLoginVariables)
            assert(DevtoolsInterceptor.redactValues(Json.JNull) == Json.JNull)
            assert(DevtoolsInterceptor.redactValues(Json.JArr(Chunk.empty)) == Json.JArr(Chunk.empty))
        }
    }

    "a custom redaction" - {

        "is applied to every recorded operation, queries included" in {
            val store = new DevtoolsOperationStore()
            val interceptor = new DevtoolsInterceptor(
                store,
                redact = (_, variables) => DevtoolsInterceptor.redactValues(variables)
            )
            StreamProbe
                .first(chainOf(interceptor).proceed(ApolloRequest(SearchQuery(), TestIds.requestUuid)))
                .map { _ =>
                    val recorded = store.queriesSnapshot.head.variables
                    assert(recorded == Json.JObj(VectorMap("term" -> redacted, "limit" -> redacted)))
                }
        }
    }

    "recording runs with the operation" - {

        "an intercepted operation that is never run records nothing" in {
            val store       = new DevtoolsOperationStore()
            val interceptor = new DevtoolsInterceptor(store)
            val mutation    = chainOf(interceptor).proceed(ApolloRequest(LoginMutation(), TestIds.requestUuid))
            val query       = chainOf(interceptor).proceed(ApolloRequest(SearchQuery(), TestIds.otherUuid))
            assert(store.mutationsSnapshot.isEmpty)
            assert(store.queriesSnapshot.isEmpty)
            StreamProbe.first(mutation).andThen(StreamProbe.first(query)).map { _ =>
                assert(store.mutationsSnapshot.size == 1)
                assert(store.queriesSnapshot.size == 1)
            }
        }

        "each execution of one mutation call is its own entry, under the id minted for it" in {
            val store = new DevtoolsOperationStore()
            val engine: HttpEngine = new HttpEngine:
                def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                    HttpResponse(200, Nil, """{"data":{"login":true}}""")
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(engine)
                .prependInterceptor(new DevtoolsInterceptor(store))
                .build()
            val call = client.mutation(LoginMutation())
            assert(store.mutationsSnapshot.isEmpty)
            call.execute.andThen(call.execute).map { _ =>
                val recorded = store.mutationsSnapshot
                assert(recorded.size == 2)
                assert(recorded.forall(m => !m.loading && m.variables == redactedLoginVariables))
            }
        }
    }
end DevtoolsInterceptorSpec
