package kyo.apollo.devtools

import kyo.{HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.api.CompiledArgument
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Mutation
import kyo.apollo.cache.normalized.MemoryCache
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.cache.normalized.normalizedCache
import kyo.apollo.json.Json
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import scala.collection.immutable.VectorMap
import scala.scalajs.js as sjs

/** The window hook `connectToDevtools` installs: nothing at all unless the caller
  * switches it on, exactly one shim when it does, and no credential of a mutation
  * on the tabs the extension reads.
  *
  * The suite runs under Node, where `window` does not exist. A test that needs one
  * sets a fake `window` on `globalThis` and removes it again within the same
  * synchronous block, so no other test can observe it.
  */
class ApolloDevtoolsSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def fakeParse(sdl: String): sjs.Any = sjs.Dynamic.literal(kind = "Document", src = sdl)

    private val globalObject: sjs.Dynamic = sjs.Dynamic.global.globalThis

    private val symbol: sjs.Any = sjs.Dynamic.global.Symbol.applyDynamic("for")("apollo.devtools")

    /** Run `f` with a fake `window` on `globalThis`, removing it before returning. */
    private def withWindow[A](f: sjs.Dynamic => A): A =
        val window = sjs.Dynamic.literal()
        globalObject.updateDynamic("window")(window)
        try f(window)
        finally discard(sjs.special.delete(globalObject, "window"))
    end withWindow

    private def registered(window: sjs.Dynamic): sjs.Any =
        sjs.Dynamic.global.Reflect.applyDynamic("get")(window, symbol)

    private def builder: ApolloClient.Builder =
        ApolloClient.builder().serverUrl("https://example.com/graphql").httpEngine(loginEngine)

    private val loginEngine: HttpEngine = new HttpEngine:
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            HttpResponse(200, Nil, """{"data":{"login":{"ok":true,"user":{"__typename":"User","id":"1"}}}}""")

    final case class LoginUser(id: String) derives Schema
    final case class LoginPayload(ok: Boolean, user: LoginUser) derives Schema
    final case class LoginData(login: LoginPayload) derives Schema

    /** `mutation Login($input: LoginInput!) { login(input: $input) { ok user { id } } }` */
    final case class LoginMutation() extends Mutation[LoginData]:
        def name: String                  = "Login"
        def document: String              = "mutation Login($input: LoginInput!) { login(input: $input) { ok user { id } } }"
        def dataSchema: Schema[LoginData] = summon[Schema[LoginData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Mutation"),
                selections = Chunk(
                    CompiledField(
                        "login",
                        CompiledNamedType("LoginPayload"),
                        arguments = Chunk(CompiledArgument.variable("input")),
                        selections = Chunk(
                            CompiledField("ok", CompiledNamedType("Boolean")),
                            CompiledField(
                                "user",
                                CompiledNamedType("User"),
                                selections = Chunk(CompiledField("id", CompiledNamedType("ID")))
                            )
                        )
                    )
                )
            )
        def variables: Json =
            Json.JObj(VectorMap(
                "input" -> Json.JObj(VectorMap("email" -> Json.JStr("a@b.example"), "password" -> Json.JStr("pw")))
            ))
    end LoginMutation

    "connectToDevtools" - {

        "disabled devtools leave window untouched" in {
            withWindow { window =>
                discard(builder.connectToDevtools("x", enabled = false))
                discard(builder.connectToDevtools("x", enabled = false, fakeParse))
                assert(sjs.isUndefined(window.selectDynamic("__APOLLO_CLIENT__")))
                assert(sjs.isUndefined(registered(window)))
                assert(sjs.Object.keys(window.asInstanceOf[sjs.Object]).length == 0)
            }
        }

        "enabled devtools install exactly once" in {
            withWindow { window =>
                discard(builder.connectToDevtools("x", enabled = true, fakeParse))
                val shim = window.selectDynamic("__APOLLO_CLIENT__")
                val reg  = registered(window).asInstanceOf[sjs.Array[sjs.Any]]
                assert(shim.selectDynamic("devtoolsConfig").selectDynamic("name").asInstanceOf[String] == "x")
                assert(reg.length == 1)
                assert(reg(0) eq shim)
            }
        }

        "enabled devtools outside a browser return the client and install nothing" in {
            val client = builder.connectToDevtools("x", enabled = true, fakeParse)
            assert(client ne null)
            assert(sjs.isUndefined(globalObject.selectDynamic("__APOLLO_CLIENT__")))
        }

        "the cache tab carries no mutation argument, the entities the mutation wrote stay" in {
            val window = sjs.Dynamic.literal()
            globalObject.updateDynamic("window")(window)
            val client =
                try builder.normalizedCache(
                        MemoryCache(),
                        IdCacheKeyGenerator(List("id"))
                    ).connectToDevtools("x", enabled = true, fakeParse)
                finally discard(sjs.special.delete(globalObject, "window"))
            client.mutation(LoginMutation()).execute.map { response =>
                assert(response.data.map(_.login.ok) == Present(true))
                val extracted = window.selectDynamic("__APOLLO_CLIENT__").selectDynamic("cache").applyDynamic("extract")(true)
                val text      = sjs.JSON.stringify(extracted)
                assert(!text.contains("pw"), text)
                assert(!text.contains("a@b.example"), text)
                assert(!text.contains("ROOT_MUTATION"), text)
                assert(text.contains("User:1"), text)
                val mutations = window.selectDynamic("__APOLLO_CLIENT__").selectDynamic("queryManager").selectDynamic("mutationStore")
                val shown     = sjs.JSON.stringify(mutations.selectDynamic("0").selectDynamic("variables"))
                assert(shown == """{"input":{"email":"<redacted>","password":"<redacted>"}}""", shown)
            }
        }

        "the switch is not optional" in {
            typeCheck("""ApolloClient.builder().connectToDevtools("x", enabled = false)""")
            typeCheckFailure("""ApolloClient.builder().connectToDevtools("x")""")("connectToDevtools")
            typeCheckFailure("""ApolloClient.builder().connectToDevtools("x", (sdl: String) => sdl: scala.scalajs.js.Any)""")(
                "Required: Boolean"
            )
        }
    }
end ApolloDevtoolsSpec
