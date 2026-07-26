package kyo.apollo.devtools

import kyo.apollo.devtools.DevtoolsOperationStore.MutationRecord
import kyo.apollo.devtools.DevtoolsOperationStore.QueryRecord
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap
import scala.scalajs.js as sjs

/** Unit tests for the synthesized devtools client shim — the JS object the Apollo
  * Client Devtools extension (v4.26.0 contract) reflects on. Asserts the
  * non-throwing reflected surface, that documents are handed through the injected
  * parser, that the polled getters recompute live, and that `installOn` writes the
  * window globals (`__APOLLO_CLIENT__` + the `Symbol.for("apollo.devtools")` array).
  */
class DevtoolsShimSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // A fake graphql-js parse: marks the doc so tests can assert it flowed through.
    private def fakeParse(sdl: String): sjs.Any = sjs.Dynamic.literal(kind = "Document", src = sdl)

    private def fakeExtract(includeOptimistic: Boolean): Json =
        Json.JObj(Map("ROOT_QUERY" -> Json.JObj(Map("__typename" -> Json.JStr("Query")))))

    private val oneQuery =
        List(
            QueryRecord(
                "Q",
                "query Q { x }",
                Json.JObj(VectorMap.empty),
                7,
                None,
                Some(Json.JObj(Map("x" -> Json.JNum(1))))
            )
        )
    private val oneMutation =
        List(MutationRecord("M", "mutation M { y }", Json.JObj(VectorMap.empty), false, None))

    private def fakeMemory(): sjs.Any =
        sjs.Dynamic.literal(
            limits = sjs.Dynamic.literal(print = 100),
            sizes = sjs.Dynamic.literal(print = 3, links = new sjs.Array[sjs.Any]())
        )

    private def shim(
        queries: () => List[QueryRecord] = () => oneQuery,
        mutations: () => List[MutationRecord] = () => oneMutation
    ): sjs.Dynamic =
        ApolloDevtools.buildShim(
            "kyo-apollo",
            fakeExtract,
            queries,
            mutations,
            fakeParse,
            () => fakeMemory()
        )

    "DevtoolsClientShim" - {

        "version is a semver >= 4.0.0 (selects the v4 handler)" in {
            assert(shim().selectDynamic("version").asInstanceOf[String] == "4.0.0")
        }

        "devtoolsConfig.name carries the client name" in {
            assert(
                shim()
                    .selectDynamic("devtoolsConfig")
                    .selectDynamic("name")
                    .asInstanceOf[String] == "kyo-apollo"
            )
        }

        "cache.extract(true) returns the normalized JSON shape" in {
            val extracted = shim().selectDynamic("cache").applyDynamic("extract")(true)
            val rootQuery = extracted.selectDynamic("ROOT_QUERY")
            assert(!sjs.isUndefined(rootQuery))
            assert(rootQuery.selectDynamic("__typename").asInstanceOf[String] == "Query")
        }

        "cache.{write,modify,writeQuery,writeFragment} and stop are functions (patch-safe)" in {
            val s     = shim()
            val cache = s.selectDynamic("cache")
            for name <- List("write", "modify", "writeQuery", "writeFragment") do
                assert(
                    sjs.typeOf(cache.selectDynamic(name)) == "function",
                    s"cache.$name must be a function"
                )
            end for
            assert(sjs.typeOf(s.selectDynamic("stop")) == "function")
        }

        "queryManager.mutationStore holds entries with the parsed mutation + loading" in {
            val store = shim().selectDynamic("queryManager").selectDynamic("mutationStore")
            val entry = store.selectDynamic("0")
            assert(entry.selectDynamic("loading").asInstanceOf[Boolean] == false)
            assert(
                entry
                    .selectDynamic("mutation")
                    .selectDynamic("src")
                    .asInstanceOf[String] == "mutation M { y }"
            )
        }

        "getObservableQueries returns a JS-iterable of query shims with parsed documents" in {
            val oq  = shim().applyDynamic("getObservableQueries")("active")
            val arr = sjs.Dynamic.global.Array.applyDynamic("from")(oq).asInstanceOf[sjs.Array[sjs.Any]]
            assert(arr.length == 1)
            val first = arr(0).asInstanceOf[sjs.Dynamic]
            assert(
                first.selectDynamic("query").selectDynamic("src").asInstanceOf[String] == "query Q { x }"
            )
            val current = first.applyDynamic("getCurrentResult")()
            assert(current.selectDynamic("networkStatus").asInstanceOf[Int] == 7)
            val cacheDiff = first.applyDynamic("getCacheDiff")()
            assert(cacheDiff.selectDynamic("result").selectDynamic("x").asInstanceOf[Int] == 1)
        }

        "getMemoryInternals() returns the mapped memory shape (limits + sizes)" in {
            val mem = shim().applyDynamic("getMemoryInternals")()
            assert(mem.selectDynamic("sizes").selectDynamic("print").asInstanceOf[Int] == 3)
            assert(mem.selectDynamic("limits").selectDynamic("print").asInstanceOf[Int] == 100)
        }

        "the polled getters recompute live (reflect a changed backing list)" in {
            var mlist = oneMutation
            val s     = shim(mutations = () => mlist)
            val before = sjs.Dynamic.global.Object
                .applyDynamic("keys")(s.selectDynamic("queryManager").selectDynamic("mutationStore"))
            assert(before.asInstanceOf[sjs.Array[sjs.Any]].length == 1)
            mlist =
                mlist :+ MutationRecord("M2", "mutation M2 { z }", Json.JObj(VectorMap.empty), true, None)
            val after = sjs.Dynamic.global.Object
                .applyDynamic("keys")(s.selectDynamic("queryManager").selectDynamic("mutationStore"))
            assert(after.asInstanceOf[sjs.Array[sjs.Any]].length == 2)
        }

        "installOn sets __APOLLO_CLIENT__ and pushes into the Symbol.for(apollo.devtools) array" in {
            val fakeWindow = sjs.Dynamic.literal()
            val s          = shim()
            ApolloDevtools.installOn(fakeWindow, s)
            assert(
                fakeWindow
                    .selectDynamic("__APOLLO_CLIENT__")
                    .selectDynamic("version")
                    .asInstanceOf[String] == "4.0.0"
            )
            val sym = sjs.Dynamic.global.Symbol.applyDynamic("for")("apollo.devtools")
            val reg = sjs.Dynamic.global.Reflect
                .applyDynamic("get")(fakeWindow, sym)
                .asInstanceOf[sjs.Array[sjs.Any]]
            assert(reg.length == 1)
        }
    }
end DevtoolsShimSpec
