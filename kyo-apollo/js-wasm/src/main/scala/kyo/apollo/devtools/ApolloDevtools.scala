package kyo.apollo.devtools

import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.apolloStore
import kyo.apollo.devtools.DevtoolsOperationStore.MutationRecord
import kyo.apollo.devtools.DevtoolsOperationStore.QueryRecord
import kyo.apollo.json.Json
import kyo.discard
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Connects a kyo-apollo client to the official **Apollo Client Devtools** browser
  * extension by synthesizing a JS object shaped like an Apollo Client and pushing
  * it into the window globals the extension reflects on. No RPC is implemented on
  * our side — the extension's injected hook does all `postMessage`/RPC work and
  * simply reads well-known getters off the object, polling them (~1s) so the tabs
  * stay live without any push.
  *
  * The reflected surface is pinned against Apollo Client Devtools **v4.26.0** (the
  * v4 handler: `gte(version,"4.0.0")`):
  *   - `version` — semver, selects the v4 handler
  *   - `devtoolsConfig.name` — client name in the dropdown
  *   - `cache.extract(optimistic)` — normalized cache JSON (Cache tab)
  *   - `cache.{write,modify,writeQuery,writeFragment}` + `stop` — functions the
  *     devtools `cacheWrite` stream monkey-patches (must exist or `patch()` throws)
  *   - `queryManager.mutationStore` — `{id: {mutation,variables,loading,error}}` (Mutations tab)
  *   - `getObservableQueries("active")` — iterable of observable-query shims (Queries tab)
  *   - `getMemoryInternals()` — kyo-apollo's real cache sizes/limits under Apollo's
  *     fixed memory-slot labels (an approximation; see `buildMemoryInternals`)
  */
object ApolloDevtools:

    /** Node's CommonJS `require`, faceted so graphql-js `parse` can be pulled in
      * lazily — bundled by esbuild in the browser, resolved from `node_modules`
      * under Node. Mirrors the `ws` fallback idiom in `JsWebSocketEngine`.
      */
    @js.native
    @JSGlobal("require")
    private def require(module: String): js.Dynamic = js.native

    /** graphql-js `parse`: turns an operation's SDL document string into the
      * `DocumentNode` AST the devtools panel `print()`s. Lazy so the `require` only
      * fires when a client is actually connected.
      */
    private lazy val graphqlParse: String => js.Any =
        val parse = require("graphql").parse
        (sdl: String) => parse(sdl).asInstanceOf[js.Any]

    /** Connect `builder`'s client to the browser devtools: prepend the recording
      * interceptor, build, and install the window hook. This is the terminal build
      * step — use it in place of `builder.build()`.
      *
      * @param parse turns an operation's SDL document into the graphql-js
      *              `DocumentNode` AST the panel `print()`s. Inject the app's bundled
      *              graphql-js `parse` (esbuild bundles it); defaults to a lazily
      *              `require`d one for Node.
      */
    def connect(
        builder: ApolloClient.Builder,
        name: String,
        parse: String => js.Any = graphqlParse
    ): ApolloClient =
        val ops = new DevtoolsOperationStore()
        builder.prependInterceptor(new DevtoolsInterceptor(ops))
        val client = builder.build()
        install(client, name, ops, parse)
    end connect

    /** Install the window devtools hook for an already-built `client` fed by `ops`.
      * A no-op (returning the client unchanged) outside a browser, so a client built
      * server-side/in tests never throws on the missing `window`.
      */
    def install(
        client: ApolloClient,
        name: String,
        ops: DevtoolsOperationStore,
        parse: String => js.Any = graphqlParse
    ): ApolloClient =
        val window = js.Dynamic.global.window
        if js.isUndefined(window) then client
        else
            // Memoize parse: documents are immutable per record, and the panel polls the
            // getters ~1s, so without this every op is re-parsed once per second. Bounded
            // (a real cap, so the devtools memory view can show a genuine size/limit pair).
            val astCap   = 100
            val astCache = scala.collection.mutable.Map.empty[String, js.Any]
            val memoParse: String => js.Any = sdl =>
                astCache.getOrElseUpdate(
                    sdl, {
                        if astCache.size >= astCap then astCache.clear()
                        parse(sdl)
                    }
                )
            val shim = buildShim(
                name = name,
                extract = includeOptimistic => extractOf(client, includeOptimistic),
                queries = () => ops.queriesSnapshot,
                mutations = () => ops.mutationsSnapshot,
                parse = memoParse,
                memory = () => buildMemoryInternals(client, ops, astCache.size, astCap)
            )
            installOn(window, shim)
            client
        end if
    end install

    /** The client's normalized cache as devtools JSON, or an empty object when no
      * normalized cache is installed (so the Cache tab stays empty, not broken).
      */
    private def extractOf(client: ApolloClient, includeOptimistic: Boolean): Json =
        try client.apolloStore.extract(includeOptimistic)
        catch case _: Throwable => Json.JObj(Map.empty)

    /** Build the JS client-shim object the devtools extension reflects on. Pure over
      * its inputs (no window, no client), so it is unit-testable with fakes. The
      * getters recompute on every access so the polled tabs stay live.
      */
    private[devtools] def buildShim(
        name: String,
        extract: Boolean => Json,
        queries: () => List[QueryRecord],
        mutations: () => List[MutationRecord],
        parse: String => js.Any,
        memory: () => js.Any
    ): js.Dynamic =
        val shim = js.Dynamic.literal()
        shim.updateDynamic("version")("4.0.0")
        shim.updateDynamic("devtoolsConfig")(js.Dynamic.literal(name = name))

        // cache.extract() returns fresh normalized JSON; the four write methods are
        // no-ops that exist only so the devtools cacheWrite `patch()` can wrap them.
        val cache = js.Dynamic.literal()
        val extractFn: js.Function1[js.Any, js.Any] = (optimistic: js.Any) =>
            val include = js.isUndefined(optimistic) || optimistic.asInstanceOf[Boolean]
            jsonToJs(extract(include))
        cache.updateDynamic("extract")(extractFn)
        val noop: js.Function = (() => ()): js.Function0[Unit]
        cache.updateDynamic("write")(noop)
        cache.updateDynamic("modify")(noop)
        cache.updateDynamic("writeQuery")(noop)
        cache.updateDynamic("writeFragment")(noop)
        shim.updateDynamic("cache")(cache)

        // queryManager.mutationStore is a live getter, rebuilt from the op store each read.
        val queryManager = js.Dynamic.literal()
        defineGetter(queryManager, "mutationStore", () => buildMutationStore(mutations(), parse))
        shim.updateDynamic("queryManager")(queryManager)

        // getObservableQueries(filter) returns a fresh js.Array (iterable) each poll.
        val getObservableQueries: js.Function1[js.Any, js.Any] = (_: js.Any) =>
            buildObservableQueries(queries(), parse)
        shim.updateDynamic("getObservableQueries")(getObservableQueries)

        // getMemoryInternals: kyo-apollo's REAL cache sizes/limits surfaced under
        // Apollo's fixed memory-slot labels (an approximation — kyo-apollo has no
        // Apollo memoization caches; see buildMemoryInternals). Recomputed each poll.
        val getMemoryInternals: js.Function0[js.Any] = () => memory()
        shim.updateDynamic("getMemoryInternals")(getMemoryInternals)

        shim.updateDynamic("stop")(noop)
        shim
    end buildShim

    /** Install `shim` into `window`: push into `Symbol.for("apollo.devtools")` (the
      * hook's registration trap) and set `__APOLLO_CLIENT__` (the legacy path); both
      * funnel to the extension's `registerClient`.
      */
    private[devtools] def installOn(window: js.Dynamic, shim: js.Dynamic): Unit =
        val reflect  = js.Dynamic.global.Reflect
        val sym      = js.Dynamic.global.Symbol.applyDynamic("for")("apollo.devtools")
        val existing = reflect.get(window, sym)
        discard {
            if js.isUndefined(existing) || existing == null then
                reflect.set(window, sym, js.Array(shim.asInstanceOf[js.Any]))
            else existing.applyDynamic("push")(shim)
        }
        window.updateDynamic("__APOLLO_CLIENT__")(shim)
    end installOn

    private def buildMutationStore(ms: List[MutationRecord], parse: String => js.Any): js.Dynamic =
        val store = js.Dynamic.literal()
        ms.zipWithIndex.foreach { (m, i) =>
            store.updateDynamic(i.toString)(
                js.Dynamic.literal(
                    mutation = parse(m.document),
                    variables = jsonToJs(m.variables),
                    loading = m.loading,
                    error = m.error.fold[js.Any](null)(errorObj)
                )
            )
        }
        store
    end buildMutationStore

    private def buildObservableQueries(
        qs: List[QueryRecord],
        parse: String => js.Any
    ): js.Array[js.Any] =
        val arr = new js.Array[js.Any]()
        qs.foreach { q =>
            arr.push(
                js.Dynamic.literal(
                    query = parse(q.document),
                    variables = jsonToJs(q.variables),
                    options = js.Dynamic.literal(),
                    pollingInfo = js.undefined,
                    getCurrentResult = (
                        () =>
                            js.Dynamic.literal(
                                networkStatus = q.networkStatus,
                                error = q.error.fold[js.Any](null)(errorObj)
                            )
                    ): js.Function0[js.Any],
                    getCacheDiff = (
                        () => js.Dynamic.literal(result = q.data.fold[js.Any](js.Dynamic.literal())(jsonToJs))
                    ): js.Function0[js.Any]
                )
            )
        }
        arr
    end buildObservableQueries

    /** kyo-apollo's real cache sizes + limits, mapped onto the Apollo Client Devtools
      * "Memoization cache" tab's fixed slot names. **Approximation:** kyo-apollo has
      * none of Apollo's internal memoization caches (print / executeSelectionSet / …),
      * so these are genuine kyo-apollo caches surfaced under the nearest Apollo label
      * (a row shows only when both a size and a limit are present):
      *   - `print` ← the bounded document→AST parse cache (size / cap)
      *   - `inMemoryCache.executeSelectionSet` ← the normalized cache (records / maxSize)
      *   - `queryManager.getDocumentInfo` ← the recorded-mutation history (count / maxMutations)
      * `limits` is flat & dot-keyed; `sizes` is nested — the shape the tab's formatter
      * joins by dotted key. `links` / `documentTransforms` must be arrays or it throws.
      */
    private def buildMemoryInternals(
        client: ApolloClient,
        ops: DevtoolsOperationStore,
        astSize: Int,
        astCap: Int
    ): js.Any =
        val recordCount =
            try client.apolloStore.cache.allRecords().size
            catch case _: Throwable => 0
        val cacheLimit =
            try client.apolloStore.cache.sizeLimit
            catch case _: Throwable => None

        val sizes = js.Dynamic.literal()
        sizes.updateDynamic("print")(astSize)
        sizes.updateDynamic("links")(new js.Array[js.Any]())
        val queryManager = js.Dynamic.literal()
        queryManager.updateDynamic("getDocumentInfo")(ops.mutationsSnapshot.size)
        queryManager.updateDynamic("documentTransforms")(new js.Array[js.Any]())
        sizes.updateDynamic("queryManager")(queryManager)
        val inMemoryCache = js.Dynamic.literal()
        inMemoryCache.updateDynamic("executeSelectionSet")(recordCount)
        sizes.updateDynamic("inMemoryCache")(inMemoryCache)

        val limits = js.Dynamic.literal()
        limits.updateDynamic("print")(astCap)
        limits.updateDynamic("queryManager.getDocumentInfo")(ops.maxMutations)
        cacheLimit.foreach(l => limits.updateDynamic("inMemoryCache.executeSelectionSet")(l))

        js.Dynamic.literal(limits = limits, sizes = sizes)
    end buildMemoryInternals

    /** A minimal Apollo `ErrorLike` (`{name, message, stack}`) — what the devtools
      * `serializeErrorLike` reads off a query/mutation error.
      */
    private def errorObj(message: String): js.Any =
        js.Dynamic.literal(name = "Error", message = message, stack = "")

    /** Convert an internal [[Json]] value to its plain JS equivalent. */
    private def jsonToJs(json: Json): js.Any = json match
        case Json.JNull    => null
        case Json.JBool(b) => b
        case Json.JNum(n)  => n
        case Json.JStr(s)  => s
        case Json.JArr(items) =>
            val arr = new js.Array[js.Any]()
            items.foreach(v => arr.push(jsonToJs(v)))
            arr
        case Json.JObj(fields) =>
            val obj = js.Dynamic.literal()
            fields.foreach((k, v) => obj.updateDynamic(k)(jsonToJs(v)))
            obj
        case Json.JUpload(_) => null // uploads never reach the cache/devtools view

    private def defineGetter(target: js.Dynamic, prop: String, get: () => js.Any): Unit =
        val descriptor = js.Dynamic.literal(
            configurable = true,
            enumerable = true,
            get = (() => get()): js.Function0[js.Any]
        )
        discard(js.Dynamic.global.Object.applyDynamic("defineProperty")(target, prop, descriptor))
    end defineGetter
end ApolloDevtools

/** Fluent entry point: `builder.serverUrl(...).normalizedCache(...).connectToDevtools("name")`
  * replaces the terminal `.build()` and wires the client to the browser devtools.
  */
extension (builder: ApolloClient.Builder)
    def connectToDevtools(name: String): ApolloClient = ApolloDevtools.connect(builder, name)

    /** As [[connectToDevtools]], injecting the app's bundled graphql-js `parse`
      * (so the browser bundle carries a real AST parser for the Queries/Mutations tabs).
      */
    def connectToDevtools(name: String, parse: String => js.Any): ApolloClient =
        ApolloDevtools.connect(builder, name, parse)
end extension
