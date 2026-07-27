package kyo.apollo

// `kyo` — the enclosing package is `kyo.apollo`, which shadows the getkyo
// library root (see the note in every kyo-ui source). The absolute path pulls in
// `<` / `Async` / `Abort` / `Scope` / `Fiber` / `Sync` / `Frame` / `Result`
// unambiguously.
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** A `CurrentUser` query + `User` record fixture (mirroring `core`'s
  * `ReactivitySpec`), so a query result and an imperative store write share the
  * one normalized `User:1` record — the seam the reactive re-emission flows
  * through.
  */
object CountryFixture:

    // `__typename` is the Scala field name (not `typename`) because kyo-schema
    // `derives Schema` encodes field names verbatim — the wire key must be
    // `__typename` for `IdCacheKeyGenerator`/normalization. Custom codecs are gone;
    // a derived `Schema` replaces the old hand-written `ObjectAdapter`.
    final case class User(__typename: String, id: String, name: String) derives Schema, CanEqual
    final case class UserData(user: User) derives Schema, CanEqual

    private def userSelections: List[CompiledSelection] = List(
        CompiledField("__typename", CompiledNamedType("String")),
        CompiledField("id", CompiledNamedType("String")),
        CompiledField("name", CompiledNamedType("String"))
    )

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections =
                    List(CompiledField("user", CompiledNamedType("User"), selections = userSelections))
            )
        // No variables: an empty variables object (matches the new `Operation.variables`).
        def variables: kyo.apollo.json.Json = kyo.apollo.json.Json.JObj(Map.empty)
    end CurrentUserQuery

    def userData(name: String): UserData = UserData(User("User", "1", name))

    /** A clean `{ data: { user } }` payload naming `capital`/`name` via `name`. */
    def body(name: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}}}"""

    /** A partial payload: `data` present *and* a GraphQL `errors` entry. */
    def partialBody(name: String, error: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}},""" +
            s""""errors":[{"message":"$error"}]}"""

    // --- fake HTTP engines (the "fake transport" reused from earlier phases) ----

    /** Answers every request with the same `(status, body)`. A non-2xx status is
      * folded by the transport into an `ApolloResponse.exception` value.
      */
    final class StaticEngine(responseBody: String, status: Int = 200) extends HttpEngine:
        var calls = 0
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            calls += 1
            HttpResponse(status, Nil, responseBody)
    end StaticEngine

    /** Parks every reply on a gate until [[release]] is called, so a watcher's
      * first (network) emission can be deferred deterministically — the reactive
      * analog of `core`'s gated transport, letting a test observe `Loading` before
      * the response, then `Success` after.
      */
    final class GatedEngine(responseBody: String) extends HttpEngine:
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private given Frame       = Frame.internal
        private val gate: Fiber.Promise[Unit, Any] =
            Sync.Unsafe.evalOrThrow(Fiber.Promise.init[Unit, Any])
        def release(): Unit =
            given AllowUnsafe = AllowUnsafe.embrace.danger
            discard(gate.unsafe.completeUnitDiscard())
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            gate.get.andThen(HttpResponse(200, Nil, responseBody))
    end GatedEngine

    /** A client with no cache — enough for the effect form, whose `.data` /
      * `.response` run `execute()` straight through the network transport.
      */
    def cacheless(engine: HttpEngine): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.invalid/graphql")
            .httpEngine(engine)
            .build()

    /** A client with a normalized cache keyed by `id`, so a query read and a store
      * write share the `User:1` record — required by `watch()` / `watchSignal`.
      */
    def cached(engine: HttpEngine): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.invalid/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()
end CountryFixture
