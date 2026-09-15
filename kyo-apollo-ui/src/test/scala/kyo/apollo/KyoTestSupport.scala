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

    private def userSelections: Chunk[CompiledSelection] = Chunk(
        CompiledField("__typename", CompiledNamedType("String")),
        CompiledField("id", CompiledNamedType("String")),
        CompiledField("name", CompiledNamedType("String"))
    )

    // Normalizable: the suites write this query's data into the store as well as read it.
    final case class CurrentUserQuery() extends Query.Normalizable[UserData]:
        def name                           = "CurrentUser"
        def document                       = "query CurrentUser { user { __typename id name } }"
        val dataCodec: JsonCodec[UserData] = JsonCodec.fromSchema[UserData]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections =
                    Chunk(CompiledField("user", CompiledNamedType("User"), selections = userSelections))
            )
        // No variables: an empty variables object (matches the new `Operation.variables`).
        def variables: kyo.apollo.json.Json = kyo.apollo.json.Json.JObj(Map.empty)
    end CurrentUserQuery

    def userData(name: String): UserData = UserData(User("User", "1", name))

    /** Wait until `signal` holds a value satisfying `p`, and yield that value — a
      * barrier on the value itself rather than a pause: the observer reads the
      * current value when it attaches, so a value that is already there, or lands
      * while it attaches, is not missed.
      */
    def awaitSignal[A](signal: Signal[A])(p: A => Boolean)(using Frame): A < Async =
        Scope.run {
            Promise.initWith[A, Any] { reached =>
                Fiber.init(signal.observe(value => if p(value) then reached.completeDiscard(Result.succeed(value)) else Kyo.unit))
                    .andThen(reached.get)
            }
        }

    /** A clean `{ data: { user } }` payload naming `capital`/`name` via `name`. */
    def body(name: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}}}"""

    /** A partial payload: `data` present *and* a GraphQL `errors` entry. */
    def partialBody(name: String, error: String): String =
        s"""{"data":{"user":{"__typename":"User","id":"1","name":"$name"}},""" +
            s""""errors":[{"message":"$error"}]}"""

    // --- fake HTTP engines (the "fake transport" reused from earlier phases) ----

    /** Answers every request with the same `(status, body)`. A non-2xx status is
      * folded by the transport into an `ApolloResponse.error` value.
      */
    final class StaticEngine(responseBody: String, status: Int = 200) extends HttpEngine:
        private val received = AtomicInt.Unsafe.init(0)(using AllowUnsafe.embrace.danger).safe

        /** How many requests reached this engine. */
        def calls(using Frame): Int < Sync = received.get

        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            received.incrementAndGet.andThen(HttpEngine.response(HttpStatus(status), responseBody))
    end StaticEngine

    /** Parks every reply on a gate until [[release]] is called, so a watcher's
      * first (network) emission can be deferred deterministically — the reactive
      * analog of `core`'s gated transport, letting a test observe `Loading` before
      * the response, then `Success` after.
      */
    final class GatedEngine(responseBody: String) extends HttpEngine:
        private given Frame = Frame.internal
        private val unsafe  = AllowUnsafe.embrace.danger
        private val gate    = Fiber.Promise.Unsafe.init[Unit, Any]()(using unsafe).safe
        private val arrived = Channel.Unsafe.init[Unit](Int.MaxValue)(using summon[Frame], unsafe).safe

        /** Completes once the next request has reached the engine and is parked —
          * the barrier a test waits on before asserting that a reply is pending,
          * instead of a pause. Each arrival is handed out once.
          */
        def nextRequest(using Frame): Unit < Async =
            Abort.run[Closed](arrived.take).map(_.getOrThrow)

        /** Release the gate so every parked (and future) reply resolves. Idempotent. */
        def release(using Frame): Unit < Sync = gate.completeUnitDiscard

        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            Abort.run[Closed](arrived.offer(()))
                .andThen(gate.get)
                .andThen(HttpEngine.response(HttpStatus.OK, responseBody))
    end GatedEngine

    private val serverUrl = "https://example.invalid/graphql"

    /** A client with no cache — enough for the effect form, whose `.data` /
      * `.response` run `execute()` straight through the network transport. Owned by
      * the enclosing `Scope`.
      */
    def cacheless(engine: HttpEngine)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(ApolloClient.Config(serverUrl).httpEngine(engine))

    /** A client with a normalized cache keyed by `id`, so a query read and a store
      * write share the `User:1` record — required by `watch()` / `watchSignal`.
      * Owned by the enclosing `Scope`.
      */
    def cached(engine: HttpEngine)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(
            ApolloClient.Config(serverUrl)
                .httpEngine(engine)
                .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
        )
end CountryFixture
