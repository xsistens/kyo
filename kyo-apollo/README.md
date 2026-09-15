<!-- doctest:default scope=inherited -->

<!-- doctest:setup
```scala
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.RootMutation
import kyo.apollo.api.RootQuery
import kyo.apollo.api.RootSubscription
import kyo.apollo.api.ScalarCodec
import kyo.apollo.api.SelectionBuilder
import kyo.apollo.api.TypeName

// The selector layer kyo-apollo-codegen emits for the schema shown in "Getting Started",
// written out by hand so this README compiles against kyo-apollo alone.
sealed trait Country

object Country:
    given TypeName[Country] = TypeName("Country")

    def code: SelectionBuilder.Deferrable[Country, (code: String)] =
        SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.id)

    def name: SelectionBuilder.Deferrable[Country, (name: String)] =
        SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

    def capital: SelectionBuilder.Deferrable[Country, (capital: Maybe[String])] =
        SelectionBuilder.scalar("capital", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string))
end Country

object Queries:
    def country[A](code: String)(
        sel: SelectionBuilder.Bidirectional[Country, A]
    ): SelectionBuilder.Deferrable[RootQuery, (country: Maybe[A])] =
        SelectionBuilder.obj(
            "country",
            CompiledNamedType("Country"),
            Chunk(SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, ScalarCodec.id.encode(code))),
            sel,
            SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
        )

    def countries[A](
        sel: SelectionBuilder.Bidirectional[Country, A]
    ): SelectionBuilder.Deferrable[RootQuery, (countries: Chunk[A])] =
        SelectionBuilder.obj(
            "countries",
            CompiledNamedType("Country").notNull.list.notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
        )
end Queries

object Mutations:
    def updateCapital[A](code: String, capital: String)(
        sel: SelectionBuilder.Bidirectional[Country, A]
    ): SelectionBuilder.Deferrable[RootMutation, (updateCapital: A)] =
        SelectionBuilder.obj(
            "updateCapital",
            CompiledNamedType("Country").notNull,
            Chunk(
                SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, ScalarCodec.id.encode(code)),
                SelectionBuilder.Arg("capital", CompiledNamedType("String").notNull, ScalarCodec.string.encode(capital))
            ),
            sel,
            SelectionBuilder.Nesting.Leaf
        )
end Mutations

object Subscriptions:
    def countryUpdated[A](
        sel: SelectionBuilder.Bidirectional[Country, A]
    ): SelectionBuilder.Deferrable[RootSubscription, (countryUpdated: A)] =
        SelectionBuilder.obj(
            "countryUpdated",
            CompiledNamedType("Country").notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Leaf
        )
end Subscriptions

val config = ApolloClient.Config("https://countries.example/graphql")
```
-->

# kyo-apollo

A GraphQL client for kyo. You write an operation as ordinary Scala against selector
objects generated from the schema, and its result type is inferred from the fields you
select. The client runs queries and mutations over HTTP and subscriptions over a
WebSocket, keeps a normalized cache whose watches re-emit when the data they read
changes, and assembles `@defer` and `@stream` responses as they arrive. It runs on the
JVM, Scala Native, Scala.js and WebAssembly, sending through kyo-http on the first two
and through the browser's `fetch` and `WebSocket` on the other two.

Its design follows apollo-kotlin: a normalized cache read and written through fetch
policies, a chain of interceptors in front of the network, and both WebSocket
subscription protocols. The API surface is kyo's own: `Maybe`/`Chunk` results,
failures as values and typed `Abort` rows, and clients and watches owned by a `Scope`.

Three companion modules complete it:

- [kyo-apollo-codegen](../kyo-apollo-codegen/README.md) generates the selector objects
  from a schema.
- [kyo-apollo-testing](../kyo-apollo-testing/README.md) provides scripted engines,
  transports and a mock WebSocket server for tests without a network.
- [kyo-apollo-itserver](../kyo-apollo-itserver/README.md) is the GraphQL server the
  cross-platform end-to-end suite runs against (not published).

## Getting Started

Add the dependency to your `build.sbt`:

```scala doctest:expect=skipped
libraryDependencies += "io.getkyo" %%% "kyo-apollo" % "<latest version>"
```

The examples in this README use a small schema:

```graphql
type Country {
  code: ID!
  name: String!
  capital: String
}

type Query {
  country(code: ID!): Country
  countries: [Country!]!
}

type Mutation {
  updateCapital(code: ID!, capital: String!): Country!
}

type Subscription {
  countryUpdated: Country!
}
```

From it, kyo-apollo-codegen generates one selector object per type (`Country`) and one
per operation root (`Queries`, `Mutations`, `Subscriptions`). A selection combines field
selectors with `~`; `toQuery()` turns a selection rooted in `Queries` into an operation:

```scala
import kyo.*
import kyo.apollo.*
import kyo.apollo.api.*

val germany = Queries.country("DE")(Country.code ~ Country.name ~ Country.capital).toQuery()
```

The result type of `germany` is `(country: Maybe[(code: String, name: String, capital: Maybe[String])])`:
a nullable field decodes to `Maybe`, a list field to `Chunk`, and nothing about it is
written by hand.

<!-- doctest:scope=inherited
```scala
val germanyIsTyped: kyo.apollo.api.Query.Normalizable[(country: Maybe[(code: String, name: String, capital: Maybe[String])])] =
    germany
```
-->

A client is created from an `ApolloClient.Config` value, whose only required field is
the server URL. `ApolloClient.use` creates the client, runs a block with it, and closes
it when the block ends:

```scala
val capital: Maybe[String] < Async =
    ApolloClient.use(ApolloClient.Config("https://countries.example/graphql")) { client =>
        Scope.run(client.query(germany).execute).map { response =>
            response.data.flatMap(_.country).flatMap(_.capital)
        }
    }
```

`client.query(germany)` only prepares an `ApolloCall`; nothing is sent until the call is
executed. `execute` sends the request and returns the first `ApolloResponse`: its
`data` holds the decoded result, and its `error` says why a call fell short (see
[Failures](#failures)).

Inside a longer-lived program, `ApolloClient.init` creates the client in the enclosing
`Scope` instead, and the end of that `Scope` closes it:

```scala
val client: ApolloClient < (Sync & Scope) =
    ApolloClient.init(ApolloClient.Config("https://countries.example/graphql"))
```

A `Config` is an immutable value. Its fluent methods (`addHttpHeader`,
`addInterceptor`, `webSocketServerUrl`, `httpEngine`, ...) return a copy, so deriving a
variant never changes the original, and every client created from it owns its own
socket.

## Queries, Mutations and Subscriptions

The three operation kinds share one call type. A mutation is built with `toMutation()`
and executed like a query:

```scala
val moveCapital =
    Mutations.updateCapital("DE", "Bonn")(Country.code ~ Country.capital).toMutation()

def move(client: ApolloClient): Maybe[String] < (Async & Scope) =
    client.mutation(moveCapital).execute.map { response =>
        response.data.flatMap(_.updateCapital.capital)
    }
```

A subscription is a stream. `stream` returns the call's responses as a kyo `Stream`
that opens the WebSocket when it is consumed and unsubscribes when its `Scope` closes:

```scala
val updates = Subscriptions.countryUpdated(Country.code ~ Country.capital).toSubscription()

def firstThree(client: ApolloClient): Chunk[Maybe[String]] < (Async & Scope) =
    client.subscription(updates).stream
        .take(3)
        .map(_.data.flatMap(_.countryUpdated.capital))
        .run
```

Most servers accept subscriptions on a URL of their own; set it with
`config.webSocketServerUrl("wss://countries.example/graphql")`. The default protocol is
`graphql-transport-ws`; `wsProtocol(SubscriptionWsProtocol)` selects the older
`subscriptions-transport-ws`. Reconnection is opt-in through `webSocketReopenWhen`.

`execute` on a query or mutation is the first element of `stream`, so every
operation runs through the same interceptor chain, and a cache-then-network read is
just a stream with two elements.

## What a selection can do

What a selection supports is its type, so a misuse does not compile:

- Every `SelectionBuilder[Origin, A]` decodes a response (`decode` returns a
  `Result[ApolloParseException, A]`). A `SelectionBuilder.Bidirectional` also
  encodes: named-tuple selections and `mapInto[C]` projections are, `map(f)`
  projections are not.
- Only `SelectionBuilder.Fields` (named-tuple selections) combine with `~`.
- Only `SelectionBuilder.Deferrable` selections, whose last-added operand is a
  single field selector, take `.deferred` and `.streamed`; `.streamed` also needs
  that field to be a list. The empty selection, a `defer(...)` group, a union
  branch, a fragment spread and a `@client` field do not.
- A nested field takes a bidirectional child, so `map` projects the whole selection
  an operation is built from, never a child (use `mapInto` there).
- A bidirectional root builds a normalizable operation (`Query.Normalizable`, ...),
  the only kind `ApolloStore.writeOperation`/`updateOperation` accept. A query built
  from a `map` projection runs and decodes normally, but the cache interceptor does
  not normalize its responses; it logs each skip at debug level.

Deferring a field makes its value `Maybe` until the deferred part arrives, and the
response stream emits again when a part changes the data:

```scala
val progressive = Queries.country("DE")(Country.code ~ Country.capital.deferred).toQuery()
```

The result type is `(country: Maybe[(code: String, capital: Maybe[Maybe[String]])])`.
Each emitted `ApolloResponse` has `complete = false` while parts are still outstanding.

<!-- doctest:scope=inherited
```scala
val progressiveIsTyped: kyo.apollo.api.Query.Normalizable[(country: Maybe[(code: String, capital: Maybe[Maybe[String]])])] =
    progressive
```
-->

A list field marked `.streamed(initialCount)` keeps its `Chunk` type and grows across
emissions as the server appends the remaining items.

## Failures

A failed operation is a response, not an exception. `ApolloResponse.error` is the one
failure channel: a network drop, a non-2xx status, a body that does not parse, a cache
miss under `CacheOnly`, and the server's own GraphQL `errors` all land there as a
`Maybe[ApolloException]`. The server's errors are also readable as typed
`GraphQLError`s through `response.errors`, and `data` can be present next to them
(GraphQL's partial result).

`ApolloException` is sealed, so a handler can name every case:

```scala
import kyo.apollo.exception.*

def describe(error: ApolloException): String =
    error match
        case e: ApolloGraphQLException         => s"the server answered with ${e.errors.size} error(s)"
        case e: ApolloHttpException            => s"HTTP status ${e.statusCode}"
        case e: ApolloNetworkException         => "no response was received"
        case e: ApolloParseException           => s"expected ${e.expected}"
        case e: ApolloWebSocketClosedException => s"the socket closed with code ${e.code}"
        case e: CacheMissException             => s"not cached: ${e.key.render}"
        case e: NoCacheIdentityException       => s"no cache identity for ${e.typeName}"
        case e: DefaultApolloException         => e.message
        case e: ApolloConfigException          => e.message
```

The leaves also mix in traits that group them by where they come from, so an effect
row can name exactly the failures of its operation: `ApolloExecuteFailure` for
executing an operation, `CacheReadFailure` for a store read (the row of
`ApolloStore.readOperation` and `readFragment`), `HttpEngineFailure` for an HTTP
round-trip that received no response (the row of `HttpEngine.execute` and of every
`HttpInterceptor`). `ApolloConfigException` belongs to none: it is a construction
error. To move a response onto a row, fail with its error and unwrap the data with
`orFailWith`:

```scala
import kyo.apollo.exception.*

def requireCapital(client: ApolloClient): String < (Async & Scope & Abort[ApolloException]) =
    client.query(germany).execute.map { response =>
        response.error match
            case Present(error) => Abort.fail(error)
            case Absent =>
                response.data.flatMap(_.country).flatMap(_.capital)
                    .orFailWith(DefaultApolloException("Germany has no capital in the response"))
    }
```

A decoder defect, such as a codec that throws, is not a failure value: it stays a
panic, so it is never mistaken for a response the server sent.

## Normalized cache

`normalizedCache` installs a cache on a `Config`; every client created from that
`Config` shares its store. Records are keyed by type and identity, so the same country
read by two different queries is stored once, and a write to it reaches every watch
that read it. A `CacheIdentity` declares which fields identify a type:

```scala
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*

given CacheIdentity[Country] = CacheIdentity.by(_ ~ Country.code)

val cached: ApolloClient.Config =
    config.normalizedCache(MemoryCache(maxSize = 10_000), CacheIdentity.generator(summon[CacheIdentity[Country]]))
```

A type without an identity is keyed by its `id` (or `_id`) field, or by its path
below the nearest keyed parent. kyo-apollo-codegen emits `SchemaIdentities.generator`, which
collects every `CacheIdentity` given in scope.

Each call picks a `FetchPolicy`: `CacheFirst` (the default), `NetworkOnly`,
`CacheOnly`, `NetworkFirst`, `CacheAndNetwork`, `NoCache` or `Standby`. `watch()`
turns a query into a live stream: it emits the first result under the call's policy
and emits again whenever a record that result was read from changes, whether a
mutation response, a subscription event or a direct store write changed it:

```scala
import kyo.apollo.cache.normalized.*

def capitals(client: ApolloClient): Chunk[Maybe[String]] < (Async & Scope) =
    client.query(germany).fetchPolicy(FetchPolicy.CacheAndNetwork).watch()
        .take(3)
        .map(_.data.flatMap(_.country).flatMap(_.capital))
        .run
```

A watch re-reads the cache when its records change (`RefetchPolicy.CacheOnly`, the
default); `refetchPolicy(RefetchPolicy.NetworkOnly)` asks the server again instead. An
unrelated write notifies nobody, and closing the watch's `Scope` ends it.

The store behind the cache is `client.apolloStore`. Writes through it publish the
records they change, so watches react to them as they do to network responses, and
reads fail on the `Abort[CacheReadFailure]` row when the cache does not hold every
selected field:

```scala
import kyo.apollo.cache.normalized.*
import kyo.apollo.exception.CacheReadFailure

def renameLocally(client: ApolloClient): Maybe[String] < (Sync & Abort[CacheReadFailure]) =
    val store = client.apolloStore
    for
        _       <- store.writeOperation(germany, (country = Present((code = "DE", name = "Deutschland", capital = Present("Berlin")))))
        current <- store.readOperation(germany)
    yield current.country.map(_.name)
    end for
end renameLocally
```

A mutation can show its result before the server answers: `optimisticUpdates(data)`
writes `data` as a layer on top of the cache, watches see it at once, and the layer is
replaced by the real response or dropped if the mutation fails.

`garbageCollect` removes the records no operation root reaches any more. A live watch
keeps the records of its last read, and `retain` keeps chosen records for as long as
its `Scope` is open, for example an entity that only a fragment reads:

```scala
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey

def collectKeeping(client: ApolloClient): Set[CacheKey] < (Sync & Async) =
    Scope.run {
        client.apolloStore.retain(Set(CacheKey("Country", "DE")))
            .andThen(client.apolloStore.garbageCollect)
    }
```

`MemoryCache` also bounds the cache by record count (LRU) and expires records and
fields by age (`expireAfterMillis`, `maxAge`).

## Type conventions

The module follows kyo's `CONTRIBUTING.md`: **model types and return types carry
kyo data types; the standard library appears only as input.**

- A nullable GraphQL field decodes to `Maybe[T]`, a list field to `Chunk[T]`
  (`SelectionBuilder.Nesting`, `ScalarCodec.maybe` / `ScalarCodec.chunk`); the
  named-tuple result of every query is therefore `Maybe`/`Chunk`-typed, and the
  codegen emits the same types for selectors, input case classes and
  argument defaults (`= Absent`).
- `CompiledField.alias`/`stream`, `CompiledFragment.defer`, `DeferDirective.if`,
  `StreamDirective.if` are `Maybe`; `selections`, `arguments`, `possibleTypes`
  are `Chunk`.
- `FieldPolicy(keyArgs: Maybe[Chunk[String]], read: Maybe[...], merge: Maybe[...])`;
  a `FieldValueMerger` receives the existing value as `Maybe[RecordValue]`.
- A union branch (`SelectionBuilder.onType`) decodes to `Maybe[A]`; a `@defer`
  group to `Maybe[S]`.
- The happy-path unwrap of a `Maybe` into a typed error channel is
  `maybe.orFailWith(e: E)` (`MaybeOps`, `E <: ApolloException`).
- Inputs that only collect values (`FieldPolicies.fromList(policies: Seq[...])`,
  `ClientField.writeAll(values: Seq[...])`) accept any `Seq`, `Chunk` included.

### Documented exceptions

- **`Map[String, Json]`**: a JSON object (`Json.JObj`) is a keyed map by
  definition; there is no kyo map type, and insertion order (which the printer
  and the cache rely on) is kept with `scala.collection.immutable.VectorMap`.
  The same holds for `variables: Map[String, Json]` and record field maps.
- **`Set[String]`**: the set of changed record keys published after a write
  (`NormalizedCache.merge`, `ChangedKeysSubject`, `ClientField.write*`) is a
  membership set with no ordering; kyo has no `Set`, so `scala.collection.immutable.Set`
  stays.

## HTTP layer

The HTTP engine speaks kyo-http's types. An `HttpEngine.Request` is a
`kyo.HttpRequest["body" ~ HttpRequestBody]` (a `kyo.HttpMethod`, a `kyo.HttpUrl`,
`kyo.HttpHeaders`), an `HttpEngine.Response` is a `kyo.HttpResponse["body" ~ String]`,
which is what `kyo.HttpClient` returns for a text body. `ApolloClient.Config.httpHeaders`,
`ApolloRequest.httpHeaders` and `ApolloHttpException.headers` are `kyo.HttpHeaders`;
`httpMethod` is a `kyo.HttpMethod`: `GET` puts the operation in the URL, any other
method (`POST` by default) carries it in the body. A file cannot ride a URL, so a `GET`
whose variables carry an `Upload` is sent as a multipart `POST`. A server URL that does
not parse makes each response an `ApolloNetworkException` whose cause is kyo-http's
parse failure. Interrupting a request cancels it on the wire: the JVM/Native engine
interrupts kyo-http's request, the JS/Wasm engine aborts the `fetch` through its
`AbortController` (a streamed request when its `Scope` closes).

kyo-apollo keeps its own types only where kyo-http has none:

- **`HttpRequestBody`** (`Empty` | `Text(json)` | `Multipart(parts)`): kyo-http carries
  a request body as a route field typed per form (`bodyText`, `bodyMultipart`, none);
  the engine seam needs one type for the three forms a GraphQL request takes. The parts
  of a multipart body are `kyo.HttpRequest.Part`s, which hold both the text fields and
  the files of the graphql-multipart-request-spec. `kyo.HttpFormCodec` does not cover
  that spec: it encodes `application/x-www-form-urlencoded`, which has no file parts.
- **`HttpStreamBody`** (`Buffered` | `Chunked`): kyo-http hands a live response body
  only inside `HttpClient.sendWith`'s continuation; the engine returns it to the
  transport as a lazy stream, in an `HttpEngine.StreamResponse`
  (`kyo.HttpResponse["body" ~ HttpStreamBody]`).

The engine seam itself exists because kyo-http's JS transport is bound to Node sockets
and does not run in a browser: on JS/Wasm the engine sends through `fetch`, on
JVM/Native it delegates to `kyo.HttpClient`.

Interceptors wrap the engine on two tiers. `addHttpInterceptor` adds an
`HttpInterceptor`, which sees requests and responses on the wire
(`AuthorizationHeaderInterceptor`, `LoggingInterceptor`, `BatchingHttpInterceptor`).
`addInterceptor` adds an `ApolloInterceptor`, which sees operations and response
streams (`AutoPersistedQueryInterceptor`, `RetryOnErrorInterceptor`, the cache).
`LoggingInterceptor` writes no variable value: the `variables` of a JSON body are
redacted and a `GET` request is logged without its query string.

## Devtools

On Scala.js, `ApolloClient.Config.connectToDevtools(name, enabled)`
(`import kyo.apollo.devtools.connectToDevtools`) creates the client in the enclosing
`Scope`, like `ApolloClient.init`, and when `enabled` connects it to the Apollo Client
Devtools browser extension. `enabled` has no default; with `enabled = false` the call is
exactly `ApolloClient.init(config)` and touches no global. The call exists only on the
JS and Wasm rows.

**Warning:** an installed hook exposes the entire normalized cache and every
operation's variables to every script in the document; pass `enabled = isDevBuild`.
The Mutations tab shows a mutation's variables with every value replaced by
`"<redacted>"`, and the Cache tab leaves out `ROOT_MUTATION`, whose keys spell out
a mutation's arguments. Query variables and query data are shown as they are.

## Cross-platform

The API is the same on every row; the engines behind `HttpEngine.default` and
`WebSocketEngine.default` differ, and so does what each row's tests exercise beyond
the shared suites:

| Row | HTTP and WebSocket engines | Tested only on this row |
|-----|----------------------------|-------------------------|
| JVM | kyo-http (`HttpClientEngine`, `KyoHttpWebSocketEngine`) | the engines against a caliban server (`ApolloCalibanSmokeSpec`) |
| Native | kyo-http, the same engines as the JVM | the kyo-http engines over real sockets on Scala Native (`ApolloLiveServerSpec`, shared with the JVM) |
| JS | browser `fetch` and `WebSocket` (`FetchHttpEngine`, `JsWebSocketEngine`) | the fetch and socket engines, the devtools hook, `UploadJs` (`dom.File`/`Blob` to `Upload`) |
| Wasm | the JS engines, compiled by the Scala.js Wasm backend | the same suites as JS |

`Upload(bytes, fileName, contentType)` carries a file as a `Span[Byte]` on every row;
`UploadJs.fromFile` bridges a browser `File` on JS and Wasm.

## Differences from Apollo Client and apollo-kotlin

These differences are deliberate:

- **One failure value.** GraphQL `errors` are not a separate response field: they
  arrive as an `ApolloGraphQLException` in `ApolloResponse.error`, and
  `ApolloResponse.errors` is a projection of it. Store reads fail on a typed
  `Abort[CacheReadFailure]` row instead of throwing.
- **A client is a scoped resource.** `ApolloClient.init` ties the client to a `Scope`,
  and its configuration is an immutable `Config` value, not a mutable builder.
- **No operation classes.** Operations are written inline against the schema's
  selector objects, and their results are named tuples; kyo-apollo-codegen reads the
  schema only, never operation documents.
- **`RefetchPolicy` has three cases**, `CacheOnly`, `NetworkOnly` and `CacheFirst`,
  where apollo-kotlin's `refetchPolicy` takes a whole `FetchPolicy`: only these three
  mean something different for a watch.
- **No field-level `cache.modify`.** A record is updated as a whole and typed, through
  `ApolloStore.updateOperation` or `ApolloStore.updateFragment`.
- **No fragment registry.** Fragments are values (`Fragment.entity`,
  `Fragment.embedded`) that a selection imports and spreads; a printed document inlines
  each spread as `... on Type { ... }` instead of a named `fragment` definition.
- **A retain ends with its `Scope`** instead of a matching `release` call.
- **Incremental delivery** requests `multipart/mixed; deferSpec=20220824` and also
  reads the older single `{data, path}` patch shape.
