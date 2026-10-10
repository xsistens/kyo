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

// The selector layer for the schemas shown in this README ("Getting Started", "Uploads",
// "Pagination"), written out by hand so this README compiles against kyo-apollo alone.
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

    def countriesPage[A](first: Int, after: Maybe[String])(
        sel: SelectionBuilder.Bidirectional[CountryConnection, A]
    ): SelectionBuilder.Deferrable[RootQuery, (countriesPage: A)] =
        SelectionBuilder.obj(
            "countriesPage",
            CompiledNamedType("CountryConnection").notNull,
            Chunk(
                SelectionBuilder.Arg("first", CompiledNamedType("Int").notNull, ScalarCodec.int.encode(first)),
                SelectionBuilder.Arg("after", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string).encode(after))
            ),
            sel,
            SelectionBuilder.Nesting.Leaf
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

    def uploadFlag[A](code: String, file: kyo.apollo.Upload)(
        sel: SelectionBuilder.Bidirectional[Country, A]
    ): SelectionBuilder.Deferrable[RootMutation, (uploadFlag: A)] =
        SelectionBuilder.obj(
            "uploadFlag",
            CompiledNamedType("Country").notNull,
            Chunk(
                SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, ScalarCodec.id.encode(code)),
                SelectionBuilder.Arg("file", CompiledNamedType("Upload").notNull, ScalarCodec.upload.encode(file))
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

// The connection types behind `Queries.countriesPage`, shown in "Pagination".
sealed trait CountryConnection
object CountryConnection:
    given TypeName[CountryConnection] = TypeName("CountryConnection")

    def edges[A](sel: SelectionBuilder.Bidirectional[CountryEdge, A])
        : SelectionBuilder.Deferrable[CountryConnection, (edges: Chunk[A])] =
        SelectionBuilder.obj(
            "edges",
            CompiledNamedType("CountryEdge").notNull.list.notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
        )

    def pageInfo[A](sel: SelectionBuilder.Bidirectional[PageInfo, A]): SelectionBuilder.Deferrable[CountryConnection, (pageInfo: A)] =
        SelectionBuilder.obj("pageInfo", CompiledNamedType("PageInfo").notNull, Chunk.empty, sel, SelectionBuilder.Nesting.Leaf)
end CountryConnection

sealed trait CountryEdge
object CountryEdge:
    given TypeName[CountryEdge] = TypeName("CountryEdge")

    def cursor: SelectionBuilder.Deferrable[CountryEdge, (cursor: String)] =
        SelectionBuilder.scalar("cursor", CompiledNamedType("String").notNull, ScalarCodec.string)

    def node[A](sel: SelectionBuilder.Bidirectional[Country, A]): SelectionBuilder.Deferrable[CountryEdge, (node: A)] =
        SelectionBuilder.obj("node", CompiledNamedType("Country").notNull, Chunk.empty, sel, SelectionBuilder.Nesting.Leaf)
end CountryEdge

sealed trait PageInfo
object PageInfo:
    given TypeName[PageInfo] = TypeName("PageInfo")

    def hasNextPage: SelectionBuilder.Deferrable[PageInfo, (hasNextPage: Boolean)] =
        SelectionBuilder.scalar("hasNextPage", CompiledNamedType("Boolean").notNull, ScalarCodec.boolean)

    def endCursor: SelectionBuilder.Deferrable[PageInfo, (endCursor: Maybe[String])] =
        SelectionBuilder.scalar("endCursor", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string))
end PageInfo

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

## When to use kyo-apollo

Reach for kyo-apollo when a kyo program talks to a GraphQL server and the results
should arrive as kyo values: a `Maybe` for a nullable field, a `Stream` for a
subscription, an `Abort` row for a failure, a `Scope` that closes the socket. It gives
you:

- **The compiler checks the operations.** An operation is Scala code against selectors
  generated from the schema, and its result type is built from the fields it selects.
  When the schema drops a field, regenerating the selectors turns every operation that
  still selects it into a compile error at that line. Operations need no `.graphql`
  files and no code generation step of their own.
- **One place to look for failure.** Network drops, HTTP statuses, unparsable bodies,
  cache misses and the server's GraphQL `errors` all arrive in one sealed
  `ApolloException`, so a `match` over it is checked for exhaustiveness.
- **Live data from the cache.** A watched query re-emits whenever any operation, a
  subscription event or a direct store write changes a record it read, which is what a
  screen or a long-running job that mirrors server state needs.
- **The same code on four platforms.** JVM, Scala Native, Scala.js and WebAssembly run
  the same API; only the engine underneath changes.
- **Tests without a server.** kyo-apollo-testing scripts responses at the HTTP, the
  operation and the WebSocket layer.

It is not the right fit when the program does not run on kyo effects (a `Future`, ZIO or
cats-effect code base would wrap every call), when the cache has to survive a restart
(`MemoryCache` is the only shipped `NormalizedCache`; persistence means implementing
one), when codegen has to run in a build other than kyo's
own (there is no sbt plugin yet; such a build runs
`kyo.apollo.codegen.CodegenRunner` itself), or when the server only speaks GraphQL 17's
`pending`/`completed` incremental format.

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

### Configuring the subscription socket

A server that authenticates subscriptions reads a payload from the socket's
`connection_init` message, and a long-lived client has to survive a dropped
connection. Both are `Config` settings:

```scala
import kyo.apollo.json.Json
import kyo.apollo.network.ws.WebSocketNetworkTransport
import scala.collection.immutable.VectorMap

val live: ApolloClient.Config =
    config
        .webSocketServerUrl("wss://countries.example/graphql")
        .webSocketConnectionPayload(Json.JObj(VectorMap("authToken" -> Json.JStr("<token>"))))
        .webSocketReopenWhen(WebSocketNetworkTransport.reconnectAlways)
        .webSocketBackoff(Schedule.exponentialBackoff(500.millis, 2.0, 30.seconds))
        .webSocketConnectTimeout(5.seconds)
        .subscriptionBufferSize(64)
```

On an abnormal drop, every active subscription first receives a response whose `error`
is the drop (an `ApolloWebSocketClosedException`), and its stream stays open. The
predicate passed to `webSocketReopenWhen` receives that failure and the attempt number
and decides whether to reopen; a reopened socket sends `connection_init` again and
resubscribes every active subscription under its original id, waiting between attempts
as `webSocketBackoff` schedules. When the schedule has no delay left, the subscriptions
end with the failure, as they do without reconnection. This is the subscriptions'
retry: `RetryOnErrorInterceptor` never retries a subscription.

`subscriptionBufferSize` (256 by default) bounds how many responses one subscription
holds for a consumer that has not taken them yet. When it is full, the socket is not
read further until that consumer catches up, and since one socket carries every
subscription, a slow consumer holds the others back too.

### Uploads

A form that attaches a file (a flag image, an avatar) sends it inside the mutation. The
file travels as an `Upload`, a `Span[Byte]` with a file name and a content type, and an
operation whose variables carry one is sent as a multipart request following the
GraphQL multipart request spec: under `POST` or another body method as that method,
under `GET` as a `POST`, because a file cannot ride a URL:

```graphql
scalar Upload

type Mutation {
  uploadFlag(code: ID!, file: Upload!): Country!
}
```

```scala
def uploadFlag(client: ApolloClient, png: Span[Byte]): Maybe[String] < (Async & Scope) =
    val flag = Upload(png, "flag.png", "image/png")
    client.mutation(Mutations.uploadFlag("DE", flag)(Country.code ~ Country.name).toMutation()).execute
        .map(_.data.map(_.uploadFlag.name))
end uploadFlag
```

On JS and Wasm, `UploadJs.fromFile` turns a browser `File` into an `Upload`.

kyo-apollo-codegen generates an argument of a schema's `Upload` scalar as an `Upload`
sent through `ScalarCodec.upload`, unless the build maps the scalar to another type. A
file can only be an operation argument: an input object field of type `Upload` stops
generation with an error naming the field, since an input object is encoded without a
place for a file.

## What a selection can do

A selection says which fields an operation reads, and its type says what the operation
can do with them.

A field whose type is a union or an interface returns objects of several types, and a
selection picks fields per type. `SelectionBuilder.onType("Track", child)` selects one
branch; it decodes to `Maybe[A]`, `Absent` when the object is of another type, and
kyo-apollo-codegen emits it as `on<Type>` on the union's selector object
([kyo-apollo-codegen](../kyo-apollo-codegen/README.md) shows it on a generated schema).

A named tuple is the right result while the data stays close to the call. When it moves
into a domain model, project it. `mapInto[C]` decodes the selection into a case class
whose field names match the selected ones, and stays bidirectional, so the operation
still reaches the cache:

```scala
final case class CountryCard(code: String, name: String, capital: Maybe[String]) derives Schema

val card = Queries.country("DE")((Country.code ~ Country.name ~ Country.capital).mapInto[CountryCard]).toQuery()
```

`card` is a `Query.Normalizable[(country: Maybe[CountryCard])]`. `map(f)` takes any
function instead, which makes it decode-only: the operation below returns the capital
directly, but its responses are not normalized and `updateOperation` does not accept it.

```scala
val capitalOfGermany: Query[Maybe[String]] =
    Queries.country("DE")(Country.capital).map(_.country.flatMap(_.capital)).toQuery()
```

Use `mapInto` for anything a watch or a store update touches, and `map` for one-off
reads where only the projected value matters.

<!-- doctest:scope=inherited
```scala
val cardIsTyped: kyo.apollo.api.Query.Normalizable[(country: Maybe[CountryCard])] = card
```
-->

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

To defer several fields as one unit, group them with `defer(label, selection)`. The
label becomes the field name of the group, whose value is `Maybe` until the part
arrives:

```scala
val withDetails = Queries.country("DE")(Country.code ~ defer("details", Country.name ~ Country.capital)).toQuery()
```

<!-- doctest:scope=inherited
```scala
val withDetailsIsTyped: kyo.apollo.api.Query.Normalizable[
    (country: Maybe[(code: String, details: Maybe[(name: String, capital: Maybe[String])])])
] = withDetails
```
-->

A list field marked `.streamed(initialCount)` keeps its `Chunk` type and grows across
emissions as the server appends the remaining items.

### Custom scalars

A schema's own scalars (`DateTime`, `Long`, `JSON`) should decode into a Scala type, not
a string. The mapping is a codegen option, `--scalar DateTime=java.time.Instant` on the
`CodegenRunner` command line, or in kyo's own build the `apolloScalarMappings` setting.
The mapped type needs a kyo `Schema`, which codegen supplies itself for
`java.time.Instant`:

```scala doctest:expect=skipped
apolloScalarMappings := Map("DateTime" -> "java.time.Instant", "Long" -> "scala.Long")
```

The generated selector for a `population: Long!` field then decodes through
`ScalarCodec.fromSchema`, which is also what a hand-written selector uses:

```scala
def population: SelectionBuilder.Deferrable[Country, (population: Long)] =
    SelectionBuilder.scalar("population", CompiledNamedType("Long").notNull, ScalarCodec.fromSchema[Long])
```

A custom scalar without a mapping is generated as `String`, the raw wire value; the one
exception is `Upload`, generated as a file (see [Uploads](#uploads)).

### What the selection types allow

The rules above follow from the selection types, so a misuse does not compile:

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
`HttpInterceptor`), `ApolloParseFailure` for a response envelope that does not decode
(the row of `GraphQLResponse.parse`). `ApolloConfigException` belongs to none: it is a construction
error. To move a response onto a row, fail with its error and unwrap the data with
`orFailWith`:

```scala
import kyo.apollo.exception.*

def requireCapital(client: ApolloClient): String < (Async & Scope & Abort[ApolloException]) =
    client.query(germany).execute.map { response =>
        response.error match
            case Present(error) => Abort.fail(error)
            case Absent         =>
                response.data.flatMap(_.country).flatMap(_.capital)
                    .orFailWith(DefaultApolloException("Germany has no capital in the response"))
    }
```

A decoder defect, such as a codec that throws, is not a failure value: it stays a
panic, so it is never mistaken for a response the server sent.

## Normalized cache

When several operations read the same object, they should see the same value, and a
change to it should reach all of them. The normalized cache does that: records are
keyed by type and identity, so the same country read by two different queries is
stored once, and a write to it reaches every watch that read it. `normalizedCache`
installs it on a `Config`, and every client created from that `Config` shares its
store. A `CacheIdentity` declares which fields identify a type:

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

Each call picks a `FetchPolicy`, by how fresh the result has to be:

- `CacheFirst` (the default) answers from the cache and asks the server only on a miss:
  data that changes rarely.
- `CacheAndNetwork` emits the cached result at once and the server's after it: a screen
  that should show something immediately and then catch up.
- `NetworkFirst` asks the server and falls back to the cache on a network error: a
  client that has to keep working offline. `NetworkOnly` always asks the server and
  writes the answer back.
- `CacheOnly` never asks the server, and a miss is an error value; `Standby` serves a
  hit and emits nothing on a miss, for a query that waits for someone else to fill
  the cache.
- `NoCache` neither reads nor writes the cache: a one-off request whose answer should
  not reach any watch.

> **Note:** `Standby` on a miss produces an empty stream, so `execute`, which waits for
> one response, answers with a response whose `error` says no response arrived. Use
> `Standby` with `watch()` or `stream` to wait for someone else to fill the cache.

`watch()` turns a query into a live stream: it emits the first result under the call's policy
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

`RefetchPolicy.CacheFirst` re-reads and asks the server only when the read misses,
which suits a watch whose records another operation can leave incomplete:

```scala
def capitalsFillingGaps(client: ApolloClient) =
    client.query(germany).refetchPolicy(RefetchPolicy.CacheFirst).watch()
```

It asks once per miss: a miss its refetch did not cure is emitted as the value. When two
operations select the same object without its key field, each one's write leaves the
other's read incomplete. A watch whose refetch already answered a miss does not refetch
when another write brings the same miss back. It keeps its last value, as Apollo Client
4.3 does, and the store's `CacheDiagnostics` (`normalizedCache(..., diagnostics =
CacheDiagnostics.toStdErr)`) reports it once. A read that succeeds after another write,
an `evict`, `remove` or `clearAll` ([Removing data](#removing-data)), or data the cache
dropped by expiry or eviction ([Bounding the cache](#bounding-the-cache)) lets the watch
refetch that miss again.

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

A local change that has no server operation behind it (a rename the user has not saved
yet, a counter the client keeps) is an update. `updateOperation` reads an operation's
result from the cache, applies a function to the typed value and writes the result
back, in one atomic step; watches react as they do to a network response:

```scala
import kyo.apollo.cache.normalized.api.CacheKey

def renameInCache(client: ApolloClient): Set[CacheKey] < Sync =
    client.apolloStore.updateOperation(card) { data =>
        (country = data.country.map(_.copy(name = "Deutschland")))
    }
```

The function runs again when a concurrent write commits first, so it has to be pure. A
result that is not cached leaves the store unchanged and returns no keys.

`updateOperation` goes through an operation's root, so it only reaches records that
operation reads. To change one record wherever it sits, address it by its key with a
fragment, a typed selection on one type:

```scala
import kyo.apollo.cache.normalized.api.*

val place = Fragment.of[Country](_ ~ Country.name ~ Country.capital)

def moveCapitalLocally(client: ApolloClient): Set[CacheKey] < Sync =
    client.apolloStore.updateFragment(place, CacheKey("Country", "DE")) { current =>
        (name = current.name, capital = Present("Bonn"))
    }
```

Every query that read `Country:DE` sees the new capital, whichever field led it there.
`readFragment` and `writeFragment` read and write the same shape without a function.

A mutation can show its result before the server answers: `optimisticUpdates(data)`
writes `data` as a layer on top of the cache, watches see it at once, and the layer is
replaced by the real response or dropped if the mutation fails:

```scala
import kyo.apollo.network.ApolloResponse

def moveToBonn(client: ApolloClient): ApolloResponse[(updateCapital: (code: String, capital: Maybe[String]))] < (Async & Scope) =
    client.mutation(moveCapital)
        .optimisticUpdates((updateCapital = (code = "DE", capital = Present("Bonn"))))
        .execute
```

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

### Bounding the cache

A cache that lives as long as the program needs bounds. `MemoryCache` evicts the least
recently used records past `maxSize`, and expires data by age: a whole record after
`expireAfterMillis`, a single field after `maxAge`, each counted from when it was
written:

```scala
val bounded = MemoryCache(maxSize = 5_000, maxAge = 5 * 60 * 1000L)
```

An expired field reads as missing, so the next `CacheFirst` call asks the server for it
again. Neither expiry nor eviction publishes anything: a watch sees the missing data
when a write makes it re-read.

### Pagination

A list that arrives page by page should read as one list. Relay-style connections
(`edges`, `pageInfo`, an `after` cursor) get that from a `ConnectionFieldPolicy`: every
page writes into the same cache slot, and the edges of later pages are appended to the
earlier ones instead of replacing them. The schema:

```graphql
type Query {
  countriesPage(first: Int!, after: String): CountryConnection!
}
type CountryConnection { edges: [CountryEdge!]! pageInfo: PageInfo! }
type CountryEdge { cursor: String! node: Country! }
type PageInfo { hasNextPage: Boolean! endCursor: String }
```

The cache needs the policy and an identity for the edges, because edges without one are
keyed by their position and each page would replace the last. `pagedBy` turns the
query into a function from cursor to page:

```scala
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*

given CacheIdentity[CountryEdge] = CacheIdentity.by(_ ~ CountryEdge.cursor)

val paged: ApolloClient.Config =
    config.normalizedCache(
        MemoryCache(),
        CacheIdentity.generator(summon[CacheIdentity[Country]], summon[CacheIdentity[CountryEdge]]),
        fieldPolicies = FieldPolicies.fromList(ConnectionFieldPolicy("Query", "countriesPage", "CountryConnection"))
    )

val countryPage =
    Queries.countriesPage(first = 20, after = Absent)(
        CountryConnection.edges(CountryEdge.cursor ~ CountryEdge.node(Country.code ~ Country.name)) ~
            CountryConnection.pageInfo(PageInfo.hasNextPage ~ PageInfo.endCursor)
    ).pagedBy()

def loadAllPages(client: ApolloClient): Unit < (Async & Scope) =
    Loop(Maybe.empty[String]) { cursor =>
        client.query(countryPage(cursor)).fetchPolicy(FetchPolicy.NetworkOnly).execute.map { response =>
            response.data.map(_.countriesPage.pageInfo) match
                case Present(info) if info.hasNextPage => Loop.continue(info.endCursor)
                case _                                 => Loop.done(())
        }
    }
```

`countryPage(Absent)` is the first page and `countryPage(Present(cursor))` the one after
`cursor`; every page has the same document, only the `after` variable differs. Pages
are fetched with `NetworkOnly` because all of them share one cache slot, so a
`CacheFirst` call for the second page would be answered by the first. A watch on
`countryPage(Absent)` reads that slot, so it re-emits with the longer list each time a
page lands. Without a `ConnectionFieldPolicy`, a `FieldPolicy` with `keyArgs` and a
`merge` function does the same for a list that is not a Relay connection.

### Removing data

Some data has to leave the cache before it expires: a record the server deleted, or
everything a signed-out user saw. `evict(key)` removes a record and publishes the
removal, so a watch that read it reacts the way its refetch policy says: under
`CacheOnly` it emits the miss as an error value, under `CacheFirst` and `NetworkOnly` it
asks the server. `cascade = true` also removes every record reachable from it,
including shared records such as a country another query still shows:

```scala
def forgetGermany(client: ApolloClient): Set[CacheKey] < Sync =
    client.apolloStore.evict(CacheKey("Country", "DE"), cascade = true)
```

`remove(key)` removes one record without the cascade, `clearAll` empties the store,
`publish(keys)` makes the watches of those records react as if they had changed,
without changing anything,
and `extract()` returns the whole cache as JSON in the shape Apollo Client's
`cache.extract()` uses, for debugging and the devtools.

### Local fields (`@client`)

UI state that belongs to an entity but not to the server (a starred country, a
selected row) lives in the cache as a client field. It is selected like a server
field, never sent to the server, and reads its default until something writes it:

```scala
import kyo.apollo.cache.normalized.api.CacheKey

val isFavorite = ClientField.create[Country, Boolean]("isFavorite", default = false)

val favourites = Queries.countries(Country.code ~ Country.name ~ isFavorite.select).toQuery()

def star(client: ApolloClient, code: String): Set[CacheKey] < Sync =
    isFavorite.write(client, code, true)
```

`write` needs the normalized cache: it stores the value on the record `Country:<code>`,
the key the `CacheIdentity` above gives a country, and publishes it, so every watch that
selected `isFavorite` re-emits as it does after a server response. A server response
never overwrites a client field, so local state survives a refetch, which a value
written with `updateFragment` does not.
`read(client, code)` reads it back, and `writeRoot`/`readRoot` keep a value on the query
root instead of an entity. kyo-apollo-codegen declares client fields from the build
(`--client-field "Country.isFavorite: Boolean = false"`), so they appear next to the
server fields on the generated selector.

### Fragments a component owns

A screen built from components wants each component to declare the fields it shows,
and the page's query to fetch them without coming to depend on them. A masked fragment
does that. It is declared next to the component, spread into the page's query, and the
query's result carries only an opaque reference to it:

```scala
object CountryPlace:
    val fields = Fragment.entity[Country](_ ~ Country.name ~ Country.capital)

val placeQuery = Queries.country("DE")(Country.code ~ CountryPlace.fields.spread).toQuery()
```

<!-- doctest:scope=inherited
```scala
val placeIsLabelled: kyo.apollo.api.Query.Normalizable[(country: Maybe[(code: String, countryPlace: CountryPlace.fields.Ref)])] =
    placeQuery
```
-->

The spread decodes to a `CountryPlace.fields.Ref` under the field `countryPlace`, so the
page's code cannot read `name` or `capital` through it, and a field the component adds
later cannot become something the page relies on. The component opens the ref:
`ref.value` gives the fields as the response delivered them. For a `Fragment.entity`
ref, which names a record, `apolloStore.readFragment(ref)` reads the record as the cache
holds it now (or the delivered fields, if the record is gone), and
`apolloStore.watchFragment(ref)` follows it, emitting again whenever a write changes it
and keeping the record from garbage collection while it runs. A `Fragment.embedded` ref,
for an object without an identity such as a `PageInfo`, opens through `value` only:

```scala
def renderPlace(client: ApolloClient, ref: CountryPlace.fields.Ref): Unit < (Async & Scope & Abort[ApolloParseException]) =
    client.apolloStore.watchFragment(ref).foreach { place =>
        Log.info(s"${place.name}: ${place.capital.getOrElse("no capital")}")
    }
```

`Fragment.of` is the unmasked form: a typed selection on one type that the store reads
and writes by key (`readFragment`, `writeFragment`, `updateFragment`), with nothing to
spread. Use it for cache access, and `Fragment.entity`/`Fragment.embedded` for the data a
component owns.

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

### Interceptors: authentication, batching, persisted queries, retries, logging

Interceptors wrap the engine on two tiers. `addHttpInterceptor` adds an
`HttpInterceptor`, which sees requests and responses on the wire
(`AuthorizationHeaderInterceptor`, `LoggingInterceptor`, `BatchingHttpInterceptor`).
`addInterceptor` adds an `ApolloInterceptor`, which sees operations and response
streams (`AutoPersistedQueryInterceptor`, `RetryOnErrorInterceptor`, the cache).
`LoggingInterceptor` writes no variable value: the `variables` of a JSON body are
redacted and a `GET` request is logged without its query string.

A client usually stacks several of them. The batching interceptor runs a window fiber,
so it is created in a `Scope` like the client:

```scala
import kyo.apollo.interceptor.*

val withInterceptors: ApolloClient.Config < (Sync & Scope) =
    BatchingHttpInterceptor.init(batchInterval = 20.millis, maxBatchSize = 20).map { batching =>
        config
            .addHttpInterceptor(AuthorizationHeaderInterceptor("Bearer <token>"))
            .addHttpInterceptor(batching)
            .addInterceptor(AutoPersistedQueryInterceptor())
            .addInterceptor(RetryOnErrorInterceptor(schedule = Schedule.exponentialBackoff(200.millis, 2.0, 5.seconds).take(3)))
    }
```

Requests that arrive within 20 milliseconds of the first one then leave as one HTTP
request, each query is sent as its hash first and in full only when the server does not
know it, and
a network failure or a 5xx status is retried three times with growing pauses.
Subscriptions are never retried (a dropped socket reopens through
`webSocketReopenWhen`, see [Configuring the subscription socket](#configuring-the-subscription-socket)),
and a GraphQL error is an answer, not a failure to retry.

The defaults are a starting point, and both the retry and the logging interceptor take
their policy as parameters. `retryWhen` chooses which failures
are worth another attempt (the default, `RetryOnErrorInterceptor.transportErrors`, is a
network failure or a 5xx status), and `jitter` spreads the pauses so that many clients
do not retry in step. `LoggingInterceptor` takes the header names it must never print:

```scala
import kyo.apollo.exception.*

val onlyNetworkFailures = RetryOnErrorInterceptor(
    schedule = Schedule.fixed(1.second).take(2),
    jitter = 0.2,
    retryWhen = _.isInstanceOf[ApolloNetworkException]
)

val logging = LoggingInterceptor(redact = LoggingInterceptor.sensitiveHeaders + "X-Api-Key")
```

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
  definition, held in a `scala.collection.immutable.VectorMap`, which keeps the
  insertion order the printer and the cache rely on. The same holds for
  `variables: Map[String, Json]` and record field maps. kyo-data's `OrderedDict`
  also keeps insertion order; moving the JSON model onto it is an open point.
- **`Set[String]`**: the set of changed record keys published after a write
  (`NormalizedCache.merge`, `ChangedKeysSubject`, `ClientField.write*`) is a
  membership set with no ordering; kyo has no `Set`, so `scala.collection.immutable.Set`
  stays.

## Putting it together

A screen that shows Germany's capital and lets the user change it combines most of the
pieces above: a cached client behind authentication, batching and retries, a watch that
renders every change, and a mutation whose result appears before the server confirms
it. Failures leave the response on the `Abort[ApolloException]` row:

```scala
import kyo.apollo.cache.normalized.*
import kyo.apollo.exception.*
import kyo.apollo.interceptor.*

def saveCapital(client: ApolloClient, capital: String): Maybe[String] < (Async & Scope & Abort[ApolloException]) =
    val save = Mutations.updateCapital("DE", capital)(Country.code ~ Country.capital).toMutation()
    client.mutation(save)
        .optimisticUpdates((updateCapital = (code = "DE", capital = Present(capital))))
        .execute
        .map { response =>
            response.error match
                case Present(error) => Abort.fail(error)
                case Absent         => response.data.map(_.updateCapital.capital).orFailWith(DefaultApolloException("no data"))
        }
end saveCapital

def capitalPage(token: String): Maybe[String] < (Async & Scope & Abort[ApolloException]) =
    for
        batching <- BatchingHttpInterceptor.init()
        client   <- ApolloClient.init(
            cached
                .addHttpInterceptor(AuthorizationHeaderInterceptor(s"Bearer $token"))
                .addHttpInterceptor(batching)
                .addInterceptor(RetryOnErrorInterceptor())
        )
        _ <- Fiber.init(client.query(card).fetchPolicy(FetchPolicy.CacheAndNetwork).watch().foreach { response =>
            Log.info(s"capital: ${response.data.flatMap(_.country).flatMap(_.capital)}")
        })
        saved <- saveCapital(client, "Bonn")
    yield saved
```

The watch logs its first result and then a line for every write that changes the
`Country:DE` record it read. The optimistic layer is such a write, so "Bonn" is logged
before the server has answered. When the page's `Scope` ends, the watch fiber, the
batching window and the client close with it.

## Differences from Apollo Client and apollo-kotlin

kyo-apollo takes its engine from apollo-kotlin: the normalized cache, the interceptor
chain and the WebSocket protocols behave as they do there. The fetch policies are
apollo-kotlin's five plus Apollo Client's `NoCache` and `Standby`, and a `CacheFirst`
refetch stops on a repeating miss as in Apollo Client 4.3. Where its API
deviates from apollo-kotlin or from Apollo Client (the TypeScript client), the reason is
one of three: a kyo convention (failures as values, resources owned by a `Scope`), a
mistake the compiler can catch instead of the running program, or a lifetime that should
not depend on a matching call. Each entry shows the other client's code, kyo-apollo's,
and why they differ.

### One failure value instead of `exception` and `errors`

apollo-kotlin 4 reports a failure in two fields, and a store read throws:

```kotlin
val response = apolloClient.query(CountryQuery(code = "DE")).execute()
when {
    response.exception != null -> showOffline(response.exception) // no response arrived
    response.errors != null -> showErrors(response.errors)         // the server answered with errors
    else -> showCountry(response.data?.country)
}
val cached = apolloClient.apolloStore.readOperation(CountryQuery(code = "DE")) // throws CacheMissException
```

In kyo-apollo, `ApolloResponse.error` is the only field to check. The server's errors
arrive there as an `ApolloGraphQLException` (`response.errors` reads them as typed
`GraphQLError`s), next to a partial `data` when the server sent one. A store read
declares its miss on the `Abort[CacheReadFailure]` row:

```scala
import kyo.apollo.exception.*

def capitalOrReason(client: ApolloClient): String < (Async & Scope) =
    client.query(germany).execute.map { response =>
        response.error match
            case Present(error) => describe(error)
            case Absent         => response.data.flatMap(_.country).flatMap(_.capital).getOrElse("no capital")
    }
```

**Why:** two fields mean two places to forget. `ApolloException` is sealed, so `describe`
(in [Failures](#failures)) is checked for exhaustiveness, and a new failure case is
reported by the compiler at every match that misses it, instead of falling into the
`else` branch. A store read whose type names its
failure cannot be called as if it always succeeded, and `Abort.fail(error)` moves a
response failure onto the same row kyo code already handles.

### The client is a scoped resource, configured by a value

apollo-kotlin's client is `Closeable`, and closing it covers the client alone:

```kotlin
ApolloClient.Builder()
    .serverUrl("https://countries.example/graphql")
    .build()
    .use { apolloClient -> run(apolloClient) }
```

kyo-apollo creates the client in a `Scope` (`ApolloClient.init`, or `ApolloClient.use`
for a block), and its configuration is an immutable `Config`:

```scala
val tenantA = config.addHttpHeader("X-Tenant", "a")
val tenantB = config.addHttpHeader("X-Tenant", "b")

def perTenant: (ApolloClient, ApolloClient) < (Sync & Scope) =
    for
        a <- ApolloClient.init(tenantA)
        b <- ApolloClient.init(tenantB)
    yield (a, b)
```

**Why:** a client owns a WebSocket, fibers and, with batching, a window fiber, and the
watches, subscriptions and retains created from it are resources too. In a `Scope` they
all close on every exit, an interrupt included, without one `use` block per resource. A
`Config` value can be derived per tenant or per test without a builder and without one
variant seeing the other's headers.

> **Note:** a `Config` with `normalizedCache` carries its store, so tenants derived from
> a cached `Config` share one cache. Install the cache per tenant when their data must
> not mix.

### Operations are Scala values, not generated classes

apollo-kotlin generates a class per operation from a `.graphql` file:

```graphql
# src/main/graphql/Country.graphql
query Country($code: ID!) {
  country(code: $code) { code name capital }
}
```

```kotlin
val response = apolloClient.query(CountryQuery(code = "DE")).execute()
val capital: String? = response.data?.country?.capital
```

kyo-apollo writes the same operation in Scala, against selectors generated from the
schema alone:

```scala
def countryQuery(code: String) = Queries.country(code)(Country.code ~ Country.name ~ Country.capital).toQuery()

val nameOnly = Queries.countries(Country.name).toQuery()
```

**Why:** the codegen runs when the schema changes, not when an operation does, so adding
or changing an operation needs no build step. An operation is an ordinary value: a
function can build it from parameters (`countryQuery`), and a selection can be shared
between operations. Each result type is a named tuple of exactly the selected fields, so
`nameOnly` has no `capital` to read by mistake, and there are no near-identical `Country`
classes per operation to convert between.

### `RefetchPolicy` is its own type with three cases

apollo-kotlin passes a `FetchPolicy` for what a watch does when the cache changes:

```kotlin
apolloClient.query(CountryQuery(code = "DE"))
    .refetchPolicy(FetchPolicy.CacheFirst)
    .watch()
```

kyo-apollo has `RefetchPolicy.CacheOnly` (re-read, the default), `NetworkOnly` (ask the
server) and `CacheFirst` (re-read, ask the server on a miss); `fetchPolicy` keeps all
seven cases for the first result:

```scala
def germanyLive(client: ApolloClient) =
    client.query(germany)
        .fetchPolicy(FetchPolicy.CacheAndNetwork)
        .refetchPolicy(RefetchPolicy.CacheFirst)
        .watch()
```

**Why:** apollo-kotlin's `refetchPolicy` takes its five-case `FetchPolicy`, though a
refetch starts after a write has already changed the cache, so only three reactions
are distinct: re-read, ask the server, or ask only on a miss. `CacheAndNetwork` as a
refetch would emit the value it just re-read and then ask the server anyway. A type
with exactly those three cases rejects the others at compile time.

### Typed updates instead of `cache.modify`

Apollo Client changes single fields by name, through a callback per field:

```ts
// with typePolicies: { Country: { keyFields: ["code"] } }
cache.modify({
  id: cache.identify({ __typename: "Country", code: "DE" }),
  fields: { capital: () => "Bonn" },
});
```

kyo-apollo updates a typed value, either an operation's result or one record through a
fragment (see [Normalized cache](#normalized-cache)):

```scala
import kyo.apollo.cache.normalized.api.*

def bonn(client: ApolloClient): Set[CacheKey] < Sync =
    client.apolloStore.updateFragment(place, CacheKey("Country", "DE"))(current => (name = current.name, capital = Present("Bonn")))
```

**Why:** `cache.modify` addresses fields by name. With its `cache.modify<Country>` type
parameter TypeScript checks those names, but the modifier still receives the raw stored
value (a reference, for an object field), so writing a nested object means building the
cache's representation by hand. Here the update receives and returns the selection's
own typed value, nested objects included. The update is one atomic step that is
re-applied when a concurrent write wins, and it publishes the changed records to watches
exactly as a server response does.

### Fragments are values, without a registry

Apollo Client shares a fragment by name, through a registry the cache is created with:

```ts
const cache = new InMemoryCache({
  fragments: createFragmentRegistry(gql`
    fragment CountryPlace on Country { name capital }
  `),
});
// query Germany { country(code: "DE") { code ...CountryPlace } }
```

kyo-apollo's fragments are values (`Fragment.of`, and `Fragment.entity` /
`Fragment.embedded` for masked fragments, see
[Fragments a component owns](#fragments-a-component-owns)) that an operation references
like any other Scala value:

```scala
val germanyWithPlace = Queries.country("DE")(Country.code ~ CountryPlace.fields.spread).toQuery()
```

The printed document carries the fragment's fields directly in the selection; the
server never sees a `fragment` definition.

**Why:** a misspelled fragment name in a document string is found at runtime. A
fragment value that does not exist is a compile error, the IDE navigates to its
definition, and no registry has to be set up before the first query runs.

### A retain ends with its `Scope`

Apollo Client keeps a record from garbage collection between two calls:

```ts
cache.retain("Country:DE");
// ...
cache.release("Country:DE");
```

kyo-apollo's `retain` holds the records for as long as the enclosing `Scope` is open.
Once `Scope.run` returns, the retain is gone, so the collection after it may remove
`Country:DE` if no operation root reaches it:

```scala
def collectAfterLeaving(client: ApolloClient): Set[CacheKey] < (Sync & Async) =
    Scope.run(client.apolloStore.retain(Set(CacheKey("Country", "DE"))))
        .andThen(client.apolloStore.garbageCollect)
```

**Why:** a `release` that an exception or an early return skips keeps the record for the
life of the cache. A `Scope` runs the release on every exit.

### Incremental delivery reads two formats

apollo-kotlin requests `multipart/mixed; deferSpec=20220824`, and Apollo Client 4 does
when it is configured with `incrementalHandler: new Defer20220824Handler()`. That
format's parts carry an `incremental` array:

```json
{"incremental": [{"data": {"capital": "Berlin"}, "path": ["country"]}], "hasNext": false}
```

kyo-apollo requests the same format and also reads the older single-patch shape:

```json
{"data": {"capital": "Berlin"}, "path": ["country"], "hasNext": false}
```

**Why:** servers built on the earlier draft of `@defer` still send the second shape, and
a client that reads both works against either. The GraphQL 17 `pending`/`completed`
format, which Apollo Client 4.1+ reads through `GraphQL17Alpha9Handler`, is not read: a
server that sends only that format does not work with kyo-apollo.

### `complete` tracks the delivery, not the data

Apollo Client 4 narrows the result type with `dataState`, which says whether deferred
parts are still missing:

```ts
const { data, dataState } = useQuery(GERMANY_WITH_DEFERRED_CAPITAL);
if (dataState === "complete") render(data.country?.capital);
```

In kyo-apollo a deferred field is already `Maybe` in the result type, so
`ApolloResponse.complete` only says whether more parts are coming, like Apollo Client's
`NetworkStatus.streaming`:

```scala
def finalCapital(client: ApolloClient): Chunk[Maybe[String]] < (Async & Scope) =
    client.query(progressive).stream
        .filter(_.complete)
        .map(_.data.flatMap(_.country).flatMap(_.capital).flatMap(identity))
        .run
```

**Why:** the type already forces the code to handle a part that has not arrived, so a
second flag for the same fact would only repeat it. `complete` answers the one question
the type cannot: whether to keep waiting.
