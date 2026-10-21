<!-- doctest:default scope=inherited -->

<!-- doctest:setup
```scala
import kyo.*
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.RootQuery
import kyo.apollo.api.ScalarCodec
import kyo.apollo.api.SelectionBuilder
import kyo.apollo.api.TypeName

// The selector layer kyo-apollo-codegen emits for `type Country { code: ID! name: String! }`
// and `type Query { country(code: ID!): Country }`, written out by hand.
sealed trait Country

object Country:
    given TypeName[Country] = TypeName("Country")

    def code: SelectionBuilder.Deferrable[Country, (code: String)] =
        SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.id)

    def name: SelectionBuilder.Deferrable[Country, (name: String)] =
        SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
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
end Queries
```
-->

# kyo-apollo-testing

Test doubles for code that uses [kyo-apollo](../kyo-apollo/README.md). They take the
place of the server at one of three layers, so a test drives a real `ApolloClient`,
with its interceptors, its response decoding and its normalized cache, and no request
leaves the process. Every double records what it received and offers a barrier to
wait on, so a test waits for an event instead of a period of time.

## Getting Started

Add the dependency to your `build.sbt`:

```scala doctest:expect=skipped
libraryDependencies += "io.getkyo" %%% "kyo-apollo-testing" % "<latest version>" % Test
```

The module is built for the JVM and Scala.js. It contains no platform-specific code;
a platform row exists where something consumes it: the JVM for suites that run
generated selectors, Scala.js for browser applications and their specs. Scala Native
and WebAssembly have no consumer and no artifact.

The examples below are kyo-test specs over a query written with the generated
selectors of a `Country` type:

```scala
import kyo.*
import kyo.apollo.*
import kyo.apollo.api.*
import kyo.apollo.testing.*

val germany = Queries.country("DE")(Country.code ~ Country.name).toQuery()

val germanyBody = """{"data":{"country":{"__typename":"Country","code":"DE","name":"Germany"}}}"""
```

## Pick a layer

| Layer | Doubles | What the client still runs |
|-------|---------|----------------------------|
| HTTP | `TestHttpEngine`, `MockServer`, `GatedHttpEngine` | request composition, the HTTP transport, JSON parsing and decoding, the cache |
| Operation | `QueueTestNetworkTransport`, `MapTestNetworkTransport` | the interceptors and the cache above the transport; responses arrive already decoded |
| WebSocket | `MockWebSocketServer` | the subscription transport and its protocol, frame by frame |

A test of how a response decodes, or of what the client sends, belongs on the HTTP
layer. A test of logic that only consumes responses is shorter on the operation layer.

`TestApolloClient` creates a client over a double, owned by the test's `Scope`:
`cacheless(engine)` and `cached(engine, keyFields)` over an `HttpEngine`,
`withTransport(transport)` and `cachedWithTransport(transport, keyFields)` over a
transport. `cached` keys records by the named fields, so the same `Country:DE` record
serves every query that reads it.

## HTTP layer

`TestHttpEngine.returning` answers every request with the same body and counts the
requests that actually ran:

```scala
class CountryQuerySpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "the country's name decodes from the answer" in {
        for
            engine   <- TestHttpEngine.returning(germanyBody)
            client   <- TestApolloClient.cacheless(engine)
            response <- client.query(germany).execute
            calls    <- engine.calls
        yield
            assert(response.data.flatMap(_.country).map(_.name) == Present("Germany"))
            assert(calls == 1)
        end for
    }

    "a network failure is the response's error value" in {
        for
            engine   <- TestHttpEngine.failing(new RuntimeException("connection refused"))
            client   <- TestApolloClient.cacheless(engine)
            response <- client.query(germany).execute
        yield
            assert(response.data.isEmpty)
            assert(response.error.exists(_.isInstanceOf[kyo.apollo.exception.ApolloNetworkException]))
        end for
    }
end CountryQuerySpec
```

`TestHttpEngine.respondWith` and `TestHttpEngine.async` compute the reply from the
request. `MockServer` serves a queue of replies in order and hands the received
requests back one at a time, which reads well for a scenario of several calls:

```scala
class CountryScenarioSpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "a failed call and its retry each reach the server" in {
        for
            server <- MockServer.init
            _      <- server.enqueue("""{"errors":[{"message":"try again"}]}""", HttpStatus.ServiceUnavailable)
            _      <- server.enqueue(germanyBody)
            client <- TestApolloClient.cacheless(server)
            failed <- client.query(germany).execute
            retry  <- client.query(germany).execute
            first  <- server.takeRequest
            count  <- server.requestCount
        yield
            assert(failed.error.isDefined)
            assert(retry.data.flatMap(_.country).map(_.code) == Present("DE"))
            assert(first.fields.body.text.exists(_.contains("\"DE\"")))
            assert(count == 2)
        end for
    }
end CountryScenarioSpec
```

## Operation layer

A transport answers with data instead of JSON. `QueueTestNetworkTransport` answers
requests in the order its responses were enqueued; `MapTestNetworkTransport` answers
each operation with the response registered for it, whatever the order.
`TestResponses` builds the common failures:

```scala
class CountryErrorSpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "the server's GraphQL error reaches the caller as a value" in {
        for
            transport <- QueueTestNetworkTransport.init
            _         <- transport.enqueue(TestResponses.graphqlError("no such country"))
            client    <- TestApolloClient.withTransport(transport)
            response  <- client.query(germany).execute
            names     <- transport.operationNames
        yield
            assert(response.errors.map(_.message) == Chunk("no such country"))
            assert(names.size == 1)
        end for
    }
end CountryErrorSpec
```

`TestResponses` also has `data`, `networkError`, `httpError(status)` and `exception`.

## Waiting on events, not on time

A double reports when something happened, and a test waits for that report. Nothing
in this module sleeps, polls or retries until a condition holds.

`GatedHttpEngine` holds every reply until `release()` and reports each request that
reaches it through `nextRequest`, so a test can observe a call in flight:

```scala
class CountryLoadingSpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "the call stays open until the server answers" in {
        val engine = GatedHttpEngine(germanyBody)
        for
            client   <- TestApolloClient.cacheless(engine)
            call     <- Fiber.init(Scope.run(client.query(germany).execute))
            _        <- engine.nextRequest
            answered <- call.done
            _        <- Sync.defer(engine.release())
            response <- call.get
        yield
            assert(!answered)
            assert(response.data.isDefined)
        end for
    }
end CountryLoadingSpec
```

After `nextRequest` the request is parked on the gate, so `call.done` is `false`
however the scheduler ran the fibers; no pause is needed to make that true.

`StreamProbe.Pull` does the same for a stream. `next` suspends until the stream's
next element has arrived, which lets a test interleave writes with the exact emissions
of a watch:

```scala
import kyo.apollo.cache.normalized.*

class CountryWatchSpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "a write to the cached country reaches the watch" in {
        for
            engine <- TestHttpEngine.returning(germanyBody)
            client <- TestApolloClient.cached(engine, keyFields = List("code"))
            pull   <- StreamProbe.Pull.open(client.query(germany).watch())
            first  <- pull.next
            _      <- client.apolloStore.writeOperation(germany, (country = Present((code = "DE", name = "Deutschland"))))
            second <- pull.next
            extra  <- pull.tryNext
        yield
            assert(first.data.flatMap(_.country).map(_.name) == Present("Germany"))
            assert(second.data.flatMap(_.country).map(_.name) == Present("Deutschland"))
            assert(extra.isEmpty)
        end for
    }
end CountryWatchSpec
```

`tryNext` returns an element only if one is already there. It proves an absence only
after a positive barrier that the unwanted element would have had to precede: above,
a second emission of the write would have arrived before `pull.next` returned
`Deutschland`. Timers, such as a retry backoff or a WebSocket keep-alive, run on the
`Clock`; inside `Clock.withTimeControl` a test fires them with `advance`.

## WebSocket layer

`MockWebSocketServer` stands in for the server of a subscription. Inject it as the
client's `webSocketEngine`, wait for each frame the client sends with `awaitSent`, and
answer it. Its `next(id, value)` frames carry `{ "value": n }`, the shape of the
bundled `Fixtures.ValueSubscription`; `push(frame)` sends any other frame text:

```scala
class ValueSubscriptionSpec extends kyo.test.Test[Any]:
    given CanEqual[Any, Any] = CanEqual.derived

    "a subscription delivers the server's events until it completes" in {
        val server = new MockWebSocketServer
        for
            client <- ApolloClient.init(
                ApolloClient.Config(TestApolloClient.DefaultServerUrl).webSocketEngine(server)
            )
            events <- Fiber.init(Scope.run(client.subscription(Fixtures.ValueSubscription()).stream.run))
            _      <- server.awaitSent(_.contains("connection_init"))
            _      <- Sync.defer(server.ack())
            _      <- server.awaitSent(_.contains("\"type\":\"subscribe\""))
            _      <- Sync.defer { server.next("0", 10); server.next("0", 20); server.complete("0") }
            seen   <- events.get
        yield assert(seen.map(_.data) == Chunk(Present(10), Present(20)))
        end for
    }
end ValueSubscriptionSpec
```

`MockWebSocketServer(MockWebSocketServer.Legacy)` speaks `subscriptions-transport-ws`
instead of `graphql-transport-ws`, and `drop(code, reason)` ends the connection
abnormally. For reconnect scenarios, `FreshWebSocketEngine` opens a new scripted
connection on every `open` and hands each one out through `nextConnection`.
