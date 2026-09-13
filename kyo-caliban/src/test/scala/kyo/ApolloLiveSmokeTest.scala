package kyo

import caliban.*
import caliban.schema.Schema
import kyo.apollo.ApolloClient
import kyo.apollo.testing.Fixtures
import kyo.apollo.testing.StreamProbe

/** End-to-end smoke test proving the JVM/Native apollo transport engines against a
  * REAL GraphQL server rather than the in-memory fakes the rest of the suite uses.
  *
  * Every other apollo JVM/Native test drives `TestHttpEngine` / `FakeWebSocketConnection`,
  * so the kyo-http-backed production engines — `HttpClientEngine` (HTTP queries) and
  * `KyoHttpWebSocketEngine` (WebSocket subscriptions) — compile and cross-link but never
  * touch a socket. Here a real in-process caliban server (served over kyo-http via
  * `Resolvers.run`) is driven by an `ApolloClient` wired with its default per-platform
  * engines, so an actual `POST /api/graphql` round-trip and a real
  * `graphql-transport-ws` subscription flow over live sockets.
  *
  * The server schema mirrors the promoted `{ value: Int }` fixtures: a `value` query
  * field and a `value` `Int`-streaming subscription field, matching
  * [[Fixtures.ValueQuery]]'s `query Value { value }` and [[Fixtures.ValueSubscription]]'s
  * `subscription Value { value }`.
  */
class ApolloLiveSmokeTest extends BaseCalibanTest:

    case class Query(value: Int) derives Schema.SemiAuto
    case class Mutation(noop: Int) derives Schema.SemiAuto
    case class Subscriptions(value: zio.stream.ZStream[Any, Nothing, Int]) derives Schema.SemiAuto

    private def api(values: Int*) =
        graphQL(RootResolver(Query(42), Mutation(0), Subscriptions(zio.stream.ZStream.fromIterable(values))))

    "HttpClientEngine executes a real query over kyo-http sockets" in {
        for
            interpreter <- Resolvers.get(api())
            server      <- Resolvers.run(interpreter)
            client = ApolloClient.builder()
                .serverUrl(s"http://localhost:${server.port}/api/graphql")
                .build()
            response <- client.query(Fixtures.ValueQuery()).execute
        yield assert(response.data == Present(42))
        end for
    }

    "KyoHttpWebSocketEngine streams a real subscription over kyo-http sockets" in {
        for
            interpreter <- Resolvers.get(api(10, 20, 30))
            server      <- Resolvers.run(interpreter)
            client = ApolloClient.builder()
                .serverUrl(s"http://localhost:${server.port}/api/graphql")
                .webSocketServerUrl(s"ws://localhost:${server.port}/api/graphql/ws")
                .build()
            responses <- StreamProbe.collect(client.subscription(Fixtures.ValueSubscription()).stream.take(3))
        yield
            val values = responses.flatMap(r =>
                r.data match
                    case Present(v) => List(v);
                    case Absent     => Nil
            )
            assert(values == List(10, 20, 30))
        end for
    }
end ApolloLiveSmokeTest
