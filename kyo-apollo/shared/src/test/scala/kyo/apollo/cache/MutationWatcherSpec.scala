package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ws.FakeWebSocketConnection
import kyo.apollo.network.ws.FakeWebSocketEngine
import scala.collection.immutable.VectorMap

/** Phase 05 Task 5: mutations update the cache and fan their changed keys out to
  * watchers. A `CurrentUserQuery` is watched while an `UpdateUserNameMutation`
  * runs through the *real* network chain — the [[kyo.apollo.cache.normalized.CacheInterceptor]]
  * write-back normalizes the mutation payload exactly like a query, merges it into
  * the shared `User:1` record, and publishes the changed key so the watcher
  * re-reads. Also covers the imperative `apolloStore.writeOperation` path. Driven
  * through a fake [[kyo.apollo.network.http.HttpEngine]] that routes by operation name
  * and the synchronous `CacheOnly` refetch path, so every assertion is
  * deterministic — no clock, no real network, no async fences. On kyo-test each
  * leaf body IS the effect; ordered pulls run on a [[StreamProbe.Pull]] and a
  * short `Async.sleep` lets the scheduler dispatch background fibers between
  * scripted socket frames.
  */
class MutationWatcherSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures ---------------------------------------------------------------

    // `__typename` is a legal Scala identifier and kyo-schema encodes field names
    // verbatim, so the field is named `__typename` to match the GraphQL wire form
    // (the old ObjectAdapter mapped a `typename` field onto the `__typename` key).
    final case class User(__typename: String, id: String, name: String) derives Schema

    private def userField(field: String): CompiledField =
        CompiledField(
            field,
            CompiledNamedType("User"),
            selections = Chunk(
                CompiledField("__typename", CompiledNamedType("String")),
                CompiledField("id", CompiledNamedType("String")),
                CompiledField("name", CompiledNamedType("String"))
            )
        )

    // A `CurrentUser` query reading the shared `User:1` record.
    final case class UserData(user: User) derives Schema

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = Chunk(userField("user")))
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    // A mutation writing back the *same* `User:1` record under a different field.
    final case class UpdateUserData(updateUser: User) derives Schema

    final case class UpdateUserNameMutation(newName: String) extends Mutation[UpdateUserData]:
        def name = "UpdateUserName"
        def document =
            "mutation UpdateUserName($name: String!) { updateUser(name: $name) { __typename id name } }"
        def dataSchema: Schema[UpdateUserData] = summon[Schema[UpdateUserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Mutation"),
                selections = Chunk(userField("updateUser"))
            )
        def variables: Json = Json.JObj(VectorMap("name" -> SchemaJson.encode(newName)))
    end UpdateUserNameMutation

    // A subscription streaming the *same* `User:1` record under a different field,
    // so a streamed event normalizes into the shared record a watcher reads.
    final case class UserUpdatedData(userUpdated: User) derives Schema

    final case class UserUpdatedSubscription() extends Subscription[UserUpdatedData]:
        def name                                = "UserUpdated"
        def document                            = "subscription UserUpdated { userUpdated { __typename id name } }"
        def dataSchema: Schema[UserUpdatedData] = summon[Schema[UserUpdatedData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Subscription"),
                selections = Chunk(userField("userUpdated"))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end UserUpdatedSubscription

    private def userData(name: String): UserData = UserData(User("User", "1", name))

    private def body(field: String, name: String): String =
        s"""{"data":{"$field":{"__typename":"User","id":"1","name":"$name"}}}"""

    /** Routes responses by operation name found in the request document: the query
      * returns Alice, the mutation echoes back its requested name.
      */
    final private class RoutingEngine extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            val text = request.body.getOrElse("")
            val payload =
                if text.contains("UpdateUserName") then
                    val name = """"name":"([^"]*)"""".r.findAllMatchIn(text).map(_.group(1)).toList.last
                    body("updateUser", name)
                else body("user", "Alice")
            kyo.apollo.network.http.HttpResponse(200, Nil, payload)
        end execute
    end RoutingEngine

    private def cachedClient(): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(RoutingEngine())
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()

    /** Seed the cache with Alice, then run `body` with a [[StreamProbe.Pull]]
      * handle over a `CacheOnly` watch — `pull.next` awaits exactly the next
      * emission, so a test can interleave a real mutation/write between exact,
      * ordered pulls.
      */
    private def watching(
        body: (ApolloClient, StreamProbe.Pull[ApolloResponse[UserData]]) => Unit < (Async & Scope)
    )(using Frame): Unit < (Async & Scope) =
        val client = cachedClient()
        for
            _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
            pull <- StreamProbe.Pull.open(
                client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
            )
            _ <- body(client, pull)
        yield ()
        end for
    end watching

    // --- tests ------------------------------------------------------------------

    "mutation → watcher" - {

        "a mutation run through the network chain re-emits the watcher" in {
            watching { (client, pull) =>
                for
                    first <- pull.next
                    _ = assert(first.data == Present(userData("Alice")))
                    // Run the mutation through the full chain: CacheInterceptor.writeBack
                    // normalizes `updateUser` into the shared `User:1` record and publishes it.
                    mutationResponse <- client
                        .mutation(UpdateUserNameMutation("Bob"))
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                        .execute
                    _ = assert(mutationResponse.error.isEmpty)
                    second <- pull.next
                yield assert(second.data == Present(userData("Bob")))
            }
        }

        "an imperative apolloStore.writeOperation re-emits the watcher" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // App code updating the cache directly, without a network round-trip.
                    changed <- Sync.defer(
                        client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol"))
                    )
                    _ = assert(changed.contains(CacheKey("User", "1")))
                    second <- pull.next
                yield assert(second.data == Present(userData("Carol")))
            }
        }

        "a mutation touching an unrelated record does not re-emit the watcher" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // Directly publish a foreign changed key, as a mutation on another record would.
                    _           <- Sync.defer(client.apolloStore.publish(Set(CacheKey("Post", "99"))))
                    maybeSecond <- pull.tryNext
                yield assert(maybeSecond == Absent)
            }
        }

        "a subscription event written back through the cache re-emits a query watcher" in {
            val conn = new FakeWebSocketConnection
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(RoutingEngine())
                .webSocketEngine(FakeWebSocketEngine(conn))
                .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
                .build()
            // Seed Alice, then watch the query off the cache.
            for
                _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull <- StreamProbe.Pull.open(
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
                )
                first <- pull.next
                _ = assert(first.data == Present(userData("Alice")))
                // Open a subscription that streams an updated `User:1` (fire-and-forget drain).
                _ <- Fiber.init(Scope.run(client.subscription(UserUpdatedSubscription()).stream.discard))
                _ <- conn.awaitSent(_.contains("connection_init"))
                _ <- Sync.defer(conn.server("""{"type":"connection_ack"}"""))
                _ <- conn.awaitSent(_.contains("\"type\":\"subscribe\""))
                // The CacheInterceptor write-back normalizes `userUpdated` into `User:1`
                // and publishes the changed key, so the watcher re-reads.
                _ <- Sync.defer(
                    conn.server(s"""{"id":"0","type":"next","payload":${body("userUpdated", "Dana")}}""")
                )
                second <- pull.next
                _      <- client.close
            yield assert(second.data == Present(userData("Dana")))
            end for
        }
    }
end MutationWatcherSpec
