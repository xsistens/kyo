package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Tests for the `apolloStore.updateOperation` (react `cache.updateQuery`) and
  * `updateFragment` (typed `cache.modify`) read → transform → write helpers: a
  * successful update rewrites the record and returns its changed keys; a cache
  * miss is a no-op (empty changed-key set). Reuses the `WatcherSpec` fixture
  * shape (an id-keyed `User`).
  */
class StoreUpdateSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class User(__typename: String, id: String, name: String) derives Schema
    final case class UserData(user: User) derives Schema

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = List(
                    CompiledField(
                        "user",
                        CompiledNamedType("User"),
                        selections = List(
                            CompiledField("__typename", CompiledNamedType("String")),
                            CompiledField("id", CompiledNamedType("String")),
                            CompiledField("name", CompiledNamedType("String"))
                        )
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    object UserFragment extends Fragment[User]:
        def dataSchema: Schema[User] = summon[Schema[User]]
        def rootField: CompiledField =
            CompiledField(
                "user",
                CompiledNamedType("User"),
                selections = List(
                    CompiledField("__typename", CompiledNamedType("String")),
                    CompiledField("id", CompiledNamedType("String")),
                    CompiledField("name", CompiledNamedType("String"))
                )
            )
    end UserFragment

    private val aliceBody =
        """{"data":{"user":{"__typename":"User","id":"1","name":"Alice"}}}"""

    final private class OkEngine extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
    end OkEngine

    private def cachedClient(): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(new OkEngine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()

    "apolloStore.updateOperation" - {

        "reads, transforms, and writes back — re-emitting via changed keys" in {
            Scope.run {
                val client = cachedClient()
                val store  = client.apolloStore
                for
                    _ <- client
                        .query(CurrentUserQuery())
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                        .execute // Alice
                    changed <- Sync.defer(
                        store.updateOperation(CurrentUserQuery())(d => UserData(d.user.copy(name = "Alice B.")))
                    )
                    _ = assert(changed.contains("User:1"), s"changed keys: $changed")
                    back <- Sync.defer(store.readOperation[UserData](CurrentUserQuery()))
                    _ = assert(back.user.name == "Alice B.", s"updated name: ${back.user.name}")
                yield ()
                end for
            }
        }

        "is a no-op on a cache miss (empty changed set, no write)" in {
            Scope.run {
                val client = cachedClient() // nothing warmed
                for changed <- Sync.defer(client.apolloStore.updateOperation(CurrentUserQuery())(identity))
                yield assert(changed.isEmpty, s"miss should not write: $changed")
            }
        }
    }

    "apolloStore.updateFragment" - {

        "reads, transforms, and writes a single record" in {
            Scope.run {
                val client = cachedClient()
                val store  = client.apolloStore
                for
                    _ <- Sync.defer(
                        store.writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Alice"))
                    )
                    changed <- Sync.defer(
                        store.updateFragment(UserFragment, CacheKey("User:1"))(u => u.copy(name = "Eve"))
                    )
                    _ = assert(changed.contains("User:1"), s"changed keys: $changed")
                    back <- Sync.defer(store.readFragment[User](UserFragment, CacheKey("User:1")))
                    _ = assert(back.name == "Eve", s"updated name: ${back.name}")
                yield ()
                end for
            }
        }

        "is a no-op on a cache miss" in {
            Scope.run {
                val client = cachedClient()
                for changed <- Sync.defer(
                        client.apolloStore.updateFragment(UserFragment, CacheKey("User:404"))(identity)
                    )
                yield assert(changed.isEmpty, s"miss should not write: $changed")
            }
        }
    }
end StoreUpdateSpec
