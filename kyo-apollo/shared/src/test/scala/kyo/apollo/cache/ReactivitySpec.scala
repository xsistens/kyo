package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ListBuffer

/** Phase 05 Task 8 — the cohesive end-to-end reactivity suite.
  *
  * Where the per-feature specs each verify one seam in isolation
  * ([[CacheBatchReaderSpec]] dependency capture, [[ChangedKeysSubjectSpec]] /
  * [[StoreSpec]] the bus, [[WatcherSpec]] a single watcher, [[FragmentSpec]] the
  * fragment APIs), this suite drives the *whole* reactive loop through one
  * watcher lifecycle and asserts, in one narrative, every behavior Task 8 calls
  * out:
  *
  *   1. dependency-key capture during reads,
  *   2. `changedKeys` emission on writes,
  *   3. a watcher re-emitting only when its dependent keys change — and NOT on an
  *      unrelated write,
  *   4. watcher cancellation stopping emissions,
  *   5. `writeFragment` triggering a watcher.
  *
  * Everything runs through a fake [[kyo.apollo.network.http.HttpEngine]] and the
  * synchronous, network-free `CacheOnly` refetch path, so emission order is
  * deterministic and assertions need no clock or async fences. On kyo-test each
  * leaf body IS the effect; ordered pulls run on a [[StreamProbe.Pull]] and
  * synchronous store writes are sequenced with `Sync.defer`.
  */
class ReactivitySpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixture: a `CurrentUser` query + a `User` fragment on the same record --

    // `__typename` field named verbatim to match the GraphQL wire form (kyo-schema
    // encodes field names as-is; the old ObjectAdapter mapped `typename` onto it).
    final case class User(__typename: String, id: String, name: String) derives Schema
    final case class UserData(user: User) derives Schema

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
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    /** `... on User { __typename id name }` rooted at a `User` record — the
      * imperative write path onto the very `User:1` the query depends on.
      */
    object UserFragment extends Fragment[User]:
        def dataSchema: Schema[User] = summon[Schema[User]]
        def rootField: CompiledField =
            CompiledField("user", CompiledNamedType("User"), selections = userSelections)
    end UserFragment

    private def userData(name: String): UserData = UserData(User("User", "1", name))

    private val aliceBody =
        """{"data":{"user":{"__typename":"User","id":"1","name":"Alice"}}}"""

    /** Serves the initial `Alice` payload for the warm-up NetworkOnly read; every
      * subsequent reactive re-emit rides the synchronous CacheOnly path, so this
      * engine is only ever hit once per scenario.
      */
    final private class AliceEngine extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls += 1
            kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
        end execute
    end AliceEngine

    private def cachedClient(): (ApolloClient, AliceEngine) =
        val engine = AliceEngine()
        val client = ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()
        (client, engine)
    end cachedClient

    /** Warm the cache with `Alice` (one network round-trip), then hand `body` a
      * client and a [[StreamProbe.Pull]] handle over a `CacheOnly` watch —
      * `pull.next` awaits exactly the next emission, so a test can interleave
      * synchronous store writes (via `Sync.defer`) between exact, ordered pulls.
      */
    private def watching(
        body: (ApolloClient, StreamProbe.Pull[ApolloResponse[UserData]]) => Unit < (Async & Scope)
    )(using Frame): Unit < (Async & Scope) =
        val (client, _) = cachedClient()
        for
            _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
            pull <- StreamProbe.Pull.open(
                client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
            )
            _ <- body(client, pull)
        yield ()
        end for
    end watching

    "reactivity end-to-end" - {

        // --- 1. dependency-key capture during reads -----------------------------

        "a read captures exactly the record keys it depended on" in {
            val (client, _) = cachedClient()
            client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute.map { _ =>
                // The store-level read reports the root plus the id-keyed User it links to.
                val (data, keys) = client.apolloStore.readOperationWithKeys(CurrentUserQuery())
                assert(data == userData("Alice"))
                assert(keys == Set("QUERY_ROOT", "User:1"))
            }
        }

        "a watch stamps its dependent keys onto the first emission's cacheInfo" in {
            watching { (_, pull) =>
                for first <- pull.next
                yield
                    val info = first.cacheInfo
                    assert(info.exists(_.isCacheHit), "initial emission should be a cache hit")
                    assert(info.map(_.dependentKeys) == Present(Set("QUERY_ROOT", "User:1")))
            }
        }

        // --- 2. changedKeys emission on writes ----------------------------------

        "every write path publishes its changed keys to a registered listener" in {
            val (client, _) = cachedClient()
            client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute.map { _ =>
                val batches = ListBuffer.empty[Set[String]]
                val sub     = client.apolloStore.addChangedKeysListener(batches += _)

                // writeOperation on the shared record → publishes User:1.
                client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob"))
                // writeFragment on the same record → publishes User:1.
                client.apolloStore.writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Eve"))
                // remove → publishes the removed key.
                client.apolloStore.remove("User:1")
                // manual/external invalidation → publishes verbatim.
                client.apolloStore.publish(Set("Post:7"))

                assert(batches.toList == List(Set("User:1"), Set("User:1"), Set("User:1"), Set("Post:7")))
                sub()

                // After unsubscribing, no further batches arrive.
                client.apolloStore.publish(Set("User:1"))
                assert(batches.size == 4)
            }
        }

        "re-writing identical data changes nothing and publishes no keys" in {
            val (client, _) = cachedClient()
            client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute.map { _ =>
                val batches = ListBuffer.empty[Set[String]]
                val sub     = client.apolloStore.addChangedKeysListener(batches += _)
                val changed = client.apolloStore.writeOperation(CurrentUserQuery(), userData("Alice"))
                assert(changed.isEmpty, "identical re-write should report no changed keys")
                assert(batches.isEmpty, "identical re-write should publish nothing")
                sub()
            }
        }

        // --- 3-5. one watcher, the whole reactive lifecycle ---------------------

        "end-to-end: watch → mutation re-emit → unrelated silence → writeFragment re-emit → cancel silence" in {
            watching { (client, pull) =>
                for
                    // (1) initial cache value, with its dependent keys stamped.
                    first <- pull.next
                    _ = assert(first.data == Present(userData("Alice")))
                    _ = assert(first.cacheInfo.map(_.dependentKeys) == Present(Set("QUERY_ROOT", "User:1")))
                    // (3a) a write to the shared User:1 record (as a mutation write-back would)
                    //      intersects the watch set → the CacheOnly re-read fires and re-emits.
                    _      <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                    second <- pull.next
                    _ = assert(second.data == Present(userData("Bob")))
                    // (3b) a change to a key OUTSIDE the watch set is ignored — no emission.
                    _      <- Sync.defer(client.apolloStore.publish(Set("Post:99")))
                    silent <- pull.tryNext
                    _ = assert(silent == Absent)
                    // (5) an imperative writeFragment onto the watched User:1 re-emits.
                    changed <- Sync.defer(
                        client.apolloStore
                            .writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Carol"))
                    )
                    _ = assert(changed.contains("User:1"))
                    third <- pull.next
                    _ = assert(third.data == Present(userData("Carol")))
                    // (4) after cancellation, no further write reaches the watcher.
                    _ <- pull.cancel
                    _ <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Dave")))
                    _ <- Sync.defer(
                        client.apolloStore
                            .writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Erin"))
                    )
                    afterCancel <- pull.tryNext
                yield assert(afterCancel == Absent)
            }
        }

        "two watchers over the same record both react; cancelling one leaves the other live" in {
            watching { (client, first) =>
                for
                    _ <- first.next
                    second <- StreamProbe.Pull.open(
                        client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
                    )
                    _ <- second.next
                    // A shared-record write fans out to both.
                    _ <- Sync.defer(
                        client.apolloStore
                            .writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Bob"))
                    )
                    firstBob  <- first.next
                    secondBob <- second.next
                    _ = assert(firstBob.data == Present(userData("Bob")))
                    _ = assert(secondBob.data == Present(userData("Bob")))
                    // Cancel only the first; the second still re-emits.
                    _ <- first.cancel
                    _ <- Sync.defer(
                        client.apolloStore
                            .writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Carol"))
                    )
                    firstAfter  <- first.tryNext
                    secondAfter <- second.next
                yield
                    assert(firstAfter == Absent) // frozen at Bob
                    assert(secondAfter.data == Present(userData("Carol")))
            }
        }
    }
end ReactivitySpec
