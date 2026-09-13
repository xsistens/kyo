package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap

/** Phase 05 Task 4 tests: a `watch()` re-emits when a record its last read
  * depended on changes (from a mutation or a direct `apolloStore` write), stays
  * silent on unrelated writes, and stops emitting once cancelled. Driven through
  * a fake [[kyo.apollo.network.http.HttpEngine]] and the synchronous, network-free
  * `CacheOnly` refetch path so every assertion is deterministic — no clock, no
  * real network, no async fences. On kyo-test: each leaf body IS the effect (the
  * runner discharges `Async & Scope`), and the ordered, assert-as-you-go
  * interleaving runs on a [[StreamProbe.Pull]] whose `next` suspends until the
  * watch's next emission has actually landed.
  */
class WatcherSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixture: a `CurrentUser` query returning a single id-keyed `User` ------

    // `__typename` field named verbatim to match the GraphQL wire form (kyo-schema
    // encodes field names as-is; the old ObjectAdapter mapped `typename` onto it).
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

    private def userData(name: String): UserData =
        UserData(User("User", "1", name))

    /** A `... on User { __typename id name }` fragment rooted at a `User` record —
      * the imperative write path onto the very `User:1` the watch depends on.
      */
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

    final private class CountingEngine extends kyo.apollo.network.http.HttpEngine:
        var calls = 0
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls += 1
            kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
        end execute
    end CountingEngine

    /** An engine whose second and later calls park on `gate` before answering and
      * signal `arrived` as they start — so a `NetworkOnly` refetch can be held in
      * flight across the watch's teardown. The first call (the cache-populating
      * fetch) answers at once.
      */
    final private class GatedEngine(calls: AtomicInt, arrived: Latch, gate: Latch)
        extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls.incrementAndGet.map { n =>
                val response = kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
                if n == 1 then response
                else arrived.release.andThen(gate.await).andThen(response)
            }
    end GatedEngine

    private def cachedClient(engine: kyo.apollo.network.http.HttpEngine): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
            .build()

    private def query(client: ApolloClient) = client.query(CurrentUserQuery())

    /** Populate the cache with Alice, then run `body` with a client + a
      * [[StreamProbe.Pull]] handle over a `CacheOnly` watch — `pull.next` awaits
      * exactly the next emission, so a test can interleave synchronous store writes
      * (wrapped in `Sync.defer`) between exact, ordered pulls. The watch's channel
      * and drain fiber are `Scope`-managed, so the runner tears them down on leaf
      * exit; `pull.cancel` remains for tests that stop a watcher mid-leaf.
      */
    private def watching(
        body: (ApolloClient, StreamProbe.Pull[ApolloResponse[UserData]]) => Unit < (Async & Scope)
    )(using Frame): Unit < (Async & Scope) =
        val engine = CountingEngine()
        val client = cachedClient(engine)
        for
            _    <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
            pull <- StreamProbe.Pull.open(query(client).fetchPolicy(FetchPolicy.CacheOnly).watch())
            _    <- body(client, pull)
        yield ()
        end for
    end watching

    // --- tests ----------------------------------------------------------------

    "watch()" - {

        "watch emits the initial cache value on subscription" in {
            watching { (_, pull) =>
                for first <- pull.next
                yield
                    assert(first.data == Present(userData("Alice")))
                    assert(first.cacheInfo.exists(_.isCacheHit))
                    assert(first.cacheInfo.exists(_.dependentKeys.contains("User:1")))
            }
        }

        "watch re-emits when a dependent record changes via a store write" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // A direct store write to the same User:1 record (as a mutation would).
                    _      <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                    second <- pull.next
                yield assert(second.data == Present(userData("Bob")))
            }
        }

        "writeFragment on a watched record re-emits the watcher" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // An imperative fragment write onto User:1 — the record the watch's
                    // last read depended on. The fragment's changed key intersects the
                    // watch set, so the CacheOnly re-read fires and the watcher emits Eve.
                    changed <- Sync.defer(
                        client.apolloStore
                            .writeFragment(UserFragment, CacheKey("User:1"), User("User", "1", "Eve"))
                    )
                    _ = assert(changed.contains("User:1"))
                    second <- pull.next
                yield assert(second.data == Present(userData("Eve")))
            }
        }

        "watch does NOT re-emit on an unrelated write" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // A changed key outside the read's dependentKeys ({QUERY_ROOT, User:1}).
                    _           <- Sync.defer(client.apolloStore.publish(Set("Post:99")))
                    maybeSecond <- pull.tryNext
                yield assert(maybeSecond == Absent) // still just the initial emission
            }
        }

        "cancelling the watch stops further emissions" in {
            watching { (client, pull) =>
                for
                    _           <- pull.next
                    _           <- pull.cancel
                    _           <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol")))
                    maybeSecond <- pull.tryNext
                yield assert(maybeSecond == Absent) // nothing after cancel
            }
        }

        "no emission reaches the channel after the scope closed" in {
            // The interleaving P2-32 is about: a NetworkOnly refetch is on the wire when
            // the watch's Scope closes. The late response arrives on a detached fetch
            // fiber and must find the watch inactive — one CAS flips `active`, and the
            // store's publisher no longer reaches the (unsubscribed) watcher either.
            for
                calls    <- AtomicInt.init(0)
                arrived  <- Latch.init(1)
                gate     <- Latch.init(1)
                tornDown <- Latch.init(1)
                client = cachedClient(GatedEngine(calls, arrived, gate))
                _     <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                probe <- Channel.init[ApolloResponse[UserData]](Int.MaxValue)
                // Scope finalizers run last-registered-first, so `tornDown` is released
                // only after the watch's own teardown has completed.
                drain <- Fiber.init(Scope.run(
                    Scope.ensure(tornDown.release).andThen(
                        query(client)
                            .fetchPolicy(FetchPolicy.CacheOnly)
                            .refetchPolicy(RefetchPolicy.NetworkOnly)
                            .watch()
                            .foreach(probe.put)
                    )
                ))
                first <- probe.take
                _ = assert(first.data == Present(userData("Alice")))
                _ <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                _ <- arrived.await // the refetch is in flight, parked on `gate`
                // Close the watch's Scope and wait for its teardown to have run.
                _ <- drain.interrupt
                _ <- tornDown.await
                subscribersAfterClose = client.apolloStore.changedKeys.subscriberCount
                // Let the late network response through; its write-back publishes User:1.
                _        <- gate.release
                _        <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol")))
                late     <- probe.poll
                callsNow <- calls.get
            yield
                assert(subscribersAfterClose == 0, "teardown must unsubscribe the watcher")
                assert(late == Absent, s"an emission reached the consumer after teardown: $late")
                assert(callsNow == 2, s"a closed watch must not refetch again, got $callsNow call(s)")
            end for
        }

        "removing a watched record re-emits a cache miss" in {
            watching { (client, pull) =>
                for
                    _      <- pull.next
                    _      <- Sync.defer(client.apolloStore.remove("User:1"))
                    second <- pull.next
                yield
                    assert(second.data.isEmpty)
                    assert(second.error.exists(_.isInstanceOf[CacheMissException]))
            }
        }

        "CacheFirst refetch: a vanished record goes to the network instead of emitting the miss" in {
            // The twin of the test above, and the whole of RefetchPolicy.CacheFirst: the same
            // eviction, answered by a fetch rather than by handing the consumer a failed query.
            // apollo-kotlin reaches this by passing the full FetchPolicy to refetchPolicy.
            val engine = CountingEngine()
            val client = cachedClient(engine)
            for
                _ <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull <- StreamProbe.Pull.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.CacheFirst)
                        .watch()
                )
                first <- pull.next
                _ = assert(first.data == Present(userData("Alice")))
                _      <- Sync.defer(client.apolloStore.remove("User:1"))
                second <- pull.next
            yield
                assert(second.error.isEmpty, s"the miss must not reach the consumer: ${second.error}")
                assert(second.data == Present(userData("Alice")))
                assert(engine.calls == 2, s"expected a refetch, got ${engine.calls} call(s)")
            end for
        }

        "a second watcher is independent — one cancel does not silence the other" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    otherPull <- StreamProbe.Pull.open(
                        query(client).fetchPolicy(FetchPolicy.CacheOnly).watch()
                    )
                    _          <- otherPull.next
                    _          <- pull.cancel // cancel the first watcher only
                    _          <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Dave")))
                    firstAfter <- pull.tryNext
                    otherAfter <- otherPull.next
                yield
                    assert(firstAfter == Absent)                         // cancelled — no update
                    assert(otherAfter.data == Present(userData("Dave"))) // still live
            }
        }

        "apolloStore throws a clear error when no cache is installed" in {
            val client = ApolloClient
                .builder()
                .serverUrl("https://example.com/graphql")
                .httpEngine(CountingEngine())
                .build()
            val _ = intercept[IllegalStateException](client.apolloStore)
        }
    }
end WatcherSpec
