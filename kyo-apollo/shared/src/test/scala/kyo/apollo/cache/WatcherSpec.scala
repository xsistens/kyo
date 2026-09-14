package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.cache.normalized.api.Record
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
                selections = Chunk(
                    CompiledField(
                        "user",
                        CompiledNamedType("User"),
                        selections = Chunk(
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
                selections = Chunk(
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

    /** An engine whose every answer differs (`Alice-<n>`, a volatile field in each
      * response) and whose second and later calls sleep one second on `clock` before
      * answering. The controlled clock is passed in explicitly: a watch's fetch fibers
      * are detached (`Fiber.Unsafe.init`, empty context) and would not see the
      * `Clock.withTimeControl` local, so `control.advance` releases exactly the
      * responses parked on this clock.
      */
    final private class SleepingEngine(clock: Clock, calls: AtomicInt) extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls.incrementAndGet.map { n =>
                val body     = s"""{"data":{"user":{"__typename":"User","id":"1","name":"Alice-$n"}}}"""
                val response = kyo.apollo.network.http.HttpResponse(200, Nil, body)
                if n == 1 then response
                else clock.sleep(1.second).map(_.get).andThen(response)
            }
    end SleepingEngine

    /** A cache that can hide one record from every read while writes to it still
      * land (and still report changed keys): the permanent miss of P2-34 — a record
      * the write-back keeps touching but the watched read can never be satisfied by.
      */
    final private class HoleCache(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val hole          = AtomicRef.Unsafe.init(Maybe.empty[CacheKey])

        def hide(key: CacheKey)(using Frame): Unit < Sync = Sync.defer(discard(hole.getAndSet(Present(key))))

        override def read[A](f: RecordLoader => A)(using Frame): A < Sync =
            delegate.read(loader => f(keys => loader.load(keys).filterNot((key, _) => hole.get().contains(key))))
    end HoleCache

    /** A cache whose read can be armed once: the first read that loads `key` after
      * arming completes normally and THEN runs `write`, before the read returns to the
      * store — so the write lands, on the reading fiber itself, between a read and the
      * moment the read's key set is adopted by the watch. That is the P2-33 window,
      * reproduced single-threaded and without a thread block (the re-read runs inside
      * the publisher's `publish`, so a latch there would park the writer).
      */
    final private class TrapCache(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val trap          = AtomicRef.Unsafe.init(Maybe.empty[(CacheKey, Unit < Sync)])

        def arm(key: CacheKey)(write: => Unit < Sync)(using Frame): Unit < Sync =
            Sync.defer(discard(trap.getAndSet(Present((key, Sync.defer(write))))))

        override def read[A](f: RecordLoader => A)(using Frame): A < Sync =
            val loadedArmed = AtomicBoolean.Unsafe.init(false)
            delegate.read { loader =>
                f { keys =>
                    if trap.get().exists((armed, _) => keys.contains(armed)) then loadedArmed.set(true)
                    loader.load(keys)
                }
            }.map { result =>
                trap.get() match
                    case Present((_, write)) if loadedArmed.get() =>
                        discard(trap.getAndSet(Absent))
                        write.andThen(result)
                    case _ => result
            }
        end read
    end TrapCache

    private def cachedClient(
        engine: kyo.apollo.network.http.HttpEngine,
        cache: NormalizedCache = MemoryCache()
    ): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(cache, IdCacheKeyGenerator(List("id")))
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
                    assert(first.cacheInfo.exists(_.dependentKeys.contains(CacheKey("User", "1"))))
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
                            .writeFragment(UserFragment, CacheKey("User", "1"), User("User", "1", "Eve"))
                    )
                    _ = assert(changed.contains(CacheKey("User", "1")))
                    second <- pull.next
                yield assert(second.data == Present(userData("Eve")))
            }
        }

        "watch does NOT re-emit on an unrelated write" in {
            watching { (client, pull) =>
                for
                    _ <- pull.next
                    // A changed key outside the read's dependentKeys ({QUERY_ROOT, User:1}).
                    _           <- Sync.defer(client.apolloStore.publish(Set(CacheKey("Post", "99"))))
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
                _                     <- drain.interrupt
                _                     <- tornDown.await
                subscribersAfterClose <- client.apolloStore.changedKeys.subscriberCount
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

        "a write landing between the re-read and the key-set adoption is not lost" in {
            // P2-33, the re-read window. The watch depends on {QUERY_ROOT, User:1}. A
            // write re-points the root at User:2, whose changed keys hit QUERY_ROOT and
            // start the re-read; while that re-read has already loaded User:2 (as "Bob")
            // but not yet adopted {QUERY_ROOT, User:2}, a second write renames User:2 to
            // "Zoe". Its changed key {User:2} is intersected against the OLD set and
            // dropped. Without a store generation the watch shows Bob for good; with it
            // the re-read notices the store moved past its stamp and reads again. The
            // closing write ("Zed") makes the missing emission observable without a
            // hang: it is the second emission only when Zoe was never emitted.
            val cache  = TrapCache(MemoryCache())
            val client = cachedClient(CountingEngine(), cache)
            for
                _     <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull  <- StreamProbe.Pull.open(query(client).fetchPolicy(FetchPolicy.CacheOnly).watch())
                first <- pull.next
                _ = assert(first.data == Present(userData("Alice")))
                _ <- cache.arm(CacheKey("User", "2")) {
                    client.apolloStore.writeFragment(UserFragment, CacheKey("User", "2"), User("User", "2", "Zoe")).unit
                }
                _      <- client.apolloStore.writeOperation(CurrentUserQuery(), UserData(User("User", "2", "Bob")))
                _      <- client.apolloStore.writeFragment(UserFragment, CacheKey("User", "2"), User("User", "2", "Zed"))
                second <- pull.next
                third  <- pull.next
                // Asserted before the last pull: without the fix the third emission IS
                // the closing write, and a further pull would wait forever.
                _ = assert(second.data == Present(UserData(User("User", "2", "Bob"))), s"stale re-read first: $second")
                _ = assert(third.data == Present(UserData(User("User", "2", "Zoe"))), s"the slipped write must be re-read: $third")
                _ = assert(third.cacheInfo.exists(_.generation == 3L), s"the re-read is stamped past the slipped write: $third")
                fourth <- pull.next
            yield assert(fourth.data == Present(UserData(User("User", "2", "Zed"))))
            end for
        }

        "establishFrom stamps the generation of its own read" in {
            // P2-33, the establishing window: the interceptor's initial CacheOnly read
            // has loaded User:1 ("Alice") when a fragment write renames it to "Bob". The
            // watch's key set is still empty, so the publish reaches nobody; the response
            // carries Alice and the generation its read was current at. Adopting that
            // stamp shows the store has moved on, and the watch re-reads to Bob — the
            // second emission a watch without generations never produces.
            val cache  = TrapCache(MemoryCache())
            val client = cachedClient(CountingEngine(), cache)
            for
                _ <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                _ <- cache.arm(CacheKey("User", "1")) {
                    client.apolloStore.writeFragment(UserFragment, CacheKey("User", "1"), User("User", "1", "Bob")).unit
                }
                pull   <- StreamProbe.Pull.open(query(client).fetchPolicy(FetchPolicy.CacheOnly).watch())
                first  <- pull.next
                second <- pull.next
            yield
                assert(first.data == Present(userData("Alice")))
                assert(first.cacheInfo.exists(_.generation == 1L), s"the hit is stamped with the pre-write generation: $first")
                assert(second.data == Present(userData("Bob")), s"the write during the establishing read must be re-read: $second")
                assert(second.cacheInfo.exists(_.generation == 2L))
            end for
        }

        "removing a watched record re-emits a cache miss" in {
            watching { (client, pull) =>
                for
                    _      <- pull.next
                    _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
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
                _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
                second <- pull.next
            yield
                assert(second.error.isEmpty, s"the miss must not reach the consumer: ${second.error}")
                assert(second.data == Present(userData("Alice")))
                assert(engine.calls == 2, s"expected a refetch, got ${engine.calls} call(s)")
            end for
        }

        "CacheFirst does not loop on a permanent miss" in {
            // P2-34. User:1 is removed AND hidden from every read, so the watched read
            // misses for good, while each network answer differs (a volatile field) and
            // its write-back publishes User:1 — which the watch depends on. Without the
            // once-per-cause rule that is an endless chain: write-back → re-read misses →
            // refetch → write-back … one network request per advance of the clock. With
            // it the miss goes to the network exactly once; the next miss is the value.
            Clock.withTimeControl { control =>
                val cache = HoleCache(MemoryCache())
                for
                    clock <- Clock.get
                    calls <- AtomicInt.init(0)
                    client = cachedClient(SleepingEngine(clock, calls), cache)
                    _ <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                    pull <- StreamProbe.Pull.open(
                        query(client)
                            .fetchPolicy(FetchPolicy.CacheOnly)
                            .refetchPolicy(RefetchPolicy.CacheFirst)
                            .watch()
                    )
                    first <- pull.next
                    _ = assert(first.data == Present(userData("Alice-1")))
                    _ <- cache.hide(CacheKey("User", "1"))
                    _ <- client.apolloStore.remove(CacheKey("User", "1"))
                    // The miss went to the network once; that response is parked on the clock.
                    _      <- control.awaitPendingSleepers(1)
                    _      <- control.advance(1.second)
                    second <- pull.next
                    _ = assert(second.data == Present(userData("Alice-2")), s"the networked value is still emitted: $second")
                    // With the fix the miss is already in the channel and this advance finds
                    // no sleeper; a looping watch has parked its next request here instead,
                    // and the advance lets that response through as the third emission.
                    _     <- control.advance(1.second)
                    third <- pull.next
                    _ = assert(
                        third.error.exists(_.isInstanceOf[CacheMissException]),
                        s"a miss the write-back did not cure is the value: $third"
                    )
                    _        <- control.advance(1.second)
                    _        <- control.advance(1.second)
                    callsNow <- calls.get
                    more     <- pull.tryNext
                yield
                    assert(callsNow == 2, s"one refetch per miss cause, got $callsNow call(s)")
                    assert(more == Absent, s"nothing follows the miss: $more")
                end for
            }
        }

        "CacheFirst refetches again for a new miss once a read has succeeded in between" in {
            // The other half of the once-per-cause rule: the refetch's write-back restores
            // User:1, the re-read hits and settles the cause, so a later eviction is a new
            // cause and goes to the network again — the watch does not go dead.
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
                _      <- pull.next
                _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
                second <- pull.next // the write-back's re-read hit
                third  <- pull.next // the networked value
                _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
                fourth <- pull.next
                fifth  <- pull.next
                more   <- pull.tryNext
            yield
                assert(Chunk(second, third, fourth, fifth).forall(_.error.isEmpty), "no miss reaches the consumer")
                assert(fifth.data == Present(userData("Alice")))
                assert(engine.calls == 3, s"each eviction is answered by one refetch, got ${engine.calls} call(s)")
                assert(more == Absent)
            end for
        }

        "notifications during an in-flight refetch coalesce into one rerun" in {
            // The in-flight guard: while a NetworkOnly refetch is parked on the gate, three
            // more writes to a watched key arrive. They book one rerun, not three fibers;
            // the rerun starts after all of them, so its response covers them all.
            for
                calls   <- AtomicInt.init(0)
                arrived <- Latch.init(1)
                gate    <- Latch.init(1)
                client = cachedClient(GatedEngine(calls, arrived, gate))
                _ <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull <- StreamProbe.Pull.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.NetworkOnly)
                        .watch()
                )
                _        <- pull.next
                _        <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                _        <- arrived.await // the refetch is in flight, parked on `gate`
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol"))
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Dave"))
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Eve"))
                _        <- gate.release
                second   <- pull.next     // the in-flight response
                third    <- pull.next     // the one rerun
                callsNow <- calls.get
                more     <- pull.tryNext
            yield
                assert(second.data == Present(userData("Alice")))
                assert(third.data == Present(userData("Alice")))
                assert(callsNow == 3, s"one in flight plus one rerun, got $callsNow call(s)")
                assert(more == Absent, s"no further refetch: $more")
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
