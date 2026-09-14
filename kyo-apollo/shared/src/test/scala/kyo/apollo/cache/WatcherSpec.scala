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
import kyo.apollo.exception.ApolloConfigException
import kyo.apollo.exception.CacheMissException
import kyo.apollo.exception.DefaultApolloException
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

    /** Answers Alice and counts its calls; the watch's fetch and refetch fibers call
      * it while the leaf reads the count, hence the atomic.
      */
    final private class CountingEngine extends kyo.apollo.network.http.HttpEngine:
        private val counter = AtomicInt.Unsafe.init(0)(using AllowUnsafe.embrace.danger)
        def calls: Int      = counter.get()(using AllowUnsafe.embrace.danger)
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            counter.safe.incrementAndGet.andThen(kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody))
        end execute
    end CountingEngine

    /** An engine whose second and later calls park on `gate` before answering and
      * signal `arrived` as they start — so a `NetworkOnly` refetch can be held in
      * flight across the watch's teardown. The first call (the cache-populating
      * fetch) answers at once. When `ended` is given, a parked call completes it with
      * how the call ended: `"answered"`, `"interrupted"`, or `"failed"`.
      */
    final private class GatedEngine(
        calls: AtomicInt,
        arrived: Latch,
        gate: Latch,
        ended: Maybe[Promise[String, Any]] = Absent
    ) extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls.incrementAndGet.map { n =>
                val response = kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
                if n == 1 then response
                else
                    Sync.ensure { (error: Maybe[Result.Error[Any]]) =>
                        val how = error match
                            case Absent                                => "answered"
                            case Present(Result.Panic(_: Interrupted)) => "interrupted"
                            case Present(_)                            => "failed"
                        ended.fold(Kyo.unit)(_.completeDiscard(Result.succeed(how)))
                    }(arrived.release.andThen(gate.await).andThen(response))
                end if
            }
    end GatedEngine

    /** An engine whose call number `defective` is a defect — it panics instead of
      * answering or failing on the engine row — and whose other calls answer Alice.
      */
    final private class DefectEngine(calls: AtomicInt, defective: Int) extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            calls.incrementAndGet.map { n =>
                if n == defective then Abort.panic(new RuntimeException("engine defect"))
                else kyo.apollo.network.http.HttpResponse(200, Nil, aliceBody)
            }
    end DefectEngine

    /** An engine whose every answer differs (`Alice-<n>`, a volatile field in each
      * response) and whose second and later calls sleep one second on `clock` before
      * answering. The controlled clock is passed in explicitly, so `control.advance`
      * releases exactly the responses parked on this clock whichever fiber the engine
      * runs on.
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

        def reveal(using Frame): Unit < Sync = Sync.defer(discard(hole.getAndSet(Absent)))

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
    )(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClient.init(
            ApolloClient.Config("https://example.com/graphql")
                .httpEngine(engine)
                .normalizedCache(cache, IdCacheKeyGenerator(List("id")))
        )

    private def query(client: ApolloClient) = client.query(CurrentUserQuery())

    /** Populate the cache with Alice, then run `body` with a client + a
      * [[StreamProbe.Pull]] handle over a `CacheOnly` watch — `pull.next` awaits
      * exactly the next emission, so a test can interleave synchronous store writes
      * (wrapped in `Sync.defer`) between exact, ordered pulls. `body` starts once the
      * initial read is established, so its writes never race the watch's own
      * follow-up on the fetching fiber. The watch's channel and drain fiber are
      * `Scope`-managed, so the runner tears them down on leaf exit.
      */
    private def watching(
        body: (ApolloClient, StreamProbe.Pull[ApolloResponse[UserData]]) => Unit < (Async & Scope)
    )(using Frame): Unit < (Async & Scope) =
        val engine = CountingEngine()
        for
            client <- cachedClient(engine)
            _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
            watch  <- ObservedWatch.open(query(client).fetchPolicy(FetchPolicy.CacheOnly))
            _      <- watch.awaitEstablished
            _      <- body(client, watch.pull)
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
                    _ <- Sync.defer(client.apolloStore.publish(Set(CacheKey("Post", "99"))))
                    // Both reactions run on this fiber, in write order: a re-emission for the
                    // unrelated key would be the next emission, ahead of Bob.
                    _      <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                    second <- pull.next
                    more   <- pull.tryNext
                yield
                    assert(second.data == Present(userData("Bob")), s"the unrelated write re-emitted: $second")
                    assert(more == Absent)
            }
        }

        "closing the watch's Scope unsubscribes it" in {
            // A cancelled pull is silent by construction (its channel is closed), so the
            // property is asserted on the store, after the teardown has provably run.
            for
                client <- cachedClient(CountingEngine())
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch  <- ObservedWatch.open(query(client).fetchPolicy(FetchPolicy.CacheOnly))
                first  <- watch.pull.next
                before <- client.apolloStore.changedKeys.subscriberCount
                _      <- watch.close
                after  <- client.apolloStore.changedKeys.subscriberCount
            yield
                assert(first.data == Present(userData("Alice")))
                assert(before == 1)
                assert(after == 0, s"the closed watch is still subscribed: $after")
            end for
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
                client   <- cachedClient(GatedEngine(calls, arrived, gate))
                _        <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                probe    <- Channel.init[ApolloResponse[UserData]](Int.MaxValue)
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

        "a refetch fiber does not outlive its watch" in {
            // P2-40. A NetworkOnly refetch is parked on the engine when the watch's Scope
            // closes. The refetch fiber belongs to that Scope, so the close interrupts the
            // request in flight: the engine sees its call end by interruption even though
            // the gate opens right after, and nothing reaches the consumer. A detached
            // fiber would outlive the watch and answer normally once the gate opens.
            for
                calls    <- AtomicInt.init(0)
                arrived  <- Latch.init(1)
                gate     <- Latch.init(1)
                ended    <- Promise.init[String, Any]
                tornDown <- Latch.init(1)
                client   <- cachedClient(GatedEngine(calls, arrived, gate, Present(ended)))
                _        <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                probe    <- Channel.init[ApolloResponse[UserData]](Int.MaxValue)
                // Registered first, so it runs after every finalizer of the watch.
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
                _       <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob"))
                _       <- arrived.await // the refetch is in flight, parked on `gate`
                _       <- drain.interrupt
                _       <- tornDown.await
                _       <- gate.release
                outcome <- ended.get
                late    <- probe.poll
            yield
                assert(outcome == "interrupted", s"closing the watch must interrupt its refetch in flight, the request $outcome")
                assert(late == Absent, s"an emission reached the consumer after teardown: $late")
            end for
        }

        "a failing refetch surfaces as a response" in {
            // P2-40. The refetch's engine call panics. The watch's consumer sees that as an
            // error response, and the watch goes on: the next write's refetch answers.
            // A detached fiber that swallows its failure emits nothing for the first write.
            for
                calls  <- AtomicInt.init(0)
                client <- cachedClient(DefectEngine(calls, defective = 2))
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch <- ObservedWatch.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.NetworkOnly)
                )
                pull = watch.pull
                first <- pull.next
                // Writes before the key set is established would reach no one and be answered
                // by a single refetch from the watch's window check: one response, not two.
                _ <- watch.awaitEstablished
                _ = assert(first.data == Present(userData("Alice")))
                _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob"))   // its refetch panics
                _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol")) // its refetch answers
                second <- pull.next
                _ = assert(second.error.nonEmpty, s"the failed refetch must reach the consumer as a response: $second")
                third <- pull.next
            yield
                assert(second.data.isEmpty)
                assert(
                    second.error.exists(e => e.isInstanceOf[DefaultApolloException] && e.getCause.getMessage == "engine defect"),
                    s"the response carries the defect as its cause: ${second.error}"
                )
                assert(third.data == Present(userData("Alice")), s"the watch refetches again after a failure: $third")
            end for
        }

        "a failing initial fetch surfaces as a response" in {
            // The initial fetch runs on a fiber of the watch too; its defect is the
            // watch's first emission rather than a silent, empty stream.
            for
                calls  <- AtomicInt.init(0)
                client <- cachedClient(DefectEngine(calls, defective = 1))
                pull   <- StreamProbe.Pull.open(query(client).fetchPolicy(FetchPolicy.NetworkOnly).watch())
                first  <- pull.next
            yield
                assert(first.data.isEmpty)
                assert(
                    first.error.exists(e => e.isInstanceOf[DefaultApolloException] && e.getCause.getMessage == "engine defect"),
                    s"the response carries the defect as its cause: ${first.error}"
                )
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
            // hang: it is the second emission only when Zoe was never emitted. The writes
            // start once the initial read is established: before that the watch's key set
            // may still be empty, the root write would reach no one, and the watch's own
            // window check would read straight to the closing write.
            val cache = TrapCache(MemoryCache())
            for
                client <- cachedClient(CountingEngine(), cache)
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch  <- ObservedWatch.open(query(client).fetchPolicy(FetchPolicy.CacheOnly))
                pull = watch.pull
                first <- pull.next
                _     <- watch.awaitEstablished
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
            val cache = TrapCache(MemoryCache())
            for
                client <- cachedClient(CountingEngine(), cache)
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
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

        "a write made on a networked emission, before the watch has established it, is not lost" in {
            // A consumer answers the first emission with a write. For a networked response
            // the watch establishes its key set by reading the records back AFTER the offer:
            // until then the set is empty and the write's publish reaches no one. Stamped
            // with that later read's own generation, the set already "contains" the write,
            // the window check finds nothing, and the watch keeps showing the networked
            // Alice while the store holds Bob. Stamped with the generation of the offer,
            // the write is newer than the emitted value and is read again. The write runs on
            // the fetching fiber right after the offer — the position of a consumer that is
            // faster than the watch — and the closing write (Zed), made once the response is
            // established, is the second emission exactly when Bob was lost.
            val engine = CountingEngine()
            for
                client  <- cachedClient(engine)
                written <- AtomicBoolean.init(false)
                watch <- ObservedWatch.open(
                    query(client).fetchPolicy(FetchPolicy.NetworkOnly),
                    onOffered = _ =>
                        written.compareAndSet(false, true).map { first =>
                            if first then client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")).unit
                            else Kyo.unit
                        }
                )
                first  <- watch.pull.next
                _      <- watch.awaitEstablished
                _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Zed"))
                second <- watch.pull.next
                _ = assert(first.data == Present(userData("Alice")))
                _ = assert(first.cacheInfo.exists(!_.isCacheHit), s"the first emission is the networked response: $first")
                _ = assert(second.data == Present(userData("Bob")), s"the write made on the first emission was lost: $second")
                third <- watch.pull.next
            yield
                assert(third.data == Present(userData("Zed")))
                assert(engine.calls == 1, s"the re-read stays in the cache: ${engine.calls} call(s)")
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
            for
                client <- cachedClient(engine)
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch <- ObservedWatch.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.CacheFirst)
                )
                pull = watch.pull
                first <- pull.next
                _     <- watch.awaitEstablished
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
                    clock  <- Clock.get
                    calls  <- AtomicInt.init(0)
                    client <- cachedClient(SleepingEngine(clock, calls), cache)
                    _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                    watch <- ObservedWatch.open(
                        query(client)
                            .fetchPolicy(FetchPolicy.CacheOnly)
                            .refetchPolicy(RefetchPolicy.CacheFirst)
                    )
                    pull = watch.pull
                    first <- pull.next
                    _     <- watch.awaitEstablished
                    _ = assert(first.data == Present(userData("Alice-1")))
                    _ <- cache.hide(CacheKey("User", "1"))
                    _ <- client.apolloStore.remove(CacheKey("User", "1"))
                    // The miss went to the network once; that response is parked on the clock.
                    _      <- control.awaitPendingSleepers(1)
                    _      <- control.advance(1.second, Duration.Zero)
                    second <- pull.next
                    _ = assert(second.data == Present(userData("Alice-2")), s"the networked value is still emitted: $second")
                    // The flight has ended. A looping watch booked the next request during it
                    // (its write-back re-read missed again); with the rule it booked nothing and
                    // settled the miss as the value.
                    rerun <- watch.awaitRefetchEnded
                    _ = assert(!rerun, "a miss the write-back did not cure booked another refetch")
                    third <- pull.next
                    _ = assert(
                        third.error.exists(_.isInstanceOf[CacheMissException]),
                        s"a miss the write-back did not cure is the value: $third"
                    )
                    callsNow <- calls.get
                    // Nothing else is running in the watch now. A write it can read is answered
                    // on this fiber; a second miss for the old cause would come first.
                    _      <- cache.reveal
                    _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Zed"))
                    fourth <- pull.next
                    more   <- pull.tryNext
                yield
                    assert(callsNow == 2, s"one refetch per miss cause, got $callsNow call(s)")
                    assert(fourth.data == Present(userData("Zed")), s"nothing follows the miss but the next write: $fourth")
                    assert(more == Absent, s"nothing follows the miss: $more")
                end for
            }
        }

        "CacheFirst refetches again for a new miss once a read has succeeded in between" in {
            // The other half of the once-per-cause rule: the refetch's write-back restores
            // User:1, the re-read hits and settles the cause, so a later eviction is a new
            // cause and goes to the network again — the watch does not go dead.
            val engine = CountingEngine()
            for
                client <- cachedClient(engine)
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch <- ObservedWatch.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.CacheFirst)
                )
                pull = watch.pull
                _      <- pull.next
                _      <- watch.awaitEstablished
                _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
                second <- pull.next // the write-back's re-read hit
                third  <- pull.next // the networked value
                rerun1 <- watch.awaitRefetchEnded
                _      <- Sync.defer(client.apolloStore.remove(CacheKey("User", "1")))
                fourth <- pull.next
                fifth  <- pull.next
                rerun2 <- watch.awaitRefetchEnded
                calls = engine.calls
                // The watch is idle: a readable write is answered on this fiber, and a miss
                // left over from either eviction would come before it.
                _     <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob"))
                sixth <- pull.next
                more  <- pull.tryNext
            yield
                assert(Chunk(second, third, fourth, fifth).forall(_.error.isEmpty), "no miss reaches the consumer")
                assert(fifth.data == Present(userData("Alice")))
                assert(!rerun1 && !rerun2, "a refetch whose write-back cured the miss books nothing further")
                assert(calls == 3, s"each eviction is answered by one refetch, got $calls call(s)")
                assert(sixth.data == Present(userData("Bob")), s"nothing but the next write follows: $sixth")
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
                client  <- cachedClient(GatedEngine(calls, arrived, gate))
                _       <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                watch <- ObservedWatch.open(
                    query(client)
                        .fetchPolicy(FetchPolicy.CacheOnly)
                        .refetchPolicy(RefetchPolicy.NetworkOnly)
                )
                pull = watch.pull
                _        <- pull.next
                _        <- watch.awaitEstablished
                _        <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                _        <- arrived.await           // the refetch is in flight, parked on `gate`
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Carol"))
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Dave"))
                _        <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Eve"))
                _        <- gate.release
                second   <- pull.next               // the in-flight response
                booked   <- watch.awaitRefetchEnded // ... whose flight booked the rerun
                third    <- pull.next               // the one rerun
                rebooked <- watch.awaitRefetchEnded // ... whose flight booked nothing more
                callsNow <- calls.get
            yield
                assert(second.data == Present(userData("Alice")))
                assert(third.data == Present(userData("Alice")))
                assert(booked, "the writes during the flight booked no rerun")
                assert(!rebooked, "the rerun booked a further refetch")
                assert(callsNow == 3, s"one in flight plus one rerun, got $callsNow call(s)")
            end for
        }

        "a second watcher is independent — closing one does not silence the other" in {
            for
                client <- cachedClient(CountingEngine())
                _      <- query(client).fetchPolicy(FetchPolicy.NetworkOnly).execute
                first  <- ObservedWatch.open(query(client).fetchPolicy(FetchPolicy.CacheOnly))
                _      <- first.pull.next
                other  <- ObservedWatch.open(query(client).fetchPolicy(FetchPolicy.CacheOnly))
                _      <- other.pull.next
                _      <- other.awaitEstablished
                _      <- first.close // the first watch only, teardown included
                left   <- client.apolloStore.changedKeys.subscriberCount
                _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Dave"))
                after  <- other.pull.next
            yield
                assert(left == 1, s"closing one watch must leave exactly the other subscribed: $left")
                assert(after.data == Present(userData("Dave")), s"the other watch is still live: $after")
            end for
        }

        "whether a client has a cache is a total question: normalizedStore" in {
            for
                bare   <- ApolloClient.init(ApolloClient.Config("https://example.com/graphql").httpEngine(CountingEngine()))
                cached <- cachedClient(CountingEngine())
            yield
                assert(bare.normalizedStore == Absent)
                assert(cached.normalizedStore.exists(_ eq cached.apolloStore))
            end for
        }

        "apolloStore on a client without a cache is a misuse panic naming the fix, not an IllegalStateException" in {
            ApolloClient.init(ApolloClient.Config("https://example.com/graphql").httpEngine(CountingEngine())).map { client =>
                val misuse = intercept[ApolloConfigException](client.apolloStore)
                assert(misuse.message.contains("normalizedCache"))
            }
        }
    }
end WatcherSpec
