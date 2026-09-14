package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheHeaders
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.cache.normalized.api.RecordValue
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.TestIds
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Optimistic updates.
  *
  * Two layers of coverage. The *store* layer exercises the overlay directly —
  * [[ApolloStore.writeOptimisticUpdates]] overlaying a mutation-id-tagged layer on
  * top of the pristine cache at read time, `rollbackOptimisticUpdates` reverting
  * it, `rollbackAndWrite` reconciling to server truth in one publish, and
  * concurrent layers stacking latest-wins. The *end-to-end* layer drives the
  * feature through the real interceptor chain and a `watch()`, proving a watcher
  * sees the optimistic value first and then the real value (success) or reverts
  * cleanly (failure) — all deterministic via a scripted [[kyo.apollo.network.http.HttpEngine]].
  * On kyo-test the mutation runs on a forked [[Fiber]] while the leaf pulls the
  * watcher's ordered emissions (optimistic, then settled) off a [[StreamProbe.Pull]].
  */
class OptimisticUpdatesSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures ---------------------------------------------------------------

    final case class User(__typename: String, id: String, name: String) derives Schema

    private val userSelections: Chunk[CompiledSelection] =
        Chunk(
            CompiledField("__typename", CompiledNamedType("String")),
            CompiledField("id", CompiledNamedType("String")),
            CompiledField("name", CompiledNamedType("String"))
        )

    private def userField(field: String): CompiledField =
        CompiledField(field, CompiledNamedType("User"), selections = userSelections)

    final case class UserData(user: User) derives Schema

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = Chunk(userField("user")))
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

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

    final case class UserWithFriend(__typename: String, id: String, name: String, friend: User) derives Schema
    final case class TwoUsersData(first: UserWithFriend) derives Schema

    /** `User:1` and its friend `User:2`, one level apart in the selection tree — so a
      * read loads them in two separate batches, which a layer change can fall between.
      */
    final case class TwoUsersQuery() extends Query[TwoUsersData]:
        def name = "TwoUsers"
        def document =
            "query TwoUsers { first { __typename id name friend { __typename id name } } }"
        def dataSchema: Schema[TwoUsersData] = summon[Schema[TwoUsersData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(
                    CompiledField("first", CompiledNamedType("User"), selections = userSelections :+ userField("friend"))
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end TwoUsersQuery

    private def twoUsers(first: String, second: String): TwoUsersData =
        TwoUsersData(UserWithFriend("User", "1", first, User("User", "2", second)))

    private def userData(name: String): UserData         = UserData(User("User", "1", name))
    private def updateData(name: String): UpdateUserData = UpdateUserData(User("User", "1", name))

    private def scalar(s: String): RecordValue = RecordValue.Scalar(Json.JStr(s))

    // --- store-level overlay ----------------------------------------------------

    private def seededStore()(using Frame): ApolloStore < Sync =
        val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
        s.writeOperation(CurrentUserQuery(), userData("Alice")).andThen(s)
    end seededStore

    "optimistic updates" - {

        "writeOptimisticUpdates overlays the optimistic value over a read; cache stays pristine" in {
            for
                s      <- seededStore()
                _      <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
                read   <- s.readOperation(CurrentUserQuery())
                record <- s.cache.loadRecord(CacheKey("User", "1"))
            yield
                // The read reflects the optimistic overlay...
                assert(read == userData("Bob"))
                // ...but the backing cache record is untouched.
                assert(record.flatMap(_.get(fk("name"))) == Present(scalar("Alice")))
            end for
        }

        "writeOptimisticUpdates publishes the record keys it touches" in {
            var seen = Maybe.empty[Set[CacheKey]]
            for
                s       <- seededStore()
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                changed <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            yield
                assert(changed.contains(CacheKey("User", "1")))
                assert(seen == Present(changed))
            end for
        }

        "rollbackOptimisticUpdates reverts the read and publishes the reverted keys" in {
            var seen = Maybe.empty[Set[CacheKey]]
            for
                s        <- seededStore()
                _        <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
                _        <- s.addChangedKeysListener(keys => seen = Present(keys))
                reverted <- s.rollbackOptimisticUpdates("m1")
                read     <- s.readOperation(CurrentUserQuery())
            yield
                assert(reverted.contains(CacheKey("User", "1")))
                assert(seen == Present(reverted))
                assert(read == userData("Alice"))
            end for
        }

        "rollbackOptimisticUpdates on an unknown mutation id is a no-op" in {
            var seen = Maybe.empty[Set[CacheKey]]
            for
                s        <- seededStore()
                _        <- s.addChangedKeysListener(keys => seen = Present(keys))
                reverted <- s.rollbackOptimisticUpdates("nope")
            yield
                assert(reverted == Set.empty[String])
                assert(seen == Absent)
            end for
        }

        "rollbackAndWrite drops the optimistic layer and merges the server truth" in {
            for
                s       <- seededStore()
                _       <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
                changed <- s.rollbackAndWrite(UpdateUserNameMutation("Carol"), updateData("Carol"), "m1")
                read    <- s.readOperation(CurrentUserQuery())
                record  <- s.cache.loadRecord(CacheKey("User", "1"))
            yield
                assert(changed.contains(CacheKey("User", "1")))
                // The layer is gone and the persisted cache now holds the real value.
                assert(read == userData("Carol"))
                assert(record.flatMap(_.get(fk("name"))) == Present(scalar("Carol")))
            end for
        }

        "a read while rollbackAndWrite settles sees the optimistic or the real value, never the one from before" in {
            // Settling touches two cells: the optimistic stack in the store and the record
            // in the backend. The trap reads the store right before and right after the
            // backend commit of the real response — the points a concurrent read can fall
            // between the two steps. Each read must answer with the optimistic value (the
            // layer still over the backend) or the server's; one that sees the value the
            // mutation started from is the flicker the single publish exists to prevent.
            val cache = new CommitTrap(MemoryCache())
            val s     = new ApolloStore(cache, cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            for
                seen      <- AtomicRef.init(Chunk.empty[String])
                published <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                probe = Abort.run[CacheReadFailure](s.readOperation(CurrentUserQuery()))
                    .map(read => seen.updateAndGet(_.append(read.map(_.user.name).getOrElse("<miss>"))).unit)
                _         <- s.writeOperation(CurrentUserQuery(), userData("Alice"))
                _         <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
                _         <- Sync.defer(cache.arm(before = probe, after = probe))
                _         <- s.addChangedKeysListener(keys => published.updateAndGet(_.append(keys)).unit)
                changed   <- s.rollbackAndWrite(UpdateUserNameMutation("Carol"), updateData("Carol"), "m1")
                names     <- seen.get
                publishes <- published.get
                layers    <- s.optimisticLayerIds
                after     <- s.readOperation(CurrentUserQuery())
            yield
                assert(names.size == 2, s"the trap must read before and after the commit: $names")
                assert(
                    names.forall(name => name == "Bob" || name == "Carol"),
                    s"a read during the settle stepped back to the value before the mutation: $names"
                )
                assert(publishes == Chunk(changed), s"the settle publishes once: $publishes")
                assert(layers.isEmpty)
                assert(after == userData("Carol"))
            end for
        }

        "concurrent optimistic layers stack latest-wins; rolling one back keeps the other" in {
            for
                s        <- seededStore()
                _        <- s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
                _        <- s.writeOptimisticUpdates(UpdateUserNameMutation("Dana"), updateData("Dana"), "m2")
                latest   <- s.readOperation(CurrentUserQuery())
                _        <- s.rollbackOptimisticUpdates("m2")
                lower    <- s.readOperation(CurrentUserQuery())
                _        <- s.rollbackOptimisticUpdates("m1")
                pristine <- s.readOperation(CurrentUserQuery())
            yield
                // Latest layer (m2) wins.
                assert(latest == userData("Dana"))
                // Drop the top layer: the lower optimistic layer (m1) is now effective.
                assert(lower == userData("Bob"))
                // Drop the last layer: back to the persisted value.
                assert(pristine == userData("Alice"))
            end for
        }

        "a read sees one consistent layer stack even when a layer is dropped mid-read" in {
            // One layer overlays both users. The trap fires the rollback of that layer
            // right after the read has loaded the batch holding `User:1` and before it
            // loads the next level's `User:2` — the interleaving of a mutation settling
            // while a watcher re-reads. The read must answer from ONE stack: both users
            // optimistic (the stack as of the read's entry) or both persisted, never one
            // of each.
            val cache = new TrapCache(MemoryCache())
            val s     = new ApolloStore(cache, cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            for
                _      <- s.writeOperation(TwoUsersQuery(), twoUsers("Alice", "Ann"))
                _      <- s.writeOptimisticUpdates(TwoUsersQuery(), twoUsers("Bob", "Ben"), "m1")
                _      <- Sync.defer(cache.arm(CacheKey("User", "1"))(s.rollbackOptimisticUpdates("m1").unit))
                read   <- s.readOperation(TwoUsersQuery())
                fired  <- Sync.defer(cache.fired)
                layers <- s.optimisticLayerIds
                // The next read starts from the stack after the rollback.
                next <- s.readOperation(TwoUsersQuery())
            yield
                assert(fired, "the trap must have fired inside the read")
                assert(
                    read == twoUsers("Bob", "Ben") || read == twoUsers("Alice", "Ann"),
                    s"the read mixed two layer stacks: $read"
                )
                assert(layers.isEmpty)
                assert(next == twoUsers("Alice", "Ann"))
            end for
        }

        // --- end-to-end through the interceptor chain + watch() -------------------

        "optimistic mutation: a watcher sees the optimistic value, then the real value" in {
            val client = cachedClient()
            // Seed Alice, then watch the query off the cache.
            for
                _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull <- StreamProbe.Pull.open(
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
                )
                first <- pull.next
                _ = assert(name(first) == Present("Alice"))
                // Fire the mutation on a forked fiber with an optimistic value distinct from
                // the server echo. The optimistic overlay is applied — and observable by the
                // watcher — as the fiber runs the chain up to (but not including) the fetch.
                fib <- Fiber.init(
                    Scope.run(
                        client
                            .mutation(UpdateUserNameMutation("Bob"))
                            .optimisticUpdates(updateData("BobOptimistic"))
                            .fetchPolicy(FetchPolicy.NetworkOnly)
                            .execute
                    )
                )
                optimistic <- pull.next
                _ = assert(name(optimistic) == Present("BobOptimistic"))
                response <- fib.get
                _ = assert(response.error.isEmpty)
                // Network truth ("Bob") replaces the optimistic value; layer is gone.
                settled <- pull.next
                _ = assert(name(settled) == Present("Bob"))
                read <- client.apolloStore.readOperation(CurrentUserQuery())
                _ = assert(read == userData("Bob"))
            yield ()
            end for
        }

        "optimistic mutation failure: the watcher reverts to the pre-optimistic value" in {
            val client = cachedClient(ScriptedEngine(mutationFails = true))
            for
                _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                pull <- StreamProbe.Pull.open(
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()
                )
                first <- pull.next
                _ = assert(name(first) == Present("Alice"))
                fib <- Fiber.init(
                    Scope.run(
                        client
                            .mutation(UpdateUserNameMutation("Bob"))
                            .optimisticUpdates(updateData("BobOptimistic"))
                            .fetchPolicy(FetchPolicy.NetworkOnly)
                            .execute
                    )
                )
                optimistic <- pull.next
                _ = assert(name(optimistic) == Present("BobOptimistic"))
                response <- fib.get
                _ = assert(response.error.isDefined) // the scripted 500 surfaced as a value
                // The optimistic layer rolled back cleanly: watcher is back to Alice, and
                // the pristine cache never took the optimistic value.
                reverted <- pull.next
                _ = assert(name(reverted) == Present("Alice"))
                read <- client.apolloStore.readOperation(CurrentUserQuery())
                _ = assert(read == userData("Alice"))
            yield ()
            end for
        }

        // --- lifecycle: the layer lives exactly as long as the consuming Scope ------

        "an interrupted optimistic mutation leaves no layer" in {
            // The layer is acquired inside the Scope the mutation stream is consumed in;
            // interrupting that consumer closes the Scope, and the close must drop the
            // layer — otherwise the optimistic value wins every later read for good.
            for
                arrived  <- Latch.init(1)
                gate     <- Latch.init(1)
                tornDown <- Latch.init(1)
                client = cachedClient(GatedEngine(arrived, gate))
                store  = client.apolloStore
                _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                // Scope finalizers run last-registered-first, so `tornDown` is released
                // only after the mutation's own layer release has completed.
                fib <- Fiber.init(Scope.run(
                    Scope.ensure(tornDown.release).andThen(
                        client
                            .mutation(UpdateUserNameMutation("Bob"))
                            .optimisticUpdates(updateData("BobOptimistic"))
                            .fetchPolicy(FetchPolicy.NetworkOnly)
                            .execute
                    )
                ))
                _              <- arrived.await // the mutation is on the wire, parked on `gate`
                layersInFlight <- store.optimisticLayerIds
                readInFlight   <- store.readOperation(CurrentUserQuery())
                _              <- fib.interrupt
                _              <- tornDown.await
                _              <- gate.release
                layersAfter    <- store.optimisticLayerIds
                readAfter      <- store.readOperation(CurrentUserQuery())
            yield
                assert(layersInFlight.size == 1, s"the layer must be overlaid while in flight: $layersInFlight")
                assert(readInFlight == userData("BobOptimistic"))
                assert(layersAfter.isEmpty, s"interrupt leaked a layer: $layersAfter")
                assert(readAfter == userData("Alice"))
            end for
        }

        "a never-consumed optimistic stream writes no layer" in {
            // Building the stream must not touch the store: the layer is acquired only
            // when the stream is consumed, so a stream that is dropped unconsumed can
            // never leave one behind.
            val request = ApolloRequest(
                UpdateUserNameMutation("Bob"),
                TestIds.requestUuid,
                optimisticData = Present(updateData("BobOptimistic"))
            )
            for
                store <- seededStore()
                _ = discard(new CacheInterceptor(store).intercept(request, InertChain))
                layers <- store.optimisticLayerIds
                read   <- store.readOperation(CurrentUserQuery())
            yield
                assert(layers.isEmpty, s"building the stream wrote a layer: $layers")
                assert(read == userData("Alice"))
            end for
        }

        "an exception raised below the cache interceptor rolls the layer back" in {
            // A transport that raises instead of answering with an `ApolloResponse`
            // value never reaches the response mapping; the Scope release still runs.
            val client = cachedClient(ScriptedEngine(mutationFails = false), belowCache = List(RaisingInterceptor))
            val store  = client.apolloStore
            for
                _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                result <- Abort.run[Throwable](Scope.run(
                    client
                        .mutation(UpdateUserNameMutation("Bob"))
                        .optimisticUpdates(updateData("BobOptimistic"))
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                        .execute
                ))
                layers <- store.optimisticLayerIds
                read   <- store.readOperation(CurrentUserQuery())
            yield
                assert(!result.isSuccess, s"the raised exception must surface: $result")
                assert(layers.isEmpty, s"the exception leaked a layer: $layers")
                assert(read == userData("Alice"))
            end for
        }

        "a settled optimistic mutation publishes once, and the Scope release finds nothing" in {
            // The reply settles the layer itself (commit, drop, one publish); the release
            // that follows when the Scope closes must neither find a layer nor publish.
            val client = cachedClient()
            val store  = client.apolloStore
            for
                _         <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                published <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _         <- store.addChangedKeysListener(keys => published.updateAndGet(_.append(keys)).unit)
                response <- Scope.run(
                    client
                        .mutation(UpdateUserNameMutation("Bob"))
                        .optimisticUpdates(updateData("BobOptimistic"))
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                        .execute
                )
                publishes <- published.get
                layers    <- store.optimisticLayerIds
                read      <- store.readOperation(CurrentUserQuery())
            yield
                assert(response.error.isEmpty)
                // One publish for the optimistic write, one for the settle — none from the release.
                assert(publishes.size == 2, s"expected the optimistic write and the settle only: $publishes")
                assert(layers.isEmpty)
                assert(read == userData("Bob"))
            end for
        }

        "two executions of one optimistic call stack two layers and roll back independently" in {
            // One call value, executed twice while both replies are held: each execution is
            // its own mutation and must own its own layer. The barrier is "both mutations
            // reached the engine" — each layer is acquired before its request is sent.
            for
                arrivedA <- Latch.init(1)
                arrivedB <- Latch.init(1)
                engine   <- HeldMutationEngine.init(Chunk(arrivedA, arrivedB))
                client = cachedClient(engine)
                store  = client.apolloStore
                call = client
                    .mutation(UpdateUserNameMutation("Bob"))
                    .optimisticUpdates(updateData("BobOptimistic"))
                    .fetchPolicy(FetchPolicy.NetworkOnly)
                _              <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                fibA           <- Fiber.init(Scope.run(call.execute))
                _              <- arrivedA.await
                fibB           <- Fiber.init(Scope.run(call.execute))
                _              <- arrivedB.await
                layersInFlight <- store.optimisticLayerIds
                _              <- engine.reply(0, "Carol")
                _              <- fibA.get
                layersAfterA   <- store.optimisticLayerIds
                readAfterA     <- store.readOperation(CurrentUserQuery())
                _              <- engine.reply(1, "Dave")
                _              <- fibB.get
                layersAfterB   <- store.optimisticLayerIds
                readAfterB     <- store.readOperation(CurrentUserQuery())
            yield
                val seen = s"in flight: $layersInFlight, after A: $layersAfterA / $readAfterA"
                assert(layersInFlight.size == 2 && layersInFlight.distinct.size == 2, s"one layer per execution — $seen")
                assert(layersAfterA.size == 1, s"settling A must drop only A's layer — $seen")
                assert(readAfterA == userData("BobOptimistic"), s"B's optimistic layer must still cover the read — $seen")
                assert(layersAfterB.isEmpty)
                assert(readAfterB == userData("Dave"))
            end for
        }

        "a commit that fails leaves the layer to the Scope release, which reverts and publishes it" in {
            // The server answered, but committing its response fails. At that point the layer
            // must still be on the stack: dropping it before the commit would leave watchers
            // on the optimistic value with nothing published. The Scope release then drops
            // it and publishes the revert.
            val cache  = new CommitTrap(MemoryCache())
            val client = cachedClient(cache = cache)
            val store  = client.apolloStore
            for
                _               <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).execute
                layersAtFailure <- AtomicRef.init(Chunk.empty[String])
                published       <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _               <- store.addChangedKeysListener(keys => published.updateAndGet(_.append(keys)).unit)
                _ <- Sync.defer(cache.arm(
                    before = store.optimisticLayerIds
                        .map(ids => layersAtFailure.set(ids))
                        .andThen(Abort.panic(new IllegalStateException("the backend refused the commit"))),
                    after = Kyo.unit
                ))
                result <- Abort.run[Throwable](Scope.run(
                    client
                        .mutation(UpdateUserNameMutation("Bob"))
                        .optimisticUpdates(updateData("BobOptimistic"))
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                        .execute
                ))
                failing   <- layersAtFailure.get
                layers    <- store.optimisticLayerIds
                publishes <- published.get
                read      <- store.readOperation(CurrentUserQuery())
            yield
                assert(result.isPanic, s"the failed commit must surface: $result")
                assert(failing.size == 1, s"the layer was already dropped when the commit failed: $failing")
                assert(layers.isEmpty, s"the release left the layer behind: $layers")
                assert(
                    publishes.size == 2 && publishes.last.contains(CacheKey("User", "1")),
                    s"the revert was never published: $publishes"
                )
                assert(read == userData("Alice"))
            end for
        }
    }

    /** A cache whose read, once armed for `key`, runs the armed action right after the
      * batch holding `key` has been loaded — inside the read, before its next batch. The
      * loader a read hands the reader is pure, so the action (a store effect) is
      * evaluated in place: that is what lands a change to the optimistic stack
      * deterministically between two levels of one read, on one fiber, on every
      * platform.
      */
    final private class TrapCache(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val trap          = AtomicRef.Unsafe.init(Maybe.empty[(CacheKey, Unit < Sync)])
        private val hit           = AtomicBoolean.Unsafe.init(false)

        def arm(key: CacheKey)(action: => Unit < Sync)(using Frame): Unit =
            discard(trap.getAndSet(Present((key, Sync.defer(action)))))

        def fired: Boolean = hit.get()

        override def read[A](f: RecordLoader => A)(using Frame): A < Sync =
            delegate.read { loader =>
                f { keys =>
                    val loaded = loader.load(keys)
                    trap.get() match
                        case Present((armed, action)) if keys.contains(armed) =>
                            discard(trap.getAndSet(Absent))
                            hit.set(true)
                            Sync.Unsafe.evalOrThrow(action)
                        case _ => ()
                    end match
                    loaded
                }
            }
    end TrapCache

    /** A cache whose next transaction, once armed, runs `before` ahead of the delegate's
      * commit and `after` once that commit has landed. `transact` is an effect, so the
      * actions — a store read, a panic — run in place on the committing fiber: that is
      * what puts a read (or a failure) exactly between the steps of a store operation
      * that commits, on every platform.
      */
    final private class CommitTrap(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val armed         = AtomicRef.Unsafe.init(Maybe.empty[(Unit < Sync, Unit < Sync)])

        def arm(before: => Unit < Sync, after: => Unit < Sync)(using Frame): Unit =
            discard(armed.getAndSet(Present((Sync.defer(before), Sync.defer(after)))))

        override def transact[A](
            f: RecordState => (RecordChanges, A),
            cacheHeaders: CacheHeaders,
            merger: RecordMerger
        )(using Frame): (Set[CacheKey], A) < Sync =
            Sync.defer(armed.getAndSet(Absent)).map {
                case Present((before, after)) =>
                    before.andThen(delegate.transact(f, cacheHeaders, merger)).map(committed => after.andThen(committed))
                case Absent => delegate.transact(f, cacheHeaders, merger)
            }
    end CommitTrap

    // --- end-to-end fixtures ----------------------------------------------------

    /** Routes the mutation to a scripted outcome: a 200 echoing the requested name,
      * or a 500 to simulate a transient transport failure (surfaced as an
      * `ApolloResponse.error` value). The seeding query always returns Alice.
      */
    final private class ScriptedEngine(mutationFails: Boolean)
        extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            val text = request.body.getOrElse("")
            if text.contains("UpdateUserName") then
                if mutationFails then kyo.apollo.network.http.HttpResponse(500, Nil, "boom")
                else
                    val name = """"name":"([^"]*)"""".r.findAllMatchIn(text).map(_.group(1)).toList.last
                    kyo.apollo.network.http.HttpResponse(
                        200,
                        Nil,
                        s"""{"data":{"updateUser":{"__typename":"User","id":"1","name":"$name"}}}"""
                    )
            else
                kyo.apollo.network.http.HttpResponse(
                    200,
                    Nil,
                    """{"data":{"user":{"__typename":"User","id":"1","name":"Alice"}}}"""
                )
            end if
        end execute
    end ScriptedEngine

    /** An engine whose mutation call signals `arrived` and then parks on `gate`, so
      * an optimistic mutation can be held in flight — layer overlaid, reply pending —
      * while the consumer is interrupted. The seeding query answers Alice at once.
      */
    final private class GatedEngine(arrived: Latch, gate: Latch)
        extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            if request.body.getOrElse("").contains("UpdateUserName") then
                arrived.release.andThen(gate.await).andThen(
                    kyo.apollo.network.http.HttpResponse(
                        200,
                        Nil,
                        """{"data":{"updateUser":{"__typename":"User","id":"1","name":"Bob"}}}"""
                    )
                )
            else
                kyo.apollo.network.http.HttpResponse(
                    200,
                    Nil,
                    """{"data":{"user":{"__typename":"User","id":"1","name":"Alice"}}}"""
                )
    end GatedEngine

    /** An engine that holds every mutation reply until the test sends it: the n-th
      * mutation to arrive releases `arrivals(n)` and parks on its own promise, which
      * [[reply]] completes with a server echo carrying `name`. Queries answer Alice at once.
      */
    final private class HeldMutationEngine private (
        arrivals: Chunk[Latch],
        held: AtomicRef[Chunk[Promise[kyo.apollo.network.http.HttpResponse, Any]]]
    ) extends kyo.apollo.network.http.HttpEngine:
        def execute(
            request: kyo.apollo.network.http.HttpRequest
        )(using Frame): kyo.apollo.network.http.HttpResponse < Async =
            if request.body.getOrElse("").contains("UpdateUserName") then
                for
                    reply <- Promise.init[kyo.apollo.network.http.HttpResponse, Any]
                    all   <- held.updateAndGet(_.append(reply))
                    _     <- arrivals(all.size - 1).release
                    sent  <- reply.get
                yield sent
            else
                kyo.apollo.network.http.HttpResponse(
                    200,
                    Nil,
                    """{"data":{"user":{"__typename":"User","id":"1","name":"Alice"}}}"""
                )

        def reply(index: Int, name: String)(using Frame): Unit < Sync =
            held.get.map(_(index).completeDiscard(Result.succeed(kyo.apollo.network.http.HttpResponse(
                200,
                Nil,
                s"""{"data":{"updateUser":{"__typename":"User","id":"1","name":"$name"}}}"""
            ))))
    end HeldMutationEngine

    private object HeldMutationEngine:
        def init(arrivals: Chunk[Latch])(using Frame): HeldMutationEngine < Sync =
            AtomicRef.init(Chunk.empty[Promise[kyo.apollo.network.http.HttpResponse, Any]])
                .map(new HeldMutationEngine(arrivals, _))
    end HeldMutationEngine

    /** A chain whose continuation answers with an empty stream — a stand-in for the
      * network leg when only the interceptor's build-time behaviour is under test.
      */
    private object InertChain extends ApolloInterceptorChain:
        def proceed[D](request: ApolloRequest[D])(using
            Frame,
            Tag[Emit[Chunk[ApolloResponse[D]]]]
        ): ResponseStream[D] =
            Stream.empty[ApolloResponse[D]]
    end InertChain

    /** An interceptor placed below the cache whose mutation stream raises when
      * consumed — an exception instead of an `ApolloResponse` value, as a transport
      * wiring failure would. Queries pass through, so the cache can still be seeded.
      */
    private object RaisingInterceptor extends ApolloInterceptor:
        def intercept[D](
            request: ApolloRequest[D],
            chain: ApolloInterceptorChain
        )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
            request.operation match
                case _: Mutation[?] =>
                    Stream.unwrap[ApolloResponse[D], Async & Scope, Sync](
                        Sync.defer(throw new IllegalStateException("transport raised"))
                    )
                case _ => chain.proceed(request)
    end RaisingInterceptor

    private def cachedClient(
        engine: kyo.apollo.network.http.HttpEngine = ScriptedEngine(mutationFails = false),
        belowCache: List[ApolloInterceptor] = Nil,
        cache: NormalizedCache = MemoryCache()
    ): ApolloClient =
        val builder = ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(cache, IdCacheKeyGenerator(List("id")))
        belowCache.foreach(builder.addInterceptor)
        builder.build()
    end cachedClient

    private def name(response: ApolloResponse[UserData]): Maybe[String] =
        response.data.map(_.user.name)
end OptimisticUpdatesSpec
