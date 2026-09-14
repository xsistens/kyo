package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.cache.normalized.api.RecordValue
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ExecutionContext
import kyo.apollo.runtime.ResponseStream
import scala.collection.immutable.VectorMap

/** Phase 07 Task 2: optimistic updates.
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

    final case class TwoUsersData(first: User, second: User) derives Schema

    /** Two entity fields off the root, read in declaration order (`User:1`, then
      * `User:2`) — the shape whose two loads can straddle a layer change.
      */
    final case class TwoUsersQuery() extends Query[TwoUsersData]:
        def name                             = "TwoUsers"
        def document                         = "query TwoUsers { first { __typename id name } second { __typename id name } }"
        def dataSchema: Schema[TwoUsersData] = summon[Schema[TwoUsersData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(userField("first"), userField("second"))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end TwoUsersQuery

    private def twoUsers(first: String, second: String): TwoUsersData =
        TwoUsersData(User("User", "1", first), User("User", "2", second))

    private def userData(name: String): UserData         = UserData(User("User", "1", name))
    private def updateData(name: String): UpdateUserData = UpdateUserData(User("User", "1", name))

    private def scalar(s: String): RecordValue = RecordValue.Scalar(Json.JStr(s))

    // --- store-level overlay ----------------------------------------------------

    private def seededStore(): ApolloStore =
        val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
        s.writeOperation(CurrentUserQuery(), userData("Alice"))
        s
    end seededStore

    "optimistic updates" - {

        "writeOptimisticUpdates overlays the optimistic value over a read; cache stays pristine" in {
            val s = seededStore()
            s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            // The read reflects the optimistic overlay...
            assert(s.readOperation(CurrentUserQuery()) == userData("Bob"))
            // ...but the backing cache record is untouched.
            assert(s.cache.loadRecord("User:1").flatMap(_.get("name")) == Present(scalar("Alice")))
        }

        "writeOptimisticUpdates publishes the record keys it touches" in {
            val s    = seededStore()
            var seen = Option.empty[Set[String]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            val changed = s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            assert(changed.contains("User:1"))
            assert(seen == Some(changed))
        }

        "rollbackOptimisticUpdates reverts the read and publishes the reverted keys" in {
            val s = seededStore()
            s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            var seen = Option.empty[Set[String]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            val reverted = s.rollbackOptimisticUpdates("m1")
            assert(reverted.contains("User:1"))
            assert(seen == Some(reverted))
            assert(s.readOperation(CurrentUserQuery()) == userData("Alice"))
        }

        "rollbackOptimisticUpdates on an unknown mutation id is a no-op" in {
            val s    = seededStore()
            var seen = Option.empty[Set[String]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            assert(s.rollbackOptimisticUpdates("nope") == Set.empty[String])
            assert(seen == None)
        }

        "rollbackAndWrite drops the optimistic layer and merges the server truth" in {
            val s = seededStore()
            s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            val changed = s.rollbackAndWrite(UpdateUserNameMutation("Carol"), updateData("Carol"), "m1")
            assert(changed.contains("User:1"))
            // The layer is gone and the persisted cache now holds the real value.
            assert(s.readOperation(CurrentUserQuery()) == userData("Carol"))
            assert(s.cache.loadRecord("User:1").flatMap(_.get("name")) == Present(scalar("Carol")))
        }

        "concurrent optimistic layers stack latest-wins; rolling one back keeps the other" in {
            val s = seededStore()
            s.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), updateData("Bob"), "m1")
            s.writeOptimisticUpdates(UpdateUserNameMutation("Dana"), updateData("Dana"), "m2")
            // Latest layer (m2) wins.
            assert(s.readOperation(CurrentUserQuery()) == userData("Dana"))
            // Drop the top layer: the lower optimistic layer (m1) is now effective.
            s.rollbackOptimisticUpdates("m2")
            assert(s.readOperation(CurrentUserQuery()) == userData("Bob"))
            // Drop the last layer: back to the persisted value.
            s.rollbackOptimisticUpdates("m1")
            assert(s.readOperation(CurrentUserQuery()) == userData("Alice"))
        }

        "a read sees one consistent layer stack even when a layer is dropped mid-read" in {
            // One layer overlays both users. The trap fires the rollback of that layer
            // right after the read has loaded `User:1` and before it loads `User:2` —
            // the interleaving of a mutation settling while a watcher re-reads. The
            // read must answer from ONE stack: both users optimistic (the stack as of
            // the read's entry) or both persisted, never one of each.
            val cache = new TrapCache(MemoryCache())
            val s     = new ApolloStore(cache, cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            s.writeOperation(TwoUsersQuery(), twoUsers("Alice", "Ann"))
            s.writeOptimisticUpdates(TwoUsersQuery(), twoUsers("Bob", "Ben"), "m1")
            cache.arm("User:1")(discard(s.rollbackOptimisticUpdates("m1")))
            val read = s.readOperation(TwoUsersQuery())
            assert(
                read == twoUsers("Bob", "Ben") || read == twoUsers("Alice", "Ann"),
                s"the read mixed two layer stacks: $read"
            )
            assert(s.optimisticLayerIds.isEmpty)
            // The next read starts from the stack after the rollback.
            assert(s.readOperation(TwoUsersQuery()) == twoUsers("Alice", "Ann"))
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
                _ = assert(client.apolloStore.readOperation(CurrentUserQuery()) == userData("Bob"))
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
                _ = assert(client.apolloStore.readOperation(CurrentUserQuery()) == userData("Alice"))
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
                _ <- arrived.await // the mutation is on the wire, parked on `gate`
                layersInFlight = store.optimisticLayerIds
                readInFlight   = store.readOperation(CurrentUserQuery())
                _ <- fib.interrupt
                _ <- tornDown.await
                _ <- gate.release
            yield
                assert(layersInFlight.size == 1, s"the layer must be overlaid while in flight: $layersInFlight")
                assert(readInFlight == userData("BobOptimistic"))
                assert(store.optimisticLayerIds.isEmpty, s"interrupt leaked a layer: ${store.optimisticLayerIds}")
                assert(store.readOperation(CurrentUserQuery()) == userData("Alice"))
            end for
        }

        "a never-consumed optimistic stream writes no layer" in {
            // Building the stream must not touch the store: the layer is acquired only
            // when the stream is consumed, so a stream that is dropped unconsumed can
            // never leave one behind.
            val store       = seededStore()
            val interceptor = new CacheInterceptor(store)
            val request = ApolloRequest
                .builder(UpdateUserNameMutation("Bob"))
                .addExecutionContext(ExecutionContext.Empty + OptimisticData(updateData("BobOptimistic"), "m1"))
                .build()
            discard(interceptor.intercept(request, InertChain))
            assert(store.optimisticLayerIds.isEmpty, s"building the stream wrote a layer: ${store.optimisticLayerIds}")
            assert(store.readOperation(CurrentUserQuery()) == userData("Alice"))
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
            yield
                assert(!result.isSuccess, s"the raised exception must surface: $result")
                assert(store.optimisticLayerIds.isEmpty, s"the exception leaked a layer: ${store.optimisticLayerIds}")
                assert(store.readOperation(CurrentUserQuery()) == userData("Alice"))
            end for
        }
    }

    /** A cache whose `loadRecord(key)` for the armed `key` loads the record and
      * THEN runs the armed action once, re-entrantly — so an action that changes
      * the store's optimistic stack lands deterministically between two loads of
      * the same read, on one thread, on every platform.
      */
    final private class TrapCache(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val trap          = AtomicRef.Unsafe.init(Maybe.empty[(String, () => Unit)])

        def arm(key: String)(action: => Unit): Unit = discard(trap.getAndSet(Present((key, () => action))))

        override def loadRecord(key: String): Maybe[Record] =
            val record = delegate.loadRecord(key)
            trap.get() match
                case Present((armed, action)) if armed == key =>
                    discard(trap.getAndSet(Absent))
                    action()
                case _ => ()
            end match
            record
        end loadRecord
    end TrapCache

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
        belowCache: List[ApolloInterceptor] = Nil
    ): ApolloClient =
        val builder = ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(engine)
            .normalizedCache(MemoryCache(), IdCacheKeyGenerator(List("id")))
        belowCache.foreach(builder.addInterceptor)
        builder.build()
    end cachedClient

    private def name(response: ApolloResponse[UserData]): Maybe[String] =
        response.data.map(_.user.name)
end OptimisticUpdatesSpec
