package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.StreamProbe
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloResponse
import scala.collection.immutable.VectorMap

/** Garbage collection, eviction, and TTL/expiration.
  *
  * Three layers of coverage:
  *   - **Reachability GC** on [[ApolloStore]] — `garbageCollect` /
  *     `removeUnreachableRecords` sweeping records unreachable from the operation
  *     roots (following references, including through lists), while an optimistic
  *     layer, a live watch or an explicit `retain` *pins* the records it depends on,
  *     and the removed keys are published.
  *   - **Eviction** — `evict(cacheKey)` dropping a record (and, with `cascade`,
  *     its referenced subtree) and publishing the removed keys.
  *   - **Per-field TTL** on [[MemoryCache]] — `maxAge` expiring individual fields,
  *     `removeExpiredRecords` sweeping them eagerly, `allRecords` snapshots, and
  *     the [[NormalizedCacheDecorator]] persistence seam.
  */
class GarbageCollectionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def scalar(s: String): RecordValue                               = RecordValue.Scalar(Json.JStr(s))
    private def ref(key: CacheKey): RecordValue                              = RecordValue.Reference(CacheReference(key))
    private def rec(key: CacheKey, fields: (FieldKey, RecordValue)*): Record = Record(key, fields.toMap)

    /** Bare-typename record keys for the backend-level cases, where the key's shape is irrelevant. */
    private val keyA = CacheKey("A", "")
    private val keyB = CacheKey("B", "")

    // --- reachability GC --------------------------------------------------------

    private def graphStore()(using Frame): ApolloStore < Sync =
        val s = new ApolloStore(MemoryCache())
        s.cache.merge(
            Chunk(
                rec(CacheKey.QueryRoot, fk("book")      -> ref(CacheKey("Book", "1"))),
                rec(CacheKey("Book", "1"), fk("title")  -> scalar("Dune"), fk("author") -> ref(CacheKey("Author", "1"))),
                rec(CacheKey("Author", "1"), fk("name") -> scalar("Herbert")),
                rec(CacheKey("Orphan", "1"), fk("x")    -> scalar("1"))
            )
        ).andThen(s)
    end graphStore

    // --- optimistic pinning -----------------------------------------------------

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

    // --- watched records --------------------------------------------------------

    final case class UserData(user: User) derives Schema

    /** `{ user { __typename id name } }` — QUERY_ROOT, then `User:<id>`. */
    final case class UserQuery() extends Query[UserData]:
        def name                         = "User"
        def document                     = "query User { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = Chunk(userField("user")))
        def variables: Json = Json.JObj(VectorMap.empty)
    end UserQuery

    private def userData(id: String, name: String): UserData = UserData(User("User", id, name))

    /** A cache whose next read, once armed, first runs `action` on the reading fiber and
      * only then reads. `read` is an effect, so the action — a garbage collection, a
      * write — lands in place inside the store operation that reads (a watcher's
      * re-read), on every platform.
      */
    final private class ReadTrap(delegate: NormalizedCache) extends NormalizedCacheDecorator(delegate):
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val armed         = AtomicRef.Unsafe.init(Maybe.empty[Unit < Sync])

        def arm(action: => Unit < Sync)(using Frame): Unit < Sync =
            Sync.defer(discard(armed.getAndSet(Present(Sync.defer(action)))))

        override def read[A](f: RecordLoader => A)(using Frame): A < Sync =
            Sync.defer(armed.getAndSet(Absent)).map {
                case Present(action) => action.andThen(delegate.read(f))
                case Absent          => delegate.read(f)
            }
    end ReadTrap

    /** A client over `cache` for `CacheOnly` watches: nothing here reaches the network. */
    private def clientOver(cache: NormalizedCache): ApolloClient =
        ApolloClient
            .builder()
            .serverUrl("https://example.com/graphql")
            .httpEngine(new kyo.apollo.network.http.HttpEngine:
                def execute(request: kyo.apollo.network.http.HttpRequest)(using Frame) =
                    Abort.panic(IllegalStateException("GarbageCollectionSpec does not reach the network")))
            .normalizedCache(cache, IdCacheKeyGenerator(List("id")))
            .build()

    private def watchUser(client: ApolloClient)(using Frame) =
        client.query(UserQuery()).fetchPolicy(FetchPolicy.CacheOnly).watch()

    "garbageCollect / evict / MemoryCache TTL / NormalizedCacheDecorator" - {

        "garbageCollect removes records unreachable from a root, transitively keeping the rest" in {
            for
                s       <- graphStore()
                removed <- s.garbageCollect
                root    <- s.cache.loadRecord(CacheKey.QueryRoot)
                book    <- s.cache.loadRecord(CacheKey("Book", "1"))
                author  <- s.cache.loadRecord(CacheKey("Author", "1"))
                orphan  <- s.cache.loadRecord(CacheKey("Orphan", "1"))
            yield
                assert(removed == Set(CacheKey("Orphan", "1")))
                assert(root.isDefined)
                assert(book.isDefined)
                assert(author.isDefined) // reachable QUERY_ROOT → Book:1 → Author:1
                assert(orphan == Absent)
            end for
        }

        "garbageCollect keeps records reachable only through a list reference" in {
            val s = new ApolloStore(MemoryCache())
            for
                _ <- s.cache.merge(
                    Chunk(
                        rec(
                            CacheKey.QueryRoot,
                            fk("books") -> RecordValue.RList(Chunk(ref(CacheKey("Book", "1")), ref(CacheKey("Book", "2"))))
                        ),
                        rec(CacheKey("Book", "1"), fk("title") -> scalar("A")),
                        rec(CacheKey("Book", "2"), fk("title") -> scalar("B")),
                        rec(CacheKey("Orphan", "1"), fk("x")   -> scalar("1"))
                    )
                )
                removed <- s.garbageCollect
                book1   <- s.cache.loadRecord(CacheKey("Book", "1"))
                book2   <- s.cache.loadRecord(CacheKey("Book", "2"))
            yield
                assert(removed == Set(CacheKey("Orphan", "1")))
                assert(book1.isDefined)
                assert(book2.isDefined)
            end for
        }

        "garbageCollect on an empty store removes nothing" in {
            val s = new ApolloStore(MemoryCache())
            s.garbageCollect.map(removed => assert(removed == Set.empty[CacheKey]))
        }

        "garbageCollect publishes the keys it removed, and a sweep that removes nothing publishes nothing" in {
            // A watcher between two reads holds no retain on a record it is about to
            // depend on; the publish is what makes it read again instead of going silent.
            for
                s         <- graphStore()
                seen      <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _         <- s.addChangedKeysListener(keys => seen.updateAndGet(_.append(keys)).unit)
                removed   <- s.garbageCollect
                again     <- s.garbageCollect
                published <- seen.get
            yield
                assert(removed == Set(CacheKey("Orphan", "1")))
                assert(again == Set.empty[CacheKey])
                assert(published == Chunk(Set(CacheKey("Orphan", "1"))), s"a subscriber must see what GC removed: $published")
            end for
        }

        "removeUnreachableRecords is the sweep helper garbageCollect delegates to" in {
            for
                s      <- graphStore()
                first  <- s.removeUnreachableRecords
                second <- s.removeUnreachableRecords // a no-op now — everything left is reachable
            yield
                assert(first == Set(CacheKey("Orphan", "1")))
                assert(second == Set.empty[CacheKey])
            end for
        }

        "an optimistic layer pins an otherwise-unreachable record from GC, releasing it on rollback" in {
            val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            for
                // A persisted record reachable from no root — a GC candidate on its own.
                _ <- s.cache.merge(Chunk(rec(CacheKey("User", "1"), fk("name") -> scalar("Alice"))))
                // An optimistic mutation references User:1, so GC must keep it alive.
                _ <- s.writeOptimisticUpdates(
                    UpdateUserNameMutation("Bob"),
                    UpdateUserData(User("User", "1", "Bob")),
                    "m1"
                )
                pinned   <- s.garbageCollect
                kept     <- s.cache.loadRecord(CacheKey("User", "1"))
                _        <- s.rollbackOptimisticUpdates("m1")
                released <- s.garbageCollect // the layer is gone, the record is unreachable again
            yield
                assert(pinned == Set.empty[CacheKey])
                assert(kept.isDefined)
                assert(released == Set(CacheKey("User", "1")))
            end for
        }

        // --- watched and retained records -------------------------------------------

        "a live watcher's dependencies survive GC after its root was rebound, until it has re-read" in {
            // The watch depends on {QUERY_ROOT, User:1}. A write re-points the root at
            // User:2 and publishes, so the watcher re-reads — and a GC runs inside that
            // re-read, before its load. No root reaches User:1 any more, but the watcher
            // still depends on it: it must survive until the watcher has adopted
            // {QUERY_ROOT, User:2}. A GC after that removes it.
            val cache  = ReadTrap(MemoryCache())
            val client = clientOver(cache)
            val store  = client.apolloStore
            for
                _         <- store.writeOperation(UserQuery(), userData("1", "Alice"))
                pull      <- StreamProbe.Pull.open(watchUser(client))
                first     <- pull.next
                during    <- AtomicRef.init(Maybe.empty[Set[CacheKey]])
                _         <- cache.arm(store.garbageCollect.map(removed => during.set(Present(removed))))
                _         <- store.writeOperation(UserQuery(), userData("2", "Bob"))
                second    <- pull.next // offered only after the re-read adopted its key set
                collected <- during.get
                after     <- store.garbageCollect
                user1     <- store.cache.loadRecord(CacheKey("User", "1"))
            yield
                assert(first.data == Present(userData("1", "Alice")))
                assert(collected == Present(Set.empty[CacheKey]), s"GC removed a record a live watcher depends on: $collected")
                assert(second.data == Present(userData("2", "Bob")))
                assert(after == Set(CacheKey("User", "1")), s"once the watcher moved on, its old record is garbage: $after")
                assert(user1 == Absent)
            end for
        }

        "a retain pins a record no root reaches for its Scope, counting every open retain" in {
            // The fragment-read case without a UI: Orphan:1 is reachable from no root. Two
            // nested retains on it; closing the inner one leaves it pinned by the outer,
            // closing both frees it.
            val orphan = CacheKey("Orphan", "1")
            for
                s <- graphStore()
                sweeps <- Scope.run {
                    s.retain(Set(orphan)).andThen {
                        Scope.run(s.retain(Set(orphan)).andThen(s.garbageCollect)).map(inner => s.garbageCollect.map((inner, _)))
                    }
                }
                released <- s.garbageCollect
            yield
                assert(sweeps._1 == Set.empty[CacheKey], s"GC removed a retained record: ${sweeps._1}")
                assert(sweeps._2 == Set.empty[CacheKey], s"closing one retain released a key another still holds: ${sweeps._2}")
                assert(released == Set(orphan), s"a closed retain still pins its key: $released")
            end for
        }

        "a retained record keeps the records it references reachable" in {
            val s      = new ApolloStore(MemoryCache())
            val book   = CacheKey("Book", "2")
            val author = CacheKey("Author", "2")
            for
                _ <- s.cache.merge(
                    Chunk(rec(book, fk("author") -> ref(author)), rec(author, fk("name") -> scalar("Le Guin")))
                )
                pinned   <- Scope.run(s.retain(Set(book)).andThen(s.garbageCollect))
                released <- s.garbageCollect
            yield
                assert(pinned == Set.empty[CacheKey])
                assert(released == Set(book, author))
            end for
        }

        "a watch that has ended no longer pins what it depended on" in {
            // The watch's own first read is trapped: a rename lands inside it, so the watch
            // re-reads once on its fetch fiber and emits a second time. That emission is
            // offered after the re-read adopted its key set, and it leaves the fetch fiber
            // nothing more to adopt — so once the watch's Scope has closed, nothing holds
            // User:1 but a retain the teardown failed to release.
            val cache  = ReadTrap(MemoryCache())
            val client = clientOver(cache)
            val store  = client.apolloStore
            for
                _        <- store.writeOperation(UserQuery(), userData("1", "Alice"))
                _        <- cache.arm(store.writeOperation(UserQuery(), userData("1", "Bob")).unit)
                tornDown <- Latch.init(1)
                probe    <- Channel.init[ApolloResponse[UserData]](Int.MaxValue)
                // Scope finalizers run last-registered-first: `tornDown` is released only
                // after the watch's own teardown has run.
                drain  <- Fiber.init(Scope.run(Scope.ensure(tornDown.release).andThen(watchUser(client).foreach(probe.put))))
                _      <- probe.take
                second <- probe.take
                _      <- drain.interrupt
                _      <- tornDown.await
                _      <- store.writeOperation(UserQuery(), userData("2", "Carol")) // no root reaches User:1 now
                freed  <- store.garbageCollect
            yield
                assert(second.data == Present(userData("1", "Bob")))
                assert(freed == Set(CacheKey("User", "1")), s"a closed watch still pins its records: $freed")
            end for
        }

        // --- eviction ---------------------------------------------------------------

        "evict removes a single record, publishes its key, and returns it" in {
            var seen = Maybe.empty[Set[CacheKey]]
            for
                s       <- graphStore()
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                evicted <- s.evict(CacheKey("Author", "1"))
                author  <- s.cache.loadRecord(CacheKey("Author", "1"))
                book    <- s.cache.loadRecord(CacheKey("Book", "1"))
            yield
                assert(evicted == Set(CacheKey("Author", "1")))
                assert(seen == Present(Set(CacheKey("Author", "1"))))
                assert(author == Absent)
                assert(book.isDefined) // referrer untouched without cascade
            end for
        }

        "evict with cascade removes the referenced subtree" in {
            for
                s       <- graphStore()
                evicted <- s.evict(CacheKey("Book", "1"), cascade = true)
                book    <- s.cache.loadRecord(CacheKey("Book", "1"))
                author  <- s.cache.loadRecord(CacheKey("Author", "1"))
                root    <- s.cache.loadRecord(CacheKey.QueryRoot)
            yield
                assert(evicted == Set(CacheKey("Book", "1"), CacheKey("Author", "1")))
                assert(book == Absent)
                assert(author == Absent)
                assert(root.isDefined) // the referrer above is not touched
            end for
        }

        "evict of an absent record returns the empty set and publishes nothing" in {
            var published = false
            for
                s       <- graphStore()
                _       <- s.addChangedKeysListener(_ => published = true)
                evicted <- s.evict(CacheKey("Nope", "1"))
            yield
                assert(evicted == Set.empty[CacheKey])
                assert(published == false)
            end for
        }

        // --- MemoryCache: allRecords snapshot ---------------------------------------

        "allRecords returns the whole-store snapshot" in {
            val cache = MemoryCache()
            for
                _   <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1")), rec(keyB, fk("y") -> scalar("2"))))
                all <- cache.allRecords
            yield assert(all.keySet == Set(keyA, keyB))
            end for
        }

        // --- MemoryCache: per-field TTL (maxAge) ------------------------------------

        "a field older than maxAge reads back absent while a fresher field survives" in {
            var now   = 1000L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            for
                _      <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1")))) // x stamped at 1000
                _      <- Sync.defer { now = 1050L }
                _      <- cache.merge(Chunk(rec(keyA, fk("y") -> scalar("2")))) // y stamped at 1050
                _      <- Sync.defer { now = 1120L }                            // x age 120 > 100 (expired); y age 70 < 100 (alive)
                loaded <- cache.loadRecord(keyA)
            yield
                assert(loaded.isDefined)
                assert(loaded.get.fields.keySet == Set(fk("y")))
            end for
        }

        "a record whose every field has expired reads back as a whole miss" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            for
                _       <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1"), fk("y") -> scalar("2"))))
                _       <- Sync.defer { now = 50L }
                alive   <- cache.loadRecord(keyA) // both fields alive
                _       <- Sync.defer { now = 250L }
                expired <- cache.loadRecord(keyA) // both expired → whole-record miss
            yield
                assert(alive.isDefined)
                assert(expired == Absent)
            end for
        }

        "the Date cache header drives the per-field expiry stamp too" in {
            val now   = 5000L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            for
                _      <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1"))), CacheHeaders.of(CacheHeaders.Date -> "1000"))
                loaded <- cache.loadRecord(keyA)
            yield assert(loaded == Absent) // 5000 - 1000 = 4000 > 100
            end for
        }

        "removeExpiredRecords sweeps whole-expired records and reports their keys" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            for
                _       <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1")), rec(keyB, fk("y") -> scalar("2"))))
                _       <- Sync.defer { now = 200L }
                removed <- cache.removeExpiredRecords
                all     <- cache.allRecords
            yield
                assert(removed == Set(keyA, keyB))
                assert(all == Map.empty[CacheKey, Record])
            end for
        }

        "removeExpiredRecords trims expired fields in place without removing the record" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            for
                _       <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1")))) // x stamped at 0
                _       <- Sync.defer { now = 150L }
                _       <- cache.merge(Chunk(rec(keyA, fk("y") -> scalar("2")))) // y stamped at 150
                _       <- Sync.defer { now = 160L }                             // x age 160 > 100 (expired); y age 10 < 100 (alive)
                removed <- cache.removeExpiredRecords
                all     <- cache.allRecords
            yield
                assert(removed == Set.empty[CacheKey]) // A survives, x trimmed
                assert(all(keyA).fields.keySet == Set(fk("y")))
            end for
        }

        "removeExpiredRecords is a no-op when no TTL is configured" in {
            val cache = MemoryCache()
            for
                _       <- cache.merge(Chunk(rec(keyA, fk("x") -> scalar("1"))))
                removed <- cache.removeExpiredRecords
                loaded  <- cache.loadRecord(keyA)
            yield
                assert(removed == Set.empty[CacheKey])
                assert(loaded.isDefined)
            end for
        }

        "with maxAge disabled records round-trip exactly (no field-date metadata)" in {
            val cache  = MemoryCache()
            val record = rec(keyA, fk("x") -> scalar("1"))
            for
                _      <- cache.merge(Chunk(record))
                loaded <- cache.loadRecord(keyA)
            yield assert(loaded == Present(record))
            end for
        }

        // --- NormalizedCacheDecorator persistence seam ------------------------------

        "a bare decorator forwards every operation to its delegate" in {
            // All six members of the backend contract: read (as one batch of two keys),
            // transact (through merge), remove, clearAll, allRecords and sizeLimit.
            val backing = MemoryCache(maxSize = 3)
            val deco    = new NormalizedCacheDecorator(backing) {}
            for
                _       <- deco.merge(Chunk(rec(keyA, fk("x") -> scalar("1")), rec(keyB, fk("y") -> scalar("2"))))
                written <- backing.loadRecord(keyA)
                batch   <- deco.read(_.load(Chunk(keyA, keyB)))
                all     <- deco.allRecords
                removed <- deco.remove(Chunk(keyA))
                gone    <- backing.loadRecord(keyA)
                _       <- deco.clearAll
                cleared <- backing.allRecords
            yield
                assert(written.flatMap(_.get(fk("x"))) == Present(scalar("1"))) // written through
                assert(batch.keySet == Set(keyA, keyB))                         // one batch read through
                assert(batch.get(keyB).map(_.get(fk("y"))) == Some(Present(scalar("2"))))
                assert(all.keySet == Set(keyA, keyB))
                assert(removed == Set(keyA))
                assert(gone == Absent)
                assert(cleared.isEmpty)
                assert(deco.sizeLimit == Present(3))
                assert(new NormalizedCacheDecorator(MemoryCache()) {}.sizeLimit == Absent)
            end for
        }

        "a decorator that overrides transact persists every write a real ApolloStore makes" in {
            // The persistence pattern the decorator exists for, driven through a store
            // rather than by calling the decorator directly: `transact` is the one write
            // primitive, so it is what the store's write reaches.
            val op   = UpdateUserNameMutation("Bob")
            val data = UpdateUserData(User("User", "1", "Bob"))
            class PersistingCache(d: NormalizedCache, persisted: AtomicRef[Set[CacheKey]])
                extends NormalizedCacheDecorator(d):
                override def transact[A](
                    f: RecordLoader => (Chunk[Record], A),
                    cacheHeaders: CacheHeaders,
                    merger: RecordMerger
                )(using Frame): (Set[CacheKey], A) < Sync =
                    super.transact(f, cacheHeaders, merger).map { (changed, a) =>
                        persisted.updateAndGet(_ ++ changed).andThen((changed, a))
                    }
            end PersistingCache
            for
                persisted <- AtomicRef.init(Set.empty[CacheKey])
                store = new ApolloStore(new PersistingCache(MemoryCache(), persisted), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
                _    <- store.writeOperation(op, data)
                keys <- persisted.get
                read <- store.readOperation(op)
            yield
                assert(keys == store.normalize(op, data).keySet, s"the store's write bypassed the decorator: $keys")
                assert(keys == Set(CacheKey.MutationRoot, CacheKey("User", "1")))
                assert(read == data) // reads still forward
            end for
        }

        "a decorator has no second write seam to override: merge is final" in {
            typeCheckFailure("""
                new kyo.apollo.cache.normalized.NormalizedCacheDecorator(kyo.apollo.cache.normalized.MemoryCache()):
                    override def merge(
                        records: kyo.Chunk[kyo.apollo.cache.normalized.api.Record],
                        cacheHeaders: kyo.apollo.cache.normalized.api.CacheHeaders,
                        merger: kyo.apollo.cache.normalized.RecordMerger
                    )(using kyo.Frame): Set[kyo.apollo.cache.normalized.api.CacheKey] < kyo.Sync = Set.empty
            """)("final")
        }
    }
end GarbageCollectionSpec
