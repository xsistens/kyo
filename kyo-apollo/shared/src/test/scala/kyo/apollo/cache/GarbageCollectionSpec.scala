package kyo.apollo.cache

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap

/** Garbage collection, eviction, and TTL/expiration.
  *
  * Three layers of coverage:
  *   - **Reachability GC** on [[ApolloStore]] — `garbageCollect` /
  *     `removeUnreachableRecords` sweeping records unreachable from the operation
  *     roots (following references, including through lists), while an optimistic
  *     layer *pins* records it points at.
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

        "garbageCollect does not publish (unreachable records have no watchers)" in {
            var published = false
            for
                s <- graphStore()
                _ <- s.addChangedKeysListener(_ => published = true)
                _ <- s.garbageCollect
            yield assert(published == false)
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
            val backing = MemoryCache()
            val deco    = new NormalizedCacheDecorator(backing) {}
            for
                _        <- deco.merge(Chunk(rec(keyA, fk("x") -> scalar("1"))))
                written  <- backing.loadRecord(keyA)
                readBack <- deco.loadRecord(keyA)
                all      <- deco.allRecords
                removed  <- deco.remove(Chunk(keyA))
                gone     <- backing.loadRecord(keyA)
            yield
                assert(written.flatMap(_.get(fk("x"))) == Present(scalar("1")))  // written through
                assert(readBack.flatMap(_.get(fk("x"))) == Present(scalar("1"))) // read through
                assert(all.keySet == Set(keyA))
                assert(removed == Set(keyA))
                assert(gone == Absent)
            end for
        }

        "a decorator that overrides transact sees every write, including the store's" in {
            val backing   = MemoryCache()
            var persisted = Chunk.empty[CacheKey]
            class PersistingCache(d: NormalizedCache) extends NormalizedCacheDecorator(d):
                override def transact[A](
                    f: RecordLoader => (Chunk[Record], A),
                    cacheHeaders: CacheHeaders,
                    merger: RecordMerger
                )(using Frame): (Set[CacheKey], A) < Sync =
                    super.transact(f, cacheHeaders, merger).map { (changed, a) =>
                        persisted = persisted ++ Chunk.from(changed)
                        (changed, a)
                    }
            end PersistingCache
            val deco  = new PersistingCache(backing)
            val store = new ApolloStore(deco, cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            for
                _      <- deco.merge(Chunk(rec(keyA, fk("x") -> scalar("1"))))
                _      <- store.writeOptimisticUpdates(UpdateUserNameMutation("Bob"), UpdateUserData(User("User", "1", "Bob")), "m1")
                _      <- store.rollbackAndWrite(UpdateUserNameMutation("Bob"), UpdateUserData(User("User", "1", "Bob")), "m1")
                loaded <- deco.loadRecord(keyA)
            yield
                assert(persisted.toSet == Set(keyA, CacheKey.MutationRoot, CacheKey("User", "1"))) // intercepted for persistence
                assert(loaded.isDefined)                                                           // inherited forwarding still works
            end for
        }
    }
end GarbageCollectionSpec
