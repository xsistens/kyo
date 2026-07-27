package kyo.apollo.cache

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.Schema
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ListBuffer

/** Phase 07 Task 4: garbage collection, eviction, and TTL/expiration.
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

    private def scalar(s: String): RecordValue                           = RecordValue.Scalar(Json.JStr(s))
    private def ref(key: String): RecordValue                            = RecordValue.Reference(CacheReference(key))
    private def rec(key: String, fields: (String, RecordValue)*): Record = Record(key, fields.toMap)

    // --- reachability GC --------------------------------------------------------

    private def graphStore(): ApolloStore =
        val s = new ApolloStore(MemoryCache())
        s.cache.merge(
            List(
                rec("QUERY_ROOT", "book" -> ref("Book:1")),
                rec("Book:1", "title"    -> scalar("Dune"), "author" -> ref("Author:1")),
                rec("Author:1", "name"   -> scalar("Herbert")),
                rec("Orphan:1", "x"      -> scalar("1"))
            )
        )
        s
    end graphStore

    // --- optimistic pinning -----------------------------------------------------

    final case class User(__typename: String, id: String, name: String) derives Schema

    private def userField(field: String): CompiledField =
        CompiledField(
            field,
            CompiledNamedType("User"),
            selections = List(
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
                selections = List(userField("updateUser"))
            )
        def variables: Json = Json.JObj(VectorMap("name" -> SchemaJson.encode(newName)))
    end UpdateUserNameMutation

    "garbageCollect / evict / MemoryCache TTL / NormalizedCacheDecorator" - {

        "garbageCollect removes records unreachable from a root, transitively keeping the rest" in {
            val s = graphStore()
            assert(s.garbageCollect() == Set("Orphan:1"))
            assert(s.cache.loadRecord("QUERY_ROOT").isDefined)
            assert(s.cache.loadRecord("Book:1").isDefined)
            assert(s.cache.loadRecord("Author:1").isDefined) // reachable QUERY_ROOT → Book:1 → Author:1
            assert(s.cache.loadRecord("Orphan:1") == Absent)
        }

        "garbageCollect keeps records reachable only through a list reference" in {
            val s = new ApolloStore(MemoryCache())
            s.cache.merge(
                List(
                    rec("QUERY_ROOT", "books" -> RecordValue.RList(Chunk(ref("Book:1"), ref("Book:2")))),
                    rec("Book:1", "title"     -> scalar("A")),
                    rec("Book:2", "title"     -> scalar("B")),
                    rec("Orphan:1", "x"       -> scalar("1"))
                )
            )
            assert(s.garbageCollect() == Set("Orphan:1"))
            assert(s.cache.loadRecord("Book:1").isDefined)
            assert(s.cache.loadRecord("Book:2").isDefined)
        }

        "garbageCollect on an empty store removes nothing" in {
            val s = new ApolloStore(MemoryCache())
            assert(s.garbageCollect() == Set.empty[String])
        }

        "garbageCollect does not publish (unreachable records have no watchers)" in {
            val s         = graphStore()
            var published = false
            s.addChangedKeysListener(_ => published = true)
            s.garbageCollect()
            assert(published == false)
        }

        "removeUnreachableRecords is the sweep helper garbageCollect delegates to" in {
            val s = graphStore()
            assert(s.removeUnreachableRecords() == Set("Orphan:1"))
            // A second sweep is now a no-op — everything left is reachable.
            assert(s.removeUnreachableRecords() == Set.empty[String])
        }

        "an optimistic layer pins an otherwise-unreachable record from GC, releasing it on rollback" in {
            val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            // A persisted record reachable from no root — a GC candidate on its own.
            s.cache.merge(List(rec("User:1", "name" -> scalar("Alice"))))
            // An optimistic mutation references User:1, so GC must keep it alive.
            s.writeOptimisticUpdates(
                UpdateUserNameMutation("Bob"),
                UpdateUserData(User("User", "1", "Bob")),
                "m1"
            )
            assert(s.garbageCollect() == Set.empty[String])
            assert(s.cache.loadRecord("User:1").isDefined)
            // Once the optimistic layer rolls back, the record is unreachable again.
            s.rollbackOptimisticUpdates("m1")
            assert(s.garbageCollect() == Set("User:1"))
        }

        // --- eviction ---------------------------------------------------------------

        "evict removes a single record, publishes its key, and returns it" in {
            val s    = graphStore()
            var seen = Option.empty[Set[String]]
            s.addChangedKeysListener(keys => seen = Some(keys))
            assert(s.evict(CacheKey("Author:1")) == Set("Author:1"))
            assert(seen == Some(Set("Author:1")))
            assert(s.cache.loadRecord("Author:1") == Absent)
            assert(s.cache.loadRecord("Book:1").isDefined) // referrer untouched without cascade
        }

        "evict with cascade removes the referenced subtree" in {
            val s = graphStore()
            assert(s.evict(CacheKey("Book:1"), cascade = true) == Set("Book:1", "Author:1"))
            assert(s.cache.loadRecord("Book:1") == Absent)
            assert(s.cache.loadRecord("Author:1") == Absent)
            assert(s.cache.loadRecord("QUERY_ROOT").isDefined) // the referrer above is not touched
        }

        "evict of an absent record returns the empty set and publishes nothing" in {
            val s         = graphStore()
            var published = false
            s.addChangedKeysListener(_ => published = true)
            assert(s.evict(CacheKey("Nope:1")) == Set.empty[String])
            assert(published == false)
        }

        // --- MemoryCache: allRecords snapshot ---------------------------------------

        "allRecords returns the whole-store snapshot" in {
            val cache = MemoryCache()
            cache.merge(List(rec("A", "x" -> scalar("1")), rec("B", "y" -> scalar("2"))))
            assert(cache.allRecords().keySet == Set("A", "B"))
        }

        // --- MemoryCache: per-field TTL (maxAge) ------------------------------------

        "a field older than maxAge reads back absent while a fresher field survives" in {
            var now   = 1000L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            cache.merge(List(rec("A", "x" -> scalar("1")))) // x stamped at 1000
            now = 1050L
            cache.merge(List(rec("A", "y" -> scalar("2")))) // y stamped at 1050
            now = 1120L // x age 120 > 100 (expired); y age 70 < 100 (alive)
            val loaded = cache.loadRecord("A")
            assert(loaded.isDefined)
            assert(loaded.get.fields.keySet == Set("y"))
        }

        "a record whose every field has expired reads back as a whole miss" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            cache.merge(List(rec("A", "x" -> scalar("1"), "y" -> scalar("2"))))
            now = 50L
            assert(cache.loadRecord("A").isDefined) // both fields alive
            now = 250L
            assert(cache.loadRecord("A") == Absent) // both expired → whole-record miss
        }

        "the Date cache header drives the per-field expiry stamp too" in {
            val now   = 5000L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            cache.merge(List(rec("A", "x" -> scalar("1"))), CacheHeaders.of(CacheHeaders.Date -> "1000"))
            assert(cache.loadRecord("A") == Absent) // 5000 - 1000 = 4000 > 100
        }

        "removeExpiredRecords sweeps whole-expired records and reports their keys" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            cache.merge(List(rec("A", "x" -> scalar("1")), rec("B", "y" -> scalar("2"))))
            now = 200L
            assert(cache.removeExpiredRecords() == Set("A", "B"))
            assert(cache.allRecords() == Map.empty[String, Record])
        }

        "removeExpiredRecords trims expired fields in place without removing the record" in {
            var now   = 0L
            val cache = MemoryCache(maxAge = 100L, nowMillis = () => now)
            cache.merge(List(rec("A", "x" -> scalar("1")))) // x stamped at 0
            now = 150L
            cache.merge(List(rec("A", "y" -> scalar("2")))) // y stamped at 150
            now = 160L                                                // x age 160 > 100 (expired); y age 10 < 100 (alive)
            assert(cache.removeExpiredRecords() == Set.empty[String]) // A survives, x trimmed
            assert(cache.allRecords()("A").fields.keySet == Set("y"))
        }

        "removeExpiredRecords is a no-op when no TTL is configured" in {
            val cache = MemoryCache()
            cache.merge(List(rec("A", "x" -> scalar("1"))))
            assert(cache.removeExpiredRecords() == Set.empty[String])
            assert(cache.loadRecord("A").isDefined)
        }

        "with maxAge disabled records round-trip exactly (no field-date metadata)" in {
            val cache  = MemoryCache()
            val record = rec("A", "x" -> scalar("1"))
            cache.merge(List(record))
            assert(cache.loadRecord("A") == Present(record))
        }

        // --- NormalizedCacheDecorator persistence seam ------------------------------

        "a bare decorator forwards every operation to its delegate" in {
            val backing = MemoryCache()
            val deco    = new NormalizedCacheDecorator(backing) {}
            deco.merge(List(rec("A", "x" -> scalar("1"))))
            assert(backing.loadRecord("A").flatMap(_.get("x")) == Present(scalar("1"))) // written through
            assert(deco.loadRecord("A").flatMap(_.get("x")) == Present(scalar("1")))    // read through
            assert(deco.allRecords().keySet == Set("A"))
            assert(deco.remove("A") == true)
            assert(backing.loadRecord("A") == Absent)
        }

        "a decorator subclass can intercept writes while inheriting the rest" in {
            val backing   = MemoryCache()
            val persisted = ListBuffer.empty[String]
            class PersistingCache(d: NormalizedCache) extends NormalizedCacheDecorator(d):
                override def merge(records: Iterable[Record], headers: CacheHeaders): Set[String] =
                    records.foreach(r => persisted += r.key)
                    super.merge(records, headers)
            end PersistingCache
            val deco = new PersistingCache(backing)
            deco.merge(List(rec("A", "x" -> scalar("1"))))
            assert(persisted.toList == List("A"))          // intercepted for persistence
            assert(deco.loadRecord("A").isDefined == true) // inherited forwarding still works
        }
    }
end GarbageCollectionSpec
