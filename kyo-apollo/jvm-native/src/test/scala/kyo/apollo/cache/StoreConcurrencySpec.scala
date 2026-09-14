package kyo.apollo.cache

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** The store under real concurrency: a store operation is parked in the middle of its
  * load by a [[LatchingCache]] while a competing operation runs on another fiber, and
  * the outcome is asserted once both have finished. Each case is an interleaving the
  * store used to get wrong — a read torn by a write (P2-37), two updates of one list
  * losing one (P2-38), a rollback splitting a read between two layer stacks (P2-36) —
  * forced with latches, so it is deterministic.
  */
class StoreConcurrencySpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures ---------------------------------------------------------------

    final case class Album(__typename: String, id: String, title: String) derives Schema
    final case class AlbumData(album: Album) derives Schema

    private val albumSelections: Chunk[CompiledSelection] =
        Chunk(
            CompiledField("__typename", CompiledNamedType("String")),
            CompiledField("id", CompiledNamedType("String")),
            CompiledField("title", CompiledNamedType("String"))
        )

    /** `{ album { __typename id title } }` — the root, then `Album:<id>`. */
    final case class AlbumQuery() extends Query[AlbumData]:
        def name                          = "Album"
        def document                      = "query Album { album { __typename id title } }"
        def dataSchema: Schema[AlbumData] = summon[Schema[AlbumData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(CompiledField("album", CompiledNamedType("Album"), selections = albumSelections))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end AlbumQuery

    final case class TagsData(tags: List[String]) derives Schema

    /** `{ tags }` — one list field on the root record. */
    final case class TagsQuery() extends Query[TagsData]:
        def name                         = "Tags"
        def document                     = "query Tags { tags }"
        def dataSchema: Schema[TagsData] = summon[Schema[TagsData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(CompiledField("tags", CompiledListType(CompiledNamedType("String"))))
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end TagsQuery

    final case class User(__typename: String, id: String, name: String) derives Schema
    final case class UserWithFriend(__typename: String, id: String, name: String, friend: User) derives Schema
    final case class TwoUsersData(first: UserWithFriend) derives Schema

    private val userSelections: Chunk[CompiledSelection] =
        Chunk(
            CompiledField("__typename", CompiledNamedType("String")),
            CompiledField("id", CompiledNamedType("String")),
            CompiledField("name", CompiledNamedType("String"))
        )

    /** `User:1` and its friend `User:2`, one level apart: two batches of one read. */
    final case class TwoUsersQuery() extends Query[TwoUsersData]:
        def name                             = "TwoUsers"
        def document                         = "query TwoUsers { first { __typename id name friend { __typename id name } } }"
        def dataSchema: Schema[TwoUsersData] = summon[Schema[TwoUsersData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(
                    CompiledField(
                        "first",
                        CompiledNamedType("User"),
                        selections = userSelections :+ CompiledField("friend", CompiledNamedType("User"), selections = userSelections)
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end TwoUsersQuery

    private def twoUsers(first: String, second: String): TwoUsersData =
        TwoUsersData(UserWithFriend("User", "1", first, User("User", "2", second)))

    private def album(id: String, title: String): AlbumData = AlbumData(Album("Album", id, title))

    private def storeOver(cache: NormalizedCache): ApolloStore =
        new ApolloStore(cache, cacheKeyGenerator = IdCacheKeyGenerator(List("id")))

    // --- tests ------------------------------------------------------------------

    "the store under concurrent operations" - {

        "a read never mixes two writes (torn read)" in {
            // The reader has loaded QUERY_ROOT (-> Album:1) and is parked before it loads
            // Album:1. Meanwhile a writer re-points the root at Album:2 and removes Album:1.
            // The read must answer from the state it started on: the whole old album — not
            // a miss on a record that existed when it began, and not a mixture.
            for
                gate    <- Latch.init(1)
                reached <- Latch.init(1)
                store = storeOver(LatchingCache(MemoryCache(), CacheKey("Album", "1"), gate, reached))
                _      <- store.writeOperation(AlbumQuery(), album("1", "One"))
                reader <- Fiber.init(store.readOperation(AlbumQuery()))
                _      <- reached.await // the reader is inside its read, between the two batches
                _      <- store.writeOperation(AlbumQuery(), album("2", "Two"))
                _      <- store.cache.remove(Chunk(CacheKey("Album", "1")))
                _      <- gate.release
                read   <- Abort.run[Throwable](reader.get)
                after  <- store.readOperation(AlbumQuery())
            yield
                assert(read == Result.succeed(album("1", "One")), s"the read saw a torn state: $read")
                assert(after == album("2", "Two"), s"the write landed while the read was parked: $after")
            end for
        }

        "two concurrent updateOperation calls both land (no lost update)" in {
            // Both updaters have read the list [1, 2] and are parked before they write.
            // Released together, one commits first; the other's commit fails its
            // compare-and-set, re-reads [1, 2, x] and applies its update again. Read-then-
            // write without a transaction ends with [1, 2, 3] or [1, 2, 4].
            for
                gate    <- Latch.init(1)
                reached <- Latch.init(2)
                store = storeOver(LatchingCache(MemoryCache(), CacheKey.QueryRoot, gate, reached))
                _        <- store.writeOperation(TagsQuery(), TagsData(List("1", "2")))
                applied  <- AtomicInt.init(0)
                three    <- Fiber.init(store.updateOperation(TagsQuery())(d => applyOnce(applied, d, "3")))
                four     <- Fiber.init(store.updateOperation(TagsQuery())(d => applyOnce(applied, d, "4")))
                _        <- reached.await // both are inside their transaction with [1, 2] loaded
                _        <- gate.release
                changedA <- three.get
                changedB <- four.get
                after    <- store.readOperation(TagsQuery())
                attempts <- applied.get
            yield
                assert(after.tags.sorted == List("1", "2", "3", "4"), s"an update was lost: ${after.tags}")
                assert(changedA == Set(CacheKey.QueryRoot) && changedB == Set(CacheKey.QueryRoot))
                assert(attempts == 3, s"the losing update runs again on the newer state, got $attempts applications")
            end for
        }

        "a rollback during a read leaves the read on one layer stack" in {
            // One optimistic layer covers both users. The reader has loaded the batch with
            // User:1 and is parked inside it; the mutation settles and drops the layer; the
            // reader then loads User:2 in its next batch. Both users must come from the
            // same stack.
            // (Writes merge without loading, so only the reader passes the gate.)
            for
                gate    <- Latch.init(1)
                reached <- Latch.init(1)
                store = storeOver(LatchingCache(MemoryCache(), CacheKey("User", "1"), gate, reached))
                _      <- store.writeOperation(TwoUsersQuery(), twoUsers("Alice", "Ann"))
                _      <- store.writeOptimisticUpdates(TwoUsersQuery(), twoUsers("Bob", "Ben"), "m1")
                reader <- Fiber.init(store.readOperation(TwoUsersQuery()))
                _      <- reached.await // parked inside the batch that loads User:1
                _      <- store.rollbackOptimisticUpdates("m1")
                _      <- gate.release
                read   <- reader.get
                layers <- store.optimisticLayerIds
                next   <- store.readOperation(TwoUsersQuery())
            yield
                assert(
                    read == twoUsers("Bob", "Ben") || read == twoUsers("Alice", "Ann"),
                    s"the read mixed two layer stacks: $read"
                )
                assert(layers.isEmpty)
                assert(next == twoUsers("Alice", "Ann"))
            end for
        }
    }

    /** Apply `update` for `tag`, counting how often an update function ran. */
    private def applyOnce(applied: AtomicInt, data: TagsData, tag: String): TagsData =
        discard(applied.unsafe.incrementAndGet()(using AllowUnsafe.embrace.danger))
        data.copy(tags = data.tags :+ tag)
end StoreConcurrencySpec
