package kyo.apollo.cache

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.TestKeys.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Phase 05 Task 6: direct fragment/cache access. A [[Fragment]] reads from and
  * writes to a *specific* [[CacheKey]] without a full operation, reusing the same
  * `Normalizer` (write) and `CacheBatchReader` (read) machinery as an operation —
  * only rooted at the caller's key. These tests cover the round-trip, the
  * dependency-key capture, the change notification `writeFragment` publishes, and
  * the cross-path sharing that lets an imperative fragment write update the very
  * record a full query reads (the seam Task 8's watcher test builds on).
  */
class FragmentSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures ---------------------------------------------------------------

    // The `__typename` field is named verbatim so kyo-schema encodes it to the
    // `__typename` response key the Normalizer/CacheBatchReader match on.
    final case class UserFields(__typename: String, id: String, name: String) derives Schema

    /** A `... on User { __typename id name }` fragment, rooted at a `User` record. */
    object UserFragment extends Fragment[UserFields]:
        def dataSchema: Schema[UserFields] = summon[Schema[UserFields]]
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

    // A full query reading the same `User:1` record by reference, to prove a
    // fragment write and an operation read share one record.
    final case class UserData(user: UserFields) derives Schema

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(UserFragment.rootField)
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    private def store(): ApolloStore =
        new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))

    private val ada     = UserFields("User", "1", "Ada")
    private val userKey = CacheKey("User", "1")

    // A user holding an id-less child, reachable both as a fragment root and through a
    // query. The two write paths root their normalizer differently — the fragment at the
    // entity key, the operation at QUERY_ROOT — so before the path was rerooted at every
    // keyed object they disagreed about where that child lives.
    final case class Avatar(url: String) derives Schema
    final case class ProfileFields(__typename: String, id: String, avatar: Avatar) derives Schema
    final case class ProfileData(user: ProfileFields) derives Schema

    private def avatarSelections: Chunk[CompiledSelection] =
        Chunk(
            CompiledField("__typename", CompiledNamedType("String")),
            CompiledField("id", CompiledNamedType("String")),
            CompiledField(
                "avatar",
                CompiledNamedType("Avatar"),
                selections = Chunk(CompiledField("url", CompiledNamedType("String")))
            )
        )

    object ProfileFragment extends Fragment[ProfileFields]:
        def dataSchema: Schema[ProfileFields] = summon[Schema[ProfileFields]]
        def rootField: CompiledField =
            CompiledField("user", CompiledNamedType("User"), selections = avatarSelections)
    end ProfileFragment

    final case class ProfileQuery() extends Query[ProfileData]:
        def name                            = "Profile"
        def document                        = "query Profile { user { __typename id avatar { url } } }"
        def dataSchema: Schema[ProfileData] = summon[Schema[ProfileData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(ProfileFragment.rootField)
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end ProfileQuery

    "Fragment cache access" - {

        // --- tests ------------------------------------------------------------------

        "writeFragment then readFragment returns typed data equal to the original" in {
            val s = store()
            for
                _    <- s.writeFragment(UserFragment, userKey, ada)
                read <- s.readFragment(UserFragment, userKey)
            yield assert(read == ada)
            end for
        }

        "writeFragment normalizes into the CacheKey's record and reports it changed" in {
            val s = store()
            for
                changed <- s.writeFragment(UserFragment, userKey, ada)
                record  <- s.cache.loadRecord(CacheKey("User", "1"))
            yield
                assert(changed == Set(CacheKey("User", "1")))
                assert(record.flatMap(_.get(fk("name"))) == Present(RecordValue.Scalar(kyo.apollo.json.Json.JStr("Ada"))))
            end for
        }

        "writeFragment publishes its changed keys to a registered listener" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _ <- s.addChangedKeysListener(keys => seen = Present(keys))
                _ <- s.writeFragment(UserFragment, userKey, ada)
            yield assert(seen == Present(Set(CacheKey("User", "1"))))
            end for
        }

        "readFragmentWithKeys reports the CacheKey as the dependent key" in {
            val s = store()
            for
                _            <- s.writeFragment(UserFragment, userKey, ada)
                (data, keys) <- s.readFragmentWithKeys(UserFragment, userKey)
            yield
                assert(data == ada)
                assert(keys == Set(CacheKey("User", "1")))
            end for
        }

        "readFragment on an empty store raises CacheMissException" in {
            val s = store()
            Abort.run[CacheMissException](s.readFragment(UserFragment, userKey)).map(result => assert(result.isFailure))
        }

        "a fragment write updates the very record a full operation reads" in {
            val s = store()
            for
                // Seed User:1 = Ada through a full query write.
                _      <- s.writeOperation(CurrentUserQuery(), UserData(ada))
                before <- s.readOperation(CurrentUserQuery())
                // Imperatively update only the name via the fragment on User:1.
                changed <- s.writeFragment(UserFragment, userKey, ada.copy(name = "Bob"))
                // The full operation read now reflects the fragment's change.
                after <- s.readOperation(CurrentUserQuery())
            yield
                assert(before.user.name == "Ada")
                assert(changed.contains(CacheKey("User", "1")))
                assert(after.user.name == "Bob")
            end for
        }

        "writeFragment and writeOperation key an entity's id-less child identically" in {
            // Found while closing GAPS.md F-18, and closed by the same line. `writeFragment`
            // roots its normalizer at the entity key and `writeOperation` at QUERY_ROOT, so
            // while the path was carried down verbatim the same avatar landed under
            // `User:1.avatar` from one writer and `QUERY_ROOT.user.avatar` from the other —
            // two records for one object, and whichever wrote last owned the pointer.
            val s       = store()
            val profile = ProfileFields("User", "1", Avatar("u"))
            for
                _    <- s.writeOperation(ProfileQuery(), ProfileData(profile))
                _    <- s.writeFragment(ProfileFragment, userKey, profile)
                all  <- s.cache.allRecords
                read <- s.readOperation(ProfileQuery())
            yield
                assert(all.keySet.filter(_.render.contains("avatar")) == Set(pathKey("User:1", "avatar")))
                assert(read == ProfileData(profile))
            end for
        }

        "a fragment write of identical data reports no changed keys and publishes nothing" in {
            val s    = store()
            var seen = Maybe.empty[Set[CacheKey]]
            for
                _       <- s.writeFragment(UserFragment, userKey, ada)
                _       <- s.addChangedKeysListener(keys => seen = Present(keys))
                changed <- s.writeFragment(UserFragment, userKey, ada)
            yield
                assert(changed == Set.empty[String])
                assert(seen == Absent)
            end for
        }
    }
end FragmentSpec
