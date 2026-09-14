package kyo.apollo.cache

import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import scala.collection.immutable.VectorMap

/** Unit tests for [[ApolloStore.extract]] — the normalized-cache dump in Apollo
  * Client's `InMemoryCache.extract()` JSON shape, which the Apollo Client Devtools
  * "Cache" tab reads via `client.cache.extract(true)`: top-level `ROOT_QUERY` /
  * `Type:id` keys, `{"__ref": …}` links, `__typename` on entities, lists as JSON
  * arrays, and the optimistic overlay honored only when requested.
  */
class ExtractSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- fixtures ---------------------------------------------------------------

    final case class Country(__typename: String, code: String, name: String) derives Schema
    final case class CountriesData(countries: List[Country]) derives Schema

    final case class CountriesQuery() extends Query[CountriesData]:
        def name                              = "Countries"
        def document                          = "query Countries { countries { __typename code name } }"
        def dataSchema: Schema[CountriesData] = summon[Schema[CountriesData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Query"),
                selections = Chunk(
                    CompiledField(
                        "countries",
                        CompiledListType(CompiledNamedType("Country")),
                        selections = Chunk(
                            CompiledField("__typename", CompiledNamedType("String")),
                            CompiledField("code", CompiledNamedType("String")),
                            CompiledField("name", CompiledNamedType("String"))
                        )
                    )
                )
            )
        def variables: Json = Json.JObj(VectorMap.empty)
    end CountriesQuery

    final case class User(__typename: String, id: String, name: String) derives Schema
    final case class UserData(user: User) derives Schema

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

    final case class CurrentUserQuery() extends Query[UserData]:
        def name                         = "CurrentUser"
        def document                     = "query CurrentUser { user { __typename id name } }"
        def dataSchema: Schema[UserData] = summon[Schema[UserData]]
        def rootField: CompiledField =
            CompiledField("data", CompiledNamedType("Query"), selections = Chunk(userField("user")))
        def variables: Json = Json.JObj(VectorMap.empty)
    end CurrentUserQuery

    final case class UpdateUserData(updateUser: User) derives Schema

    final case class RenameMutation(newName: String) extends Mutation[UpdateUserData]:
        def name = "Rename"
        def document =
            "mutation Rename($name: String!) { updateUser(name: $name) { __typename id name } }"
        def dataSchema: Schema[UpdateUserData] = summon[Schema[UpdateUserData]]
        def rootField: CompiledField =
            CompiledField(
                "data",
                CompiledNamedType("Mutation"),
                selections = Chunk(userField("updateUser"))
            )
        def variables: Json = Json.JObj(VectorMap("name" -> SchemaJson.encode(newName)))
    end RenameMutation

    private def countryStore()(using Frame): ApolloStore < Sync =
        val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("code")))
        s.writeOperation(
            CountriesQuery(),
            CountriesData(List(Country("Country", "DE", "Germany"), Country("Country", "FR", "France")))
        ).andThen(s)
    end countryStore

    /** The persisted (no optimistic overlay) dump of [[countryStore]]'s records. */
    private def countryFields()(using Frame): Map[String, Json] < Sync =
        countryStore().map(_.extract(false)).map {
            case Json.JObj(fields) => fields
            case other             => throw new AssertionError(s"expected JObj, got $other")
        }

    "ApolloStore.extract" - {

        "empty store extracts to an empty JSON object (does not throw)" in {
            val s = new ApolloStore(MemoryCache())
            s.extract().map(json => assert(json == Json.JObj(Map.empty)))
        }

        "extract contains a top-level ROOT_QUERY key (translated from QUERY_ROOT)" in {
            countryFields().map(fields => assert(fields.contains("ROOT_QUERY")))
        }

        "entity records appear under Type:id keys" in {
            countryFields().map(fields => assert(fields.contains("Country:DE") && fields.contains("Country:FR")))
        }

        "references serialize as {\"__ref\": key} and a list as a JSON array of refs" in {
            countryStore().map(_.extract(false)).map { json =>
                val rendered = json.render
                assert(rendered.contains("\"__ref\":\"Country:DE\""))
                assert(rendered.contains("\"__ref\":\"Country:FR\""))
                // a list field renders as an array whose elements are ref objects
                assert(rendered.contains("[{\"__ref\":"))
            }
        }

        "every entity object carries __typename" in {
            countryFields().map { fields =>
                fields("Country:DE") match
                    case Json.JObj(f) => assert(f.get("__typename") == Some(Json.JStr("Country")))
                    case other        => assert(false, s"expected JObj, got $other")
            }
        }

        "includeOptimistic=true overlays pending layers; false omits them" in {
            val s = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("id")))
            for
                _ <- s.writeOperation(CurrentUserQuery(), UserData(User("User", "1", "Alice")))
                _ <- s.writeOptimisticUpdates(
                    RenameMutation("Bob"),
                    UpdateUserData(User("User", "1", "Bob")),
                    "m1"
                )
                withLayers    <- s.extract(true)
                withoutLayers <- s.extract(false)
            yield
                assert(withLayers.render.contains("\"name\":\"Bob\""))
                assert(!withoutLayers.render.contains("\"name\":\"Bob\""))
                assert(withoutLayers.render.contains("\"name\":\"Alice\""))
            end for
        }
    }
end ExtractSpec
