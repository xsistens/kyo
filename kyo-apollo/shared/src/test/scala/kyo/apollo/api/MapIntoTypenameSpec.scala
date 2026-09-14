package kyo.apollo.api

import kyo.Chunk
import kyo.Maybe
import kyo.Result
import kyo.Schema
import kyo.apollo.cache.normalized.ApolloStore
import kyo.apollo.cache.normalized.MemoryCache
import kyo.apollo.cache.normalized.api.IdCacheKeyGenerator
import kyo.apollo.json.JsonParser
import kyo.apollo.json.SchemaJson

/** Guards the `mapInto` decode contract against the implicit `__typename`.
  *
  * Every object selection requests `__typename` (for cache keying), so the JSON
  * an object node hands its child codec ALWAYS carries a `__typename` member the
  * projected case class does not declare. `.mapInto[B]` decodes that JSON via
  * `Schema[B]` — so kyo-schema's decode must tolerate (skip) unknown fields, or
  * every mapInto projection would fail at runtime on the first live response.
  *
  * A `mapInto` projection also encodes, so it nests into a parent field and its
  * operation is one the normalized cache can write.
  */
class MapIntoTypenameSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private case class CountryDto(code: String, name: String, capital: Option[String]) derives Schema

    sealed private trait CountryT

    private object GCountry:
        def code: SelectionBuilder.Deferrable[CountryT, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("String").notNull, ScalarCodec.string)
        def name: SelectionBuilder.Deferrable[CountryT, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)
        def capital: SelectionBuilder.Deferrable[CountryT, (capital: Maybe[String])] =
            SelectionBuilder.scalar("capital", CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string))
    end GCountry

    private def countries[A](sel: SelectionBuilder.Bidirectional[CountryT, A])
        : SelectionBuilder.Deferrable[RootQuery, (countries: Chunk[A])] =
        SelectionBuilder.obj(
            "countries",
            CompiledNamedType("Country").notNull.list.notNull,
            Chunk.empty,
            sel,
            SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
        )

    "mapInto-style Schema decode" - {

        "tolerates the implicit __typename member" in {
            val json = JsonParser.parse(
                """{"__typename":"Country","code":"DE","name":"Germany","capital":"Berlin"}"""
            ).getOrThrow
            assert(SchemaJson.decode[CountryDto](json) == Result.succeed(CountryDto("DE", "Germany", Some("Berlin"))))
        }

        "tolerates __typename in nested position" in {
            case class Wrap(country: CountryDto) derives Schema
            val json = JsonParser.parse(
                """{"country":{"__typename":"Country","code":"FR","name":"France","capital":null}}"""
            ).getOrThrow
            assert(SchemaJson.decode[Wrap](json) == Result.succeed(Wrap(CountryDto("FR", "France", None))))
        }
    }

    "a mapInto projection is normalizable" - {

        "its operation round-trips through the normalized store, an absent nullable field included" in {
            val q: Query.Normalizable[(countries: Chunk[CountryDto])] =
                countries((GCountry.code ~ GCountry.name ~ GCountry.capital).mapInto[CountryDto]).toQuery("Countries")
            val store = new ApolloStore(MemoryCache(), cacheKeyGenerator = IdCacheKeyGenerator(List("code")))
            val data  = (countries = Chunk(CountryDto("DE", "Germany", Some("Berlin")), CountryDto("FR", "France", None)))
            for
                _    <- store.writeOperation(q, data)
                back <- store.readOperation(q)
            yield assert(back == data)
            end for
        }
    }
end MapIntoTypenameSpec
