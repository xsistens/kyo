package kyo.apollo.api

import kyo.Result
import kyo.Schema
import kyo.apollo.json.JsonParser
import kyo.apollo.json.SchemaJson

/** Guards the `mapInto` decode contract against the implicit `__typename`.
  *
  * Every object selection requests `__typename` (for cache keying), so the JSON
  * an object node hands its child codec ALWAYS carries a `__typename` member the
  * projected case class does not declare. `.mapInto[B]` decodes that JSON via
  * `Schema[B]` — so kyo-schema's decode must tolerate (skip) unknown fields, or
  * every mapInto projection would fail at runtime on the first live response.
  */
class MapIntoTypenameSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private case class CountryDto(code: String, name: String, capital: Option[String]) derives Schema

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
end MapIntoTypenameSpec
