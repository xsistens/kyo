package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Empty

/** Spike / Go-No-Go gate for the inline `SelectionBuilder` design.
  *
  * The hand-written selector layer below (phantom `Origin` markers + `Country` /
  * `Queries` objects) mimics exactly what the schema-driven generator will emit,
  * so this proves the whole approach end to end *before* the generator exists:
  *   - named-tuple `~` concatenation types correctly (`(name: …) ~ (capital: …)`),
  *   - a nested optional object selection composes and decodes,
  *   - the structural codec round-trips response JSON ⇄ named tuple,
  *   - and — critically — all of it LINKS and RUNS under Scala.js / kyo RC5,
  *     because nothing on this path uses `derives` (the sum-type derivation that
  *     breaks the linker at RC5).
  */
class SelectionBuilderSpikeSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // --- Hand-written "generated" selector layer -------------------------------

    sealed trait RootQuery
    sealed trait Country

    sealed trait Continent

    object Country:
        given TypeName[Country] = TypeName("Country")

        def select: SelectionBuilder.Fields[Country, Empty] = SelectionBuilder.empty[Country]

        def code: SelectionBuilder.Deferrable[Country, (code: String)] =
            SelectionBuilder.scalar("code", CompiledNamedType("ID").notNull, ScalarCodec.id)

        def name: SelectionBuilder.Deferrable[Country, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

        def capital: SelectionBuilder.Deferrable[Country, (capital: Maybe[String])] =
            SelectionBuilder.scalar(
                "capital",
                CompiledNamedType("String"),
                ScalarCodec.maybe(ScalarCodec.string)
            )
    end Country

    object Continent:
        def countries[A <: AnyNamedTuple](
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder.Deferrable[Continent, (countries: Chunk[A])] =
            SelectionBuilder.obj(
                "countries",
                CompiledNamedType("Country").notNull.list.notNull,
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
            )
    end Continent

    object CountryName:
        val fields = Fragment.embedded[Country](_ ~ Country.name)

    object Queries:
        def country[A <: AnyNamedTuple](code: String)(
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder.Deferrable[RootQuery, (country: Maybe[A])] =
            SelectionBuilder.obj(
                "country",
                CompiledNamedType("Country"),
                Chunk(
                    SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, ScalarCodec.id.encode(code))
                ),
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

    /** The same `country` root field on the library's own [[kyo.apollo.api.RootQuery]], which the
      * terminal `toQuery` extensions key on.
      */
    object ApiQueries:
        def country[A <: AnyNamedTuple](
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder.Deferrable[kyo.apollo.api.RootQuery, (country: Maybe[A])] =
            SelectionBuilder.obj(
                "country",
                CompiledNamedType("Country"),
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end ApiQueries

    // --- Tests -----------------------------------------------------------------

    "SelectionBuilder spike" - {

        "named-tuple ~ concat + structural decode of a nested optional object" in {
            val sel = Queries.country("DE")(Country.name ~ Country.capital)

            val response = Json.JObj(
                Map(
                    "country" -> Json.JObj(
                        Map(
                            "name"    -> Json.JStr("Germany"),
                            "capital" -> Json.JStr("Berlin")
                        )
                    )
                )
            )

            val result = sel.decode(response)
            // Static type here IS `(country: Maybe[(name: String, capital: Maybe[String])])`.
            assert(result.country.map(_.name) == Present("Germany"))
            assert(result.country.flatMap(_.capital) == Present("Berlin"))
        }

        "null object → Absent; null scalar within a present object → Absent" in {
            val sel = Queries.country("XX")(Country.name ~ Country.capital)

            val missing = Json.JObj(Map("country" -> Json.JNull))
            assert(sel.decode(missing).country == Absent)

            val present = Json.JObj(
                Map(
                    "country" -> Json.JObj(Map("name" -> Json.JStr("Narnia"), "capital" -> Json.JNull))
                )
            )
            val result = sel.decode(present)
            assert(result.country.map(_.name) == Present("Narnia"))
            assert(result.country.flatMap(_.capital) == Absent)
        }

        "structural encode round-trips back to the response object shape (with __typename)" in {
            val sel = Queries.country("DE")(Country.name ~ Country.capital)
            // encode re-emits the auto-injected __typename, so the round-trip target
            // carries it too.
            val response = Json.JObj(
                Map(
                    "country" -> Json.JObj(
                        Map(
                            "__typename" -> Json.JStr("Country"),
                            "name"       -> Json.JStr("Germany"),
                            "capital"    -> Json.JStr("Berlin")
                        )
                    )
                )
            )
            assert(sel.encode(sel.decode(response)) == response)
        }

        "selection tree binds the argument and auto-injects __typename" in {
            val sel     = Queries.country("DE")(Country.name ~ Country.capital)
            val country = sel.selections.collect { case f: CompiledField => f }.head
            assert(country.name == "country")
            assert(country.arguments == Chunk(CompiledArgument.variable("code")))
            assert(
                country.selections.collect { case f: CompiledField => f.name } ==
                    Chunk("__typename", "name", "capital")
            )
        }

        "the result type speaks kyo: a nullable field is Maybe, never Option" in {
            val json = Json.JObj(Map("capital" -> Json.JStr("Berlin")))
            // Positive: the decoded slot ascribes to `Maybe[String]` ...
            val capital: Maybe[String] = Country.capital.decode(json).capital
            assert(capital == Present("Berlin"))
            // ... and the stdlib type is a compile error, not a silent runtime cast.
            typeCheckFailure(
                "val c: Option[String] = Country.capital.decode(Json.JObj(Map.empty)).capital"
            )("Required: Option[String]")
            typeCheckFailure(
                "val o: Option[(name: String)] = Queries.country(\"DE\")(Country.name).decode(Json.JObj(Map.empty)).country"
            )("Required: Option[(name : String)]")
        }

        "capabilities are types, not runtime checks" - {

            "deferring needs a selected field: the empty selection has no .deferred" in {
                typeCheckFailure("Country.select.deferred")("value deferred is not a member of")
            }

            "a @defer group is not a list field: it has no .streamed" in {
                typeCheckFailure("defer(\"x\", Country.code).streamed(2)")("value streamed is not a member of")
            }

            "a fragment spread is not a list field: it has no .streamed" in {
                typeCheckFailure("CountryName.fields.spread.streamed(2)")("value streamed is not a member of")
            }

            "@stream applies to a list field only" in {
                typeCheckFailure("Country.code.streamed(2)")(
                    "`.streamed` applies to a list field, but the last field of (code : String) is not a list"
                )
            }

            "a .map projection does not combine" in {
                typeCheckFailure("Country.code.map(identity) ~ Country.name")("value ~ is not a member of")
            }

            "what a field selection can do still compiles, and keeps its runtime shape" in {
                typeCheck("Country.select ~ Country.code ~ Country.capital.deferred")
                typeCheck("(Country.code ~ Country.name).deferred ~ Country.capital.deferred")
                typeCheck("Continent.countries(Country.name).streamed(2).deferred")
                typeCheck("defer(\"details\", Country.name ~ Country.capital) ~ Country.code")
                val deferred  = (Country.select ~ Country.code ~ Country.capital).deferred
                val fragments = deferred.selections.collect { case f: CompiledFragment => f }
                assert(fragments.map(_.defer.map(_.label)) == Chunk(Present("capital")))
                val result: (code: String, capital: Maybe[Maybe[String]]) =
                    deferred.decode(Json.JObj(Map("code" -> Json.JStr("DE"))))
                assert(result.capital == Absent)
            }
        }
    }
end SelectionBuilderSpikeSpec
