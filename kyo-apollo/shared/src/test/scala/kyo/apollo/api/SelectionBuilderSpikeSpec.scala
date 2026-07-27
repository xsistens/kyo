package kyo.apollo.api

import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple

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

    object Country:
        def name: SelectionBuilder[Country, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

        def capital: SelectionBuilder[Country, (capital: Option[String])] =
            SelectionBuilder.scalar(
                "capital",
                CompiledNamedType("String"),
                ScalarCodec.option(ScalarCodec.string)
            )
    end Country

    object Queries:
        def country[A <: AnyNamedTuple](code: String)(
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder[RootQuery, (country: Option[A])] =
            SelectionBuilder.obj(
                "country",
                CompiledNamedType("Country"),
                List(
                    SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, ScalarCodec.id.encode(code))
                ),
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

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
            // Static type here IS `(country: Option[(name: String, capital: Option[String])])`.
            assert(result.country.map(_.name) == Some("Germany"))
            assert(result.country.flatMap(_.capital) == Some("Berlin"))
        }

        "null object → None; null scalar within a present object → None" in {
            val sel = Queries.country("XX")(Country.name ~ Country.capital)

            val missing = Json.JObj(Map("country" -> Json.JNull))
            assert(sel.decode(missing).country == None)

            val present = Json.JObj(
                Map(
                    "country" -> Json.JObj(Map("name" -> Json.JStr("Narnia"), "capital" -> Json.JNull))
                )
            )
            val result = sel.decode(present)
            assert(result.country.map(_.name) == Some("Narnia"))
            assert(result.country.flatMap(_.capital) == None)
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
            assert(country.arguments == List(CompiledArgument.variable("code")))
            assert(
                country.selections.collect { case f: CompiledField => f.name } ==
                    List("__typename", "name", "capital")
            )
        }
    }
end SelectionBuilderSpikeSpec
