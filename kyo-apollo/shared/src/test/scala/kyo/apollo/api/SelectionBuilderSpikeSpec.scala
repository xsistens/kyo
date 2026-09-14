package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Empty
import scala.collection.immutable.VectorMap

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
            sel: SelectionBuilder.Bidirectional[Country, A]
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
            sel: SelectionBuilder.Bidirectional[Country, A]
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
            sel: SelectionBuilder.Bidirectional[Country, A]
        ): SelectionBuilder.Deferrable[kyo.apollo.api.RootQuery, (country: Maybe[A])] =
            SelectionBuilder.obj(
                "country",
                CompiledNamedType("Country"),
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Nullable(SelectionBuilder.Nesting.Leaf)
            )
    end ApiQueries

    /** Twenty-one fields on one object, for a selection wide enough that an offset or a
      * split in the combination tree would land a value in the wrong slot.
      */
    sealed trait Wide

    object Wide:
        private def int[R <: AnyNamedTuple](name: String): SelectionBuilder.Deferrable[Wide, R] =
            SelectionBuilder.scalar(name, CompiledNamedType("Int").notNull, ScalarCodec.int)
        private def text[R <: AnyNamedTuple](name: String): SelectionBuilder.Deferrable[Wide, R] =
            SelectionBuilder.scalar(name, CompiledNamedType("String"), ScalarCodec.maybe(ScalarCodec.string))

        def f01: SelectionBuilder.Deferrable[Wide, (f01: Int)]           = int("f01")
        def f02: SelectionBuilder.Deferrable[Wide, (f02: Int)]           = int("f02")
        def f03: SelectionBuilder.Deferrable[Wide, (f03: Int)]           = int("f03")
        def f04: SelectionBuilder.Deferrable[Wide, (f04: Int)]           = int("f04")
        def f05: SelectionBuilder.Deferrable[Wide, (f05: Maybe[String])] = text("f05")
        def f06: SelectionBuilder.Deferrable[Wide, (f06: Int)]           = int("f06")
        def f07: SelectionBuilder.Deferrable[Wide, (f07: Maybe[String])] = text("f07")
        def f08: SelectionBuilder.Deferrable[Wide, (f08: Int)]           = int("f08")
        def f09: SelectionBuilder.Deferrable[Wide, (f09: Int)]           = int("f09")
        def f10: SelectionBuilder.Deferrable[Wide, (f10: Int)]           = int("f10")
        def g1: SelectionBuilder.Deferrable[Wide, (g1: Maybe[String])]   = text("g1")
        def g2: SelectionBuilder.Deferrable[Wide, (g2: Int)]             = int("g2")
        def f12: SelectionBuilder.Deferrable[Wide, (f12: Int)] =
            SelectionBuilder.scalar(
                "f12",
                CompiledNamedType("Int").notNull,
                ScalarCodec.int,
                Chunk(SelectionBuilder.Arg("n", CompiledNamedType("Int").notNull, Json.JInt(12)))
            )
        def f13: SelectionBuilder.Deferrable[Wide, (f13: Int)]           = int("f13")
        def f14: SelectionBuilder.Deferrable[Wide, (f14: Int)]           = int("f14")
        def f15: SelectionBuilder.Deferrable[Wide, (f15: Maybe[String])] = text("f15")
        def f16: SelectionBuilder.Deferrable[Wide, (f16: Int)]           = int("f16")
        def f17: SelectionBuilder.Deferrable[Wide, (f17: Int)]           = int("f17")
        def f18: SelectionBuilder.Deferrable[Wide, (f18: Int)]           = int("f18")
        def f19: SelectionBuilder.Deferrable[Wide, (f19: Int)]           = int("f19")
        def f20: SelectionBuilder.Deferrable[Wide, (f20: Int)] =
            SelectionBuilder.scalar(
                "f20",
                CompiledNamedType("Int").notNull,
                ScalarCodec.int,
                Chunk(SelectionBuilder.Arg("m", CompiledNamedType("Int").notNull, Json.JInt(20)))
            )

        /** Twenty result slots: a `.deferred` field at slot 10 and a two-field `defer`
          * group at slot 11, in the middle of the left-leaning combination tree.
          */
        val selection =
            f01 ~ f02 ~ f03 ~ f04 ~ f05 ~ f06 ~ f07 ~ f08 ~ f09 ~ f10.deferred ~ defer("grp", g1 ~ g2) ~
                f12 ~ f13 ~ f14 ~ f15 ~ f16 ~ f17 ~ f18 ~ f19 ~ f20
    end Wide

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

            val result = sel.decode(response).getOrThrow
            // Static type here IS `(country: Maybe[(name: String, capital: Maybe[String])])`.
            assert(result.country.map(_.name) == Present("Germany"))
            assert(result.country.flatMap(_.capital) == Present("Berlin"))
        }

        "null object → Absent; null scalar within a present object → Absent" in {
            val sel = Queries.country("XX")(Country.name ~ Country.capital)

            val missing = Json.JObj(Map("country" -> Json.JNull))
            assert(sel.decode(missing).getOrThrow.country == Absent)

            val present = Json.JObj(
                Map(
                    "country" -> Json.JObj(Map("name" -> Json.JStr("Narnia"), "capital" -> Json.JNull))
                )
            )
            val result = sel.decode(present).getOrThrow
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
            assert(sel.encode(sel.decode(response).getOrThrow) == response)
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
            val capital: Maybe[String] = Country.capital.decode(json).getOrThrow.capital
            assert(capital == Present("Berlin"))
            // ... and the stdlib type is a compile error, not a silent runtime cast.
            typeCheckFailure(
                "val c: Option[String] = Country.capital.decode(Json.JObj(Map.empty)).getOrThrow.capital"
            )("Required: Option[String]")
            typeCheckFailure(
                "val o: Option[(name: String)] = Queries.country(\"DE\")(Country.name).decode(Json.JObj(Map.empty)).getOrThrow.country"
            )("Required: Option[(name : String)]")
        }

        "a response of the wrong shape decodes to a failure value naming JSON types, never a value" in {
            val sel = Queries.country("DE")(Country.name ~ Country.capital)
            // `message` is the exception's own text; `getMessage` would add the source lines around
            // the frame in development mode, and those hold this test's literals.
            def failureOf(json: Json): String =
                sel.decode(json) match
                    case kyo.Result.Failure(e) => e.message
                    case other                 => fail(s"expected a parse failure, got $other")
            val notAnObject = failureOf(Json.JObj(Map("country" -> Json.JStr("secret-country"))))
            assert(notAnObject.contains("Expected a GraphQL object but got a string"), notAnObject)
            assert(!notAnObject.contains("secret-country"), notAnObject)
            val wrongLeaf =
                failureOf(Json.JObj(Map("country" -> Json.JObj(Map("name" -> Json.JInt(4242), "capital" -> Json.JNull)))))
            assert(wrongLeaf.contains("Expected a GraphQL String but got a number"), wrongLeaf)
            assert(!wrongLeaf.contains("4242"), wrongLeaf)
            val notAList = Continent.countries(Country.name).decode(Json.JObj(Map("countries" -> Json.JBool(true))))
            assert(notAList.failure.exists(_.message == "Expected a GraphQL list but got a boolean"), notAList.toString)
        }

        "a 20-field selection" - {

            def row(entries: (String, Json)*): Json = Json.JObj(VectorMap(entries*))
            def int(i: Int): Json                   = Json.JInt(i.toLong)

            val before = Seq("f01" -> int(1), "f02" -> int(2), "f03" -> int(3), "f04" -> int(4), "f05" -> Json.JNull)
            val middle = Seq("f06" -> int(6), "f07" -> Json.JStr("seven"), "f08" -> int(8), "f09" -> int(9))
            val after = Seq(
                "f12" -> int(12),
                "f13" -> int(13),
                "f14" -> int(14),
                "f15" -> Json.JStr("fifteen"),
                "f16" -> int(16),
                "f17" -> int(17),
                "f18" -> int(18),
                "f19" -> int(19),
                "f20" -> int(20)
            )
            val deferredPart = Seq("f10" -> int(10), "g1" -> Json.JStr("one"), "g2" -> int(2222))

            val initial  = row(before ++ middle ++ after*)
            val complete = row(before ++ middle ++ deferredPart ++ after*)

            val head = List(1, 2, 3, 4, Absent, 6, Present("seven"), 8, 9)
            val tail = List(12, 13, 14, Present("fifteen"), 16, 17, 18, 19, 20)

            "decodes every slot at its offset, the deferred ones Absent before their payload" in {
                val decoded = Wide.selection.decode(initial).getOrThrow
                assert(Wide.selection.arity == 20)
                assert(decoded.toTuple.toList == head ++ List(Absent, Absent) ++ tail)
                assert(decoded.f10 == Absent)
                assert(decoded.grp == Absent)
                assert(decoded.f20 == 20)
            }

            "decodes every slot at its offset, the deferred ones Present once their payload is in" in {
                val decoded = Wide.selection.decode(complete).getOrThrow
                assert(decoded.f10 == Present(10))
                assert(decoded.grp.map(_.toTuple) == Present((Present("one"), 2222)))
                assert(decoded.toTuple.toList.take(9) == head)
                assert(decoded.toTuple.toList.drop(11) == tail)
            }

            "encode(decode(json)) is json, field order included" in {
                assert(Wide.selection.encode(Wide.selection.decode(initial).getOrThrow).render == initial.render)
                assert(Wide.selection.encode(Wide.selection.decode(complete).getOrThrow).render == complete.render)
            }

            "a wrong leaf after the deferred slots fails the whole decode" in {
                val broken = row(before ++ middle ++ deferredPart ++ after.updated(8, "f20" -> Json.JStr("x"))*)
                assert(Wide.selection.decode(broken).failure.exists(_.message == "Expected a GraphQL Int but got a string"))
            }

            "its selections are computed once, when it is built" in {
                assert(Wide.selection.selections eq Wide.selection.selections)
                val field = Wide.f01
                assert(field.selections eq field.selections)
                assert(Wide.selection.selections.size == 20)
            }

            "its arguments are computed once, when it is built" in {
                assert(Wide.selection.argEntries eq Wide.selection.argEntries)
                assert(Wide.selection.argEntries.map(_.name) == Chunk("n", "m"))
            }
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

            "a .map projection cannot be written to the normalized cache" in {
                typeCheckFailure(
                    "(store: kyo.apollo.cache.normalized.ApolloStore, data: (country: Maybe[(name: String)])) => store.writeOperation(ApiQueries.country(Country.name).map(identity).toQuery(), data)"
                )("Required: kyo.apollo.api.Operation.Normalizable[")
                typeCheck(
                    "(store: kyo.apollo.cache.normalized.ApolloStore, data: (country: Maybe[(name: String)])) => store.writeOperation(ApiQueries.country(Country.name).toQuery(), data)"
                )
            }

            "a .map projection does not nest into a parent field; it projects the whole selection" in {
                typeCheckFailure("ApiQueries.country(Country.name.map(_.name))")(
                    "Required: kyo.apollo.api.SelectionBuilder.Bidirectional["
                )
                typeCheck("val q: Query[Maybe[String]] = ApiQueries.country(Country.name).map(_.country.map(_.name)).toQuery()")
            }

            "the operation a selection builds is normalizable exactly when the selection encodes" in {
                val bidirectional: SelectionBuilder[kyo.apollo.api.RootQuery, (country: Maybe[(name: String)])] =
                    ApiQueries.country(Country.name)
                val projected = ApiQueries.country(Country.name).map(identity)
                assert(bidirectional.toQuery("Q").isInstanceOf[Operation.Normalizable[?]])
                assert(!projected.toQuery("Q").isInstanceOf[Operation.Normalizable[?]])
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
                    deferred.decode(Json.JObj(Map("code" -> Json.JStr("DE")))).getOrThrow
                assert(result.capital == Absent)
            }
        }
    }
end SelectionBuilderSpikeSpec
