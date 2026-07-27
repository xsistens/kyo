package kyo.apollo.api

import kyo.apollo.json.JsonParser
import scala.NamedTuple.AnyNamedTuple

/** Unit tests for the `.streamed(initialCount)` combinator: it marks the last-added
  * list field with an auto-labelled `@stream` directive while leaving the result
  * type UNCHANGED (`List[A]` stays `List[A]` — no `Maybe` wrapper, unlike
  * `.deferred`). Mirrors the hand-written selector layer of [[SelectionBuilderSpikeSpec]].
  */
class StreamBuilderSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    sealed trait RootQuery
    sealed trait Country

    object Country:
        def name: SelectionBuilder[Country, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

    object Queries:
        def hello: SelectionBuilder[RootQuery, (hello: String)] =
            SelectionBuilder.scalar("hello", CompiledNamedType("String").notNull, ScalarCodec.string)

        def countries[A <: AnyNamedTuple](
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder[RootQuery, (countries: List[A])] =
            SelectionBuilder.obj(
                "countries",
                CompiledNamedType("Country").notNull.list.notNull,
                Nil,
                sel,
                SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

    private def fields(sb: SelectionBuilder[?, ?]): List[CompiledField] =
        sb.selections.collect { case f: CompiledField => f }

    "`.streamed`" - {

        "marks the list field with an auto-labelled @stream directive (label = response name)" in {
            val sel = Queries.countries(Country.name).streamed(initialCount = 2)
            val f   = fields(sel).head
            assert(f.name == "countries")
            assert(f.stream == Some(StreamDirective("countries", 2, None)))
        }

        "leaves the result type unchanged (List[A]) — the streamed list still decodes" in {
            val sel   = Queries.countries(Country.name).streamed(initialCount = 1)
            val short = JsonParser.parse("""{"countries":[{"name":"Germany"}]}""")
            // Static type here IS `(countries: List[(name: String)])` — proving no wrapper.
            val result = sel.decode(short)
            assert(result.countries.map(_.name) == List("Germany"))
        }

        "applied to a combined selection, streams only the LAST-added field" in {
            val sel = (Queries.hello ~ Queries.countries(Country.name)).streamed(initialCount = 3)
            val fs  = fields(sel)
            assert(fs.find(_.name == "hello").flatMap(_.stream) == None)
            assert(
                fs.find(_.name == "countries").flatMap(_.stream) == Some(
                    StreamDirective("countries", 3, None)
                )
            )
        }

        "the streamed field flips Defer.has (drives the multipart Accept + streaming path)" in {
            val streamed = Queries.countries(Country.name).streamed(initialCount = 1)
            val plain    = Queries.countries(Country.name)
            assert(
                Defer.has(
                    CompiledField("data", CompiledNamedType("Query"), selections = streamed.selections)
                )
            )
            assert(
                !Defer.has(CompiledField("data", CompiledNamedType("Query"), selections = plain.selections))
            )
        }
    }
end StreamBuilderSpec
