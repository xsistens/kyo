package kyo.apollo.api

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.apollo.json.JsonParser
import scala.NamedTuple.AnyNamedTuple

/** Unit tests for the `.streamed(initialCount)` combinator: it marks the last-added
  * list field with an auto-labelled `@stream` directive while leaving the result
  * type UNCHANGED (`Chunk[A]` stays `Chunk[A]` — no `Maybe` wrapper, unlike
  * `.deferred`). Mirrors the hand-written selector layer of [[SelectionBuilderSpikeSpec]].
  */
class StreamBuilderSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    sealed trait RootQuery
    sealed trait Country

    object Country:
        def name: SelectionBuilder.Deferrable[Country, (name: String)] =
            SelectionBuilder.scalar("name", CompiledNamedType("String").notNull, ScalarCodec.string)

    object Queries:
        def hello: SelectionBuilder.Deferrable[RootQuery, (hello: String)] =
            SelectionBuilder.scalar("hello", CompiledNamedType("String").notNull, ScalarCodec.string)

        def countries[A <: AnyNamedTuple](
            sel: SelectionBuilder[Country, A]
        ): SelectionBuilder.Deferrable[RootQuery, (countries: Chunk[A])] =
            SelectionBuilder.obj(
                "countries",
                CompiledNamedType("Country").notNull.list.notNull,
                Chunk.empty,
                sel,
                SelectionBuilder.Nesting.Listed(SelectionBuilder.Nesting.Leaf)
            )
    end Queries

    private def fields(sb: SelectionBuilder[?, ?]): Chunk[CompiledField] =
        sb.selections.collect { case f: CompiledField => f }

    "`.streamed`" - {

        "marks the list field with an auto-labelled @stream directive (label = response name)" in {
            val sel = Queries.countries(Country.name).streamed(initialCount = 2)
            val f   = fields(sel).head
            assert(f.name == "countries")
            assert(f.stream == Present(StreamDirective("countries", 2, Absent)))
        }

        "leaves the result type unchanged (Chunk[A]) — the streamed list still decodes" in {
            val sel   = Queries.countries(Country.name).streamed(initialCount = 1)
            val short = JsonParser.parse("""{"countries":[{"name":"Germany"}]}""").getOrThrow
            // Static type here IS `(countries: List[(name: String)])` — proving no wrapper.
            val result = sel.decode(short)
            assert(result.countries.map(_.name) == Chunk("Germany"))
        }

        "applied to a combined selection, streams only the LAST-added field" in {
            val sel = (Queries.hello ~ Queries.countries(Country.name)).streamed(initialCount = 3)
            val fs  = fields(sel)
            assert(fs.find(_.name == "hello").get.stream == Absent)
            assert(
                fs.find(_.name == "countries").get.stream == Present(
                    StreamDirective("countries", 3, Absent)
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
