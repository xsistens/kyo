package kyo.apollo.api

import kyo.Chunk
import kyo.Present
import kyo.apollo.json.Json

/** Unit tests for the runtime GraphQL printer that renders an inline
  * [[SelectionBuilder]]'s compiled tree back into a document string.
  */
class DocumentPrinterSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "DocumentPrinter.render" - {

        "renders a scalar variable arg + nested selection" in {
            val args =
                Chunk(SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, Json.JStr("DE")))
            val sels = Chunk(
                CompiledField(
                    "country",
                    CompiledNamedType("Country"),
                    arguments = Chunk(CompiledArgument.variable("code")),
                    selections = Chunk(
                        CompiledField("name", CompiledNamedType("String").notNull),
                        CompiledField("capital", CompiledNamedType("String"))
                    )
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountry", args, sels) ==
                    "query GetCountry($code: ID!) { country(code: $code) { name capital } }"
            )
        }

        "renders list/non-null variable types in the header" in {
            val args =
                Chunk(SelectionBuilder.Arg("filter", CompiledNamedType("CountryFilter"), Json.JNull))
            val sels = Chunk(
                CompiledField(
                    "countries",
                    CompiledNamedType("Country").notNull.list.notNull,
                    arguments = Chunk(CompiledArgument.variable("filter")),
                    selections = Chunk(CompiledField("code", CompiledNamedType("ID").notNull))
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountries", args, sels) ==
                    "query GetCountries($filter: CountryFilter) { countries(filter: $filter) { code } }"
            )
        }

        "renders a no-argument operation without a variable header" in {
            val sels = Chunk(
                CompiledField(
                    "me",
                    CompiledNamedType("User"),
                    selections = Chunk(CompiledField("id", CompiledNamedType("ID").notNull))
                )
            )
            assert(DocumentPrinter.render("query", "Me", Chunk.empty, sels) == "query Me { me { id } }")
        }

        "renders an anonymous @defer fragment inside a field" in {
            val sels = Chunk(
                CompiledField(
                    "country",
                    CompiledNamedType("Country"),
                    selections = Chunk(
                        CompiledField("code", CompiledNamedType("ID").notNull),
                        CompiledFragment(
                            "",
                            Chunk.empty,
                            Chunk(CompiledField("capital", CompiledNamedType("String"))),
                            defer = Present(DeferDirective("details"))
                        )
                    )
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountry", Chunk.empty, sels) ==
                    "query GetCountry { country { code ... @defer(label: \"details\") { capital } } }"
            )
        }

        "renders a typed @defer fragment carrying an if condition" in {
            val sels = Chunk(
                CompiledFragment(
                    "Launch",
                    Chunk("Launch"),
                    Chunk(CompiledField("site", CompiledNamedType("String"))),
                    defer = Present(DeferDirective("more", `if` = Present("expand")))
                )
            )
            assert(
                DocumentPrinter.render("query", "Q", Chunk.empty, sels) ==
                    "query Q { ... on Launch @defer(label: \"more\", if: $expand) { site } }"
            )
        }

        "renders a @stream directive on a list field (initialCount first)" in {
            val sels = Chunk(
                CompiledField(
                    "countries",
                    CompiledNamedType("Country").notNull.list.notNull,
                    selections = Chunk(CompiledField("name", CompiledNamedType("String").notNull)),
                    stream = Present(StreamDirective("countries", 1))
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountries", Chunk.empty, sels) ==
                    "query GetCountries { countries @stream(initialCount: 1, label: \"countries\") { name } }"
            )
        }

        "renders @stream after the field arguments and before the selection set, with if" in {
            val sels = Chunk(
                CompiledField(
                    "feed",
                    CompiledNamedType("Post").notNull.list.notNull,
                    arguments = Chunk(CompiledArgument.variable("limit")),
                    selections = Chunk(CompiledField("id", CompiledNamedType("ID").notNull)),
                    stream = Present(StreamDirective("feed", 2, `if` = Present("live")))
                )
            )
            assert(
                DocumentPrinter.render("query", "Q", Chunk.empty, sels) ==
                    "query Q { feed(limit: $limit) @stream(initialCount: 2, label: \"feed\", if: $live) { id } }"
            )
        }
    }
end DocumentPrinterSpec
