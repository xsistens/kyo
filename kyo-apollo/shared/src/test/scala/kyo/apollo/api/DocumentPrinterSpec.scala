package kyo.apollo.api

import kyo.apollo.json.Json

/** Unit tests for the runtime GraphQL printer that renders an inline
  * [[SelectionBuilder]]'s compiled tree back into a document string.
  */
class DocumentPrinterSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "DocumentPrinter.render" - {

        "renders a scalar variable arg + nested selection" in {
            val args =
                List(SelectionBuilder.Arg("code", CompiledNamedType("ID").notNull, Json.JStr("DE")))
            val sels = List(
                CompiledField(
                    "country",
                    CompiledNamedType("Country"),
                    arguments = List(CompiledArgument.variable("code")),
                    selections = List(
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
                List(SelectionBuilder.Arg("filter", CompiledNamedType("CountryFilter"), Json.JNull))
            val sels = List(
                CompiledField(
                    "countries",
                    CompiledNamedType("Country").notNull.list.notNull,
                    arguments = List(CompiledArgument.variable("filter")),
                    selections = List(CompiledField("code", CompiledNamedType("ID").notNull))
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountries", args, sels) ==
                    "query GetCountries($filter: CountryFilter) { countries(filter: $filter) { code } }"
            )
        }

        "renders a no-argument operation without a variable header" in {
            val sels = List(
                CompiledField(
                    "me",
                    CompiledNamedType("User"),
                    selections = List(CompiledField("id", CompiledNamedType("ID").notNull))
                )
            )
            assert(DocumentPrinter.render("query", "Me", Nil, sels) == "query Me { me { id } }")
        }

        "renders an anonymous @defer fragment inside a field" in {
            val sels = List(
                CompiledField(
                    "country",
                    CompiledNamedType("Country"),
                    selections = List(
                        CompiledField("code", CompiledNamedType("ID").notNull),
                        CompiledFragment(
                            "",
                            Nil,
                            List(CompiledField("capital", CompiledNamedType("String"))),
                            defer = Some(DeferDirective("details"))
                        )
                    )
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountry", Nil, sels) ==
                    "query GetCountry { country { code ... @defer(label: \"details\") { capital } } }"
            )
        }

        "renders a typed @defer fragment carrying an if condition" in {
            val sels = List(
                CompiledFragment(
                    "Launch",
                    List("Launch"),
                    List(CompiledField("site", CompiledNamedType("String"))),
                    defer = Some(DeferDirective("more", `if` = Some("expand")))
                )
            )
            assert(
                DocumentPrinter.render("query", "Q", Nil, sels) ==
                    "query Q { ... on Launch @defer(label: \"more\", if: $expand) { site } }"
            )
        }

        "renders a @stream directive on a list field (initialCount first)" in {
            val sels = List(
                CompiledField(
                    "countries",
                    CompiledNamedType("Country").notNull.list.notNull,
                    selections = List(CompiledField("name", CompiledNamedType("String").notNull)),
                    stream = Some(StreamDirective("countries", 1))
                )
            )
            assert(
                DocumentPrinter.render("query", "GetCountries", Nil, sels) ==
                    "query GetCountries { countries @stream(initialCount: 1, label: \"countries\") { name } }"
            )
        }

        "renders @stream after the field arguments and before the selection set, with if" in {
            val sels = List(
                CompiledField(
                    "feed",
                    CompiledNamedType("Post").notNull.list.notNull,
                    arguments = List(CompiledArgument.variable("limit")),
                    selections = List(CompiledField("id", CompiledNamedType("ID").notNull)),
                    stream = Some(StreamDirective("feed", 2, `if` = Some("live")))
                )
            )
            assert(
                DocumentPrinter.render("query", "Q", Nil, sels) ==
                    "query Q { feed(limit: $limit) @stream(initialCount: 2, label: \"feed\", if: $live) { id } }"
            )
        }
    }
end DocumentPrinterSpec
