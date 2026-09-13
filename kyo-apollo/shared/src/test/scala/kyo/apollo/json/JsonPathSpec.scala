package kyo.apollo.json

/** Unit tests for [[JsonPath]] — parsing a wire response path and splicing an
  * incremental-delivery patch (field-union) into a JSON tree at that path.
  */
class JsonPathSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def j(s: String): Json = JsonParser.parse(s)

    "JsonPath.parse" - {
        "parses mixed field-name / list-index segments" in {
            assert(
                JsonPath.parse(j("""["country", 0, "capital"]""")) ==
                    List[String | Int]("country", 0, "capital")
            )
        }
    }

    "JsonPath.splice" - {

        "an empty path field-unions the patch into the root object" in {
            assert(
                JsonPath.splice(j("""{"code":"DE"}"""), Nil, j("""{"capital":"Berlin"}""")) ==
                    j("""{"code":"DE","capital":"Berlin"}""")
            )
        }

        "a field path merges into a nested object" in {
            assert(
                JsonPath.splice(
                    j("""{"country":{"code":"DE"}}"""),
                    List("country"),
                    j("""{"capital":"Berlin"}""")
                ) == j("""{"country":{"code":"DE","capital":"Berlin"}}""")
            )
        }

        "an index path merges into a list element" in {
            assert(
                JsonPath.splice(
                    j("""{"items":[{"id":1}]}"""),
                    List("items", 0),
                    j("""{"name":"x"}""")
                ) == j("""{"items":[{"id":1,"name":"x"}]}""")
            )
        }

        "a path descending into a non-object leaf is a no-op" in {
            assert(
                JsonPath.splice(j("""{"a":1}"""), List("a", "deep"), j("""{"x":2}""")) == j("""{"a":1}""")
            )
        }
    }

    "JsonPath.spliceItems" - {

        "appends a single @stream item at the tail index of a nested list" in {
            assert(
                JsonPath.spliceItems(
                    j("""{"items":[{"id":"a"}]}"""),
                    List("items", 1),
                    List(j("""{"id":"b"}"""))
                ) == j("""{"items":[{"id":"a"},{"id":"b"}]}""")
            )
        }

        "appends multiple items delivered in one part at consecutive indices" in {
            assert(
                JsonPath.spliceItems(
                    j("""{"xs":["a"]}"""),
                    List("xs", 1),
                    List(j(""""b""""), j(""""c""""))
                ) ==
                    j("""{"xs":["a","b","c"]}""")
            )
        }

        "an in-range index overwrites rather than growing" in {
            assert(
                JsonPath.spliceItems(j("""{"xs":["a","z"]}"""), List("xs", 1), List(j(""""b""""))) ==
                    j("""{"xs":["a","b"]}""")
            )
        }

        "an index into a non-list is a no-op" in {
            assert(
                JsonPath.spliceItems(j("""{"xs":{"n":1}}"""), List("xs", 0), List(j("""1"""))) ==
                    j("""{"xs":{"n":1}}""")
            )
        }

        "a negative start index is a no-op (never throws on a malformed path)" in {
            assert(
                JsonPath.spliceItems(j("""{"xs":["a"]}"""), List("xs", -1), List(j(""""b""""))) ==
                    j("""{"xs":["a"]}""")
            )
        }
    }
end JsonPathSpec
