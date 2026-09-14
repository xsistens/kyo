package kyo.apollo.json

import kyo.Absent
import kyo.Chunk
import kyo.Present

/** Unit tests for [[JsonPath]] — parsing a wire response path and splicing an
  * incremental-delivery patch (field-union) into a JSON tree at that path. A path
  * that does not fit is `Absent`, never the unchanged tree.
  */
class JsonPathSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def j(s: String): Json = JsonParser.parse(s).getOrThrow

    private def path(segments: (String | Int)*): Chunk[String | Int] = Chunk.from(segments)

    "JsonPath.parse" - {
        "parses mixed field-name / list-index segments" in {
            assert(JsonPath.parse(j("""["country", 0, "capital"]""")) == Present(path("country", 0, "capital")))
        }

        "the empty array is the root path" in {
            assert(JsonPath.parse(j("""[]""")) == Present(path()))
        }

        "a segment that is neither a field name nor an exact non-negative Int is Absent" in {
            assert(JsonPath.parse(j("""["a", true]""")) == Absent)
            assert(JsonPath.parse(j("""["a", 1.5]""")) == Absent)
            assert(JsonPath.parse(j("""["a", -1]""")) == Absent)
            assert(JsonPath.parse(j("""["a", 2147483648]""")) == Absent)
        }

        "a path that is not an array is Absent" in {
            assert(JsonPath.parse(j("""{"a":1}""")) == Absent)
            assert(JsonPath.parse(j("""null""")) == Absent)
        }
    }

    "JsonPath.splice" - {

        "an empty path field-unions the patch into the root object" in {
            assert(
                JsonPath.splice(j("""{"code":"DE"}"""), path(), j("""{"capital":"Berlin"}""")) ==
                    Present(j("""{"code":"DE","capital":"Berlin"}"""))
            )
        }

        "a field path merges into a nested object" in {
            assert(
                JsonPath.splice(j("""{"country":{"code":"DE"}}"""), path("country"), j("""{"capital":"Berlin"}""")) ==
                    Present(j("""{"country":{"code":"DE","capital":"Berlin"}}"""))
            )
        }

        "an index path merges into a list element" in {
            assert(
                JsonPath.splice(j("""{"items":[{"id":1},{"id":2}]}"""), path("items", 1), j("""{"name":"x"}""")) ==
                    Present(j("""{"items":[{"id":1},{"id":2,"name":"x"}]}"""))
            )
        }

        "a path descending into a non-object leaf is Absent" in {
            assert(JsonPath.splice(j("""{"a":1}"""), path("a", "deep"), j("""{"x":2}""")) == Absent)
        }

        "an index into an object, or past the end of a list, is Absent" in {
            assert(JsonPath.splice(j("""{"country":{"code":"DE"}}"""), path("country", 0), j("""{"x":2}""")) == Absent)
            assert(JsonPath.splice(j("""{"items":[{"id":1}]}"""), path("items", 1), j("""{"x":2}""")) == Absent)
        }
    }

    "JsonPath.spliceItems" - {

        "appends a single @stream item at the tail index of a nested list" in {
            assert(
                JsonPath.spliceItems(j("""{"items":[{"id":"a"}]}"""), path("items", 1), Chunk(j("""{"id":"b"}"""))) ==
                    Present(j("""{"items":[{"id":"a"},{"id":"b"}]}"""))
            )
        }

        "appends multiple items delivered in one part at consecutive indices" in {
            assert(
                JsonPath.spliceItems(j("""{"xs":["a"]}"""), path("xs", 1), Chunk(j(""""b""""), j(""""c""""))) ==
                    Present(j("""{"xs":["a","b","c"]}"""))
            )
        }

        "an in-range index overwrites from there, growing past the end if needed" in {
            assert(
                JsonPath.spliceItems(j("""{"xs":["a","z"]}"""), path("xs", 1), Chunk(j(""""b""""))) ==
                    Present(j("""{"xs":["a","b"]}"""))
            )
            assert(
                JsonPath.spliceItems(j("""{"xs":["a","z"]}"""), path("xs", 1), Chunk(j(""""b""""), j(""""c""""))) ==
                    Present(j("""{"xs":["a","b","c"]}"""))
            )
        }

        "a list nested in a list element is reached through its index" in {
            assert(
                JsonPath.spliceItems(j("""{"rows":[{"xs":[1]}]}"""), path("rows", 0, "xs", 1), Chunk(j("""2"""))) ==
                    Present(j("""{"rows":[{"xs":[1,2]}]}"""))
            )
        }

        "an index into a non-list is Absent" in {
            assert(JsonPath.spliceItems(j("""{"xs":{"n":1}}"""), path("xs", 0), Chunk(j("""1"""))) == Absent)
        }

        "a negative start index is Absent" in {
            assert(JsonPath.spliceItems(j("""{"xs":["a"]}"""), path("xs", -1), Chunk(j(""""b""""))) == Absent)
        }

        "a start index past the tail (a gap) is Absent" in {
            assert(JsonPath.spliceItems(j("""{"xs":["a"]}"""), path("xs", 3), Chunk(j(""""b""""))) == Absent)
        }

        "a path that does not end in an index is Absent" in {
            assert(JsonPath.spliceItems(j("""{"xs":["a"]}"""), path("xs"), Chunk(j(""""b""""))) == Absent)
            assert(JsonPath.spliceItems(j("""["a"]"""), path(), Chunk(j(""""b""""))) == Absent)
        }

        "spliceItems appends 1000 single-item patches" in {
            val start = j("""{"xs":[0]}""")
            val grown = (1 to 1000).foldLeft(start) { (tree, i) =>
                JsonPath.spliceItems(tree, path("xs", i), Chunk(Json.JInt(i))).getOrElse(fail(s"patch $i did not fit"))
            }
            grown match
                case Json.JObj(fields) =>
                    fields.get("xs") match
                        case Some(Json.JArr(xs)) =>
                            assert(xs.size == 1001)
                            assert(xs(0) == Json.JInt(0) && xs(500) == Json.JInt(500) && xs(1000) == Json.JInt(1000))
                            assert(xs == Chunk.from((0 to 1000).map(i => Json.JInt(i))))
                        case other => fail(s"expected the grown list, got $other")
                case other => fail(s"expected an object, got $other")
            end match
        }
    }
end JsonPathSpec
