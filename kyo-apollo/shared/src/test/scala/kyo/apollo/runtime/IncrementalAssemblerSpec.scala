package kyo.apollo.runtime

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.json.JsonParser
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.http.MultipartPart
import scala.collection.immutable.VectorMap

/** Tests that [[IncrementalAssembler]] folds a `@defer` part stream into a stream
  * of progressively-fuller [[kyo.apollo.network.ApolloResponse]] — the initial
  * payload, then the deferred patch spliced at its path and re-decoded.
  */
class IncrementalAssemblerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Loc(code: String, capital: Option[String]) derives Schema
    final case class Data(country: Option[Loc]) derives Schema

    final case class Q() extends Query[Data]:
        def name: String             = "Q"
        def document: String         = "query Q { country { code capital } }"
        def dataSchema: Schema[Data] = summon[Schema[Data]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end Q

    final case class Item(id: String) derives Schema
    final case class ListData(items: List[Item]) derives Schema

    /** A `@stream` operation whose `items` list grows across incremental parts. */
    final case class ListQ() extends Query[ListData]:
        def name: String                 = "Q"
        def document: String             = "query Q { items @stream(initialCount: 1) { id } }"
        def dataSchema: Schema[ListData] = summon[Schema[ListData]]
        def rootField: CompiledField     = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json              = Json.JObj(VectorMap.empty)
    end ListQ

    private def part(s: String): MultipartPart = MultipartPart(JsonParser.parse(s).getOrThrow)

    "IncrementalAssembler" - {

        "emits the initial payload, then the deferred patch merged at its path" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"country":{"code":"DE"}},"hasNext":true}"""),
                    part(
                        """{"incremental":[{"data":{"capital":"Berlin"},"path":["country"]}],"hasNext":false}"""
                    )
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(Q()), parts)).map { responses =>
                assert(responses.size == 2)
                assert(responses(0).data == Present(Data(Some(Loc("DE", None)))))
                assert(responses(1).data == Present(Data(Some(Loc("DE", Some("Berlin"))))))
            }
        }

        "supports the legacy single-patch part shape ({data, path})" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"country":{"code":"FR"}},"hasNext":true}"""),
                    part("""{"data":{"capital":"Paris"},"path":["country"],"hasNext":false}""")
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(Q()), parts)).map { responses =>
                assert(responses.size == 2)
                assert(responses(1).data == Present(Data(Some(Loc("FR", Some("Paris"))))))
            }
        }

        "a final part with no data change emits nothing" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"country":{"code":"US"}},"hasNext":true}"""),
                    part("""{"hasNext":false}""")
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(Q()), parts)).map { responses =>
                assert(responses.size == 1)
                assert(responses(0).data == Present(Data(Some(Loc("US", None)))))
            }
        }

        "appends @stream `items` into the list across parts, growing it" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"items":[{"id":"a"}]},"hasNext":true}"""),
                    part("""{"incremental":[{"items":[{"id":"b"}],"path":["items",1]}],"hasNext":true}"""),
                    part("""{"incremental":[{"items":[{"id":"c"}],"path":["items",2]}],"hasNext":false}""")
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(ListQ()), parts)).map {
                responses =>
                    assert(responses.size == 3)
                    assert(responses(0).data == Present(ListData(List(Item("a")))))
                    assert(responses(1).data == Present(ListData(List(Item("a"), Item("b")))))
                    assert(responses(2).data == Present(ListData(List(Item("a"), Item("b"), Item("c")))))
            }
        }

        "surfaces an incremental part that carries only errors (even on the terminal part)" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"items":[{"id":"a"}]},"hasNext":true}"""),
                    part(
                        """{"incremental":[{"errors":[{"message":"boom"}],"path":["items",1]}],"hasNext":false}"""
                    )
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(ListQ()), parts)).map {
                responses =>
                    assert(responses.size == 2)
                    assert(responses(1).errors.nonEmpty)
            }
        }

        "appends multiple @stream items delivered in a single part" in {
            val parts = Stream.init(
                Seq(
                    part("""{"data":{"items":[{"id":"a"}]},"hasNext":true}"""),
                    part(
                        """{"incremental":[{"items":[{"id":"b"},{"id":"c"}],"path":["items",1]}],"hasNext":false}"""
                    )
                )
            )
            StreamProbe.collect(IncrementalAssembler.stream(ApolloRequest(ListQ()), parts)).map {
                responses =>
                    assert(responses.size == 2)
                    assert(responses(1).data == Present(ListData(List(Item("a"), Item("b"), Item("c")))))
            }
        }
    }
end IncrementalAssemblerSpec
