package kyo.apollo.network.http

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.api.StreamDirective
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.interceptor.NetworkInterceptor
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpHeader
import scala.collection.immutable.VectorMap

/** End-to-end `@stream` incremental delivery through [[HttpNetworkTransport]]: a
  * `multipart/mixed` reply carrying `items` patches is folded into a stream of
  * responses whose list GROWS item-by-item; a server that ignores `@stream` (plain
  * JSON) collapses to a single full list; and [[NetworkInterceptor]] routes a
  * `@stream` operation to the streaming path. Sibling of [[DeferSpec]].
  */
class StreamSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Item(id: String) derives Schema
    final case class Data(items: List[Item]) derives Schema

    /** An operation whose `items` list field carries a `@stream` directive. */
    final case class StreamQ() extends Query[Data]:
        def name: String             = "Q"
        def document: String         = "query Q { items @stream(initialCount: 1) { id } }"
        def dataSchema: Schema[Data] = summon[Schema[Data]]
        def rootField: CompiledField = CompiledField(
            "data",
            CompiledNamedType("Query"),
            selections = List(
                CompiledField(
                    "items",
                    CompiledNamedType("Item").notNull.list.notNull,
                    selections = List(CompiledField("id", CompiledNamedType("ID").notNull)),
                    stream = Some(StreamDirective("items", 1))
                )
            )
        )
        def variables: Json = Json.JObj(VectorMap.empty)
    end StreamQ

    private val boundary    = "graphql"
    private val contentType = s"multipart/mixed; boundary=$boundary"

    /** A three-part `@stream` body: initial `[a]`, then `b` appended at index 1, then
      * `c` appended at index 2.
      */
    private val multipart =
        s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"data":{"items":[{"id":"a"}]},"hasNext":true}""" +
            s"\r\n--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"incremental":[{"items":[{"id":"b"}],"path":["items",1]}],"hasNext":true}""" +
            s"\r\n--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"incremental":[{"items":[{"id":"c"}],"path":["items",2]}],"hasNext":false}""" +
            s"\r\n--$boundary--\r\n"

    private def engineOf(f: HttpRequest => HttpResponse < Async): HttpEngine =
        new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async = f(request)

    private def transport(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport("https://x/graphql", engine)

    private def respond(status: Int, ct: String, body: String): HttpRequest => HttpResponse < Async =
        _ => HttpResponse(status, List(HttpHeader("Content-Type", ct)), body)

    "transport.executeStreaming (@stream)" - {

        "emits the initial list, then each appended item, growing the list" in {
            val t = transport(engineOf(respond(200, contentType, multipart)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(StreamQ()))).map { rs =>
                assert(rs.size == 3)
                assert(rs(0).data == Present(Data(List(Item("a")))))
                assert(rs(1).data == Present(Data(List(Item("a"), Item("b")))))
                assert(rs(2).data == Present(Data(List(Item("a"), Item("b"), Item("c")))))
            }
        }

        "a server that ignores @stream (plain JSON) collapses to a single full list" in {
            val body = """{"data":{"items":[{"id":"a"},{"id":"b"}]}}"""
            val t    = transport(engineOf(respond(200, "application/json", body)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(StreamQ()))).map { rs =>
                assert(rs.size == 1)
                assert(rs(0).data == Present(Data(List(Item("a"), Item("b")))))
            }
        }
    }

    "NetworkInterceptor routing (@stream)" - {

        "routes a @stream op to the streaming path (list grows)" in {
            val engine = engineOf(respond(200, contentType, multipart))
            val chain  = DefaultApolloInterceptorChain(Chunk(new NetworkInterceptor(transport(engine))), 0)
            StreamProbe.collect(chain.proceed(ApolloRequest(StreamQ()))).map { streamed =>
                assert(streamed.size == 3)
                assert(streamed(2).data == Present(Data(List(Item("a"), Item("b"), Item("c")))))
            }
        }
    }
end StreamSpec
