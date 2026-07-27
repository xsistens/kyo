package kyo.apollo.network.http

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledFragment
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.DeferDirective
import kyo.apollo.api.Query
import kyo.apollo.interceptor.DefaultApolloInterceptorChain
import kyo.apollo.interceptor.NetworkInterceptor
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpHeader
import scala.collection.immutable.VectorMap
import scala.concurrent.Future

/** End-to-end `@defer` incremental delivery through [[HttpNetworkTransport]]:
  * a `multipart/mixed` reply is split + folded into a stream of progressively
  * fuller responses; a server that ignores `@defer`, a non-2xx status, and a
  * network drop each collapse to a single value; and [[NetworkInterceptor]] routes
  * a deferred operation to the streaming path and a plain one to the single path.
  */
class DeferSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Loc(code: String, capital: Option[String]) derives Schema
    final case class Data(country: Option[Loc]) derives Schema

    /** An operation whose selection tree carries a `@defer`ed fragment. */
    final case class DeferQ() extends Query[Data]:
        def name: String             = "Q"
        def document: String         = "query Q { country { code ... @defer(label: \"capital\") { capital } } }"
        def dataSchema: Schema[Data] = summon[Schema[Data]]
        def rootField: CompiledField = CompiledField(
            "data",
            CompiledNamedType("Query"),
            selections = List(
                CompiledField(
                    "country",
                    CompiledNamedType("Loc"),
                    selections = List(
                        CompiledField("code", CompiledNamedType("String").notNull),
                        CompiledFragment(
                            "",
                            Nil,
                            List(CompiledField("capital", CompiledNamedType("String"))),
                            defer = Some(DeferDirective("capital"))
                        )
                    )
                )
            )
        )
        def variables: Json = Json.JObj(VectorMap.empty)
    end DeferQ

    /** Same data shape, but no `@defer` — [[kyo.apollo.api.Defer.has]] is false. */
    final case class PlainQ() extends Query[Data]:
        def name: String             = "Q"
        def document: String         = "query Q { country { code capital } }"
        def dataSchema: Schema[Data] = summon[Schema[Data]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end PlainQ

    private val boundary    = "graphql"
    private val contentType = s"multipart/mixed; boundary=$boundary"

    /** A two-part incremental body: initial `country.code`, then the deferred
      * `country.capital` patch.
      */
    private val multipart =
        s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"data":{"country":{"code":"DE"}},"hasNext":true}""" +
            s"\r\n--$boundary\r\nContent-Type: application/json\r\n\r\n" +
            """{"incremental":[{"data":{"capital":"Berlin"},"path":["country"]}],"hasNext":false}""" +
            s"\r\n--$boundary--\r\n"

    private def engineOf(f: HttpRequest => HttpResponse < Async): HttpEngine =
        new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async = f(request)

    private def transport(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport("https://x/graphql", engine)

    private def respond(status: Int, ct: String, body: String): HttpRequest => HttpResponse < Async =
        _ => HttpResponse(status, List(HttpHeader("Content-Type", ct)), body)

    "transport.executeStreaming" - {

        "emits the initial payload then the merged @defer patch" in {
            val t = transport(engineOf(respond(200, contentType, multipart)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 2)
                assert(rs(0).data == Present(Data(Some(Loc("DE", None)))))
                assert(rs(1).data == Present(Data(Some(Loc("DE", Some("Berlin"))))))
            }
        }

        "a server that ignores @defer (plain JSON) collapses to a single response" in {
            val body = """{"data":{"country":{"code":"DE","capital":"Berlin"}}}"""
            val t    = transport(engineOf(respond(200, "application/json", body)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 1)
                assert(rs(0).data == Present(Data(Some(Loc("DE", Some("Berlin"))))))
            }
        }

        "a non-2xx status arrives as an exception value" in {
            val t = transport(engineOf(respond(500, contentType, "")))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 1)
                assert(rs(0).exception.isDefined)
                assert(rs(0).data == Absent)
            }
        }

        "a network drop arrives as an exception value" in {
            val boom = new RuntimeException("dropped")
            val t    = transport(engineOf(_ => Async.fromFuture(Future.failed[HttpResponse](boom))))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 1)
                assert(rs(0).exception.isDefined)
            }
        }
    }

    "NetworkInterceptor routing" - {

        "routes an @defer op to streaming and a plain op to the single path" in {
            val engine = engineOf(respond(200, contentType, multipart))
            val chain  = DefaultApolloInterceptorChain(Chunk(new NetworkInterceptor(transport(engine))), 0)
            for
                deferred <- StreamProbe.collect(chain.proceed(ApolloRequest(DeferQ())))
                plain    <- StreamProbe.collect(chain.proceed(ApolloRequest(PlainQ())))
            yield
                assert(deferred.size == 2) // @defer → executeStreaming splits the multipart body
                assert(deferred(1).data == Present(Data(Some(Loc("DE", Some("Berlin"))))))
                assert(plain.size == 1) // plain → execute → the multipart body is not single JSON
                assert(plain(0).exception.isDefined)
            end for
        }
    }
end DeferSpec
