package kyo.apollo.network.http

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledFragment
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.DeferDirective
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloNetworkException
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
                assert(rs(0).error.isDefined)
                assert(rs(0).data == Absent)
            }
        }

        "a network drop arrives as an exception value" in {
            val boom = new RuntimeException("dropped")
            val t    = transport(engineOf(_ => Async.fromFuture(Future.failed[HttpResponse](boom))))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 1)
                assert(rs(0).error.isDefined)
            }
        }

        "a truncated incremental stream (last payload still hasNext:true) ends with a terminal exception value" in {
            // Both delivered payloads promise more (hasNext:true) but the terminal
            // hasNext:false payload never arrives — the shape kyo-http hands apollo when a
            // @defer connection drops mid-stream (it maps the drop to a clean EOF, so the
            // protocol's hasNext is the only signal of incompleteness). The assembler must
            // append a terminal exception rather than present the partial data as final.
            val truncated =
                s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
                    """{"data":{"country":{"code":"DE"}},"hasNext":true}""" +
                    s"\r\n--$boundary\r\nContent-Type: application/json\r\n\r\n" +
                    """{"incremental":[{"data":{"capital":"Berlin"},"path":["country"]}],"hasNext":true}""" +
                    s"\r\n--$boundary--\r\n"
            val t = transport(engineOf(respond(200, contentType, truncated)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.size == 3) // the two delivered patches, then the truncation error
                assert(rs(0).data == Present(Data(Some(Loc("DE", None)))))
                assert(rs(1).data == Present(Data(Some(Loc("DE", Some("Berlin"))))))
                assert(rs(2).error.exists {
                    case _: ApolloNetworkException => true
                    case _                         => false
                })
            }
        }

        "a complete incremental stream (terminal hasNext:false) emits no truncation error" in {
            // The regular two-part body ends with hasNext:false, so awaitingMore is back to
            // false at end-of-stream and no terminal error is appended (guards the new
            // check against firing on a well-formed stream).
            val t = transport(engineOf(respond(200, contentType, multipart)))
            StreamProbe.collect(t.executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.forall(_.error.isEmpty))
            }
        }

        "a live body that drops mid-stream ends with a terminal exception value, not a silent truncation" in {
            // A Chunked engine whose body emits the initial part (flushed by the second
            // part's leading delimiter) then aborts — the shape of a real TCP drop after
            // the response head. The transport must fold that failure into a terminal
            // ApolloNetworkException appended after the already-emitted part, rather than
            // letting it escape as a panic (which would crash the caller) or vanish.
            val boom = new RuntimeException("mid-stream drop")
            val firstEmission =
                s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
                    """{"data":{"country":{"code":"DE"}},"hasNext":true}""" +
                    s"\r\n--$boundary\r\n" // the next delimiter flushes part 1 before the drop
            val streamingEngine = new HttpEngine:
                def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                    HttpResponse(200, List(HttpHeader("Content-Type", contentType)), "")
                override def executeStreaming(request: HttpRequest)(using Frame): HttpStreamResponse < (Async & Scope) =
                    val body = Stream.init(Seq(firstEmission)).concat(Stream.unwrap(Sync.defer(throw boom)))
                    HttpStreamResponse(200, List(HttpHeader("Content-Type", contentType)), HttpStreamBody.Chunked(body))
            StreamProbe.collect(transport(streamingEngine).executeStreaming(ApolloRequest(DeferQ()))).map { rs =>
                assert(rs.nonEmpty)
                assert(rs.head.data == Present(Data(Some(Loc("DE", None))))) // the part before the drop still arrives
                assert(rs.last.error.exists {
                    case _: ApolloNetworkException => true
                    case _                         => false
                })
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
                assert(plain(0).error.isDefined)
            end for
        }
    }
end DeferSpec
