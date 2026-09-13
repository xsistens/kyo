package kyo.apollo.network.http

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpHeader
import scala.collection.immutable.VectorMap
import scala.concurrent.Future

/** Tests [[HttpNetworkTransport]]'s decode/error-mapping against a fake
  * [[HttpEngine]] — no network. Covers the "failures are values" contract:
  * success lifts to typed data, while non-2xx / malformed body / connection
  * error all land in `ApolloResponse.error` rather than being thrown.
  */
class HttpNetworkTransportSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape the query's `data` decodes from; the
      * operation's `D` stays `Int` by transforming this derived object schema.
      */
    final case class ValueData(value: Int) derives Schema

    /** A query whose `data` is a single `{ "value": Int }` object. */
    final case class ValueQuery() extends Query[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        def dataSchema: Schema[Int] =
            summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply)
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    /** An [[HttpEngine]] that always yields the same canned response. */
    private def engineReturning(response: HttpResponse): HttpEngine =
        new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async = response

    /** An [[HttpEngine]] that always fails (a connection error) — via the SAME
      * async-rejection path the production [[FetchHttpEngine]] uses
      * (`Async.fromFuture` of a rejected `fetch`), so this actually guards how a
      * real network drop is folded rather than a synthetic synchronous throw.
      */
    private def engineFailing(cause: Throwable): HttpEngine =
        new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                Async.fromFuture(Future.failed(cause))

    private def transport(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport("https://example.com/graphql", engine)

    "HttpNetworkTransport" - {

        "2xx with a valid envelope lifts to typed data, no exception" in {
            val engine = engineReturning(HttpResponse(200, Nil, """{"data":{"value":42}}"""))
            transport(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Present(42))
                assert(response.error == Absent)
                assert(!response.hasErrors)
            }
        }

        "non-2xx status becomes an ApolloHttpException value" in {
            val headers = List(HttpHeader("Retry-After", "5"))
            val engine  = engineReturning(HttpResponse(503, headers, "service down"))
            transport(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                response.error match
                    case Present(e: ApolloHttpException) =>
                        assert(e.statusCode == 503)
                        assert(e.headers == headers)
                    case other => fail(s"expected ApolloHttpException, got $other")
                end match
                assert(response.hasErrors)
            }
        }

        "malformed body (not JSON) becomes an ApolloParseException value" in {
            val engine = engineReturning(HttpResponse(200, Nil, "not json at all"))
            transport(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
            }
        }

        "shape-invalid envelope (JSON array) becomes an ApolloParseException" in {
            val engine = engineReturning(HttpResponse(200, Nil, "[1,2,3]"))
            transport(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
            }
        }

        "connection error becomes an ApolloNetworkException value" in {
            val engine = engineFailing(new RuntimeException("ECONNREFUSED"))
            transport(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[ApolloNetworkException]))
            }
        }

        "the request uuid is echoed onto the response" in {
            val request = ApolloRequest(ValueQuery())
            val engine  = engineReturning(HttpResponse(200, Nil, """{"data":{"value":1}}"""))
            transport(engine).execute(request).map { response =>
                assert(response.requestUuid == request.requestUuid)
            }
        }
    }
end HttpNetworkTransportSpec
