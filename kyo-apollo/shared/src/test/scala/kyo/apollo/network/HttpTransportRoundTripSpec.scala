package kyo.apollo.network

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpNetworkTransport
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse
import scala.collection.immutable.VectorMap

/** End-to-end `ApolloRequest` → composed `HttpRequest` → mocked `HttpResponse`
  * → `ApolloResponse` round-trip, driven through a real [[HttpNetworkTransport]]
  * wired to a **capturing fake [[HttpEngine]]** — the no-network "fake
  * transport" seam. No socket is ever opened.
  *
  * Complements the finer-grained `network/http` specs by exercising both ends
  * together and by covering the cases they don't: asserting the exact wire
  * request the transport emits for GET vs POST, and — crucially — the
  * distinction the "failures are values" contract draws between GraphQL
  * `errors` (a valid 200 envelope: `errors` populated, `exception` still
  * `None`) and a transport/parse `exception`.
  */
class HttpTransportRoundTripSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape the query's `data` decodes from; `D`
      * stays `Int` by transforming this derived object schema.
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

    private val url = "https://example.com/graphql"

    /** An [[HttpEngine]] that records the request it is handed and replies with a
      * canned response — the "fake transport" the unit tests run against.
      */
    final private class CapturingEngine(response: HttpResponse) extends HttpEngine:
        var lastRequest: Option[HttpRequest] = None
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            lastRequest = Some(request)
            response
    end CapturingEngine

    private def transportWith(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport(url, engine)

    "HttpTransportRoundTrip" - {

        // --- Composer output, as emitted by the transport ---------------------

        "default POST sends the JSON body the composer produced" in {
            val engine = CapturingEngine(HttpResponse(200, Nil, """{"data":{"value":1}}"""))
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.method == HttpMethod.Post)
                assert(sent.url == url)
                assert(
                    sent.body ==
                        Some("""{"query":"query Value { value }","operationName":"Value","variables":{}}""")
                )
                // Content-Type / Accept defaults land ahead of any per-request header.
                assert(sent.headers.head == HttpHeader("Content-Type", "application/json"))
            }
        }

        "a GET request carries no body and encodes the query in the URL" in {
            val engine  = CapturingEngine(HttpResponse(200, Nil, """{"data":{"value":1}}"""))
            val request = ApolloRequest.builder(ValueQuery()).httpMethod(HttpMethod.Get).build()
            transportWith(engine).execute(request).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.method == HttpMethod.Get)
                assert(sent.body == None)
                assert(sent.url.startsWith(url + "?query="))
            }
        }

        "a per-request header rides through to the wire request" in {
            val engine = CapturingEngine(HttpResponse(200, Nil, """{"data":{"value":1}}"""))
            val request = ApolloRequest
                .builder(ValueQuery())
                .addHttpHeader("Authorization", "Bearer t")
                .build()
            transportWith(engine).execute(request).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.headers.contains(HttpHeader("Authorization", "Bearer t")))
            }
        }

        // --- ApolloResponse construction from mocked HTTP responses ------------

        "success: a 200 envelope decodes to typed data with no errors/exception" in {
            val engine = CapturingEngine(HttpResponse(200, Nil, """{"data":{"value":42}}"""))
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Present(42))
                assert(response.errors == Chunk.empty)
                assert(response.error == Absent)
                assert(!response.hasErrors)
            }
        }

        "GraphQL errors on a 200 land on the error channel, not as a transport failure" in {
            // A valid envelope carrying only errors is a value, not a throw. It shares the
            // one error channel with transport failures but stays distinguishable from
            // them by its type — which is what `hasTransportError` reads.
            val engine = CapturingEngine(
                HttpResponse(200, Nil, """{"data":null,"errors":[{"message":"boom"}]}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                assert(response.errors.map(_.message) == Chunk("boom"))
                assert(response.error.exists(_.isInstanceOf[ApolloGraphQLException]))
                assert(response.hasErrors)
                assert(!response.hasTransportError)
            }
        }

        "partial result: `data` and `errors` are both carried through" in {
            val engine = CapturingEngine(
                HttpResponse(200, Nil, """{"data":{"value":7},"errors":[{"message":"partial"}]}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Present(7))
                assert(response.errors.map(_.message) == Chunk("partial"))
                assert(!response.hasTransportError)
            }
        }

        "top-level `extensions` are passed through onto the ApolloResponse" in {
            val engine = CapturingEngine(
                HttpResponse(200, Nil, """{"data":{"value":1},"extensions":{"cost":3}}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.extensions == Map[String, Json]("cost" -> Json.JNum(3.0)))
            }
        }

        "HTTP error: a non-2xx status folds into an ApolloHttpException value" in {
            val headers = List(HttpHeader("Retry-After", "5"))
            val engine  = CapturingEngine(HttpResponse(500, headers, "internal error"))
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                response.error match
                    case Present(e: ApolloHttpException) =>
                        assert(e.statusCode == 500)
                        assert(e.headers == headers)
                    case other => fail(s"expected ApolloHttpException, got $other")
                end match
                assert(response.hasErrors)
            }
        }

        "malformed body: a parse failure folds into an ApolloParseException value" in {
            val engine = CapturingEngine(HttpResponse(200, Nil, "not json at all"))
            transportWith(engine).execute(ApolloRequest(ValueQuery())).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
                // A parse failure is a value — the effect completed successfully.
                assert(response.hasErrors)
            }
        }

        "the originating request uuid is echoed onto the response" in {
            val request = ApolloRequest(ValueQuery())
            val engine  = CapturingEngine(HttpResponse(200, Nil, """{"data":{"value":1}}"""))
            transportWith(engine).execute(request).map { response =>
                assert(response.requestUuid == request.requestUuid)
            }
        }
    }
end HttpTransportRoundTripSpec
