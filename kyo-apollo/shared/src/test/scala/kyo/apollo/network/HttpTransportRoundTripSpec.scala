package kyo.apollo.network

import kyo.*
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.json.Json
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpNetworkTransport
import kyo.apollo.network.http.HttpRequestBody
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
    final case class ValueQuery() extends Query.Normalizable[Int]:
        def name: String     = "Value"
        def document: String = "query Value { value }"
        val dataCodec: JsonCodec[Int] =
            JsonCodec.fromSchema(using summon[Schema[ValueData]].transform[Int](_.value)(ValueData.apply))
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end ValueQuery

    private val url = "https://example.com/graphql"

    /** An [[HttpEngine]] that records the request it is handed and replies with a
      * canned response — the "fake transport" the unit tests run against.
      */
    final private class CapturingEngine(response: HttpEngine.Response) extends HttpEngine:
        var lastRequest: Maybe[HttpEngine.Request] = Absent
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            lastRequest = Present(request)
            response
    end CapturingEngine

    private def transportWith(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport(url, engine)

    "HttpTransportRoundTrip" - {

        // --- Composer output, as emitted by the transport ---------------------

        "default POST sends the JSON body the composer produced" in {
            val engine = CapturingEngine(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.method == HttpMethod.POST)
                assert(sent.url.full == url)
                assert(
                    sent.fields.body ==
                        HttpRequestBody.Text("""{"query":"query Value { value }","operationName":"Value","variables":{}}""")
                )
                // Content-Type / Accept defaults land ahead of any per-request header.
                assert(
                    sent.headers.foldLeft(Chunk.empty[String])((names, name, _) => names.append(name)).headMaybe == Present("Content-Type")
                )
                assert(sent.headers.get("Content-Type") == Present("application/json"))
            }
        }

        "a GET request carries no body and encodes the query in the URL" in {
            val engine  = CapturingEngine(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            val request = ApolloRequest(ValueQuery(), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            transportWith(engine).execute(request).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.method == HttpMethod.GET)
                assert(sent.fields.body == HttpRequestBody.Empty)
                assert(sent.url.full.startsWith(url + "?query="))
            }
        }

        "a per-request header rides through to the wire request" in {
            val engine = CapturingEngine(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            val request =
                ApolloRequest(ValueQuery(), TestIds.requestUuid, httpHeaders = HttpHeaders.empty.add("Authorization", "Bearer t"))
            transportWith(engine).execute(request).map { _ =>
                val sent = engine.lastRequest.getOrElse(fail("engine never called"))
                assert(sent.headers.getAll("Authorization").contains("Bearer t"))
            }
        }

        // --- ApolloResponse construction from mocked HTTP responses ------------

        "success: a 200 envelope decodes to typed data with no errors/exception" in {
            val engine = CapturingEngine(HttpEngine.response(HttpStatus.OK, """{"data":{"value":42}}"""))
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
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
                HttpEngine.response(HttpStatus.OK, """{"data":null,"errors":[{"message":"boom"}]}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Absent)
                assert(response.errors.map(_.message) == Chunk("boom"))
                assert(response.error.exists(_.isInstanceOf[ApolloGraphQLException]))
                assert(response.hasErrors)
                assert(!response.hasTransportError)
            }
        }

        "partial result: `data` and `errors` are both carried through" in {
            val engine = CapturingEngine(
                HttpEngine.response(HttpStatus.OK, """{"data":{"value":7},"errors":[{"message":"partial"}]}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Present(7))
                assert(response.errors.map(_.message) == Chunk("partial"))
                assert(!response.hasTransportError)
            }
        }

        "top-level `extensions` are passed through onto the ApolloResponse" in {
            val engine = CapturingEngine(
                HttpEngine.response(HttpStatus.OK, """{"data":{"value":1},"extensions":{"cost":3}}""")
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.extensions == Map[String, Json]("cost" -> Json.JInt(3)))
            }
        }

        "a non-2xx application/graphql-response+json body is parsed as the GraphQL response it is" in {
            // GraphQL-over-HTTP: that media type promises a well-formed envelope whatever
            // the status, so a 400 carrying typed `errors` must not be flattened into an
            // opaque ApolloHttpException.
            val engine = CapturingEngine(
                HttpEngine.response(
                    HttpStatus(400),
                    """{"data":null,"errors":[{"message":"unknown field `nope`"}]}""",
                    HttpHeaders.empty.add("Content-Type", "application/graphql-response+json; charset=utf-8")
                )
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.errors.map(_.message) == Chunk("unknown field `nope`"))
                assert(response.error.exists(_.isInstanceOf[ApolloGraphQLException]))
                assert(!response.hasTransportError)
            }
        }

        "a non-2xx graphql-response+json body that does not parse falls back to the status" in {
            val engine = CapturingEngine(
                HttpEngine.response(
                    HttpStatus(503),
                    "<html>gateway down</html>",
                    HttpHeaders.empty.add("Content-Type", "application/graphql-response+json")
                )
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.error.exists(_.isInstanceOf[ApolloHttpException]))
                assert(response.hasTransportError)
            }
        }

        "a non-2xx application/json body is NOT parsed — the status says nothing about it" in {
            val engine = CapturingEngine(
                HttpEngine.response(
                    HttpStatus(400),
                    """{"data":null,"errors":[{"message":"legacy"}]}""",
                    HttpHeaders.empty.add("Content-Type", "application/json")
                )
            )
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.error.exists(_.isInstanceOf[ApolloHttpException]))
                assert(response.errors.isEmpty)
            }
        }

        "HTTP error: a non-2xx status folds into an ApolloHttpException value" in {
            val headers = HttpHeaders.empty.add("Retry-After", "5")
            val engine  = CapturingEngine(HttpEngine.response(HttpStatus(500), "internal error", headers))
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
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
            val engine = CapturingEngine(HttpEngine.response(HttpStatus.OK, "not json at all"))
            transportWith(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
                // A parse failure is a value — the effect completed successfully.
                assert(response.hasErrors)
            }
        }

        "the originating request uuid is echoed onto the response" in {
            val request = ApolloRequest(ValueQuery(), TestIds.requestUuid)
            val engine  = CapturingEngine(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            transportWith(engine).execute(request).map { response =>
                assert(response.requestUuid == request.requestUuid)
            }
        }
    }
end HttpTransportRoundTripSpec
