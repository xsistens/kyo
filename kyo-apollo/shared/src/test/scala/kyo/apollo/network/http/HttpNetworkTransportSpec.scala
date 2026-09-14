package kyo.apollo.network.http

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Query
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.TestIds
import scala.collection.immutable.VectorMap

/** Tests [[HttpNetworkTransport]]'s decode/error-mapping against a fake
  * [[HttpEngine]] — no network. Covers the "failures are values" contract:
  * success lifts to typed data, while non-2xx / malformed body / the engine's
  * failure row all land in `ApolloResponse.error` rather than being thrown — and
  * its limit: only the engine's row is folded, so a decoder defect or an interrupt
  * stays a panic instead of posing as a network failure.
  */
class HttpNetworkTransportSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The `{ "value": Int }` object shape the query's `data` decodes from; the
      * operation's `D` stays `Int` by transforming this derived object schema.
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

    /** The same query with a defective codec: decoding a well-formed payload throws. */
    final case class DefectiveQuery() extends Query.Normalizable[Int]:
        def name: String             = "Value"
        def document: String         = "query Value { value }"
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap.empty)
        def dataCodec: JsonCodec[Int] = new JsonCodec[Int]:
            def decode(json: Json)(using Frame): Result[ApolloParseException, Int] = throw new ClassCastException("defective codec")
            def encode(value: Int): Json                                           = Json.JNull
    end DefectiveQuery

    /** An [[HttpEngine]] that runs `respond` for every request. */
    private def engineOf(respond: => HttpEngine.Response < (Async & Abort[HttpEngineFailure])): HttpEngine =
        new HttpEngine:
            def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) = respond

    /** An [[HttpEngine]] that always yields the same canned response. */
    private def engineReturning(response: HttpEngine.Response): HttpEngine = engineOf(response)

    private def transport(engine: HttpEngine): HttpNetworkTransport =
        HttpNetworkTransport("https://example.com/graphql", engine)

    "HttpNetworkTransport" - {

        "2xx with a valid envelope lifts to typed data, no exception" in {
            val engine = engineReturning(HttpEngine.response(HttpStatus.OK, """{"data":{"value":42}}"""))
            transport(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Present(42))
                assert(response.error == Absent)
                assert(!response.hasErrors)
            }
        }

        "non-2xx status becomes an ApolloHttpException value" in {
            val headers = HttpHeaders.empty.add("Retry-After", "5")
            val engine  = engineReturning(HttpEngine.response(HttpStatus(503), "service down", headers))
            transport(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
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
            val engine = engineReturning(HttpEngine.response(HttpStatus.OK, "not json at all"))
            transport(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
            }
        }

        "shape-invalid envelope (JSON array) becomes an ApolloParseException" in {
            val engine = engineReturning(HttpEngine.response(HttpStatus.OK, "[1,2,3]"))
            transport(engine).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.error.exists(_.isInstanceOf[ApolloParseException]))
            }
        }

        "the engine's Abort failure becomes that failure as the response's error value" in {
            val refused = ApolloNetworkException("ECONNREFUSED")
            transport(engineOf(Abort.fail(refused))).execute(ApolloRequest(ValueQuery(), TestIds.requestUuid)).map { response =>
                assert(response.data == Absent)
                assert(response.error.exists(_ eq refused))
            }
        }

        "a decoder defect on a 2xx body stays a panic, not a network or parse value" in {
            val engine = engineReturning(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            Abort.run[Throwable](transport(engine).execute(ApolloRequest(DefectiveQuery(), TestIds.requestUuid))).map {
                case Result.Panic(e) => assert(e.isInstanceOf[ClassCastException])
                case other           => fail(s"expected the codec's ClassCastException as a panic, got $other")
            }
        }

        "an interrupt raised by the engine stays a panic, not a network value" in {
            val interrupted = Interrupted(summon[Frame])
            Abort.run[Throwable](transport(engineOf(Abort.panic(interrupted))).execute(ApolloRequest(
                ValueQuery(),
                TestIds.requestUuid
            ))).map {
                case Result.Panic(e) => assert(e eq interrupted)
                case other           => fail(s"expected the Interrupted panic, got $other")
            }
        }

        "streaming: a decoder defect stays a panic too" in {
            val engine = engineReturning(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            Abort.run[Throwable](StreamProbe.collect(transport(engine).executeStreaming(ApolloRequest(
                DefectiveQuery(),
                TestIds.requestUuid
            )))).map {
                case Result.Panic(e) => assert(e.isInstanceOf[ClassCastException])
                case other           => fail(s"expected the codec's ClassCastException as a panic, got $other")
            }
        }

        "a server URL that does not parse is an ApolloNetworkException value with kyo-http's parse failure as cause, and nothing is sent" in {
            for
                sent <- AtomicInt.init
                broken = HttpNetworkTransport(
                    "",
                    engineOf(sent.incrementAndGet.andThen(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}""")))
                )
                single    <- broken.execute(ApolloRequest(ValueQuery(), TestIds.requestUuid))
                streaming <- StreamProbe.collect(broken.executeStreaming(ApolloRequest(ValueQuery(), TestIds.requestUuid)))
                calls     <- sent.get
            yield
                val errors = single.error.toList ++ streaming.flatMap(_.error.toList)
                assert(streaming.size == 1, s"expected one terminal response, got $streaming")
                assert(errors.size == 2, s"expected an error on both paths, got $errors")
                errors.foreach {
                    case e: ApolloNetworkException =>
                        assert(e.getCause.isInstanceOf[HttpUrlParseException], s"cause: ${e.getCause}")
                    case other => fail(s"expected an ApolloNetworkException, got $other")
                }
                assert(calls == 0, s"a request with no parseable URL reached the engine $calls time(s)")
            end for
        }

        "the request uuid is echoed onto the response" in {
            val request = ApolloRequest(ValueQuery(), TestIds.requestUuid)
            val engine  = engineReturning(HttpEngine.response(HttpStatus.OK, """{"data":{"value":1}}"""))
            transport(engine).execute(request).map { response =>
                assert(response.requestUuid == request.requestUuid)
            }
        }
    }
end HttpNetworkTransportSpec
