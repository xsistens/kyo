package kyo.apollo.exception

import kyo.Absent
import kyo.Chunk
import kyo.KyoException
import kyo.Present
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.FieldKey
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.TestIds

/** Tests the [[ApolloException]] hierarchy: every leaf is a `KyoException` (no stack
  * trace, the creation `Frame`), carries its typed payload, belongs to exactly the
  * operation traits whose rows it can appear on, and folds into an
  * [[ApolloResponse.error]] value.
  */
class ApolloExceptionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "ApolloException" - {

        "every leaf is a KyoException: no stack trace is captured" in {
            val leaves: List[ApolloException] = List(
                ApolloNetworkException(),
                ApolloHttpException(500, Nil, "boom"),
                ApolloParseException(Json.JNull, "an object"),
                ApolloWebSocketClosedException(1006),
                ApolloGraphQLException(Chunk.empty),
                DefaultApolloException(),
                ApolloConfigException("no serverUrl"),
                CacheMissException(CacheKey("User", "1")),
                NoCacheIdentityException("Country")
            )
            leaves.foreach { ex =>
                assert(ex.isInstanceOf[KyoException])
                assert(ex.getStackTrace.isEmpty)
            }
        }

        "network exception: default message, wraps cause" in {
            val cause = new RuntimeException("socket reset")
            val ex    = ApolloNetworkException(cause = cause)
            assert(ex.getMessage.contains("Failed to execute GraphQL HTTP request"))
            assert(ex.getCause == cause)
        }

        "http exception: carries status code and response headers" in {
            val headers = List(HttpHeader("Retry-After", "30"))
            val ex = ApolloHttpException(
                statusCode = 503,
                headers = headers,
                message = "service unavailable"
            )
            assert(ex.statusCode == 503)
            assert(ex.headers == headers)
            assert(ex.getMessage.contains("service unavailable"))
        }

        "parse exception: typed actual/expected fields build the message, the decoder error is the cause" in {
            val cause  = new IllegalStateException("not an object")
            val actual = Json.JArr(Chunk(Json.JNum(1)))
            val ex     = ApolloParseException(actual, "a GraphQL response object", cause)
            assert(ex.actual == actual)
            assert(ex.expected == "a GraphQL response object")
            assert(ex.message == "Expected a GraphQL response object but got an array")
            assert(ex.getMessage.contains("Expected a GraphQL response object but got an array"))
            assert(ex.getMessage.contains("IllegalStateException"))
            assert(ex.getCause == cause)
        }

        "parse exception: no text of it renders a value of actual or the cause's message, the field keeps it whole" in {
            // The values live in `ParseLeak`, away from these lines: a development-mode
            // `getMessage` quotes the source around the construction site.
            import ParseLeak.*
            val cases = Chunk(
                ApolloParseException(text, "a JSON document"),
                ApolloParseException(text, "a JSON document", cause),
                ApolloParseException(row, "an object", cause),
                ApolloParseException(numbers, "an object", cause)
            )
            cases.foreach { ex =>
                Chunk(ex.message, ex.getMessage, ex.toString).foreach { rendered =>
                    markers.foreach(marker => assert(!rendered.contains(marker), rendered))
                }
            }
            assert(cases.head.actual == text)
            assert(cases(1).getCause == cause)
            assert(ApolloParseException(Json.JNull, "x").message == "Expected x but got null")
            assert(ApolloParseException(Json.JDec(BigDecimal("1.5")), "x").message == "Expected x but got a number")
        }

        "websocket-closed exception: code without a reason" in {
            val ex = ApolloWebSocketClosedException(code = 1000)
            assert(ex.code == 1000)
            assert(ex.reason == None)
            assert(ex.getMessage.contains("WebSocket closed with code 1000"))
        }

        "websocket-closed exception: code with a reason renders both" in {
            val ex = ApolloWebSocketClosedException(1011, Some("internal error"))
            assert(ex.code == 1011)
            assert(ex.reason == Some("internal error"))
            assert(ex.getMessage.contains("WebSocket closed with code 1011: internal error"))
        }

        "cache miss: a whole-record miss names the record, a field miss the record and the field's storage key" in {
            val name   = FieldKey(CompiledField("name", CompiledNamedType("String")))
            val record = CacheMissException(CacheKey("User", "1"))
            val field  = CacheMissException(CacheKey("User", "1"), name)
            assert(record.key == CacheKey("User", "1") && record.fieldKey == Absent)
            assert(record.message == "Object 'User:1' not found in the cache")
            assert(field.fieldKey == Present(name))
            assert(field.message == "Object 'User:1' has no field named 'name' in the cache")
        }

        "no cache identity: names the type the key generator does not identify" in {
            val ex = NoCacheIdentityException("Country")
            assert(ex.typeName == "Country")
            assert(ex.message.contains("no identity for an object of type 'Country'"))
        }

        "default exception: catch-all message" in {
            assert(DefaultApolloException().getMessage.contains("Apollo operation failed"))
        }

        "operation traits: each leaf is on exactly the rows it can appear on" - {

            "execute: network, HTTP status, parse, websocket close, GraphQL errors" in {
                typeCheck("""val e: ApolloExecuteFailure = ApolloNetworkException()""")
                typeCheck("""val e: ApolloExecuteFailure = ApolloHttpException(500, Nil, "boom")""")
                typeCheck("""val e: ApolloExecuteFailure = ApolloParseException(Json.JNull, "x")""")
                typeCheck("""val e: ApolloExecuteFailure = ApolloWebSocketClosedException(1006)""")
                typeCheck("""val e: ApolloExecuteFailure = ApolloGraphQLException(Chunk.empty)""")
            }

            "a construction failure is on no operation's row" in {
                typeCheckFailure("""val e: ApolloExecuteFailure = ApolloConfigException("")""")("ApolloExecuteFailure")
                typeCheckFailure("""val e: HttpEngineFailure = ApolloConfigException("")""")("HttpEngineFailure")
                typeCheckFailure("""val e: ApolloParseFailure = ApolloConfigException("")""")("ApolloParseFailure")
                typeCheckFailure("""val e: CacheReadFailure = ApolloConfigException("")""")("CacheReadFailure")
            }

            "the engine row is only 'no response received': an HTTP status or a parse failure is not on it" in {
                typeCheck("""val e: HttpEngineFailure = ApolloNetworkException()""")
                typeCheckFailure("""val e: HttpEngineFailure = ApolloHttpException(500, Nil, "boom")""")("HttpEngineFailure")
                typeCheckFailure("""val e: HttpEngineFailure = ApolloParseException(Json.JNull, "x")""")("HttpEngineFailure")
            }

            "the parse row holds only the parse leaf, the cache-read row only the miss and the missing identity" in {
                typeCheck("""val e: ApolloParseFailure = ApolloParseException(Json.JNull, "x")""")
                typeCheckFailure("""val e: ApolloParseFailure = ApolloNetworkException()""")("ApolloParseFailure")
                typeCheck("""val e: CacheReadFailure = CacheMissException(CacheKey("User", "1"))""")
                typeCheck("""val e: CacheReadFailure = NoCacheIdentityException("Country")""")
                typeCheckFailure("""val e: CacheReadFailure = ApolloNetworkException()""")("CacheReadFailure")
                typeCheckFailure("""val e: ApolloExecuteFailure = CacheMissException(CacheKey("User", "1"))""")(
                    "ApolloExecuteFailure"
                )
            }

            "a cache-read failure matches exhaustively on its two leaves" in {
                def describe(failure: CacheReadFailure): String = failure match
                    case e: CacheMissException       => e.key.render
                    case e: NoCacheIdentityException => e.typeName
                assert(describe(CacheMissException(CacheKey("User", "1"))) == "User:1")
                assert(describe(NoCacheIdentityException("Country")) == "Country")
            }

            "an engine failure matches exhaustively on its single leaf" in {
                def describe(failure: HttpEngineFailure): String = failure match
                    case e: ApolloNetworkException => e.getClass.getSimpleName
                assert(describe(ApolloNetworkException()) == "ApolloNetworkException")
            }
        }

        "every leaf folds into an ApolloResponse.error value (no throw)" in {
            val id = TestIds.requestUuid
            val leaves: List[ApolloException] = List(
                ApolloNetworkException(),
                ApolloHttpException(500, Nil, "boom"),
                ApolloParseException(Json.JNull, "an object"),
                ApolloWebSocketClosedException(1006),
                DefaultApolloException()
            )
            leaves.foreach { ex =>
                val response = ApolloResponse.fromException[Nothing](id, ex)
                assert(response.error == Present(ex))
                assert(response.data == Absent)
                assert(response.hasErrors)
            }
        }
    }
end ApolloExceptionSpec

/** Payloads whose values must never reach a parse exception's text. */
private object ParseLeak:
    val markers: Chunk[String] = Chunk("secret-token-123", "4711", "true")

    val text: Json = Json.JStr("secret-token-123" * 100)

    val row: Json = Json.JObj(Map("token" -> Json.JStr("secret-token-123")))

    val numbers: Json = Json.JArr(Chunk(Json.JInt(4711L), Json.JBool(true)))

    val cause: Throwable = new IllegalArgumentException("rejected secret-token-123")
end ParseLeak
