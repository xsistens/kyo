package kyo.apollo.exception

import kyo.Absent
import kyo.Chunk
import kyo.KyoException
import kyo.Present
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.Uuid

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
                CacheMissException("User:1")
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
            assert(ex.getMessage.contains("Expected a GraphQL response object but got: [1]"))
            assert(ex.getCause == cause)
        }

        "parse exception: a large actual value is cut in the message, kept whole in the field" in {
            val body = "x" * 10000
            val ex   = ApolloParseException(Json.JStr(body), "a JSON document")
            assert(ex.actual == Json.JStr(body))
            assert(!ex.getMessage.contains("x" * 1000))
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

            "the parse row holds only the parse leaf, the cache-read row only the miss" in {
                typeCheck("""val e: ApolloParseFailure = ApolloParseException(Json.JNull, "x")""")
                typeCheckFailure("""val e: ApolloParseFailure = ApolloNetworkException()""")("ApolloParseFailure")
                typeCheck("""val e: CacheReadFailure = CacheMissException("User:1")""")
                typeCheckFailure("""val e: CacheReadFailure = ApolloNetworkException()""")("CacheReadFailure")
            }

            "an engine failure matches exhaustively on its single leaf" in {
                def describe(failure: HttpEngineFailure): String = failure match
                    case e: ApolloNetworkException => e.getClass.getSimpleName
                assert(describe(ApolloNetworkException()) == "ApolloNetworkException")
            }
        }

        "every leaf folds into an ApolloResponse.error value (no throw)" in {
            val id = Uuid.random()
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
