package kyo.apollo.exception

import kyo.Absent
import kyo.Present
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.Uuid

/** Tests the completed [[ApolloException]] hierarchy (Phase 03 Task 4): every
  * concrete subtype constructs, carries its payload/message, is an
  * `ApolloException`, and folds into an [[ApolloResponse.exception]] value —
  * the "failures are values" contract from the Task 1 design (§3).
  */
class ApolloExceptionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "ApolloException" - {

        "network exception: default message, wraps cause, is an ApolloException" in {
            val cause = new RuntimeException("socket reset")
            val ex    = ApolloNetworkException(cause = cause)
            assert(ex.isInstanceOf[ApolloException])
            assert(ex.getMessage == "Failed to execute GraphQL HTTP request")
            assert(ex.getCause == cause)
        }

        "http exception: carries status code and response headers" in {
            val headers = List(HttpHeader("Retry-After", "30"))
            val ex = ApolloHttpException(
                statusCode = 503,
                headers = headers,
                message = "service unavailable"
            )
            assert(ex.isInstanceOf[ApolloException])
            assert(ex.statusCode == 503)
            assert(ex.headers == headers)
            assert(ex.getMessage == "service unavailable")
        }

        "parse exception: default message, wraps the thrown parse error" in {
            val cause = new IllegalStateException("not an object")
            val ex    = ApolloParseException(cause = cause)
            assert(ex.getMessage == "Failed to parse GraphQL HTTP response body")
            assert(ex.getCause == cause)
        }

        "websocket-closed exception: code without a reason" in {
            val ex = ApolloWebSocketClosedException(code = 1000)
            assert(ex.isInstanceOf[ApolloException])
            assert(ex.code == 1000)
            assert(ex.reason == None)
            assert(ex.getMessage == "WebSocket closed with code 1000")
        }

        "websocket-closed exception: code with a reason renders both" in {
            val ex = ApolloWebSocketClosedException(1011, Some("internal error"))
            assert(ex.code == 1011)
            assert(ex.reason == Some("internal error"))
            assert(ex.getMessage == "WebSocket closed with code 1011: internal error")
        }

        "default exception: catch-all message, is an ApolloException" in {
            val ex = DefaultApolloException()
            assert(ex.isInstanceOf[ApolloException])
            assert(ex.getMessage == "Apollo operation failed")
        }

        "every subtype folds into an ApolloResponse.exception value (no throw)" in {
            val id = Uuid.random()
            val subtypes: List[ApolloException] = List(
                ApolloNetworkException(),
                ApolloHttpException(500, Nil, "boom"),
                ApolloParseException(),
                ApolloWebSocketClosedException(1006),
                DefaultApolloException()
            )
            subtypes.foreach { ex =>
                val response = ApolloResponse.fromException[Nothing](id, ex)
                assert(response.exception == Present(ex))
                assert(response.data == Absent)
                // hasErrors folds in a present exception, so a failure response reports it
                assert(response.hasErrors)
            }
        }
    }
end ApolloExceptionSpec
