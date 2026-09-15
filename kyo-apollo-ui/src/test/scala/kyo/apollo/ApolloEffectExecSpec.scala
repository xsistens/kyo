package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.normalizedStore
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.network.ws.FakeWebSocketConnection
import kyo.apollo.network.ws.FakeWebSocketEngine
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WsTestSupport

/** Task 8 — the effect form driven to completion, now on kyo-test. Each leaf
  * body IS the effect: the runner executes it, so there is no `KyoRun` bridge.
  * `.data`'s typed `Abort` outcome is surfaced with `Abort.run`.
  */
class ApolloEffectExecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "client construction (Apollo.client / .clientLayer)" - {

        "a Config without a serverUrl does not compile (no runtime configuration failure left)" in {
            typeCheck("""Apollo.client(ApolloClient.Config("https://example.com/graphql"))""")
            typeCheckFailure("""Apollo.client(ApolloClient.Config())""")("serverUrl")
        }

        "client creates a client on the Scope from a valid config" in {
            for client <- Apollo.client(ApolloClient.Config("https://example.com/graphql"))
            yield assert(client.normalizedStore.isEmpty)
        }
    }

    "effect form (.data / .response)" - {

        ".data yields the typed data on a clean success" in {
            for
                client <- cacheless(StaticEngine(body("Alice")))
                result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Success(data) => assert(data == userData("Alice"))
                case other                => fail(s"expected Success(Alice), got $other")
        }

        ".data folds GraphQL errors onto the Abort channel" in {
            for
                client <- cacheless(StaticEngine(partialBody("Alice", "boom")))
                result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Failure(ex: ApolloGraphQLException) =>
                    // Apollo JS parity: the bare error message, no prefix.
                    assert(ex.message == "boom")
                case other => fail(s"expected Abort(ApolloGraphQLException), got $other")
        }

        ".data folds a transport (HTTP) exception onto the Abort channel" in {
            for
                client <- cacheless(StaticEngine("""{"errors":[]}""", status = 500))
                result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Failure(_: ApolloHttpException) => assert(true)
                case other                                  => fail(s"expected Abort(ApolloHttpException), got $other")
        }

        ".response preserves partial data alongside GraphQL errors (no Abort)" in {
            for
                client <- cacheless(StaticEngine(partialBody("Alice", "half")))
                resp   <- client.query(CurrentUserQuery()).response
            yield
                assert(resp.data == Present(userData("Alice")))
                assert(resp.errors.nonEmpty)
                assert(ApolloSignal.project(resp) == QueryState.PartialData(userData("Alice"), resp.errors))
            end for
        }

        "ApolloClientResource closes the client's subscription socket on Scope exit" in {
            val conn = new FakeWebSocketConnection
            val config = ApolloClient.Config("wss://example.invalid/graphql")
                .webSocketEngine(new FakeWebSocketEngine(conn))
            for _ <- Scope.run {
                for
                    client <- ApolloClientResource.acquire(config)
                    // Open a subscription so a live socket exists (opened lazily on
                    // consume), and wait until its handshake went out.
                    _ <- Fiber.init(client.subscription(WsTestSupport.ValueSubscription()).stream.discard)
                    _ <- conn.awaitSent(_ == """{"type":"connection_init"}""")
                yield assert(conn.closedWith.isEmpty)
            }
            // Exiting the Scope closed the client, and with it the socket.
            yield assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }
    }
end ApolloEffectExecSpec
