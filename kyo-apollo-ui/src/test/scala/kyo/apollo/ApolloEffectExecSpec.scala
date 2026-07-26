package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.exception.ApolloConfigException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloHttpException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ws.FakeWebSocketConnection
import kyo.apollo.network.ws.FakeWebSocketEngine
import kyo.apollo.network.ws.ManualWsScheduler
import kyo.apollo.network.ws.WebSocketConnection
import kyo.apollo.network.ws.WsTestSupport

/** Task 8 — the effect form driven to completion, now on kyo-test. Each leaf
  * body IS the effect: the runner executes it, so there is no `KyoRun` bridge.
  * `.data`'s typed `Abort` outcome is surfaced with `Abort.run`.
  */
class ApolloEffectExecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "client construction (Apollo.client / .clientLayer)" - {

        "init without a serverUrl aborts with ApolloConfigException (no throw)" in {
            for result <- Scope.run(Abort.run[ApolloConfigException](Apollo.client(identity)))
            yield result match
                case Result.Failure(_: ApolloConfigException) => assert(true)
                case other                                    => fail(s"expected Abort(ApolloConfigException), got $other")
        }

        "init builds and acquires a client on the Scope for a valid config" in {
            for result <- Scope.run(
                    Abort.run[ApolloConfigException](
                        Apollo.client(_.serverUrl("https://example.com/graphql")).map(_ => true)
                    )
                )
            yield result match
                case Result.Success(true) => assert(true)
                case other                => fail(s"expected Success(true), got $other")
        }
    }

    "effect form (.data / .response)" - {

        ".data yields the typed data on a clean success" in {
            val client = cacheless(StaticEngine(body("Alice")))
            for result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Success(data) => assert(data == userData("Alice"))
                case other                => fail(s"expected Success(Alice), got $other")
        }

        ".data folds GraphQL errors onto the Abort channel" in {
            val client = cacheless(StaticEngine(partialBody("Alice", "boom")))
            for result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Failure(ex: ApolloGraphQLException) =>
                    // Apollo JS parity: the bare error message, no prefix.
                    assert(ex.getMessage == "boom")
                case other => fail(s"expected Abort(ApolloGraphQLException), got $other")
        }

        ".data folds a transport (HTTP) exception onto the Abort channel" in {
            val client = cacheless(StaticEngine("""{"errors":[]}""", status = 500))
            for result <- Abort.run(client.query(CurrentUserQuery()).data)
            yield result match
                case Result.Failure(_: ApolloHttpException) => assert(true)
                case other                                  => fail(s"expected Abort(ApolloHttpException), got $other")
        }

        ".response preserves partial data alongside GraphQL errors (no Abort)" in {
            val client = cacheless(StaticEngine(partialBody("Alice", "half")))
            for resp <- client.query(CurrentUserQuery()).response
            yield
                assert(resp.data == Present(userData("Alice")))
                assert(resp.errors.nonEmpty)
                assert(ApolloSignal.project(resp) == QueryState.PartialData(userData("Alice"), resp.errors))
            end for
        }

        "ApolloClientResource closes the client's subscription socket on Scope exit" in {
            val conn = new FakeWebSocketConnection
            val client = ApolloClient
                .builder()
                .serverUrl("wss://example.invalid/graphql")
                .webSocketEngine(new FakeWebSocketEngine(conn))
                .webSocketScheduler(new ManualWsScheduler)
                .build()
            // Open a subscription so a live socket exists (opened lazily on consume);
            // the open Future resolves on a microtask — an Async.sleep lets it settle.
            for
                _ <- Fiber.init(
                    Scope.run(client.subscription(WsTestSupport.ValueSubscription()).stream.discard)
                )
                _ <- Async.sleep(20L.millis)
                _ = assert(conn.closedWith.isEmpty)
                // Acquire the (already-open) client in a Scope; exiting the Scope runs
                // the registered release, i.e. `client.close()`.
                _ <- Scope.run(ApolloClientResource.acquire(client).map(_ => ()))
            yield assert(conn.closedWith.map(_._1) == Some(WebSocketConnection.NormalClosure))
            end for
        }
    }
end ApolloEffectExecSpec
