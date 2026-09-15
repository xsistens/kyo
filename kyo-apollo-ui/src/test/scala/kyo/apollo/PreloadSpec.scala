package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.network.http.HttpEngine

/** `Apollo.preload` / `PreloadedQuery` — the `preloadQuery` + `useReadQuery`
  * analog: the fetch starts at preload time (parallel loaders, no waterfall),
  * `read` suspends until the first data, `state` never suspends.
  */
class PreloadSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Gated engine that counts how many requests are IN FLIGHT before release. */
    final private class CountingGatedEngine(body: String) extends HttpEngine:
        private given Frame = Frame.internal
        private val unsafe  = AllowUnsafe.embrace.danger
        private val gate    = Fiber.Promise.Unsafe.init[Unit, Any]()(using unsafe).safe
        private val arrived = AtomicInt.Unsafe.init(0)(using unsafe).safe

        /** How many requests have reached the engine so far. */
        def started(using Frame): Int < Sync = arrived.get

        /** Release the gate so every parked (and future) reply resolves. */
        def release(using Frame): Unit < Sync = gate.completeUnitDiscard

        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            arrived.incrementAndGet.andThen(gate.get).andThen(HttpEngine.response(HttpStatus.OK, body))
    end CountingGatedEngine

    "Apollo.preload" - {

        "starts the fetch at preload time; read then yields the data" in {
            val engine = new CountingGatedEngine(body("Alice"))
            for
                client    <- cached(engine)
                preloaded <- Apollo.preload(client.query(CurrentUserQuery()))
                // The fetch left before anyone read; `state` answers without suspending. Waiting on the
                // observable count rather than a time budget: how long the preload fiber needs to reach
                // the engine is a scheduling question, and under the suite's parallelism a fixed sleep
                // is a coin flip.
                _       <- assertEventually(engine.started.map(_ == 1))
                pending <- preloaded.state.current
                _ = assert(pending == QueryState.Loading)
                _    <- engine.release
                sig  <- preloaded.read(UpdateFailure.Notify)
                data <- sig.current
            yield assert(data == userData("Alice"))
            end for
        }

        "two preloads run their fetches in parallel — no waterfall" in {
            val engine = new CountingGatedEngine(body("Alice"))
            for
                client <- cached(engine)
                first  <- Apollo.preload(client.query(CurrentUserQuery()))
                second <- Apollo.preload(client.query(CurrentUserQuery()))
                // Both left before EITHER was read — the loader shape, not a waterfall. The count is
                // waited on rather than slept for: the claim is that neither fetch waits for the other,
                // not that both finish inside a fixed budget.
                _  <- assertEventually(engine.started.map(_ == 2))
                _  <- engine.release
                s1 <- first.read(UpdateFailure.Notify)
                s2 <- second.read(UpdateFailure.Notify)
                d1 <- s1.current
                d2 <- s2.current
            yield
                assert(d1 == userData("Alice"))
                assert(d2 == userData("Alice"))
            end for
        }

        "read aborts on the Apollo channel when the first settle is a failure" in {
            val engine = new CountingGatedEngine("""{"data":null,"errors":[{"message":"boom"}]}""")
            for
                client    <- cached(engine)
                preloaded <- Apollo.preload(client.query(CurrentUserQuery()))
                _         <- engine.release
                outcome   <- Abort.run[ApolloException](preloaded.read(UpdateFailure.Notify))
            yield outcome match
                case Result.Failure(ex) => assert(ex.message == "boom")
                case other              => fail(s"expected the first-settle failure, got $other")
            end for
        }
    }
end PreloadSpec
