package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** `Apollo.preload` / `PreloadedQuery` — the `preloadQuery` + `useReadQuery`
  * analog: the fetch starts at preload time (parallel loaders, no waterfall),
  * `read` suspends until the first data, `state` never suspends.
  */
class PreloadSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Gated engine that counts how many requests are IN FLIGHT before release. */
    final private class CountingGatedEngine(body: String) extends HttpEngine:
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private given Frame       = Frame.internal
        @volatile var started     = 0
        private val gate: Fiber.Promise[Unit, Any] =
            Sync.Unsafe.evalOrThrow(Fiber.Promise.init[Unit, Any])
        def release(): Unit =
            given AllowUnsafe = AllowUnsafe.embrace.danger
            discard(gate.unsafe.completeUnitDiscard())
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            started += 1
            gate.get.andThen(HttpResponse(200, Nil, body))
    end CountingGatedEngine

    "Apollo.preload" - {

        "starts the fetch at preload time; read then yields the data" in {
            val engine         = new CountingGatedEngine(body("Alice"))
            given ApolloClient = cached(engine)
            Scope.run {
                for
                    preloaded <- Apollo.preload(
                        summon[ApolloClient].query(CurrentUserQuery())
                    )
                    // The fetch left before anyone read; `state` answers without suspending.
                    _       <- Async.sleep(30L.millis)
                    pending <- preloaded.state.current
                    _ = assert(engine.started == 1)
                    _ = assert(pending == QueryState.Loading)
                    _    <- Sync.defer(engine.release())
                    sig  <- preloaded.read(UpdateFailure.Notify)
                    data <- sig.current
                yield assert(data == userData("Alice"))
            }
        }

        "two preloads run their fetches in parallel — no waterfall" in {
            val engine         = new CountingGatedEngine(body("Alice"))
            given ApolloClient = cached(engine)
            Scope.run {
                for
                    first  <- Apollo.preload(summon[ApolloClient].query(CurrentUserQuery()))
                    second <- Apollo.preload(summon[ApolloClient].query(CurrentUserQuery()))
                    _      <- Async.sleep(30L.millis)
                    // Both left before EITHER was read — the loader shape, not a waterfall.
                    _ = assert(engine.started == 2)
                    _ = engine.release()
                    s1 <- first.read(UpdateFailure.Notify)
                    s2 <- second.read(UpdateFailure.Notify)
                    d1 <- s1.current
                    d2 <- s2.current
                yield
                    assert(d1 == userData("Alice"))
                    assert(d2 == userData("Alice"))
            }
        }

        "read aborts on the Apollo channel when the first settle is a failure" in {
            val engine         = new CountingGatedEngine("""{"data":null,"errors":[{"message":"boom"}]}""")
            given ApolloClient = cached(engine)
            Scope.run {
                for
                    preloaded <- Apollo.preload(summon[ApolloClient].query(CurrentUserQuery()))
                    _ = engine.release()
                    outcome <- Abort.run[ApolloException](preloaded.read(UpdateFailure.Notify))
                yield outcome match
                    case Result.Failure(ex) => assert(ex.getMessage == "boom")
                    case other              => fail(s"expected the first-settle failure, got $other")
            }
        }
    }
end PreloadSpec
