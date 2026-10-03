package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.exception.ApolloException

/** Task 8 — the reactive form driven to completion, now on kyo-test. Each leaf
  * body is the effect; the runner executes it (no `KyoRun` bridge). A signal is
  * driven by a background watcher fiber, so every read waits on the value it
  * expects (`assertEventually`) or on a barrier (the gated engine's parked
  * request), never on a pause.
  */
class ApolloSignalExecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def isSuccess(state: QueryState[UserData], name: String): Boolean =
        state match
            case QueryState.Success(data, _, _) => data == userData(name)
            case _                              => false

    "reactive form (watchSignal)" - {

        "watchSignal seeds Loading, then emits Success once the network resolves" in {
            val engine = new GatedEngine(body("Alice"))
            for
                client <- cached(engine)
                signal <- Apollo.watchSignal(
                    client
                        .query(CurrentUserQuery())
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                )
                // The fetch is parked at the engine: nothing can have answered yet.
                _       <- engine.nextRequest
                loading <- signal.current
                _       <- engine.release
                _       <- assertEventually(signal.current.map(isSuccess(_, "Alice")))
            yield assert(loading == QueryState.Loading)
            end for
        }

        "watchSignal re-emits when a watched cache record changes" in {
            for
                client <- cached(StaticEngine(body("Alice")))
                // Warm the cache so the CacheOnly watch opens on populated data.
                _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                signal <- Apollo.watchSignal(client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly))
                _      <- assertEventually(signal.current.map(_ == QueryState.Success(userData("Alice"), fromCache = true)))
                // A write to the shared User:1 record — as a mutation write-back would.
                _ <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob"))
                _ <- assertEventually(signal.current.map(_ == QueryState.Success(userData("Bob"), fromCache = true)))
            yield ()
        }

        "watchSignal unsubscribes its watcher when the Scope is released" in {
            for
                client <- cached(StaticEngine(body("Alice")))
                _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                // Open the watch inside a Scope and let the Signal escape; the Scope then
                // EXITS, tearing the watcher down.
                escaped <- Scope.run {
                    for
                        signal <- Apollo.watchSignal(client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly))
                        // The watcher's first emission reached the signal before the Scope exits.
                        _ <- assertEventually(signal.current.map(_ == QueryState.Success(userData("Alice"), fromCache = true)))
                    yield signal
                }
                // A watcher that is still open sees the write: the barrier that it was
                // published, so the escaped watcher would have seen it too.
                live  <- Apollo.watchSignal(client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly))
                _     <- assertEventually(live.current.map(_ == QueryState.Success(userData("Alice"), fromCache = true)))
                _     <- client.apolloStore.writeOperation(CurrentUserQuery(), userData("Zed"))
                _     <- assertEventually(live.current.map(_ == QueryState.Success(userData("Zed"), fromCache = true)))
                after <- escaped.current
            yield assert(after == QueryState.Success(userData("Alice"), fromCache = true))
        }

        "watchSignal projects a transport exception to Failure" in {
            for
                client <- cached(StaticEngine("""{"errors":[]}""", status = 500))
                signal <- Apollo.watchSignal(
                    client
                        .query(CurrentUserQuery())
                        .fetchPolicy(FetchPolicy.NetworkOnly)
                )
                _ <- assertEventually(signal.current.map {
                    case QueryState.Failure(_: ApolloException, _) => true
                    case _                                         => false
                })
            yield ()
        }

        "QueryHandle.mapData projects state AND refetch; a rejection lands in Failure / the Abort channel" in {
            val rejected = kyo.apollo.exception.DefaultApolloException("unusable")
            for
                client <- cached(StaticEngine(body("Alice")))
                handle <- Apollo.query(
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly)
                )
                _ <- assertEventually(handle.state.current.map(isSuccess(_, "Alice")))
                mapped = handle.mapData(d => d.user.name)
                projectedState <- mapped.state.current
                refetched      <- mapped.refetch
                rejecting = handle.mapData(_ => (Abort.fail(rejected): String < Abort[ApolloException]))
                rejectedState   <- rejecting.state.current
                rejectedRefetch <- Abort.run[ApolloException](rejecting.refetch)
            yield
                projectedState match
                    case QueryState.Success(name, _, _) => assert(name == "Alice")
                    case other                          => fail(s"expected Success(Alice), got $other")
                assert(refetched == "Alice")
                assert(rejectedState == QueryState.Failure(rejected))
                assert(rejectedRefetch == Result.Failure(rejected))
            end for
        }

        "QueryHandle.failed pins state to Failure and refetch to the same Abort" in {
            val boom = kyo.apollo.exception.DefaultApolloException("nope")
            for
                h  <- QueryHandle.failed[Int](boom)
                st <- h.state.current
                r  <- Abort.run[ApolloException](h.refetch)
            yield
                assert(st == QueryState.Failure(boom))
                assert(r == Result.Failure(boom))
            end for
        }

        "Signal.mapData derives a rejecting projection signal (Abort.fail lands in Failure)" in {
            val rejected = kyo.apollo.exception.DefaultApolloException("no game")
            for
                ref <- Signal.initRef[QueryState[Int]](QueryState.Loading)
                mapped = ref.mapData(n => if n < 0 then Abort.fail(rejected) else n * 2)
                loading <- mapped.current
                _       <- ref.set(QueryState.Success(21, fromCache = false))
                doubled <- mapped.current
                _       <- ref.set(QueryState.Success(-1, fromCache = false))
                failed  <- mapped.current
            yield
                assert(loading == QueryState.Loading)
                assert(doubled == QueryState.Success(42, fromCache = false))
                assert(failed == QueryState.Failure(rejected))
            end for
        }
    }
end ApolloSignalExecSpec
