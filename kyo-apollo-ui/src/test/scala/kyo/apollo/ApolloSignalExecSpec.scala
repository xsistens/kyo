package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.exception.ApolloException

/** Task 8 — the reactive form driven to completion, now on kyo-test. Each leaf
  * body is the effect; the runner executes it (no `KyoRun` bridge). The runner
  * uses a LIVE clock, so the `Async.sleep` microtask-propagation waits behave as
  * before. `Signal.current` is read at controlled points to assert real
  * reactive behaviour.
  */
class ApolloSignalExecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "reactive form (watchSignal)" - {

        "watchSignal seeds Loading, then emits Success once the network resolves" in {
            val engine = new GatedEngine(body("Alice"))
            val client = cached(engine)
            Scope.run {
                for
                    signal <- Apollo.watchSignal(
                        client
                            .query(CurrentUserQuery())
                            .fetchPolicy(FetchPolicy.NetworkOnly)
                    )
                    loading <- signal.current
                    _       <- Sync.defer(engine.release())
                    _       <- Async.sleep(30L.millis)
                    settled <- signal.current
                yield
                    assert(loading == QueryState.Loading)
                    settled match
                        case QueryState.Success(data, _) => assert(data == userData("Alice"))
                        case other                       => fail(s"expected Success(Alice), got $other")
            }
        }

        "watchSignal re-emits when a watched cache record changes" in {
            val client = cached(StaticEngine(body("Alice")))
            Scope.run {
                for
                    // Warm the cache so the CacheOnly watch opens on populated data.
                    _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                    signal <- Apollo.watchSignal(client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly))
                    // watchSignal consumes the watch on a detached background fiber (kyo's
                    // own scheduler, not this turn), so give it a moment to process the
                    // initial cache-hit emission before reading the seeded signal.
                    _     <- Async.sleep(30L.millis)
                    first <- signal.current
                    // A write to the shared User:1 record — as a mutation write-back would.
                    _      <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                    _      <- Async.sleep(30L.millis)
                    second <- signal.current
                yield
                    assert(first == QueryState.Success(userData("Alice"), fromCache = true))
                    assert(second == QueryState.Success(userData("Bob"), fromCache = true))
            }
        }

        "watchSignal unsubscribes its watcher when the Scope is released" in {
            val client                                = cached(StaticEngine(body("Alice")))
            var escaped: Signal[QueryState[UserData]] = null
            for
                // Warm + open the watch inside a Scope, letting the Signal escape; the
                // Scope then EXITS, tearing the watcher down.
                _ <- Scope.run {
                    for
                        _ <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                        signal <- Apollo.watchSignal(
                            client
                                .query(CurrentUserQuery())
                                .fetchPolicy(FetchPolicy.CacheOnly)
                        )
                        // Let the background watcher fiber process its initial cache-hit
                        // emission BEFORE the Scope exits (and interrupts it) below.
                        _ <- Async.sleep(30L.millis)
                        _ <- Sync.defer { escaped = signal }
                    yield ()
                }
                before <- escaped.current
                // This write WOULD drive a re-emit if the watcher were still subscribed.
                _     <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Zed")))
                after <- escaped.current
            yield
                assert(after == before)
                assert(after == QueryState.Success(userData("Alice"), fromCache = true))
            end for
        }

        "watchSignal projects a transport exception to Failure" in {
            val client = cached(StaticEngine("""{"errors":[]}""", status = 500))
            Scope.run {
                for
                    signal <- Apollo.watchSignal(
                        client
                            .query(CurrentUserQuery())
                            .fetchPolicy(FetchPolicy.NetworkOnly)
                    )
                    _     <- Async.sleep(30L.millis)
                    state <- signal.current
                yield state match
                    case QueryState.Failure(ex) => assert(ex.isInstanceOf[ApolloException])
                    case other                  => fail(s"expected Failure, got $other")
            }
        }

        "QueryHandle.mapData projects state AND refetch; a rejection lands in Failure / the Abort channel" in {
            val client   = cached(StaticEngine(body("Alice")))
            val rejected = kyo.apollo.exception.DefaultApolloException("unusable")
            Scope.run {
                for
                    handle <- Apollo.query(
                        client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly)
                    )
                    _ <- Async.sleep(30L.millis)
                    mapped = handle.mapData(d => d.user.name)
                    projectedState <- mapped.state.current
                    refetched      <- mapped.refetch
                    rejecting = handle.mapData(_ => (Abort.fail(rejected): String < Abort[ApolloException]))
                    rejectedState   <- rejecting.state.current
                    rejectedRefetch <- Abort.run[ApolloException](rejecting.refetch)
                yield
                    projectedState match
                        case QueryState.Success(name, _) => assert(name == "Alice")
                        case other                       => fail(s"expected Success(Alice), got $other")
                    assert(refetched == "Alice")
                    assert(rejectedState == QueryState.Failure(rejected))
                    assert(rejectedRefetch == Result.Failure(rejected))
            }
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
