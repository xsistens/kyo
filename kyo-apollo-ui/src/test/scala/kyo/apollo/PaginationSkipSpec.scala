package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.cache.normalized.*

/** The `skip` gate on a paginated query: parking one must keep what it painted.
  *
  * A page that hosts several tabs under ONE mount has to park the tabs it is not
  * showing, or entering it fires every tab's operation. `query` and `watchSignal`
  * park through `skip` and keep their last value; before this overload existed a
  * paginated tab had only the `values` gate, whose `Absent` resets the state to
  * [[QueryState.Idle]] — so that one tab, alone, re-painted its loading line on
  * every return.
  */
class PaginationSkipSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** The cursor never varies here: what is under test is the GATE, not the
      * connection merge (which is the cache's `ConnectionFieldPolicy`, unchanged).
      */
    private def cachedPage(client: ApolloClient) =
        (_: Maybe[String]) => client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)

    private def painted(name: String): QueryState[UserData] = QueryState.Success(userData(name), fromCache = true)

    "paginatedQuery with a skip gate" - {

        "parking keeps the page it painted, stops watching, and repaints on return" in {
            for
                client <- cached(StaticEngine(body("Alice")))
                // Warm the cache so the CacheOnly watcher opens on populated data.
                _    <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                skip <- Signal.initRef[Boolean](false)
                paged <- Apollo.paginatedQuery(
                    initial = Absent: Maybe[String],
                    skip = skip,
                    mode = SkipMode.Unsubscribe
                )(cachedPage(client))
                active <- awaitSignal(paged.state)(_ == painted("Alice"))
                // An open watcher on the same record: the barrier that a write was published.
                live   <- Apollo.watchSignal(cachedPage(client)(Absent))
                writes <- AtomicInt.init
                // Park it. The watcher is torn down on its own fiber, so wait until a
                // write provably no longer reaches it: the open watcher shows the write,
                // and the parked state keeps what it had.
                _ <- skip.set(true)
                _ <- assertEventually {
                    for
                        before <- paged.state.current
                        name   <- writes.incrementAndGet.map(n => s"Bob$n")
                        _      <- client.apolloStore.writeOperation(CurrentUserQuery(), userData(name))
                        _      <- awaitSignal(live)(_ == painted(name))
                        after  <- paged.state.current
                    yield after == before
                }
                parked <- paged.state.current
                last   <- writes.get.map(n => s"Bob$n")
                // Return to it: the watcher re-opens and reads the cache.
                _       <- skip.set(false)
                resumed <- awaitSignal(paged.state)(_ == painted(last))
            yield
                assert(active == painted("Alice"))
                // The two halves of the gate: the state was NOT reset (it is still a
                // Success, not Idle), and the watcher was really torn down (the last
                // write reached it only once it re-opened).
                parked match
                    case QueryState.Success(data, true, _) => assert(data != userData(last))
                    case other                             => fail(s"expected the painted page to stay, got $other")
                assert(resumed == painted(last))
            end for
        }

        "the `values` gate parks at Idle instead — the difference this overload exists for" in {
            for
                client <- cached(StaticEngine(body("Alice")))
                _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                values <- Signal.initRef[Maybe[String]](Present("live"))
                paged <- Apollo.paginatedQuery(values)(initial = Absent: Maybe[String])((_, _) =>
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)
                )
                active <- awaitSignal(paged.state)(_ == painted("Alice"))
                _      <- values.set(Absent)
                parked <- awaitSignal(paged.state)(_ == QueryState.Idle)
            yield
                assert(active == painted("Alice"))
                assert(parked == QueryState.Idle)
        }

        "fetchMore stays live while parked, the way refetch does" in {
            val engine = new StaticEngine(body("Alice"))
            for
                client <- cached(engine)
                _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                skip   <- Signal.initRef[Boolean](false)
                // The single-connection sugar, which had no skip form at all.
                handle <- Apollo.paginatedQuery(
                    cachedPage(client),
                    skip,
                    SkipMode.Unsubscribe
                )(_ => Present("page-2"))
                _       <- awaitSignal(handle.state)(_ == painted("Alice"))
                warmed  <- engine.calls
                _       <- skip.set(true)
                _       <- handle.fetchMore
                fetched <- engine.calls
            yield
                // One network call to warm the cache; the CacheOnly watcher adds none.
                assert(warmed == 1)
                assert(fetched == 2)
            end for
        }

        "a live-variables paginated query can still be flattened to a handle" in {
            for
                client <- cached(StaticEngine(body("Alice")))
                _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                values <- Signal.initRef[Maybe[String]](Present("live"))
                paged <- Apollo.paginatedQuery(values)(initial = Absent: Maybe[String])((_, _) =>
                    client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)
                )
                // The two lines a caller used to have to write out by hand.
                handle = paged.singleConnection(_ => Present("page-2"))
                state <- awaitSignal(handle.state)(_ == painted("Alice"))
            yield assert(state == painted("Alice"))
        }
    }
end PaginationSkipSpec
