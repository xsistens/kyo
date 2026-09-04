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
        (_: Option[String]) => client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)

    "paginatedQuery with a skip gate" - {

        "parking keeps the page it painted, stops watching, and repaints on return" in {
            val client = cached(StaticEngine(body("Alice")))
            Scope.run {
                for
                    // Warm the cache so the CacheOnly watcher opens on populated data.
                    _    <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                    skip <- Signal.initRef[Boolean](false)
                    paged <- Apollo.paginatedQuery(
                        initial = None: Option[String],
                        skip = skip,
                        mode = SkipMode.Unsubscribe
                    )(cachedPage(client))
                    _      <- Async.sleep(30L.millis)
                    active <- paged.state.current
                    // Park it, then change the record it was watching.
                    _      <- skip.set(true)
                    _      <- Async.sleep(30L.millis)
                    _      <- Sync.defer(client.apolloStore.writeOperation(CurrentUserQuery(), userData("Bob")))
                    _      <- Async.sleep(30L.millis)
                    parked <- paged.state.current
                    // Return to it: the watcher re-opens and reads the cache.
                    _       <- skip.set(false)
                    _       <- Async.sleep(50L.millis)
                    resumed <- paged.state.current
                yield
                    assert(active == QueryState.Success(userData("Alice"), fromCache = true))
                    // The two halves of the gate, in one assertion each: the state was NOT
                    // reset (it is still a Success, not Idle), and the watcher was really
                    // torn down (Bob's write did not reach it).
                    assert(parked == QueryState.Success(userData("Alice"), fromCache = true))
                    assert(resumed == QueryState.Success(userData("Bob"), fromCache = true))
            }
        }

        "the `values` gate parks at Idle instead — the difference this overload exists for" in {
            val client = cached(StaticEngine(body("Alice")))
            Scope.run {
                for
                    _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                    values <- Signal.initRef[Maybe[String]](Present("live"))
                    paged <- Apollo.paginatedQuery(values)(initial = None: Option[String])((_, _) =>
                        client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)
                    )
                    _      <- Async.sleep(30L.millis)
                    active <- paged.state.current
                    _      <- values.set(Absent)
                    _      <- Async.sleep(30L.millis)
                    parked <- paged.state.current
                yield
                    assert(active == QueryState.Success(userData("Alice"), fromCache = true))
                    assert(parked == QueryState.Idle)
            }
        }

        "fetchMore stays live while parked, the way refetch does" in {
            val engine = new StaticEngine(body("Alice"))
            val client = cached(engine)
            Scope.run {
                for
                    _    <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                    skip <- Signal.initRef[Boolean](false)
                    // The single-connection sugar, which had no skip form at all.
                    handle <- Apollo.paginatedQuery(
                        cachedPage(client),
                        skip,
                        SkipMode.Unsubscribe
                    )(_ => Some("page-2"))
                    _       <- Async.sleep(30L.millis)
                    warmed  <- Sync.defer(engine.calls)
                    _       <- skip.set(true)
                    _       <- Async.sleep(30L.millis)
                    _       <- handle.fetchMore
                    fetched <- Sync.defer(engine.calls)
                yield
                    // One network call to warm the cache; the CacheOnly watcher adds none.
                    assert(warmed == 1)
                    assert(fetched == 2)
            }
        }

        "a live-variables paginated query can still be flattened to a handle" in {
            val client = cached(StaticEngine(body("Alice")))
            Scope.run {
                for
                    _      <- client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.NetworkOnly).response
                    values <- Signal.initRef[Maybe[String]](Present("live"))
                    paged <- Apollo.paginatedQuery(values)(initial = None: Option[String])((_, _) =>
                        client.query(CurrentUserQuery()).fetchPolicy(FetchPolicy.CacheOnly)
                    )
                    // The two lines a caller used to have to write out by hand.
                    handle = paged.singleConnection(_ => Some("page-2"))
                    _     <- Async.sleep(30L.millis)
                    state <- handle.state.current
                yield assert(state == QueryState.Success(userData("Alice"), fromCache = true))
            }
        }
    }
end PaginationSkipSpec
