package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.json.Json
import kyo.apollo.network.http.HttpEngine

/** `client.refetchQueries` and `client.resetStore` reach every live watcher a client has open, whichever entry point opened
  * it: once per query (the same operation, document and variables send one request, and the cache hands the answer to
  * every watcher), and never while it is paused (`skip`, live variables at `Absent`, a lazy query not yet loaded).
  *
  * Requests are counted at the engine. A count is read once the effect that sends the request has returned, so every
  * assertion waits on the value it expects or on that return, never on a pause.
  */
class ApolloRefetchSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Answers with the body it holds now, counts what reaches it, and can hold replies until released. */
    final class ServerEngine(initial: String) extends HttpEngine:
        private given AllowUnsafe = AllowUnsafe.embrace.danger
        private val bodyRef       = AtomicRef.Unsafe.init(initial).safe
        private val statusRef     = AtomicInt.Unsafe.init(200).safe
        private val received      = AtomicInt.Unsafe.init(0).safe
        private val gateRef       = AtomicRef.Unsafe.init(Maybe.empty[Fiber.Promise[Unit, Any]]).safe
        private val arrived       = Channel.Unsafe.init[Unit](Int.MaxValue)(using Frame.internal).safe

        def calls(using Frame): Int < Sync                 = received.get
        def answer(body: String)(using Frame): Unit < Sync = bodyRef.set(body)
        def fail(status: Int)(using Frame): Unit < Sync    = statusRef.set(status)

        /** Holds every reply from now on until [[release]]. */
        def hold(using Frame): Unit < Sync = Fiber.Promise.init[Unit, Any].map(p => gateRef.set(Present(p)))

        /** Completes once the next held request has reached the engine. */
        def nextHeld(using Frame): Unit < Async = Abort.run[Closed](arrived.take).unit

        def release(using Frame): Unit < Sync =
            gateRef.getAndSet(Absent).map(_.fold(Kyo.unit)(_.completeUnitDiscard))

        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            for
                _      <- received.incrementAndGet
                gate   <- gateRef.get
                _      <- gate.fold(Kyo.unit)(p => Abort.run[Closed](arrived.offer(())).andThen(p.get))
                body   <- bodyRef.get
                status <- statusRef.get
            yield HttpEngine.response(HttpStatus.init(status).getOrThrow, body)
    end ServerEngine

    /** The user by id: the same operation with variables, so two ids are two queries. */
    final case class UserByIdQuery(id: String) extends Query.Normalizable[UserData]:
        def name                           = "UserById"
        def document                       = "query UserById($id: ID!) { user { __typename id name } }"
        val dataCodec: JsonCodec[UserData] = JsonCodec.fromSchema[UserData]
        def rootField: CompiledField       = CurrentUserQuery().rootField
        def variables: Json                = Json.JObj(Map("id" -> Json.JStr(id)))
    end UserByIdQuery

    private def named(name: String)(state: QueryState[UserData]): Boolean =
        state match
            case QueryState.Success(data, _, _) => data == userData(name)
            case _                              => false

    /** How many requests `effect` sent. */
    private def sent[A](engine: ServerEngine)(effect: A < (Async & Abort[ApolloException]))(using
        Frame
    ): Int < (Async & Abort[ApolloException]) =
        for
            before <- engine.calls
            _      <- effect
            after  <- engine.calls
        yield after - before

    "every entry point is refetched while it is live" - {

        "watchSignal" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                signal <- Apollo.watchSignal(client.query(CurrentUserQuery()))
                _      <- awaitSignal(signal)(named("Alice"))
                _      <- engine.answer(body("Bob"))
                n      <- sent(engine)(client.refetchQueries("CurrentUser"))
                _      <- awaitSignal(signal)(named("Bob"))
            yield assert(n == 1)
            end for
        }

        "pollingSignal" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                signal <- Apollo.pollingSignal(client.query(CurrentUserQuery()), 1.hour)
                _      <- awaitSignal(signal)(named("Alice"))
                _      <- engine.answer(body("Bob"))
                n      <- sent(engine)(client.refetchQueries("CurrentUser"))
                _      <- awaitSignal(signal)(named("Bob"))
            yield assert(n == 1)
            end for
        }

        "preload" in {
            val engine = ServerEngine(body("Alice"))
            for
                client  <- cached(engine)
                preload <- Apollo.preload(client.query(CurrentUserQuery()))
                _       <- awaitSignal(preload.state)(named("Alice"))
                _       <- engine.answer(body("Bob"))
                n       <- sent(engine)(client.refetchQueries("CurrentUser"))
                _       <- awaitSignal(preload.state)(named("Bob"))
            yield assert(n == 1)
            end for
        }

        "paginatedQuery over a fixed operation" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                paged  <- Apollo.paginatedQuery(())(_ => client.query(CurrentUserQuery()))
                _      <- awaitSignal(paged.state)(named("Alice"))
                n      <- sent(engine)(client.refetchQueries("CurrentUser"))
            yield assert(n == 1)
            end for
        }

        "paginatedQuery over live variables" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                values <- Signal.initRef[Maybe[String]](Present("1"))
                paged  <- Apollo.paginatedQuery(values)(())((id, _) => client.query(UserByIdQuery(id)))
                _      <- awaitSignal(paged.state)(named("Alice"))
                n      <- sent(engine)(client.refetchQueries("UserById"))
            yield assert(n == 1)
            end for
        }

        "watchSignal over live variables" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                values <- Signal.initRef[Maybe[String]](Present("1"))
                signal <- Apollo.watchSignal(values)(id => client.query(UserByIdQuery(id)))
                _      <- awaitSignal(signal)(named("Alice"))
                n      <- sent(engine)(client.refetchQueries("UserById"))
            yield assert(n == 1)
            end for
        }

        "a lazy query, once it has loaded" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                lazyQ  <- Apollo.lazyQuery(client.query(CurrentUserQuery()))
                before <- sent(engine)(client.refetchQueries("CurrentUser"))
                _      <- lazyQ.load
                _      <- engine.answer(body("Bob"))
                after  <- sent(engine)(client.refetchQueries("CurrentUser"))
                _      <- awaitSignal(lazyQ.state)(named("Bob"))
            yield
                assert(before == 0)
                assert(after == 1)
            end for
        }
    }

    "resetStore brings a watchSignal to the server's answer" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            signal <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            _      <- awaitSignal(signal)(named("Alice"))
            _      <- engine.answer(body("Bob"))
            _      <- client.resetStore
            _      <- awaitSignal(signal)(named("Bob"))
        yield succeed
        end for
    }

    "one query is one request, however many watch it" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            first  <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            second <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            handle <- Apollo.query(client.query(CurrentUserQuery()))
            _      <- awaitSignal(first)(named("Alice"))
            _      <- awaitSignal(second)(named("Alice"))
            _      <- awaitSignal(handle.state)(named("Alice"))
            n      <- sent(engine)(client.refetchQueries("CurrentUser"))
        yield assert(n == 1)
        end for
    }

    "a handle among the watchers of a query shows the refetch while it runs" in {
        val engine = ServerEngine(body("Alice"))
        for
            client  <- cached(engine)
            watcher <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            handle  <- Apollo.query(client.query(CurrentUserQuery()))
            _       <- awaitSignal(watcher)(named("Alice"))
            _       <- awaitSignal(handle.state)(named("Alice"))
            _       <- engine.hold
            refetch <- Fiber.init(client.refetchQueries("CurrentUser"))
            _       <- engine.nextHeld
            during  <- handle.networkStatus.current
            _       <- engine.release
            _       <- refetch.get
            after   <- awaitSignal(handle.networkStatus)(_ != NetworkStatus.Refetch)
        yield assert(during == NetworkStatus.Refetch)
        end for
    }

    "two variables are two queries" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            a      <- Apollo.watchSignal(client.query(UserByIdQuery("a")))
            b      <- Apollo.watchSignal(client.query(UserByIdQuery("b")))
            _      <- awaitSignal(a)(named("Alice"))
            _      <- awaitSignal(b)(named("Alice"))
            n      <- sent(engine)(client.refetchQueries("UserById"))
        yield assert(n == 2)
        end for
    }

    "a paused query is not refetched" - {

        "watchSignal with skip" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                skip   <- Signal.initRef(false)
                signal <- Apollo.watchSignal(client.query(CurrentUserQuery()), skip)
                _      <- awaitSignal(signal)(named("Alice"))
                _      <- skip.set(true)
                paused <- sent(engine)(client.refetchQueries("CurrentUser"))
                before <- engine.calls
                _      <- skip.set(false)
                // The refetch it was left out of is made up for once it resumes.
                _       <- assertEventually(engine.calls.map(_ == before + 1))
                resumed <- sent(engine)(client.refetchQueries("CurrentUser"))
            yield
                assert(paused == 0)
                assert(resumed == 1)
            end for
        }

        "a query handle with skip" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                skip   <- Signal.initRef(false)
                handle <- Apollo.query(client.query(CurrentUserQuery()), skip)
                _      <- awaitSignal(handle.state)(named("Alice"))
                _      <- skip.set(true)
                paused <- sent(engine)(client.refetchQueries("CurrentUser"))
                before <- engine.calls
                _      <- skip.set(false)
                // The refetch it was left out of is made up for once it resumes.
                _       <- assertEventually(engine.calls.map(_ == before + 1))
                resumed <- sent(engine)(client.refetchQueries("CurrentUser"))
            yield
                assert(paused == 0)
                assert(resumed == 1)
            end for
        }

        "watchSignal with skip, paused through a resetStore, shows the server's answer when it resumes" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                skip   <- Signal.initRef(false)
                signal <- Apollo.watchSignal(client.query(CurrentUserQuery()), skip)
                _      <- awaitSignal(signal)(named("Alice"))
                _      <- skip.set(true)
                _      <- engine.answer(body("Bob"))
                _      <- client.resetStore
                _      <- skip.set(false)
                _      <- awaitSignal(signal)(named("Bob"))
            yield succeed
            end for
        }

        "a query handle with skip, paused through a refetch of its name, catches up when it resumes" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                skip   <- Signal.initRef(false)
                handle <- Apollo.query(client.query(CurrentUserQuery()), skip)
                _      <- awaitSignal(handle.state)(named("Alice"))
                _      <- skip.set(true)
                _      <- engine.answer(body("Bob"))
                _      <- client.refetchQueries("CurrentUser")
                _      <- skip.set(false)
                _      <- awaitSignal(handle.state)(named("Bob"))
            yield succeed
            end for
        }

        "watchSignal over live variables at Absent" in {
            val engine = ServerEngine(body("Alice"))
            for
                client <- cached(engine)
                values <- Signal.initRef[Maybe[String]](Absent)
                _      <- Apollo.watchSignal(values)(id => client.query(UserByIdQuery(id)))
                n      <- sent(engine)(client.refetchQueries())
            yield assert(n == 0)
            end for
        }
    }

    "a name picks its own queries and no others" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            signal <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            _      <- awaitSignal(signal)(named("Alice"))
            n      <- sent(engine)(client.refetchQueries("Other"))
        yield assert(n == 0)
        end for
    }

    "a watcher leaves the registry when its Scope closes" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            _      <- Scope.run {
                Apollo.watchSignal(client.query(CurrentUserQuery())).map(awaitSignal(_)(named("Alice")))
            }
            entries <- client.activeQueries.entries
            n       <- sent(engine)(client.refetchQueries())
        yield
            assert(entries.isEmpty)
            assert(n == 0)
        end for
    }

    "a refetch that fails reaches the caller" in {
        val engine = ServerEngine(body("Alice"))
        for
            client <- cached(engine)
            signal <- Apollo.watchSignal(client.query(CurrentUserQuery()))
            _      <- awaitSignal(signal)(named("Alice"))
            _      <- engine.fail(500)
            result <- Abort.run[ApolloException](client.refetchQueries("CurrentUser"))
        yield assert(result.isFailure)
        end for
    }
end ApolloRefetchSpec
