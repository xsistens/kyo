package kyo.apollo

import kyo.*
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.EmbeddedFragment
import kyo.apollo.cache.normalized.api.EntityFragment
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.apolloStore
import kyo.apollo.cache.normalized.watch
import kyo.apollo.exception.ApolloConfigException
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.CacheMissException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import scala.NamedTuple.AnyNamedTuple

/** The `Apollo` namespace — the constructor entry points of the `kyo-ui` binding.
  *
  * Every operation that turns a prepared [[ApolloCall]] (or a [[Fragment]], or a
  * builder configuration) into a running/reactive result lives here as
  * `Apollo.<verb>(…)`: the query/mutation/fragment/pagination handles, the reactive
  * `Signal` builders (`watchSignal`/`pollingSignal`/`subscribe`), and client
  * construction (`client`/`clientLayer`). Fluent per-call configuration
  * (`call.fetchPolicy`…), the one-shot run verbs (`call.data`/`call.response`), the
  * raw stream views (`call.watch()`/`call.watchStream`/`call.keepCacheWarm`), and the
  * instance methods on the returned handles/signals/store stay as methods on their
  * receiver.
  */
object Apollo:

    // --- Queries -------------------------------------------------------------

    /** Prepare a live query — react-apollo's `useQuery`: reactive
      * [[QueryHandle.state]] paired with a `refetch` handle. Bind inside a
      * `Scope.run { … }`; the watcher tears down when the block exits.
      */
    def query[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): RawQueryHandle[D] < (Async & Scope) =
        buildQueryHandle(call, watchSignal(call))

    /** [[query]] with a live `skip` (react-apollo's `skip`): while `skip` is `true`
      * the reactive state freezes ([[SkipMode.Freeze]]) or the watcher is torn down
      * ([[SkipMode.Unsubscribe]]); `refetch` is unaffected.
      */
    def query[D](call: ApolloCall[D], skip: Signal[Boolean], mode: SkipMode = SkipMode.Freeze)(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): RawQueryHandle[D] < (Async & Scope) =
        buildQueryHandle(call, watchSignal(call, skip, mode))

    /** Prepare a query that fetches only when asked — react-apollo's `useLazyQuery`:
      * `state` is `Idle` until `load` is invoked.
      */
    def lazyQuery[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): LazyQueryHandle[D] < (Async & Scope) =
        Signal.initRef[QueryState[D]](QueryState.Idle).map { ref =>
            LazyQueryHandle(
                ref,
                // Arm `Loading`, fetch NetworkOnly, then project the outcome into `state`
                // and hand the typed data back (re-raising any Abort/Panic after recording
                // a `Failure`, so a failed load both surfaces the error AND shows in the UI).
                load = ref.set(QueryState.Loading).andThen {
                    Abort.run[ApolloException](call.fetchPolicy(FetchPolicy.NetworkOnly).data).map {
                        case Result.Success(data) =>
                            ref.set(QueryState.Success(data, fromCache = false)).andThen(data)
                        case failed =>
                            val ex = failed match
                                case Result.Failure(e) => e
                                case _                 => DefaultApolloException("lazy query failed")
                            ref.set(QueryState.Failure(ex)).andThen(Abort.get(failed))
                    }
                }
            )
        }

    // --- Mutations -----------------------------------------------------------

    /** Prepare a mutation with an observable state handle — react-apollo's
      * `useMutation` over a fully-prepared call. `run(())` fires it.
      */
    def mutation[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): MutationHandle[Unit, D] < Sync =
        mutation[Unit, D](_ => call)

    /** Prepare a mutation whose variables are supplied at fire-time — the
      * react-apollo `mutate(variables)` shape. `build` turns an input `I` into the
      * prepared call, and `run(i)` fires it, driving [[MutationHandle.state]].
      */
    def mutation[I, D](build: I => ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): MutationHandle[I, D] < Sync =
        Signal.initRef[MutationState[D]](MutationState.Idle).map { ref =>
            MutationHandle(
                state = ref,
                run = (input: I) =>
                    val call = build(input)
                    val policy =
                        call.apolloRequest.executionContext.get(ErrorPolicy).getOrElse(ErrorPolicy.Default)
                    // Arm `Loading`, run ONCE via `.response` (total — keeps partial data), then
                    // project the response into `state` AND yield the strict `.data` outcome
                    // (honoring `errorPolicy`), re-raising on `Abort` after recording it.
                    for
                        _    <- ref.set(MutationState.Loading)
                        resp <- call.response
                        _    <- ref.set(MutationState.fromResponse(resp))
                        data <- Abort.get(ApolloEffect.projectData(resp, policy))
                    yield data
                    end for
                ,
                reset = ref.set(MutationState.Idle)
            )
        }

    // --- Fragments -----------------------------------------------------------

    /** Reactively read one cached entity as a [[Signal]] of [[Maybe]] — react-apollo's
      * `useFragment`. Seeds the current value and re-reads on every change to a key
      * the read depended on; the change listener is dropped when the enclosing `Scope`
      * is released. The [[ApolloClient]] is taken as a `given`.
      */
    def fragment[D](fragment: Fragment[D], cacheKey: CacheKey)(using
        client: ApolloClient,
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[Maybe[D]] < (Async & Scope) =
        val store = client.apolloStore
        // A plain (non-effect) read: the denormalized value + the keys it depended on,
        // or Absent + just the entity key on a cache miss.
        def read(): (Maybe[D], Set[String]) =
            try
                val (data, keys) = store.readFragmentWithKeys(fragment, cacheKey)
                (Maybe(data), keys + cacheKey.key)
            catch case _: CacheMissException => (Maybe.empty, Set(cacheKey.key))

        val (initial, initialKeys) = read()
        Signal.initRef[Maybe[D]](initial).map { ref =>
            Channel.initUnscoped[Set[String]](Int.MaxValue).map { channel =>
                given AllowUnsafe = AllowUnsafe.embrace.danger
                // Push every change notification into the channel (synchronous callback).
                val unsubscribe = store.addChangedKeysListener { changed =>
                    val _ = channel.unsafe.offer(changed)
                }
                // Consume: when a change touches a key this read depends on, re-read and
                // update the signal (tracking the new dependent-key set).
                var watched = initialKeys
                val consume = channel.streamUntilClosed().foreach { changed =>
                    if changed.exists(watched) then
                        val (next, keys) = read()
                        watched = keys
                        ref.set(next)
                    else Sync.defer(())
                }
                // Teardown on Scope exit: drop the listener and close the channel (which
                // ends `streamUntilClosed`, so the consume fiber completes).
                Scope
                    .ensure(Sync.defer {
                        unsubscribe()
                        val _ = channel.unsafe.close()
                    })
                    .andThen(Fiber.init(consume))
                    .andThen(ref)
            }
        }
    end fragment

    /** Reactively read a masked fragment through its spread-produced ref — the
      * masked `useFragment`, and the ONLY door to a ref's contents: the fields a
      * [[EntityFragment]] selected are `private[apollo]` on the ref, so a parent
      * can pass it here (or to the component that declared the fragment) but never
      * read through it.
      *
      * Total, unlike the [[Fragment]]+[[CacheKey]] overload's `Maybe`: the ref was
      * decoded from a response that contained the fragment's fields, so there is
      * always something to render — the signal seeds from the entity record when
      * the cache has it and from the ref's own captured slice when it does not
      * (evicted, or a cache-less client). Later cache changes touching a dependent
      * key re-read and re-emit; a re-read that misses keeps the last value rather
      * than blanking a working view.
      */
    def fragment[Origin, D <: AnyNamedTuple](ref: EntityFragment[Origin, D]#Ref)(using
        client: ApolloClient,
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < (Async & Scope) =
        val definition = ref.definition
        // A cache-less client has no store to watch — the ref's captured slice IS
        // the data, so the signal degenerates to a constant. This is what keeps the
        // masked read total instead of inheriting `apolloStore`'s throw.
        val maybeStore =
            try Some(client.apolloStore)
            catch case _: IllegalStateException => None
        maybeStore match
            case None        => Signal.initRef[D](ref.decoded)
            case Some(store) => fragmentSignal(ref, definition, store)
    end fragment

    private def fragmentSignal[Origin, D <: AnyNamedTuple](
        ref: EntityFragment[Origin, D]#Ref,
        definition: EntityFragment[Origin, D],
        store: kyo.apollo.cache.normalized.ApolloStore
    )(using
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < (Async & Scope) =
        def read(): Maybe[(D, Set[String])] =
            try
                val (data, keys) = store.readFragmentWithKeys(definition.cacheFragment, ref.key)
                Present((data, keys + ref.key.key))
            catch case _: CacheMissException => Absent

        val (initial, initialKeys) = read() match
            case Present((data, keys)) => (data, keys)
            case Absent                => (ref.decoded, Set(ref.key.key))
        Signal.initRef[D](initial).map { signalRef =>
            Channel.initUnscoped[Set[String]](Int.MaxValue).map { channel =>
                given AllowUnsafe = AllowUnsafe.embrace.danger
                val unsubscribe = store.addChangedKeysListener { changed =>
                    val _ = channel.unsafe.offer(changed)
                }
                var watched = initialKeys
                val consume = channel.streamUntilClosed().foreach { changed =>
                    if changed.exists(watched) then
                        read() match
                            case Present((next, keys)) =>
                                watched = keys
                                signalRef.set(next)
                            // Keep the last value on a miss (an eviction mid-life); the
                            // dependent-key set keeps watching, so a re-population re-emits.
                            case Absent => Sync.defer(())
                    else Sync.defer(())
                }
                Scope
                    .ensure(Sync.defer {
                        unsubscribe()
                        val _ = channel.unsafe.close()
                    })
                    .andThen(Fiber.init(consume))
                    .andThen(signalRef)
            }
        }
    end fragmentSignal

    /** Read a masked embedded fragment's ref — the value-carrying counterpart of
      * the entity overload. The object has no cache identity, so there is nothing
      * to watch: the signal is constant, and updates arrive the way the value did —
      * through the parent's reactivity re-rendering the child with a fresh ref.
      */
    def fragment[Origin, D <: AnyNamedTuple](ref: EmbeddedFragment[Origin, D]#Ref)(using
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < Sync =
        Signal.initRef[D](ref.value)

    // --- Pagination ----------------------------------------------------------

    /** Prepare a paginated query over a cursor state `C` — the general
      * react-apollo `fetchMore` shape. `page(cursors)` re-issues the operation for a
      * cursor state; pages share their connection slots in the cache.
      */
    def paginatedQuery[D, C](initial: C)(page: C => ApolloCall[D])(using
        Frame,
        CanEqual[C, C],
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQuery[D, C] < (Async & Scope) =
        Signal.initRef[C](initial).map { cursors =>
            watchSignal(page(initial)).map { state =>
                val refetch: C => Unit < (Async & Abort[ApolloException]) =
                    c => page(c).fetchPolicy(FetchPolicy.NetworkOnly).data.unit
                val advance: ((C, D) => Option[C]) => (Unit < (Async & Abort[ApolloException])) =
                    reduce =>
                        cursors.current.map { c =>
                            state.current.map { qs =>
                                val next: Option[C] = PaginatedQuery.dataOf(qs) match
                                    case Some(d) => reduce(c, d)
                                    case None    => None
                                next match
                                    case Some(c2) => cursors.set(c2).andThen(refetch(c2))
                                    case None     => ()
                            }
                        }
                new PaginatedQuery(state, advance)
            }
        }

    /** Prepare a paginated query with a single connection — the sugar over the
      * general form. `page(None)` is the first page; `page(Some(cursor))` each next
      * one. Yields a flat [[PaginatedQueryHandle]] whose `fetchMore` advances it.
      */
    def paginatedQuery[D](page: Option[String] => ApolloCall[D])(
        cursorOf: D => Option[String]
    )(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQueryHandle[D] < (Async & Scope) =
        paginatedQuery[D, Option[String]](initial = None)(page).map { paged =>
            val conn = paged.connection(cursorOf, (_, cur) => cur)
            PaginatedQueryHandle(paged.state, conn.fetchMore)
        }

    // --- Preloading ------------------------------------------------------------

    /** Start `call`'s watcher NOW and hand back a [[PreloadedQuery]] immediately —
      * react-apollo's `preloadQuery`. The fetch is in flight from this moment;
      * nothing suspends until a consumer calls [[PreloadedQuery.read]]. Preload
      * every query a route needs in one loader and the fetches run in parallel
      * instead of waterfalling mount by mount. The watcher lives in the `Scope`
      * this is bound in.
      */
    def preload[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PreloadedQuery[D] < (Async & Scope) =
        watchSignal(call).map(PreloadedQuery(_))

    // --- Reactive Signal builders --------------------------------------------

    /** Watch this operation as a live `kyo-ui` [[Signal]] of [[QueryState]]: seeds
      * [[QueryState.Loading]], drains the cache watcher (`call.watch()`) on a forked
      * `Scope`-bound fiber, and pushes [[ApolloSignal.project]] of each emission into
      * the signal — `Loading`, then the first response, then re-emits on every cache
      * change.
      */
    def watchSignal[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- Signal.initRef[QueryState[D]](QueryState.Loading)
            // `push`, not a bare `project`: a mid-stream transport failure must retain
            // the last good data (same contract as the gated overload).
            _ <- Fiber.init(Scope.run(call.watch().foreach(resp => ApolloSignal.push(ref, resp))))
        yield ref

    /** [[watchSignal]] with a live `skip` — react-apollo's `skip` (urql's `pause`).
      * While `skip` is `true`, [[SkipMode.Freeze]] (default) keeps the watcher live but
      * stops updating; [[SkipMode.Unsubscribe]] tears it down and re-establishes on resume.
      */
    def watchSignal[D](call: ApolloCall[D], skip: Signal[Boolean], mode: SkipMode = SkipMode.Freeze)(
        using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- Signal.initRef[QueryState[D]](QueryState.Loading)
            _   <- Fiber.init(ApolloSignal.driveGated(call.watch(), ref, skip, mode))
        yield ref

    /** [[watchSignal]] that additionally **polls** the network every `interval` — the
      * react-apollo `pollInterval` shape. The poll's write-back lands in the cache, so
      * the watcher re-emits the fresh [[QueryState]]. A failed tick is swallowed.
      */
    def pollingSignal[D](call: ApolloCall[D], interval: Duration)(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        watchSignal(call).map { signal =>
            def pollLoop: Unit < Async =
                Async.sleep(interval).andThen {
                    Abort
                        .run[ApolloException](call.fetchPolicy(FetchPolicy.NetworkOnly).data)
                        .andThen(pollLoop)
                }
            Fiber.init(pollLoop).andThen(signal)
        }

    /** [[pollingSignal]] with a live `skip`: while skipped, the watcher is gated (per
      * `mode`) AND the periodic network poll is suspended — resuming restarts both.
      */
    def pollingSignal[D](
        call: ApolloCall[D],
        interval: Duration,
        skip: Signal[Boolean],
        mode: SkipMode = SkipMode.Freeze
    )(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        watchSignal(call, skip, mode).map { signal =>
            def pollLoop: Unit < Async =
                Async.sleep(interval).andThen {
                    skip.current.map {
                        case true => pollLoop // paused: skip the network poll, keep the timer loop alive
                        case false =>
                            Abort
                                .run[ApolloException](call.fetchPolicy(FetchPolicy.NetworkOnly).data)
                                .andThen(pollLoop)
                    }
                }
            Fiber.init(pollLoop).andThen(signal)
        }

    /** Watch a **subscription** as a live [[Signal]] of [[QueryState]] — the
      * react-apollo `useSubscription` shape. Seeds [[QueryState.Loading]], then
      * re-emits the projection of each event; the socket is torn down when the
      * enclosing `Scope` is released.
      */
    def subscribe[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- Signal.initRef[QueryState[D]](QueryState.Loading)
            _   <- Fiber.init(Scope.run(call.stream.foreach(resp => ref.set(ApolloSignal.project(resp)))))
        yield ref

    /** [[subscribe]] with a live `skip` — pause/resume a subscription feed.
      * [[SkipMode.Freeze]] (default) keeps the socket open but drops events while
      * skipped; [[SkipMode.Unsubscribe]] closes it while skipped and reopens on resume.
      */
    def subscribe[D](call: ApolloCall[D], skip: Signal[Boolean], mode: SkipMode = SkipMode.Freeze)(
        using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- Signal.initRef[QueryState[D]](QueryState.Loading)
            _   <- Fiber.init(ApolloSignal.driveGated(call.stream, ref, skip, mode))
        yield ref

    // --- Client construction -------------------------------------------------

    /** The blessed effectful client constructor: configure a fresh
      * [[ApolloClient.Builder]], validate it without throwing
      * ([[ApolloClient.Builder.buildResult]] → `Abort`), and acquire the built client
      * on the current `Scope` (so its socket is released on teardown) — a `def init`
      * shape returning `… < (Async & Scope & Abort[ApolloConfigException])`,
      * mirroring `HttpServer.init`.
      *
      * {{{
      * Apollo.client(_.serverUrl(url).webSocketServerUrl(wsUrl))
      * }}}
      */
    def client(
        configure: ApolloClient.Builder => ApolloClient.Builder
    )(using Frame): ApolloClient < (Async & Scope & Abort[ApolloConfigException]) =
        Abort.get(configure(ApolloClient.builder()).buildResult()).map(ApolloClientResource.acquire(_))

    /** The same construction as [[client]], packaged as a [[kyo.Layer]] so the client
      * is provided once at the app root through `Env[ApolloClient]` (the single blessed
      * provision channel) rather than threaded by hand.
      */
    def clientLayer(
        configure: ApolloClient.Builder => ApolloClient.Builder
    )(using Frame): Layer[ApolloClient, Async & Scope & Abort[ApolloConfigException]] =
        Layer(client(configure))

end Apollo
