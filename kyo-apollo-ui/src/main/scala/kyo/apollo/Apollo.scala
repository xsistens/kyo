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
  * raw stream views (`call.watch()`/`call.watchStream`), and the
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
                    .andThen(UI.fork(consume))
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
                    .andThen(UI.fork(consume))
                    .andThen(signalRef)
            }
        }
    end fragmentSignal

    /** Reactively read a LIST of masked refs — [[fragment]] for a collection, and
      * the shape a table of masked rows actually has.
      *
      * `refOf` says which elements carry a ref, so a list that mixes masked entities
      * with plain ones (a playlist of tracks and podcast episodes) stays ONE list:
      * `Absent` elements pass through untouched, and `row` builds the view value from
      * the element together with the data its ref opened. The result is a single
      * signal, so nothing downstream has to zip a list of values back against the
      * list it came from and get the glitch window wrong.
      *
      * ==Why this is not `Kyo.foreach(refs)(fragment)`==
      *
      * A per-ref [[fragment]] owns a store listener and a channel, and therefore a
      * `Scope` that has to live exactly as long as that row does. Nothing outside a
      * mount owns a lifetime per list element, so a growing list — one `fetchMore`
      * appending a page — leaves a caller opening every row's watcher again on each
      * append, or opening them inside a per-row `UI.mounted` and giving up on ever
      * holding the rows as data.
      *
      * This reads the whole list through ONE listener instead. There is nothing per
      * row to own: the dependent keys of every row are watched together, a change
      * that touches none of them costs a set intersection, and one that touches some
      * re-reads the list. Adding rows changes what is watched, not what is
      * subscribed.
      *
      * Total in the same way [[fragment]] is: a row seeds from the entity record when
      * the cache has it, from the last value it read when a later re-read misses (an
      * eviction mid-life), and from the ref's own captured slice when it never had
      * one. A cache-less client has no store to watch, so the values are the refs'
      * slices and only the list moves.
      */
    private def fragmentList[Origin, D <: AnyNamedTuple, A, B](items: Signal[Seq[A]])(
        refOf: A => Maybe[EntityFragment[Origin, D]#Ref]
    )(row: (A, Maybe[D]) => B)(using
        client: ApolloClient,
        frame: Frame,
        canEqualD: CanEqual[D, D],
        canEqualB: CanEqual[B, B]
    ): Signal[Seq[B]] < (Async & Scope) =
        val maybeStore =
            try Some(client.apolloStore)
            catch case _: IllegalStateException => None

        // One row: the store's value plus the keys it depended on, or None on a miss.
        def read(ref: EntityFragment[Origin, D]#Ref): Option[(D, Set[String])] =
            maybeStore.flatMap { store =>
                try
                    val (data, keys) = store.readFragmentWithKeys(ref.definition.cacheFragment, ref.key)
                    Some((data, keys + ref.key.key))
                catch case _: CacheMissException => None
            }

        // Pure, so it can run inside `AtomicRef.updateAndGet` (which may retry) and so
        // the two drivers below cannot interleave a half-updated key set.
        def project(as: Seq[A])(prev: Apollo.FragmentList[D, B]): Apollo.FragmentList[D, B] =
            var keys  = Set.empty[String]
            var known = prev.last
            val rows = as.map { a =>
                refOf(a) match
                    case Absent => row(a, Absent)
                    case Present(ref) =>
                        val id = ref.key.key
                        val d = read(ref) match
                            case Some((data, ks)) =>
                                keys = keys ++ ks
                                known = known.updated(id, data)
                                data
                            case None =>
                                keys = keys + id
                                known.getOrElse(id, ref.decoded)
                        row(a, Present(d))
            }
            // Values are remembered only for rows still in the list, so a list that
            // scrolls forever does not accumulate the ones that left.
            val live = as.flatMap(a => refOf(a).map(_.key.key)).toSet
            Apollo.FragmentList(keys, known.view.filterKeys(live).toMap, rows)
        end project

        items.current.map { initial =>
            AtomicRef.init(project(initial)(Apollo.FragmentList(Set.empty, Map.empty, Seq.empty[B]))).map { state =>
                state.get.map { seeded =>
                    Signal.initRef[Seq[B]](seeded.rows).map { out =>
                        def reproject: Unit < Async =
                            items.current.map(as => state.updateAndGet(project(as)).map(s => out.set(s.rows)))

                        // The list driver. Seeded with `initial`, so the value already
                        // projected above is not projected a second time.
                        val follow = items.observe(Present(initial), Signal.defaultRepairInterval)(_ => reproject)

                        maybeStore match
                            case None => UI.fork(follow).andThen(out)
                            case Some(store) =>
                                Channel.initUnscoped[Set[String]](Int.MaxValue).map { channel =>
                                    given AllowUnsafe = AllowUnsafe.embrace.danger
                                    val unsubscribe = store.addChangedKeysListener { changed =>
                                        val _ = channel.unsafe.offer(changed)
                                    }
                                    val consume = channel.streamUntilClosed().foreach { changed =>
                                        state.get.map { s =>
                                            if changed.exists(s.watched) then reproject
                                            else Sync.defer(())
                                        }
                                    }
                                    Scope
                                        .ensure(Sync.defer {
                                            unsubscribe()
                                            val _ = channel.unsafe.close()
                                        })
                                        .andThen(UI.fork(consume))
                                        .andThen(UI.fork(follow))
                                        .andThen(out)
                                }
                        end match
                    }
                }
            }
        }
    end fragmentList

    /** [[fragments]] over a list whose elements are not bare refs — a list of table
      * rows, where each row CARRIES a ref (or does not).
      *
      * `refOf` says which elements carry one, so a list that mixes masked entities
      * with plain ones (a playlist of tracks and podcast episodes) stays ONE list and
      * ONE signal: `row` builds a view value from an element and the data its ref
      * opened, `plain` from an element that carries no ref. Two functions rather than
      * one taking a `Maybe`, so neither of them has a case that cannot happen.
      *
      * A separate name and not an overload of [[fragments]]: an overload cannot be
      * resolved before its function arguments are typed, and these two are exactly the
      * arguments whose parameter types the caller wants inferred.
      */
    def fragmentRows[Origin, D <: AnyNamedTuple, A, B](items: Signal[Seq[A]])(
        refOf: A => Maybe[EntityFragment[Origin, D]#Ref]
    )(row: (A, D) => B, plain: A => B)(using
        client: ApolloClient,
        frame: Frame,
        canEqualD: CanEqual[D, D],
        canEqualB: CanEqual[B, B]
    ): Signal[Seq[B]] < (Async & Scope) =
        fragmentList(items)(refOf) { (a, d) =>
            d match
                case Present(v) => row(a, v)
                case Absent     => plain(a)
        }

    /** [[fragmentRows]] over a list that is nothing but refs.
      *
      * The fragment comes first and the refs read their type off it
      * (`fragments(TrackRow.fields)(refs)`). A `Signal` is invariant, so a
      * `Signal[Seq[TrackRow.fields.Ref]]` does not on its own tell the compiler what
      * `Origin` and `D` are the way a single `ref` argument does — naming the
      * definition once is what makes the element type follow.
      */
    def fragments[Origin, D <: AnyNamedTuple](fragment: EntityFragment[Origin, D])(
        refs: Signal[Seq[fragment.Ref]]
    )(using
        client: ApolloClient,
        frame: Frame,
        canEqualD: CanEqual[D, D]
    ): Signal[Seq[D]] < (Async & Scope) =
        // `refOf` answers `Present` for every element, so the core never reaches the
        // Absent branch; there is no value of `D` to put there and none is needed.
        fragmentList[Origin, D, fragment.Ref, D](refs)(Present(_)) { (_, d) =>
            d.getOrElse(throw new NoSuchElementException("Apollo.fragments: a ref opened to nothing"))
        }

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
        watchSignal(page(initial)).map(pagedOver(initial, page))

    /** [[paginatedQuery]] with a live `skip` — the same gate [[query]] and
      * [[watchSignal]] carry, and the same retention: while `skip` is `true` the
      * watcher freezes ([[SkipMode.Freeze]]) or is torn down
      * ([[SkipMode.Unsubscribe]]), and the last emitted page **stays** in
      * [[PaginatedQuery.state]]. A tab parked by a URL-derived signal therefore
      * repaints what it had instead of flashing its loading line.
      *
      * Not the same gate as the `values` form below, though the two read as siblings:
      * `values = Absent` says "this query has no variables yet" and parks the state at
      * [[QueryState.Idle]], discarding what was on screen; `skip = true` says "not
      * now", and keeps it. Reach for `skip` to PARK a query, for `values` to RE-POINT
      * one.
      *
      * The cursor state is deliberately not reset on resume (a `values` switch does
      * reset it). The watcher always reads `page(initial)`, whose cache slot still
      * holds every page merged into it, so resuming repaints the whole accumulated
      * list and the next `fetchMore` continues where it stopped.
      *
      * `fetchMore` stays live while skipped, the way [[query]]'s `refetch` does: the
      * gate is on the watcher, not on the imperative verbs. Under
      * [[SkipMode.Unsubscribe]] its write-back lands in the cache unobserved and is
      * picked up by the re-opened watcher on resume.
      *
      * `mode` is explicit here, where [[query]] and [[watchSignal]] default it to
      * [[SkipMode.Freeze]]: Scala allows default arguments on only one alternative of
      * an overloaded method, and `paginatedQuery` has two that take a `skip`. Naming
      * the mode is no loss — parking a route's tab wants [[SkipMode.Unsubscribe]],
      * which is not the default anywhere else either.
      */
    def paginatedQuery[D, C](initial: C, skip: Signal[Boolean], mode: SkipMode)(
        page: C => ApolloCall[D]
    )(using
        Frame,
        CanEqual[C, C],
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQuery[D, C] < (Async & Scope) =
        watchSignal(page(initial), skip, mode).map(pagedOver(initial, page))

    /** The cursor cell and the `fetchMore` reducer every [[paginatedQuery]] form over
      * a fixed operation shares — what differs between them is only how `state` is
      * driven. (The `values` form keeps its own copy: its reducer additionally has to
      * read the current variables, and is a no-op without them.)
      */
    private def pagedOver[D, C](initial: C, page: C => ApolloCall[D])(
        state: Signal[QueryState[D]]
    )(using
        Frame,
        CanEqual[C, C],
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQuery[D, C] < Sync =
        Signal.initRef[C](initial).map { cursors =>
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

    /** Prepare a paginated query whose operation **variables** are live — the shape a
      * search page needs, where a new term must restart pagination without the page
      * being rebuilt.
      *
      * Pagination and variables are two different axes and are driven differently, the
      * way react-apollo drives them:
      *
      *   - The watcher is opened on `page(v, initial)` — the FIRST page's cursors,
      *     always. It is never re-pointed at a later cursor; `fetchMore` is a separate
      *     `NetworkOnly` shot whose write-back merges into the same cache slot (per the
      *     connection's [[kyo.apollo.cache.normalized.api.ConnectionFieldPolicy]]) and
      *     re-emits [[PaginatedQuery.state]] with every page loaded so far.
      *   - A change in `values` re-opens that watcher and resets the cursor state to
      *     `initial` in the same step. The DATA reset needs no help: a different value
      *     produces a different cache field key, so the new connection starts empty —
      *     and returning to an earlier value repaints from the cache with the pages it
      *     had already accumulated.
      *
      * `Absent` parks the watcher at [[QueryState.Idle]] and makes every `fetchMore` a
      * no-op — the state a page holds before its parameter exists, which DISCARDS
      * whatever was on screen. To park a query that already painted and have it
      * repaint on return, use the `skip` overload above; the two gates are not
      * interchangeable.
      */
    def paginatedQuery[D, C, V](values: Signal[Maybe[V]])(initial: C)(page: (V, C) => ApolloCall[D])(
        using
        Frame,
        CanEqual[C, C],
        CanEqual[V, V],
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQuery[D, C] < (Async & Scope) =
        for
            cursors <- Signal.initRef[C](initial)
            ref     <- values.currentWith(v0 => Signal.initRef[QueryState[D]](ApolloSignal.seedFor(v0)))
            _ <- UI.fork(
                ApolloSignal.driveSwitching(
                    values,
                    (v: V) => page(v, initial).watch(),
                    ref,
                    _ => cursors.set(initial)
                )
            )
        yield
            val advance: ((C, D) => Option[C]) => (Unit < (Async & Abort[ApolloException])) =
                reduce =>
                    values.current.map {
                        case Absent => ()
                        case Present(v) =>
                            cursors.current.map { c =>
                                ref.current.map { qs =>
                                    val next: Option[C] = PaginatedQuery.dataOf(qs) match
                                        case Some(d) => reduce(c, d)
                                        case None    => None
                                    next match
                                        case Some(c2) =>
                                            cursors.set(c2).andThen(
                                                page(v, c2).fetchPolicy(FetchPolicy.NetworkOnly).data.unit
                                            )
                                        case None => ()
                                    end match
                                }
                            }
                    }
            new PaginatedQuery(ref, advance)
        end for
    end paginatedQuery

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
        paginatedQuery[D, Option[String]](initial = None)(page).map(_.singleConnection(cursorOf))

    /** The single-connection sugar with a live `skip` — [[paginatedQuery]]'s gate,
      * yielding the flat handle rather than making the caller re-assemble one.
      */
    def paginatedQuery[D](
        page: Option[String] => ApolloCall[D],
        skip: Signal[Boolean],
        mode: SkipMode
    )(
        cursorOf: D => Option[String]
    )(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQueryHandle[D] < (Async & Scope) =
        paginatedQuery[D, Option[String]](None, skip, mode)(page).map(_.singleConnection(cursorOf))

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
            _ <- UI.fork(Scope.run(call.watch().foreach(resp => ApolloSignal.push(ref, resp))))
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
            _   <- UI.fork(ApolloSignal.driveGated(call.watch(), ref, skip, mode))
        yield ref

    /** [[watchSignal]] over **live variables** — react-apollo's variables change on a
      * mounted `useQuery`, where the hook instance survives and only the operation is
      * re-pointed.
      *
      * One watcher at a time, re-opened whenever `values` emits a genuinely different
      * value; `Absent` parks it (nothing subscribed, state [[QueryState.Idle]]), which
      * is the signal-shaped `skip` for a parameter that does not exist yet. Across a
      * `Present` → `Present` switch the previous data stays in the signal until the new
      * response arrives, so a consumer re-renders from stale content instead of
      * flashing a placeholder.
      *
      * This is what lets a component take its parameter as a `Signal` instead of a
      * captured value — and therefore stop putting that value in its `UI.mounted` key,
      * where every change costs a full teardown of the subtree, its scope and its DOM.
      */
    def watchSignal[D, V](values: Signal[Maybe[V]])(call: V => ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D],
        CanEqual[V, V]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- values.currentWith(v0 => Signal.initRef[QueryState[D]](ApolloSignal.seedFor(v0)))
            _ <- UI.fork(
                ApolloSignal.driveSwitching(values, (v: V) => call(v).watch(), ref, _ => (): Unit < Sync)
            )
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
            UI.fork(pollLoop).andThen(signal)
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
            UI.fork(pollLoop).andThen(signal)
        }

    /** Watch a **subscription** as a live [[Signal]] of [[QueryState]] — the
      * react-apollo `useSubscription` shape. Seeds [[QueryState.Loading]], then
      * re-emits the projection of each event; the socket is torn down when the
      * enclosing `Scope` is released.
      *
      * This is also the "subscription that exists only to keep the normalized cache
      * warm" pattern: the drain is what gives the shared socket a consumer, and each
      * reply normalizes into the store and re-emits every dependent `watchSignal`.
      * Ignore the returned signal and the cache side effect is all you get; observe
      * it and the terminal state (a [[QueryState.Failure]], or a feed that simply
      * stopped) stays a value you can act on instead of a silent gap.
      */
    def subscribe[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): Signal[QueryState[D]] < (Async & Scope) =
        for
            ref <- Signal.initRef[QueryState[D]](QueryState.Loading)
            _   <- UI.fork(Scope.run(call.stream.foreach(resp => ref.set(ApolloSignal.project(resp)))))
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
            _   <- UI.fork(ApolloSignal.driveGated(call.stream, ref, skip, mode))
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

    /** What [[fragments]] carries between emissions: the union of the dependent keys of
      * every row (so a change that touches none of them costs one set intersection), the
      * last value read per row (so a re-read that misses keeps a working view instead of
      * blanking it), and the projected rows themselves.
      *
      * One value in one `AtomicRef` and not three vars, because the list driver and the
      * store driver are separate fibers and either may project.
      */
    final private case class FragmentList[D, B](watched: Set[String], last: Map[String, D], rows: Seq[B])

end Apollo
