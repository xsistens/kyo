package kyo.apollo

import kyo.*
import kyo.apollo.cache.normalized.ApolloStore
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.EmbeddedFragment
import kyo.apollo.cache.normalized.api.EntityFragment
import kyo.apollo.cache.normalized.api.Fragment
import kyo.apollo.cache.normalized.apolloStore
import kyo.apollo.cache.normalized.normalizedStore
import kyo.apollo.cache.normalized.watch
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloParseException
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import scala.NamedTuple.AnyNamedTuple
import scala.annotation.tailrec

/** The `Apollo` namespace — the constructor entry points of the `kyo-ui` binding.
  *
  * Every operation that turns a prepared [[ApolloCall]] (or a [[Fragment]], or a
  * client configuration) into a running/reactive result lives here as
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
                        call.requestBuilder.executionContext.get(ErrorPolicy).getOrElse(ErrorPolicy.Default)
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
      * is released. Until then the keys of the last read stay retained in the store,
      * so a garbage collection keeps what the signal shows. The [[ApolloClient]] is
      * taken as a `given`.
      */
    def fragment[D](fragment: Fragment[D], cacheKey: CacheKey)(using
        client: ApolloClient,
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[Maybe[D]] < (Async & Scope) =
        val store = client.apolloStore
        // The denormalized value and the keys it depended on, or Absent and just the
        // entity key on a cache miss. A decode defect is not a miss: it stays a panic.
        val read: (Maybe[D], Set[CacheKey]) < Sync =
            Abort.recover[CacheReadFailure](_ => (Maybe.empty[D], Set(cacheKey)))(
                store.readFragmentWithKeys(fragment, cacheKey).map((data, keys) => (Maybe(data), keys + cacheKey))
            )
        read.map { (initial, initialKeys) =>
            Signal.initRef[Maybe[D]](initial).map { ref =>
                watchStore(store, initialKeys)(read.map(Present(_)))(next => ref.set(next)).andThen(ref)
            }
        }
    end fragment

    /** Reactively read a masked fragment through its spread-produced ref — the
      * masked `useFragment`, and the ONLY door to a ref's contents: the fields a
      * [[EntityFragment]] selected are `private[apollo]` on the ref, so a parent
      * can pass it here (or to the component that declared the fragment) but never
      * read through it.
      *
      * No `Maybe`, unlike the [[Fragment]]+[[CacheKey]] overload: the ref captured
      * the response slice the fragment selected, so there is always something to
      * render — the signal seeds from the entity record when the cache has it and
      * from the ref's own slice when it does not (evicted, a type the store's key
      * generator gives no identity, or a cache-less client). Later cache changes
      * touching a dependent key re-read and re-emit; a re-read that misses keeps the
      * last value rather than blanking a working view. A slice that does not decode
      * as the fragment's fields aborts with an [[ApolloParseException]].
      */
    def fragment[Origin, D <: AnyNamedTuple](ref: EntityFragment[Origin, D]#Ref)(using
        client: ApolloClient,
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < (Async & Scope & Abort[ApolloParseException]) =
        // A cache-less client has no store to watch — the ref's captured slice IS
        // the data, so the signal degenerates to a constant.
        client.normalizedStore match
            case Absent         => Abort.get(ref.decoded).map(Signal.initRef[D](_))
            case Present(store) => fragmentSignal(ref, store)
    end fragment

    private def fragmentSignal[Origin, D <: AnyNamedTuple](
        ref: EntityFragment[Origin, D]#Ref,
        store: ApolloStore
    )(using
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < (Async & Scope & Abort[ApolloParseException]) =
        // The ref's record key comes from the generator that normalized the response.
        Abort.run[CacheReadFailure](store.keyOf(ref.typeName, ref.raw)).map {
            case Result.Success(key) =>
                // The record's value and the keys it depended on; Absent on a miss.
                val read: Maybe[(D, Set[CacheKey])] < Sync =
                    Abort.recover[CacheReadFailure](_ => Maybe.empty[(D, Set[CacheKey])])(
                        store.readFragmentWithKeys(ref.definition.cacheFragment, key)
                            .map((data, keys) => Maybe((data, keys + key)))
                    )
                read.map {
                    case Present((data, keys)) => (data, keys)
                    case Absent                => Abort.get(ref.decoded).map((_, Set(key)))
                }.map { (initial, initialKeys) =>
                    Signal.initRef[D](initial).map { out =>
                        // A miss keeps the last value (an eviction mid-life) and the
                        // dependent keys, so a re-population re-emits.
                        watchStore(store, initialKeys)(read)(next => out.set(next)).andThen(out)
                    }
                }
            // The store's generator gives this type no identity, so there is no record
            // to watch: the ref's slice is the data.
            case Result.Failure(_)    => Abort.get(ref.decoded).map(Signal.initRef[D](_))
            case Result.Panic(defect) => Abort.panic(defect)
        }
    end fragmentSignal

    /** Keep a fragment signal current with `store`: every change that touches a key
      * the last read depended on runs `reread`, which answers the next value and its
      * keys (handed to `publish` and watched from then on), or `Absent` to keep the
      * last value and the keys. The watched keys start at `initialKeys` and stay
      * retained in the store until the enclosing `Scope` closes, which also drops the
      * change listener and ends the consumer.
      */
    private def watchStore[V](store: ApolloStore, initialKeys: Set[CacheKey])(
        reread: Maybe[(V, Set[CacheKey])] < Sync
    )(publish: V => Unit < Sync)(using Frame): Unit < (Async & Scope) =
        RetainedKeys.init(store, initialKeys).map { retained =>
            Channel.initUnscoped[Set[CacheKey]](Int.MaxValue).map { channel =>
                val consume =
                    channel.streamUntilClosed().fold(initialKeys) { (watched, changed) =>
                        if !changed.exists(watched) then watched
                        else
                            reread.map {
                                case Present((next, keys)) => retained.hold(keys).andThen(publish(next)).andThen(keys)
                                case Absent                => watched
                            }
                    }
                // Closing the channel ends `streamUntilClosed`, so the consumer completes.
                Scope
                    .ensure(channel.close)
                    .andThen(store.addChangedKeysListener(changed => Abort.run[Closed](channel.offer(changed)).unit))
                    .andThen(UI.fork(consume))
                    .unit
            }
        }
    end watchStore

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
      * one or its type has no identity in the store. A cache-less client has no store
      * to watch, so the values are the refs' slices and only the list moves. A slice
      * that does not decode aborts the seed, or fails the driver later, with an
      * [[ApolloParseException]].
      */
    private def fragmentList[Origin, D <: AnyNamedTuple, A, B](items: Signal[Seq[A]])(
        refOf: A => Maybe[EntityFragment[Origin, D]#Ref]
    )(row: (A, Maybe[D]) => B)(using
        client: ApolloClient,
        frame: Frame,
        canEqualD: CanEqual[D, D],
        canEqualB: CanEqual[B, B]
    ): Signal[Seq[B]] < (Async & Scope & Abort[ApolloParseException]) =
        val maybeStore = client.normalizedStore

        // What one element reads from the store. The reads are effects, so every row
        // is read first and the projection over the reads stays pure.
        def readRow(a: A): Apollo.RowRead[Origin, D] < Sync =
            refOf(a) match
                case Absent => Apollo.RowRead.Plain()
                case Present(ref) =>
                    maybeStore match
                        case Absent => Apollo.RowRead.Unkeyed(ref)
                        case Present(store) =>
                            Abort.run[CacheReadFailure](store.keyOf(ref.typeName, ref.raw)).map {
                                case Result.Success(key) =>
                                    Abort.recover[CacheReadFailure](_ => Apollo.RowRead.Miss(key, ref))(
                                        store.readFragmentWithKeys(ref.definition.cacheFragment, key)
                                            .map((data, keys) => Apollo.RowRead.Hit(key, data, keys + key))
                                    )
                                case Result.Failure(_)    => Apollo.RowRead.Unkeyed(ref)
                                case Result.Panic(defect) => Abort.panic(defect)
                            }

        def reproject(as: Seq[A], last: Map[CacheKey, D]): Apollo.FragmentList[D, B] < (Sync & Abort[ApolloParseException]) =
            Kyo.foreach(as)(a => readRow(a).map((a, _))).map(reads => Abort.get(Apollo.projectRows(reads, last)(row)))

        items.current.map { initial =>
            reproject(initial, Map.empty).map { seeded =>
                Signal.initRef[Seq[B]](seeded.rows).map { out =>
                    maybeStore match
                        case Absent =>
                            // The list driver alone. Seeded with `initial`, so the value
                            // already projected above is not projected a second time.
                            UI.fork(items.observe(Present(initial), Signal.defaultRepairInterval) { as =>
                                reproject(as, Map.empty).map(next => out.set(next.rows))
                            }).andThen(out)
                        case Present(store) =>
                            RetainedKeys.init(store, seeded.watched).map { retained =>
                                // `Absent` says the list moved, `Present` names the keys a
                                // write changed. ONE consumer re-projects for both, so two
                                // re-projections never race to install their key sets.
                                Channel.initUnscoped[Maybe[Set[CacheKey]]](Int.MaxValue).map { channel =>
                                    def offer(event: Maybe[Set[CacheKey]]): Unit < Sync =
                                        Abort.run[Closed](channel.offer(event)).unit
                                    val consume =
                                        channel.streamUntilClosed().fold(seeded) { (state, event) =>
                                            val affected = event match
                                                case Absent           => true
                                                case Present(changed) => changed.exists(state.watched)
                                            if !affected then state
                                            else
                                                items.current.map(as => reproject(as, state.last)).map { next =>
                                                    retained.hold(next.watched).andThen(out.set(next.rows)).andThen(next)
                                                }
                                            end if
                                        }
                                    Scope
                                        .ensure(channel.close)
                                        .andThen(store.addChangedKeysListener(changed => offer(Present(changed))))
                                        .andThen(UI.fork(consume))
                                        .andThen(UI.fork(items.observe(Present(initial), Signal.defaultRepairInterval)(_ =>
                                            offer(Absent)
                                        )))
                                        .andThen(out)
                                }
                            }
                    end match
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
    ): Signal[Seq[B]] < (Async & Scope & Abort[ApolloParseException]) =
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
    ): Signal[Seq[D]] < (Async & Scope & Abort[ApolloParseException]) =
        // `refOf` answers `Present` for every element, so the core never reaches the
        // Absent branch; there is no value of `D` to put there and none is needed.
        fragmentList[Origin, D, fragment.Ref, D](refs)(Present(_)) { (_, d) =>
            d.getOrElse(throw new NoSuchElementException("Apollo.fragments: a ref opened to nothing"))
        }

    /** Read a masked embedded fragment's ref — the value-carrying counterpart of
      * the entity overload. The object has no cache identity, so there is nothing
      * to watch: the signal is constant, and updates arrive the way the value did —
      * through the parent's reactivity re-rendering the child with a fresh ref. A
      * value that does not decode as the fragment's fields aborts with an
      * [[ApolloParseException]].
      */
    def fragment[Origin, D <: AnyNamedTuple](ref: EmbeddedFragment[Origin, D]#Ref)(using
        frame: Frame,
        canEqual: CanEqual[D, D]
    ): Signal[D] < (Sync & Abort[ApolloParseException]) =
        Abort.get(ref.value).map(Signal.initRef[D](_))

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
            val advance: ((C, D) => Maybe[C]) => (Unit < (Async & Abort[ApolloException])) =
                reduce =>
                    cursors.current.map { c =>
                        state.current.map { qs =>
                            val next: Maybe[C] = PaginatedQuery.dataOf(qs) match
                                case Present(d) => reduce(c, d)
                                case Absent     => Absent
                            next match
                                case Present(c2) => cursors.set(c2).andThen(refetch(c2))
                                case Absent      => ()
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
            val advance: ((C, D) => Maybe[C]) => (Unit < (Async & Abort[ApolloException])) =
                reduce =>
                    values.current.map {
                        case Absent => ()
                        case Present(v) =>
                            cursors.current.map { c =>
                                ref.current.map { qs =>
                                    val next: Maybe[C] = PaginatedQuery.dataOf(qs) match
                                        case Present(d) => reduce(c, d)
                                        case Absent     => Absent
                                    next match
                                        case Present(c2) =>
                                            cursors.set(c2).andThen(
                                                page(v, c2).fetchPolicy(FetchPolicy.NetworkOnly).data.unit
                                            )
                                        case Absent => ()
                                    end match
                                }
                            }
                    }
            new PaginatedQuery(ref, advance)
        end for
    end paginatedQuery

    /** Prepare a paginated query with a single connection — the sugar over the
      * general form. `page(Absent)` is the first page; `page(Present(cursor))` each
      * next one. Yields a flat [[PaginatedQueryHandle]] whose `fetchMore` advances it.
      */
    def paginatedQuery[D](page: Maybe[String] => ApolloCall[D])(
        cursorOf: D => Maybe[String]
    )(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQueryHandle[D] < (Async & Scope) =
        paginatedQuery[D, Maybe[String]](initial = Absent)(page).map(_.singleConnection(cursorOf))

    /** The single-connection sugar with a live `skip` — [[paginatedQuery]]'s gate,
      * yielding the flat handle rather than making the caller re-assemble one.
      */
    def paginatedQuery[D](
        page: Maybe[String] => ApolloCall[D],
        skip: Signal[Boolean],
        mode: SkipMode
    )(
        cursorOf: D => Maybe[String]
    )(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]],
        CanEqual[D, D]
    ): PaginatedQueryHandle[D] < (Async & Scope) =
        paginatedQuery[D, Maybe[String]](Absent, skip, mode)(page).map(_.singleConnection(cursorOf))

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

    /** The blessed effectful client constructor: create a client for `config` owned
      * by the current `Scope` (so its socket is released on teardown) — a `def init`
      * shape, mirroring `HttpServer.init`. The configuration is a value whose one
      * required field is `serverUrl`, so an incomplete one does not compile.
      *
      * {{{
      * Apollo.client(ApolloClient.Config(url).webSocketServerUrl(wsUrl))
      * }}}
      */
    def client(config: ApolloClient.Config)(using Frame): ApolloClient < (Sync & Scope) =
        ApolloClientResource.acquire(config)

    /** The same construction as [[client]], packaged as a [[kyo.Layer]] so the client
      * is provided once at the app root through `Env[ApolloClient]` (the single blessed
      * provision channel) rather than threaded by hand.
      */
    def clientLayer(config: ApolloClient.Config)(using Frame): Layer[ApolloClient, Sync & Scope] =
        Layer(client(config))

    /** What [[fragments]] carries between emissions: the union of the dependent keys of
      * every row (so a change that touches none of them costs one set intersection), the
      * last value read per entity record (so a re-read that misses keeps a working view
      * instead of blanking it), and the projected rows themselves.
      */
    final private case class FragmentList[D, B](watched: Set[CacheKey], last: Map[CacheKey, D], rows: Seq[B])

    /** What one list element read from the store: no ref ([[Plain]]), a ref whose type
      * has no identity in the store or whose client has no store ([[Unkeyed]]: its
      * slice is the data), or a ref whose record was found ([[Hit]], with the keys the
      * read depended on) or missed ([[Miss]]).
      */
    private enum RowRead[Origin, D <: AnyNamedTuple]:
        case Plain()
        case Unkeyed(ref: EntityFragment[Origin, D]#Ref)
        case Hit(key: CacheKey, data: D, keys: Set[CacheKey])
        case Miss(key: CacheKey, ref: EntityFragment[Origin, D]#Ref)
    end RowRead

    /** Project a list's rows from what they read: the keys to watch, the values to
      * remember (only for rows still in the list, so a list that scrolls forever does
      * not accumulate the ones that left), and each element's view value. A row whose
      * re-read missed shows the last value its record had; one that never had a value
      * shows its ref's own slice, which fails the projection if it does not decode.
      */
    private def projectRows[Origin, D <: AnyNamedTuple, A, B](
        reads: Seq[(A, RowRead[Origin, D])],
        last: Map[CacheKey, D]
    )(row: (A, Maybe[D]) => B)(using Frame): Result[ApolloParseException, FragmentList[D, B]] =
        val watched = reads.foldLeft(Set.empty[CacheKey]) { case (keys, (_, read)) =>
            read match
                case RowRead.Hit(_, _, dependent) => keys ++ dependent
                case RowRead.Miss(key, _)         => keys + key
                case _                            => keys
        }
        val remembered = reads.foldLeft(Map.empty[CacheKey, D]) { case (known, (_, read)) =>
            read match
                case RowRead.Hit(key, data, _) => known.updated(key, data)
                case RowRead.Miss(key, _)      => last.get(key).fold(known)(known.updated(key, _))
                case _                         => known
        }
        def present(a: A, data: Result[ApolloParseException, D]): Result[ApolloParseException, B] =
            data.map(d => row(a, Present(d)))
        Result.collect(reads.map { (a, read) =>
            read match
                case RowRead.Plain()         => Result.succeed(row(a, Absent))
                case RowRead.Unkeyed(ref)    => present(a, ref.decoded)
                case RowRead.Hit(_, data, _) => present(a, Result.succeed(data))
                case RowRead.Miss(key, ref)  => present(a, remembered.get(key).fold(ref.decoded)(Result.succeed(_)))
        }).map(rows => FragmentList(watched, remembered, rows))
    end projectRows

    /** The keys a fragment signal watches, retained in its store for as long as the
      * signal lives: a garbage collection keeps the records the last read depended on
      * even when no operation root reaches them any more.
      *
      * `held` is `Absent` once the enclosing `Scope` has closed. A [[hold]] moves the
      * retain hand over hand through [[ApolloStore.swapRetained]], so a key the signal
      * still watches never counts as unretained, and a hold that arrives after the
      * teardown retains nothing.
      */
    final private class RetainedKeys private (store: ApolloStore, held: AtomicRef.Unsafe[Maybe[Set[CacheKey]]]):

        def hold(keys: Set[CacheKey])(using Frame): Unit < Sync =
            Sync.Unsafe.defer(discard(store.swapRetained(keys)(install(keys))))

        @tailrec private def install(keys: Set[CacheKey])(using AllowUnsafe): Maybe[Set[CacheKey]] =
            val current = held.get()
            current match
                case Absent => Absent
                case Present(replaced) =>
                    if held.compareAndSet(current, Present(keys)) then Present(replaced) else install(keys)
            end match
        end install

        private def release(using Frame): Unit < Sync =
            Sync.Unsafe.defer(held.getAndSet(Absent).foreach(keys => store.releaseRetained(keys)))
    end RetainedKeys

    private object RetainedKeys:
        /** Retain `keys` until the enclosing `Scope` closes. */
        def init(store: ApolloStore, keys: Set[CacheKey])(using Frame): RetainedKeys < (Sync & Scope) =
            val nothingHeld: Maybe[Set[CacheKey]] = Present(Set.empty)
            Sync.Unsafe.defer(new RetainedKeys(store, AtomicRef.Unsafe.init(nothingHeld))).map { retained =>
                Scope.ensure(retained.release).andThen(retained.hold(keys)).andThen(retained)
            }
        end init
    end RetainedKeys

end Apollo
