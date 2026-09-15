package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Async` / `Abort` / `Scope` / `Signal` / `Frame`.
import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.api.Operation
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.cache.normalized.apolloStore
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse

/** The **boilerplate-free surface** of the `kyo-ui` binding (Phase 09, Task 6).
  *
  * Everything an app needs arrives through a single `import kyo.apollo.*`:
  *
  *   - the effect form ([[ApolloEffect]]) — `call.data` / `call.response`;
  *   - the reactive form ([[ApolloSignal]]) — `call.watchSignal` /
  *     `call.watchStream`;
  *   - and this file's [[useQuery]] helper, a react-apollo-style pairing of the
  *     live [[Signal]] with a `refetch` effect.
  *
  * So app code writes, with no per-operation wiring:
  *
  * {{{
  * import kyo.apollo.*
  *
  * // one-shot effect
  * val data = client.query(GetUser(id)).data
  *
  * // live reactive state + a refetch handle (react-apollo `useQuery` shape)
  * Scope.run:
  *   useQuery(client.query(GetUser(id))).map: q =>
  *     render(q.state)            // Signal[QueryState[User]]
  *     onClick(q.refetch)         // D < (Async & Abort[ApolloException])
  * }}}
  *
  * It is deliberately thin: [[useQuery]] is nothing but `call.watchSignal` for
  * the reactive state paired with `call.data` for `refetch`, so it introduces no
  * new coupling to `core` beyond the `call.watch()` cache-watcher stream the two
  * forms already drive off.
  */

/** The coarse-to-fine network activity of a live query — react-apollo's
  * `networkStatus`, reduced to the states meaningful for a single watched query.
  * Read off [[QueryHandle.networkStatus]] so a UI can tell an initial load apart
  * from a background refresh (and show a different indicator for each).
  */
enum NetworkStatus derives CanEqual:

    /** The first fetch is in flight — no data yet (react `loading`). */
    case Loading

    /** Data is present and no fetch is in flight (react `ready`). */
    case Ready

    /** A background `refetch` is in flight over existing data (react `refetch`). */
    case Refetch

    /** A background poll tick is in flight over existing data (react `poll`). */
    case Poll

    /** The last fetch failed (react `error`). */
    case Error
end NetworkStatus

object NetworkStatus:

    /** The status implied purely by the current [[QueryState]], with no in-flight
      * background activity overlaid: no data yet → [[Loading]]; data → [[Ready]]; a
      * failure → [[Error]].
      */
    private[kyo] def base(qs: QueryState[?]): NetworkStatus = qs match
        case QueryState.Idle              => NetworkStatus.Loading
        case QueryState.Loading           => NetworkStatus.Loading
        case QueryState.Success(_, _, _)  => NetworkStatus.Ready
        case QueryState.PartialData(_, _) => NetworkStatus.Ready
        case QueryState.Failure(_, _)     => NetworkStatus.Error
end NetworkStatus

/** The **projection-capable** surface of a live query — everything a data
  * projection can honestly carry: the reactive [[state]], the derived
  * [[networkStatus]], the typed [[refetch]], `Scope`-bound [[polling]], and
  * [[mapData]] itself.
  *
  * There are deliberately no `onCompleted`/`onError` callbacks (Apollo Client 4
  * removed them from `useQuery`/`useLazyQuery` for the same reason): on a live
  * query they fire per emission with no defined relationship to a render, and
  * [[state]] already carries every settle as a value. Tap it directly —
  * `state.current` / `state.next` is the primitive the callbacks were built on.
  * A one-shot mutation has no such pair either: [[MutationHandle.run]] returns the
  * effect, so success and failure compose at the call site (`run(i).map(use)`,
  * `Abort.run(run(i))`) instead of being handed to the handle.
  *
  * [[RawQueryHandle]] extends this with the operations a
  * projection CANNOT offer: `subscribeToMore` must write merged data back into the
  * store in the operation's RAW shape (a projection has no inverse to encode
  * through), and the imperative `startPolling`/`stopPolling` pair parks its loop
  * fiber for the handle's build-time `Scope` to clean up (a later-created
  * projection has no such hook — use [[polling]], which scopes itself).
  *
  * `networkStatus` overlays any in-flight background activity (a `refetch`) on top
  * of the status implied by `state`, via the shared `activity` cell the handle's
  * operations set while they run — so the UI can distinguish an initial
  * [[NetworkStatus.Loading]] from a background [[NetworkStatus.Refetch]] over
  * existing data.
  *
  * @param state        the [[Signal]] of [[QueryState]] that seeds `Loading`, emits
  *                     the first response, and re-emits on every subsequent cache
  *                     change — already projected on a mapped handle.
  * @param activity     the overlay cell: `Present(status)` while a background fetch
  *                     runs, `Absent` otherwise. SHARED with every projection of the
  *                     same handle, so they report one another's in-flight activity.
  * @param networkFetch the typed `NetworkOnly` fetch [[refetch]] and [[polling]]
  *                     run — the raw operation's fetch composed with the projection
  *                     on a mapped handle (a rejection rides the fetch's own
  *                     `Abort[ApolloException]` channel).
  */
class QueryHandle[D] private[apollo] (
    val state: Signal[QueryState[D]],
    private[apollo] val activity: Signal.SignalRef[Maybe[NetworkStatus]],
    private[apollo] val networkFetch: D < (Async & Abort[ApolloException])
)(using Frame, CanEqual[D, D]):

    /** Fine-grained network status: an in-flight background activity (from the
      * `activity` cell) wins, else the status implied by [[state]].
      */
    val networkStatus: Signal[NetworkStatus] =
        state.combineLatest(activity).map((qs, act) => act.getOrElse(NetworkStatus.base(qs)))

    /** Re-execute the operation over the network (`FetchPolicy.NetworkOnly`), which
      * rewrites the store and re-emits [[state]]; yields the typed data or an
      * [[kyo.apollo.exception.ApolloException]] on `Abort`. While it runs,
      * [[networkStatus]] reads [[NetworkStatus.Refetch]] (react's `refetch`). On a
      * mapped handle the yield is the PROJECTED data — a rejecting projection aborts
      * with its exception, on this same channel.
      */
    val refetch: D < (Async & Abort[ApolloException]) =
        withActivity(NetworkStatus.Refetch)(networkFetch)

    /** Run `fetch` with [[networkStatus]] overlaid as `status` for its duration,
      * clearing the overlay afterward on success OR failure (then re-raising).
      */
    private[apollo] def withActivity[A](status: NetworkStatus)(
        fetch: A < (Async & Abort[ApolloException])
    ): A < (Async & Abort[ApolloException]) =
        activity.set(Present(status)).andThen {
            Abort.run[ApolloException](fetch).map(r => activity.set(Absent).andThen(Abort.get(r)))
        }

    /** [[QueryState.mapData]] lifted onto the whole handle: derive a handle whose
      * [[state]] AND [[refetch]] carry the projected type, with `Abort.fail(e)`
      * rejecting into [[QueryState.Failure]] (state) / the `Abort` channel
      * (refetch). [[networkStatus]] stays coupled to the underlying operation via
      * the shared activity cell; [[polling]] and the callbacks work on the
      * projected values. See the class doc for why the result is a
      * plain [[QueryHandle]] and not a [[RawQueryHandle]].
      */
    def mapData[B](f: D => B < Abort[ApolloException])(using Frame, CanEqual[B, B]): QueryHandle[B] =
        // `state.map(_.mapData(f))`, not the Signal `mapData` extension: this file's
        // effect-level `mapData` extension shadows the Signal one within the package,
        // and Scala 3 does not fall back across same-name extension groups.
        QueryHandle(state.map(_.mapData(f)), activity, networkFetch.map(f))

    /** Background polling as a `Scope`-bound resource — the value-oriented,
      * `var`-free alternative to the imperative `startPolling` / `stopPolling` pair
      * on [[QueryHandle]]. Forks a loop that re-fetches every `interval`
      * (`NetworkOnly`, rewriting the store and re-emitting [[state]], with
      * [[networkStatus]] reading [[NetworkStatus.Poll]] per tick) and binds its
      * interruption to the enclosing `Scope`: polling stops automatically when the
      * `Scope` closes — there is no mutable cell and nothing to remember to stop. A
      * failed tick is swallowed (the loop keeps polling), like `startPolling`.
      * Scope polling composes naturally — poll for the lifetime of exactly the
      * block you want:
      *
      * {{{
      * useQuery(call).flatMap: q =>
      *   Scope.run(q.polling(2.seconds).andThen(renderFor(q.state)))  // polls only here
      * }}}
      */
    def polling(interval: Duration)(using Frame): Unit < (Async & Scope) =
        def loop: Unit < Async =
            Async.sleep(interval).andThen {
                Abort
                    .run[ApolloException](withActivity(NetworkStatus.Poll)(networkFetch))
                    .andThen(loop)
            }
        Fiber.initUnscoped(loop).map(f => Scope.ensure(f.interrupt.unit))
    end polling

    // The running poll fiber, if any (react-parity imperative polling). A plain cell
    // — single-threaded JS — driven by [[startPolling]] / [[stopPolling]]; its fiber
    // is cleaned up when the handle's enclosing `Scope` closes (see [[useQuery]]).
    private var pollFiberCell: Maybe[Fiber[Unit, Any]] = Absent

    /** Start (or restart) background polling — react's `startPolling`. Every
      * `interval`, re-run the handle's network fetch (rewriting the store and
      * re-emitting [[state]]), with [[networkStatus]] reading [[NetworkStatus.Poll]]
      * during each tick. A prior poll loop is stopped first, so calling this again
      * with a new `interval` just changes the cadence. Runs until [[stopPolling]] or
      * the handle's enclosing `Scope` closes. A failed tick is swallowed (the loop
      * keeps polling), mirroring react-apollo's resilient polling.
      *
      * Available on every handle: because it drives [[refetch]]'s own `networkFetch`,
      * it works on a [[mapData]]-projected handle too (polling the underlying
      * operation and re-projecting), so a mapped handle no longer silently loses the
      * imperative polling pair. The `Scope`-bound [[polling]] is the `var`-free
      * alternative when you can bound polling to a block.
      */
    def startPolling(interval: Duration)(using Frame): Unit < Async =
        stopPolling.andThen {
            def loop: Unit < Async =
                Async.sleep(interval).andThen {
                    Abort
                        .run[ApolloException](withActivity(NetworkStatus.Poll)(networkFetch))
                        .andThen(loop)
                }
            Fiber.initUnscoped(loop).map(f => pollFiberCell = Present(f))
        }

    /** Stop background polling — react's `stopPolling`. Idempotent. */
    def stopPolling(using Frame): Unit < Sync =
        Sync.defer(pollFiberCell).map {
            case Present(f) =>
                pollFiberCell = Absent
                f.interrupt.unit
            case Absent => Sync.defer(())
        }

end QueryHandle

object QueryHandle:

    /** A synthetic, terminally-failed handle — NO watcher behind it: [[QueryHandle.state]] is pinned to
      * `QueryState.Failure(exception)` and [[QueryHandle.refetch]] aborts with the same exception. For
      * app-made error states (a malformed id in a URL, a missing prerequisite) that should render through
      * the same loading/error/loaded taxonomy as a transport failure.
      */
    def failed[D](exception: ApolloException)(using Frame, CanEqual[D, D]): QueryHandle[D] < Sync =
        for
            state    <- Signal.initRef[QueryState[D]](QueryState.Failure(exception))
            activity <- Signal.initRef[Maybe[NetworkStatus]](Absent)
        yield QueryHandle(state, activity, Abort.fail(exception))
end QueryHandle

/** The result of `Apollo.query` — the react-apollo `useQuery` shape, mapped onto
  * Kyo: the full [[QueryHandle]] surface (reactive [[state]], [[networkStatus]],
  * [[refetch]], scoped [[polling]], the imperative [[QueryHandle.startPolling]]/
  * [[QueryHandle.stopPolling]] pair, [[mapData]]) PLUS the one operation
  * a projection cannot carry: [[subscribeToMore]] (it writes merged data back into
  * the store in the operation's OWN shape, which a `mapData`-projected handle — a
  * one-way `D => B` — cannot invert). That is the sole reason this subtype exists;
  * [[mapData]] therefore narrows to a plain [[QueryHandle]], losing only
  * `subscribeToMore`. Widening a handle to the base type no longer drops polling —
  * that pair lives on [[QueryHandle]].
  *
  * @param call the prepared operation (private) — [[subscribeToMore]] runs against it.
  */
final class RawQueryHandle[D] private[apollo] (
    state: Signal[QueryState[D]],
    private val call: ApolloCall[D],
    activity: Signal.SignalRef[Maybe[NetworkStatus]]
)(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]], CanEqual[D, D])
    extends QueryHandle[D](state, activity, call.fetchPolicy(FetchPolicy.NetworkOnly).data):

    /** Feed a subscription's events into this query's cached data — react's
      * `subscribeToMore`. Consumes `sub` as a live stream; for each event, reads the
      * current query data from the store, applies `merge(prev, event)`, and writes
      * the result back as one atomic store update — which re-emits [[state]]. If the
      * query has no cached data yet, that event is skipped. `merge` must be pure: when
      * another write lands between the read and the write, it is applied again to the
      * newer data. Scope-bound: the subscription tears down when the enclosing `Scope`
      * closes.
      *
      * A `.map` projection's operation decodes only and cannot be written back, so
      * for such a query no event is merged; that is logged at debug level once.
      */
    def subscribeToMore[E](sub: ApolloCall[E])(merge: (D, E) => D)(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[E]]]]
    ): Unit < (Async & Scope) =
        val store = call.apolloClient.apolloStore
        call.requestBuilder.operation match
            case op: Operation.Normalizable[D] =>
                Fiber
                    .init(Scope.run(sub.stream.foreach { resp =>
                        resp.data match
                            case Present(event) => store.updateOperation(op)(merge(_, event)).unit
                            case Absent         => Kyo.unit
                    }))
                    .unit
            case op =>
                Log.debug(
                    s"subscribeToMore: ${op.name} decodes only (a .map projection), so its cached data cannot be written and no event is merged"
                )
        end match
    end subscribeToMore

end RawQueryHandle

/** [[QueryState.mapData]] lifted onto a reactive signal: derive a page-facing
  * `Signal[QueryState[B]]` whose data projection may reject into
  * [[QueryState.Failure]] via `Abort.fail` — one projection instead of duplicated
  * `Success`/`PartialData` arms plus identity cases. Pure (`Signal.map`); the
  * abort is discharged per emission inside the projection.
  *
  * (Both `mapData` extensions live in THIS file: same-name top-level definitions
  * in one package must share a compilation unit, or use sites hit "defined in …
  * and also in …" resolution errors.)
  */
extension [D](sig: Signal[QueryState[D]])
    def mapData[B](f: D => B < Abort[ApolloException])(using Frame, CanEqual[B, B]): Signal[QueryState[B]] =
        sig.map(_.mapData(f))

/** [[QueryHandle.mapData]] chained directly on the effect [[useQuery]] returns —
  * `useQuery(call).mapData(f)` — so a route binder projects in the same pipeline
  * step that opens the watcher, with no intermediate `.map`:
  *
  * {{{
  * for
  *   lobbies <- useQuery(lobbiesCall).mapData(_.lobbies)   // QueryHandle[List[Lobby]]
  * yield State(lobbies, …)
  * }}}
  */
extension [D](handle: QueryHandle[D] < (Async & Scope))
    def mapData[B](f: D => B < Abort[ApolloException])(using Frame, CanEqual[B, B]): QueryHandle[B] < (Async & Scope) =
        handle.map(_.mapData(f))

/** Build a [[RawQueryHandle]] over a reactive `state` source: subscribe it, then mint
  * the private `activity` overlay cell the handle's `networkStatus` / `refetch`
  * (and the imperative operations) drive.
  */
private[apollo] def buildQueryHandle[D](
    call: ApolloCall[D],
    stateEff: Signal[QueryState[D]] < (Async & Scope)
)(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]], CanEqual[D, D]): RawQueryHandle[D] < (Async & Scope) =
    for
        state    <- stateEff
        activity <- Signal.initRef[Maybe[NetworkStatus]](Absent)
        handle = new RawQueryHandle(state, call, activity)
        // Register this live query so `client.refetchQueries` / `client.resetStore`
        // can force a network refetch of it; the registration ends with this Scope.
        // The registry takes the execute row only: a NetworkOnly refetch fails with
        // nothing else, and `asExecuteFailure` states that in the type.
        _ <- call.apolloClient.activeQueries
            .register(call.requestBuilder.operation.name, ApolloEffect.asExecuteFailure(handle.refetch.unit))
        // Tie any running poll loop to this handle's Scope.
        _ <- Scope.ensure(handle.stopPolling)
    yield handle

/** The result of [[useLazyQuery]] — react-apollo's `useLazyQuery` shape: a
  * reactive [[state]] that starts [[QueryState.Idle]] (no fetch on setup) and a
  * [[load]] effect that fires the query on demand.
  *
  * @param state a [[Signal]] that seeds [[QueryState.Idle]] and, once [[load]] is
  *              invoked, reflects that fetch — `Loading` while in flight, then
  *              `Success` (or `Failure`). Each `load` refreshes the state.
  * @param load  a one-shot effect that performs the query `NetworkOnly` (writing
  *              the normalized cache) and yields its typed data, or raises an
  *              [[kyo.apollo.exception.ApolloException]] on `Abort` (with [[state]]
  *              set to `Failure`).
  */
final case class LazyQueryHandle[D](
    state: Signal[QueryState[D]],
    load: D < (Async & Abort[ApolloException])
)

// `Apollo.lazyQuery` (the react-apollo `useLazyQuery`) constructs a
// [[LazyQueryHandle]]; see [[Apollo]].
