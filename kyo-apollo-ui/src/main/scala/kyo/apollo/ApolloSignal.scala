package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Async` / `Scope` / `Signal` / `Stream` / `Channel` / `Sync` / `Frame` /
// `Tag` / `Emit` / `Chunk` / `AllowUnsafe`.
import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.api.GraphQLError
import kyo.apollo.cache.normalized.FetchPolicy
import kyo.apollo.cache.normalized.watch
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse

/** The **reactive / UI** half of the `kyo-ui` binding (Phase 09, Task 5).
  *
  * Turns a prepared [[kyo.apollo.ApolloCall]]'s cache watcher into the two reactive
  * shapes app code consumes behind the single `import kyo.apollo.*` (ADR D1):
  *
  *   - `call.watchSignal` — a `kyo-ui` [[Signal]] of [[QueryState]] that seeds
  *     `Loading`, then re-emits on every watcher push (network reply *and* later
  *     cache change) and unsubscribes when its `Scope` is released;
  *   - `call.watchStream` — the raw [[ApolloResponse]] effect-[[Stream]] for
  *     non-UI consumers who want the untransformed responses.
  *
  * Both drive off `core`'s cold `call.watch()` cache-watcher [[Stream]], so `core`
  * stays Kyo-free. The shared bridge is a forked fiber that drains that stream and
  * pushes each [[ApolloSignal.project]]ed emission into a `kyo-ui`
  * [[Signal.SignalRef]]; the fiber is bound to the enclosing `Scope`, so the
  * subscription is torn down deterministically on scope release.
  */
enum QueryState[+D] derives CanEqual:

    /** No fetch has been requested yet — the initial value of a lazy query
      * ([[useLazyQuery]]) before its `load` effect is invoked. Distinct from
      * [[Loading]], which means a fetch is in flight.
      */
    case Idle

    /** No response has arrived yet — the initial value a fresh watcher seeds. */
    case Loading

    /** A fully clean response: `data` present, no GraphQL `errors`, no transport
      * `exception`. `fromCache` records whether the normalized cache served it
      * (as opposed to the network), from [[kyo.apollo.network.CacheInfo.fromCache]].
      *
      * `complete` is false while an incremental delivery (`@defer` / `@stream`)
      * still has payloads outstanding — Apollo Client 4's `dataState:
      * "streaming"` — so a view can show a growing list with a "loading more"
      * affordance instead of guessing. A default field rather than a separate
      * enum case: the ~90 % of queries that never stream are untouched.
      */
    case Success[+D](data: D, fromCache: Boolean, complete: Boolean = true) extends QueryState[D]

    /** A response that carried `data` *and* GraphQL `errors` — a partial success
      * the UI can render while still surfacing the errors.
      */
    case PartialData[+D](data: D, errors: Chunk[GraphQLError]) extends QueryState[D]

    /** A failed response: a transport / parse `exception` was present, or the call
      * returned neither `data` nor a renderable partial payload.
      *
      * `last` carries the most recent data this operation had delivered before the
      * failure, when there was one. A live query whose connection drops mid-stream
      * should not blank the screen: with `last` the view renders the stale data
      * behind an error banner, which is the whole reason a failure keeps a data
      * slot at all. It is [[Absent]] when the operation never succeeded (the first
      * fetch failed) — the genuine "nothing to show" case, and the default, so a
      * `Failure(ex)` built by hand stays exactly what it says.
      *
      * `ApolloSignal.driveGated` fills it in; [[ApolloSignal.project]] cannot,
      * being a pure function of one response with no memory of earlier ones.
      */
    case Failure[+D](exception: ApolloException, last: Maybe[D] = Absent) extends QueryState[D]

    /** Project the carried data with `f`, preserving the state — the functor an app
      * needs to derive a page-facing signal from an operation-shaped one (e.g. a
      * root named tuple unwrapped to its single field, or a DTO composed into a
      * view model). `Idle`/`Loading` pass through unchanged; a `Failure`'s retained
      * `last` is projected too, so a stale-data-plus-banner view survives `mapData`.
      */
    def map[B](f: D => B): QueryState[B] = this match
        case QueryState.Idle                            => QueryState.Idle
        case QueryState.Loading                         => QueryState.Loading
        case QueryState.Success(d, fromCache, complete) => QueryState.Success(f(d), fromCache, complete)
        case QueryState.PartialData(d, errors)          => QueryState.PartialData(f(d), errors)
        case QueryState.Failure(ex, last)               => QueryState.Failure(ex, last.map(f))

    /** Like [[map]], but the projection may reject the data: an `Abort.fail(e)` in `f`
      * lands in [[QueryState.Failure]] — the escape hatch for "data arrived but is
      * unusable" projections (e.g. a stitched field the page requires being absent).
      * The data-carrying shape is preserved on the success path (`Success` keeps its
      * `fromCache`, `PartialData` keeps its `errors`); a rejected `PartialData`
      * becomes a plain `Failure` (the abort wins over the partial errors).
      * `Idle`/`Loading` pass through unchanged; a `Failure` keeps its retained
      * `last`, projected through `f` — a rejection there drops it, since data `f`
      * refuses is not data the view can render.
      *
      * `f`'s ONLY effect is `Abort[ApolloException]`, so it is discharged locally and
      * purely (`Abort.run(...).eval`) — which is what keeps this method (and its
      * `Signal` lift, the `mapData` extension in ApolloQuery.scala) usable inside pure contexts
      * like `Signal.map`. The signature deliberately admits no other effect:
      * anything wider would make the local `.eval` unsound.
      */
    def mapData[B](f: D => B < Abort[ApolloException])(using Frame): QueryState[B] = this match
        case QueryState.Idle    => QueryState.Idle
        case QueryState.Loading => QueryState.Loading
        case QueryState.Failure(ex, last) =>
            QueryState.Failure(ex, last.flatMap(d => QueryState.runDataMaybe(f(d))))
        case QueryState.Success(d, fromCache, complete) =>
            QueryState.runData(f(d))(QueryState.Success(_, fromCache, complete))
        case QueryState.PartialData(d, errors) =>
            QueryState.runData(f(d))(QueryState.PartialData(_, errors))
end QueryState

object QueryState:

    /** Discharge a projection's `Abort` purely: success rebuilds the data-carrying
      * state via `onData`, a failure (and a defect surfaced as `Result.Panic`) lands
      * in [[QueryState.Failure]].
      */
    private def runData[B](projected: B < Abort[ApolloException])(
        onData: B => QueryState[B]
    )(using Frame): QueryState[B] =
        Abort.run(projected).eval match
            case Result.Success(b) => onData(b)
            case Result.Failure(e) => QueryState.Failure(e)
            case Result.Panic(t) =>
                QueryState.Failure(DefaultApolloException(Option(t.getMessage).getOrElse(t.toString)))

    /** Discharge a projection over a `Failure`'s retained `last`, keeping only a
      * clean result. A rejected or panicking projection yields [[Absent]]: the state
      * is already a failure, so there is no second error to report — only the
      * question of whether stale data is still renderable, and it is not.
      */
    private def runDataMaybe[B](projected: B < Abort[ApolloException])(using Frame): Maybe[B] =
        Abort.run(projected).eval match
            case Result.Success(b) => Present(b)
            case _                 => Absent

    /** Project an [[ApolloResponse]] onto a [[QueryState]] — the same total
      * projection [[watchSignal]] applies internally (see [[ApolloSignal.project]]),
      * exposed publicly so an app can drive its own `Signal[QueryState[D]]` from a
      * one-shot `.response` or a subscription stream without re-deriving the
      * exception/errors/absent-data taxonomy.
      */
    def of[D](response: ApolloResponse[D]): QueryState[D] = ApolloSignal.project(response)
end QueryState

/** How a reactive watcher / subscription reacts while its `skip` signal is `true`
  * — the kyo-ui form of react-apollo's `skip` (and urql's `pause`), driven by a
  * live `Signal[Boolean]` so a button can toggle it.
  */
enum SkipMode derives CanEqual:

    /** Keep the underlying watcher / subscription live, but stop pushing new values
      * into the `Signal` while skipped — the last value stays visible (a UI "pause
      * live updates"). The cheapest mode: the network / cache work still happens,
      * only the observer is silenced.
      */
    case Freeze

    /** Tear the underlying watcher / subscription down entirely while skipped
      * (unsubscribing from the store's change bus / closing the socket) and
      * re-establish it when unskipped — react's strict `skip: true`. No network or
      * cache work happens while skipped; the last emitted value stays in the cell.
      */
    case Unsubscribe
end SkipMode

extension [D](call: ApolloCall[D])

    /** Watch this operation as a raw [[ApolloResponse]] effect-[[Stream]].
      *
      * The non-UI counterpart to [[watchSignal]]: emits each watcher response
      * verbatim (no [[QueryState]] projection) as a `Stream[ApolloResponse[D],
      * Async & Scope]`. The watcher pushes into an unbounded [[Channel]] whose
      * `streamUntilClosed` drives the stream; the `Scope` finalizer both cancels
      * the watcher and closes the channel (terminating the stream) on scope
      * release. `Scope` rides in the effect row because that teardown is scoped —
      * consume it inside a `Scope.run { … }`.
      */
    def watchStream(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): Stream[ApolloResponse[D], Async & Scope] =
        // The pivot makes `core`'s `watch()` already a `Stream[ApolloResponse[D],
        // Async & Scope]` (channel-backed, scope-torn-down), so the non-UI shape is
        // just that stream verbatim — no bridge left to build.
        call.watch()
end extension

extension [D](sig: Signal[QueryState[D]])
    /** Consume a live operation '''suspense-style''': await the FIRST settled
      * [[QueryState]] — `Success`/`PartialData` seeds the returned `Signal[D]`,
      * `Failure` aborts into the shared Apollo error channel — then follow every
      * later data emission. The pending phase suspends THIS effect, so a UI
      * mount's placeholder covers loading and its error render covers the first
      * failure: the view consumes plain data with no `QueryState` arm left.
      *
      * A live stream can still fail AFTER the enclosing mount completed — its
      * effect row is gone by then, so a tail failure cannot abort. It goes to the
      * '''mandatory''' [[onTailFailure]] handler instead (surface it — a toast, a
      * log — never swallow it silently), while the signal keeps the last
      * delivered data. Later `Idle`/`Loading` emissions (a gated re-subscribe,
      * a refetch) also keep the last data — the suspense shape has no reified
      * loading state to fall back to.
      *
      * The first-settled await rides a scoped observer torn down as soon as the
      * seed resolves (`Scope.run`); the follow observer lives in the CALLER's
      * `Scope` — release it to stop following (the same lifetime contract as
      * [[watchSignal]]/[[subscribeSignal]]).
      */
    def dataSignal(onTailFailure: ApolloException => Unit < Async)(using
        Frame,
        CanEqual[D, D]
    ): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        for
            // Await the first settled state on a promise carrying the Apollo error
            // channel; the observer completing it is scoped to this block only.
            seed <- Scope.run {
                Promise.initWith[D, Abort[ApolloException]] { p =>
                    Fiber.init(sig.observe(st => ApolloSignal.completeSettled(p, st))).andThen(p.get)
                }
            }
            ref <- Signal.initRef[D](seed)
            // The follow observer re-reads the CURRENT state on attach (observe's
            // contract), so a value that lands between seeding and attaching is
            // caught up immediately — no gap.
            _ <- UI.fork(sig.observe {
                case QueryState.Success(d, _, _)  => ref.set(d)
                case QueryState.PartialData(d, _) => ref.set(d)
                case QueryState.Failure(ex, _)    => onTailFailure(ex)
                case _                            => (): Unit
            })
        yield ref

    /** The '''escalating''' suspense form — [[dataSignal]] without a tail handler: a tail
      * `QueryState.Failure` is logged and then FAILS the follow-observer fiber (`Abort.fail`).
      *
      * The follow observer is forked with `UI.fork`, so on an engine with node-scope supervision that
      * fiber failure flips the enclosing mounted node into its error state — the same routing a failed
      * mount takes (node `.onError`, then the default error UI), so head AND tail failures land in one
      * channel. On a vanilla engine it degrades to logged-and-stopped (the
      * signal keeps the last delivered data) — never silently swallowed, but nothing repaints. Use
      * the `(onTailFailure)` overload to keep the node alive and surface tail failures in-app (a
      * toast) instead.
      */
    def dataSignal(using
        Frame,
        CanEqual[D, D]
    ): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        for
            seed <- Scope.run {
                Promise.initWith[D, Abort[ApolloException]] { p =>
                    Fiber.init(sig.observe(st => ApolloSignal.completeSettled(p, st))).andThen(p.get)
                }
            }
            ref <- Signal.initRef[D](seed)
            _ <- UI.fork(sig.observe {
                case QueryState.Success(d, _, _)  => ref.set(d)
                case QueryState.PartialData(d, _) => ref.set(d)
                case QueryState.Failure(ex, _)    =>
                    // Escalate: fail this follow fiber so a supervising mount node flips; the log keeps
                    // the failure visible on engines without supervision.
                    Log.error("apollo live operation failed after first data", ex).andThen(Abort.fail(ex))
                case _ => (): Unit
            })
        yield ref
end extension

/** Helpers backing the reactive-form extension methods, kept off the extension
  * so the pure projection is unit-testable without a Kyo runtime (mirroring
  * [[ApolloEffect]]) and the subscription bridge is shared by both shapes.
  */
object ApolloSignal:

    /** Complete `p` once the state settles: data succeeds it, a failure fails it;
      * `Idle`/`Loading` leave it pending ([[dataSignal]]'s first-settled await).
      * Repeat completions are no-ops (`completeDiscard`).
      */
    private[apollo] def completeSettled[D](
        p: Promise[D, Abort[ApolloException]],
        st: QueryState[D]
    )(using Frame): Unit < Sync =
        st match
            case QueryState.Success(d, _, _)  => p.completeDiscard(Result.succeed(d))
            case QueryState.PartialData(d, _) => p.completeDiscard(Result.succeed(d))
            case QueryState.Failure(ex, _)    => p.completeDiscard(Result.fail(ex))
            case _                            => (): Unit

    /** Project a response onto the UI-facing [[QueryState]].
      *
      * A transport failure wins as [[QueryState.Failure]]. The server answering with
      * GraphQL `errors` is [[QueryState.PartialData]] when data accompanies them and
      * [[QueryState.Failure]] when it does not. A clean response with `data` is a
      * [[QueryState.Success]] (its `fromCache` read from
      * [[kyo.apollo.network.CacheInfo]]); a clean response without `data` is a
      * [[QueryState.Failure]] carrying a [[DefaultApolloException]].
      * Total — a `Signal` needs a value for every emission, so there is no throw.
      */
    private[kyo] def project[D](response: ApolloResponse[D]): QueryState[D] =
        response.error match
            case Present(gql: ApolloGraphQLException) =>
                response.data match
                    case Present(d) => QueryState.PartialData(d, gql.errors)
                    case Absent     => QueryState.Failure(gql)
            case Present(ex) => QueryState.Failure(ex)
            case Absent =>
                response.data match
                    case Present(d) =>
                        QueryState.Success(
                            d,
                            fromCache = response.cacheInfo.exists(_.fromCache),
                            complete = response.complete
                        )
                    case Absent =>
                        QueryState.Failure(
                            DefaultApolloException("The server did not return any data")
                        )

    /** Drive `ref` from a response source (`call.watch()` or `call.stream`), gated by
      * a live `skip` signal — the shared engine behind every `skip` overload.
      *
      *   - [[SkipMode.Freeze]]: consume the source once; each emission updates `ref`
      *     only while `skip` is `false` (a skipped emission is dropped, freezing the
      *     last value). The source stays subscribed the whole time.
      *   - [[SkipMode.Unsubscribe]]: run the source in a child fiber only while `skip`
      *     is `false`; when `skip` flips `true` the fiber is interrupted (tearing the
      *     watcher/socket down via its own `Scope`), and a fresh source is opened when
      *     `skip` returns to `false`. `newSource` is by-name so each window rebuilds a
      *     fresh cold stream.
      *
      * Every write goes through [[retainingData]], so a mid-stream failure keeps the
      * last data it delivered instead of blanking the view.
      *
      * Returns a scope-free `Unit < Async` (each mode discharges the source's `Scope`
      * internally), so callers fork it with a plain `Fiber.init` bound to the
      * enclosing watcher `Scope`.
      */
    private[kyo] def driveGated[D](
        newSource: => Stream[ApolloResponse[D], Async & Scope],
        ref: Signal.SignalRef[QueryState[D]],
        skip: Signal[Boolean],
        mode: SkipMode
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): Unit < Async =
        mode match
            case SkipMode.Freeze =>
                Scope.run(newSource.foreach { resp =>
                    skip.current.map {
                        case true  => Sync.defer(())
                        case false => push(ref, resp)
                    }
                })
            case SkipMode.Unsubscribe =>
                def window: Unit < Async =
                    skip.current.map {
                        case true => skip.next.andThen(window)
                        case false =>
                            Fiber
                                .use(Scope.run(newSource.foreach(resp => push(ref, resp))))(_ => skip.next)
                                .andThen(window)
                    }
                window

    /** Project `resp` and write it to `ref`, carrying forward the data the operation
      * had already delivered if the projection is a bare [[QueryState.Failure]].
      */
    private[apollo] def push[D](ref: Signal.SignalRef[QueryState[D]], resp: ApolloResponse[D])(using
        Frame
    ): Unit < Sync =
        ref.currentWith(previous => ref.set(retainingData(previous, project(resp))))

    /** Merge a freshly projected state with the one it replaces: a `Failure` that
      * carries no data of its own inherits whatever `previous` had.
      *
      * This is the piece a per-response projection cannot do. A live query that has
      * been serving data and then loses its connection would otherwise emit a bare
      * `Failure`, and a view keyed on that state blanks — replacing a working screen
      * with an error page over a blip. Carrying the data forward lets the view show
      * it behind a banner instead. Every other state replaces outright: fresh data
      * supersedes stale data, and `Idle`/`Loading` are deliberate resets.
      */
    private[kyo] def retainingData[D](
        previous: QueryState[D],
        next: QueryState[D]
    ): QueryState[D] =
        next match
            case QueryState.Failure(ex, Absent) => QueryState.Failure(ex, dataOf(previous))
            case settled                        => settled

    /** The renderable data a state carries, if any — including a failure's retained
      * `last`, so the carry-forward survives a run of consecutive failures.
      */
    private def dataOf[D](state: QueryState[D]): Maybe[D] =
        state match
            case QueryState.Success(d, _, _)  => Present(d)
            case QueryState.PartialData(d, _) => Present(d)
            case QueryState.Failure(_, last)  => last
            case _                            => Absent
end ApolloSignal
