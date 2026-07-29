package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Async` / `Abort` / `Signal` / `Sync` / `Frame` / `Tag` / `Emit` / `Chunk`.
import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.api.GraphQLError
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse

/** The **mutation** shape of the `kyo-ui` binding — react-apollo's `useMutation`
  * result tuple `[mutate, { loading, data, error, called, reset }]`, mapped onto
  * Kyo (Step A2 of the react-parity build).
  *
  * A mutation is already runnable as the effect `op.call.optimisticUpdates(…).data`;
  * this adds the observable *state handle* React apps reach for: a live
  * [[Signal]] of [[MutationState]] the UI renders (spinner / error / result), a
  * `run` effect that fires the mutation and drives that state, and a `reset` back
  * to [[MutationState.Idle]].
  *
  * {{{
  * import kyo.apollo.*
  *
  * useMutation(saveCall).map: m =>
  *   render(m.state)          // Signal[MutationState[D]] — Idle→Loading→Success/Failure
  *   onClick(m.run(()))       // fires; optimisticUpdates / errorPolicy on the call still apply
  *   onClick(m.reset)         // back to Idle (e.g. clear a form)
  * }}}
  *
  * Unlike a query watcher, a mutation handle owns no subscription — so
  * [[useMutation]] is a bare `Sync` effect with **no `Scope`**.
  */
enum MutationState[+D] derives CanEqual:

    /** No mutation has run yet (the initial state), or the handle was `reset`. The
      * react `called == false` state.
      */
    case Idle

    /** A mutation is in flight. */
    case Loading

    /** A clean result: `data` present, no GraphQL `errors`, no transport
      * `exception`. (A mutation is never served from cache, so — unlike
      * [[QueryState.Success]] — there is no `fromCache` flag.)
      */
    case Success[+D](data: D) extends MutationState[D]

    /** A result that carried `data` *and* GraphQL `errors` — a partial success the
      * UI can render while still surfacing the errors.
      */
    case PartialData[+D](data: D, errors: Chunk[GraphQLError]) extends MutationState[D]

    /** A failed mutation: a transport / parse `exception`, or no renderable `data`. */
    case Failure(exception: ApolloException) extends MutationState[Nothing]
end MutationState

/** The react-apollo `useMutation` result, mapped onto Kyo: a live reactive
  * [[state]] the UI renders, a [[run]] effect that fires the mutation, and a
  * [[reset]] effect that returns the state to [[MutationState.Idle]].
  *
  * @param state the [[Signal]] of [[MutationState]]: seeds [[MutationState.Idle]],
  *              flips to `Loading` on `run`, then `Success` / `PartialData` /
  *              `Failure`. React's `loading` / `data` / `error` / `called` all read
  *              off this one cell (`called == (state != Idle)`).
  * @param run   fires the mutation for an input `I`, drives [[state]], and yields
  *              the typed `data` — or raises an
  *              [[kyo.apollo.exception.ApolloException]] on `Abort` (honoring the
  *              call's [[kyo.apollo.ErrorPolicy]], exactly like `.data`). Any
  *              `.optimisticUpdates` / `.fetchPolicy` pinned on the call still
  *              apply, since `run` executes that prepared call.
  * @param reset returns [[state]] to [[MutationState.Idle]] (react's `reset()`).
  */
final case class MutationHandle[I, D](
    state: Signal[MutationState[D]],
    run: I => D < (Async & Abort[ApolloException]),
    reset: Unit < Sync
):

    /** À-la-carte success callback — the composable alternative to passing
      * `onCompleted` at construction. Returns a new handle whose `run` also fires
      * `f(data)` after a successful run (a `Right` projection, honoring
      * `errorPolicy`), before yielding the data. Pure `run`-wrapping — no extra
      * `Scope` or fiber. Chains with [[onError]]; use either alone, no no-op filler:
      *
      * {{{
      * useMutation(saveCall).map(_.onError(toast))                   // only error
      * useMutation(build).map(_.onCompleted(refresh).onError(toast)) // both, chained
      * }}}
      *
      * Prefer composing `run` itself where you can — `handle.run(i).map(use)` already
      * delivers the result effect.
      */
    def onCompleted(f: D => Any < Async)(using Frame): MutationHandle[I, D] =
        copy(run = input => run(input).map(data => f(data).andThen(data)))

    /** À-la-carte error callback — fires `f(exception)` when `run` aborts (honoring
      * the call's `ErrorPolicy`), then re-raises. See [[onCompleted]] for the shape.
      */
    def onError(f: ApolloException => Any < Async)(using Frame): MutationHandle[I, D] =
        copy(run =
            input =>
                Abort.run[ApolloException](run(input)).map {
                    case Result.Failure(ex) => f(ex).andThen(Abort.fail(ex))
                    case other              => Abort.get(other)
                }
        )
end MutationHandle

// `Apollo.mutation` (the react-apollo `useMutation`, both the ready-call and the
// `I => call` builder overloads) constructs a [[MutationHandle]]; see [[Apollo]].

/** Helpers backing `Apollo.mutation`, kept off the surface so the pure projection is
  * unit-testable without a Kyo runtime (mirroring [[ApolloSignal]]).
  */
object MutationState:

    /** Project a response onto the UI-facing [[MutationState]] (the mutation analog
      * of [[ApolloSignal.project]]): a transport failure → [[Failure]]; clean
      * `data` → [[Success]]; `data` with GraphQL `errors` → [[PartialData]]; absent
      * `data` → [[Failure]]. Total — a `Signal` needs a value for every emission.
      */
    private[kyo] def fromResponse[D](resp: ApolloResponse[D]): MutationState[D] =
        resp.error match
            case Present(gql: ApolloGraphQLException) =>
                resp.data match
                    case Present(d) => MutationState.PartialData(d, gql.errors)
                    case Absent     => MutationState.Failure(gql)
            case Present(ex) => MutationState.Failure(ex)
            case Absent =>
                resp.data match
                    case Present(d) => MutationState.Success(d)
                    case Absent =>
                        MutationState.Failure(
                            DefaultApolloException("The server did not return any data")
                        )
end MutationState
