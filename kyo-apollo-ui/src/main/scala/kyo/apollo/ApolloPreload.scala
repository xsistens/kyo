package kyo.apollo

// getkyo.io library — shared `package kyo.apollo` (see the note in ApolloEffect.scala).
import kyo.*
import kyo.apollo.exception.ApolloException

/** A query started ahead of its consumer — the react-apollo `preloadQuery` +
  * `useReadQuery` pair, mapped onto Kyo.
  *
  * `Apollo.preload(call)` starts the watcher (the fetch is in flight from that
  * moment) and returns this handle immediately. A router loader preloads every
  * query a route needs in one place, then hands the handles to the views:
  *
  * {{{
  * // loader — both fetches leave in parallel, no waterfall
  * for
  *     user  <- Apollo.preload(client.query(GetUser(id)))
  *     posts <- Apollo.preload(client.query(GetPosts(id)))
  * yield route(user, posts)
  *
  * // view — suspends only until ITS data is there
  * preloaded.read.map(render)
  * }}}
  *
  * [[read]] is the suspense form: it awaits the FIRST settled state (failure
  * aborts on the Apollo error channel), then follows every later emission — the
  * exact contract of `Signal[QueryState].dataSignal`, which it delegates to.
  * [[state]] is the non-suspending escape hatch: the raw reactive
  * [[QueryState]], `Loading` included, for a view that renders its own pending
  * arm.
  *
  * The watcher's lifetime is the `Scope` [[Apollo.preload]] was bound in — the
  * loader's scope, so a route change tears down every query it preloaded.
  */
final class PreloadedQuery[D] private[apollo] (val state: Signal[QueryState[D]])(using CanEqual[D, D]):

    /** Suspend until the first settled data, then follow (`useReadQuery`). `onUpdateFailure` picks what a
      * failure AFTER that first data does: escalate through node-scope supervision (the default) or report
      * it to the app's notice sink while the last data stays on screen. See [[kyo.apollo.UpdateFailure]].
      */
    def read(onUpdateFailure: UpdateFailure)(using
        Frame
    ): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        state.dataSignal(onUpdateFailure)

    /** [[read]] with the default policy, [[UpdateFailure.Escalate]]: kept as its own overload so the
      * common point-free form stays `preloaded.read`.
      */
    def read(using Frame): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        state.dataSignal

end PreloadedQuery
