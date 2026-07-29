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

    /** Suspend until the first settled data, then follow — `useReadQuery`. A tail
      * failure escalates through node-scope supervision (see
      * `dataSignal`'s escalating overload); use the other overload to surface it
      * in-app instead.
      */
    def read(using Frame): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        state.dataSignal

    /** [[read]] with an explicit tail-failure handler — the signal keeps the last
      * delivered data and `onTailFailure` surfaces the error (a toast, a log).
      */
    def read(onTailFailure: ApolloException => Unit < Async)(using
        Frame
    ): Signal[D] < (Async & Abort[ApolloException] & Scope) =
        state.dataSignal(onTailFailure)
end PreloadedQuery
