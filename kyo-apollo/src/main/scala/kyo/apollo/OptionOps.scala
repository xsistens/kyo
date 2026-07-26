package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException

/** Happy-path unwrapping of `Option` into the Apollo error channel.
  *
  * kyo's own `Abort.get(Option)` aborts with `Absent`, which cannot travel the
  * `Abort[ApolloException]` row that the one-shot effects (`call.data`) and the
  * reactive projections (`mapData`) share. These extensions keep data-shaping
  * code on the happy path: `Some` yields the value, `None` aborts into the same
  * typed channel every other Apollo failure uses — so one boundary (an enclosing
  * `Abort.recover`, a UI mount's error render, or a `mapData` reification into
  * `QueryState.Failure`) handles all of them uniformly.
  *
  * The failure parameter is by-name in both forms: it is only constructed on
  * the `None` path (a localized message lookup, for instance, must not run on
  * every `Some`).
  *
  * (Two names rather than one overload: by-name parameters erase to
  * `Function0`, so same-name overloads would clash after erasure.)
  */
extension [A](self: Option[A])

    /** `Some(a)` yields `a`; `None` aborts with a [[kyo.apollo.exception.DefaultApolloException]]
      * carrying the lazily-built `message`.
      */
    def orFail(message: => String)(using Frame): A < Abort[ApolloException] =
        self match
            case Some(a) => a
            case None    => Abort.fail(DefaultApolloException(message))

    /** `Some(a)` yields `a`; `None` aborts with the lazily-built `ex` — the typed
      * form for callers that raise a specific [[kyo.apollo.exception.ApolloException]] subtype.
      */
    def orFailWith(ex: => ApolloException)(using Frame): A < Abort[ApolloException] =
        self match
            case Some(a) => a
            case None    => Abort.fail(ex)
end extension
