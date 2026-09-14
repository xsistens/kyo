package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException

/** Happy-path unwrapping of a `Maybe` into a typed Apollo error channel.
  *
  * Nullable fields and union branches decode to `Maybe`, so data-shaping code
  * mostly reaches for kyo's own `Maybe.getOrElse` / `Abort.get`. The one thing
  * those cannot do is abort with a *specific* [[ApolloException]] subtype:
  * `Abort.get(maybe)` fails with `Absent`, which cannot travel an
  * `Abort[CacheMissException]`-style row. `orFailWith` fills that gap — `Present`
  * yields the value, `Absent` aborts with the lazily-built `e`, and the effect
  * row is exactly `Abort[E]` for the `E` the caller names (no blanket
  * `ApolloException` row).
  *
  * The failure is by-name: it is only constructed on the `Absent` path (a
  * localized message lookup, for instance, must not run on every `Present`).
  */
extension [A](self: Maybe[A])

    /** `Present(a)` yields `a`; `Absent` aborts with the lazily-built `e`. */
    def orFailWith[E <: ApolloException](e: => E)(using Frame): A < Abort[E] =
        self match
            case Present(a) => a
            case Absent     => Abort.fail(e)
end extension
