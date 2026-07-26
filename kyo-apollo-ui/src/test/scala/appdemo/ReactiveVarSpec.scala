package appdemo

import kyo.*
// The ONE import an app writes. Like [[UseQuerySurfaceSpec]], this suite lives
// OUTSIDE `kyo.apollo` on purpose: from a foreign package the `ReactiveVar`
// alias and its `reactiveVar` constructor are reachable *only* through this
// single import, so compiling this file is the proof of the surface.
import kyo.apollo.*

/** Proves Step 7a: react-apollo's reactive variables map onto `Signal.SignalRef`
  * with a thin `reactiveVar` constructor and a `ReactiveVar[A]` alias — no
  * wrapper. Three properties:
  *   1. `current` reads the initial value, then the value written by `set`;
  *   2. the read-only `Signal[A]` view (the alias *is* a `Signal`) observes a
  *      later write — this is the react `useReactiveVar` reactive read;
  *   3. `updateAndGet` is an atomic read-modify-write returning the new value.
  */
class ReactiveVarSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private enum Filter derives CanEqual:
        case All, Active
        def toggle: Filter = this match
            case All    => Active
            case Active => All
    end Filter

    // Compile-only proof: the alias and constructor resolve solely through the
    // single `import kyo.apollo.*`. Never executed.
    private def surface(using Frame): ReactiveVar[Filter] < Sync =
        val rv = reactiveVar(Filter.All)
        val _ = rv.map { v =>
            val _: ReactiveVar[Filter] = v
            val _: Signal[Filter]      = v // the alias IS a Signal — read-only view
            ()
        }
        rv
    end surface

    "reactiveVar (react `makeVar`)" - {

        "the alias + constructor resolve through the single import (compiles)" in {
            val _ = surface
            assert(true)
        }

        "current reads the initial value, then the value written by set" in {
            reactiveVar(0).map { v =>
                for
                    a <- v.current
                    _ = assert(a == 0)
                    _ <- v.set(5)
                    b <- v.current
                    _ = assert(b == 5)
                yield ()
            }
        }

        "the read-only Signal view observes a later write" in {
            reactiveVar(Filter.All).map { v =>
                // Hand out only the Signal[A] view — the capability boundary: `view`
                // has no `set`, yet still tracks the var it was taken from.
                val view: Signal[Filter] = v
                for
                    before <- view.current
                    _ = assert(before == Filter.All)
                    _     <- v.set(Filter.Active)
                    after <- view.current
                    _ = assert(after == Filter.Active)
                yield ()
                end for
            }
        }

        "updateAndGet is an atomic read-modify-write returning the new value" in {
            reactiveVar(0).map { v =>
                for
                    r1 <- v.updateAndGet(_ + 1)
                    r2 <- v.updateAndGet(_ + 1)
                    r3 <- v.updateAndGet(_ + 1)
                    _ = assert((r1, r2, r3) == (1, 2, 3))
                    now <- v.current
                    _ = assert(now == 3)
                yield ()
            }
        }
    }
end ReactiveVarSpec
