package kyo.apollo.network

import kyo.Absent
import kyo.Present

/** Tests the type-indexed [[ExecutionContext]]: typed `get`, element addition
  * and replacement, and right-biased merge via `++`.
  */
class ExecutionContextSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Marker(label: String) extends ExecutionContext.Element:
        def key: ExecutionContext.Key[Marker] = Marker
    object Marker extends ExecutionContext.Key[Marker]

    final case class Attempt(n: Int) extends ExecutionContext.Element:
        def key: ExecutionContext.Key[Attempt] = Attempt
    object Attempt extends ExecutionContext.Key[Attempt]

    "ExecutionContext" - {

        "Empty has no elements" in {
            assert(ExecutionContext.Empty.isEmpty)
            assert(ExecutionContext.Empty.get(Marker) == Absent)
        }

        "get returns the element typed by its key" in {
            val ctx = ExecutionContext.Empty + Marker("cache") + Attempt(2)
            assert(ctx.get(Marker) == Present(Marker("cache")))
            assert(ctx.get(Attempt) == Present(Attempt(2)))
            assert(ctx.contains(Marker))
            assert(!ctx.isEmpty)
        }

        "adding the same key replaces the previous element" in {
            val ctx = ExecutionContext.Empty + Marker("first") + Marker("second")
            assert(ctx.get(Marker) == Present(Marker("second")))
        }

        "++ merges with right-hand elements winning" in {
            val left   = ExecutionContext.Empty + Marker("left") + Attempt(1)
            val right  = ExecutionContext.Empty + Marker("right")
            val merged = left ++ right
            assert(merged.get(Marker) == Present(Marker("right")))
            assert(merged.get(Attempt) == Present(Attempt(1)))
        }

        "++ Empty is a no-op returning the same context" in {
            val ctx = ExecutionContext.Empty + Marker("x")
            assert((ctx ++ ExecutionContext.Empty) eq ctx)
        }
    }
end ExecutionContextSpec
