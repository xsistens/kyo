package appdemo

import kyo.*
// The ONE import an app writes. It must bring every extension method AND the
// `useQuery` helper into scope with no per-operation wiring (Phase 09, Task 6).
// This suite lives OUTSIDE `kyo.apollo` on purpose: from a foreign package the
// extensions are reachable *only* through this single import, so its compilation
// is the actual proof of the boilerplate-free surface.
import kyo.apollo.*
import kyo.apollo.ApolloCall
import kyo.apollo.exception.ApolloException
import kyo.apollo.network.ApolloResponse

/** Proves Task 6's headline: a single `import kyo.apollo.*` exposes the whole
  * app-facing surface — the effect form (`.data` / `.response`), the reactive
  * form (`.watchSignal` / `.watchStream`), and the `useQuery` helper — with no
  * other `kyo.apollo` import. Now on kyo-test.
  */
class UseQuerySurfaceSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    // Compile-only proof. Every entry point below resolves solely through the
    // single `import kyo.apollo.*` above; if any failed to, this method — and thus
    // the whole suite — would not compile. It is never executed.
    private def surface[D](call: ApolloCall[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): QueryHandle[D] < (Async & Scope) =
        val _: D < (Async & Abort[ApolloException])     = call.data
        val _: ApolloResponse[D] < Async                = call.response
        val _: Signal[QueryState[D]] < (Async & Scope)  = Apollo.watchSignal(call)
        val _: Stream[ApolloResponse[D], Async & Scope] = call.watchStream
        val handle                                      = Apollo.query(call)
        // useQuery yields the RAW handle (store-bound ops included)…
        val _: RawQueryHandle[D] < (Async & Scope) = handle
        // …and mapData chains directly on the effect, yielding the projected handle.
        val _: QueryHandle[String] < (Async & Scope) = Apollo.query(call).mapData(_.toString)
        val _ = handle.map { q =>
            val _: Signal[QueryState[D]]                = q.state
            val _: D < (Async & Abort[ApolloException]) = q.refetch
            // The projected handle: state AND refetch carry the projected type.
            val p: QueryHandle[String]                       = q.mapData(_.toString)
            val _: Signal[QueryState[String]]                = p.state
            val _: String < (Async & Abort[ApolloException]) = p.refetch
            ()
        }
        handle
    end surface

    "single-import app surface" - {

        "a single `import kyo.apollo.*` exposes the whole surface (compiles)" in {
            // Referencing `surface` forces its compilation and marks it used; it is
            // never invoked (Task 8 owns the runtime harness that builds a real call).
            val _ = surface[Int]
            assert(true)
        }

        "QueryState, reached via the single import, compares by structure" in {
            val a: QueryState[Int] = QueryState.Success(1, fromCache = false)
            val b: QueryState[Int] = QueryState.Success(1, fromCache = false)
            val c: QueryState[Int] = QueryState.Success(1, fromCache = true)
            assert(a == b)
            assert(a != c)
            assert((QueryState.Loading: QueryState[Int]) != a)
        }
    }
end UseQuerySurfaceSpec
