package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.CacheMissException

/** Tests for the `Maybe → Abort[E]` happy-path extension ([[MaybeOps]]): value
  * pass-through, the typed abort on `Absent` (the effect row is exactly the `E`
  * the caller names, not a blanket `ApolloException`), and the by-name laziness
  * contract (the failure argument must not evaluate on `Present`).
  */
class MaybeOpsSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "Maybe.orFailWith" - {

        "Present yields the value and never builds the exception" in {
            var evaluated = false
            def ex: CacheMissException =
                evaluated = true
                CacheMissException("should not be built")
            Abort.run[CacheMissException](Present("ok").orFailWith(ex)).map { r =>
                assert(r == Result.succeed("ok"))
                assert(!evaluated, "by-name exception was evaluated on the Present path")
            }
        }

        "Absent aborts with the given typed exception" in {
            Abort.run[CacheMissException]((Absent: Maybe[Int]).orFailWith(CacheMissException("User:1"))).map {
                case Result.Failure(ex) => assert(ex.key == "User:1")
                case other              => fail(s"expected CacheMissException, got $other")
            }
        }

        "the effect row is the named leaf, not the blanket ApolloException row" in {
            // A handler for the leaf type discharges the whole row: nothing is left
            // to handle, so the result is a plain `Result` (compiles only if the row
            // is exactly `Abort[CacheMissException]`).
            val pure: Result[CacheMissException, Int] < Any =
                Abort.run[CacheMissException](Present(1).orFailWith(CacheMissException("x")))
            pure.map(r => assert(r == Result.succeed(1)))
        }

        "the leaf type is bounded by ApolloException" in {
            typeCheckFailure("""Present(1).orFailWith(new RuntimeException("not an ApolloException"))""")
            // A widened leaf still works when the caller names it.
            val widened: Int < Abort[ApolloException] = Present(1).orFailWith[ApolloException](CacheMissException("x"))
            Abort.run[ApolloException](widened).map(r => assert(r == Result.succeed(1)))
        }
    }
end MaybeOpsSpec
