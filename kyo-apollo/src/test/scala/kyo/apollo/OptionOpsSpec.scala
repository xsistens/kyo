package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.CacheMissException
import kyo.apollo.exception.DefaultApolloException

/** Tests for the `Option → Abort[ApolloException]` happy-path extensions
  * ([[OptionOps]]): value pass-through, typed abort on `None`, and the by-name
  * laziness contract (the failure argument must not evaluate on `Some`).
  */
class OptionOpsSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "Option.orFail" - {

        "Some yields the value and never evaluates the message" in {
            var evaluated = false
            def message: String =
                evaluated = true
                "should not be built"
            Abort.run[ApolloException](Some(42).orFail(message)).map { r =>
                assert(r == Result.succeed(42))
                assert(!evaluated, "by-name message was evaluated on the Some path")
            }
        }

        "None aborts with a DefaultApolloException carrying the message" in {
            Abort.run[ApolloException]((None: Option[Int]).orFail("nothing here")).map {
                case Result.Failure(ex: DefaultApolloException) => assert(ex.getMessage == "nothing here")
                case other                                      => fail(s"expected DefaultApolloException, got $other")
            }
        }
    }

    "Option.orFailWith" - {

        "Some yields the value and never builds the exception" in {
            var evaluated = false
            def ex: ApolloException =
                evaluated = true
                DefaultApolloException("should not be built")
            Abort.run[ApolloException](Some("ok").orFailWith(ex)).map { r =>
                assert(r == Result.succeed("ok"))
                assert(!evaluated, "by-name exception was evaluated on the Some path")
            }
        }

        "None aborts with the given typed exception" in {
            Abort.run[ApolloException]((None: Option[Int]).orFailWith(CacheMissException("User:1"))).map {
                case Result.Failure(ex: CacheMissException) => assert(ex.key == "User:1")
                case other                                  => fail(s"expected CacheMissException, got $other")
            }
        }
    }
end OptionOpsSpec
