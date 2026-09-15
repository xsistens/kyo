package kyo.apollo

import kyo.*
import kyo.apollo.api.GraphQLError
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloExecuteFailure
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.CacheMissException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.Uuid

/** Pure tests for the effect form's response→outcome projection (Phase 09,
  * Task 4), now on kyo-test. [[ApolloEffect.projectData]] is the pure heart of
  * `call.data`, so these leaves need no effect execution — bare `assert`.
  */
class ApolloEffectSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** `errors` is the convenience for "the server answered with errors"; `error`
      * sets the channel directly, which is how a transport failure is expressed.
      * Passing both is a caller error — `error` wins, matching the real model where
      * a response has one failure, not two.
      */
    private def resp[D](
        data: Maybe[D] = Absent,
        errors: Chunk[GraphQLError] = Chunk.empty,
        error: Maybe[ApolloException] = Absent
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = Uuid("00000000-0000-4000-8000-000000000000"),
            data = data,
            error =
                if error.isDefined then error
                else if errors.isEmpty then Absent
                else Present(ApolloGraphQLException(errors))
        )

    "ApolloEffect.projectData / asApolloException" - {

        "projectData yields the data on a clean success (data, no errors)" in {
            assert(ApolloEffect.projectData(resp(data = Present(42))) == Right(42))
        }

        "projectData surfaces a transport exception as Left(exception)" in {
            val boom = ApolloNetworkException("connection dropped")
            assert(
                ApolloEffect.projectData(resp[Int](data = Absent, error = Present(boom))) == Left(boom)
            )
        }

        "projectData prefers the exception even when partial data is present" in {
            val boom = ApolloNetworkException("half a response")
            assert(
                ApolloEffect.projectData(resp[Int](data = Present(7), error = Present(boom))) == Left(
                    boom
                )
            )
        }

        "projectData maps GraphQL errors to Left even with partial data" in {
            ApolloEffect.projectData(
                resp[Int](data = Present(7), errors = Chunk(GraphQLError("boom"), GraphQLError("bang")))
            ) match
                case Left(ex: ApolloGraphQLException) =>
                    // Apollo JS parity (CombinedGraphQLErrors): raw messages joined by
                    // newline, no prefix — UIs toast `message` verbatim (`getMessage` adds
                    // the KyoException framing).
                    assert(ex.message == "boom\nbang")
                    assert(ex.errors.map(_.message) == Chunk("boom", "bang"))
                case other => fail(s"expected Left(ApolloGraphQLException), got $other")
        }

        "projectData maps an empty response (no data, no errors) to Left" in {
            ApolloEffect.projectData(resp[Int]()) match
                case Left(_: DefaultApolloException) => assert(true)
                case other                           => fail(s"expected Left(DefaultApolloException), got $other")
        }

        "asApolloException passes an ApolloException through unchanged" in {
            val boom = ApolloNetworkException("keep me")
            assert(ApolloEffect.asApolloException(boom) == boom)
        }

        "asApolloException wraps a foreign Throwable as the cause" in {
            val cause = new RuntimeException("wiring blew up")
            ApolloEffect.asApolloException(cause) match
                case wrapped: DefaultApolloException => assert(wrapped.getCause == cause)
                case other                           => fail(s"expected DefaultApolloException, got $other")
        }
    }

    "ApolloEffect.asExecuteFailure (the row a registered refetch carries)" - {

        "an execute failure passes through unchanged" in {
            val boom = ApolloNetworkException("connection dropped")
            for result <- Abort.run[ApolloExecuteFailure](ApolloEffect.asExecuteFailure(Abort.fail(boom): Int < Abort[ApolloException]))
            yield assert(result == Result.Failure(boom))
        }

        "any other failure becomes a DefaultApolloException carrying it as the cause" in {
            val miss = CacheMissException(CacheKey("User", "1"))
            for result <- Abort.run[ApolloExecuteFailure](ApolloEffect.asExecuteFailure(Abort.fail(miss): Int < Abort[ApolloException]))
            yield result match
                case Result.Failure(wrapped: DefaultApolloException) =>
                    assert(wrapped.getCause == miss)
                    assert(wrapped.message == miss.message)
                case other => fail(s"expected Failure(DefaultApolloException), got $other")
        }

        "a success and a panic are left as they are" in {
            val defect = new IllegalStateException("defect")
            for
                success <- Abort.run[ApolloExecuteFailure](ApolloEffect.asExecuteFailure(42: Int < Abort[ApolloException]))
                panic <- Abort.run[Throwable](
                    ApolloEffect.asExecuteFailure(Abort.panic(defect): Int < Abort[ApolloException])
                )
            yield
                assert(success == Result.Success(42))
                assert(panic == Result.Panic(defect))
            end for
        }
    }
end ApolloEffectSpec
