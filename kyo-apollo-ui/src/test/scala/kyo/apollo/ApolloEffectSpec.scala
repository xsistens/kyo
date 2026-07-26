package kyo.apollo

import kyo.*
import kyo.apollo.api.GraphQLError
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.Uuid

/** Pure tests for the effect form's response→outcome projection (Phase 09,
  * Task 4), now on kyo-test. [[ApolloEffect.projectData]] is the pure heart of
  * `call.data`, so these leaves need no effect execution — bare `assert`.
  */
class ApolloEffectSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def resp[D](
        data: Maybe[D] = Absent,
        errors: Chunk[GraphQLError] = Chunk.empty,
        exception: Maybe[ApolloException] = Absent
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = Uuid.random(),
            data = data,
            errors = errors,
            exception = exception
        )

    "ApolloEffect.projectData / asApolloException" - {

        "projectData yields the data on a clean success (data, no errors)" in {
            assert(ApolloEffect.projectData(resp(data = Present(42))) == Right(42))
        }

        "projectData surfaces a transport exception as Left(exception)" in {
            val boom = ApolloNetworkException("connection dropped")
            assert(
                ApolloEffect.projectData(resp[Int](data = Absent, exception = Present(boom))) == Left(boom)
            )
        }

        "projectData prefers the exception even when partial data is present" in {
            val boom = ApolloNetworkException("half a response")
            assert(
                ApolloEffect.projectData(resp[Int](data = Present(7), exception = Present(boom))) == Left(
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
                    // newline, no prefix — UIs toast getMessage verbatim.
                    assert(ex.getMessage == "boom\nbang")
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
end ApolloEffectSpec
