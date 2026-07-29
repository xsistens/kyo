package kyo.apollo

import kyo.*
import kyo.apollo.api.GraphQLError
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.CacheInfo
import kyo.apollo.network.Uuid

/** Pure tests for the reactive form's response→[[QueryState]] projection
  * (Phase 09, Task 5), now on kyo-test. [[ApolloSignal.project]] is pure, so
  * these leaves need no effect execution.
  */
class ApolloSignalSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** `errors` is the convenience for "the server answered with errors"; `error`
      * sets the channel directly, which is how a transport failure is expressed.
      * Passing both is a caller error — `error` wins, matching the real model where
      * a response has one failure, not two.
      */
    private def resp[D](
        data: Maybe[D] = Absent,
        errors: Chunk[GraphQLError] = Chunk.empty,
        error: Maybe[ApolloException] = Absent,
        cacheInfo: Maybe[CacheInfo] = Absent
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = Uuid.random(),
            data = data,
            error =
                if error.isDefined then error
                else if errors.isEmpty then Absent
                else Present(ApolloGraphQLException(errors)),
            cacheInfo = cacheInfo
        )

    "ApolloSignal.project" - {

        "project maps a clean response to Success(data, fromCache = false)" in {
            assert(
                ApolloSignal.project(resp(data = Present(42))) == QueryState.Success(42, fromCache = false)
            )
        }

        "project reads fromCache from a cache-served response" in {
            assert(
                ApolloSignal.project(resp(data = Present(42), cacheInfo = Present(CacheInfo.hit)))
                    == QueryState.Success(42, fromCache = true)
            )
        }

        "project reads fromCache = false from a networked response" in {
            assert(
                ApolloSignal.project(resp(data = Present(42), cacheInfo = Present(CacheInfo.network)))
                    == QueryState.Success(42, fromCache = false)
            )
        }

        "project maps data + GraphQL errors to PartialData" in {
            val errs = Chunk(GraphQLError("boom"), GraphQLError("bang"))
            assert(
                ApolloSignal
                    .project(resp(data = Present(7), errors = errs)) == QueryState.PartialData(7, errs)
            )
        }

        "project prefers a transport exception over partial data" in {
            val boom = ApolloNetworkException("connection dropped")
            ApolloSignal.project(resp[Int](data = Present(7), error = Present(boom))) match
                case QueryState.Failure(ex) => assert(ex == boom)
                case other                  => fail(s"expected Failure($boom), got $other")
        }

        "project maps an empty response (no data, no errors) to Failure" in {
            ApolloSignal.project(resp[Int]()) match
                case QueryState.Failure(ex: DefaultApolloException) =>
                    assert(ex.getMessage.contains("did not return any data"))
                case other => fail(s"expected Failure(DefaultApolloException), got $other")
        }

        "project maps no-data-with-errors to Failure describing the errors" in {
            ApolloSignal.project(resp[Int](data = Absent, errors = Chunk(GraphQLError("nope")))) match
                case QueryState.Failure(ex: ApolloGraphQLException) =>
                    // Apollo JS parity: the bare error message, no prefix.
                    assert(ex.getMessage == "nope")
                    assert(ex.errors.map(_.message) == Chunk("nope"))
                case other => fail(s"expected Failure(ApolloGraphQLException), got $other")
        }

        "QueryState values compare by structure (Signal CanEqual de-dup)" in {
            assert(QueryState.Loading == QueryState.Loading)
            assert(QueryState.Success(1, true) == QueryState.Success(1, true))
            assert(QueryState.Success(1, true) != QueryState.Success(1, false))
            assert(QueryState.PartialData(1, Chunk.empty) != (QueryState.Loading: QueryState[Int]))
        }

        "QueryState.of is the public alias of the projection" in {
            assert(QueryState.of(resp(data = Present(42))) == QueryState.Success(42, fromCache = false))
            QueryState.of(resp[Int]()) match
                case QueryState.Failure(_: DefaultApolloException) => assert(true)
                case other                                         => fail(s"expected Failure(DefaultApolloException), got $other")
        }

        "QueryState.map projects data in Success and PartialData, passes the rest through" in {
            assert(QueryState.Success(21, fromCache = true).map(_ * 2) == QueryState.Success(42, fromCache = true))
            val errs = Chunk(GraphQLError("boom"))
            assert(QueryState.PartialData(21, errs).map(_ * 2) == QueryState.PartialData(42, errs))
            assert((QueryState.Idle: QueryState[Int]).map(_ * 2) == QueryState.Idle)
            assert((QueryState.Loading: QueryState[Int]).map(_ * 2) == QueryState.Loading)
            val failure = QueryState.Failure(ApolloNetworkException("down"))
            assert((failure: QueryState[Int]).map(_ * 2) == failure)
        }

        "QueryState.mapData projects like map on the success path, preserving the shape" in {
            assert(
                QueryState.Success(21, fromCache = true).mapData(_ * 2) == QueryState.Success(42, fromCache = true)
            )
            val errs = Chunk(GraphQLError("boom"))
            assert(QueryState.PartialData(21, errs).mapData(_ * 2) == QueryState.PartialData(42, errs))
        }

        "QueryState.mapData lands an Abort.fail in Failure — from Success and from PartialData" in {
            val rejected = DefaultApolloException("unusable")
            def reject(qs: QueryState[Int]): QueryState[Int] =
                qs.mapData(n => if n < 0 then Abort.fail(rejected) else n)
            assert(reject(QueryState.Success(-1, fromCache = false)) == QueryState.Failure(rejected))
            // The abort wins over the partial errors: a rejected PartialData is a plain Failure.
            assert(reject(QueryState.PartialData(-1, Chunk(GraphQLError("boom")))) == QueryState.Failure(rejected))
            assert(reject(QueryState.Success(7, fromCache = false)) == QueryState.Success(7, fromCache = false))
        }

        "QueryState.mapData passes Idle, Loading and Failure through untouched" in {
            def widen(qs: QueryState[Int]): QueryState[String] = qs.mapData(_.toString)
            assert(widen(QueryState.Idle) == QueryState.Idle)
            assert(widen(QueryState.Loading) == QueryState.Loading)
            val failure = QueryState.Failure(ApolloNetworkException("down"))
            assert(widen(failure) == failure)
        }
    }
end ApolloSignalSpec
