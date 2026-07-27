package kyo.apollo.network

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.json.Json

/** Tests [[ApolloResponse]] construction from a decoded [[GraphQLResponse]]
  * (field-for-field copy under the same names, no re-parse) and the
  * `hasErrors` predicate.
  *
  * The `exception`-populated path (transport/parse failures folded into a value
  * via [[ApolloResponse.fromException]]) is exercised in Task 8, once Task 4
  * adds the instantiable [[kyo.apollo.exception.ApolloException]] subtypes — the
  * base is `sealed`, so no concrete exception exists to construct yet.
  */
class ApolloResponseSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Hero(name: String)

    "ApolloResponse" - {

        "fromGraphQLResponse copies data/errors/extensions and attaches the uuid" in {
            val id = Uuid.random()
            val gql = GraphQLResponse(
                data = Present(Hero("Luke")),
                errors = Chunk(GraphQLError("partial")),
                extensions = Map[String, Json]("cost" -> Json.JNum(3.0))
            )
            val response = ApolloResponse.fromGraphQLResponse(id, gql)

            assert(response.requestUuid == id)
            assert(response.data == Present(Hero("Luke")))
            assert(response.errors == Chunk(GraphQLError("partial")))
            assert(response.extensions == Map[String, Json]("cost" -> Json.JNum(3.0)))
            assert(response.exception == Absent)
        }

        "defaults: no data, no errors/extensions, empty context, no exception" in {
            val response = ApolloResponse[Hero](Uuid.random())
            assert(response.data == Absent)
            assert(response.errors == Chunk.empty)
            assert(response.extensions == Map.empty[String, Json])
            assert(response.executionContext.isEmpty)
            assert(response.exception == Absent)
            assert(!response.hasErrors)
        }

        "hasErrors is true when GraphQL errors are present" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Absent, errors = Chunk(GraphQLError("boom")))
            )
            assert(response.hasErrors)
        }

        "hasErrors and exceptionOrNull reflect a transport exception" in {
            val boom     = new ApolloNetworkException("dropped")
            val response = ApolloResponse.fromException[Hero](Uuid.random(), boom)
            assert(response.hasErrors)
            assert(response.exceptionOrNull == boom)
        }

        "exceptionOrNull is null on a clean response" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk.empty)
            )
            assert(response.exceptionOrNull == null)
        }

        "dataOrThrow returns data on success" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk.empty)
            )
            assert(response.dataOrThrow() == Hero("Luke"))
        }

        "dataOrThrow returns partial data even when GraphQL errors are present" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk(GraphQLError("partial")))
            )
            assert(response.dataOrThrow() == Hero("Luke"))
        }

        "dataOrThrow rethrows the transport exception when present" in {
            val boom     = new ApolloNetworkException("dropped")
            val response = ApolloResponse.fromException[Hero](Uuid.random(), boom)
            val thrown   = intercept[ApolloNetworkException](response.dataOrThrow())
            assert(thrown == boom)
        }

        "dataOrThrow throws when there is neither data nor exception" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse[Hero](data = Absent, errors = Chunk(GraphQLError("boom")))
            )
            val _ = intercept[DefaultApolloException](response.dataOrThrow())
        }

        "dataAssertNoErrors returns data on a fully clean response" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk.empty)
            )
            assert(response.dataAssertNoErrors() == Hero("Luke"))
        }

        "dataAssertNoErrors throws when GraphQL errors accompany partial data" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk(GraphQLError("partial")))
            )
            val thrown = intercept[ApolloGraphQLException](response.dataAssertNoErrors())
            assert(thrown.getMessage == "partial")
            assert(thrown.errors.map(_.message) == Chunk("partial"))
        }

        "dataAssertNoErrors rethrows the transport exception before checking data" in {
            val boom     = new ApolloNetworkException("dropped")
            val response = ApolloResponse.fromException[Hero](Uuid.random(), boom)
            val thrown   = intercept[ApolloNetworkException](response.dataAssertNoErrors())
            assert(thrown == boom)
        }
    }
end ApolloResponseSpec
