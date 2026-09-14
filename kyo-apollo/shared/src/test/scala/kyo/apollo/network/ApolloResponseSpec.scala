package kyo.apollo.network

import kyo.Absent
import kyo.Chunk
import kyo.Present
import kyo.apollo.api.GraphQLError
import kyo.apollo.api.GraphQLResponse
import kyo.apollo.exception.ApolloGraphQLException
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.json.Json

/** Tests [[ApolloResponse]] construction from a decoded [[GraphQLResponse]] and
  * the behaviour of its single `error` channel.
  *
  * The consolidation these tests pin down: a response has exactly one place to
  * look for failure. The server's own GraphQL `errors` arrive there as an
  * [[ApolloGraphQLException]] — which is what makes them distinguishable from a
  * transport failure without a second field — and remain reachable in typed form
  * through the `errors` projection.
  */
class ApolloResponseSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Hero(name: String)

    "ApolloResponse" - {

        "fromGraphQLResponse copies data/extensions and attaches the uuid" in {
            val id = Uuid.random()
            val gql = GraphQLResponse(
                data = Present(Hero("Luke")),
                errors = Chunk.empty,
                extensions = Map[String, Json]("cost" -> Json.JNum(3.0))
            )
            val response = ApolloResponse.fromGraphQLResponse(id, gql)

            assert(response.requestUuid == id)
            assert(response.data == Present(Hero("Luke")))
            assert(response.extensions == Map[String, Json]("cost" -> Json.JNum(3.0)))
            assert(response.error == Absent)
            assert(response.errors == Chunk.empty)
        }

        "defaults: no data, no error, no extensions, empty context" in {
            val response = ApolloResponse[Hero](Uuid.random())
            assert(response.data == Absent)
            assert(response.error == Absent)
            assert(response.errors == Chunk.empty)
            assert(response.extensions == Map.empty[String, Json])
            assert(response.executionContext.isEmpty)
            assert(!response.hasErrors)
            assert(!response.hasTransportError)
        }

        "GraphQL errors become an ApolloGraphQLException on the error channel" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse[Hero](data = Absent, errors = Chunk(GraphQLError("boom")))
            )
            assert(response.hasErrors)
            assert(response.error.exists(_.isInstanceOf[ApolloGraphQLException]))
            assert(response.errors == Chunk(GraphQLError("boom")))
        }

        "partial data keeps both the data and the errors" in {
            val response = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk(GraphQLError("partial")))
            )
            assert(response.data == Present(Hero("Luke")))
            assert(response.errors.map(_.message) == Chunk("partial"))
            assert(response.error.exists(_.message == "partial"))
        }

        "a transport failure carries the exception itself and projects no errors" in {
            val boom     = new ApolloNetworkException("dropped")
            val response = ApolloResponse.fromException[Hero](Uuid.random(), boom)
            assert(response.data == Absent)
            assert(response.error == Present(boom))
            assert(response.hasErrors)
            assert(response.errors == Chunk.empty)
        }

        "hasTransportError separates a transport failure from the server's errors" in {
            val dropped = ApolloResponse.fromException[Hero](Uuid.random(), new ApolloNetworkException("dropped"))
            val answered = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk(GraphQLError("partial")))
            )
            val clean = ApolloResponse.fromGraphQLResponse(
                Uuid.random(),
                GraphQLResponse(data = Present(Hero("Luke")), errors = Chunk.empty)
            )

            assert(dropped.hasTransportError)
            assert(!answered.hasTransportError)
            assert(!clean.hasTransportError)
        }
    }
end ApolloResponseSpec
