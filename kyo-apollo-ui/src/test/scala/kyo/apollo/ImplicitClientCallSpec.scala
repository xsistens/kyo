package kyo.apollo

import CountryFixture.*
import kyo.*
import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.FetchPolicy

/** Proves the `.call` bridge (`core`'s `OperationCall.scala`): with a
  * `given ApolloClient` in scope, an inline operation runs the whole fluent chain
  * directly — `op.call.fetchPolicy(…).data` — dropping the repeated
  * `client.query(op)` prefix. `.call` lives in `package kyo.apollo`, so this
  * same-package spec sees it without an import (a foreign-package app gets it via
  * `import kyo.apollo.*`, like the demos).
  */
class ImplicitClientCallSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "the `.call` bridge with a given ApolloClient" - {

        "op.call.data is equivalent to client.query(op).data" in {
            given client: ApolloClient = cacheless(StaticEngine(body("Alice")))
            for
                viaCall   <- Abort.run(CurrentUserQuery().call.data)
                viaClient <- Abort.run(client.query(CurrentUserQuery()).data)
            yield viaCall match
                case Result.Success(data) =>
                    assert(data == userData("Alice"))
                    assert(Result.Success(data) == viaClient)
                case other => fail(s"expected Success(Alice), got $other")
            end for
        }

        "op.call.fetchPolicy(…).response threads the builder hop" in {
            given ApolloClient = cacheless(StaticEngine(body("Alice")))
            for resp <- CurrentUserQuery().call.fetchPolicy(FetchPolicy.NetworkOnly).response
            yield assert(resp.data == Present(userData("Alice")))
        }
    }
end ImplicitClientCallSpec
