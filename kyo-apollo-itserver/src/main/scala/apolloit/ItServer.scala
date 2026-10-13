package apolloit

import caliban.*
import caliban.schema.Schema
import kyo.*

/** The shared backend for the cross-platform apollo E2E suite: one real caliban
  * GraphQL server every client platform (JVM/JS/Native/Wasm) runs its
  * `ApolloE2ESpec` against. Schema mirrors the `{ value }` fixtures — a `value`
  * query returning 42 and a `value` subscription streaming 10, 20, 30.
  *
  * Started via `kyo-apollo-itserverJVM/run`; it binds an OS-assigned port on
  * 127.0.0.1 and prints `APOLLO_IT_PORT=<port>` so the orchestrator can point the
  * client suites at it, then blocks until killed.
  */
object ItServer extends KyoApp:

    case class Query(value: Int) derives Schema.SemiAuto
    case class Mutation(noop: Int) derives Schema.SemiAuto
    case class Subscriptions(value: zio.stream.ZStream[Any, Nothing, Int]) derives Schema.SemiAuto

    private val api =
        graphQL(RootResolver(
            Query(42),
            Mutation(0),
            Subscriptions(zio.stream.ZStream.fromIterable(Seq(10, 20, 30)))
        ))

    run {
        for
            interpreter <- Resolvers.get(api)
            server      <- Resolvers.run(interpreter)
            _           <- Sync.defer(println(s"APOLLO_IT_PORT=${server.port}"))
            _           <- Async.never
        yield ()
    }
end ItServer
