# kyo-apollo-itserver

A standalone GraphQL server for the end-to-end suite of
[kyo-apollo](../kyo-apollo/README.md). It is built on
[kyo-caliban](../kyo-caliban/README.md), runs on the JVM, and is not published. Every
client platform (JVM, Scala Native, Scala.js, WebAssembly) runs `ApolloE2ESpec`
against this one running process, so the suite checks each platform's production
HTTP and WebSocket engines over real sockets against a server implementation the
client's own tests did not write.

## What it serves

`apolloit.ItServer` serves this schema at `/api/graphql`, with subscriptions on the
WebSocket endpoint `/api/graphql/ws` (both `graphql-transport-ws` and the older
`graphql-ws` subprotocol):

```graphql
type Query {
  value: Int!         # always 42
}

type Mutation {
  noop: Int!          # always 0
}

type Subscription {
  value: Int!         # emits 10, 20, 30, then completes
}
```

The shape matches the `{ value }` fixtures of kyo-apollo's tests and of
kyo-apollo-testing, so the same operation types drive a scripted double and the real
server.

## Running the end-to-end suite

```bash
scripts/apollo-e2e.sh            # all four client platforms
scripts/apollo-e2e.sh JVM JS     # a subset
```

The script starts the server as a plain `java` process from the exported runtime
classpath of `kyo-apollo-itserverJVM`, reads the port the server prints
(`APOLLO_IT_PORT=<port>`; it binds an OS-assigned port on 127.0.0.1), exports
`APOLLO_IT_URL=127.0.0.1:<port>`, and runs
`kyo-apollo<Platform>/testOnly kyo.apollo.ApolloE2ESpec` for each platform. On its own,
`kyo-apollo-itserverJVM/run` starts the server, prints the port and blocks until it is
stopped.

`ApolloE2ESpec` reads `APOLLO_IT_URL`. Without it, for example in a plain `sbt test`,
its test is reported as cancelled rather than passed, so a run without the server
never counts as end-to-end coverage. With it, each platform creates a client for
`http://<APOLLO_IT_URL>/api/graphql` and `ws://<APOLLO_IT_URL>/api/graphql/ws`,
expects `42` from the query, and expects `10, 20, 30` from the subscription.
