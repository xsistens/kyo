package kyo.apollo.runtime

import kyo.*
import kyo.apollo.network.ApolloResponse

/** The asynchronous **effect/result contract** for executing one GraphQL
  * operation — the value an [[kyo.apollo.interceptor.ApolloInterceptor]] chain is
  * ultimately consumed through, and the handle [[kyo.apollo.ApolloClient]] returns
  * from its `query` / `mutation` / `subscription` calls.
  *
  * ==Why this shape==
  *
  * `core` is Kyo-effect-native: the async abstraction is a Kyo [[Stream]]
  * ([[ResponseStream]]). The
  * primitive is [[stream]]: a single network reply and the multiple emissions a
  * normalized cache or a subscription will later produce share **one** return
  * type — a `Stream[ApolloResponse[D], Async & Scope]` — so no call site changes
  * its type across cache-then-network or subscription streaming. [[execute]] is
  * then merely "take the first / only emission" over that stream: the ergonomic
  * entry point for the common single-response query or mutation. Deriving
  * `execute` *from* `stream` (rather than the reverse) keeps the two provably
  * consistent — the single-response result is, by construction, the stream's
  * first value.
  *
  * Failures follow the "failures are values" contract: network / HTTP / parse
  * problems arrive inside `ApolloResponse.exception` (a value), never on the
  * `Async` error channel. The effect from [[execute]] therefore raises only for a
  * genuine wiring error (an exhausted interceptor chain) or an empty stream —
  * never for an ordinary GraphQL/transport error.
  *
  * `ApolloClient` supplies the concrete [[ApolloCall]] by building the interceptor
  * chain and implementing [[stream]]; the fluent builder methods
  * (`.addHttpHeader`, `.httpMethod`, …) belong to that concrete type.
  */
trait ApolloCall[D]:

    /** The stream-first primitive: the full sequence of [[ApolloResponse]] values
      * this operation produces, in emission order. One emission for a plain network
      * query today; the same type carries cache-then-network and subscription
      * streams. Cold — nothing runs until it is consumed within an `Async` (and, for
      * a live stream, `Scope`) context.
      */
    def stream(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D]

    /** Execute the operation and yield its **single / first** [[ApolloResponse]] —
      * the convenience path for the common query/mutation. Derived from [[stream]]
      * so the two can never disagree: it takes the first emission.
      *
      * The effect carries a transport/GraphQL error *inside*
      * `ApolloResponse.exception` (a value); it raises outright only if the stream
      * completes without emitting (a `NoSuchElementException`) or the chain raises a
      * wiring error.
      */
    def execute(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ApolloResponse[D] < (Async & Scope) =
        stream.take(1).run.map(_.head)
end ApolloCall
