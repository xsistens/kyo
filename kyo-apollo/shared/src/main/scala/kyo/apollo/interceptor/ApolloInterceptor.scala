package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.runtime.ResponseStream

/** Intercepts at the **operation layer** — above HTTP, in terms of the typed
  * [[ApolloRequest]] / [[ApolloResponse]] envelope. This is where cache reads,
  * request retries, APQ negotiation, and subscription plumbing live.
  *
  * Unlike an [[HttpInterceptor]] (which sees exactly one request/response), an
  * `ApolloInterceptor` returns a [[ResponseStream]] so a single operation can
  * yield **multiple emissions** — the cache-then-network pattern, or a stream of
  * subscription events. A single HTTP query is just the one-emission case of the
  * same type. Interceptors run in registration order, each wrapping the rest of
  * the chain, which terminates at [[NetworkInterceptor]].
  *
  * Mirrors apollo-kotlin's `ApolloInterceptor` (whose `Flow<ApolloResponse<D>>`
  * this Kyo `Stream` stands in for on Scala.js).
  */
trait ApolloInterceptor:

    /** Handle `request`, delegating to `chain.proceed` to continue toward the
      * network, and return the resulting stream of [[ApolloResponse]] values.
      */
    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D]
end ApolloInterceptor
