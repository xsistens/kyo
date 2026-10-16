package kyo.apollo.runtime

import kyo.*
import kyo.apollo.network.ApolloResponse

/** The uniform effect-stream every operation produces (Schritt 2.2).
  *
  * A single network reply, a cache-then-network pair, and a long-lived
  * subscription all share this one type — a Kyo [[Stream]] of [[ApolloResponse]]
  * values carrying the `Async & Scope` effect row. `Async` is the asynchrony of
  * the transport; `Scope` is the teardown seam a live stream (a subscription or a
  * cache watcher) binds its socket/watcher unsubscription to. A finite
  * query/mutation stream (effect row `Async`) widens into this type for free —
  * `Stream` is contravariant in its effect parameter, so `Async <: Async & Scope`
  * makes `Stream[V, Async] <: Stream[V, Async & Scope]`.
  *
  * `core` is Kyo-effect-native: the interceptor chains speak Kyo `Stream` end to
  * end and `kyo-ui` binds those effects directly, with no framework bridge in
  * between.
  */
type ResponseStream[D] = Stream[ApolloResponse[D], Async & Scope]
