package kyo.apollo

import kyo.apollo.api.Mutation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription

/** Bind an inline operation to a `given ApolloClient`, yielding the [[ApolloCall]]
  * the whole fluent chain hangs off (`.fetchPolicy` / `.optimisticUpdates` /
  * `.data` / `.watchSignal` / `.execute` / `.watch` / …).
  *
  * `client.query`/`mutation`/`subscription` are pure factories that only wrap the
  * operation in an [[ApolloCall]], so the single missing hop to run an inline
  * operation is `Operation -> ApolloCall`. Once a client is in scope
  * (`given ApolloClient = client`), a call site can drop the repeated
  * `client.query(op)` prefix:
  *
  * {{{
  * given ApolloClient = client
  * val warmed  = getCountry.call.fetchPolicy(FetchPolicy.NetworkOnly).data
  * val listSig = getCountries.call.fetchPolicy(FetchPolicy.CacheOnly).watchSignal
  * }}}
  *
  * Defined on the concrete `Query`/`Mutation`/`Subscription` subtypes (not on the
  * `Operation` supertype) so each dispatches to the semantically matching client
  * method and no public method has to be added to [[ApolloClient]]. The `toQuery` /
  * `toMutation` / `toSubscription` terminals already yield those concrete types, so
  * overload resolution picks the right `.call` by receiver type.
  */
extension [D](op: Query[D]) def call(using client: ApolloClient): ApolloCall[D] = client.query(op)

extension [D](op: Mutation[D])
    def call(using client: ApolloClient): ApolloCall[D] = client.mutation(op)

extension [D](op: Subscription[D])
    def call(using client: ApolloClient): ApolloCall[D] = client.subscription(op)
