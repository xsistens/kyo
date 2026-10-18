package kyo.apollo.cache.normalized

import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.api.CacheKeyGenerator
import kyo.apollo.cache.normalized.api.CacheKeyResolver
import kyo.apollo.cache.normalized.api.FieldPolicies

/** The one-call installer that turns a plain [[ApolloClient]] into a
  * cache-backed one.
  *
  * `normalizedCache` builds an [[ApolloStore]] over the supplied backend and
  * registers a [[CacheInterceptor]] for it. Because the client appends its
  * terminal `NetworkInterceptor` internally, adding the cache interceptor here
  * places it exactly where it must be — after any user interceptors, before the
  * network — so reads are served from the store and successful network responses
  * are written back. It is an extension (rather than a method on `ApolloClient`)
  * so the core client type stays unaware of the cache layer; the dependency runs
  * cache → client, mirroring apollo-kotlin's `normalizedCache(...)` builder
  * extension living in the cache artifact.
  */
extension (config: ApolloClient.Config)
    /** A copy of this config with a normalized cache installed. The store is part of
      * the returned config, so every client created from it shares that store.
      *
      * @param cache         the record backend (e.g. a [[MemoryCache]])
      * @param keyGenerator  the write-side object → [[kyo.apollo.cache.normalized.api.CacheKey]] policy
      *                      (e.g. a [[kyo.apollo.cache.normalized.api.TypePolicyCacheKeyGenerator]])
      * @param keyResolver   the read-side field → cache-key redirect policy
      * @param fieldPolicies per-field declarative policies — connection pagination
      *                      keys, field read redirects, and custom merges (default:
      *                      none). Build with
      *                      [[kyo.apollo.cache.normalized.api.FieldPolicies.of]] /
      *                      [[kyo.apollo.cache.normalized.api.FieldPolicies.fromList]].
      */
    def normalizedCache(
        cache: NormalizedCache,
        keyGenerator: CacheKeyGenerator = CacheKeyGenerator.default,
        keyResolver: CacheKeyResolver = CacheKeyResolver.default,
        fieldPolicies: FieldPolicies = FieldPolicies.empty
    ): ApolloClient.Config =
        config.addInterceptor(
            new CacheInterceptor(new ApolloStore(cache, keyGenerator, keyResolver, fieldPolicies))
        )
end extension
