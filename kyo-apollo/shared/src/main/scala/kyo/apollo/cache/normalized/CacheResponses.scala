package kyo.apollo.cache.normalized

import kyo.Present
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.exception.CacheMissException
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.CacheInfo

/** The two cache-served [[ApolloResponse]] shapes — a hit carrying data and its
  * `dependentKeys`, and a miss carrying the [[CacheMissException]] as a value —
  * built in one place so the [[CacheInterceptor]] (initial reads) and the
  * `watch()` extension (re-reads) stamp identical metadata. Extracted rather than
  * duplicated: the "one metadata model" reuse rule from the Phase 05 design.
  */
private[normalized] object CacheResponses:

    /** A cache-served success response carrying `data`, the `dependentKeys` the
      * read touched and the store `generation` it was read at (both stamped onto
      * [[CacheInfo]] so a watcher knows what to watch and whether the store has
      * moved on since).
      */
    def hit[D](
        request: ApolloRequest[D],
        data: D,
        dependentKeys: Set[String],
        generation: Long
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = request.requestUuid,
            data = Present(data),
            executionContext = request.executionContext,
            cacheInfo = Present(CacheInfo.hit(dependentKeys, generation))
        )

    /** A cache-miss response carrying the miss on its `error` channel. Any
      * non-[[CacheMissException]] cause is wrapped as a whole-root miss so the
      * failure still travels as a value rather than escaping the stream.
      */
    def miss[D](request: ApolloRequest[D], cause: Throwable): ApolloResponse[D] =
        val missException = cause match
            case m: CacheMissException => m
            case other =>
                new CacheMissException(CacheKey.rootKey(request.operation).key, None, other)
        ApolloResponse(
            requestUuid = request.requestUuid,
            executionContext = request.executionContext,
            error = Present(missException),
            cacheInfo = Present(CacheInfo.miss(missException))
        )
    end miss
end CacheResponses
