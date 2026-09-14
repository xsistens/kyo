package kyo.apollo.cache.normalized

import kyo.Present
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.CacheInfo

/** The two cache-served [[ApolloResponse]] shapes — a hit carrying data and its
  * `dependentKeys`, and a miss carrying the [[CacheReadFailure]] as a value —
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
        dependentKeys: Set[CacheKey],
        generation: Long
    ): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = request.requestUuid,
            data = Present(data),
            executionContext = request.executionContext,
            cacheInfo = Present(CacheInfo.hit(dependentKeys, generation))
        )

    /** A cache-miss response carrying the read's `failure` on its `error` channel.
      * Only a read failure is ever a miss value: a defect in the read is a panic
      * and never reaches this shape.
      */
    def miss[D](request: ApolloRequest[D], failure: CacheReadFailure): ApolloResponse[D] =
        ApolloResponse(
            requestUuid = request.requestUuid,
            executionContext = request.executionContext,
            error = Present(failure),
            cacheInfo = Present(CacheInfo.miss(failure))
        )
end CacheResponses
