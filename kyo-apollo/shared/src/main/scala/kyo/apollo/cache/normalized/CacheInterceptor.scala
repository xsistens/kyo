package kyo.apollo.cache.normalized

import kyo.*
import kyo.apollo.api.Mutation
import kyo.apollo.api.Subscription
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.CacheInfo
import kyo.apollo.runtime.ResponseStream

/** The [[ApolloInterceptor]] that plugs the normalized cache into the operation
  * layer. Inserted before the terminal `NetworkInterceptor`, it reads from and
  * writes back to an [[ApolloStore]] according to the request's [[FetchPolicy]],
  * and emits the response sequence that policy prescribes — each stamped with
  * [[CacheInfo]] so callers can see whether a value came from cache or network.
  *
  * Everything is a composition of [[Stream]] and the store's effectful
  * `readOperationStamped` / `writeOperation`: a policy that reads the cache first
  * does so when its stream is consumed (`Stream.unwrap` over the read), and the
  * network leg writes each response back as it flows through. `CacheAndNetwork`'s
  * "cache then network" is the cache value followed by the network stream. Mirrors
  * apollo-kotlin's `CacheInterceptor` / fetch-policy interceptors.
  *
  * @param store the coordinator this interceptor reads from and writes to
  */
final class CacheInterceptor(private[normalized] val store: ApolloStore) extends ApolloInterceptor:

    def intercept[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        // A subscription is a long-lived stream, not a cache-read candidate: it must
        // never be short-circuited by a fetch policy's cache read (that would serve
        // one stale value and never open the socket). It flows straight to the
        // network leg, so every streamed event is still written back and normalizes
        // into the store — triggering watchers — exactly like a network query.
        request.operation match
            case _: Subscription[?] => network(request, chain)
            case _: Mutation[?]     =>
                // A mutation is inherently network-bound and must NEVER be answered
                // from the cache: its own write-back stores its result under
                // MUTATION_ROOT, so a repeat of the same mutation (or, with a read
                // redirect like CacheKeyResolver.byIdArgument, any id-carrying
                // mutation whose entity is already cached) would otherwise be a
                // cache hit under CacheFirst and silently never reach the server.
                // With optimistic data the store is additionally overlaid before the
                // network call and reconciled against the reply.
                request.executionContext.get(OptimisticData) match
                    case Present(optimistic) => optimisticMutation(request, chain, optimistic)
                    case Absent              => network(request, chain)
            case _ =>
                request.executionContext.get(FetchPolicy).getOrElse(FetchPolicy.Default) match
                    case FetchPolicy.CacheFirst      => cacheFirst(request, chain)
                    case FetchPolicy.NetworkOnly     => networkOnly(request, chain)
                    case FetchPolicy.CacheOnly       => cacheOnly(request)
                    case FetchPolicy.NetworkFirst    => networkFirst(request, chain)
                    case FetchPolicy.CacheAndNetwork => cacheAndNetwork(request, chain)
                    case FetchPolicy.NoCache         => noCache(request, chain)
                    case FetchPolicy.Standby         => standby(request)

    // --- policies -------------------------------------------------------------

    /** Cache hit → serve it; miss → network, written back. */
    private def cacheFirst[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap(readFromCache(request).map {
            case Result.Success((data, keys, gen)) => Stream.init(Seq(cacheHit(request, data, keys, gen)))
            case _                                 => network(request, chain)
        })

    /** Never read the cache; run the network and always write it back. */
    private def networkOnly[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        network(request, chain)

    /** Cache only: a hit is served, a miss becomes a `CacheMissException` value. */
    private def cacheOnly[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        Stream.unwrap(readFromCache(request).map {
            case Result.Success((data, keys, gen)) => Stream.init(Seq(cacheHit(request, data, keys, gen)))
            case Result.Failure(cause)             => Stream.init(Seq(cacheMiss(request, cause)))
            case Result.Panic(cause)               => Stream.init(Seq(cacheMiss(request, cause)))
        })

    /** Network first; on a network error fall back to the cache, else re-emit the
      * network error.
      */
    private def networkFirst[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        chain.proceed(request).map { response =>
            if !response.hasTransportError then writeBack(request, response)
            else
                readFromCache(request).map {
                    case Result.Success((data, keys, gen)) => cacheHit(request, data, keys, gen)
                    case _                                 => response.copy(cacheInfo = Present(CacheInfo.network))
                }
        }

    /** No cache: run the network and pass the response straight through — the cache
      * is neither read before nor written after (react `no-cache`). Only the
      * network `cacheInfo` stamp is added, so callers still see it came from the wire.
      */
    private def noCache[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        chain.proceed(request).mapPure(_.copy(cacheInfo = Present(CacheInfo.network)))

    /** Standby: never fetch. A cache hit is served; a miss emits nothing (an empty
      * stream) rather than a miss error — so the operation is "parked" and, under a
      * watcher, stays quiet until an external write populates the record, then
      * reacts. Distinct from `CacheOnly`, whose miss is a surfaced exception value.
      */
    private def standby[D](request: ApolloRequest[D])(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        Stream.unwrap(readFromCache(request).map {
            case Result.Success((data, keys, gen)) => Stream.init(Seq(cacheHit(request, data, keys, gen)))
            case _                                 => Stream.init(Seq.empty[ApolloResponse[D]])
        })

    /** A cache response (when hit) then the network response, in that order. */
    private def cacheAndNetwork[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        Stream.unwrap(readFromCache(request).map {
            case Result.Success((data, keys, gen)) =>
                Stream.init(Seq(cacheHit(request, data, keys, gen))).concat(network(request, chain))
            case _ => network(request, chain)
        })

    /** Run a mutation carrying optimistic data: overlay it into the store before the
      * network call (so watchers show it at once), then on the network reply drop the
      * optimistic layer and merge the server truth (success) or revert cleanly
      * (failure). The layer's keys and the merged result publish as one union
      * ([[ApolloStore.rollbackAndWrite]]), so a watcher converges straight onto server
      * truth with no intermediate flicker. Errored/empty responses roll the layer back
      * without a merge, mirroring [[writeBack]]'s "only persist a clean success" rule.
      *
      * The layer is owned by the `Scope` the stream is consumed in: it is acquired
      * when consumption starts (never when the stream is merely built) and released
      * — rolled back — when that Scope closes, whether by interrupt, timeout, a
      * raised exception below this interceptor, or the consumer dropping the stream.
      * The success path drops the layer itself through `rollbackAndWrite`, so the
      * Scope release then finds nothing and publishes nothing; the reply is the only
      * publication a settled mutation makes.
      */
    private def optimisticMutation[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain,
        optimistic: OptimisticData
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        val mutationId = optimistic.mutationId
        Stream.unwrap {
            Scope.acquireRelease(
                store.writeOptimisticUpdates(request.operation, optimistic.data.asInstanceOf[D], mutationId)
            )(_ => store.rollbackOptimisticUpdates(mutationId).unit).andThen {
                chain.proceed(request).map { response =>
                    val settle =
                        if !response.hasTransportError then
                            response.data match
                                case Present(data) => store.rollbackAndWrite(request.operation, data, mutationId)
                                case Absent        => store.rollbackOptimisticUpdates(mutationId)
                        else store.rollbackOptimisticUpdates(mutationId)
                    settle.andThen(response.copy(cacheInfo = Present(CacheInfo.network)))
                }
            }
        }
    end optimisticMutation

    // --- shared steps ---------------------------------------------------------

    /** Run the network leg and write every successful response back to the store. */
    private def network[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        chain.proceed(request).map(writeBack(request, _))

    /** Read `request`'s operation from the store — data, the record keys the read
      * depended on, and the store generation it was current at — capturing a miss
      * (raised inside the read) as the result's error.
      */
    private def readFromCache[D](request: ApolloRequest[D])(using
        Frame
    ): Result[Throwable, (D, Set[CacheKey], Long)] < Sync =
        Abort.run[Throwable](store.readOperationStamped(request.operation))

    /** Persist a successful network `response` (data present, no exception) and tag
      * it as network-sourced, stamping the record keys the write-back changed onto
      * `CacheInfo.dependentKeys` — a watcher's fallback watch set when its own
      * post-write re-read cannot be satisfied. Errored/empty responses are passed
      * through untouched but for the `cacheInfo` stamp.
      */
    private def writeBack[D](
        request: ApolloRequest[D],
        response: ApolloResponse[D]
    )(using Frame): ApolloResponse[D] < Sync =
        val changed: Set[CacheKey] < Sync =
            if !response.hasTransportError then
                response.data match
                    case Present(data) => store.writeOperation(request.operation, data)
                    case Absent        => Set.empty[CacheKey]
            else Set.empty[CacheKey]
        changed.map(keys => response.copy(cacheInfo = Present(CacheInfo.network(keys))))
    end writeBack

    /** A cache-served success response carrying `data`, the `dependentKeys` the
      * read touched and the store `generation` it was read at (stamped onto
      * [[CacheInfo]] for watchers) — built by the shared [[CacheResponses]] so
      * re-reads in `watch()` stamp identical metadata.
      */
    private def cacheHit[D](
        request: ApolloRequest[D],
        data: D,
        dependentKeys: Set[CacheKey],
        generation: Long
    ): ApolloResponse[D] =
        CacheResponses.hit(request, data, dependentKeys, generation)

    /** A cache-miss response carrying the miss as an `exception` value (see
      * [[CacheResponses.miss]]).
      */
    private def cacheMiss[D](
        request: ApolloRequest[D],
        cause: Throwable
    )(using Frame): ApolloResponse[D] =
        CacheResponses.miss(request, cause)
end CacheInterceptor
