package kyo.apollo.cache.normalized

import kyo.*
import kyo.apollo.api.Subscription
import kyo.apollo.interceptor.ApolloInterceptor
import kyo.apollo.interceptor.ApolloInterceptorChain
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.CacheInfo
import kyo.apollo.runtime.ResponseStream
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** The [[ApolloInterceptor]] that plugs the normalized cache into the operation
  * layer. Inserted before the terminal `NetworkInterceptor`, it reads from and
  * writes back to an [[ApolloStore]] according to the request's [[FetchPolicy]],
  * and emits the response sequence that policy prescribes — each stamped with
  * [[CacheInfo]] so callers can see whether a value came from cache or network.
  *
  * Everything is a composition of the existing [[Flow]] primitive and the store's
  * synchronous `readOperation` / `writeOperation`; no new async machinery is
  * introduced. `CacheAndNetwork`'s "cache then network" is a small [[Flow]] that
  * emits the (synchronous) cache value before delegating to the network stream —
  * built inline rather than by adding a combinator to `Flow`. Mirrors
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
            case _                  =>
                // A mutation carrying optimistic data is inherently network-bound: overlay
                // the optimistic value now, then reconcile it against the network reply —
                // ahead of (and bypassing) the fetch-policy cache-read dispatch.
                request.executionContext.get(OptimisticData) match
                    case Present(optimistic) => optimisticMutation(request, chain, optimistic)
                    case Absent =>
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
        readFromCache(request) match
            case Success((data, keys)) => Stream.init(Seq(cacheHit(request, data, keys)))
            case Failure(_)            => network(request, chain)

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
        readFromCache(request) match
            case Success((data, keys)) => Stream.init(Seq(cacheHit(request, data, keys)))
            case Failure(cause)        => Stream.init(Seq(cacheMiss(request, cause)))

    /** Network first; on a network error fall back to the cache, else re-emit the
      * network error.
      */
    private def networkFirst[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        chain.proceed(request).mapPure { response =>
            if !response.hasTransportError then writeBack(request, response)
            else
                readFromCache(request) match
                    case Success((data, keys)) => cacheHit(request, data, keys)
                    case Failure(_)            => response.copy(cacheInfo = Present(CacheInfo.network))
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
        readFromCache(request) match
            case Success((data, keys)) => Stream.init(Seq(cacheHit(request, data, keys)))
            case Failure(_)            => Stream.init(Seq.empty[ApolloResponse[D]])

    /** A cache response (when hit) then the network response, in that order. */
    private def cacheAndNetwork[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        val net = network(request, chain)
        readFromCache(request) match
            case Success((data, keys)) =>
                Stream.init(Seq(cacheHit(request, data, keys))).concat(net)
            case Failure(_) => net
        end match
    end cacheAndNetwork

    /** Run a mutation carrying optimistic data: overlay it into the store before the
      * network call (so watchers show it at once), then on the network reply drop the
      * optimistic layer and merge the server truth (success) or revert cleanly
      * (failure). The layer's keys and the merged result publish as one union
      * ([[ApolloStore.rollbackAndWrite]]), so a watcher converges straight onto server
      * truth with no intermediate flicker. Errored/empty responses roll the layer back
      * without a merge, mirroring [[writeBack]]'s "only persist a clean success" rule.
      */
    private def optimisticMutation[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain,
        optimistic: OptimisticData
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        val mutationId = optimistic.mutationId
        discard(store.writeOptimisticUpdates(request.operation, optimistic.data.asInstanceOf[D], mutationId))
        chain.proceed(request).mapPure { response =>
            discard {
                if !response.hasTransportError then
                    response.data match
                        case Present(data) => store.rollbackAndWrite(request.operation, data, mutationId)
                        case Absent        => store.rollbackOptimisticUpdates(mutationId)
                else store.rollbackOptimisticUpdates(mutationId)
            }
            response.copy(cacheInfo = Present(CacheInfo.network))
        }
    end optimisticMutation

    // --- shared steps ---------------------------------------------------------

    /** Run the network leg and write every successful response back to the store. */
    private def network[D](
        request: ApolloRequest[D],
        chain: ApolloInterceptorChain
    )(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] =
        chain.proceed(request).mapPure(writeBack(request, _))

    /** Read `request`'s operation from the store — data plus the record keys the
      * read depended on — capturing a miss as a `Failure`.
      */
    private def readFromCache[D](request: ApolloRequest[D]): Try[(D, Set[String])] =
        Try(store.readOperationWithKeys(request.operation))

    /** Persist a successful network `response` (data present, no exception) and tag
      * it as network-sourced, stamping the record keys the write-back changed onto
      * `CacheInfo.dependentKeys` — a watcher's fallback watch set when its own
      * post-write re-read cannot be satisfied. Errored/empty responses are passed
      * through untouched but for the `cacheInfo` stamp.
      */
    private def writeBack[D](
        request: ApolloRequest[D],
        response: ApolloResponse[D]
    ): ApolloResponse[D] =
        val changed =
            if !response.hasTransportError then
                response.data match
                    case Present(data) => store.writeOperation(request.operation, data)
                    case Absent        => Set.empty[String]
            else Set.empty[String]
        response.copy(cacheInfo = Present(CacheInfo.network(changed)))
    end writeBack

    /** A cache-served success response carrying `data` and the `dependentKeys` the
      * read touched (stamped onto [[CacheInfo]] for Phase 05 watchers) — built by
      * the shared [[CacheResponses]] so re-reads in `watch()` stamp identical metadata.
      */
    private def cacheHit[D](
        request: ApolloRequest[D],
        data: D,
        dependentKeys: Set[String]
    ): ApolloResponse[D] =
        CacheResponses.hit(request, data, dependentKeys)

    /** A cache-miss response carrying the miss as an `exception` value (see
      * [[CacheResponses.miss]]).
      */
    private def cacheMiss[D](
        request: ApolloRequest[D],
        cause: Throwable
    ): ApolloResponse[D] =
        CacheResponses.miss(request, cause)
end CacheInterceptor
