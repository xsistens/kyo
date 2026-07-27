package kyo.apollo.cache.normalized

import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ExecutionContext
import kyo.apollo.runtime.ResponseStream
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Reactive `watch()` — the Phase 05 payload that turns a one-shot operation into
  * a live stream.
  *
  * These extensions live in the cache package (not on the core `ApolloCall`/
  * `ApolloClient` types) so `runtime` stays cache-unaware; the dependency runs
  * cache → client, mirroring apollo-kotlin's `apolloClient.query(q).watch()`
  * shipping in the normalized-cache artifact. Everything is a composition of
  * pieces that already exist — the [[ResponseStream]] output type, the
  * [[ApolloStore.changedKeys]] bus, the [[kyo.apollo.cache.normalized.internal.CacheBatchReader]]
  * re-read, and the [[FetchPolicy]]/[[RefetchPolicy]] context elements.
  */

extension (client: ApolloClient)
    /** The single [[ApolloStore]] backing this client's normalized cache — the
      * store built once by `normalizedCache(...)` and reached here by locating the
      * installed [[CacheInterceptor]]. Both `watch()` and imperative
      * `apolloStore.writeOperation(...)` updates go through this one store, so a
      * write and a watch see the same cache and the same changed-keys stream.
      *
      * @throws IllegalStateException if no normalized cache is installed
      */
    def apolloStore: ApolloStore =
        client.registeredInterceptors
            .collectFirst { case interceptor: CacheInterceptor => interceptor.store }
            .getOrElse(
                throw new IllegalStateException(
                    "No normalized cache is installed on this ApolloClient. Call " +
                        "`.normalizedCache(...)` on the builder before reading the store or watching a query."
                )
            )
end extension

extension [D](call: ApolloCall[D])

    /** Pin the [[RefetchPolicy]] for this watch — how it re-produces its result
      * when a dependent record changes (default [[RefetchPolicy.Default]], a
      * network-free cache re-read). Rides the request's [[ExecutionContext]] exactly
      * like `fetchPolicy`, so no new request field is introduced; ignored by a plain
      * `execute()`/`toFlow` (only `watch()` reads it back).
      */
    def refetchPolicy(policy: RefetchPolicy): ApolloCall[D] =
        call.withRequest(
            call.apolloRequest.newBuilder
                .addExecutionContext(ExecutionContext.Empty + policy)
                .build()
        )

    /** Observe this operation as a live [[ResponseStream]]: it emits the initial
      * result (honoring the call's [[FetchPolicy]]) and then re-emits automatically
      * whenever any record the last read depended on changes — from another query,
      * a mutation, or a direct `apolloStore` write.
      *
      * The returned stream is cold: nothing runs until it is consumed within an
      * `Async & Scope` context. Once consumed it holds a live
      * [[ApolloStore.changedKeys]] subscription for the stream's lifetime; the
      * enclosing `Scope`'s teardown unsubscribes cleanly, stops all further
      * emissions, and closes the backing channel (ending the stream). An unrelated
      * write (one whose changed keys do not intersect this read's `dependentKeys`)
      * notifies nobody.
      *
      * ==How the effect pivot (Schritt 2.2) implements it==
      *
      * A per-watch unbounded [[Channel]] backs the stream; its `streamUntilClosed`
      * is the stream body. The synchronous change-notification machine is preserved
      * verbatim — `changedKeys.subscribe` is a plain callback, and cache re-reads
      * push into the channel via `channel.unsafe.offer`. The two *async* legs (the
      * initial fetch and a `NetworkOnly` refetch) run as detached fibers
      * (`Fiber.Unsafe.init`) that drive `call.stream` / `client.executeAsStream`
      * into the same channel — the idiomatic callback→Kyo interop, mirroring the WS
      * transport and `kyo-ui`'s reactive bridge. Those fetch streams are finite, so
      * they complete on their own; teardown only flips `active`, unsubscribes, and
      * closes the channel.
      */
    def watch()(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ResponseStream[D] =
        val client  = call.apolloClient
        val request = call.apolloRequest
        val store   = client.apolloStore
        val refetchPolicy =
            request.executionContext.get(RefetchPolicy).getOrElse(RefetchPolicy.Default)

        Stream.unwrap {
            Channel.initUnscoped[ApolloResponse[D]](Int.MaxValue).map { channel =>
                given AllowUnsafe = AllowUnsafe.embrace.danger

                // Single-threaded (JS) mutable state captured by the callbacks below.
                var active                = true
                var watchSet: Set[String] = Set.empty

                def offer(response: ApolloResponse[D]): Unit =
                    val _ = channel.unsafe.offer(response)

                // Launch a finite response stream (initial fetch / network refetch) as a
                // detached fiber that pushes each emission through `emitFresh`.
                def spawn(src: ResponseStream[D]): Unit =
                    val _ = Fiber.Unsafe.init[Throwable, Unit](Scope.run(src.foreach(emitFresh)))

                // Refresh the watch set from a just-emitted response. A cache hit already
                // carries the `dependentKeys` its read touched; a networked response
                // derives them by reading the records it just wrote back out of the
                // store — falling back to the write-back's own changed keys (stamped on
                // `CacheInfo.dependentKeys` by the cache interceptor) when that re-read
                // cannot be satisfied, so a watcher is never born dead.
                def establishFrom(response: ApolloResponse[D]): Unit =
                    val stamped = response.cacheInfo.map(_.dependentKeys).getOrElse(Set.empty)
                    if response.cacheInfo.exists(_.isCacheHit) && stamped.nonEmpty then watchSet = stamped
                    else
                        Try(store.readOperationWithKeys(request.operation)) match
                            case Success((_, keys)) => watchSet = keys
                            case Failure(_)         => if stamped.nonEmpty then watchSet = stamped
                    end if
                end establishFrom

                def emitFresh(response: ApolloResponse[D]): Unit =
                    if active then
                        offer(response)
                        establishFrom(response)

                // CacheOnly update: re-read through the same denormalization path as the
                // first read, so a re-emitted value equals a fresh read. A read that now
                // misses (the watched record was removed) surfaces the miss as a value —
                // but the watch set is KEPT, so a later write restoring the data revives
                // the watcher (Apollo Client watchers stay registered across incomplete
                // reads). A watch that never established a set stays silent.
                def reread(): Unit =
                    Try(store.readOperationWithKeys(request.operation)) match
                        case Success((data, keys)) =>
                            watchSet = keys
                            if active then offer(CacheResponses.hit(request, data, keys))
                        case Failure(cause) =>
                            if active && watchSet.nonEmpty then offer(CacheResponses.miss(request, cause))

                // NetworkOnly update: re-run the operation over the network (which writes
                // the response back into the store) and emit the networked value.
                def refetchOverNetwork(): Unit =
                    val networked = request.newBuilder
                        .addExecutionContext(ExecutionContext.Empty + FetchPolicy.NetworkOnly)
                        .build()
                    spawn(client.executeAsStream(networked))
                end refetchOverNetwork

                def onChangedKeys(changedKeys: Set[String]): Unit =
                    if active && changedKeys.intersect(watchSet).nonEmpty then
                        refetchPolicy match
                            case RefetchPolicy.CacheOnly   => reread()
                            case RefetchPolicy.NetworkOnly => refetchOverNetwork()

                // Subscribe to the store *before* the initial fetch so a write landing
                // during the fetch is never missed; the empty `watchSet` guards against
                // re-emitting for the initial fetch's own write-back (nothing intersects
                // the empty set).
                val unsubscribe = store.changedKeys.subscribe(onChangedKeys)
                spawn(call.stream)

                Scope
                    .ensure(Sync.defer {
                        given AllowUnsafe = AllowUnsafe.embrace.danger
                        active = false
                        unsubscribe()
                        val _ = channel.unsafe.close()
                    })
                    .andThen(channel.streamUntilClosed())
            }
        }
    end watch
end extension
