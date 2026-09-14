package kyo.apollo.cache.normalized

import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.ApolloClient
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.exception.ApolloConfigException
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.ExecutionContext
import kyo.apollo.runtime.ResponseStream
import scala.annotation.tailrec

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
    /** The single [[ApolloStore]] backing this client's normalized cache, or
      * `Absent` when the client was built without `normalizedCache(...)` — the store
      * built once by the builder and reached here by locating the installed
      * [[CacheInterceptor]]. Both `watch()` and imperative
      * `writeOperation(...)` updates go through this one store, so a write and a
      * watch see the same cache and the same changed-keys stream.
      *
      * The total form: ask it whether a client has a cache at all (a view that
      * degrades to a constant value without one, a devtools panel that shows an
      * empty cache).
      */
    def normalizedStore: Maybe[ApolloStore] =
        Maybe.fromOption(client.registeredInterceptors.collectFirst { case interceptor: CacheInterceptor =>
            interceptor.store
        })

    /** [[normalizedStore]] for code that requires the cache — `watch()`, a
      * `ClientField`, a test of a client built with `normalizedCache(...)`.
      *
      * Using it on a client without a cache is a programmer error, not a failure a
      * caller handles: it panics with an [[kyo.apollo.exception.ApolloConfigException]]
      * and appears on no row. Code that has to cope with a client without a cache
      * asks [[normalizedStore]] instead of catching this.
      */
    def apolloStore(using Frame): ApolloStore =
        normalizedStore.getOrElse(
            throw ApolloConfigException(
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
      * is the stream body. The change notification is an effect the store runs on
      * the publishing fiber — `changedKeys.subscribe` registers it for the stream's
      * `Scope` — and cache re-reads push into the channel via `channel.unsafe.offer`.
      * The two *async* legs (the initial fetch and a network refetch) run as
      * detached fibers (`Fiber.Unsafe.init`) that drive `call.stream` /
      * `client.executeAsStream` into the same channel — the idiomatic callback→Kyo
      * interop, mirroring the WS transport and `kyo-ui`'s reactive bridge. Those
      * fetch streams are finite, so they complete on their own; teardown only flips
      * `active`, closes the channel, and ends the subscription.
      *
      * The reactions run on three different contexts — the detached fetch fibers,
      * whichever fiber writes to the store (its `publish` runs `onChangedKeys`
      * before the write returns), and the `Scope` teardown — so the watch's mutable state
      * lives in one [[AtomicRef]] over a [[WatchState]]: "still active?" and "which
      * keys?" are read together, and teardown's `active = false` is visible to every
      * later callback through the CAS.
      *
      * A key set is adopted together with the [[ApolloStore.currentGeneration]] its
      * read was current at. A write published between that read and the adoption
      * was intersected against the previous set — and possibly dropped — but it
      * moved the store's generation past the read's stamp, which the watch checks
      * right after adopting and answers with the reaction the dropped publish would
      * have caused. A watch therefore never settles on a value that a write in its
      * own dependency set has already overtaken.
      *
      * The adopted key set is retained in the store (see [[ApolloStore.retain]]) until
      * the next set replaces it or the `Scope` closes: a garbage collection keeps the
      * records the watch's last read depended on even when no operation root reaches
      * them any more — a query whose root field was re-pointed keeps its records until
      * the watch has read again. A record collected between a read and the adoption of
      * its set is published like a write, so it moves the generation past that read's
      * stamp and the watch reads again.
      *
      * A network refetch (`RefetchPolicy.NetworkOnly`, or `CacheFirst` answering a
      * miss) runs at most one fiber at a time; requests during that flight book a
      * single rerun. Under `CacheFirst` a miss is sent to the network once per
      * cause: if no read has succeeded by the end of that flight, the miss is
      * emitted as the value, as `CacheOnly` would, and so is any later miss until a
      * read succeeds again — a read the write-back cannot satisfy never becomes an
      * endless chain of requests.
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

                val state = AtomicRef.Unsafe.init(WatchState.initial)

                // The one path by which the key set changes: move to `keys` if `transition`
                // allows it on the state as it is, retaining the set in the store so a
                // garbage collection keeps the records the watch depends on. The move goes
                // hand over hand (`swapRetained`): `keys` is retained before the
                // compare-and-set, and the set it replaced — or `keys`, if `transition`
                // declined — is released after it, so no interleaving of two moves leaves
                // a watched record unpinned. A read that yields the set already held (the
                // usual re-read) moves no retain. Returns whether `keys` was installed.
                def hold(keys: Set[CacheKey])(transition: WatchState => Maybe[WatchState]): Boolean =
                    val s = state.get()
                    transition(s) match
                        case Absent => false
                        case Present(next) =>
                            if s.keys == keys && state.compareAndSet(s, next) then true
                            else store.swapRetained(keys)(install(transition))
                    end match
                end hold

                @tailrec def install(transition: WatchState => Maybe[WatchState]): Maybe[Set[CacheKey]] =
                    val s = state.get()
                    transition(s) match
                        case Absent        => Absent
                        case Present(next) => if state.compareAndSet(s, next) then Present(s.keys) else install(transition)
                end install

                // Adopt `keys` together with the store generation `gen` their read was
                // current at, unless the watch has ended or the set already held comes
                // from a younger read (an older read must not roll it back; its datum is
                // then not offered either). Every caller stands behind a read that
                // SUCCEEDED, which settles the miss a `CacheFirst` refetch may have been
                // asked for: `refetched` is cleared in the same compare-and-set.
                def adopt(keys: Set[CacheKey], gen: Long): Boolean =
                    hold(keys)(s =>
                        if !s.active || gen < s.gen then Absent
                        else Present(s.copy(keys = keys, gen = gen, refetched = false))
                    )

                // The write-back fallback for a networked response the store cannot read
                // back: seeds a watch that has no read behind its set yet (generation 0)
                // and never rolls back one that has. No read succeeded here, so the miss
                // a refetch was asked for stays unsettled.
                def seed(keys: Set[CacheKey]): Unit =
                    discard(hold(keys)(s => if !s.active || s.gen != 0L then Absent else Present(s.copy(keys = keys))))

                // Close the window between a read and the adoption of its key set. A
                // write published in between was intersected against the PREVIOUS set
                // and may have been dropped, but it left `currentGeneration` above the
                // stamp the read carried — so react to it exactly as `onChangedKeys`
                // would have. Terminates: each pass adopts the generation of its own
                // read, and a network refetch publishes BEFORE the read that establishes
                // it, so only a genuinely concurrent write drives another pass.
                def closeWindow(): Unit < Sync =
                    Sync.Unsafe.defer {
                        val s = state.get()
                        if !s.active then Kyo.unit
                        else store.currentGeneration.map(current => if s.gen < current then react() else Kyo.unit)
                    }

                // Only ever called from inside a running reaction, so the unsafe read and
                // offer happen when the reaction runs, not when it is built.
                def offer(response: ApolloResponse[D]): Unit =
                    if state.get().active then discard(channel.unsafe.offer(response))

                // The store read behind every reaction: `hit` gets the data, its key set and
                // the generation it was read at; `miss` gets the read's failure. Nothing
                // else is a miss — a decode defect or an interrupt is a panic and passes
                // through untouched.
                def readStamped(
                    hit: (D, Set[CacheKey], Long) => Unit < Sync,
                    miss: CacheReadFailure => Unit < Sync
                ): Unit < Sync =
                    Abort.recover[CacheReadFailure](miss)(
                        store.readOperationStamped(request.operation).map(hit(_, _, _))
                    )

                // Launch the initial fetch as a detached fiber that pushes each emission
                // through `emitFresh`. Network refetches go through `requestRefetch`.
                def spawn(src: ResponseStream[D]): Unit =
                    val _ = Fiber.Unsafe.init[Throwable, Unit](Scope.run(src.foreach(emitFresh)))

                // Refresh the watch set from a just-emitted response. A cache hit already
                // carries the `dependentKeys` its read touched and the generation it was
                // read at; a networked response derives them by reading the records it
                // just wrote back out of the store — falling back to the write-back's own
                // changed keys (stamped on `CacheInfo.dependentKeys` by the cache
                // interceptor) when that re-read cannot be satisfied, so a watcher is
                // never born dead. The fallback carries generation 0: it only ever seeds
                // a watch that has no read behind its set yet, and never rolls back one
                // that has. Only a set backed by a real read closes its window.
                def establishFrom(response: ApolloResponse[D]): Unit < Sync =
                    val info    = response.cacheInfo
                    val stamped = info.map(_.dependentKeys).getOrElse(Set.empty)
                    if info.exists(_.isCacheHit) && stamped.nonEmpty then
                        Sync.Unsafe.defer(discard(adopt(stamped, info.map(_.generation).getOrElse(0L))))
                            .andThen(closeWindow())
                    else
                        readStamped(
                            hit = (_, keys, gen) =>
                                discard(adopt(keys, gen))
                                closeWindow()
                            ,
                            miss = _ => Sync.Unsafe.defer(if stamped.nonEmpty then seed(stamped))
                        )
                    end if
                end establishFrom

                def emitFresh(response: ApolloResponse[D]): Unit < Sync =
                    Sync.Unsafe.defer {
                        if !state.get().active then Kyo.unit
                        else
                            offer(response)
                            establishFrom(response)
                    }

                // Re-read through the same denormalization path as the first read, so a
                // re-emitted value equals a fresh read. What a read that now MISSES means is
                // the caller's policy, which is why the miss leg is a parameter: `CacheOnly`
                // surfaces it as a value, `CacheFirst` goes to the network for it.
                //
                // Either way the watch set is KEPT, so a later write restoring the data
                // revives the watcher (Apollo Client watchers stay registered across
                // incomplete reads). A watch that never established a set stays silent.
                def reread(onMiss: CacheReadFailure => Unit < Sync): Unit < Sync =
                    readStamped(
                        hit = (data, keys, gen) =>
                            if adopt(keys, gen) then offer(CacheResponses.hit(request, data, keys, gen))
                            closeWindow()
                        ,
                        miss = onMiss
                    )

                /** [[RefetchPolicy.CacheOnly]]'s miss leg: the miss IS the value. */
                def emitMiss(failure: CacheReadFailure): Unit < Sync =
                    Sync.Unsafe.defer {
                        val s = state.get()
                        if s.active && s.keys.nonEmpty then offer(CacheResponses.miss(request, failure))
                    }

                // Re-run the operation over the network (which writes the response back
                // into the store) and emit the networked value — at most one such fiber
                // at a time. A request that arrives while one is in flight books a single
                // rerun instead of a second fiber: the rerun starts after the writes that
                // asked for it, so its response covers all of them, and the in-flight
                // response is emitted regardless. n writes in a row cost at most two
                // network requests (the running one and the rerun).
                def requestRefetch(): Unit < Sync = Sync.Unsafe.defer(bookRefetch())

                @tailrec def bookRefetch(): Unit =
                    val s = state.get()
                    if !s.active then ()
                    else if s.inflight then
                        if !state.compareAndSet(s, s.copy(rerun = true)) then bookRefetch()
                    else if state.compareAndSet(s, s.copy(inflight = true, rerun = false)) then
                        val networked = request.newBuilder
                            .addExecutionContext(ExecutionContext.Empty + FetchPolicy.NetworkOnly)
                            .build()
                        discard(Fiber.Unsafe.init[Throwable, Unit](
                            Scope.run(
                                Sync.ensure(finishRefetch())(
                                    client.executeAsStream(networked).foreach(emitFresh)
                                )
                            )
                        ))
                    else bookRefetch()
                    end if
                end bookRefetch

                // Runs when the refetch fiber ends, however it ends: hand the flight back
                // and run the one booked rerun, if any. Otherwise, if a `CacheFirst` miss
                // is still unsettled — no read succeeded during the flight, so the
                // write-back did not cure it — the miss is now the value: one re-read,
                // which emits the miss (or a hit, if a concurrent write cured it meanwhile).
                def finishRefetch(): Unit < Sync =
                    Sync.Unsafe.defer {
                        val before = state.getAndUpdate(_.copy(inflight = false, rerun = false))
                        if before.rerun then requestRefetch()
                        else if before.refetched then reread(emitMiss)
                        else Kyo.unit
                    }

                // CacheFirst's miss leg: the first miss for a cause goes to the network,
                // once. A miss with no successful read since that request is the value,
                // exactly as under CacheOnly: emitted right away, or — while the refetch
                // is still in flight — settled once at the end of that flight, because
                // the refetch's own write-back publishes (and so re-reads) while its fiber
                // is still running. Without this rule a permanent miss plus a volatile
                // field in every response (each write-back publishes a watched key, each
                // re-read misses again) is an endless chain of network requests out of a
                // single watch.
                def refetchOnce(failure: CacheReadFailure): Unit < Sync = Sync.Unsafe.defer(settleMiss(failure))

                @tailrec def settleMiss(failure: CacheReadFailure): Unit < Sync =
                    val s = state.get()
                    if !s.active then Kyo.unit
                    else if s.refetched && s.inflight then Kyo.unit // settled once, at the end of the flight
                    else if s.refetched then emitMiss(failure)
                    else if state.compareAndSet(s, s.copy(refetched = true)) then requestRefetch()
                    else settleMiss(failure)
                    end if
                end settleMiss

                // The reaction a change in the watch set calls for, by policy — shared by
                // the changed-keys callback and by `closeWindow`, so a write that slipped
                // past the callback is answered the same way it would have been.
                def react(): Unit < Sync =
                    refetchPolicy match
                        case RefetchPolicy.CacheOnly   => reread(emitMiss)
                        case RefetchPolicy.NetworkOnly => requestRefetch()
                        case RefetchPolicy.CacheFirst  => reread(refetchOnce)

                // Runs on the publishing fiber, before that write returns.
                def onChangedKeys(changedKeys: Set[CacheKey]): Unit < Sync =
                    Sync.Unsafe.defer {
                        val s = state.get() // one read: `active` and `keys` belong together
                        if s.active && changedKeys.intersect(s.keys).nonEmpty then react() else Kyo.unit
                    }

                // Subscribe to the store *before* the initial fetch so a write landing
                // during the fetch is never missed; the initially empty key set guards
                // against re-emitting for the initial fetch's own write-back (nothing
                // intersects the empty set). The subscription ends with the stream's
                // Scope; the teardown registered after it runs first, flips `active`
                // and releases the last retained key set in the same step, so no
                // reaction offers anything once the Scope is closing and no later move
                // retains a set again (`hold` declines once the watch is inactive).
                store.changedKeys.subscribe(onChangedKeys)
                    .andThen(Scope.ensure(Sync.Unsafe.defer {
                        val before = state.getAndUpdate(_.copy(active = false, keys = Set.empty))
                        if before.active then store.releaseRetained(before.keys)
                        discard(channel.unsafe.close())
                    }))
                    .andThen(Sync.Unsafe.defer(spawn(call.stream)))
                    .andThen(channel.streamUntilClosed())
            }
        }
    end watch
end extension

/** The whole mutable state of one `watch()`, held in a single [[AtomicRef]] so a
  * callback reads "still active?" and "which keys?" as one snapshot and every
  * transition is one compare-and-set. `active` is cleared once by the `Scope`
  * teardown and never set again; `keys` is the dependent-key set of the last
  * established read — empty until the initial fetch has landed and again once the
  * watch has ended, and retained in the store while it is installed — and `gen` the
  * [[ApolloStore.currentGeneration]] that read was current at (0 for a set that
  * no store read stands behind, i.e. the initial state and the write-back
  * fallback).
  *
  * The network refetch is guarded by the same cell: `inflight` while its fiber
  * runs, `rerun` when a further request arrived during that flight (at most one
  * is booked; it runs once the fiber ends), and `refetched` once a `CacheFirst`
  * miss has been answered with a refetch and no read has succeeded since — a
  * miss is then emitted (once the flight has ended) instead of refetched again.
  *
  * Transitions go through `copy`, so a further per-watch fact is added as a
  * field here and rides the same CAS rather than a second cell.
  */
final private[normalized] case class WatchState(
    active: Boolean,
    keys: Set[CacheKey],
    gen: Long,
    inflight: Boolean,
    rerun: Boolean,
    refetched: Boolean
)

private[normalized] object WatchState:
    val initial: WatchState =
        WatchState(active = true, keys = Set.empty, gen = 0L, inflight = false, rerun = false, refetched = false)
