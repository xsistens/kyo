package kyo.apollo.cache.normalized

import kyo.apollo.cache.normalized.api.CacheKey

/** The store's change-notification bus: a hot, multicast source of the
  * `Set[CacheKey]` of record keys mutated by every write.
  *
  * This is the single seam Phase 05 watchers observe. Every store write path
  * ([[ApolloStore.writeOperation]], [[ApolloStore.remove]], and the manual
  * [[ApolloStore.publish]] escape hatch) funnels its changed keys through
  * [[publish]], which fans them out to each current subscriber. A watcher
  * [[subscribe]]s, compares each emitted set against the keys its last read
  * depended on, and re-emits only when they intersect — so an unrelated write
  * notifies nobody who cares.
  *
  * A plain synchronous callback bus (the effect pivot, Schritt 2.2, keeps this
  * hot in-memory multicast as-is rather than forcing it onto `Hub` — its writes
  * come from synchronous store mutations): [[subscribe]] returns an unsubscribe
  * thunk (`() => Unit`) that removes exactly that subscription. Three guarantees
  * the task requires:
  *
  *   - **Deterministic order.** Subscribers are notified in registration order
  *     (FIFO): [[subscribe]] appends, [[publish]] iterates front-to-back.
  *   - **Re-entrancy-safe.** [[publish]] snapshots the subscriber list before
  *     iterating, so a subscriber that writes (and thus re-publishes) or
  *     unsubscribes *during* delivery cannot mutate the list being walked — the
  *     change takes effect on the next publish. Single-threaded JS means no true
  *     concurrency, but the snapshot keeps nested/self-triggered publishes sound.
  *   - **Idempotent teardown.** A subscription's [[Cancelable]] removes it at most
  *     once; double-close and unsubscribe-during-delivery are both safe.
  *
  * Mirrors the callback bus in apollo-kotlin's `DefaultApolloStore`.
  */
final class ChangedKeysSubject:

    /** A single registration. Identity (`eq`) distinguishes subscriptions, so a
      * callback may be registered more than once and each removes independently.
      */
    final private class Subscription(val onChangedKeys: Set[CacheKey] => Unit)

    /** Current subscriptions in registration (FIFO) order. Reassigned wholesale
      * (never mutated in place) so a snapshot taken by [[publish]] stays stable
      * even as concurrent/nested calls register or remove.
      */
    private var subscriptions: Vector[Subscription] = Vector.empty

    /** Register `onChangedKeys` to receive every subsequent non-empty changed-key
      * set, and return an unsubscribe thunk that removes this subscription. The same
      * callback may be subscribed multiple times; each returned thunk removes only
      * its own registration. Removal is idempotent (identity `filterNot`), so
      * calling the thunk more than once is safe.
      */
    def subscribe(onChangedKeys: Set[CacheKey] => Unit): () => Unit =
        val subscription = new Subscription(onChangedKeys)
        synchronized { subscriptions = subscriptions :+ subscription }
        () => remove(subscription)
    end subscribe

    /** Remove every subscription registered for `onChangedKeys` (by identity). The
      * `removeChangedKeysListener` teardown for callers that hold the callback
      * rather than the [[Cancelable]]; a no-op if it was never subscribed.
      */
    def unsubscribe(onChangedKeys: Set[CacheKey] => Unit): Unit = synchronized {
        subscriptions = subscriptions.filterNot(_.onChangedKeys eq onChangedKeys)
    }

    /** Fan `changedKeys` out to every current subscriber, in registration order.
      * A no-op when the set is empty (an identical re-write changes nothing, so
      * nobody is notified). Subscribers are snapshotted before delivery, so one
      * that writes or unsubscribes mid-delivery does not disturb this emission.
      */
    def publish(changedKeys: Set[CacheKey]): Unit =
        if changedKeys.nonEmpty then
            val current = synchronized(subscriptions)
            current.foreach(_.onChangedKeys(changedKeys))

    /** The number of active subscriptions — for tests and diagnostics. */
    private[cache] def subscriberCount: Int = synchronized(subscriptions.size)

    private def remove(subscription: Subscription): Unit = synchronized {
        subscriptions = subscriptions.filterNot(_ eq subscription)
    }
end ChangedKeysSubject
