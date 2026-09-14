package kyo.apollo.cache.normalized

import kyo.<
import kyo.AllowUnsafe
import kyo.AtomicRef
import kyo.Chunk
import kyo.Frame
import kyo.Kyo
import kyo.Scope
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheKey

/** The store's change-notification bus: a hot, multicast source of the
  * `Set[CacheKey]` of record keys mutated by every write.
  *
  * This is the single seam watchers observe. Every store write path
  * ([[ApolloStore.writeOperation]], [[ApolloStore.remove]], and the manual
  * [[ApolloStore.publish]] escape hatch) funnels its changed keys through
  * [[publish]], which runs each current subscriber's effect in turn. A watcher
  * [[subscribe]]s, compares each emitted set against the keys its last read
  * depended on, and re-reads only when they intersect — so an unrelated write
  * notifies nobody who cares.
  *
  * The subscriber list is one immutable [[Chunk]] behind an [[AtomicRef]]; every
  * registration and removal swaps in a new list. Three guarantees:
  *
  *   - **Deterministic order.** Subscribers are notified in registration order
  *     (FIFO): [[subscribe]] appends, [[publish]] runs them front-to-back, one after
  *     another on the publishing fiber.
  *   - **Re-entrancy-safe.** [[publish]] delivers over the list as it was when the
  *     publish started, so a subscriber that writes (and thus re-publishes) or
  *     unsubscribes *during* delivery does not disturb this emission — the change
  *     takes effect on the next publish.
  *   - **Idempotent teardown.** A subscription ends when the `Scope` it was made in
  *     closes; removal filters by identity and is harmless to repeat.
  *
  * A subscriber that fails aborts the publish that reached it (per-subscriber
  * isolation is not part of this bus yet). Mirrors the callback bus in
  * apollo-kotlin's `DefaultApolloStore`.
  */
final class ChangedKeysSubject:

    /** A single registration. Identity (`eq`) distinguishes subscriptions, so a
      * callback may be registered more than once and each removes independently.
      */
    final private class Subscription(val onChangedKeys: Set[CacheKey] => Unit < Sync)

    /** Current subscriptions in registration (FIFO) order. */
    private val subscriptions: AtomicRef[Chunk[Subscription]] =
        AtomicRef.Unsafe.init(Chunk.empty[Subscription])(using AllowUnsafe.embrace.danger).safe

    /** Register `onChangedKeys` to run for every subsequent non-empty changed-key set
      * until the enclosing `Scope` closes. The same callback may be subscribed several
      * times; each registration is removed on its own.
      */
    def subscribe(onChangedKeys: Set[CacheKey] => Unit < Sync)(using Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            Sync.defer(new Subscription(onChangedKeys)).map { subscription =>
                subscriptions.updateAndGet(_.append(subscription)).andThen(subscription)
            }
        )(subscription => subscriptions.updateAndGet(_.filter(_ ne subscription)).unit).unit

    /** Remove every subscription registered for `onChangedKeys` (by identity) — a
      * no-op if it was never subscribed.
      */
    def unsubscribe(onChangedKeys: Set[CacheKey] => Unit < Sync)(using Frame): Unit < Sync =
        subscriptions.updateAndGet(_.filter(_.onChangedKeys ne onChangedKeys)).unit

    /** Run every current subscriber on `changedKeys`, in registration order. A no-op
      * when the set is empty (an identical re-write changes nothing, so nobody is
      * notified). Subscribers are read once, before delivery, so one that writes or
      * unsubscribes mid-delivery does not disturb this emission.
      */
    def publish(changedKeys: Set[CacheKey])(using Frame): Unit < Sync =
        if changedKeys.isEmpty then Kyo.unit
        else subscriptions.get.map(current => Kyo.foreachDiscard(current)(_.onChangedKeys(changedKeys)))

    /** The number of active subscriptions — for tests and diagnostics. */
    private[cache] def subscriberCount(using Frame): Int < Sync = subscriptions.get.map(_.size)
end ChangedKeysSubject
