package kyo.apollo.cache.normalized

import kyo.<
import kyo.Abort
import kyo.AllowUnsafe
import kyo.AtomicRef
import kyo.Chunk
import kyo.Frame
import kyo.Interrupted
import kyo.Kyo
import kyo.Log
import kyo.Result
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
  * registration and removal swaps in a new list. Four guarantees:
  *
  *   - **Deterministic order.** Subscribers are notified in registration order
  *     (FIFO): [[subscribe]] appends, [[publish]] runs them front-to-back, one after
  *     another on the publishing fiber.
  *   - **Re-entrancy-safe.** [[publish]] delivers over the list as it was when the
  *     publish started, so a subscriber that writes (and thus re-publishes) *during*
  *     delivery, or a subscription made or ended on another fiber meanwhile, does
  *     not disturb this emission — the change takes effect on the next publish.
  *   - **Idempotent teardown.** A subscription ends when the `Scope` it was made in
  *     closes, and only then; removal filters by identity, so it removes exactly that
  *     registration and is harmless to repeat.
  *   - **Failure isolation.** A subscriber whose effect fails (a thrown exception or
  *     an `Abort.panic`) is reported through `Log.error` with its cause and skipped
  *     for this emission only: every subscriber after it receives the same set, the
  *     failing one stays subscribed, and [[publish]] itself succeeds — so a write
  *     that landed is never reported to its caller as failed. An [[Interrupted]]
  *     panic is not a subscriber failure: the publishing fiber is being torn down,
  *     so it ends the delivery and passes through [[publish]] untouched.
  *
  * Mirrors the callback bus in apollo-kotlin's `DefaultApolloStore`.
  */
final class ChangedKeysSubject:

    /** A single registration. Identity (`eq`) distinguishes subscriptions, so a
      * callback may be registered more than once and each is removed on its own.
      * `registeredAt` names the subscriber in the log line of a failed delivery.
      */
    final private class Subscription(val onChangedKeys: Set[CacheKey] => Unit < Sync, val registeredAt: Frame)

    /** Current subscriptions in registration (FIFO) order. */
    private val subscriptions: AtomicRef[Chunk[Subscription]] =
        AtomicRef.Unsafe.init(Chunk.empty[Subscription])(using AllowUnsafe.embrace.danger).safe

    /** Register `onChangedKeys` to run for every subsequent non-empty changed-key set
      * until the enclosing `Scope` closes. The same callback may be subscribed several
      * times; each registration is removed on its own.
      */
    def subscribe(onChangedKeys: Set[CacheKey] => Unit < Sync)(using frame: Frame): Unit < (Sync & Scope) =
        Scope.acquireRelease(
            Sync.defer(new Subscription(onChangedKeys, frame)).map { subscription =>
                subscriptions.updateAndGet(_.append(subscription)).andThen(subscription)
            }
        )(subscription => subscriptions.updateAndGet(_.filter(_ ne subscription)).unit).unit

    /** Run every current subscriber on `changedKeys`, in registration order. A no-op
      * when the set is empty (an identical re-write changes nothing, so nobody is
      * notified). Subscribers are read once, before delivery, so a write or a
      * subscription change mid-delivery does not disturb this emission. A failing
      * subscriber is logged and the delivery moves on; only an [[Interrupted]]
      * panic fails the publish.
      */
    def publish(changedKeys: Set[CacheKey])(using Frame): Unit < Sync =
        if changedKeys.isEmpty then Kyo.unit
        else subscriptions.get.map(current => Kyo.foreachDiscard(current)(deliver(_, changedKeys)))

    /** Run one subscriber, containing its failure to itself (see "Failure isolation"). */
    private def deliver(subscription: Subscription, changedKeys: Set[CacheKey])(using Frame): Unit < Sync =
        Abort.run[Throwable](subscription.onChangedKeys(changedKeys)).map {
            case Result.Success(_)            => Kyo.unit
            case Result.Error(e: Interrupted) => Abort.panic(e)
            case Result.Error(e) =>
                Log.error(
                    s"changed-keys subscriber registered at ${subscription.registeredAt.position.show} failed on a " +
                        s"publish of ${changedKeys.size} key(s); the subscribers after it still receive it",
                    e
                )
        }

    /** The number of active subscriptions — for tests and diagnostics. */
    private[cache] def subscriberCount(using Frame): Int < Sync = subscriptions.get.map(_.size)
end ChangedKeysSubject
