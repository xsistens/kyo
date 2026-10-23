package kyo.apollo.cache.normalized

import kyo.<
import kyo.Absent
import kyo.AllowUnsafe
import kyo.AtomicRef
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.Scope
import kyo.Sync
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.discard
import scala.annotation.tailrec

/** The keys a store follower watches, retained in its store for as long as the
  * follower lives: a garbage collection keeps the records the last read depended on
  * even when no operation root reaches them any more.
  *
  * `held` is `Absent` once the enclosing `Scope` has closed. A [[hold]] moves the
  * retain hand over hand through [[ApolloStore.swapRetained]], so a key the follower
  * still watches never counts as unretained, and a hold that arrives after the
  * teardown retains nothing.
  */
final private[apollo] class RetainedKeys private (store: ApolloStore, held: AtomicRef.Unsafe[Maybe[Set[CacheKey]]]):

    /** Watch and retain `keys` in place of the keys held so far. */
    def hold(keys: Set[CacheKey])(using Frame): Unit < Sync =
        Sync.Unsafe.defer(discard(store.swapRetained(keys)(install(keys))))

    /** Whether `changed` names a key held now; never once the `Scope` has closed. */
    def touches(changed: Set[CacheKey])(using Frame): Boolean < Sync =
        Sync.Unsafe.defer(held.get().exists(keys => changed.exists(keys.contains)))

    @tailrec private def install(keys: Set[CacheKey])(using AllowUnsafe): Maybe[Set[CacheKey]] =
        val current = held.get()
        current match
            case Absent            => Absent
            case Present(replaced) =>
                if held.compareAndSet(current, Present(keys)) then Present(replaced) else install(keys)
        end match
    end install

    private def release(using Frame): Unit < Sync =
        Sync.Unsafe.defer(held.getAndSet(Absent).foreach(keys => store.releaseRetained(keys)))
end RetainedKeys

private[apollo] object RetainedKeys:
    /** Retain `keys` until the enclosing `Scope` closes. */
    def init(store: ApolloStore, keys: Set[CacheKey])(using Frame): RetainedKeys < (Sync & Scope) =
        val nothingHeld: Maybe[Set[CacheKey]] = Present(Set.empty)
        Sync.Unsafe.defer(new RetainedKeys(store, AtomicRef.Unsafe.init(nothingHeld))).map { retained =>
            Scope.ensure(retained.release).andThen(retained.hold(keys)).andThen(retained)
        }
    end init
end RetainedKeys
