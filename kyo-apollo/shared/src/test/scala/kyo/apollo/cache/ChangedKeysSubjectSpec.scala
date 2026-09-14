package kyo.apollo.cache

import kyo.*
import kyo.apollo.cache.normalized.ChangedKeysSubject
import kyo.apollo.cache.normalized.api.CacheKey

/** Unit tests for the change-notification bus ([[ChangedKeysSubject]]): multicast
  * delivery, FIFO ordering, Scope-bound subscriptions and removal by callback
  * identity, the empty-set no-op, and the two hard guarantees — re-entrancy safety
  * (write/unsubscribe during delivery) and idempotent teardown. Subscribers are
  * effects the publish runs on the publishing fiber, so a plain local records what
  * they saw by the time the publish returns.
  */
class ChangedKeysSubjectSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val user1 = CacheKey("User", "1")
    private val k     = CacheKey("K", "1")
    private val outer = CacheKey("Outer", "1")
    private val inner = CacheKey("Inner", "1")

    "ChangedKeysSubject" - {

        "publish fans a changed-key set out to every subscriber" in {
            val subject = new ChangedKeysSubject
            var a       = Set.empty[CacheKey]
            var b       = Set.empty[CacheKey]
            for
                _ <- subject.subscribe(keys => a = keys)
                _ <- subject.subscribe(keys => b = keys)
                _ <- subject.publish(Set(user1))
            yield
                assert(a == Set(user1))
                assert(b == Set(user1))
            end for
        }

        "subscribers are notified in registration (FIFO) order" in {
            val subject = new ChangedKeysSubject
            var order   = List.empty[Int]
            for
                _ <- subject.subscribe(_ => order = order :+ 1)
                _ <- subject.subscribe(_ => order = order :+ 2)
                _ <- subject.subscribe(_ => order = order :+ 3)
                _ <- subject.publish(Set(k))
            yield assert(order == List(1, 2, 3))
            end for
        }

        "publish of an empty set notifies no one" in {
            val subject = new ChangedKeysSubject
            var calls   = 0
            for
                _ <- subject.subscribe(_ => calls += 1)
                _ <- subject.publish(Set.empty)
            yield assert(calls == 0)
            end for
        }

        "closing the Scope a subscription was made in removes it" in {
            val subject = new ChangedKeysSubject
            var calls   = 0
            for
                _     <- Scope.run(subject.subscribe(_ => calls += 1).andThen(subject.publish(Set(k))))
                _     <- subject.publish(Set(k))
                count <- subject.subscriberCount
            yield
                assert(calls == 1)
                assert(count == 0)
            end for
        }

        "an inner Scope's teardown removes only its own subscription" in {
            val subject = new ChangedKeysSubject
            for
                _      <- subject.subscribe(_ => ()) // a subscription that must survive
                during <- Scope.run(subject.subscribe(_ => ()).andThen(subject.subscriberCount))
                after  <- subject.subscriberCount
            yield
                assert(during == 2)
                assert(after == 1)
            end for
        }

        "unsubscribe removes a subscription by callback identity" in {
            val subject                                = new ChangedKeysSubject
            var calls                                  = 0
            val listener: Set[CacheKey] => Unit < Sync = _ => calls += 1
            for
                _ <- subject.subscribe(listener)
                _ <- subject.unsubscribe(listener)
                _ <- subject.unsubscribe(listener) // idempotent
                _ <- subject.publish(Set(k))
            yield assert(calls == 0)
            end for
        }

        "the same callback subscribed twice is delivered to twice" in {
            val subject                                = new ChangedKeysSubject
            var calls                                  = 0
            val listener: Set[CacheKey] => Unit < Sync = _ => calls += 1
            for
                _ <- subject.subscribe(listener)
                _ <- subject.subscribe(listener)
                _ <- subject.publish(Set(k))
            yield assert(calls == 2)
            end for
        }

        "a subscriber that publishes during delivery does not disturb this emission" in {
            val subject = new ChangedKeysSubject
            var seen    = List.empty[Set[CacheKey]]
            // First subscriber re-publishes on the outer set. Because the outer publish
            // reads the subscriber list before delivering, the nested "inner" publish runs
            // to completion inside sub1's delivery, and the outer emission still reaches
            // sub2 with "outer" afterward — so sub2 receives BOTH, nothing is lost.
            for
                _ <- subject.subscribe(keys => if keys == Set(outer) then subject.publish(Set(inner)) else Kyo.unit)
                _ <- subject.subscribe(keys => seen = seen :+ keys)
                _ <- subject.publish(Set(outer))
            yield assert(seen == List(Set(inner), Set(outer)))
            end for
        }

        "a subscriber that unsubscribes another during delivery is re-entrancy-safe" in {
            val subject                              = new ChangedKeysSubject
            var secondCalls                          = 0
            val second: Set[CacheKey] => Unit < Sync = _ => secondCalls += 1
            // First subscriber removes the second mid-delivery; the second still receives
            // THIS emission (the list was read before delivery), and is gone from the next.
            for
                _ <- subject.subscribe(_ => subject.unsubscribe(second))
                _ <- subject.subscribe(second)
                _ <- subject.publish(Set(k))
                afterFirst = secondCalls
                _ <- subject.publish(Set(k))
                afterSecond = secondCalls
            yield
                assert(afterFirst == 1)  // delivered from the list as it was
                assert(afterSecond == 1) // gone on the next emission
            end for
        }
    }
end ChangedKeysSubjectSpec
