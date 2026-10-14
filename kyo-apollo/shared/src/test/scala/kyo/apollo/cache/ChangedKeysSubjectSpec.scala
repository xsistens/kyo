package kyo.apollo.cache

import kyo.*
import kyo.apollo.cache.normalized.ChangedKeysSubject
import kyo.apollo.cache.normalized.api.CacheKey

/** Unit tests for the change-notification bus ([[ChangedKeysSubject]]): multicast
  * delivery, FIFO ordering, Scope-bound subscriptions (removed per registration,
  * only by their Scope), the empty-set no-op, and the hard guarantees — re-entrancy
  * safety (a write during delivery), idempotent teardown, and failure isolation (a
  * failing subscriber is logged and starves no one; an interrupt is not swallowed).
  * Subscribers are effects the publish runs on the publishing fiber, so a local
  * records what they saw by the time the publish returns.
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

        "one callback subscribed in two Scopes ends with each Scope on its own" in {
            val subject                                = new ChangedKeysSubject
            var calls                                  = 0
            val listener: Set[CacheKey] => Unit < Sync = _ => calls += 1
            for
                _     <- subject.subscribe(listener)
                _     <- Scope.run(subject.subscribe(listener))
                _     <- subject.publish(Set(k))
                count <- subject.subscriberCount
            yield
                assert(calls == 1) // the inner registration is gone, the outer one of the same callback is not
                assert(count == 1)
            end for
        }

        "a subscription has no way to end but its Scope" in {
            typeCheckFailure("""
                val subject = new kyo.apollo.cache.normalized.ChangedKeysSubject
                subject.unsubscribe(_ => kyo.Kyo.unit)(using kyo.Frame.internal)
            """)("value unsubscribe is not a member of kyo.apollo.cache.normalized.ChangedKeysSubject")
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

        "a subscriber failing inside a nested publish fails neither publish" in {
            val subject = new ChangedKeysSubject
            val boom    = new RuntimeException("inner boom")
            for
                probe   <- LogProbe.init
                seen    <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _       <- subject.subscribe(keys => if keys == Set(outer) then subject.publish(Set(inner)) else Kyo.unit)
                _       <- subject.subscribe(keys => if keys == Set(inner) then Abort.panic(boom) else Kyo.unit)
                _       <- subject.subscribe(keys => seen.updateAndGet(_.append(keys)).unit)
                outcome <- Abort.run[Throwable](probe.run(subject.publish(Set(outer))))
                last    <- seen.get
                errors  <- probe.errors
            yield
                assert(outcome == Result.unit, s"the outer publish failed: $outcome")
                assert(last == Chunk(Set(inner), Set(outer)))
                assert(errors.map(_.error) == Chunk(Present(boom)))
            end for
        }

        "a failing subscriber does not starve the ones after it" in {
            val subject = new ChangedKeysSubject
            val boom    = new RuntimeException("boom")
            for
                probe   <- LogProbe.init
                first   <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                third   <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _       <- subject.subscribe(keys => first.updateAndGet(_.append(keys)).unit)
                _       <- subject.subscribe(_ => Abort.panic(boom))
                _       <- subject.subscribe(keys => third.updateAndGet(_.append(keys)).unit)
                outcome <- Abort.run[Throwable](probe.run(subject.publish(Set(k))))
                before  <- first.get
                after   <- third.get
                errors  <- probe.errors
            yield
                assert(outcome == Result.unit, s"the publish itself failed: $outcome")
                assert(before == Chunk(Set(k)))
                assert(after == Chunk(Set(k)), "the subscriber after the failing one never saw the change")
                assert(errors.size == 1, s"expected exactly one error line, got $errors")
                assert(errors.head.error == Present(boom))
                assert(errors.head.message.contains("ChangedKeysSubjectSpec.scala")) // names where the failing one subscribed
            end for
        }

        "a subscriber that throws is isolated like one that panics" in {
            val subject = new ChangedKeysSubject
            val boom    = new IllegalStateException("thrown")
            for
                probe   <- LogProbe.init
                later   <- AtomicRef.init(Chunk.empty[Set[CacheKey]])
                _       <- subject.subscribe(_ => Sync.defer(throw boom))
                _       <- subject.subscribe(keys => later.updateAndGet(_.append(keys)).unit)
                outcome <- Abort.run[Throwable](probe.run(subject.publish(Set(k))))
                seen    <- later.get
                errors  <- probe.errors
            yield
                assert(outcome == Result.unit, s"the publish itself failed: $outcome")
                assert(seen == Chunk(Set(k)))
                assert(errors.map(_.error) == Chunk(Present(boom)))
            end for
        }

        "an Interrupted panic is not swallowed" in {
            val subject   = new ChangedKeysSubject
            val interrupt = Interrupted(summon[Frame], "the test")
            for
                probe   <- LogProbe.init
                later   <- AtomicInt.init(0)
                _       <- subject.subscribe(_ => Abort.panic(interrupt))
                _       <- subject.subscribe(_ => later.incrementAndGet.unit)
                outcome <- Abort.run[Throwable](probe.run(subject.publish(Set(k))))
                calls   <- later.get
                errors  <- probe.errors
            yield
                assert(outcome == Result.panic(interrupt), s"the interrupt did not reach the publisher: $outcome")
                assert(calls == 0) // an interrupt ends the publish; it is not a subscriber's failure to skip
                assert(errors.isEmpty, s"an interrupt is not logged as a failure: $errors")
            end for
        }
    }
end ChangedKeysSubjectSpec
