package kyo.apollo.cache

import kyo.apollo.cache.normalized.ChangedKeysSubject

/** Unit tests for the Phase 05 change-notification bus ([[ChangedKeysSubject]]):
  * multicast delivery, FIFO ordering, unsubscribe (via the returned `Cancelable`
  * and by callback identity), the empty-set no-op, and the two hard guarantees —
  * re-entrancy safety (write/unsubscribe during delivery) and idempotent teardown.
  */
class ChangedKeysSubjectSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "ChangedKeysSubject" - {

        "publish fans a changed-key set out to every subscriber" in {
            val subject = new ChangedKeysSubject
            var a       = Set.empty[String]
            var b       = Set.empty[String]
            subject.subscribe(keys => a = keys)
            subject.subscribe(keys => b = keys)
            subject.publish(Set("User:1"))
            assert(a == Set("User:1"))
            assert(b == Set("User:1"))
        }

        "subscribers are notified in registration (FIFO) order" in {
            val subject = new ChangedKeysSubject
            var order   = List.empty[Int]
            subject.subscribe(_ => order = order :+ 1)
            subject.subscribe(_ => order = order :+ 2)
            subject.subscribe(_ => order = order :+ 3)
            subject.publish(Set("k"))
            assert(order == List(1, 2, 3))
        }

        "publish of an empty set notifies no one" in {
            val subject = new ChangedKeysSubject
            var calls   = 0
            subject.subscribe(_ => calls += 1)
            subject.publish(Set.empty)
            assert(calls == 0)
        }

        "the cancel thunk returned by subscribe removes that subscription" in {
            val subject = new ChangedKeysSubject
            var calls   = 0
            val handle  = subject.subscribe(_ => calls += 1)
            subject.publish(Set("k"))
            handle()
            subject.publish(Set("k"))
            assert(calls == 1)
            assert(subject.subscriberCount == 0)
        }

        "calling the cancel thunk twice tears the subscription down at most once" in {
            val subject = new ChangedKeysSubject
            subject.subscribe(_ => ()) // a second subscription that must survive
            val handle = subject.subscribe(_ => ())
            assert(subject.subscriberCount == 2)
            handle()
            handle() // idempotent — must not remove the surviving subscription
            assert(subject.subscriberCount == 1)
        }

        "unsubscribe removes a subscription by callback identity" in {
            val subject                       = new ChangedKeysSubject
            var calls                         = 0
            val listener: Set[String] => Unit = _ => calls += 1
            subject.subscribe(listener)
            subject.unsubscribe(listener)
            subject.publish(Set("k"))
            assert(calls == 0)
        }

        "the same callback subscribed twice is delivered to twice" in {
            val subject                       = new ChangedKeysSubject
            var calls                         = 0
            val listener: Set[String] => Unit = _ => calls += 1
            subject.subscribe(listener)
            subject.subscribe(listener)
            subject.publish(Set("k"))
            assert(calls == 2)
        }

        "a subscriber that publishes during delivery does not disturb this emission" in {
            val subject = new ChangedKeysSubject
            var seen    = List.empty[Set[String]]
            // First subscriber re-publishes synchronously on the outer set. Because the
            // outer publish snapshots before iterating, the nested "inner" publish runs
            // to completion inside sub1's delivery, and the outer emission still reaches
            // sub2 with "outer" afterward — so sub2 receives BOTH, nothing is lost.
            subject.subscribe { keys =>
                if keys == Set("outer") then subject.publish(Set("inner"))
            }
            subject.subscribe(keys => seen = seen :+ keys)
            subject.publish(Set("outer"))
            assert(seen == List(Set("inner"), Set("outer")))
        }

        "a subscriber that unsubscribes another during delivery is re-entrancy-safe" in {
            val subject             = new ChangedKeysSubject
            var secondCalls         = 0
            var handle2: () => Unit = () => ()
            // First subscriber cancels the second mid-delivery; the snapshot means the
            // second still receives THIS emission, and is gone from the next one.
            subject.subscribe(_ => handle2())
            handle2 = subject.subscribe(_ => secondCalls += 1)
            subject.publish(Set("k"))
            assert(secondCalls == 1) // delivered from the pre-cancel snapshot
            subject.publish(Set("k"))
            assert(secondCalls == 1) // gone on the next emission
        }
    }
end ChangedKeysSubjectSpec
