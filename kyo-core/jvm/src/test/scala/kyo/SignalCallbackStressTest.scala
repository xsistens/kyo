package kyo

import AllowUnsafe.embrace.danger
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

/** JVM-only stress test for the callback observation path (`Signal.unsafeObserveProjected`).
  *
  * The single-threaded suite cannot reach the race it pins: a pump that registers on a promise another thread has just completed runs the
  * callback inline, and the two arms that follow (the nested one and the outer one) race a writer's re-arm on the same slot. A stale,
  * completed promise left in that slot makes every later re-arm drop its own registration, so the signal goes deaf while it still holds
  * subscribers.
  */
class SignalCallbackStressTest extends kyo.test.Test[Any]:

    private val rounds = 400
    private val writes = 2000
    private val churns = 500

    private def runAll(bodies: (() => Unit)*): Unit =
        val start   = new CountDownLatch(1)
        val threads = bodies.map(b =>
            new Thread(() =>
                start.await()
                b()
            )
        )
        threads.foreach(_.start())
        start.countDown()
        threads.foreach(_.join())
    end runAll

    "concurrent writers and subscribers leave the signal armed and every subscriber current" in {
        val failures = (1 to rounds).flatMap { round =>
            val r    = Signal.SignalRef.Unsafe.init(0)
            val last = new AtomicReference[Int](-1)
            discard(r.safe.unsafeObserveProjected[Int](identity, Absent, v => last.set(v)))
            runAll(
                () => for i <- 1 to writes do r.set(i * 2),
                () => for i <- 1 to writes do r.set(i * 2 + 1),
                () =>
                    for _ <- 1 to churns do
                        r.safe.unsafeObserveProjected[Int](identity, Absent, _ => ()).foreach(_())
            )
            val settled = last.get() == r.get()
            r.set(-2)
            val delivered = last.get() == -2
            val armed     = r.waiters() == 1
            if settled && delivered && armed then None
            else Some(s"round $round: settled=$settled delivered=$delivered waiters=${r.waiters()}")
        }
        assert(failures.isEmpty, failures.take(5).mkString("\n"))
    }

    "a delivery that overtakes a slower one on another thread is not undone by it" in {
        val seen    = new AtomicReference[Int](0)
        val entered = new CountDownLatch(1)
        val gate    = new CountDownLatch(1)
        val sub     = new Signal.Sub[Int, Int](
            identity,
            v =>
                if v == 1 then
                    entered.countDown()
                    gate.await()
                seen.set(v)
            ,
            Absent
        )
        val slow = new Thread(() => sub.deliver(1))
        slow.start()
        entered.await()
        val fast = new Thread(() => sub.deliver(2))
        fast.start()
        fast.join()
        gate.countDown()
        slow.join()
        // The pump delivers the current value again once it sees the version moved; it must not be taken for a duplicate.
        sub.deliver(2)
        assert(seen.get() == 2)
    }

end SignalCallbackStressTest
