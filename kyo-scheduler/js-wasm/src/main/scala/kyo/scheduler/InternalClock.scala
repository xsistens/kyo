package kyo.scheduler

import java.util.concurrent.Executor
import scala.annotation.nowarn

/** The scheduler's clock on JS: monotonic milliseconds from an arbitrary origin, read on every call.
  *
  * A duration source, never a calendar time. The kernel's safepoint judges the slice deadlines built from these readings against
  * `System.nanoTime` in milliseconds, so the two must stay on the same basis.
  *
  * The jvm-native clock caches a timestamp that a thread publishes every millisecond. JS has no thread to keep a cache current: a cache
  * refreshed on reads grows stale for as long as nobody reads it, and a deadline derived from a stale reading has already passed when the
  * slice it bounds begins. A read of the clock is cheap enough on JS for the scheduler to take one per task.
  */
@nowarn
final class InternalClock(executor: Executor = null) {

    def currentMillis(): Long = InternalClock.monotonicMillis()

    def stop(): Unit = {}

}

object InternalClock {
    private[kyo] def monotonicMillis(): Long = System.nanoTime() / 1000000L
}
