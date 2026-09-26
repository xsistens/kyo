package kyo.scheduler

import java.util.concurrent.Executor
import scala.annotation.nowarn

/** The scheduler's clock on JS: every read is a read of the system clock.
  *
  * The jvm-native clock caches a timestamp that a thread publishes every millisecond. JS has no thread to keep a cache current: a cache
  * refreshed on reads grows stale for as long as nobody reads it, and a deadline derived from a stale reading has already passed when the
  * slice it bounds begins. A read of the system clock is cheap enough on JS for the scheduler to take one per task.
  */
@nowarn
final class InternalClock(executor: Executor = null) {

    def currentMillis(): Long = System.currentTimeMillis()

    def stop(): Unit = {}

}
