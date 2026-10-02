package kyo.scheduler

import org.scalatest.NonImplicitAssertions
import org.scalatest.freespec.AnyFreeSpec

class InternalClockTest extends AnyFreeSpec with NonImplicitAssertions {

    private def nextMillis(): Long = {
        val from = InternalClock.monotonicMillis()
        var now  = from
        while (now == from) now = InternalClock.monotonicMillis()
        now
    }

    "a read is never behind the monotonic clock read before it" in {
        val clock = new InternalClock()
        val lags  = (1 to 256).flatMap { _ =>
            val monotonic = nextMillis()
            val read      = clock.currentMillis()
            if (read < monotonic) Some(monotonic - read) else None
        }
        assert(lags.size == 0, s"of 256 reads, ${lags.size} lagged the monotonic clock, by up to ${lags.maxOption.getOrElse(0L)} ms")
    }
}
