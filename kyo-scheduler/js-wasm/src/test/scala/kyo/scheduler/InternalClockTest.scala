package kyo.scheduler

import org.scalatest.NonImplicitAssertions
import org.scalatest.freespec.AnyFreeSpec

class InternalClockTest extends AnyFreeSpec with NonImplicitAssertions {

    private def nextMillis(): Long = {
        val from = System.currentTimeMillis()
        var now  = from
        while (now == from) now = System.currentTimeMillis()
        now
    }

    "a read is never behind the system clock read before it" in {
        val clock = new InternalClock()
        val lags  = (1 to 256).flatMap { _ =>
            val system = nextMillis()
            val read   = clock.currentMillis()
            if (read < system) Some(system - read) else None
        }
        assert(lags.size == 0, s"of 256 reads, ${lags.size} lagged the system clock, by up to ${lags.maxOption.getOrElse(0L)} ms")
    }
}
