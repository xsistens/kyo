package kyo.scheduler

import org.scalatest.freespec.AnyFreeSpec

class InternalClockTest extends AnyFreeSpec {

    "caches between samples rather than reading the system clock on every call" in {
        // The whole point of the cache: real time may move without the reading following it, as long as
        // fewer than the sampling interval of reads have happened. `& sampleEvery` instead of
        // `& (sampleEvery - 1)` samples on half of all reads, and this assertion is what catches it —
        // the reading would track real time and no longer equal the first one.
        val clock    = new InternalClock()
        val first    = clock.currentMillis()
        val sysStart = System.currentTimeMillis()
        var reads    = 0
        while (System.currentTimeMillis() - sysStart < 5)
            if (reads < 100) {
                val _ = clock.currentMillis()
                reads += 1
            }
        assert(System.currentTimeMillis() - sysStart >= 5) // real time moved
        assert(clock.currentMillis() == first)             // the reading did not
    }

    "picks real time back up once the interval is crossed" in {
        // The other half: caching forever would be a broken clock, not a cheap one.
        val clock    = new InternalClock()
        val first    = clock.currentMillis()
        val sysStart = System.currentTimeMillis()
        while (System.currentTimeMillis() - sysStart < 5) ()
        while (clock.steps < 300) { val _ = clock.currentMillis() }
        assert(clock.currentMillis() > first)
    }
}
