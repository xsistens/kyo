package kyo.scheduler

import java.util.concurrent.Executor
import scala.annotation.nowarn

/** Low-resolution clock optimized for frequent access in the scheduler.
  *
  * Same purpose as the jvm-native clock — spare the scheduler a system call on every time check — reached differently: there is no thread to
  * publish a timestamp from, so the sample is taken on every `sampleEvery`-th read and cached in between.
  *
  * The reading can therefore lag real time by however long the intervening reads take, which is the same trade the jvm-native clock makes
  * with its one-millisecond publish interval, and acceptable for what the scheduler measures with it: task runtime and slice deadlines.
  */
@nowarn
final class InternalClock(executor: Executor = null) {

    var steps = 0
    var curr  = System.currentTimeMillis()

    def currentMillis(): Long = {
        steps += 1
        // `& (sampleEvery - 1)`, the mask that makes this "every sampleEvery-th call". `& sampleEvery` isolates
        // the bit instead, which is zero for a RUN of sampleEvery calls and set for the next run: it sampled on
        // half of all reads, in bursts, and so paid the system call this cache exists to avoid.
        if ((steps & (InternalClock.sampleEvery - 1)) == 0)
            curr = System.currentTimeMillis()
        curr
    }

    def stop(): Unit = {}

}

object InternalClock {

    /** Reads between samples. Power of two: `currentMillis` masks against it. */
    private val sampleEvery = 128

}
