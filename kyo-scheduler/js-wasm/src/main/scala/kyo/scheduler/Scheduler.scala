package kyo.scheduler

import org.scalajs.macrotaskexecutor.MacrotaskExecutor
import scala.collection.mutable.ArrayDeque
import scala.concurrent.ExecutionContext

object Scheduler {
    val get = new Scheduler
}

class Scheduler {

    private val timeSlice     = timeSliceMs()
    private val drainBudgetNs = drainBudgetMs().toLong * 1000000L
    private val clock         = new InternalClock()

    // A run queue, drained by at most one scheduled pump. Without it every task cost its own macrotask: a
    // postMessage round trip per wake, per continuation and per re-arm. Tearing down a thousand fibers was
    // ~2000 turns whose own overhead dwarfed the work inside them.
    //
    // No atomics: JS runs this on one thread, the same reason UnboundedUnsafeQueueBase is a plain ArrayDeque
    // here. A schedule from inside a running task therefore lands in the queue the current pump is still
    // draining, which is the point -- the interrupt -> task -> Scope.close chain collapses into one turn.
    private val queue         = new ArrayDeque[Task](16)
    private var pumpScheduled = false

    def schedule(t: Task): Unit = {
        queue += t
        if (!pumpScheduled) {
            pumpScheduled = true
            MacrotaskExecutor.execute(pump)
        }
    }

    // The jvm-native scheduler schedules onto a worker OTHER than the caller's so the task is not picked up
    // re-entrantly on the current worker. On JS there is no other worker to pick, and none is needed: `schedule`
    // only appends: even called from inside the pump it never runs the task on the caller's stack, it hands it
    // to the drain that is already in progress. So `scheduleExcludingCurrent` is `schedule`.
    def scheduleExcludingCurrent(t: Task): Unit = schedule(t)

    /** Drains the queue until the wall-clock budget runs out, then re-posts itself if work remains.
      *
      * The budget is the only thing bounding this loop, and it is wall-clock on purpose: a bound on the NUMBER
      * of tasks would not survive CPU throttling -- the same count takes 4x as long under a 4x slowdown and the
      * turn grows with it -- while a duration is invariant under it.
      *
      * Blocking is not worse than the form this replaces. Each task's deadline is capped at the budget that
      * remains, and the budget is clamped to timeSlice, so the longest a turn can run is the budget plus the
      * overrun of one non-preemptible task: the same expression as the single-task form, with a smaller first
      * term.
      *
      * Timed with System.nanoTime rather than the InternalClock, which only refreshes on some calls and reports
      * milliseconds -- both too coarse to bound a few-millisecond budget.
      */
    private val pump: Runnable = new Runnable {
        def run(): Unit = {
            val budgetEnd = System.nanoTime() + drainBudgetNs
            try {
                var draining = true
                while (draining) {
                    val remainingNs = budgetEnd - System.nanoTime()
                    if (queue.isEmpty || remainingNs <= 0)
                        draining = false
                    else {
                        val t     = queue.removeHead()
                        val now   = clock.currentMillis()
                        val slice = Math.max(1L, Math.min(timeSlice.toLong, remainingNs / 1000000L))
                        try
                            if (t.run(now, clock, now + slice) == Task.Preempted)
                                queue += t
                        catch {
                            // Without this the throw would leave the loop, strand pumpScheduled and stall a
                            // non-empty queue for good. Before the queue existed each task owned its macrotask,
                            // so a throw could only take that one task down and the rest still ran; keep that.
                            case ex: Throwable => ExecutionContext.defaultReporter(ex)
                        }
                    }
                }
            } finally {
                // In finally for the same reason as the catch above: nothing that escapes the loop may leave
                // the queue unattended.
                pumpScheduled = false
                if (queue.nonEmpty) {
                    pumpScheduled = true
                    MacrotaskExecutor.execute(pump)
                }
            }
        }
    }

    def asExecutionContext: ExecutionContext = MacrotaskExecutor

    def flush(): Unit = {}

    def reject(): Boolean = false

    def reject(key: String): Boolean = false

    def reject(key: Int): Boolean = false

    def notifyInterrupt(): Unit = {}

}
