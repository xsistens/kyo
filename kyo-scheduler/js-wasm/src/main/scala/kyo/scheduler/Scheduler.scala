package kyo.scheduler

import org.scalajs.macrotaskexecutor.MacrotaskExecutor
import scala.collection.mutable.ArrayDeque
import scala.concurrent.ExecutionContext
import scala.scalajs.js

object Scheduler {
    val get = new Scheduler
}

/** Single-threaded scheduler that drains a run queue in Node macrotasks.
  *
  * With `statusFile` set (the `topStatusFile` flag the JVM and Native schedulers also write), a timer rewrites a one-line snapshot there
  * every `statusFileMs`. The timer shares the event loop with every task, so a slice that never returns stops the rewrites; ci-monitor
  * reports the snapshot's age, which is how a blocked event loop shows from outside the process. The counters are kept only while a status
  * file is configured, so the default path does no extra work per slice.
  */
class Scheduler(statusFile: String, statusFileMs: Int, nowMs: () => Long, sliceMs: () => Long) {

    // Two sources: `ts` is a calendar time ci-monitor compares with its own clock to age the snapshot, while a slice is a duration and
    // must not move with a wall-clock step.
    def this() = this(topStatusFile(), topStatusFileMs(), () => System.currentTimeMillis(), () => InternalClock.monotonicMillis())

    private val timeSlice   = timeSliceMs()
    private val drainBudget = drainBudgetMs()
    private val clock       = new InternalClock()

    val writesStatus: Boolean = statusFile.nonEmpty

    private var scheduled  = 0L
    private var ran        = 0L
    private var preempted  = 0L
    private var maxSliceMs = 0L

    private val statusTimer: js.UndefOr[js.timers.SetIntervalHandle] =
        if (writesStatus) StatusFile.start(statusFile, if (statusFileMs > 0) statusFileMs else 1000, () => statusLine())
        else js.undefined

    /** Tasks waiting to run, drained by at most one scheduled [[pump]], so a task does not cost a macrotask of its own.
      *
      * No atomics: JS runs this on one thread. A schedule from inside a running task lands in the queue the current pump is still
      * draining, so a chain of tasks that schedule each other (an interrupt waking a fiber that closes a scope) runs in one turn.
      */
    private val queue         = new ArrayDeque[Task](16)
    private var pumpScheduled = false

    def schedule(t: Task): Unit = {
        if (writesStatus) scheduled += 1
        queue += t
        if (!pumpScheduled) {
            pumpScheduled = true
            MacrotaskExecutor.execute(pump)
        }
    }

    // The jvm-native scheduler schedules onto a worker OTHER than the caller's so the task is not picked up
    // re-entrantly on the current worker. On JS there is no other worker, and none is needed: `schedule` only appends,
    // so even from inside the pump it never runs the task on the caller's stack. `scheduleExcludingCurrent` is `schedule`.
    def scheduleExcludingCurrent(t: Task): Unit = schedule(t)

    /** Drains the queue, starting tasks until the time budget runs out, then re-posts itself if work remains.
      *
      * The budget is a duration on purpose: a bound on the number of tasks would stretch with a slower CPU, a duration does not.
      *
      * All tasks of one turn share one deadline, the turn's start plus `timeSliceMs`, so a turn runs at most one time slice plus the
      * overrun of one task that does not yield, the same bound as a turn that runs a single task. The budget only decides whether another
      * task starts, and since it is clamped to `timeSliceMs`, every task starts with at least `timeSliceMs - drainBudgetMs` of its slice
      * left.
      *
      * The deadline is read from the same monotonic clock that `kyo.kernel.internal.Safepoint` compares it against, so a task never starts
      * with a deadline that has already passed.
      */
    private val pump: Runnable = new Runnable {
        def run(): Unit = {
            val start     = clock.currentMillis()
            val budgetEnd = start + drainBudget
            val deadline  = start + timeSlice
            try {
                var draining = true
                while (draining) {
                    val now = clock.currentMillis()
                    if (queue.isEmpty || now >= budgetEnd)
                        draining = false
                    else {
                        val t = queue.removeHead()
                        try
                            if (writesStatus) runCounted(t, now, deadline)
                            else if (t.run(now, clock, deadline) == Task.Preempted)
                                queue += t
                        catch {
                            // A throw must take down only its own task, not the loop and the tasks queued behind it.
                            case ex: Throwable => ExecutionContext.defaultReporter(ex)
                        }
                    }
                }
            } finally {
                // Nothing that escapes the loop may leave the queue unattended.
                pumpScheduled = false
                if (queue.nonEmpty) {
                    pumpScheduled = true
                    MacrotaskExecutor.execute(pump)
                }
            }
        }
    }

    // The counters are kept only while a status file is configured, so the default path does no extra work per task.
    private def runCounted(t: Task, now: Long, deadline: Long): Unit = {
        ran += 1
        val start  = sliceMs()
        val result = t.run(now, clock, deadline)
        val took   = sliceMs() - start
        if (took > maxSliceMs) maxSliceMs = took
        if (result == Task.Preempted) {
            preempted += 1
            scheduled += 1
            queue += t
        }
    }

    /** The snapshot ci-monitor appends to its line. Each call resets `maxSliceMs`, so a line reports the longest slice since the previous one. */
    def statusLine(): String = {
        val line =
            s"kyo.sched ts=${nowMs()} platform=js scheduled=$scheduled ran=$ran preempted=$preempted pending=${scheduled - ran} maxSliceMs=$maxSliceMs"
        maxSliceMs = 0
        line
    }

    def asExecutionContext: ExecutionContext = MacrotaskExecutor

    def flush(): Unit = {}

    def reject(): Boolean = false

    def reject(key: String): Boolean = false

    def reject(key: Int): Boolean = false

    def notifyInterrupt(): Unit = {}

    def notifyInterruptReleased(task: Task): Unit = {}

    def shutdown(): Unit =
        statusTimer.foreach(js.timers.clearInterval)

}
