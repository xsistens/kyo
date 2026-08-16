package kyo.scheduler

import org.scalajs.macrotaskexecutor.MacrotaskExecutor
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

/** The two properties the run queue introduces, and that the one-task-per-macrotask form gave away for free.
  */
class SchedulerDrainTest extends AsyncFreeSpec {

    implicit override def executionContext: scala.concurrent.ExecutionContext = MacrotaskExecutor

    private def task(body: () => Unit): Task =
        new Task {
            def run(startMillis: Long, clock: InternalClock, deadline: Long): Task.Result = {
                body()
                Task.Done
            }
        }

    private def post(body: () => Unit): Unit =
        MacrotaskExecutor.execute(new Runnable { def run(): Unit = body() })

    "a throwing task leaves the rest of the queue running" in {
        // Before the queue, every task owned its macrotask, so a throw could only take that task down.
        // Draining them in one loop means an uncaught throw would exit the loop with work still queued and
        // `pumpScheduled` stuck true -- the scheduler would stop for good, silently.
        val scheduler = new Scheduler
        val ran       = Promise[Boolean]()
        scheduler.schedule(task(() => throw new RuntimeException("deliberate") with NoStackTrace))
        scheduler.schedule(task(() => ran.success(true)))
        ran.future.map(v => assert(v))
    }

    "a task scheduled from inside a running task runs in the same turn" in {
        // The whole point: interrupt -> task -> Scope.close used to be three macrotasks because each link
        // scheduled the next. It collapses only if a schedule from inside the pump joins the drain in progress.
        //
        // The sentinel is posted BEFORE the inner schedule, so the ordering discriminates: draining, the inner
        // task runs within this turn and sees the sentinel pending; one-task-per-macrotask, the sentinel's turn
        // was queued first and has already run by the time the inner task gets its own.
        val scheduler        = new Scheduler
        var sentinelRan      = false
        val innerSawSentinel = Promise[Boolean]()
        scheduler.schedule(task { () =>
            post(() => sentinelRan = true)
            scheduler.schedule(task(() => innerSawSentinel.success(sentinelRan)))
        })
        innerSawSentinel.future.map(seen => assert(!seen))
    }
}
