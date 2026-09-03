package kyo

import kyo.UI.render
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** Whether the keyed-rebuild hint actually REACHES the engine, not just whether its counter counts.
  *
  * `MountKeyedMissTest` pins the streak in isolation. What it cannot see is the wiring: that a keyed
  * mount which really is rebuilt on every pass routes through the claim's miss branch with a STABLE
  * path, so the streak accumulates instead of restarting. Measured in the spotify example on
  * 2026-09-04, a keyed mount two regions deep was rebuilt ten times over and the console stayed
  * empty — either the hint does not fire where it was built to fire, or the app's case is not a miss
  * at all. This suite is the difference between those two readings.
  *
  * The shape is the app's: an outer region (the page's query state) whose re-render rebuilds an
  * inner region (a deferred section), with the keyed mount inside the inner one. One region deep is
  * already covered by `MountedTest` and survives; two is where continuity ends.
  */
class MountKeyedMissWiringTest extends kyo.test.Test[Any]:

    /** Collects what a run logged, so a warning can be asserted on rather than eyeballed. */
    final private class Capture extends Log.Unsafe:
        private val buf = scala.collection.mutable.ArrayBuffer.empty[String]

        private def add(msg: String): Unit =
            buf.synchronized { buf += msg; () }

        def lines: Seq[String] = buf.synchronized(buf.toSeq)

        def level: Log.Level                   = Log.Level.trace
        def name: String                       = "capture"
        def withName(name: String): Log.Unsafe = this

        def trace(msg: => String)(using Frame, AllowUnsafe): Unit                  = add(msg)
        def trace(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = add(msg)
        def debug(msg: => String)(using Frame, AllowUnsafe): Unit                  = add(msg)
        def debug(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = add(msg)
        def info(msg: => String)(using Frame, AllowUnsafe): Unit                   = add(msg)
        def info(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = add(msg)
        def warn(msg: => String)(using Frame, AllowUnsafe): Unit                   = add(msg)
        def warn(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = add(msg)
        def error(msg: => String)(using Frame, AllowUnsafe): Unit                  = add(msg)
        def error(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = add(msg)
    end Capture

    /** The engine needs a sink; nothing here asserts on the painted HTML. */
    private val quiet: UIExchange =
        new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async = ()

    private def drive(passes: Int)(using Frame): (Int, Seq[String]) < (Async & Scope) =
        val cap = new Capture
        Log.let(cap.safe) {
            for
                outer <- Signal.initRef(0)
                runs  <- AtomicInt.init(0)
                ui = UI.div(
                    outer.render(_ =>
                        UI.div(
                            outer.render(_ =>
                                UI.mounted(runs.incrementAndGet.map(r => UI.span(s"runs:$r")))
                                    .keyed("box")
                            )
                        )
                    )
                )
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, quiet)
                _    <- Async.sleep(50.millis)
                _ <- Kyo.foreachDiscard(1 to passes) { _ =>
                    outer.getAndUpdate(_ + 1).andThen(Async.sleep(30.millis))
                }
                _ <- Async.sleep(300.millis)
                n <- runs.get
            yield (n, cap.lines)
        }
    end drive

    "a keyed mount two regions deep really is rebuilt on every outer pass" in {
        Scope.run(drive(12).map { (runs, _) =>
            assert(runs > 1, s"expected rebuilds, the mount effect ran $runs time(s)")
        })
    }

    "and the rebuild is reported, or the hint is decoration" in {
        Scope.run(drive(12).map { (runs, lines) =>
            // The threshold is 10 consecutive misses at one path; twelve passes clear it with room.
            assert(
                lines.exists(_.contains("was re-created")),
                s"mount effect ran $runs time(s) and nothing was logged; captured: ${lines.mkString(" | ")}"
            )
        })
    }

end MountKeyedMissWiringTest
