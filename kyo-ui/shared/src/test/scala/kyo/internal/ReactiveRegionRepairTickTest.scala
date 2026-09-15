package kyo.internal

import kyo.*
import kyo.UI.render

/** A region over a combinator signal, mounted on virtual time, across its repair ticks.
  *
  * A combinator cannot be observed exactly, so the region's loop also wakes once per repair interval and re-reads its source. What it
  * renders is a freshly built UI tree with fresh handlers, never `==` to the last one, so a wakeup that re-rendered an unchanged source
  * would repaint the region once per interval. A tick over unchanged sources paints nothing; a real change still paints once.
  *
  * Every wait is a barrier, the region's re-armed repair timer or a paint count, never the wall clock.
  */
class ReactiveRegionRepairTickTest extends kyo.test.Test[Any]:

    private val ri = Signal.defaultRepairInterval

    /** Counts paints and opens latches at chosen counts. */
    final private class Paints(count: AtomicInt, waiters: AtomicRef[Chunk[(Int, Latch)]]):
        val exchange = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async =
                count.incrementAndGet.map { n =>
                    waiters.get.map(ws => Kyo.foreachDiscard(ws)(w => if n >= w._1 then w._2.release else Kyo.unit))
                }

        def get(using Frame): Int < Sync = count.get

        /** Waits until paint `n` has happened. */
        def await(n: Int)(using Frame): Unit < Async =
            Latch.init(1).map { latch =>
                waiters.updateAndGet(_.append((n, latch)))
                    .andThen(count.get.map(c => if c >= n then latch.release else Kyo.unit))
                    .andThen(latch.await)
            }
    end Paints

    private def initPaints(using Frame): Paints < Sync =
        AtomicInt.init.map(count => AtomicRef.init(Chunk.empty[(Int, Latch)]).map(new Paints(count, _)))

    /** Mounts `region` on virtual time and returns its paints across five repair ticks over unchanged sources, and across `change` plus one
      * tick after it.
      */
    private def paintsAcrossTicks(region: UI, change: Unit < Sync)(using Frame): (Int, Int) < (Async & Abort[Any]) =
        Clock.withTimeControl { tc =>
            // Fires the repair timer, then waits until the loop has handled the wakeup and armed the next one.
            val tick = tc.advance(ri).andThen(tc.awaitPendingSleepers(1))
            Scope.run {
                for
                    paints <- initPaints
                    root   <- ReactiveUI.normalize(UI.div(region), Seq.empty)
                    _      <- ReactiveUI.subscribe(root, paints.exchange)
                    // The loop has painted the first value and armed its first repair timer.
                    _      <- tc.awaitPendingSleepers(1)
                    idle   <- paints.get
                    _      <- Kyo.foreachDiscard(1 to 5)(_ => tick)
                    ticked <- paints.get
                    _      <- change
                    _      <- paints.await(ticked + 1)
                    _      <- tc.awaitPendingSleepers(1)
                    _      <- tick
                    after  <- paints.get
                yield (ticked - idle, after - ticked)
            }
        }

    "a region over a combinator does not repaint on repair ticks while its sources hold" in {
        for
            a <- Signal.initRef(0)
            b <- Signal.initRef(0)
            counts <- paintsAcrossTicks(
                a.combineLatest(b).render((x, y) => UI.button(s"$x/$y").onClick(Kyo.unit)),
                a.set(1)
            )
        yield
            val (ticks, change) = counts
            assert(
                ticks == 0 && change == 1,
                s"5 repair ticks painted $ticks times, expected 0; a change of a and one tick after it painted $change times, expected 1"
            )
        end for
    }

    "a region over a map of a combinator does not repaint on repair ticks while its sources hold" in {
        for
            busy    <- Signal.initRef(false)
            invalid <- Signal.initRef(false)
            counts <- paintsAcrossTicks(
                busy.combineLatest(invalid).map(_ || _).render(off => UI.button(if off then "wait" else "go").onClick(Kyo.unit)),
                busy.set(true)
            )
        yield
            val (ticks, change) = counts
            assert(
                ticks == 0 && change == 1,
                s"5 repair ticks painted $ticks times, expected 0; a change of busy and one tick after it painted $change times, expected 1"
            )
        end for
    }

end ReactiveRegionRepairTickTest
