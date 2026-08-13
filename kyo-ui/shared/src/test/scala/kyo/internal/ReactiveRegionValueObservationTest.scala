package kyo.internal

import kyo.*
import kyo.UI.render

/** A region over a PROJECTION of a signal, while the signal keeps moving underneath it.
  *
  * `Signal.map` composes projections down to the root, so a region observed in UI space compares the root's values against an image that
  * is never `==` — a rendered tree carries freshly allocated handlers. A record that ticks once a second would then repaint every region
  * drawing any slice of it, however still that slice is. The region carries its pre-projection signal instead
  * ([[kyo.UI.Ast.Reactive.Source]]) and is observed in VALUE space, where the slice is what gets compared.
  *
  * The second region is the witness: it draws a field that moves on every emission, so its paint proves the emission reached the region
  * layer, and the next one is only set afterwards — nothing is coalesced away, and "it never saw them" cannot masquerade as "it ignored
  * them". Every wait is a paint count, never the wall clock.
  */
class ReactiveRegionValueObservationTest extends kyo.test.Test[Any]:

    /** A record with a slice that moves on every emission and one that does not. */
    private case class Playback(progressMs: Int, volume: Int) derives CanEqual

    /** Counts paints and opens latches at chosen counts. */
    final private class Paints(count: AtomicInt, waiters: AtomicRef[Chunk[(Int, Latch)]]):
        val exchange = new UIExchange:
            def onChange(path: Seq[String], changed: UI, mount: Boolean)(using Frame): Unit < Async =
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

    private def mount(region: UI, paints: Paints)(using Frame): Unit < (Async & Scope & Abort[Any]) =
        ReactiveUI.normalize(UI.div(region), Seq.empty).map(root => ReactiveUI.subscribe(root, paints.exchange).unit)

    "a region over a projection ignores source changes that leave the projection alone" in {
        Scope.run {
            for
                slice   <- initPaints
                witness <- initPaints
                state   <- Signal.initRef(Playback(0, 65))
                _       <- mount(state.map(_.volume).render(v => UI.button(v.toString).onClick(Kyo.unit)), slice)
                _       <- mount(state.render(st => UI.button(st.progressMs.toString).onClick(Kyo.unit)), witness)
                _       <- slice.await(1)
                _       <- witness.await(1)
                // Five emissions that move the record but not the volume, each waited on through the witness.
                _     <- Kyo.foreachDiscard(1 to 5)(i => state.set(Playback(i, 65)).andThen(witness.await(1 + i)))
                quiet <- slice.get
                _     <- state.set(Playback(6, 70))
                _     <- slice.await(2)
                after <- slice.get
                seen  <- witness.get
            yield assert(
                after == 2 && seen >= 6,
                s"the volume region painted $after times in total, expected 2 (one mount, one change); it stood at $quiet before the " +
                    s"change, expected 1. The witness painted $seen times, expected at least 6 — below that the emissions never " +
                    s"reached the regions and this proves nothing."
            )
            end for
        }
    }

    "a region over the whole record repaints when the record changes" in {
        Scope.run {
            for
                paints <- initPaints
                state  <- Signal.initRef(Playback(0, 65))
                _      <- mount(state.render(st => UI.button(s"${st.progressMs}/${st.volume}").onClick(Kyo.unit)), paints)
                _      <- paints.await(1)
                _      <- state.set(Playback(1, 70))
                _      <- paints.await(2)
                after  <- paints.get
            yield assert(after == 2, s"the region painted $after times, expected 2 (one mount, one change)")
        }
    }

end ReactiveRegionValueObservationTest
