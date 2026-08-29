package kyo.uic

import kyo.*

/** RowSource's behaviour: which blocks it fetches, which it reuses, and what it publishes.
  *
  * Nothing here waits on a clock. Every barrier is a value the source itself published,
  * taken off a channel fed by an observer, so a test proceeds exactly when the thing it
  * is waiting for has happened.
  *
  * Absence is asserted the same way. "Block 5 was never fetched" cannot be shown by
  * looking at a snapshot, so those tests change the QUERY and wait for its first fetch:
  * every block of the old query is settled by then, and the recorded calls carry the
  * query they were made for.
  */
class RowSourceTest extends UicTest:

    final case class Row(id: Int) derives CanEqual

    private val all = (0 until 10).map(Row(_))

    /** One source under test, with everything it published as a stream. */
    final private case class Probe(
        source: RowSource[String, Row],
        query: SignalRef[String],
        rows: Channel[Seq[Row]],
        loading: Channel[Boolean],
        calls: Channel[List[(String, Int)]]
    )

    /** Every value a signal publishes, as a channel. The observer delivers the current
      * value first, so the first take is the state at subscription.
      */
    private def watch[A](sig: Signal[A])(using Frame, CanEqual[A, A]): Channel[A] < (Async & Scope) =
        for
            ch <- Channel.init[A](64)
            _  <- Fiber.init(sig.observe(v => ch.put(v)))
        yield ch

    /** Takes until a value satisfies `p`, which is what makes a step deterministic. */
    private def until[A](ch: Channel[A])(p: A => Boolean)(using Frame): A < (Async & Abort[Closed]) =
        ch.take.map(v => if p(v) then v else until(ch)(p))

    private def probe(
        pageSize: Int = 2,
        config: RowSource.Config = RowSource.Config(),
        rows: String => Seq[Row] = _ => all,
        total: Maybe[Total] = Absent,
        gate: Maybe[(Int, Latch)] = Absent
    )(using Frame): Probe < (Async & Scope) =
        for
            q     <- Signal.initRef("a")
            calls <- Signal.initRef(List.empty[(String, Int)])
            src <- RowSource.init(q, pageSize, config) { (query, offset, limit) =>
                val data  = rows(query)
                val block = offset / limit
                for
                    _ <- calls.getAndUpdate(_ :+ (query, block))
                    _ <- (gate match
                        case Present((b, latch)) if b == block => latch.await
                        case _                                 => ()
                    ): Unit < Async
                yield (data.slice(offset, offset + limit), total.getOrElse(Total.Known(data.size)))
                end for
            }
            rowsCh <- watch(src.rows)
            loadCh <- watch(src.loading)
            callCh <- watch(calls)
        yield Probe(src, q, rowsCh, loadCh, callCh)

    /** The blocks fetched for `q`, in the order the fetches recorded themselves. */
    private def blocksOf(calls: List[(String, Int)], q: String): List[Int] =
        calls.collect { case (`q`, b) => b }

    "a Known count speaks for every block of a query, an Unknown one only for the last" in {
        assert(Total.combine(Seq(Total.Unknown(true), Total.Known(9))) == Total.Known(9))
        assert(Total.combine(Seq(Total.Known(9), Total.Unknown(true))) == Total.Known(9))
        assert(Total.combine(Seq(Total.Unknown(true), Total.Unknown(false))) == Total.Unknown(false))
        assert(Total.combine(Nil) == Total.Unknown(false), "nothing said is nothing known")
    }

    "a source serves the page it was asked for and reaches one block past it" in {
        for
            p    <- probe()
            rows <- until(p.rows)(_.nonEmpty)
            c    <- until(p.calls)(_.size == 2)
        yield
            assert(rows == all.take(2), "the first page")
            assert(blocksOf(c, "a") == List(0, 1) || blocksOf(c, "a") == List(1, 0), "and the block behind it")
    }

    // The whole point of the buffer: the second page is already there, so nothing is
    // fetched for it and the reader is never shown a mask.
    //
    // "Already there" means RESOLVED, not merely started, and a recorded call proves only
    // the second: the probe records itself at the top of the fetch. Waiting on the calls
    // alone let the reader arrive while block 1 was still in flight, where the mask goes up
    // by design (the test below pins that), and the assertion failed once in a few dozen
    // runs. The visit-and-return is what makes the block resolved rather than likely to be:
    // serving page 1 the first time AWAITS block 1, so on the second visit it can only be
    // in the buffer.
    "a page already in the buffer costs no fetch and raises no mask" in {
        for
            p    <- probe()
            _    <- until(p.rows)(_ == all.take(2))
            _    <- p.source.page.set(1)
            _    <- until(p.rows)(_ == all.slice(2, 4))
            _    <- p.source.page.set(0)
            _    <- until(p.rows)(_ == all.take(2))
            _    <- until(p.loading)(_ == false)
            _    <- p.loading.drain
            _    <- p.source.page.set(1)
            rows <- until(p.rows)(_ == all.slice(2, 4))
            c    <- until(p.calls)(c => blocksOf(c, "a").toSet == Set(0, 1, 2))
            busy <- p.loading.drain
        yield
            assert(rows == all.slice(2, 4), "the page came out of the buffer")
            assert(blocksOf(c, "a").count(_ == 1) == 1, "and block 1 was never fetched twice")
            assert(busy.isEmpty, "the mask never moved for a page that was already there")
    }

    // The other side of the same rule, and the interleaving the test above used to hit by
    // accident: a prefetch that has started but not landed is NOT a buffered page, so the
    // reader who arrives on it waits and is told so.
    "a page the reader reaches before its prefetch landed does show the mask" in {
        for
            latch <- Latch.init(1)
            p     <- probe(gate = Present((1, latch)))
            _     <- until(p.rows)(_ == all.take(2))
            _     <- until(p.loading)(_ == false)
            _     <- until(p.calls)(c => blocksOf(c, "a").toSet == Set(0, 1))
            _     <- p.source.page.set(1)
            _     <- until(p.loading)(_ == true)
            _     <- latch.release
            rows  <- until(p.rows)(_ == all.slice(2, 4))
            _     <- until(p.loading)(_ == false)
        yield assert(rows == all.slice(2, 4), "and the rows arrive when the fetch lands")
    }

    // The regression guard for the rule the class doc explains: observe closes its
    // per-value scope the moment the demand changes, so a fetch forked into that scope
    // would die exactly when the reader arrives on the page it was fetching.
    "a fetch the reader moved onto is not interrupted by the move" in {
        for
            latch <- Latch.init(1)
            p     <- probe(gate = Present((1, latch)))
            _     <- until(p.calls)(c => blocksOf(c, "a").toSet == Set(0, 1))
            _     <- p.source.page.set(1)
            _     <- latch.release
            rows  <- until(p.rows)(_ == all.slice(2, 4))
            c     <- until(p.calls)(c => blocksOf(c, "a").contains(2))
        yield
            assert(rows == all.slice(2, 4), "the blocked fetch finished and served the reader")
            assert(blocksOf(c, "a").count(_ == 1) == 1, "nothing restarted it")
    }

    "a new query is a new set of blocks" in {
        for
            p    <- probe(rows = q => if q == "a" then all else all.reverse)
            _    <- until(p.rows)(_ == all.take(2))
            _    <- p.query.set("b")
            rows <- until(p.rows)(_ == all.reverse.take(2))
            c    <- until(p.calls)(c => blocksOf(c, "b").nonEmpty)
        yield
            assert(rows == all.reverse.take(2), "the new query answered")
            assert(blocksOf(c, "b").contains(0), "and block 0 was fetched again under it")
    }

    "an unknown total that says nothing follows is not reached past" in {
        for
            p <- probe(total = Present(Total.Unknown(false)))
            _ <- until(p.rows)(_.nonEmpty)
            _ <- p.query.set("b")
            c <- until(p.calls)(c => blocksOf(c, "b").nonEmpty)
        yield assert(blocksOf(c, "a") == List(0), "one page asked for, one block fetched")
    }

    "a known total stops the reach at the last block" in {
        for
            p <- probe()
            _ <- until(p.calls)(c => blocksOf(c, "a").toSet == Set(0, 1))
            _ <- p.source.page.set(4)
            _ <- until(p.rows)(_ == all.slice(8, 10))
            _ <- p.query.set("b")
            c <- until(p.calls)(c => blocksOf(c, "b").nonEmpty)
        yield
            assert(blocksOf(c, "a").toSet == Set(0, 1, 3, 4), "the block before it, and nothing past the end")
            assert(!blocksOf(c, "a").contains(5), "there is no block 5 for ten rows of two")
    }

    "a block larger than a page serves several pages from one fetch" in {
        for
            p    <- probe(config = RowSource.Config(blockSize = Present(4)))
            _    <- until(p.rows)(_ == all.take(2))
            _    <- p.source.page.set(1)
            rows <- until(p.rows)(_ == all.slice(2, 4))
            _    <- p.query.set("b")
            c    <- until(p.calls)(c => blocksOf(c, "b").nonEmpty)
        yield
            assert(rows == all.slice(2, 4), "the second page came out of the first block")
            assert(blocksOf(c, "a").count(_ == 0) == 1, "which was fetched once")
    }

    // What a viewport does: ask for a range that starts inside one block and ends inside
    // another. The rows are stitched out of the blocks that cover it.
    "a range across two block boundaries is stitched out of the blocks under it" in {
        for
            p    <- probe()
            _    <- until(p.rows)(_.nonEmpty)
            _    <- p.source.demand.set(RowSource.Demand(1, 4))
            rows <- until(p.rows)(_.size == 4)
        yield assert(rows == all.slice(1, 5), "rows 1 to 4, out of blocks 0, 1 and 2")
    }

end RowSourceTest
