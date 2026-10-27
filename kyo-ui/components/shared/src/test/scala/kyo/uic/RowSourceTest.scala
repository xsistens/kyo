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

    /** One fetch: the query it was for, the block it asked for and the block size it asked in. */
    final case class Call(query: String, block: Int, size: Int) derives CanEqual

    private val all = (0 until 10).map(Row(_))

    /** One source under test, with everything it published as a stream. */
    final private case class Probe(
        source: RowSource[String, Row],
        query: SignalRef[String],
        rows: Channel[Seq[Row]],
        loading: Channel[Boolean],
        calls: Channel[List[Call]]
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
        pageSize: Int | SignalRef[Int] = 2,
        config: RowSource.Config = RowSource.Config(),
        rows: String => Seq[Row] = _ => all,
        total: Maybe[Total] = Absent,
        gate: Maybe[(Int, Latch)] = Absent,
        running: Boolean = true
    )(using Frame): Probe < (Async & Scope) =
        for
            q     <- Signal.initRef("a")
            calls <- Signal.initRef(List.empty[Call])
            src   <- RowSource.init(q, pageSize, config) { (query, offset, limit) =>
                val data  = rows(query)
                val block = offset / limit
                for
                    _ <- calls.getAndUpdate(_ :+ Call(query, block, limit))
                    _ <- (gate match
                        case Present((b, latch)) if b == block => latch.await
                        case _                                 => ()
                    ): Unit < Async
                yield (data.slice(offset, offset + limit), total.getOrElse(Total.Known(data.size)))
                end for
            }
            // What an app does that runs a source without a component: on the test's own scope.
            _      <- (if running then src.run else Kyo.unit): Unit < (Async & Scope)
            rowsCh <- watch(src.rows)
            loadCh <- watch(src.loading)
            callCh <- watch(calls)
        yield Probe(src, q, rowsCh, loadCh, callCh)

    /** The blocks fetched for `q`, in the order the fetches recorded themselves. */
    private def blocksOf(calls: List[Call], q: String): List[Int] =
        calls.collect { case Call(`q`, b, _) => b }

    /** The blocks fetched for `q` in blocks of `size` rows. */
    private def blocksOf(calls: List[Call], q: String, size: Int): List[Int] =
        calls.collect { case Call(`q`, b, `size`) => b }

    "a Known count speaks for every block of a query, an Unknown one only for the last" in {
        assert(Total.combine(Seq(Total.Unknown(true), Total.Known(9))) == Total.Known(9))
        assert(Total.combine(Seq(Total.Known(9), Total.Unknown(true))) == Total.Known(9))
        assert(Total.combine(Seq(Total.Unknown(true), Total.Unknown(false))) == Total.Unknown(false))
        assert(Total.combine(Nil) == Total.Unknown(false), "nothing said is nothing known")
        assert(
            Total.combine(Seq(Total.Unknown(true, 40), Total.Unknown(true, 12))) == Total.Unknown(true, 40),
            "and the floor under the length is the highest any block put there"
        )
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

    // A reader who moves on from a page that is still loading wants the next one, not the one they left. The feed used
    // to finish waiting for the old range before it looked at the new one, so a slow page held up every page after it.
    "a new page is served while the one the reader left is still fetching" in {
        for
            latch <- Latch.init(1)
            p     <- probe(gate = Present((1, latch)))
            _     <- until(p.rows)(_ == all.take(2))
            _     <- until(p.calls)(c => blocksOf(c, "a").toSet == Set(0, 1))
            _     <- p.source.page.set(1)
            _     <- until(p.loading)(_ == true)
            _     <- p.source.page.set(3)
            rows  <- until(p.rows)(_ == all.slice(6, 8))
            _     <- latch.release
        yield assert(rows == all.slice(6, 8), "page 4 arrived with block 1 still held")
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

    // The page observer once ran on a fork, so its first delivery landed some time AFTER the
    // source started. Anything a viewport wrote in between was a range the observer then
    // overwrote with page 0's, putting the reader back at the top of a list they had already
    // scrolled. A table writes exactly there: the scroller reports its position on the first
    // render after the source was built. It is registered inside `run` now and seeded, and this
    // holds it so.
    "a range written as the source starts is not reset to the first page" in {
        // Built here rather than through `probe`, because the write has to be the FIRST thing
        // after `run`: every step in between is another chance for the observer to have
        // delivered already, and then there is no race left to lose. Which of the two wins is
        // down to the scheduler, so the scenario runs many times over: on the defect 49 of 50
        // runs lost the range, and a single run passed often enough to look green.
        def once(using Frame) =
            Scope.run {
                for
                    q   <- Signal.initRef("a")
                    src <- RowSource.init(q, pageSize = 2) { (_, offset, limit) =>
                        (all.slice(offset, offset + limit), Total.Known(all.size))
                    }
                    _    <- src.run
                    _    <- src.demand.set(RowSource.Demand(6, 2))
                    rows <- watch(src.rows)
                    // The source having served a window is the barrier: whatever the observer
                    // was going to write has been written by then, and the range that survived
                    // it is the range the reader is looking at.
                    _ <- until(rows)(_.nonEmpty)
                    d <- src.demand.get
                yield d
            }
        Kyo.foreach(1 to 50)(_ => once).map { seen =>
            val lost = seen.count(_ != RowSource.Demand(6, 2))
            assert(lost == 0, s"$lost of ${seen.size} runs put the reader back on the first page")
        }
    }

    // A viewport sizes its scrollbar off the floor under the length, so the floor falling
    // would shrink the scrollbar the moment the reader scrolled back up.
    //
    // The rows are the barrier for the total here, which holds because the source publishes
    // the total FIRST: a window that has arrived is a total that has already been written.
    "the floor under an unknown length never falls" in {
        for
            p    <- probe(total = Present(Total.Unknown(true)))
            _    <- until(p.rows)(_ == all.take(2))
            _    <- p.source.demand.set(RowSource.Demand(6, 2))
            _    <- until(p.rows)(_ == all.slice(6, 8))
            far  <- p.source.total.currentWith(t => t)
            _    <- p.source.demand.set(RowSource.Demand(0, 2))
            _    <- until(p.rows)(_ == all.take(2))
            back <- p.source.total.currentWith(t => t)
        yield
            assert(far == Total.Unknown(true, 8), "eight rows served is eight rows known to exist")
            assert(back == Total.Unknown(true, 8), "and scrolling back does not unknow them")
    }

    "a block saying nothing follows it makes the length exact" in {
        for
            p <- probe(total = Present(Total.Unknown(false)))
            _ <- until(p.rows)(_ == all.take(2))
            t <- p.source.total.currentWith(t => t)
        yield assert(t == Total.Known(2), "nothing after row 1 is a total of two")
    }

    // The barrier is the PREFETCH of the new query and not its first fetch, because a fetch
    // records itself as it starts and the total is written when it lands. What the source
    // reaches for afterwards is the first thing that happens once the total is out. The
    // rows cannot be the barrier here: both queries answer with the same rows, so the
    // window never changes and never publishes.
    "a new query starts the floor again" in {
        for
            p     <- probe(total = Present(Total.Unknown(true)))
            _     <- until(p.rows)(_ == all.take(2))
            _     <- p.source.demand.set(RowSource.Demand(6, 2))
            _     <- until(p.rows)(_ == all.slice(6, 8))
            _     <- p.source.demand.set(RowSource.Demand(0, 2))
            _     <- until(p.rows)(_ == all.take(2))
            held  <- p.source.total.currentWith(t => t)
            _     <- p.query.set("b")
            _     <- until(p.calls)(c => blocksOf(c, "b").contains(1))
            fresh <- p.source.total.currentWith(t => t)
        yield
            assert(held == Total.Unknown(true, 8), "the old query had been served eight rows")
            assert(fresh == Total.Unknown(true, 2), "the old query's length is not this one's")
    }

    // ---- reloading ----

    /** A running source over rows that can change behind it, the way a server's do. */
    private def overData(
        data: SignalRef[Seq[Row]],
        first: Maybe[Latch] = Absent,
        gate: Maybe[AtomicRef[Maybe[Latch]]] = Absent
    )(using
        Frame
    ): (RowSource[String, Row], Channel[Seq[Row]], SignalRef[Int]) < (Async & Scope) =
        for
            q     <- Signal.initRef("a")
            calls <- Signal.initRef(0)
            src   <- RowSource.init(q, pageSize = 2) { (_, offset, limit) =>
                for
                    n    <- calls.getAndUpdate(_ + 1)
                    rows <- data.get
                    // The very first fetch can be held, so a test can let it land after a reload.
                    _ <- (first match
                        case Present(latch) if n == 0 => latch.await
                        case _                        => ()
                    ): Unit < Async
                    // And every fetch can be held from a moment the test picks, so it can read a state before any
                    // fetch has answered.
                    shut <- gate.fold(Absent: Maybe[Latch] < Sync)(_.get)
                    _    <- shut.fold(Kyo.unit: Unit < Async)(_.await)
                yield (rows.slice(offset, offset + limit), Total.Known(rows.size))
            }
            _    <- src.run
            rows <- watch(src.rows)
        yield (src, rows, calls)

    // The rows behind a source changed (someone else edited them on the server). The cache still holds the old ones,
    // so asking for the page again would only answer from it; reload is what fetches them again. Until the new rows
    // land, the ones on the screen stay, since they are still the best answer anyone has.
    "a reload fetches the page again and keeps the rows shown until the new ones land" in {
        for
            data               <- Signal.initRef[Seq[Row]](all)
            gate               <- AtomicRef.init(Absent: Maybe[Latch])
            (src, rows, calls) <- overData(data, gate = Present(gate))
            _                  <- until(rows)(_ == all.take(2))
            before             <- calls.get
            // The fetch the reload makes is held, so what the reader sees while it is out can be read at all.
            shut <- Latch.init(1)
            _    <- gate.set(Present(shut))
            _    <- data.set(all.reverse)
            _    <- src.reload
            kept <- src.rows.current
            busy <- src.loading.current
            _    <- shut.release
            // Every window up to the new page: the old page or the new one, never an empty or a cut one between them.
            windows <- collect(rows)(_ == all.reverse.take(2))
            after   <- calls.get
            page    <- src.page.get
        yield
            assert(kept == all.take(2), s"the old rows stay until the new ones arrive: $kept")
            assert(busy, "while the source says it is loading")
            assert(windows.forall(w => w == all.take(2) || w == all.reverse.take(2)), s"nothing else in between: $windows")
            assert(after > before, "the new page took a fetch, not the buffer")
            assert(page == 0, "on the page the reader was on")
    }

    // A reset is a source as it opened: the first page, no rows and no length, so a table over it shows itself loading
    // again rather than the old rows under a mask.
    // The buffer still holds the old first page when the reset lands, and the rows behind it have changed, so a turn that
    // got past the reset would put the old page back where the reset had cleared it.
    "a reset puts the source back to how it opened, and fetches the first page again" in {
        for
            data           <- Signal.initRef[Seq[Row]](all)
            gate           <- AtomicRef.init(Absent: Maybe[Latch])
            (src, rows, _) <- overData(data, gate = Present(gate))
            _              <- until(rows)(_ == all.take(2))
            _              <- src.page.set(3)
            _              <- until(rows)(_ == all.slice(6, 8))
            shut           <- Latch.init(1)
            _              <- gate.set(Present(shut))
            _              <- data.set(all.reverse)
            _              <- src.reset
            page           <- src.page.get
            demand         <- src.demand.get
            shown          <- src.rows.current
            total          <- src.total.current
            busy           <- src.loading.current
            _              <- shut.release
            windows        <- collect(rows)(_ == all.reverse.take(2))
        yield
            assert(page == 0 && demand == RowSource.Demand(0, 2), s"back on the first page: $page, $demand")
            assert(shown.isEmpty && total == Total.Unknown(false), s"with no rows and no length: $shown, $total")
            assert(busy, "and loading")
            assert(
                !windows.exists(w => w == all.take(2) || w == all.slice(6, 8)),
                s"no rows from before the reset came back: $windows"
            )
    }

    // A fetch started before the reload answers for the rows as they were. Landing after the reload, it must not put
    // them back on the screen over the rows fetched since.
    "a fetch from before a reload does not land after it" in {
        for
            data               <- Signal.initRef[Seq[Row]](all)
            held               <- Latch.init(1)
            (src, rows, calls) <- overData(data, Present(held))
            // The held fetch has to be the one from before the reload: the feed starts on a fiber of its own, and a
            // reload landing first would make the first fetch a fetch of the new generation, held and never let go.
            started <- watch(calls)
            _       <- until(started)(_ >= 1)
            _       <- data.set(all.reverse)
            _       <- src.reload
            _       <- until(rows)(_ == all.reverse.take(2))
            _       <- held.release
            // Another page, fetched and shown, is the barrier: the held fetch has landed by the time the window moved on
            // and back, and every window published in between is collected, not skipped.
            _     <- src.page.set(1)
            moved <- collect(rows)(_ == all.reverse.slice(2, 4))
            _     <- src.page.set(0)
            back  <- collect(rows)(_ == all.reverse.take(2))
            stale <- rows.drain
            windows = moved ++ back ++ stale
        yield assert(!windows.contains(all.take(2)), s"the old first page came back: $windows")
    }

    /** Every value taken up to and including the first that satisfies `p`. */
    private def collect[A](ch: Channel[A])(p: A => Boolean)(using Frame): Chunk[A] < (Async & Abort[Closed]) =
        ch.take.map(v => if p(v) then Chunk(v) else collect(ch)(p).map(Chunk(v) ++ _))

    // ---- a source runs where it is bound ----

    // Opening a source allocates its state and nothing else, so a page can hold one as plainly as
    // it holds a ref, and the component that binds it is what starts it. The ascription is the
    // point: a source that needed a Scope to open could only be opened inside a mount.
    "a source does nothing until it is run, then starts from where it was left" in {
        for
            q     <- Signal.initRef("a")
            calls <- Signal.initRef(List.empty[Call])
            src   <- (RowSource.init(q, pageSize = 2) { (query, offset, limit) =>
                calls.getAndUpdate(_ :+ Call(query, offset / limit, limit))
                    .andThen((all.slice(offset, offset + limit), Total.Known(all.size)))
            }: RowSource[String, Row] < Sync)
            _      <- src.page.set(3)
            idle   <- src.demand.get
            rows   <- watch(src.rows)
            callCh <- watch(calls)
            served <- Scope.run(src.run.andThen(until(rows)(_.nonEmpty)))
            _      <- q.set("b")
            c      <- Scope.run(src.run.andThen(until(callCh)(c => blocksOf(c, "b").nonEmpty)))
        yield
            assert(idle == RowSource.Demand(0, 2), "nothing follows the page while nothing runs")
            assert(served == all.slice(6, 8), "and a run starts on the page that was written")
            assert(!blocksOf(c, "a").contains(0), s"never on page 0: ${blocksOf(c, "a")}")
    }

    // A table and a list over one source each run it. The feed is one, and it lives as long as
    // ANY of them does: the first to start it is not its owner.
    "two runs share one feed, which outlives the first and stops with the last" in {
        for
            p        <- probe(running = false)
            started  <- Latch.init(1)
            leave    <- Latch.init(1)
            first    <- Fiber.init(Scope.run(p.source.run.andThen(started.release).andThen(leave.await)))
            _        <- started.await
            outlived <- Scope.run {
                for
                    _ <- p.source.run
                    _ <- until(p.rows)(_ == all.take(2))
                    _ <- leave.release
                    _ <- first.get
                    _ <- p.source.page.set(2)
                    r <- until(p.rows)(_ == all.slice(4, 6))
                yield r
            }
            _       <- p.source.page.set(4)
            stopped <- p.source.demand.get
        yield
            assert(outlived == all.slice(4, 6), "the second run kept the feed after the first had gone")
            assert(stopped == RowSource.Demand(4, 2), s"and with both gone nothing follows the page: $stopped")
    }

    // A run is a node's mount, and a node can go at any moment, the moment a run is taking its place among the holders
    // included. Whatever moment that is, the feed has to stop once every run is gone: a place counted with no release
    // registered for it, or a feed started and never recorded, would keep fetching for a page nobody shows. Which moment
    // a stop lands at is down to the scheduler, so the runs are many and each is stopped as soon as it is forked.
    "runs stopped at any moment leave no feed running" in {
        for
            p      <- probe(running = false)
            fibers <- Kyo.foreach(1 to 200) { i =>
                for
                    f <- Fiber.init(Scope.run(p.source.run.andThen(Async.never)))
                    _ <- if i % 3 == 0 then Async.sleep(1.millis) else Kyo.unit
                    _ <- f.interrupt
                yield f
            }
            _ <- Kyo.foreachDiscard(fibers)(_.getResult.unit)
            // A stopped fiber reports its end before its scope's finalizers are through, so the barrier is the holders
            // the releases empty, not the fibers. A place that is never given back keeps this waiting, which fails too.
            holding <- watch(p.source.holding)
            _       <- until(holding)(_ == 0)
            left    <- p.source.feeding
        yield assert(!left, "a feed was left running after every run had gone")
    }

    // The size can change while nothing runs (the table that ran the source was unmounted), and
    // the next run applies it the way a running source would have.
    "a size written while nothing ran is applied by the next run" in {
        for
            size <- Signal.initRef(2)
            p    <- probe(pageSize = size, running = false)
            _    <- Scope.run {
                for
                    _ <- p.source.run
                    _ <- p.source.page.set(4)
                    _ <- until(p.rows)(_ == all.slice(8, 10))
                yield ()
            }
            _    <- size.set(5)
            rows <- Scope.run(p.source.run.andThen(until(p.rows)(_ == all.slice(5, 10))))
            page <- p.source.page.get
        yield
            assert(rows == all.slice(5, 10), "a page of five that still shows row 8")
            assert(page == 1, s"which is page 1, not $page")
    }

    // ---- a page size that changes ----

    // What a rows-per-page dropdown does to a paginated source: the page grows, and the reader
    // keeps the row they were looking at, the way Paginator moves its own page on the same pick.
    "a bound page size serves pages of the size it holds and keeps the first row shown" in {
        for
            size <- Signal.initRef(2)
            p    <- probe(pageSize = size)
            _    <- until(p.rows)(_ == all.take(2))
            _    <- p.source.page.set(2)
            _    <- until(p.rows)(_ == all.slice(4, 6))
            _    <- size.set(3)
            rows <- until(p.rows)(_.size == 3)
            page <- p.source.page.get
            seen <- p.source.pageSize.current
        yield
            assert(rows == all.slice(3, 6), "a page of three that still shows row 4")
            assert(page == 1, s"which is page 1 of three rows, not $page")
            assert(seen == 3, "and the source says how many rows a page holds now")
    }

    // A paginator writes the size and then the page. Between the two the source must not take
    // the old page number at the new size: page 4 of two rows is row 8, page 4 of five rows is
    // row 20, and asking for that is a request for rows the reader never asked to see. The
    // source's observer of a ref runs inside the write, so the range is read right after it:
    // whether a fetch for the wrong range got to start before the next write is down to the
    // scheduler, and a test waiting for it would pass by luck.
    "a page size change moves to the page that shows the first row before anything else is written" in {
        for
            size   <- Signal.initRef(2)
            p      <- probe(pageSize = size)
            _      <- until(p.rows)(_ == all.take(2))
            _      <- p.source.page.set(4)
            _      <- until(p.rows)(_ == all.slice(8, 10))
            _      <- size.set(5)
            demand <- p.source.demand.get
            page   <- p.source.page.get
            // What the paginator writes next, which then changes nothing.
            _    <- p.source.page.set(1)
            rows <- until(p.rows)(_ == all.slice(5, 10))
        yield
            assert(demand == RowSource.Demand(5, 5), s"rows 5 to 9, not $demand")
            assert(page == 1, s"page 1 of five rows, not $page")
            assert(rows == all.slice(5, 10))
    }

    // A viewport writes its range directly and has no page; the size is not what places it.
    "a page size change leaves a range a viewport wrote where it was" in {
        for
            size   <- Signal.initRef(2)
            p      <- probe(pageSize = size)
            _      <- until(p.rows)(_.nonEmpty)
            _      <- p.source.demand.set(RowSource.Demand(1, 4))
            _      <- until(p.rows)(_.size == 4)
            _      <- size.set(3)
            demand <- p.source.demand.get
            page   <- p.source.page.get
        yield
            assert(demand == RowSource.Demand(1, 4), s"the viewport's range, not $demand")
            assert(page == 0, "and no page was invented for it")
    }

    // The cache is sized for one range and its buffers, so a range read again is not fetched
    // twice. A page that grows past what the source opened with is a range of more blocks, and
    // the room has to grow with it: here the source opened with room for three blocks of two
    // rows, and a page of eight is four of them before its buffer.
    "a page that grows past its opening size is read again from the buffer" in {
        for
            size <- Signal.initRef(2)
            p    <- probe(pageSize = size, config = RowSource.Config(blockSize = Present(2), maxBlocks = 1))
            _    <- until(p.rows)(_ == all.take(2))
            _    <- size.set(8)
            _    <- until(p.rows)(_ == all.take(8))
            // The same blocks under another range, which is what a viewport over the page asks.
            _ <- p.source.demand.set(RowSource.Demand(1, 7))
            _ <- until(p.rows)(_ == all.slice(1, 8))
            _ <- p.query.set("b")
            c <- until(p.calls)(c => blocksOf(c, "b").nonEmpty)
        yield
            val twice = blocksOf(c, "a").filter(_ <= 3).groupBy(identity).collect { case (b, n) if n.size > 1 => b }
            assert(twice.isEmpty, s"blocks of the page fetched twice: $twice")
    }

    // ---- the viewport arithmetic ----

    /** Five rows fit (200 / 40), and one overscan row is added on either side. */
    private val view = RowSource.Viewport(itemSize = 40, height = 200, overscan = 1)

    "a viewport asks for what fits, plus the overscan on either side" in {
        assert(view.visible == 5, "five rows of forty fit two hundred")
        assert(view.windowSize == 8, "five, one over each edge, and the row a scroll of less than one reaches")
        assert(view.span(0.0) == (0, 8), "at rest it starts at the top rather than one row above it")
        assert(view.span(400.0) == (9, 17), "ten rows down, and one above that")
        assert(view.demand(400.0) == RowSource.Demand(9, 8), "which is exactly what it asks for")
        assert(view.span(-50.0) == (0, 8), "a position the browser should never report is still a position")
    }

    "a partial row still counts as a row that fits" in {
        val odd = RowSource.Viewport(itemSize = 30, height = 100, overscan = 0)
        assert(odd.visible == 4, "three whole rows and the sliver of a fourth")
    }

    "how far a viewport reaches is what the total says, or a screen past what has loaded" in {
        assert(view.extent(Total.Known(100), 6) == 100, "a known count speaks for the whole list")
        assert(view.extent(Total.Unknown(true, 0), 6) == 11, "six loaded and a screen to scroll into")
        assert(view.extent(Total.Unknown(true, 40), 6) == 45, "or as far as the source has ever served")
        assert(view.extent(Total.Unknown(false, 0), 6) == 6, "and nothing following stops at what there is")
    }

    "a viewport cannot be scrolled past the end of what it holds" in {
        assert(view.maxScroll(100) == 3800.0, "a hundred rows of forty, less the two hundred on the screen")
        assert(view.maxScroll(2) == 0.0, "a list that fits does not scroll at all")
        assert(view.clamp(9999.0, 100) == 3800.0)
        assert(view.clamp(-1.0, 100) == 0.0)
    }

end RowSourceTest
