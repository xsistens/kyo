package kyo.uic

import kyo.*

/** How many rows a query matched, and whether that is knowable at all.
  *
  * A `count(*)` beside the page answers [[Total.Known]], and a paginator built on it can
  * say how many pages there are. Plenty of sources cannot answer that, or will not pay
  * for it: a cursor API, a search index, a `limit n + 1` probe. Those answer
  * [[Total.Unknown]], which carries only whether anything follows the rows just handed
  * over, and a paginator built on it grows one page at a time.
  *
  * Modelling the second case rather than demanding a number is what keeps a caller from
  * inventing one. A fabricated total is not a smaller wrong than a missing one: it puts
  * pages in the paginator that no query will ever fill.
  */
enum Total derives CanEqual:
    /** The query matched `count` rows in all. */
    case Known(count: Int)

    /** How many rows the query matched is not known. `hasMore` says whether anything
      * follows the rows just delivered, and `atLeast` how many the source has served so
      * far, which is the floor under the length of the whole.
      *
      * A caller answers with `hasMore` alone and leaves `atLeast` at zero: a fetch knows
      * about its own block and nothing else. [[RowSource]] fills it in, and never lets it
      * fall, because a viewport that sized its scrollbar off what it had seen must not
      * see it shrink when the reader scrolls back up.
      */
    case Unknown(hasMore: Boolean, atLeast: Int = 0)
end Total

object Total:
    /** What several blocks of ONE query say together.
      *
      * A `Known` count is a property of the query, so any block carrying one speaks for
      * all of them. Without one, only the LAST block can say whether anything follows,
      * since that is the only place the question is about.
      */
    private[uic] def combine(parts: Seq[Total]): Total =
        parts.collectFirst { case k: Total.Known => k }.getOrElse(
            Total.Unknown(
                parts.lastOption.exists {
                    case Total.Unknown(more, _) => more
                    case _                      => false
                },
                parts.collect { case Total.Unknown(_, a) => a }.maxOption.getOrElse(0)
            )
        )
end Total

/** Where a table's rows come from when it is not holding them: a block-cached, prefetching
  * window onto a set that lives somewhere else.
  *
  * The caller writes one function, `(query, offset, limit) => (rows, total)`, and binds the
  * source to a component. Everything between the two, which rows are wanted, which blocks
  * cover them, what to fetch ahead, what to keep and what to drop, is the source's.
  *
  * ==What it is made of==
  *
  * Rows are fetched in fixed-size BLOCKS, never in pages, because a viewport asks for rows
  * 37 to 52 and that lies across any page boundary. A page is then just a range that starts
  * on a block boundary, which is why [[RowSource.Config.blockSize]] defaults to the page
  * size and may be set to anything else: one large block serves several pages from one
  * request.
  *
  * The cache is keyed by the QUERY together with the block index, so a new sort or a new
  * filter is a different key and the old blocks fall out on their own. Nothing has to be
  * invalidated, because nothing is ever overwritten.
  *
  * ==The rule that makes it correct==
  *
  * A fetch is never interrupted, only the waiting for it. Both follow from how the pieces
  * underneath behave: `Signal.observe` runs its callback inside a scope PER VALUE and
  * closes it the moment the value changes, so a fetch forked inside it with `Fiber.init`
  * would be killed exactly when the reader moves to the page it was fetching. So every
  * fetch is forked with `Fiber.initUnscoped` and only ever awaited from inside that scope,
  * where an interruption costs nothing: the waiter dies, the fetch completes, and the next
  * reader of that block finds it warm.
  *
  * For the same reason the cache is filled SYNCHRONOUSLY on the observer's own fiber, and
  * only the awaiting suspends. Two callers can therefore never start the same block twice,
  * without a promise handshake to get wrong.
  *
  * ==What it does not do==
  *
  * It does not decide what is on the screen. A paginator writes [[page]], a viewport writes
  * [[demand]], and the source answers the range it is given; a component that reads
  * [[rows]] renders what it is handed. That split is what lets one source serve a
  * paginated table, an infinitely scrolled one, and a virtual list at the same time.
  *
  * ==Opened and run==
  *
  * [[RowSource.init]] only allocates: the refs, the cache, the counters. A page holds a
  * source the way it holds a ref, and its refs are there from the start for anything
  * outside the table to read or write (a search field resetting [[page]], a count of what
  * was fetched). Fetching is [[run]], which the component the source is bound to calls in
  * its own mount, so a table over a source needs no mount of the page's own and no copy of
  * itself as that mount's placeholder: unmounted, it renders itself loading. Any number of
  * runs may overlap and share one feed, which stops when the last of them ends.
  *
  * ==A page size that changes==
  *
  * A size bound as a `SignalRef` is the reader's to change through a rows-per-page
  * dropdown. The source then moves [[page]] to the page that still shows the first row the
  * reader saw, in the same write, so a paginator that writes the size and then that page
  * finds it already there and nothing is fetched for the old page number at the new size.
  * A range a viewport wrote is not a page and stays where it is.
  */
final class RowSource[Q, A] private[uic] (
    /** How many rows one page holds, which is also what a paginator steps by. */
    val pageSize: Signal[Int],

    /** The size as the caller handed it: a constant, or the ref a rows-per-page dropdown
      * writes. A component binds this rather than [[pageSize]], so a constant stays one the
      * reader cannot change.
      */
    private[uic] val pageSizeBinding: Int | SignalRef[Int],

    /** The 0-based page, for a paginator to drive. Writing it rewrites [[demand]]. */
    val page: SignalRef[Int],

    /** The row range currently asked for. A viewport writes this directly; a paginated
      * table gets it written from [[page]].
      */
    val demand: SignalRef[RowSource.Demand],
    private[uic] val windowRef: SignalRef[RowSource.Window[A]],
    private[uic] val totalRef: SignalRef[Total],
    private[uic] val loadingRef: SignalRef[Boolean],
    runner: RowSource.Runner
)(using CanEqual[A, A], Frame):
    /** Fetches for as long as the enclosing `Scope` is open: follows [[page]] and [[pageSize]]
      * into [[demand]], and serves every range [[demand]] asks for.
      *
      * A component the source is bound to calls this in its own mount, so a page seldom
      * does; it is public for a page that drives a source with no component, an export or
      * a view of its own. It starts from where the source was left: a page or a size written
      * while nothing ran is applied as it starts, and a range a viewport wrote is served as
      * it stands. Runs that overlap share one feed, which ends with the last of them; the
      * cache outlives them all, so a component mounted again finds its blocks still there.
      * A fetch that fails is reported on every node running the source, as a failure of a
      * fiber forked with `UI.fork`, and the next run starts the feed again.
      */
    def run(using Frame): Unit < (Async & Scope) = runner.run

    /** Whether a feed is running, which is what a test of the runs' bookkeeping reads. */
    private[uic] def feeding(using Frame): Boolean < Sync = runner.feeding

    /** How many runs hold the feed. A run's release can land after its fiber has ended (a stopped fiber reports its end
      * before its scope's finalizers are through), so a test waits on this rather than on the fibers.
      */
    private[uic] def holding(using Frame): Signal[Int] = runner.holding

    /** The rows currently served, together with the row index the first of them sits at.
      *
      * The offset travels WITH the rows because a consumer that positions them, a virtual
      * viewport above all, must not position what it asked for at where it asked for it: a
      * window published for the previous range would then be drawn at the new one's
      * offsets for exactly one frame.
      */
    def window: Signal[RowSource.Window[A]] = windowRef

    /** The rows of the range currently asked for, for a consumer that renders them in
      * order and has no offsets to place them at.
      */
    def rows: Signal[Seq[A]] = windowRef.map(_.rows)

    /** What is known about the size of the whole set. */
    def total: Signal[Total] = totalRef

    /** Whether a fetch the reader is waiting on is in flight. A range served from the
      * buffer never raises this, which is the whole point of the buffer.
      */
    def loading: Signal[Boolean] = loadingRef
end RowSource

object RowSource:

    /** The row range a component is asking for: `limit` rows starting at `offset`. */
    final case class Demand(offset: Int, limit: Int) derives CanEqual

    /** The rows of one range, and the row index the first of them sits at. */
    final case class Window[A](offset: Int, rows: Seq[A]) derives CanEqual

    /** The geometry of a viewport whose rows are all the same height: what fits in it,
      * what it should ask for at a given scroll position, and how far it reaches.
      *
      * It is the whole arithmetic a windowed list is made of, and it is the same
      * arithmetic whether the rows are `div`s or table rows, so it lives beside the
      * [[Demand]] it computes rather than inside either component. A window that a
      * component draws and a range that it asks for are then one calculation and cannot
      * drift apart.
      *
      * @param itemSize
      *   Row height in px. The rows really have to be this tall: it is what turns a scroll
      *   position into a row index.
      * @param height
      *   Viewport height in px.
      * @param overscan
      *   Rows drawn above and below what fits, so a scroll of less than one row has
      *   somewhere to go before the next window arrives.
      */
    final private[uic] case class Viewport(itemSize: Int, height: Int, overscan: Int):
        /** Rows that fit the viewport, which is the window before the overscan. */
        def visible: Int = math.ceil(height.toDouble / math.max(1, itemSize)).toInt

        /** How many rows one window holds, the overscan on both sides included. */
        def windowSize: Int = visible + 2 * overscan + 1

        /** The row range at `scrollTop`, before any clamp at the end: a scroll handler has
          * no row count to clamp against, and the render that has one clamps it there.
          */
        def span(scrollTop: Double): (Int, Int) =
            val first = math.max(0, (math.max(0.0, scrollTop) / math.max(1, itemSize)).toInt - overscan)
            (first, first + windowSize)

        /** The range to ask a [[RowSource]] for at `scrollTop`, which is the range about to
          * be drawn.
          */
        def demand(scrollTop: Double): Demand =
            val (first, last) = span(scrollTop)
            Demand(first, last - first)

        /** How many rows there are to scroll over. A [[Total.Known]] count says it
          * outright; an unknown one spans what has loaded plus one screen while anything
          * follows, so there is always somewhere left to scroll into and the next scroll
          * asks for it. That is infinite scrolling, and it needs no mode of its own.
          */
        def extent(total: Total, loadedEnd: Int): Int = total match
            case Total.Known(n)                => math.max(0, n)
            case Total.Unknown(true, atLeast)  => math.max(loadedEnd, atLeast) + visible
            case Total.Unknown(false, atLeast) => math.max(loadedEnd, atLeast)

        /** The furthest `count` rows can be scrolled. */
        def maxScroll(count: Int): Double = math.max(0.0, (count * itemSize - height).toDouble)

        /** `scrollTop` held inside what `count` rows can be scrolled by. */
        def clamp(scrollTop: Double, count: Int): Double =
            math.max(0.0, math.min(maxScroll(count), scrollTop))
    end Viewport

    /** How the buffer behaves.
      *
      * @param blockSize
      *   Rows per fetch. Defaults to the page size and follows it when it changes, so one
      *   page is one request; a larger block serves several pages from one.
      * @param buffer
      *   How many blocks to fetch ahead on either side of the range asked for. One is the
      *   next page and the previous one.
      * @param maxBlocks
      *   How many blocks the cache holds before it starts dropping the least recently
      *   used. Raised automatically to whatever the range asked for plus its buffers needs,
      *   also when a page grows, since a block dropped before the range that asked for it
      *   is read again would be fetched twice.
      * @param expireAfterWrite
      *   How long a fetched block stays usable. `Duration.Zero` keeps it until it is
      *   evicted or the query changes, which is right for data that does not move under
      *   the reader and wrong for data that does.
      */
    final case class Config(
        blockSize: Maybe[Int] = Absent,
        buffer: Int = 1,
        maxBlocks: Int = 8,
        expireAfterWrite: Duration = Duration.Zero
    )

    /** Opens a source over `fetch`, fed from `query` and from whatever writes
      * [[RowSource.page]], [[RowSource.pageSize]] or [[RowSource.demand]] once it runs.
      *
      * Opening allocates and starts nothing, so a page opens a source where it opens its
      * refs, outside any mount. The component the source is bound to runs it
      * ([[RowSource.run]]) in its own mount, and that is when the first range is fetched.
      *
      * @param query
      *   Everything besides the range that decides which rows match: the search text, the
      *   sort spec, the filters. It is part of the cache key, so changing it retires the
      *   blocks of the old query without invalidating anything.
      * @param pageSize
      *   Rows per page, and the default block size. A `SignalRef` binds two way: a
      *   rows-per-page dropdown writes the reader's pick into it, and the source pages by
      *   whatever it holds.
      * @param fetch
      *   `(query, offset, limit)` to the rows of that range plus what is known about the
      *   size of the whole. It is called with block-aligned offsets, so `limit` is the
      *   block size except possibly at the end.
      */
    def init[Q, A](query: Signal[Q], pageSize: Int | SignalRef[Int], config: Config = Config())(
        fetch: (Q, Int, Int) => (Seq[A], Total) < Async
    )(using Frame, CanEqual[Q, Q], CanEqual[A, A]): RowSource[Q, A] < Sync =
        val ahead                        = math.max(0, config.buffer)
        val sized: SignalRef[Int] < Sync = pageSize match
            case n: Int                       => Signal.initRef(math.max(1, n))
            case r: SignalRef[Int] @unchecked => r
        for
            sizeRef    <- sized
            openSize   <- sizeRef.get.map(math.max(1, _))
            pageRef    <- Signal.initRef(0)
            demandRef  <- Signal.initRef(Demand(0, openSize))
            windowRef  <- Signal.initRef(Window(0, Seq.empty[A]))
            totalRef   <- Signal.initRef(Total.Unknown(false): Total)
            loadingRef <- Signal.initRef(true)
            cache      <- AtomicRef.init(Absent: Maybe[(Int, Cache[(Q, Int, Int), Fiber[(Seq[A], Total), Any]])])
            seen       <- AtomicRef.init(Absent: Maybe[(Q, Int)])
            laid       <- AtomicRef.init((0, openSize))
            mutex      <- Meter.initMutexUnscoped
            holders    <- Signal.initRef(Set.empty[Long])
            ids        <- AtomicLong.init(0L)
            running    <- AtomicRef.init(Absent: Maybe[Fiber[Unit, Any]])
            feed   = new Feed(query, fetch, cache, seen, sizeRef, config, ahead, windowRef, totalRef, loadingRef)
            runner = new Run(query, feed, pageRef, sizeRef, demandRef, laid, mutex, holders, ids, running)
        yield new RowSource(sizeRef.map(math.max(1, _)), pageSize, pageRef, demandRef, windowRef, totalRef, loadingRef, runner)
        end for
    end init

    /** The range of page `p` at `size` rows a page. */
    private def pageRange(p: Int, size: Int): Demand =
        val s = math.max(1, size)
        Demand(math.max(0, p) * s, s)

    /** What [[RowSource.run]] runs, kept apart so the source's public value carries no type of the feed's. */
    private[uic] trait Runner:
        def run(using Frame): Unit < (Async & Scope)
        def feeding(using Frame): Boolean < Sync
        def holding(using Frame): Signal[Int]
    end Runner

    /** The running half: the page and size followed into the range, and the one feed every run shares.
      *
      * `laid` is the page and size the range was last laid out from as a page. A range equal to that page's is a page,
      * which a new size moves and a new page replaces; any other range is a viewport's, which neither touches.
      */
    final private class Run[Q, A](
        query: Signal[Q],
        feed: Feed[Q, A],
        pageRef: SignalRef[Int],
        sizeRef: SignalRef[Int],
        demandRef: SignalRef[Demand],
        laid: AtomicRef[(Int, Int)],
        mutex: Meter,
        holders: SignalRef[Set[Long]],
        ids: AtomicLong,
        running: AtomicRef[Maybe[Fiber[Unit, Any]]]
    )(using CanEqual[Q, Q]) extends Runner:

        def run(using Frame): Unit < (Async & Scope) =
            for
                _ <- catchUp
                p <- pageRef.get
                s <- sizeRef.get
                // Both are `onChange`, which runs inside the write that changed the ref, so the range follows a page or a
                // size before the writer's `set` returns: a size and the page a paginator writes after it are applied in
                // that order, and the second finds the range the first already moved to. `observe` would deliver them
                // later on a fiber of its own, where the size could be applied to a range the page had not reached yet.
                // Seeded with what `catchUp` just applied, so a write between the two is delivered rather than lost. Each
                // run registers its own, and they agree: every write they make is one the other has already made.
                _     <- pageRef.onChange(Present(p))(paged)
                _     <- sizeRef.onChange(Present(s))(resized)
                token <- ids.incrementAndGet
                _     <- Scope.ensure(locked(release(token)))
                feed  <- locked(acquire(token))
                // The feed is shared, so it cannot be forked on this run's scope; this run waits for it to end instead,
                // and a feed that fails is then a failure of a fiber this node forked. Not on the feed itself: a fiber
                // waiting on another passes its interrupt on, so this run's end would stop the feed every other run is
                // still reading. Each run waits on a promise of its own, which the feed's end completes.
                ended <- Promise.init[Unit, Any]
                _     <- feed.onComplete(ended.completeDiscard(_))
                _     <- UI.fork(ended.get)
            yield ()

        /** A page and a size written while nothing ran, applied as a running source would have applied them. A page
          * that moved is the page now shown; a size alone moves the page as a new size does. A viewport's range stays.
          */
        private def catchUp(using Frame): Unit < Sync =
            for
                (lp, ls) <- laid.get
                p        <- pageRef.get
                s        <- sizeRef.get.map(math.max(1, _))
                d        <- demandRef.get
                _        <-
                    if d != pageRange(lp, ls) then Kyo.unit
                    else if p != lp then paged(p)
                    else if s != ls then resized(s)
                    else Kyo.unit
            yield ()

        private def paged(p: Int)(using Frame): Unit < Sync =
            sizeRef.get.map { size =>
                val s = math.max(1, size)
                laid.set((math.max(0, p), s)).andThen(demandRef.set(pageRange(p, s)))
            }

        /** A new page size, applied to the range the reader is on.
          *
          * A range laid out as a page at the previous size moves to the page that still shows its first row, which is
          * where a paginator moves its own page on the same pick, and the range is written with it. This runs inside the
          * write of the size, so by the time a paginator writes its page that page is already the source's and the write
          * changes nothing: no range is ever served for the old page number at the new size. A range a viewport wrote is
          * left alone, since it is not a page at all.
          */
        private def resized(size: Int)(using Frame): Unit < Sync =
            val s = math.max(1, size)
            for
                (lp, ls) <- laid.get
                d        <- demandRef.get
                _        <-
                    if d == pageRange(lp, ls) && ls != s then
                        val moved = lp * ls / s
                        laid.set((moved, s)).andThen(pageRef.set(moved)).andThen(demandRef.set(pageRange(moved, s)))
                    else Kyo.unit
            yield ()
            end for
        end resized

        def feeding(using Frame): Boolean < Sync =
            running.get.map(_.fold(false: Boolean < Sync)(_.poll.map(_.isEmpty)))

        def holding(using Frame): Signal[Int] = holders.map(_.size)

        /** Holds the feed for run `token`: the shared feed, started by the first run and again by any run that finds it
          * ended (a fetch failed).
          *
          * One step, so a stop cannot land between taking a place among the holders, starting the feed and recording
          * it: a place taken with the feed unrecorded, or a feed started and never recorded, would keep it running after
          * the last run had gone.
          */
        private def acquire(token: Long)(using Frame): Fiber[Unit, Any] < Sync =
            Sync.Unsafe.defer {
                // Unsafe: each safe call evaluated inline, which is what keeps the whole of it one step.
                discard(Sync.Unsafe.evalOrThrow(holders.updateAndGet(_ + token)))
                val held = Sync.Unsafe.evalOrThrow(running.get)
                held.filter(f => Sync.Unsafe.evalOrThrow(f.poll).isEmpty).getOrElse {
                    val f = Sync.Unsafe.evalOrThrow(
                        Fiber.initUnscoped(query.combineLatest(demandRef).observe((q, d) => feed.serve(q, d)))
                    )
                    Sync.Unsafe.evalOrThrow(running.set(Present(f)))
                    f
                }
            }

        /** Lets go of the feed for run `token`; the last one stops it. The cache stays, so the next run finds its blocks.
          *
          * Registered BEFORE the run takes its place, so a run stopped anywhere in between still lets go: a token that
          * never got a place takes nothing away, and stops the feed only if no other run holds it either. One step, for
          * the same reason as [[acquire]].
          */
        private def release(token: Long)(using Frame): Unit < Sync =
            Sync.Unsafe.defer {
                // Unsafe: each safe call evaluated inline, which is what keeps the whole of it one step.
                if Sync.Unsafe.evalOrThrow(holders.updateAndGet(_ - token)).isEmpty then
                    Sync.Unsafe.evalOrThrow(running.getAndSet(Absent)).foreach(f => discard(Sync.Unsafe.evalOrThrow(f.interrupt)))
            }

        /** Starting and stopping the feed under one lock, so a run ending as another starts can never stop the feed the
          * other just started. The mutex is never closed, so `Closed` cannot occur here; if it ever did, that is a bug.
          */
        private def locked[B](v: => B < Sync)(using Frame): B < Async =
            Abort.run[Closed](mutex.run(v)).map(_.getOrThrow)
    end Run

    /** The fetching half, kept out of [[RowSource]] so the value a caller holds carries
      * only what a caller reads.
      */
    final private class Feed[Q, A](
        query: Signal[Q],
        fetch: (Q, Int, Int) => (Seq[A], Total) < Async,
        cacheRef: AtomicRef[Maybe[(Int, Cache[(Q, Int, Int), Fiber[(Seq[A], Total), Any]])]],
        seen: AtomicRef[Maybe[(Q, Int)]],
        sizeRef: SignalRef[Int],
        config: Config,
        ahead: Int,
        windowRef: SignalRef[Window[A]],
        totalRef: SignalRef[Total],
        loadingRef: SignalRef[Boolean]
    )(using CanEqual[Q, Q]):
        /** The cache, with room for `blocks` blocks at least.
          *
          * A range spans that many blocks with its buffers, and dropping one of them before
          * the range that asked for it is read again would fetch it a second time. A range
          * of more blocks than the cache holds (a page that grew, a viewport taller than a
          * page) gets a larger cache, which takes over the blocks of that range and its
          * buffers from the old one. The rest of the old one is let go: it is a buffer, and
          * what it held is fetched again if it is ever asked for.
          */
        private def room(q: Q, block: Int, first: Int, last: Int)(using
            Frame
        ): Cache[(Q, Int, Int), Fiber[(Seq[A], Total), Any]] < Sync =
            val blocks = math.max(config.maxBlocks, last - first + 1 + 2 * ahead)
            cacheRef.get.map {
                case Present((held, cache)) if held >= blocks => cache
                case old                                      =>
                    Cache.init[(Q, Int, Int), Fiber[(Seq[A], Total), Any]](
                        maxSize = blocks,
                        expireAfterWrite = config.expireAfterWrite
                    ).map { grown =>
                        val keep                 = (math.max(0, first - ahead) to (last + ahead)).toList
                        val carried: Unit < Sync = old match
                            case Present((_, cache)) =>
                                Kyo.foreachDiscard(keep)(at =>
                                    cache.get((q, block, at)).map(_.fold(Kyo.unit)(f => grown.add((q, block, at), f).unit))
                                )
                            case Absent => Kyo.unit
                        carried.andThen(cacheRef.set(Present((blocks, grown)))).andThen(grown)
                    }
            }
        end room

        /** The block already in flight or done for this key, or a fresh one started for it.
          *
          * Synchronous on purpose: every call runs on the observer's own fiber, so two
          * callers cannot both miss and both start. `initUnscoped` is what keeps the fetch
          * out of the caller's scope, which is the rule the class doc explains, and the
          * `Sync` in the return type is what keeps it that way: a scoped `Fiber.init`
          * needs a `Scope` here and does not compile.
          *
          * The block size is part of the key, since block 3 of five rows is not block 3 of
          * two: a page size that changes leaves the old blocks to fall out on their own.
          */
        private def warm(
            cache: Cache[(Q, Int, Int), Fiber[(Seq[A], Total), Any]],
            q: Q,
            block: Int,
            at: Int
        )(using Frame): Fiber[(Seq[A], Total), Any] < Sync =
            cache.get((q, block, at)).map {
                case Present(f) => f
                case Absent     =>
                    Fiber.initUnscoped(fetch(q, at * block, block)).map(f => cache.add((q, block, at), f))
            }

        /** One turn of the loop: cover the range, publish it, then reach one buffer out on
          * either side. The reaching is the last thing, so a reader never waits on it.
          */
        def serve(q: Q, d: Demand)(using Frame): Unit < Async =
            sizeRef.get.map { size =>
                val block = math.max(1, config.blockSize.getOrElse(math.max(1, size)))
                val first = math.max(0, d.offset) / block
                val last  = math.max(first, (math.max(0, d.offset) + math.max(1, d.limit) - 1) / block)
                for
                    cache  <- room(q, block, first, last)
                    wanted <- Kyo.foreach((first to last).toList)(warm(cache, q, block, _))
                    // Warm already means served: the mask is for a reader who has to wait.
                    ready <- Kyo.foreach(wanted)(_.poll).map(_.forall(_.isDefined))
                    _     <- loadingRef.set(!ready)
                    parts <- Kyo.foreach(wanted)(_.get)
                    total <- settle(q, block, first, parts)
                    _     <- report(parts, block, first, total)
                    // The total goes out BEFORE the rows, and the order is load-bearing: a
                    // consumer that positions rows by the total, a virtual viewport above all,
                    // would otherwise be handed rows at an offset the total does not reach yet
                    // and draw them as though they were not there. Ahead is the harmless side
                    // to be on: an extent that is already long enough costs a spacer that is
                    // briefly too tall, where the other way round costs the rows themselves.
                    _ <- totalRef.set(total)
                    _ <- windowRef.set(Window(math.max(0, d.offset), slice(parts, block, first, d)))
                    _ <- loadingRef.set(false)
                    _ <- Kyo.foreach(neighbours(block, first, last, total))(warm(cache, q, block, _))
                yield ()
                end for
            }
        end serve

        /** What the blocks say together, with everything the source knows added to it.
          *
          * Two things are known that no single fetch is: how far the source has served for
          * this query, which is the floor under an unknown length and must never fall, and
          * that a block saying nothing follows it makes the length exact. The floor is kept
          * WITH its query, so a new one starts from nothing rather than inheriting the
          * length of the last.
          */
        private def settle(q: Q, block: Int, first: Int, parts: Seq[(Seq[A], Total)])(using Frame): Total < Sync =
            val served = first * block + parts.map(_._1.size).sum
            seen.updateAndGet {
                case Present((prev, floor)) if prev == q => Present((q, math.max(floor, served)))
                case _                                   => Present((q, served))
            }.map { held =>
                val floor = held.fold(served)(_._2)
                Total.combine(parts.map(_._2)) match
                    case Total.Known(n)            => Total.Known(n)
                    case Total.Unknown(false, _)   => Total.Known(served)
                    case Total.Unknown(true, said) => Total.Unknown(true, math.max(said, floor))
                end match
            }
        end settle

        /** The rows of the range, cut out of the blocks that cover it. */
        private def slice(parts: Seq[(Seq[A], Total)], block: Int, first: Int, d: Demand): Seq[A] =
            val from = math.max(0, d.offset) - first * block
            parts.flatMap(_._1).slice(from, from + math.max(1, d.limit))

        /** The blocks to fetch ahead, clamped at the start and, where the size is known, at
          * the end: reaching past either would fetch a range that cannot exist.
          */
        private def neighbours(block: Int, first: Int, last: Int, total: Total): List[Int] =
            val before = (math.max(0, first - ahead) until first).toList
            val after  = total match
                case Total.Known(n) =>
                    val lastBlock = math.max(0, (n - 1) / block)
                    ((last + 1) to math.min(last + ahead, lastBlock)).toList
                case Total.Unknown(true, _)  => ((last + 1) to (last + ahead)).toList
                case Total.Unknown(false, _) => Nil
            before ++ after
        end neighbours

        /** A block that came back short where a full one was expected. It is not an error:
          * rows can be deleted between two fetches, and the rows on the screen are still
          * real rows. It does shift everything behind it by one, which is worth saying out
          * loud rather than leaving to be noticed.
          */
        private def report(parts: Seq[(Seq[A], Total)], block: Int, first: Int, total: Total)(using Frame): Unit < Async =
            val lastBlock = total match
                case Total.Known(n)      => Present(math.max(0, (n - 1) / block))
                case Total.Unknown(_, _) => Absent
            val odd = parts.zipWithIndex.collect {
                case ((rows, _), i) if rows.size > block =>
                    s"block ${first + i} answered with ${rows.size} rows for a block size of $block"
                case ((rows, _), i) if rows.size < block && !lastBlock.contains(first + i) =>
                    s"block ${first + i} answered with ${rows.size} rows where a full block of $block was expected"
            }
            Kyo.foreach(odd)(m => Log.warn(s"RowSource: $m")).unit
        end report
    end Feed
end RowSource
