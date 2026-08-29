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

    /** How many rows the query matched is not known; `hasMore` says whether anything
      * follows the rows just delivered.
      */
    case Unknown(hasMore: Boolean)
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
            Total.Unknown(parts.lastOption.exists {
                case Total.Unknown(more) => more
                case _                   => false
            })
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
  */
final class RowSource[Q, A] private[uic] (
    /** How many rows one page holds, which is also what a paginator steps by. */
    val pageSize: Int,
    /** The 0-based page, for a paginator to drive. Writing it rewrites [[demand]]. */
    val page: SignalRef[Int],
    /** The row range currently asked for. A viewport writes this directly; a paginated
      * table gets it written from [[page]].
      */
    val demand: SignalRef[RowSource.Demand],
    private[uic] val rowsRef: SignalRef[Seq[A]],
    private[uic] val totalRef: SignalRef[Total],
    private[uic] val loadingRef: SignalRef[Boolean]
):
    /** The rows of the range currently asked for. */
    def rows: Signal[Seq[A]] = rowsRef

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

    /** How the buffer behaves.
      *
      * @param blockSize
      *   Rows per fetch. Defaults to the page size, so one page is one request; a larger
      *   block serves several pages from one.
      * @param buffer
      *   How many blocks to fetch ahead on either side of the range asked for. One is the
      *   next page and the previous one.
      * @param maxBlocks
      *   How many blocks the cache holds before it starts dropping the least recently
      *   used. Raised automatically to whatever one range plus its buffers needs, since a
      *   block dropped before the range that asked for it is read would be fetched twice.
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

    /** Opens a source over `fetch`, feeding it from `query` and from whatever writes
      * [[RowSource.page]] or [[RowSource.demand]].
      *
      * Two fibers are forked on the enclosing scope, so this belongs inside a
      * `UI.mounted` effect: the feed then stops when the node does, and a fetch that
      * fails surfaces on the node rather than leaving stale rows standing.
      *
      * @param query
      *   Everything besides the range that decides which rows match: the search text, the
      *   sort spec, the filters. It is part of the cache key, so changing it retires the
      *   blocks of the old query without invalidating anything.
      * @param pageSize
      *   Rows per page, and the default block size.
      * @param fetch
      *   `(query, offset, limit)` to the rows of that range plus what is known about the
      *   size of the whole. It is called with block-aligned offsets, so `limit` is the
      *   block size except possibly at the end.
      */
    def init[Q, A](query: Signal[Q], pageSize: Int, config: Config = Config())(
        fetch: (Q, Int, Int) => (Seq[A], Total) < Async
    )(using Frame, CanEqual[Q, Q], CanEqual[A, A]): RowSource[Q, A] < (Sync & Async & Scope) =
        val size  = math.max(1, pageSize)
        val block = math.max(1, config.blockSize.getOrElse(size))
        val ahead = math.max(0, config.buffer)
        // A range spans at most this many blocks, and dropping one of them before the
        // range that asked for it is read would fetch it a second time.
        val span = (size + block - 1) / block + 1
        val room = math.max(config.maxBlocks, span + 2 * ahead)
        for
            pageRef    <- Signal.initRef(0)
            demandRef  <- Signal.initRef(Demand(0, size))
            rowsRef    <- Signal.initRef(Seq.empty[A])
            totalRef   <- Signal.initRef(Total.Unknown(false): Total)
            loadingRef <- Signal.initRef(true)
            cache <- Cache.init[(Q, Int), Fiber[(Seq[A], Total), Any]](
                maxSize = room,
                expireAfterWrite = config.expireAfterWrite
            )
            feed = new Feed(query, fetch, cache, block, ahead, rowsRef, totalRef, loadingRef)
            // A page is a range that starts on a page boundary. Keeping the two refs
            // rather than folding them is what lets a viewport write the range directly.
            _ <- UI.fork(pageRef.observe(p => demandRef.set(Demand(math.max(0, p) * size, size))))
            _ <- UI.fork(query.combineLatest(demandRef).observe((q, d) => feed.serve(q, d)))
        yield new RowSource(size, pageRef, demandRef, rowsRef, totalRef, loadingRef)
        end for
    end init

    /** The fetching half, kept out of [[RowSource]] so the value a caller holds carries
      * only what a caller reads.
      */
    final private class Feed[Q, A](
        query: Signal[Q],
        fetch: (Q, Int, Int) => (Seq[A], Total) < Async,
        cache: Cache[(Q, Int), Fiber[(Seq[A], Total), Any]],
        block: Int,
        ahead: Int,
        rowsRef: SignalRef[Seq[A]],
        totalRef: SignalRef[Total],
        loadingRef: SignalRef[Boolean]
    ):
        /** The block already in flight or done for this key, or a fresh one started for it.
          *
          * Synchronous on purpose: every call runs on the observer's own fiber, so two
          * callers cannot both miss and both start. `initUnscoped` is what keeps the fetch
          * out of the caller's scope, which is the rule the class doc explains, and the
          * `Sync` in the return type is what keeps it that way: a scoped `Fiber.init`
          * needs a `Scope` here and does not compile.
          */
        private def warm(q: Q, at: Int)(using Frame): Fiber[(Seq[A], Total), Any] < Sync =
            cache.get((q, at)).map {
                case Present(f) => f
                case Absent =>
                    Fiber.initUnscoped(fetch(q, at * block, block)).map(f => cache.add((q, at), f))
            }

        /** One turn of the loop: cover the range, publish it, then reach one buffer out on
          * either side. The reaching is the last thing, so a reader never waits on it.
          */
        def serve(q: Q, d: Demand)(using Frame): Unit < Async =
            val first = math.max(0, d.offset) / block
            val last  = math.max(first, (math.max(0, d.offset) + math.max(1, d.limit) - 1) / block)
            for
                wanted <- Kyo.foreach((first to last).toList)(warm(q, _))
                // Warm already means served: the mask is for a reader who has to wait.
                ready <- Kyo.foreach(wanted)(_.poll).map(_.forall(_.isDefined))
                _     <- loadingRef.set(!ready)
                parts <- Kyo.foreach(wanted)(_.get)
                total = Total.combine(parts.map(_._2))
                _ <- report(parts, first, total)
                _ <- rowsRef.set(slice(parts, first, d))
                _ <- totalRef.set(total)
                _ <- loadingRef.set(false)
                _ <- Kyo.foreach(neighbours(first, last, total))(warm(q, _))
            yield ()
            end for
        end serve

        /** The rows of the range, cut out of the blocks that cover it. */
        private def slice(parts: Seq[(Seq[A], Total)], first: Int, d: Demand): Seq[A] =
            val from = math.max(0, d.offset) - first * block
            parts.flatMap(_._1).slice(from, from + math.max(1, d.limit))

        /** The blocks to fetch ahead, clamped at the start and, where the size is known, at
          * the end: reaching past either would fetch a range that cannot exist.
          */
        private def neighbours(first: Int, last: Int, total: Total): List[Int] =
            val before = (math.max(0, first - ahead) until first).toList
            val after = total match
                case Total.Known(n) =>
                    val lastBlock = math.max(0, (n - 1) / block)
                    ((last + 1) to math.min(last + ahead, lastBlock)).toList
                case Total.Unknown(true)  => ((last + 1) to (last + ahead)).toList
                case Total.Unknown(false) => Nil
            before ++ after
        end neighbours

        /** A block that came back short where a full one was expected. It is not an error:
          * rows can be deleted between two fetches, and the rows on the screen are still
          * real rows. It does shift everything behind it by one, which is worth saying out
          * loud rather than leaving to be noticed.
          */
        private def report(parts: Seq[(Seq[A], Total)], first: Int, total: Total)(using Frame): Unit < Async =
            val lastBlock = total match
                case Total.Known(n)   => Present(math.max(0, (n - 1) / block))
                case Total.Unknown(_) => Absent
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
