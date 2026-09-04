package kyo.devtools

import java.util.concurrent.ConcurrentHashMap
import kyo.*
import scala.jdk.CollectionConverters.*

/** Turns the engine's stream of render events into the handful of numbers an overlay can draw.
  *
  * Everything expensive about a devtool is here rather than in the browser layer, for two reasons. It is the
  * part with real logic — a rate estimate, a percentile, a ring of buckets — and logic belongs where it can be
  * tested by asserting on numbers instead of by looking at a page. And it is the part that runs per event, on
  * the fiber that just did the render, where the budget is a map lookup and a few field writes; anything more
  * and the tool starts showing its own cost as the app's.
  *
  * '''Concurrency.''' In the browser there is one thread and none of this matters. Under the server-push
  * transport every region has its own fiber and they all report here, so the region table is concurrent and
  * each region's counters are guarded by that region's own monitor. Contention is nearly nil in practice: a
  * region is written by its own fiber, and the only other writer is a channel callback on the same node.
  */
final class DevtoolsStore(
    val config: DevtoolsConfig = DevtoolsConfig.default,
    // Injectable so the rate, the percentile and the sparkline can be asserted on exact numbers rather than
    // on "roughly, after a sleep". Everything here is a function of elapsed time, so a test that cannot move
    // the clock can only check that the numbers are not obviously wrong.
    // `java.lang.System` in full: `import kyo.*` brings kyo's own `System` into scope and shadows it.
    nanoTime: () => Long = () => java.lang.System.nanoTime()
):

    import DevtoolsStore.*

    private val regions = new ConcurrentHashMap[String, Region]

    @volatile private var overflowed = false
    @volatile private var recording  = true

    /** The sink to hand to [[kyo.UI.devtools]]:
      *
      * {{{
      * val devtools = DevtoolsStore()
      * UI.devtools(devtools.record)(UI.runMount(shell, "#app"))
      * }}}
      */
    val record: UI.RenderEvent => Unit = event =>
        if recording then
            val key = event.path.mkString(".")
            val region = regions.get(key) match
                case null =>
                    // Beyond the cap new regions are ignored rather than evicted. A page whose tree grows
                    // without bound is exactly what this tool is for finding, and answering it by quietly
                    // recycling entries would make the counters restart under the reader's eyes.
                    if regions.size >= config.maxRegions then
                        overflowed = true
                        null
                    else
                        val fresh = new Region(key, event.path, Label.of(event.frame))
                        val prior = regions.putIfAbsent(key, fresh)
                        if prior eq null then fresh else prior
                    end if
                case existing => existing
            if region ne null then region.add(event)
        end if

    /** Everything the overlay draws, as of now. Ordering is by rate, hottest first, which is the order the
      * "top offenders" list wants and the order a badge budget should be spent in.
      */
    def snapshot(): Snapshot =
        val now  = nanoTime()
        val rows = Chunk.from(regions.values.asScala.map(_.snapshot(now)).toSeq.sortBy(-_.rate))
        Snapshot(rows, overflowed, recording)
    end snapshot

    /** Forget everything. The regions themselves are dropped, not zeroed: one that is still live re-appears on
      * its next event, one that is gone stays gone, and neither needs a liveness check to say so.
      */
    def reset(): Unit =
        regions.clear()
        overflowed = false

    /** Stop recording without losing what is already collected — for holding a page still while reading it. */
    def pause(): Unit = recording = false

    def resume(): Unit = recording = true

    def isRecording: Boolean = recording

    private val tauSeconds = config.rateTau.toNanos / 1e9

    /** One region's running totals. Every field is guarded by this object's monitor. */
    final private class Region(key: String, path: Seq[String], label: Label):
        private var regionId: Maybe[String] = Absent
        private var repaints                = 0
        private var created                 = 0
        private var wasted                  = 0
        private var channelWrites           = 0
        private var textWrites              = 0
        private var rowsRepainted           = 0L
        private var rowsSeen                = 0L
        private var bytes                   = 0L
        private var rate                    = 0.0
        private var peakRate                = 0.0
        private var lastRepaintNanos        = nanoTime()

        // Last N render durations, oldest overwritten. Exact over its window, and small enough that the
        // percentile can be taken by sorting a copy when someone opens the panel — which beats an approximate
        // digest nobody can check by hand.
        private val durations     = new Array[Long](config.durationSamples)
        private var durationCount = 0
        private var durationNext  = 0

        // One bucket per second of the sparkline, indexed by absolute second modulo its length. Buckets
        // between the last write and now are cleared on the way past, so a region that went quiet shows the
        // gap rather than its old shape frozen.
        private val history       = new Array[Int](config.historySeconds)
        private var historySecond = nanoTime() / 1000000000L

        def add(event: UI.RenderEvent): Unit = synchronized {
            val now = nanoTime()
            if event.regionId.nonEmpty then regionId = event.regionId
            bytes += event.bytes
            event.kind match
                case UI.RenderKind.Repaint =>
                    countRender(event, now)
                case UI.RenderKind.ListPatch(changed, total) =>
                    countRender(event, now)
                    rowsRepainted += changed
                    rowsSeen += total
                // The two cheap kinds are counted but deliberately kept OUT of the rate and the sparkline.
                // Those two drive how loudly a region is drawn, and an attribute write is the one update in
                // this engine that is already as cheap as it can be: making it glow would send every reader
                // to optimize the thing that needs it least.
                case _: UI.RenderKind.Channel => channelWrites += 1
                case UI.RenderKind.Text       => textWrites += 1
            end match
        }

        private def countRender(event: UI.RenderEvent, now: Long): Unit =
            repaints += 1
            if event.cause == UI.RenderCause.Created then created += 1
            if event.wasted then wasted += 1
            if event.durationNanos > 0L then
                durations(durationNext) = event.durationNanos
                durationNext = (durationNext + 1) % durations.length
                if durationCount < durations.length then durationCount += 1
            end if
            advanceHistory(now)
            history((historySecond % history.length).toInt) += 1
            // Exponentially weighted event rate, the estimator a load average uses: the running value decays
            // by exp(-dt/tau) since the last event and each event adds 1/tau. A steady r renders per second
            // converges to r, a burst shows up while it is happening, and a region that stops fades on its
            // own — which is what lets the overlay be quiet by default without anyone clearing anything.
            rate = decayedRate(now) + 1.0 / tauSeconds
            lastRepaintNanos = now
            if rate > peakRate then peakRate = rate
        end countRender

        private def decayedRate(now: Long): Double =
            val elapsed = math.max(0L, now - lastRepaintNanos) / 1e9
            if elapsed <= 0.0 then rate else rate * math.exp(-elapsed / tauSeconds)

        private def advanceHistory(now: Long): Unit =
            val second = now / 1000000000L
            if second != historySecond then
                val length = history.length
                val gap    = math.min(second - historySecond, length.toLong).toInt
                var i      = 1
                while i <= gap do
                    history(((historySecond + i) % length).toInt) = 0
                    i += 1
                historySecond = second
            end if
        end advanceHistory

        def snapshot(now: Long): RegionSnapshot = synchronized {
            advanceHistory(now)
            val samples = java.util.Arrays.copyOf(durations, durationCount)
            java.util.Arrays.sort(samples)
            var total = 0L
            var i     = 0
            while i < durationCount do
                total += samples(i)
                i += 1
            val length  = history.length
            val buckets = new Array[Int](length)
            i = 0
            while i < length do
                buckets(i) = history(((historySecond + 1 + i) % length).toInt)
                i += 1
            RegionSnapshot(
                key,
                path,
                regionId,
                label,
                repaints,
                created,
                wasted,
                channelWrites,
                textWrites,
                rowsRepainted,
                rowsSeen,
                bytes,
                decayedRate(now),
                peakRate,
                math.max(0L, now - lastRepaintNanos) / 1e9,
                if durationCount == 0 then 0L else total / durationCount,
                // Nearest-rank p95: the smallest sample at or above 95% of the window, which is the reading a
                // reader expects and the one they can check by hand against a short list.
                if durationCount == 0 then 0L
                else samples(math.max(0, math.min(durationCount - 1, math.ceil(0.95 * durationCount).toInt - 1))),
                Chunk.from(buckets.toSeq)
            )
        }
    end Region

end DevtoolsStore

object DevtoolsStore:

    /** Where a region was mounted from, out of the `Frame` its AST node carries.
      *
      * '''Not the region's name, and not its own source line.''' That is what one reaches for first, and it
      * does not survive contact with idiomatic kyo: a component written `def view(using Frame): UI` hands its
      * own frame to every node it builds, because that is what a `using` parameter does. In an app that
      * threads `Frame` from the entry point — which the kyo-apollo spotify showcase does, and which is normal
      * style — every region in the tree reports the same file and line, the entry point's.
      *
      * A fresh position per node cannot be recovered here either: the AST nodes are constructed inside
      * package `kyo`, where the `Frame` macro deliberately refuses to expand so that frames point at user
      * code. The `using Frame` at the API boundary IS that mechanism, and inheriting is its normal outcome.
      *
      * So this is reported as the mount site, honestly labelled, and the NAME a reader sees comes from the
      * DOM instead — the tag the region painted plus its most telling class, which for a kyo-uic component is
      * that component's own `p-<name>` class. See `labelOf` in [[DevtoolsJs]]. Where a region has nothing
      * painted, `display` is what is left, which is why it is still carried.
      *
      * @param display
      *   `Class.method` where both are known, else the file's base name
      * @param callee
      *   the function that built the node — `foreachKeyed`, `render`, `mounted` — which says WHAT kind of
      *   region this is in the author's own vocabulary
      */
    final case class Label(display: String, file: String, line: Int, callee: String, snippet: String)

    object Label:
        private val engine = Label("‹engine›", "<internal>", 0, "", "")

        def of(frame: Frame): Label =
            val position = frame.position
            val file     = position.fileName
            if file == "<internal>" then engine
            else
                val caller = frame.callerName
                val owner  = simpleName(frame.className)
                val display =
                    if owner.isEmpty && caller.isEmpty then file.stripSuffix(".scala")
                    else if owner.isEmpty then caller
                    else if caller.isEmpty then owner
                    else s"$owner.$caller"
                Label(display, file, position.lineNumber, frame.calleeName, frame.snippetShort)
            end if
        end of

        /** Last segment of a JVM-ish class name, with the object marker dropped: `spotify.TrackList$` reads as
          * `TrackList`, which is what the author wrote and what a reader is looking for.
          */
        private def simpleName(className: String): String =
            val trimmed = className.stripSuffix("$").replace('$', '.')
            val dot     = trimmed.lastIndexOf('.')
            if dot < 0 then trimmed else trimmed.substring(dot + 1)
        end simpleName
    end Label

    /** One region, as of one instant. */
    final case class RegionSnapshot(
        key: String,
        path: Seq[String],
        regionId: Maybe[String],
        label: Label,
        repaints: Int,
        created: Int,
        wasted: Int,
        channelWrites: Int,
        textWrites: Int,
        rowsRepainted: Long,
        rowsSeen: Long,
        bytes: Long,
        rate: Double,
        peakRate: Double,
        idleSeconds: Double,
        avgNanos: Long,
        p95Nanos: Long,
        history: Chunk[Int]
    ):
        /** Renders that were for nothing, as a share of the renders that could have been. Only repaints count:
          * a channel or text write is a single assignment with no rendering to waste.
          */
        def wastedShare: Double = if repaints == 0 then 0.0 else wasted.toDouble / repaints

        /** Renders this region did because its own signal fired, as opposed to being rebuilt from above. */
        def selfDriven: Int = repaints - created
    end RegionSnapshot

    final case class Snapshot(regions: Chunk[RegionSnapshot], overflowed: Boolean, recording: Boolean):
        def totalRate: Double = regions.foldLeft(0.0)(_ + _.rate)

        def totalRepaints: Int = regions.foldLeft(0)(_ + _.repaints)

        def totalWasted: Int = regions.foldLeft(0)(_ + _.wasted)

        def wastedShare: Double =
            val total = totalRepaints
            if total == 0 then 0.0 else totalWasted.toDouble / total

        /** Renders per second across the whole page, oldest bucket first — the band that answers "it stuttered
          * a moment ago, what was that".
          *
          * Summed from the per-region rings rather than kept as its own counter: a store-level bucket would be
          * written by every region fiber on every render, which is the one place in this design where real
          * contention could appear, and it would buy nothing — the regions already hold the numbers, and
          * adding them up once per snapshot is a few hundred microseconds at the cap.
          */
        lazy val history: Chunk[Int] =
            regions.headOption match
                case None => Chunk.empty
                case Some(first) =>
                    val totals = new Array[Int](first.history.size)
                    regions.foreach { region =>
                        var i = 0
                        while i < totals.length && i < region.history.size do
                            totals(i) += region.history(i)
                            i += 1
                    }
                    Chunk.from(totals.toSeq)
    end Snapshot

end DevtoolsStore
