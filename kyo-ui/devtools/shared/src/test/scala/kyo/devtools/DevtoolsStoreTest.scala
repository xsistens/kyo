package kyo.devtools

import kyo.*

/** The aggregation an overlay draws from.
  *
  * The store is the only part of the devtools with arithmetic in it, and every number it produces is a claim
  * about the app someone will act on: a badge that glows says "look here", a wasted count says "this work was
  * for nothing". So the arithmetic is asserted on exact values against a clock the test moves itself, not on
  * "roughly, after a sleep" — a rate estimator checked with a `sleep` proves only that it is not zero.
  */
class DevtoolsStoreTest extends kyo.test.Test[Any]:

    /** A clock the test winds by hand. */
    final private class TestClock:
        private var nanos: Long        = 0L
        val read: () => Long           = () => nanos
        def advance(d: Duration): Unit = nanos += d.toNanos
    end TestClock

    private def store(clock: TestClock, config: DevtoolsConfig = DevtoolsConfig.default): DevtoolsStore =
        new DevtoolsStore(config, clock.read)

    private def repaint(
        path: Seq[String] = Seq("0"),
        cause: UI.RenderCause = UI.RenderCause.Signal,
        wasted: Boolean = false,
        durationNanos: Long = 0L,
        bytes: Int = 0
    )(using Frame): UI.RenderEvent =
        UI.RenderEvent(path, Present("r0"), summon[Frame], UI.RenderKind.Repaint, cause, wasted, durationNanos, bytes)

    private def channel(name: String, path: Seq[String] = Seq("0"))(using Frame): UI.RenderEvent =
        UI.RenderEvent(
            path,
            Absent,
            summon[Frame],
            UI.RenderKind.Channel(name),
            UI.RenderCause.Signal,
            wasted = false,
            durationNanos = 0L,
            bytes = 0
        )

    private def listPatch(changed: Int, total: Int, path: Seq[String] = Seq("0"))(using Frame): UI.RenderEvent =
        UI.RenderEvent(
            path,
            Present("r0"),
            summon[Frame],
            UI.RenderKind.ListPatch(changed, total),
            UI.RenderCause.Signal,
            wasted = changed == 0,
            durationNanos = 0L,
            bytes = 0
        )

    "a cheap write is counted but does not make a region look hot" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(repaint())
        clock.advance(1.second)
        // Twenty attribute writes in one second. If these fed the rate the region would read as the hottest
        // thing on the page, and the reader would go optimize the one update kyo-ui already does optimally.
        (1 to 20).foreach(i => s.record(channel(s"a$i")))
        s.record(channel("title"))
        val region  = s.snapshot().regions.head
        val oneOnly = new DevtoolsStore(DevtoolsConfig.default, clock.read)
        oneOnly.record(repaint())
        assert(region.repaints == 1, s"repaints: ${region.repaints}")
        assert(region.channelWrites == 21, s"channel writes: ${region.channelWrites}")
        assert(
            region.rate <= oneOnly.snapshot().regions.head.rate,
            s"channel writes raised the rate to ${region.rate}"
        )
    }

    "the rate converges to the render frequency and decays once it stops" in {
        val clock = new TestClock
        val s     = store(clock)
        // Ten renders a second for twenty seconds — ten time constants, far past settling.
        (1 to 200).foreach { _ =>
            s.record(repaint())
            clock.advance(100.millis)
        }
        val settled = s.snapshot().regions.head.rate
        // Five time constants of silence: exp(-5) is under a hundredth of the settled value.
        clock.advance(10.seconds)
        val faded = s.snapshot().regions.head.rate
        assert(math.abs(settled - 10.0) < 0.5, s"expected ~10 renders/s, got $settled")
        assert(faded < 0.15, s"an idle region should fade, rate is still $faded")
        assert(s.snapshot().regions.head.peakRate >= settled, "peak should not be below the settled rate")
    }

    "a rebuild from above is not the same number as a region's own render" in {
        val clock = new TestClock
        val s     = store(clock)
        (1 to 5).foreach(_ => s.record(repaint(cause = UI.RenderCause.Signal)))
        (1 to 3).foreach(_ => s.record(repaint(cause = UI.RenderCause.Created)))
        val region = s.snapshot().regions.head
        assert(region.repaints == 8, s"repaints: ${region.repaints}")
        assert(region.created == 3, s"created: ${region.created}")
        assert(region.selfDriven == 5, s"self-driven: ${region.selfDriven}")
    }

    "wasted renders are reported as a share of the renders that could have been wasted" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(repaint(wasted = true))
        (1 to 3).foreach(_ => s.record(repaint()))
        // A channel write has no rendering to waste, so it must not dilute the share.
        s.record(channel("title"))
        val region = s.snapshot().regions.head
        assert(region.wasted == 1, s"wasted: ${region.wasted}")
        assert(region.wastedShare == 0.25, s"share: ${region.wastedShare}")
        assert(s.snapshot().wastedShare == 0.25, s"global share: ${s.snapshot().wastedShare}")
    }

    "the duration percentile is the nearest rank over the window" in {
        val clock = new TestClock
        val s     = store(clock, DevtoolsConfig.default.copy(durationSamples = 100))
        (1 to 100).foreach(n => s.record(repaint(durationNanos = n.toLong)))
        val region = s.snapshot().regions.head
        assert(region.p95Nanos == 95L, s"p95: ${region.p95Nanos}")
        assert(region.avgNanos == 50L, s"avg: ${region.avgNanos}")
    }

    "the window forgets: only the last samples count" in {
        val clock = new TestClock
        val s     = store(clock, DevtoolsConfig.default.copy(durationSamples = 4))
        // The first four are enormous and must be gone by the end; a window that kept them would make a
        // region that USED to be slow look slow forever, which is the opposite of a live tool.
        Seq(1000L, 1000L, 1000L, 1000L, 10L, 20L, 30L, 40L).foreach(d => s.record(repaint(durationNanos = d)))
        assert(s.snapshot().regions.head.avgNanos == 25L, s"avg: ${s.snapshot().regions.head.avgNanos}")
    }

    "the sparkline shows the silence, not the last shape frozen" in {
        val clock = new TestClock
        val s     = store(clock, DevtoolsConfig.default.copy(historySeconds = 8))
        (1 to 3).foreach(_ => s.record(repaint()))
        clock.advance(2.seconds)
        // Oldest bucket first, so the last entry is the second happening now. Two seconds of silence have to
        // read as two empty buckets, with the burst sitting where it happened.
        val history = s.snapshot().regions.head.history
        assert(history.size == 8, s"buckets: ${history.size}")
        assert(history(5) == 3, s"the burst moved: $history")
        assert(history(6) == 0 && history(7) == 0, s"silence was not cleared: $history")
    }

    "a keyed list contributes the rows it repainted" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(listPatch(changed = 1, total = 100))
        s.record(listPatch(changed = 0, total = 100))
        val region = s.snapshot().regions.head
        assert(region.rowsRepainted == 1L, s"rows repainted: ${region.rowsRepainted}")
        assert(region.rowsSeen == 200L, s"rows seen: ${region.rowsSeen}")
        assert(region.repaints == 2 && region.wasted == 1, s"$region")
    }

    "the region cap stops adding rather than recycling live regions" in {
        val clock = new TestClock
        val s     = store(clock, DevtoolsConfig.default.copy(maxRegions = 2))
        s.record(repaint(path = Seq("0")))
        s.record(repaint(path = Seq("1")))
        s.record(repaint(path = Seq("2")))
        // The two that got in keep counting: a cap that evicted would make their totals restart, and a
        // restarting counter in a tool whose whole subject is counters is worse than a missing one.
        s.record(repaint(path = Seq("0")))
        val snapshot = s.snapshot()
        assert(snapshot.regions.size == 2, s"tracked: ${snapshot.regions.map(_.key)}")
        assert(snapshot.overflowed, "the cap was hit but the snapshot does not say so")
        assert(
            snapshot.regions.find(_.key == "0").exists(_.repaints == 2),
            s"an admitted region stopped counting: ${snapshot.regions.map(r => (r.key, r.repaints))}"
        )
    }

    "a region is named after the source that built it, not its path" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(repaint(path = Seq("3", "1", "0")))
        val label = s.snapshot().regions.head.label
        assert(label.file == "DevtoolsStoreTest.scala", s"file: ${label.file}")
        assert(label.line > 0, s"line: ${label.line}")
        assert(label.display.startsWith("DevtoolsStoreTest"), s"display: ${label.display}")
    }

    "pausing holds what is collected and drops what arrives" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(repaint())
        s.pause()
        s.record(repaint())
        val held = s.snapshot()
        s.resume()
        s.record(repaint())
        assert(held.regions.head.repaints == 1, s"paused store kept recording: ${held.regions.head.repaints}")
        assert(!held.recording, "the snapshot does not report the pause")
        assert(s.snapshot().regions.head.repaints == 2, "resume did not resume")
    }

    "reset forgets everything" in {
        val clock = new TestClock
        val s     = store(clock)
        s.record(repaint())
        s.reset()
        assert(s.snapshot().regions.isEmpty, "reset left regions behind")
    }

end DevtoolsStoreTest
