package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*
import kyo.UI.foreachKeyed
import kyo.UI.render
import kyo.internal.RecordingExchange.Op

/** Direct-engine tests for the reusable keyed Foreach path (RowRegistry): rows whose key and item persist
  * across list emissions are neither re-rendered nor re-subscribed, their observers stay live across reorders,
  * and a removed or changed row's observers are torn down before the new value paints.
  */
class ForeachReuseTest extends kyo.test.Test[Any]:

    final private class Recording:
        // A transport that addresses rows, so the rows it is not told to repaint keep their subscriptions.
        val exchange          = new RecordingExchange(keepsRows = true)
        def changes: Int      = exchange.recorded.count { case _: Op.Paint | _: Op.List => true; case _ => false }
        def classPatches: Int = exchange.recorded.count(_.isInstanceOf[Op.Class])
        def trueClassOns: Int = exchange.recorded.count { case op: Op.Class => op.on; case _ => false }
    end Recording

    /** Waits until `n` observers wait on `ref`. An observer parks on its signal again only once it has handled the value before,
      * so this is the point where everything it does for that value is done: a list region has adopted or patched its rows.
      */
    private def parked(ref: Signal.SignalRef[?], n: Int)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(ref.waiters.map(_ == n))

    "appending re-renders only the added row" in {
        Scope.run {
            for
                renders <- Sync.defer(new AtomicInteger(0))
                rows    <- Signal.initRef(Chunk("a", "b", "c"))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    discard(renders.incrementAndGet())
                    UI.li(item)
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                base = renders.get
                _ <- rows.getAndUpdate(_.append("d"))
                _ <- assertEventually(Sync.defer(rec.changes >= 1))
                delta = renders.get - base
            yield assert(delta == 1, s"expected exactly 1 render for the added row, got $delta")
        }
    }

    "reordering re-renders no rows and keeps row observers alive" in {
        Scope.run {
            for
                renders  <- Sync.defer(new AtomicInteger(0))
                rows     <- Signal.initRef(Chunk("a", "b", "c"))
                selected <- Signal.initRef("")
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    discard(renders.incrementAndGet())
                    UI.li(item).when(selected.map(_ == item))(_.cssClass("sel"))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                base = renders.get
                _ <- rows.getAndUpdate(c => Chunk.from(c.toSeq.reverse))
                _ <- assertEventually(Sync.defer(rec.changes >= 1))
                reorderDelta = renders.get - base
                // The retained rows' channel observers must still be live after the reorder.
                _ <- selected.set("b")
                _ <- assertEventually(Sync.defer(rec.classPatches >= 1))
            yield assert(reorderDelta == 0, s"expected no row re-renders on reorder, got $reorderDelta")
        }
    }

    "removing a row tears its observers down while others stay live" in {
        Scope.run {
            for
                rows     <- Signal.initRef(Chunk("a", "b", "c"))
                selected <- Signal.initRef("")
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    UI.li(item).when(selected.map(_ == item))(_.cssClass("sel"))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                _    <- parked(selected, 3)
                _    <- rows.getAndUpdate(_.filter(_ != "b"))
                _    <- assertEventually(Sync.defer(rec.changes >= 1))
                // b's observer is gone: only a's and c's still wait on `selected`. A live one would keep waiting there.
                _ <- parked(selected, 2)
                // The surviving rows' mapped signals re-emit their unchanged `false` on the source change (mapped observe
                // re-emits equal images for distinct source values), so the discriminator is the patched value: only a live
                // b observer could patch `on = true` for selected == "b". Both survivors have handled it once they wait again.
                _ <- selected.set("b")
                _ <- parked(selected, 2)
                deadTrueOns = rec.trueClassOns
                // A surviving row still patches true in place.
                _ <- selected.set("a")
                _ <- assertEventually(Sync.defer(rec.trueClassOns >= deadTrueOns + 1))
            yield assert(deadTrueOns == 0, s"expected no on=true patch from the removed row's observer, got $deadTrueOns")
        }
    }

    "a freshly added row's root channel patches" in {
        // The added row is built by the reuse path (not the initial walk): its root element carries the
        // channel, so the row walk must promote it to a ReactiveUI node or the observer never starts.
        Scope.run {
            for
                rows     <- Signal.initRef(Chunk("a", "b"))
                selected <- Signal.initRef("")
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    UI.li(item).when(selected.map(_ == item))(_.cssClass("sel"))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                _    <- rows.getAndUpdate(_.append("c"))
                _    <- assertEventually(Sync.defer(rec.changes >= 1))
                _    <- selected.set("c")
                _    <- assertEventually(Sync.defer(rec.trueClassOns >= 1))
            yield assert(true)
        }
    }

    "event dispatch on live rows does not re-render the list" in {
        // Registry-first dispatch: an event resolves the target row from the live RowRegistry (cached
        // handler for rows built by an emission, on-demand single-row walk for seeded rows) instead of
        // re-rendering and re-walking the whole list per event. Each of the two clicks costs one render, the
        // single resolution its click-target questions are answered from. A whole-list dispatch walk would
        // blow the bound (11 > 2), and a walk per question shows up as 4 > 2.
        Scope.run {
            for
                renders <- Sync.defer(new AtomicInteger(0))
                clicks  <- Sync.defer(new AtomicInteger(0))
                rows    <- Signal.initRef(Chunk.from((0 until 10).map(i => s"r$i")))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    discard(renders.incrementAndGet())
                    UI.li(item).onClick(Sync.defer(discard(clicks.incrementAndGet())))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                // One emission so "r10" carries a cached handler; r0-r9 stay seeded (no cached handler).
                _ <- rows.getAndUpdate(_.append("r10"))
                // The registry is updated after the paint notification; the region waits on the list again once it has.
                _ <- parked(rows, 1)
                base = renders.get
                _ <- root.handle(Seq("0", "r10"), UIEvent.Click(Seq("0", "r10"), MouseEventData(UI.Modifiers.none, Absent)))
                _ <- root.handle(Seq("0", "r5"), UIEvent.Click(Seq("0", "r5"), MouseEventData(UI.Modifiers.none, Absent)))
                _ <- assertEventually(Sync.defer(clicks.get >= 2))
                delta = renders.get - base
            yield assert(delta <= 2, s"expected at most 2 target-resolution renders from event dispatch, got $delta")
        }
    }

    "removing a row tears down a nested region inside it" in {
        // A row's lifetime is a bare Finalizer, not a fiber. A nested region forks through `Fiber.init`, which
        // is `Scope.acquireRelease(...)(_.interrupt)`, so its interrupt is registered ON that finalizer and
        // closing the row must stop it. Nothing else would: the row has no fiber to interrupt.
        Scope.run {
            for
                inner <- Signal.initRef("x")
                rows  <- Signal.initRef(Chunk("a", "b", "c"))
                aHits <- Sync.defer(new AtomicInteger(0))
                bHits <- Sync.defer(new AtomicInteger(0))
                exchange = new RecordingExchange(beforePaint =
                    (path, _) =>
                        Sync.defer {
                            if path.contains("a") then discard(aHits.incrementAndGet())
                            if path.contains("b") then discard(bHits.incrementAndGet())
                        }
                )
                ui = UI.ul(rows.foreachKeyed(identity) { item =>
                    UI.li(inner.map(v => UI.span(s"$item-$v")))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, exchange)
                _    <- parked(rows, 1)
                _    <- parked(inner, 3)
                _    <- rows.getAndUpdate(_.filter(_ != "b"))
                _    <- parked(rows, 1)
                // b's nested region is stopped: only a's and c's still wait on `inner`. A live one would keep waiting there.
                _ <- parked(inner, 2)
                _ <- Sync.defer { aHits.set(0); bHits.set(0) }
                _ <- inner.set("y")
                // A surviving row's nested region repaints, which is what proves the emission was delivered
                // at all before the removed row's silence is read as evidence; both survivors have handled it once they wait again.
                _ <- assertEventually(Sync.defer(aHits.get >= 1))
                _ <- parked(inner, 2)
            yield assert(bHits.get == 0, s"expected the removed row's nested region to be dead, got ${bHits.get} repaints")
        }
    }

    "a changed item rebuilds only that row" in {
        Scope.run {
            for
                renders <- Sync.defer(new AtomicInteger(0))
                rows    <- Signal.initRef(Chunk("a:1", "b:1", "c:1"))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(_.takeWhile(_ != ':')) { item =>
                    discard(renders.incrementAndGet())
                    UI.li(item)
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                base = renders.get
                _ <- rows.getAndUpdate(c => Chunk.from(c.toSeq.map(s => if s.startsWith("b") then "b:2" else s)))
                _ <- assertEventually(Sync.defer(rec.changes >= 1))
                delta = renders.get - base
            yield assert(delta == 1, s"expected exactly 1 render for the changed row, got $delta")
        }
    }

    "keyed mounted nodes inside retained rows survive an emission, a removed row's is released" in {
        // The emission's mount-key sets are what keep a retained row's mounted instance claimed: a wrong
        // (empty) keep set would evict every instance, and a retained row is never re-walked, so nothing
        // would bring it back.
        def component(key: String, runs: AtomicInteger, released: AtomicInteger): UI < (Async & Scope) =
            for
                _ <- Sync.defer(discard(runs.incrementAndGet()))
                _ <- Scope.ensure(Sync.defer(discard(released.incrementAndGet())))
            yield UI.span(s"instance:$key")
        Scope.run {
            for
                runs     <- Sync.defer(new AtomicInteger(0))
                released <- Sync.defer(new AtomicInteger(0))
                rows     <- Signal.initRef(Chunk("a", "b", "c"))
                rec = new Recording
                ui  = UI.ul(rows.foreachKeyed(identity) { item =>
                    UI.li(UI.mounted(component(item, runs, released)).keyed(item))
                })
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- assertEventually(Sync.defer(runs.get == 3))
                _    <- parked(rows, 1)
                _    <- rows.getAndUpdate(_.append("d"))
                _    <- assertEventually(Sync.defer(runs.get == 4))
                // The emission is through, and with it every eviction it makes, once the region waits on the list again.
                _ <- parked(rows, 1)
                afterAppend = released.get
                _ <- rows.getAndUpdate(_.filter(_ != "b"))
                _ <- assertEventually(Sync.defer(released.get == afterAppend + 1))
                _ <- parked(rows, 1)
            yield
                assert(afterAppend == 0, s"expected no instance released by appending a row, got $afterAppend")
                assert(runs.get == 4, s"expected the retained rows' instances to be kept, got ${runs.get} runs")
        }
    }

    /** A keyed table of `size` rows, each with a region over `tick` and, with `mounts`, a keyed mount, inside a region over `show` next to
      * a sibling region over `tick` and a keyless mount; one more region over `tick` lies outside. The list empties while the region
      * around it closes. The sibling mount's release waits until the list has begun to release its first row, and it runs before the
      * list's own release, so the close interrupts the list's fiber while that fiber is releasing its rows.
      */
    private def hideWhileEmptying(size: Int, mounts: Boolean)(using Frame, kyo.test.AssertScope) =
        def card(starts: AtomicInteger, releases: AtomicInteger): UI < (Async & Scope) =
            Sync.defer(discard(starts.incrementAndGet()))
                .andThen(Scope.ensure(Sync.defer(discard(releases.incrementAndGet()))))
                .andThen(UI.span("card"))
        Scope.run {
            for
                starts   <- Sync.defer(new AtomicInteger(0))
                releases <- Sync.defer(new AtomicInteger(0))
                armed    <- Sync.defer(new AtomicInteger(0))
                evicting <- Latch.init(1)
                show     <- Signal.initRef(true)
                rows     <- Signal.initRef(Chunk.from((1 to size).map(i => s"r$i")))
                tick     <- Signal.initRef(0)
                rec = new Recording
                // Both effects register their release before they count themselves armed.
                firstRow = UI.mounted(Scope.ensure(evicting.release).andThen(Sync.defer(discard(armed.incrementAndGet()))).andThen(
                    UI.span("first")
                ))
                sibling = UI.mounted(Scope.ensure(evicting.await).andThen(Sync.defer(discard(armed.incrementAndGet()))).andThen(
                    UI.span("sibling")
                ))
                ui = UI.div(
                    tick.render(t => UI.span(s"outside:$t")),
                    show.render(on =>
                        if on then
                            UI.div(
                                UI.table(UI.tbody(rows.foreachKeyed(identity)(item =>
                                    UI.tr(
                                        UI.td(tick.render(t => UI.span(s"$item:$t"))),
                                        if mounts then UI.td(UI.mounted(card(starts, releases)).keyed("card")) else UI.td("-"),
                                        if item == "r1" then UI.td(firstRow) else UI.td("-")
                                    )
                                ))),
                                tick.render(t => UI.span(s"sibling:$t")),
                                sibling
                            )
                        else UI.span("off")
                    )
                )
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- parked(rows, 1)
                _    <- parked(show, 1)
                _    <- parked(tick, size + 2)
                _    <- assertEventually(Sync.defer(armed.get == 2 && (!mounts || starts.get == size)))
                _    <- show.set(false)
                _    <- rows.set(Chunk.empty)
                // The region waits on `show` again once the value before has closed, the list and every row in it included: then only
                // the region outside observes `tick`, and no mount is registered. A row never closed keeps a region waiting there.
                _          <- parked(show, 1)
                observing  <- tick.waiters
                registered <- root.mountDispatch.get.registered
                // A keyed instance is released by its table, which settles on a fiber of its own.
                _ <- assertEventually(Sync.defer(releases.get == starts.get))
            yield assert(
                observing == 1 && registered == 0,
                s"regions observing tick: $observing (expected 1), mount instances registered: $registered (expected 0)"
            )
            end for
        }
    end hideWhileEmptying

    "hiding the region around a keyed list that empties at once closes every row" in {
        hideWhileEmptying(size = 20, mounts = false)
    }

    "hiding the region around a keyed list that empties at once releases the keyed mount of every row" in {
        hideWhileEmptying(size = 20, mounts = true)
    }

    // A release a row's finalizer runs on the evicting fiber is a step that an interrupt of that fiber would abandon halfway: the
    // finalizer hands only the steps not yet started to a fiber of their own. The eviction runs where no interrupt reaches it, and
    // closing every row, as the scope around the list does after interrupting its fiber, waits for that eviction to finish.
    "a row release interrupted halfway still completes, and closing every row waits for it" in {
        Scope.run {
            for
                entered  <- Latch.init(1)
                gate     <- Latch.init(1)
                done     <- AtomicBoolean.init(false)
                registry <- Sync.Unsafe.defer(RowRegistry.initNow())
                scope    <- Sync.Unsafe.defer(new RowScope(Scope.Finalizer.Unsafe.initInline(), ReleaseGroup.root()))
                _        <- scope.finalizer.ensure(_ => entered.release.andThen(gate.await).andThen(done.set(true)))
                row = new RowInstance("a", "a", UI.empty, Seq.empty, Absent, scope, holdsMount = false)
                _       <- registry.replaceAll(Seq(row), Seq(row))
                evicter <- Fiber.initUnscoped(registry.evictExcept(_ => false))
                _       <- entered.await
                _       <- evicter.interrupt
                _       <- evicter.getResult
                closer  <- Fiber.initUnscoped(registry.evictAll)
                _       <- gate.release
                _       <- closer.get
                // `closer` returns only once the release it found running has finished.
                finished <- done.get
            yield assert(finished, "the interrupted release was abandoned, or closing every row did not wait for it")
            end for
        }
    }

end ForeachReuseTest
