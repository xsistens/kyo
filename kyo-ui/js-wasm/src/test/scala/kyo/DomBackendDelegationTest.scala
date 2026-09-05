package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js as scalajs

/** Tests for the SPA event-delegation forwarding gate and its mount-scope lifecycle.
  *
  * These run against a real jsdom document (see [[DomTestEnv]]), so the traversal is exercised through the actual DOM
  * API: `createElement`, attribute reads, `parentNode` links, and the `instanceof Element` check in the walk.
  */
class DomBackendDelegationTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def el(parent: dom.Node, ev: String = null): dom.Element =
        val e = dom.document.createElement("div")
        if ev != null then e.setAttribute("data-kyo-ev", ev)
        if parent != null then discard(parent.appendChild(e))
        e
    end el

    final private case class ListenerCall(eventType: String, listener: scalajs.Any, options: scalajs.Any)

    final private class ListenerTracker(
        failOnAdd: String = null,
        failOnDocumentAdd: String = null,
        chronology: ArrayBuffer[String] = ArrayBuffer.empty
    ):
        val added           = ArrayBuffer.empty[ListenerCall]
        val removed         = ArrayBuffer.empty[ListenerCall]
        val documentAdded   = ArrayBuffer.empty[ListenerCall]
        val documentRemoved = ArrayBuffer.empty[ListenerCall]
        val attempts        = ArrayBuffer.empty[String]
        var activeTimers    = 0

        private val body                   = dom.document.body.asInstanceOf[scalajs.Dynamic]
        private val document               = dom.document.asInstanceOf[scalajs.Dynamic]
        private val window                 = dom.window.asInstanceOf[scalajs.Dynamic]
        private val originalAdd            = body.addEventListener
        private val originalRemove         = body.removeEventListener
        private val originalDocumentAdd    = document.addEventListener
        private val originalDocumentRemove = document.removeEventListener
        private val originalSetInterval    = window.setInterval
        private val originalClearInterval  = window.clearInterval

        def install(): ListenerTracker =
            body.updateDynamic("addEventListener")((eventType: String, listener: scalajs.Any, options: scalajs.Any) =>
                attempts += eventType
                if eventType == failOnAdd then throw new scalajs.JavaScriptException(s"failed add: $eventType")
                else
                    discard(originalAdd.call(body, eventType, listener, options))
                    added += ListenerCall(eventType, listener, options)
                end if
            )
            body.updateDynamic("removeEventListener")((eventType: String, listener: scalajs.Any, options: scalajs.Any) =>
                discard(originalRemove.call(body, eventType, listener, options))
                removed += ListenerCall(eventType, listener, options)
                chronology += s"remove:$eventType"
            )
            document.updateDynamic("addEventListener")((eventType: String, listener: scalajs.Any, options: scalajs.Any) =>
                if eventType == failOnDocumentAdd then throw new scalajs.JavaScriptException(s"failed document add: $eventType")
                else
                    discard(originalDocumentAdd.call(document, eventType, listener, options))
                    documentAdded += ListenerCall(eventType, listener, options)
            )
            document.updateDynamic("removeEventListener")((eventType: String, listener: scalajs.Any, options: scalajs.Any) =>
                discard(originalDocumentRemove.call(document, eventType, listener, options))
                documentRemoved += ListenerCall(eventType, listener, options)
                chronology += s"remove-document:$eventType"
            )
            window.updateDynamic("setInterval")((callback: scalajs.Any, millis: scalajs.Any) =>
                activeTimers += 1
                originalSetInterval.call(window, callback, millis)
            )
            window.updateDynamic("clearInterval")((id: scalajs.Any) =>
                discard(originalClearInterval.call(window, id))
                activeTimers -= 1
                chronology += "clear-interval"
            )
            this
        end install

        def restore(): Unit =
            body.updateDynamic("addEventListener")(originalAdd)
            body.updateDynamic("removeEventListener")(originalRemove)
            document.updateDynamic("addEventListener")(originalDocumentAdd)
            document.updateDynamic("removeEventListener")(originalDocumentRemove)
            window.updateDynamic("setInterval")(originalSetInterval)
            window.updateDynamic("clearInterval")(originalClearInterval)
        end restore
    end ListenerTracker

    private def sameCall(left: ListenerCall, right: ListenerCall): Boolean =
        left.eventType == right.eventType &&
            scalajs.special.strictEquals(left.listener, right.listener) &&
            scalajs.special.strictEquals(left.options, right.options)

    private def captureTrue(call: ListenerCall): Boolean =
        scalajs.typeOf(call.options) == "boolean" && call.options.asInstanceOf[Boolean]

    private def wheelOptions(call: ListenerCall): Boolean =
        scalajs.typeOf(call.options) == "object" &&
            call.options.asInstanceOf[scalajs.Dynamic].capture.asInstanceOf[Boolean] &&
            !call.options.asInstanceOf[scalajs.Dynamic].passive.asInstanceOf[Boolean]

    private def scrollOptions(call: ListenerCall): Boolean =
        scalajs.typeOf(call.options) == "object" &&
            call.options.asInstanceOf[scalajs.Dynamic].capture.asInstanceOf[Boolean] &&
            call.options.asInstanceOf[scalajs.Dynamic].passive.asInstanceOf[Boolean]

    /** The two listeners registered with an options object rather than a bare `capture` boolean. */
    private val optionObjectTypes = Set("wheel", "scroll")

    /** [[DomTestEnv.MountReady]]'s installation barrier plus a transcript of the teardown hooks.
      *
      * The lifecycle tests below wait on `installed`, inherited from `MountReady`, rather than on a listener COUNT
      * (`added.size == 28`) as they used to. A count is not a completion signal — it is a second copy of the
      * registration list, and it went stale three times in six weeks (`contextmenu`, the pointer listeners, the
      * input-masking listeners). Each time, four leaves turned into two-minute timeouts that read like flakiness
      * rather than drift (GAPS.md F-35). `MountReady` already existed for exactly this; keeping a second copy of
      * the signal here would have been the same mistake one size down.
      */
    final private class LifecycleChronology(events: ArrayBuffer[String] = ArrayBuffer.empty) extends DomTestEnv.MountReady:
        override def channelClosed(): Unit     = events += "channel-close"
        override def drainInterrupting(): Unit = events += "drain-interrupt"
        override def drainJoined(): Unit       = events += "drain-joined"
    end LifecycleChronology

    /** Budget for a signal the mount reaches in milliseconds when it reaches it at all. */
    private val installBudget = 10.seconds

    private def mountAndStop(tracker: ListenerTracker)(using Frame, kyo.test.AssertScope): Unit < Async =
        val diagnostics = new LifecycleChronology()
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div("mounted"), diagnostics)))
            _     <- assertEventually(Sync.defer(diagnostics.installed), installBudget)
            _     <- fiber.interrupt
            _     <- fiber.getResult
            _     <- assertEventually(Sync.defer(tracker.removed.size == tracker.added.size), installBudget)
        yield ()
        end for
    end mountAndStop

    "forwards when the target itself declares the type" in {
        val target = el(dom.document.body, ev = "keydown")
        assert(DomBackend.declaredInChain(target, "keydown"))
    }

    "forwards when only an ancestor declares the type" in {
        // The regression this PR fixes: a keydown declared on an ancestor panel must be forwarded even
        // though the event target carries no data-kyo-ev of its own.
        val ancestor = el(dom.document.body, ev = "keydown")
        val mid      = el(ancestor)
        val target   = el(mid)
        assert(DomBackend.declaredInChain(target, "keydown"))
    }

    "does not forward when no element in the chain declares the type" in {
        val ancestor = el(dom.document.body, ev = "click")
        val target   = el(ancestor)
        assert(!DomBackend.declaredInChain(target, "keydown"))
    }

    "matches individual entries of a comma-separated declaration" in {
        val target = el(dom.document.body, ev = "click,keydown")
        assert(DomBackend.declaredInChain(target, "keydown"))
        assert(DomBackend.declaredInChain(target, "click"))
        assert(!DomBackend.declaredInChain(target, "key"))
    }

    "does not consult a declaration on document.body itself" in {
        dom.document.body.setAttribute("data-kyo-ev", "click")
        val target = el(dom.document.body)
        val result = DomBackend.declaredInChain(target, "click")
        dom.document.body.removeAttribute("data-kyo-ev")
        assert(!result)
    }

    "stops at a non-element parent without crashing" in {
        // documentElement's parent is the document node, which is not an Element, so the walk must end there.
        assert(!DomBackend.declaredInChain(dom.document.documentElement, "click"))
    }

    "returns false for a detached element" in {
        val target = el(null, ev = null)
        assert(!DomBackend.declaredInChain(target, "click"))
    }

    "removes every delegated body listener when the mount scope closes" in {
        Scope.acquireRelease(Sync.defer(new ListenerTracker().install()))(tracker => Sync.defer(tracker.restore())).map { tracker =>
            mountAndStop(tracker).map { _ =>
                // In registration order, grouped by the installer that owns each group. This is the ONE place a new
                // delegated event has to be declared; every other test below derives from what was actually added, so
                // adding a listener without touching this list fails here with a readable diff and nowhere else.
                val expected = Seq(
                    // setupEventDelegation
                    "click",
                    "contextmenu",
                    "input",
                    "change",
                    "submit",
                    "keydown",
                    "keyup",
                    "focus",
                    "blur",
                    "mouseover",
                    "mouseout",
                    "wheel",
                    "scroll",
                    // setupPointerDelegation
                    "pointerdown",
                    "pointermove",
                    "pointerup",
                    // setupInputMasking
                    "beforeinput",
                    "compositionend",
                    // DomDragRuntime.install
                    "dragstart",
                    "dragenter",
                    "dragover",
                    "dragleave",
                    "drop",
                    "dragend",
                    "pointerdown",
                    "pointermove",
                    "pointerup",
                    "pointercancel",
                    "touchstart",
                    "touchmove",
                    "touchend",
                    "touchcancel",
                    "keydown"
                )
                assert(tracker.added.map(_.eventType) == expected)
                val plain = tracker.added.filterNot(call => optionObjectTypes.contains(call.eventType))
                assert(plain.size == expected.count(t => !optionObjectTypes.contains(t)))
                assert(plain.forall(captureTrue))
                val wheel = tracker.added.filter(_.eventType == "wheel")
                assert(wheel.size == 1)
                assert(wheel.forall(wheelOptions))
                val scroll = tracker.added.filter(_.eventType == "scroll")
                assert(scroll.size == 1)
                assert(scroll.forall(scrollOptions))
                assert(tracker.removed.size == tracker.added.size)
                assert(tracker.removed.filterNot(call => optionObjectTypes.contains(call.eventType)).forall(captureTrue))
                assert(tracker.removed.filter(_.eventType == "wheel").forall(wheelOptions))
                assert(tracker.removed.filter(_.eventType == "scroll").forall(scrollOptions))
                assert(tracker.removed.zip(tracker.added.reverse).forall((removal, addition) => sameCall(removal, addition)))
                tracker.added.foreach { addition =>
                    assert(tracker.removed.count(removal => sameCall(addition, removal)) == 1)
                }
                assert(tracker.documentAdded.map(_.eventType) == Seq("kyo:resolve-drag"))
                assert(tracker.documentRemoved.size == 1)
                assert(sameCall(tracker.documentRemoved.head, tracker.documentAdded.head))
                assert(tracker.activeTimers == 0)
            }
        }
    }

    "cleans up a partially installed listener set when a later add fails" in {
        val chronology = ArrayBuffer.empty[String]
        Scope.acquireRelease(Sync.defer(new ListenerTracker(failOnAdd = "submit", chronology = chronology).install()))(tracker =>
            Sync.defer(tracker.restore())
        ).map {
            tracker =>
                for
                    result <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div("mounted"), new LifecycleChronology(chronology))))
                        .map(_.getResult)
                    _ <- assertEventually(Sync.defer(chronology.contains("drain-joined")), installBudget)
                yield
                    assert(result.isPanic)
                    // The property, not a transcript: installation stops AT the failing type, everything attempted
                    // before it went in, and every one of those came back out in reverse. Spelling the sequence out
                    // instead pinned `submit` to a fixed position in the delegation list, so inserting `contextmenu`
                    // ahead of it broke a test that has nothing to do with context menus (GAPS.md F-35).
                    assert(tracker.attempts.last == "submit")
                    assert(!tracker.attempts.init.contains("submit"))
                    assert(tracker.added.map(_.eventType) == tracker.attempts.init)
                    assert(tracker.removed.map(_.eventType) == tracker.added.map(_.eventType).reverse)
                    assert(tracker.removed.zip(tracker.added.reverse).forall((removal, addition) => sameCall(removal, addition)))
                    assert(chronology == tracker.added.map(call => s"remove:${call.eventType}").reverse ++ Seq(
                        "channel-close",
                        "drain-interrupt",
                        "drain-joined"
                    ))
        }
    }

    "tears down listeners before the event channel and drain" in {
        val chronology = ArrayBuffer.empty[String]
        Scope.acquireRelease(Sync.defer(new ListenerTracker(chronology = chronology).install()))(tracker =>
            Sync.defer(tracker.restore())
        ).map {
            tracker =>
                val diagnostics = new LifecycleChronology(chronology)
                for
                    fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div("mounted"), diagnostics)))
                    _     <- assertEventually(Sync.defer(diagnostics.installed), installBudget)
                    _     <- fiber.interrupt
                    _     <- fiber.getResult
                    _     <- assertEventually(Sync.defer(chronology.contains("drain-joined")), installBudget)
                yield
                    val channelClose = chronology.indexOf("channel-close")
                    val interrupt    = chronology.indexOf("drain-interrupt")
                    val joined       = chronology.indexOf("drain-joined")
                    // Everything torn down before the channel closes: the drag runtime's interval, its document
                    // listener, then one removal per body listener that was added.
                    assert(chronology.take(channelClose).size == tracker.added.size + 2)
                    assert(chronology.take(channelClose).head == "clear-interval")
                    assert(chronology.take(channelClose)(1) == "remove-document:kyo:resolve-drag")
                    assert(chronology.take(channelClose).drop(2).forall(_.startsWith("remove:")))
                    assert(channelClose < interrupt)
                    assert(interrupt < joined)
                end for
        }
    }

    "interrupts and joins an active delegated event handler during teardown" in {
        val chronology = ArrayBuffer.empty[String]
        Scope.acquireRelease(Sync.defer(new ListenerTracker(chronology = chronology).install()))(tracker =>
            Sync.defer(tracker.restore())
        ).map { tracker =>
            for
                entered   <- Latch.init(1)
                blocker   <- Latch.init(1)
                finalized <- Latch.init(1)
                handler     = Sync.ensure(finalized.release)(entered.release.andThen(blocker.await))
                diagnostics = new LifecycleChronology(chronology)
                fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(
                    UI.button("active").id("active-handler").onClick(handler),
                    diagnostics
                )))
                _ <- assertEventually(Sync.defer(diagnostics.installed), installBudget)
                _ <- Sync.defer {
                    val button = dom.document.getElementById("active-handler")
                    val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                        "click",
                        scalajs.Dynamic.literal(bubbles = true)
                    )
                    discard(button.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
                }
                _ <- entered.await
                _ <- fiber.interrupt
                _ <- finalized.await
                _ <- fiber.getResult
            yield
                val channelClose = chronology.indexOf("channel-close")
                val interrupt    = chronology.indexOf("drain-interrupt")
                val joined       = chronology.indexOf("drain-joined")
                assert(tracker.removed.size == tracker.added.size)
                assert(chronology.take(channelClose).size == tracker.added.size + 2)
                assert(chronology.take(channelClose).head == "clear-interval")
                assert(chronology.take(channelClose)(1) == "remove-document:kyo:resolve-drag")
                assert(chronology.take(channelClose).drop(2).forall(_.startsWith("remove:")))
                assert(channelClose < interrupt)
                assert(interrupt < joined)
        }
    }

    "returns delegated body listeners to baseline after every mount cycle" in {
        Scope.acquireRelease(Sync.defer(new ListenerTracker().install()))(tracker => Sync.defer(tracker.restore())).map { tracker =>
            // The regression this guards is a listener that outlives its mount: three unscoped body listeners once
            // survived every teardown, so a page that mounted twice dispatched pointer events into the FIRST mount's
            // closed event channel. Counting per cycle rather than against a fixed total is what makes that visible
            // without also having to know the total (GAPS.md F-35).
            var firstCycle = Seq.empty[String]
            Kyo.foreachDiscard(1 to 3) { cycle =>
                val addedBefore   = tracker.added.size
                val removedBefore = tracker.removed.size
                mountAndStop(tracker).map { _ =>
                    val cycleAdded   = tracker.added.drop(addedBefore)
                    val cycleRemoved = tracker.removed.drop(removedBefore)
                    assert(cycleAdded.nonEmpty)
                    assert(cycleRemoved.size == cycleAdded.size)
                    // Nothing is left behind, so the cumulative totals stay equal cycle after cycle.
                    assert(tracker.removed.size == tracker.added.size)
                    if cycle == 1 then firstCycle = cycleAdded.map(_.eventType).toSeq
                    else assert(cycleAdded.map(_.eventType).toSeq == firstCycle)
                    assert(cycleRemoved.zip(cycleAdded.reverse).forall((removal, addition) => sameCall(removal, addition)))
                    cycleAdded.foreach { addition =>
                        assert(cycleRemoved.count(removal => sameCall(addition, removal)) == 1)
                    }
                }
            }
        }
    }

    "rolls back existing and drag listeners when a late drag listener add fails" in {
        val chronology = ArrayBuffer.empty[String]
        Scope.acquireRelease(Sync.defer(new ListenerTracker(failOnAdd = "drop", chronology = chronology).install()))(tracker =>
            Sync.defer(tracker.restore())
        ).map { tracker =>
            for
                result <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div("mounted"), new LifecycleChronology(chronology))))
                    .map(_.getResult)
                _ <- assertEventually(Sync.defer(chronology.contains("drain-joined")), installBudget)
            yield
                assert(result.isPanic)
                assert(tracker.attempts.takeRight(5) == Seq("dragstart", "dragenter", "dragover", "dragleave", "drop"))
                // Everything attempted before `drop` was installed, and the rollback reaches back past the drag
                // runtime's own listeners into the ones the earlier installers had already put on the body.
                assert(tracker.added.map(_.eventType) == tracker.attempts.init)
                assert(tracker.added.map(_.eventType).contains("click"))
                assert(tracker.removed.size == tracker.added.size)
                assert(tracker.removed.zip(tracker.added.reverse).forall((removal, addition) => sameCall(removal, addition)))
                assert(tracker.documentAdded.isEmpty)
                assert(tracker.activeTimers == 0)
                val channelClose = chronology.indexOf("channel-close")
                assert(chronology.take(channelClose).forall(_.startsWith("remove:")))
        }
    }

    "rolls back all body listeners when the resolution listener add fails" in {
        val chronology = ArrayBuffer.empty[String]
        Scope.acquireRelease(Sync.defer(
            new ListenerTracker(failOnDocumentAdd = "kyo:resolve-drag", chronology = chronology).install()
        ))(tracker => Sync.defer(tracker.restore())).map { tracker =>
            for
                result <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div("mounted"), new LifecycleChronology(chronology))))
                    .map(_.getResult)
                _ <- assertEventually(Sync.defer(chronology.contains("drain-joined")), installBudget)
            yield
                assert(result.isPanic)
                // The document listener is the last thing installed, so every body add had already succeeded: the
                // rollback has to undo the complete set, not a prefix of it.
                assert(tracker.added.map(_.eventType) == tracker.attempts)
                assert(tracker.removed.size == tracker.added.size)
                assert(tracker.removed.zip(tracker.added.reverse).forall((removal, addition) => sameCall(removal, addition)))
                assert(tracker.documentAdded.isEmpty)
                assert(tracker.documentRemoved.isEmpty)
                assert(tracker.activeTimers == 0)
                val channelClose = chronology.indexOf("channel-close")
                assert(chronology.take(channelClose).size == tracker.added.size)
                assert(chronology.take(channelClose).forall(_.startsWith("remove:")))
        }
    }

    "an anchor that also runs a handler keeps the browser's default on a MODIFIED click" in {
        // The dispatcher claims an anchor's click when the anchor carries a kyo handler, so the
        // handler rather than the href drives the action. A ctrl/cmd/shift/alt click is not that
        // case: it is the user asking the browser for a new tab, and preventDefault removes the one
        // capability a link has over a button. The handler runs either way — the gate is on the
        // browser default alone — so both clicks release the latch. Same test `UILocation`'s own
        // anchor interceptor applies to the same click.
        for
            ready   <- Sync.defer(new DomTestEnv.MountReady)
            clicked <- Latch.init(2)
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(
                UI.div(UI.a.href(UI.Href.Path("/target")).id("modified-click").onClick(clicked.release)("Link")),
                ready
            )))
            _ <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("modified-click") != null), installBudget)
            prevented <- Sync.defer {
                val target = dom.document.getElementById("modified-click")
                def click(ctrl: Boolean): Boolean =
                    val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                        "click",
                        scalajs.Dynamic.literal(bubbles = true, cancelable = true, ctrlKey = ctrl)
                    )
                    discard(target.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
                    event.defaultPrevented.asInstanceOf[Boolean]
                end click
                (click(false), click(true))
            }
            // Both clicks reached the handler; only the plain one had its default taken.
            _ <- clicked.await
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield
            assert(prevented._1)
            assert(!prevented._2)
        end for
    }

    "suppresses a scroll key on a target whose isContentEditable the DOM does not define" in {
        // `scrollKeyPrevented` consults `isContentEditable`, which is an HTMLElement member: an SVG target does not
        // carry it, and jsdom implements contentEditable on no element at all, so it reads as undefined. Reading it
        // as a plain Boolean threw a ClassCastException out of the keydown listener, which lost both the
        // preventDefault below and every other event the listener would have delegated.
        for
            ready <- Sync.defer(new DomTestEnv.MountReady)
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(
                UI.div(UI.button("nav").id("scroll-key-target").tabIndex(0).preventScrollKeys),
                ready
            )))
            _ <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("scroll-key-target") != null), installBudget)
            prevented <- Sync.defer {
                val target = dom.document.getElementById("scroll-key-target")
                assert(scalajs.isUndefined(target.asInstanceOf[scalajs.Dynamic].isContentEditable))
                val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].KeyboardEvent)(
                    "keydown",
                    scalajs.Dynamic.literal(key = "ArrowDown", bubbles = true, cancelable = true)
                )
                discard(target.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
                event.defaultPrevented.asInstanceOf[Boolean]
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(prevented)
        end for
    }

end DomBackendDelegationTest
