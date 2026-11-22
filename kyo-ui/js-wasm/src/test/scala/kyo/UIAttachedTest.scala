package kyo

import kyo.UI.render
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** [[kyo.onAttached]]: the hook starts once its element is in the document, wherever the element sits, and its scope ends when the
  * element leaves.
  */
class UIAttachedTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** What the hooks did, in order: `start:<id>:<in the document>`, `release:<id>`, `use:<id>:<value>`. */
    private def recorder(): scalajs.Array[String] = scalajs.Array[String]()

    /** An element whose hook records its start, whether the element was in the document then, and the release of its scope. */
    private def tracked(id: String, log: scalajs.Array[String])(using Frame): UI =
        UI.div.id(id).onAttached { el =>
            // Read once: a morph that keeps the node gives it the next element's id before this scope closes.
            val seen = el.id
            Sync.defer(discard(log.push(s"start:$seen:${el.isConnected}")))
                .andThen(Scope.ensure(Sync.defer(discard(log.push(s"release:$seen")))))
        }

    private def click(id: String)(using Frame): Unit < Sync =
        Sync.defer {
            val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                "click",
                scalajs.Dynamic.literal(bubbles = true)
            )
            discard(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        }

    /** A button whose clicks are counted. The drain runs one queued thing at a time, so a counted click is a barrier: whatever was queued
      * before it, an attach or a close, has been dispatched.
      */
    private def barrier(clicks: AtomicInt)(using Frame): UI = UI.button("barrier").id("barrier").onClick(clicks.incrementAndGet.unit)

    private def drained(clicks: AtomicInt)(using Frame, kyo.test.AssertScope): Unit < Async =
        clicks.get.map(before => click("barrier").andThen(assertEventually(clicks.get.map(_ > before))))

    private def mount(ui: UI)(using Frame, kyo.test.AssertScope): DomTestEnv.MountReady < (Async & Scope) =
        mountWith(ui, Absent)

    /** Mounts into `#<container>`, a child of the body, where one is given: an application's mount, whose container is not the body. */
    private def mountWith(ui: UI, container: Maybe[String])(using Frame, kyo.test.AssertScope): DomTestEnv.MountReady < (Async & Scope) =
        val ready = new DomTestEnv.MountReady
        for
            _     <- Sync.defer(dom.document.body.innerHTML = container.fold("")(id => s"""<div id="$id"></div>"""))
            fiber <- Fiber.initUnscoped(Scope.run(container.fold(DomBackend.mount(ui, ready))(id => DomBackend.mount(ui, s"#$id", ready))))
            _     <- Scope.ensure(fiber.interrupt.andThen(fiber.getResult).unit)
            _     <- assertEventually(Sync.defer(ready.installed))
        yield ready
        end for
    end mountWith

    "the hook gets its element, in the document, on the first paint" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                _      <- mount(UI.div(barrier(clicks), tracked("at-first", log)))
                _      <- assertEventually(Sync.defer(log.nonEmpty))
                _      <- drained(clicks)
            yield assert(log.toSeq == Seq("start:at-first:true"), log.mkString(", "))
        }
    }

    "inside a mount whose placeholder holds its content back, the hook starts once the content is shown" in {
        // The case that left the demo's code editor empty: a mount's own placeholder is a boundary, and the effects inside it run
        // before the boundary shows anything.
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                gate   <- Promise.init[Unit, Any]
                held = UI.mounted(gate.get.andThen(UI.div(tracked("held", log)))).keyed("held").placeholder(UI.span("loading"))
                _ <- mount(UI.div(barrier(clicks), held))
                _ <- drained(clicks)
                before = log.toSeq
                _ <- gate.completeUnitDiscard
                _ <- assertEventually(Sync.defer(log.nonEmpty))
                _ <- drained(clicks)
            yield
                assert(before.isEmpty, s"the hook started while the content was held back: $before")
                assert(log.toSeq == Seq("start:held:true"), log.mkString(", "))
        }
    }

    "inside a portal under the body, an element a later patch brings starts its hook" in {
        // A portal leaves the mount's container for document.body, so a sweep over the container alone never meets what a patch
        // brings into it afterwards. The mount has a container of its own: on one into the body itself, the body holds the portal.
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                page   <- Signal.initRef("a")
                ui = UI.div(barrier(clicks), UI.div(page.render(p => tracked(s"ported-$p", log))).id("portal-host").portal(true))
                _      <- mountWith(ui, Present("app"))
                _      <- assertEventually(Sync.defer(log.contains("start:ported-a:true")))
                parent <- Sync.defer(dom.document.getElementById("portal-host").parentNode eq dom.document.body)
                _      <- page.set("b")
                _      <- assertEventually(Sync.defer(log.contains("start:ported-b:true")))
                _      <- drained(clicks)
            yield
                assert(parent, "the portal was not moved to the body, so this does not test a portaled element")
                assert(log.toSeq == Seq("start:ported-a:true", "release:ported-a", "start:ported-b:true"), log.mkString(", "))
        }
    }

    "an element replaced by a region closes its scope, and the new one starts its own hook once" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                page   <- Signal.initRef("a")
                _      <- mount(UI.div(barrier(clicks), page.render(p => tracked(s"page-$p", log))))
                _      <- assertEventually(Sync.defer(log.contains("start:page-a:true")))
                _      <- page.set("b")
                _      <- assertEventually(Sync.defer(log.contains("start:page-b:true")))
                _      <- drained(clicks)
                _      <- drained(clicks)
            yield assert(log.toSeq == Seq("start:page-a:true", "release:page-a", "start:page-b:true"), log.mkString(", "))
        }
    }

    "a re-render that rebuilds the element ends its hook and starts it again, even where the page keeps the node" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                label  <- Signal.initRef("one")
                ui = UI.div(barrier(clicks), label.render(l => UI.div(tracked("rebuilt", log), UI.span(l).id("label"))))
                _    <- mount(ui)
                _    <- assertEventually(Sync.defer(log.nonEmpty))
                node <- Sync.defer(dom.document.getElementById("rebuilt"))
                _    <- label.set("two")
                _    <- assertEventually(Sync.defer(log.length == 3))
                _    <- drained(clicks)
                _    <- drained(clicks)
                same <- Sync.defer(dom.document.getElementById("rebuilt") eq node)
            yield
                assert(log.toSeq == Seq("start:rebuilt:true", "release:rebuilt", "start:rebuilt:true"), log.mkString(", "))
                // Not what the test is about, but what makes it bite: the morph kept the node, and the hook restarted anyway.
                assert(same, "the morph replaced the node, so this no longer shows a restart on a kept node")
        }
    }

    "an element value a re-render shows again keeps its hook and what the hook put inside it" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                label  <- Signal.initRef("one")
                widget = UI.div.id("widget").onAttached { el =>
                    Sync.defer {
                        val child = dom.document.createElement("canvas")
                        child.id = "drawn"
                        discard(el.appendChild(child))
                        discard(log.push("start"))
                    }
                }
                ui = UI.div(barrier(clicks), label.render(l => UI.div(widget, UI.span(l).id("label"))))
                _     <- mount(ui)
                _     <- assertEventually(Sync.defer(dom.document.getElementById("drawn") != null))
                _     <- label.set("two")
                _     <- assertEventually(Sync.defer(dom.document.getElementById("label").textContent == "two"))
                _     <- drained(clicks)
                _     <- drained(clicks)
                drawn <- Sync.defer(dom.document.getElementById("drawn") != null)
            yield
                assert(log.toSeq == Seq("start"), log.mkString(", "))
                assert(drawn, "the re-render removed what the hook put inside the element")
        }
    }

    "an element that leaves before its content was shown never starts its hook" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                gate   <- Promise.init[Unit, Any]
                show   <- Signal.initRef(true)
                held = UI.mounted(gate.get.andThen(UI.div(tracked("never", log)))).keyed("never").placeholder(UI.span("loading"))
                _ <- mount(UI.div(barrier(clicks), show.render(on => if on then held else UI.span("gone").id("gone"))))
                _ <- drained(clicks)
                _ <- show.set(false)
                _ <- assertEventually(Sync.defer(dom.document.getElementById("gone") != null))
                _ <- gate.completeUnitDiscard
                _ <- drained(clicks)
                _ <- drained(clicks)
            yield assert(log.isEmpty, log.mkString(", "))
        }
    }

    "work started before the element arrives is used once it is there, and a hook that waits holds up no event" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                result <- Promise.init[String, Any]
                ui = UI.div(
                    barrier(clicks),
                    UI.mounted(
                        for computing <- Fiber.init(result.get)
                        yield UI.div.id("chart").onAttached { el =>
                            Sync.defer(discard(log.push(s"waiting:${el.isConnected}")))
                                .andThen(computing.get)
                                .map(v => discard(log.push(s"use:${el.id}:$v")))
                        }
                    )
                )
                _ <- mount(ui)
                _ <- assertEventually(Sync.defer(log.contains("waiting:true")))
                // The hook is waiting, and the drain still takes the next event.
                _ <- drained(clicks)
                _ <- result.completeDiscard(Result.succeed("42"))
                _ <- assertEventually(Sync.defer(log.contains("use:chart:42")))
            yield assert(log.toSeq == Seq("waiting:true", "use:chart:42"), log.mkString(", "))
        }
    }

    "an element that leaves while its hook waits interrupts the wait and releases its scope" in {
        val log = recorder()
        Scope.run {
            for
                clicks <- AtomicInt.init
                never  <- Promise.init[String, Any]
                show   <- Signal.initRef(true)
                waiting = UI.div.id("waiting").onAttached { el =>
                    Scope.ensure(Sync.defer(discard(log.push("release"))))
                        .andThen(Sync.defer(discard(log.push("waiting"))))
                        .andThen(never.get)
                        .map(v => discard(log.push(s"use:$v")))
                }
                _ <- mount(UI.div(barrier(clicks), show.render(on => if on then waiting else UI.span("gone"))))
                _ <- assertEventually(Sync.defer(log.contains("waiting")))
                _ <- show.set(false)
                _ <- assertEventually(Sync.defer(log.contains("release")))
                _ <- drained(clicks)
            yield assert(log.toSeq == Seq("waiting", "release"), log.mkString(", "))
        }
    }

    "a failure the hook leaves goes to the onFailure rules around the element" in {
        Scope.run {
            for
                clicks  <- AtomicInt.init
                handled <- Promise.init[Any, Any]
                ui = UI.div(
                    barrier(clicks),
                    UI.div.id("failing").onAttached(_ => Abort.fail("broken"))
                ).onFailure { case failure => UI.Reaction.Run(handled.completeDiscard(Result.succeed(failure))) }
                _       <- mount(ui)
                failure <- handled.get
            yield assert(failure.toString == "broken", s"failure: $failure")
        }
    }

    "two mounts on one page each start only the hooks of their own elements" in {
        val log = recorder()
        Scope.run {
            for
                _ <- Sync.defer(dom.document.body.innerHTML = """<div id="at-a"></div><div id="at-b"></div>""")
                readyA = new DomTestEnv.MountReady
                readyB = new DomTestEnv.MountReady
                fiberA <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div(tracked("in-a", log)), "#at-a", readyA)))
                _      <- Scope.ensure(fiberA.interrupt.andThen(fiberA.getResult).unit)
                _      <- assertEventually(Sync.defer(readyA.installed && log.contains("start:in-a:true")))
                clicks <- AtomicInt.init
                fiberB <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div(tracked("in-b", log), barrier(clicks)), "#at-b", readyB)))
                _      <- Scope.ensure(fiberB.interrupt.andThen(fiberB.getResult).unit)
                _      <- assertEventually(Sync.defer(readyB.installed && log.contains("start:in-b:true")))
                // B's first sweep saw A's element too, and both elements sit at path 0 of their own tree, so B dispatching A's
                // element would run B's hook on it. Anything it started has run by the second barrier.
                _ <- drained(clicks)
                _ <- drained(clicks)
            yield assert(log.toSeq.sorted == Seq("start:in-a:true", "start:in-b:true"), log.mkString(", "))
        }
    }

    "the session's end closes every scope still open" in {
        val log = recorder()
        for
            _ <- Scope.run {
                for
                    clicks <- AtomicInt.init
                    _      <- mount(UI.div(barrier(clicks), tracked("to-close", log)))
                    _      <- assertEventually(Sync.defer(log.nonEmpty))
                yield ()
            }
        yield assert(log.toSeq == Seq("start:to-close:true", "release:to-close"), log.mkString(", "))
        end for
    }

end UIAttachedTest
