package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** `UI.Bound.Follow` on the browser mount: the patches write what the reader's edit moved, under jsdom. Every patch lands inside the
  * writer's `set` or the dispatch that decides it. [[UIBoundFollowDomTest]] holds the same cases over the server transport, in real Chrome.
  */
class DomBackendFollowTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def el(id: String): scalajs.Dynamic = dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic]

    private def fire(id: String, kind: String): Unit =
        val event =
            scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].Event)(kind, scalajs.Dynamic.literal(bubbles = true))
        discard(el(id).dispatchEvent(event))
    end fire

    /** Mounts `ui` and runs `f` once the mount is wired, then ends the mount. */
    private def mounted[A](ui: UI, ids: String*)(f: => A < Async)(using Frame, kyo.test.AssertScope): A < Async =
        val ready = new DomTestEnv.MountReady
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && ids.forall(dom.document.getElementById(_) != null)))
            a     <- f
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield a
        end for
    end mounted

    "a write to the signal reaches the field without a repaint, after the reader typed" in {
        for
            held <- Signal.initRef("held")
            r    <- mounted(UI.div(UI.input.id("t").value(held.readOnly)), "t") {
                for
                    _     <- Sync.defer(el("t").value = "typed")
                    node  <- Sync.defer(el("t"))
                    _     <- held.set("next")
                    shown <- Sync.defer(el("t").value.asInstanceOf[String])
                    same  <- Sync.defer(scalajs.special.strictEquals(node, el("t")))
                yield (shown, same)
            }
        yield assert(r == ("next", true))
        end for
    }

    "a declined keystroke in a textarea is put back, also while it has focus" in {
        for
            held <- Signal.initRef("held")
            seen <- AtomicInt.init(0)
            r    <- mounted(UI.div(UI.textarea.id("t").value(held.readOnly).onInput(_ => seen.incrementAndGet.unit)), "t") {
                for
                    _ <- Sync.defer {
                        el("t").focus()
                        el("t").value = "typed"
                        fire("t", "input")
                    }
                    _     <- assertEventually(seen.get.map(_ == 1).map(_ && el("t").value.asInstanceOf[String] == "held"))
                    focus <- Sync.defer(scalajs.special.strictEquals(dom.document.activeElement, el("t")))
                yield focus
            }
        yield assert(r, "the patch kept the element and its focus")
        end for
    }

    "a bool patch of checked writes the property of a box the reader toggled" in {
        for
            held  <- Signal.initRef(false)
            shown <- mounted(UI.div(UI.checkbox.id("c").checked(held.readOnly)), "c") {
                for
                    _ <- Sync.defer(el("c").checked = true)
                    _ <- held.set(true)
                    _ <- Sync.defer(el("c").checked = false)
                    _ <- held.set(false)
                    _ <- held.set(true)
                yield el("c").checked.asInstanceOf[Boolean]
            }
        yield assert(shown)
        end for
    }

    "a toggle its owner declined takes the box back" in {
        for
            held <- Signal.initRef(false)
            seen <- AtomicInt.init(0)
            _    <- mounted(UI.div(UI.checkbox.id("c").checked(held.readOnly).onChange(_ => seen.incrementAndGet.unit)), "c") {
                Sync.defer(el("c").click()).andThen(assertEventually(seen.get.map(_ == 1).map(_ && !el("c").checked.asInstanceOf[Boolean])))
            }
        yield succeed
        end for
    }

    "a radio its owner declined gives the group its selection back" in {
        for
            a    <- Signal.initRef(true)
            b    <- Signal.initRef(false)
            seen <- AtomicInt.init(0)
            ui = UI.div(
                UI.radio.id("ra").name("g").checked(a.readOnly).onChange(_ => seen.incrementAndGet.unit),
                UI.radio.id("rb").name("g").checked(b.readOnly).onChange(_ => seen.incrementAndGet.unit)
            )
            _ <- mounted(ui, "ra", "rb") {
                Sync.defer(el("rb").click()).andThen(assertEventually(seen.get.map(_ == 1).map(_ =>
                    el("ra").checked.asInstanceOf[Boolean] && !el("rb").checked.asInstanceOf[Boolean]
                )))
            }
        yield succeed
        end for
    }

    "a free radio beside a bound group keeps its tick" in {
        for
            b <- Signal.initRef(false)
            ui = UI.div(UI.radio.id("free").name("other"), UI.radio.id("rb").name("g").checked(b.readOnly))
            _ <- mounted(ui, "free", "rb") {
                Sync.defer(el("free").click()).andThen(Sync.defer(el("rb").click())).andThen(assertEventually(Sync.defer(
                    el("free").checked.asInstanceOf[Boolean] && !el("rb").checked.asInstanceOf[Boolean]
                )))
            }
        yield succeed
        end for
    }

    "a native select follows its signal" in {
        for
            held  <- Signal.initRef("a")
            shown <- mounted(UI.div(UI.select.id("s").value(held.readOnly)(UI.option("A").value("a"), UI.option("B").value("b"))), "s") {
                held.set("b").andThen(Sync.defer(el("s").value.asInstanceOf[String]))
            }
        yield assert(shown == "b")
        end for
    }

end DomBackendFollowTest
