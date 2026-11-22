package kyo

import kyo.UI.foreachKeyed
import kyo.UI.render
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** The native state of a control on the browser mount, under jsdom: a bound control (a constant, a signal or a ref) shows its binding
  * once the handlers of an edit and the regions they woke have run, and a default leaves the control to the reader.
  *
  * Each case waits on a barrier, never on the clock: a click on a button after the edit, whose effect shows once the event queue has
  * handled everything before it, the put-back of the edit included.
  */
class DomBackendNativeStateTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def el(id: String): scalajs.Dynamic = dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic]
    private def checked(id: String): Boolean    = el(id).checked.asInstanceOf[Boolean]
    private def value(id: String): String       = el(id).value.asInstanceOf[String]

    private def fire(id: String, kind: String): Unit =
        val event =
            scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].Event)(kind, scalajs.Dynamic.literal(bubbles = true))
        discard(el(id).dispatchEvent(event))
    end fire

    private def typeInto(id: String, text: String): Unit =
        el(id).value = text
        fire(id, "input")

    private def choose(id: String, v: String): Unit =
        el(id).value = v
        fire(id, "change")

    /** Counts the writes kyo-ui makes to `prop` of the element `id`: the reader's own toggle never goes through the JS setter. */
    private def countWrites(id: String, prop: String): scalajs.Dynamic =
        val counter = scalajs.Dynamic.literal(n = 0)
        val install = scalajs.Dynamic.newInstance(scalajs.Dynamic.global.Function)(
            "el",
            "prop",
            "c",
            """var p=Object.getPrototypeOf(el),d;while(p&&!(d=Object.getOwnPropertyDescriptor(p,prop)))p=Object.getPrototypeOf(p);
              |Object.defineProperty(el,prop,{configurable:true,get:function(){return d.get.call(this)},set:function(v){c.n++;d.set.call(this,v)}});""".stripMargin
        )
        discard(install.asInstanceOf[scalajs.Dynamic](el(id), prop, counter))
        counter
    end countWrites

    /** Mounts `ui` beside a barrier button, runs `f` once the mount is wired, then ends the mount. `settled` clicks the barrier and waits
      * until its handler ran, which is after every event before it has been handled, put-back included.
      */
    private def mounted[A](ui: UI, ids: String*)(f: (() => Unit < (Async & Abort[Any])) => A < (Async & Abort[Any]))(using
        Frame,
        kyo.test.AssertScope
    ): A < (Async & Abort[Any]) =
        val ready = new DomTestEnv.MountReady
        for
            passed <- AtomicInt.init(0)
            barrier = UI.button("barrier").id("barrier").onClick(passed.incrementAndGet.unit)
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div(ui, barrier), ready)))
            _     <- assertEventually(Sync.defer(ready.installed && ("barrier" +: ids).forall(dom.document.getElementById(_) != null)))
            settled = () =>
                passed.get.map(before =>
                    Sync.defer(discard(el("barrier").click())).andThen(assertEventually(passed.get.map(_ > before)))
                )
            a <- f(settled)
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield a
        end for
    end mounted

    "a tick its owner declined on a box bound to a constant in a region is taken back" in {
        for
            model <- Signal.initRef(false)
            r     <- mounted(model.render(v => UI.checkbox.id("c").checked(v).onChange(_ => Kyo.unit)), "c") { settled =>
                for
                    writes <- Sync.defer(countWrites("c", "checked"))
                    _      <- Sync.defer(discard(el("c").click()))
                    _      <- settled()
                yield (checked("c"), writes.n.asInstanceOf[Int])
            }
        yield assert(r == (false, 1), s"box and writes: $r")
        end for
    }

    "a tick its owner took on a box bound to a constant in a region costs no write" in {
        for
            model <- Signal.initRef(false)
            r     <- mounted(model.render(v => UI.checkbox.id("c").checked(v).onChange(model.set(_))), "c") { settled =>
                for
                    writes <- Sync.defer(countWrites("c", "checked"))
                    _      <- Sync.defer(discard(el("c").click()))
                    _      <- settled()
                    m      <- model.get
                yield (m, checked("c"), writes.n.asInstanceOf[Int])
            }
        yield assert(r == (true, true, 0), s"model, box and writes: $r")
        end for
    }

    "a tick the owner took and the model took back before the region ran is taken back" in {
        for
            model <- Signal.initRef(false)
            r     <- mounted(model.render(v => UI.checkbox.id("c").checked(v).onChange(on => model.set(on).andThen(model.set(!on)))), "c") {
                settled => Sync.defer(discard(el("c").click())).andThen(settled()).andThen(model.get.map(m => (m, checked("c"))))
            }
        yield assert(r == (false, false), s"model and box: $r")
        end for
    }

    "a reset to the value the model already holds finds the box already taken back" in {
        for
            model <- Signal.initRef(false)
            r     <- mounted(model.render(v => UI.checkbox.id("c").checked(v).onChange(_ => Kyo.unit)), "c") { settled =>
                for
                    _ <- Sync.defer(discard(el("c").click()))
                    _ <- settled()
                    _ <- model.set(false)
                    _ <- settled()
                yield checked("c")
            }
        yield assert(!r, "the box shows a tick the model never held")
        end for
    }

    "a static constant keeps a box at its value" in {
        for
            seen <- AtomicInt.init(0)
            r    <- mounted(UI.checkbox.id("c").checked(false).onChange(_ => seen.incrementAndGet.unit), "c") { settled =>
                Sync.defer(discard(el("c").click())).andThen(settled()).andThen(seen.get.map(n => (n, checked("c"))))
            }
        yield assert(r == (1, false), s"heard and box: $r")
        end for
    }

    "a box bound to a constant with no handler is taken back" in {
        for
            r <- mounted(UI.checkbox.id("c").checked(true), "c") { settled =>
                Sync.defer(discard(el("c").click())).andThen(settled()).andThen(Sync.defer(checked("c")))
            }
        yield assert(r, "the box lost the tick its constant holds")
        end for
    }

    "a ref whose hearer writes the old value back takes the box back" in {
        for
            model <- Signal.initRef(false)
            r     <- mounted(UI.checkbox.id("c").checked(model).onChange(on => model.set(!on)), "c") { settled =>
                Sync.defer(discard(el("c").click())).andThen(settled()).andThen(model.get.map(m => (m, checked("c"))))
            }
        yield assert(r == (false, false), s"model and box: $r")
        end for
    }

    "a select bound to a constant in a region takes back a pick its owner declined" in {
        for
            model <- Signal.initRef("a")
            ui = model.render(v => UI.select(UI.option("A").value("a"), UI.option("B").value("b")).id("s").value(v).onChange(_ => Kyo.unit))
            r <- mounted(ui, "s")(settled => Sync.defer(choose("s", "b")).andThen(settled()).andThen(Sync.defer(value("s"))))
        yield assert(r == "a")
        end for
    }

    "a select whose option is selected by a constant takes back a pick" in {
        for
            r <- mounted(UI.select(UI.option("A").value("a"), UI.option("B").value("b").selected(true)).id("s"), "s") { settled =>
                Sync.defer(choose("s", "a")).andThen(settled()).andThen(Sync.defer(value("s")))
            }
        yield assert(r == "b")
        end for
    }

    "a select whose option is a default keeps the reader's pick" in {
        for
            r <- mounted(UI.select(UI.option("A").value("a"), UI.option("B").value("b").defaultSelected(true)).id("s"), "s") { settled =>
                Sync.defer(choose("s", "a")).andThen(settled()).andThen(Sync.defer(value("s")))
            }
        yield assert(r == "a")
        end for
    }

    "a field bound to a constant takes back a keystroke its owner declined" in {
        for
            r <- mounted(UI.input.id("t").value("held").onInput(_ => Kyo.unit), "t") { settled =>
                Sync.defer(typeInto("t", "heldx")).andThen(settled()).andThen(Sync.defer(value("t")))
            }
        yield assert(r == "held")
        end for
    }

    "a field bound to a constant in a region shows what its owner filtered" in {
        for
            model <- Signal.initRef("12")
            r     <- mounted(model.render(s => UI.input.id("t").value(s).onInput(t => model.set(t.filter(_.isDigit)))), "t") { settled =>
                Sync.defer(typeInto("t", "12a")).andThen(settled()).andThen(model.get.map(m => (m, value("t"))))
            }
        yield assert(r == ("12", "12"))
        end for
    }

    "a field with a default keeps what the reader types, also through a repaint around it" in {
        for
            other <- Signal.initRef(0)
            ui = other.render(o => UI.div(UI.input.id("t").defaultValue("start").onInput(_ => Kyo.unit), UI.span(s"o=$o").id("o")))
            r <- mounted(ui, "t") { settled =>
                for
                    _ <- Sync.defer(typeInto("t", "typed"))
                    _ <- settled()
                    typed = value("t")
                    _ <- other.set(1)
                    _ <- assertEventually(Sync.defer(dom.document.getElementById("o").textContent == "o=1"))
                yield (typed, value("t"))
            }
        yield assert(r == ("typed", "typed"))
        end for
    }

    "a field whose default changes shows the new default" in {
        for
            start <- Signal.initRef("one")
            ui = start.render(s => UI.input.id("t").defaultValue(s))
            r <- mounted(ui, "t") { settled =>
                for
                    _ <- Sync.defer(typeInto("t", "typed"))
                    _ <- start.set("two")
                    _ <- assertEventually(Sync.defer(value("t") == "two"))
                yield value("t")
            }
        yield assert(r == "two")
        end for
    }

    "a box with a default keeps the reader's tick through a repaint around it" in {
        for
            other <- Signal.initRef(0)
            ui = other.render(o => UI.div(UI.checkbox.id("c").defaultChecked(false).onChange(_ => Kyo.unit), UI.span(s"o=$o").id("o")))
            r <- mounted(ui, "c") { settled =>
                for
                    _ <- Sync.defer(discard(el("c").click()))
                    _ <- settled()
                    ticked = checked("c")
                    _ <- other.set(1)
                    _ <- assertEventually(Sync.defer(dom.document.getElementById("o").textContent == "o=1"))
                yield (ticked, checked("c"))
            }
        yield assert(r == (true, true))
        end for
    }

    "a click puts back the indeterminate state a box states" in {
        for
            selection <- Signal.initRef(Set(1))
            ui = selection.render(s =>
                UI.checkbox.id("c").checked(s.size == 3).indeterminate(s.nonEmpty && s.size < 3).onChange(_ => Kyo.unit)
            )
            r <- mounted(ui, "c") { settled =>
                Sync.defer(discard(el("c").click())).andThen(settled()).andThen(Sync.defer(
                    (checked("c"), el("c").indeterminate.asInstanceOf[Boolean])
                ))
            }
        yield assert(r == (false, true), s"checked and indeterminate: $r")
        end for
    }

    "Clear unticks every row, also when the last tick and Clear land before that row's region runs" in {
        for
            picked <- Signal.initRef(Set.empty[Int])
            rows = Seq(1, 2, 3).map(i =>
                picked.map(_.contains(i)).render(on =>
                    UI.checkbox.id(s"r$i").checked(on).onChange(take =>
                        picked.getAndUpdate(s => if take then s + i else s - i).unit.andThen(if i == 3 then picked.set(Set.empty)
                        else Kyo.unit)
                    )
                )
            )
            r <- mounted(UI.div(rows(0), rows(1), rows(2)), "r1", "r2", "r3") { settled =>
                for
                    _ <- Sync.defer(Seq(1, 2).foreach(i => discard(el(s"r$i").click())))
                    _ <- settled()
                    _ <- Sync.defer(discard(el("r3").click()))
                    _ <- settled()
                    m <- picked.get
                yield (m, Seq(1, 2, 3).map(i => checked(s"r$i")))
            }
        yield assert(r == (Set.empty[Int], Seq(false, false, false)), s"model and boxes: $r")
        end for
    }

    "a row the reader ticked, removed and re-added in the same step, shows its binding" in {
        for
            rows <- Signal.initRef(Chunk("a", "b"))
            ui = rows.foreachKeyed(identity)(id => UI.checkbox.id(s"row-$id").checked(false).onChange(_ => Kyo.unit))
            r <- mounted(ui, "row-b") { settled =>
                for
                    _ <- Sync.defer(discard(el("row-b").click()))
                    _ <- settled()
                    _ <- rows.set(Chunk("a"))
                    _ <- rows.set(Chunk("a", "b"))
                    _ <- settled()
                yield checked("row-b")
            }
        yield assert(!r, "the re-added row kept a tick its binding never gave")
        end for
    }

end DomBackendNativeStateTest
