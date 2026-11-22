package kyo

import kyo.UI.*
import kyo.internal.DomBackend
import kyo.internal.DomReactiveRegions
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** A mount with a `Sync` effect in the browser engine: what it draws is on the page from the first paint and takes events from there on. */
class DomBackendDrawnTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def click(id: String): Unit =
        val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
            "click",
            scalajs.Dynamic.literal(bubbles = true)
        )
        discard(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
    end click

    private def text(id: String): String =
        val el = dom.document.getElementById(id)
        if el == null then "" else el.textContent

    /** Mounts `ui`, runs `first` right after the first paint, before the subscription behind it has run, then `f` on the live page. */
    private def mounted[A](ui: UI, first: () => Unit = () => ())(f: => A < Async)(using Frame, kyo.test.AssertScope): A < Async =
        val diagnostics = new DomTestEnv.MountReady:
            override def regionsOpened(opened: DomReactiveRegions): Unit =
                super.regionsOpened(opened)
                first()
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, diagnostics)))
            _     <- assertEventually(Sync.defer(diagnostics.installed), 30.seconds)
            a     <- f
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield a
        end for
    end mounted

    /** A button that counts its clicks in state it owns, inside a mount. */
    private def counter(id: String, seen: AtomicRef[Chunk[Int]])(using Frame): UI =
        UI.mounted(Signal.initRef(0).map(own =>
            UI.button(own.map(n => s"$id:$n")).id(id).onClick(own.updateAndGet(_ + 1).map(n => seen.getAndUpdate(_.append(n))).unit)
        ))

    "the first paint shows it, and a click right there reaches it with its state" in {
        var atPaint = ""
        for
            seen <- AtomicRef.init(Chunk.empty[Int])
            ui = UI.div(counter("c", seen))
            out <- mounted(
                ui,
                () =>
                    atPaint = text("c")
                    click("c")
            ) {
                for
                    _     <- assertEventually(seen.get.map(_.size == 1))
                    _     <- assertEventually(Sync.defer(text("c") == "c:1"))
                    _     <- Sync.defer(click("c"))
                    _     <- assertEventually(Sync.defer(text("c") == "c:2"))
                    heard <- seen.get
                yield heard
            }
        yield
            assert(atPaint == "c:0", s"the first paint showed '$atPaint'")
            assert(out == Chunk(1, 2))
        end for
    }

    "a click inside a region right after the first paint reaches it too" in {
        for
            seen  <- AtomicRef.init(Chunk.empty[Int])
            shown <- Signal.initRef(true)
            ui = UI.div(shown.render(on => if on then counter("r", seen) else UI.span("none")))
            out <- mounted(ui, () => click("r")) {
                assertEventually(Sync.defer(text("r") == "r:1")).andThen(seen.get)
            }
        yield assert(out == Chunk(1))
        end for
    }

    "a keyed instance keeps its element and its state across a repaint around it" in {
        for
            seen <- AtomicRef.init(Chunk.empty[Int])
            tick <- Signal.initRef(0)
            node = UI.mounted(Signal.initRef(0).map(own =>
                UI.button(own.map(n => s"k:$n")).id("k").onClick(own.updateAndGet(_ + 1).map(n => seen.getAndUpdate(_.append(n))).unit)
            )).keyed("k")
            ui = UI.div(tick.render(n => UI.div(UI.span(s"tick:$n").id("t"), node)))
            kept <- mounted(ui) {
                for
                    _      <- assertEventually(tick.waiters.map(_ == 1))
                    before <- Sync.defer(dom.document.getElementById("k"))
                    _      <- Sync.defer(click("k"))
                    _      <- assertEventually(Sync.defer(text("k") == "k:1"))
                    _      <- tick.set(1)
                    _      <- assertEventually(Sync.defer(text("t") == "tick:1"))
                    _      <- assertEventually(tick.waiters.map(_ == 1))
                    after  <- Sync.defer(dom.document.getElementById("k"))
                    shown  <- Sync.defer(text("k"))
                yield (before eq after, shown)
            }
        yield assert(kept == (true, "k:1"))
        end for
    }

    "a keyless instance starts over at a repaint around it, and the page shows the new state" in {
        for
            seen <- AtomicRef.init(Chunk.empty[Int])
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(_ => counter("f", seen)))
            shown <- mounted(ui) {
                for
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    _ <- Sync.defer(click("f"))
                    _ <- assertEventually(Sync.defer(text("f") == "f:1"))
                    _ <- tick.set(1)
                    _ <- assertEventually(Sync.defer(text("f") == "f:0"))
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    // The handlers are the new instance's too.
                    _ <- Sync.defer(click("f"))
                    _ <- assertEventually(Sync.defer(text("f") == "f:1"))
                yield text("f")
            }
        yield assert(shown == "f:1")
        end for
    }

    "a new keyed instance in a slot of the same key shows its own state" in {
        // The outer mount has no key, so a repaint gives it a new table and the inner mount a new instance, under the same key.
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n =>
                UI.mounted(
                    Kyo.lift(UI.div(UI.mounted(Signal.initRef(n).map(own => UI.span(own.map(v => s"v:$v")).id("v"))).keyed("inner")))
                )
            ))
            shown <- mounted(ui) {
                for
                    _ <- assertEventually(tick.waiters.map(_ == 1))
                    _ <- assertEventually(Sync.defer(text("v") == "v:0"))
                    _ <- tick.set(1)
                    _ <- assertEventually(Sync.defer(text("v") == "v:1"))
                yield text("v")
            }
        yield assert(shown == "v:1")
        end for
    }

    "what runs while it is shown finds its element on the page" in {
        for
            found <- Promise.init[Boolean, Any]
            ui = UI.div(UI.mounted(UI.id.map(id =>
                UI.whileMounted(Sync.defer(dom.document.getElementById(id) != null).map(on => found.completeDiscard(Result.succeed(on))))
                    .andThen(UI.span("x").id(id))
            )))
            on <- mounted(ui)(found.get)
        yield assert(on)
        end for
    }

    // The rule decided on 2026-11-05: an unkeyed element effect belongs to the current run of its region, as an unkeyed mount does. This case
    // said before that it survived a repaint around it.
    "an unkeyed element effect finds the element on the page, starts again with each run of its region, and ends with it" in {
        for
            runs   <- AtomicInt.init(0)
            found  <- AtomicRef.init(Chunk.empty[Boolean])
            closed <- AtomicInt.init(0)
            state  <- Signal.initRef(0)
            ui = UI.div(state.render(n =>
                if n < 0 then UI.span("gone").id("gone"): UI
                else
                    UI.div(
                        UI.span(s"tick:$n").id("el-t"),
                        UI.span("host").id("el-host").whileMounted(
                            runs.incrementAndGet.andThen(Scope.ensure(closed.incrementAndGet.unit)).andThen(
                                Sync.defer(dom.document.getElementById("el-host") != null).map(on => found.getAndUpdate(_.append(on)))
                            ).andThen(Async.never)
                        )
                    )
            ))
            out <- mounted(ui) {
                for
                    _ <- assertEventually(runs.get.map(_ == 1))
                    _ <- assertEventually(state.waiters.map(_ == 1))
                    _ <- state.set(1)
                    _ <- assertEventually(Sync.defer(text("el-t") == "tick:1"))
                    _ <- assertEventually(runs.get.map(_ == 2))
                    _ <- assertEventually(closed.get.map(_ == 1))
                    _ <- assertEventually(state.waiters.map(_ == 1))
                    _ <- state.set(-1)
                    _ <- assertEventually(closed.get.map(_ == 2))
                    f <- found.get
                yield f
            }
        yield assert(out == Chunk(true, true))
        end for
    }

    "a spliced sibling that appears before a keyed node and goes again changes nothing on the page" - {
        def spliced(el: UI)(runsOf: => Int < Sync, endedOf: => Boolean < Sync)(using Frame, kyo.test.AssertScope) =
            for
                where <- Signal.initRef(0)
                ui = UI.div(
                    UI.span("marker").id("sp-w"),
                    where.render(n =>
                        UI.fragment(((if n == 1 then List[UI](UI.span("first")) else Nil) :+ UI.span(s"w$n").id("sp-n") :+ el)*)
                    )
                )
                out <- mounted(ui) {
                    for
                        _ <- assertEventually(runsOf.map(_ == 1))
                        _ <- assertEventually(where.waiters.map(_ == 1))
                        _ <- where.set(1)
                        _ <- assertEventually(Sync.defer(text("sp-n") == "w1"))
                        _ <- assertEventually(where.waiters.map(_ == 1))
                        _ <- where.set(0)
                        _ <- assertEventually(Sync.defer(text("sp-n") == "w0"))
                        _ <- assertEventually(where.waiters.map(_ == 1))
                        n <- runsOf
                        d <- endedOf
                    yield (n, d)
                }
            yield assert(out == (1, false), s"started, and a run ended: $out")
        "a keyed element effect" in {
            for
                runs   <- AtomicInt.init(0)
                closed <- Promise.init[Unit, Any]
                _      <- spliced(UI.span("x").whileMountedKeyed("sp-kept")(
                    runs.incrementAndGet.andThen(Scope.ensure(closed.completeUnitDiscard)).andThen(Async.never)
                ))(runs.get, closed.done)
            yield succeed
        }
        "a keyed mount, which keeps its state" in {
            for
                draws <- AtomicInt.init(0)
                _     <- spliced(UI.mounted(draws.incrementAndGet.andThen(Signal.initRef(0)).map(r =>
                    UI.span(r.map(v => s"v$v")).id("sp-state").onClick(r.getAndUpdate(_ + 1).unit)
                )).keyed("sp-mount"))(draws.get, Kyo.lift(false))
            yield succeed
        }
    }

    "on the page too, UI.id of a keyed instance is the same whether or not a spliced sibling stands before it" in {
        val node                                    = UI.mounted(UI.id.map(i => UI.span("kept").id(i))).keyed("dom-kept")
        def page(sibling: Boolean)(using Frame): UI =
            UI.div(UI.fragment(((if sibling then List[UI](UI.span("first")) else Nil) :+ node)*))
        def idOnPage(sibling: Boolean)(using Frame, kyo.test.AssertScope): String < Async =
            mounted(page(sibling)) {
                assertEventually(Sync.defer(dom.document.querySelector("span[id]") != null))
                    .andThen(Sync.defer(dom.document.querySelector("span[id]").id))
            }
        for
            alone  <- idOnPage(false)
            behind <- idOnPage(true)
        yield assert(alone == behind, s"alone: $alone, behind a sibling: $behind")
        end for
    }

    "on the page, each row of a keyed list keeps its unkeyed instance while another row goes" - {
        def keeps(kind: String)(using Frame, kyo.test.AssertScope): Unit < Async =
            for
                started <- AtomicRef.init(Chunk.empty[String])
                ended   <- AtomicRef.init(Chunk.empty[String])
                aEnded  <- Promise.init[Unit, Any]
                rows    <- Signal.initRef(Chunk("a", "b"))
                run = (r: String) =>
                    Scope.ensure(ended.getAndUpdate(_.append(r)).andThen(if r == "a" then aEnded.completeUnitDiscard else Kyo.unit))
                        .andThen(started.getAndUpdate(_.append(r)).unit)
                ui = UI.div(rows.foreachKeyed(identity)(r =>
                    UI.div(
                        UI.span(r),
                        kind match
                            case "element" => UI.span("x").whileMounted(run(r).andThen(Async.never))
                            case "drawn"   => UI.mounted(UI.whileMounted(run(r).andThen(Async.never)).andThen(UI.span("m")))
                            case _         => UI.mounted(run(r).andThen(UI.span("m")): UI < (Abort[Any] & Async & Scope))
                    )
                ))
                out <- mounted(ui) {
                    for
                        _ <- assertEventually(started.get.map(_.size == 2))
                        _ <- assertEventually(rows.waiters.map(_ == 1))
                        _ <- rows.set(Chunk("b"))
                        _ <- aEnded.get
                        _ <- assertEventually(rows.waiters.map(_ == 1))
                        e <- ended.get
                        s <- started.get
                    yield (s.sorted, e)
                }
            yield assert(out == (Chunk("a", "b"), Chunk("a")), s"started, then ended: $out")
        "an element's effect" in keeps("element")
        "a mount that draws" in keeps("drawn")
        "a mount that waits" in keeps("waiting")
    }

    "an element's command waits for the element, also inside a boundary that holds it back" in {
        for
            measured <- Promise.init[Boolean, Any]
            gate     <- Promise.init[Unit, Any]
            ui = UI.div(UI.boundary.placeholder(UI.span("wait"))(
                UI.div(
                    UI.span("late").id("el-late").whileMounted(
                        UI.commands.map(_.requestMeasureById("el-late")).map(r => measured.completeDiscard(Result.succeed(r.nonEmpty)))
                    ),
                    UI.mounted(gate.get.andThen(UI.span("loaded")))
                )
            ))
            on <- mounted(ui) {
                for
                    d <- measured.done
                    _ <- gate.completeUnitDiscard
                    m <- measured.get
                yield (d, m)
            }
        yield assert(on == (false, true))
        end for
    }

    "UI.scrollIntoView reaches the page from an element's effect and from a mount's, not only from a handler" in {
        val seen  = scalajs.Array[String]()
        val proto = scalajs.Dynamic.global.window.Element.prototype
        val prior = proto.scrollIntoView
        Scope.run {
            for
                _ <- Scope.ensure(Sync.defer(proto.scrollIntoView = prior))
                _ <- Sync.defer(proto.scrollIntoView =
                    (
                        (self: dom.Element, _: scalajs.Any) => discard(seen.push(self.id))
                    ): scalajs.ThisFunction1[dom.Element, scalajs.Any, Unit]
                )
                ui = UI.div(
                    UI.span("a").id("sink-el").whileMounted(UI.scrollIntoView("sink-el")),
                    UI.mounted(UI.whileMounted(UI.scrollIntoView("sink-mount")).andThen(UI.span("b").id("sink-mount")))
                )
                on <- mounted(ui) {
                    assertEventually(Sync.defer(seen.toSet == Set("sink-el", "sink-mount")), 5.seconds)
                        .andThen(Sync.defer(seen.toList.sorted))
                }
            yield assert(on == List("sink-el", "sink-mount"))
        }
    }

end DomBackendDrawnTest
