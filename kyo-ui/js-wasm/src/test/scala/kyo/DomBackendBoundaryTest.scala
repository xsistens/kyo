package kyo

import kyo.UI.foreachKeyed
import kyo.UI.render
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** [[kyo.UI.boundary]] on the in-browser engine: a page that keeps showing old content while its next content loads is marked inert and
  * busy, as on the server-push transport, and the marks go when the next content is revealed.
  */
class DomBackendBoundaryTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def click(id: String)(using Frame): Unit < Sync =
        Sync.defer {
            val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                "click",
                scalajs.Dynamic.literal(bubbles = true)
            )
            discard(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        }

    private def el(id: String): dom.Element = dom.document.getElementById(id)

    private def text(): String = dom.document.body.textContent

    "a page that waits is inert and busy, the field in it loses focus, and the marks go with the reveal" in {
        for
            route   <- Signal.initRef("1")
            started <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            detail = (id: String) =>
                val load = if id == "1" then Kyo.unit else started.completeUnitDiscard.andThen(gate.get)
                UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title"), UI.textarea.id(s"comment-$id")))).keyed(id)
            ui = UI.boundary.placeholder(UI.span("skel"))(UI.div(
                UI.button("go").id("go").onClick(route.set("2")),
                route.render(id => UI.div(detail(id)).id("page"))
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && el("title") != null && el("title").textContent == "title:1"))
            _     <- Sync.defer(el("comment-1").asInstanceOf[dom.html.TextArea].focus())
            _     <- click("go")
            _     <- started.get
            _     <- assertEventually(Sync.defer(el("page").hasAttribute("inert") && el("page").getAttribute("aria-busy") == "true"))
            left  <- Sync.defer(dom.document.activeElement eq dom.document.body)
            shown <- Sync.defer(el("title").textContent)
            _     <- gate.completeUnitDiscard
            _     <- assertEventually(Sync.defer(el("title").textContent == "title:2" && dom.document.querySelector("[inert]") == null))
            busy  <- Sync.defer(dom.document.querySelector("[aria-busy]") == null)
            after <- Sync.defer(dom.document.activeElement eq dom.document.body)
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(left, "focus stayed in the waiting page")
            assert(shown == "title:1", s"the page while issue 2 loads: $shown")
            assert(busy, "a mark stayed after the reveal")
            assert(after, "focus moved into the revealed page")
        end for
    }

    "navigating twice while the first page loads keeps the page inert until the last one is revealed" in {
        for
            route    <- Signal.initRef("1")
            started2 <- Promise.init[Unit, Any]
            started3 <- Promise.init[Unit, Any]
            never    <- Promise.init[Unit, Any]
            gate3    <- Promise.init[Unit, Any]
            detail = (id: String) =>
                val load = id match
                    case "2" => started2.completeUnitDiscard.andThen(never.get)
                    case "3" => started3.completeUnitDiscard.andThen(gate3.get)
                    case _   => Kyo.unit
                UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title")))).keyed(id)
            ui = UI.boundary.placeholder(UI.span("skel"))(UI.div(
                UI.button("2").id("go2").onClick(route.set("2")),
                UI.button("3").id("go3").onClick(route.set("3")),
                route.render(id => UI.div(detail(id)).id("page"))
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && el("title") != null && el("title").textContent == "title:1"))
            _     <- click("go2")
            _     <- started2.get
            _     <- assertEventually(Sync.defer(el("page").hasAttribute("inert")))
            _     <- click("go3")
            _     <- started3.get
            // Issue 2's wait ended and issue 3's began; the page is held again, and still shows issue 1.
            _ <- assertEventually(Sync.defer(el("page").hasAttribute("inert") && el("title").textContent == "title:1"))
            _ <- gate3.completeUnitDiscard
            _ <- assertEventually(Sync.defer(el("title").textContent == "title:3" && dom.document.querySelector("[inert]") == null))
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield succeed
        end for
    }

    "unmounting while a page waits leaves no mark behind" in {
        for
            route   <- Signal.initRef("1")
            started <- Promise.init[Unit, Any]
            never   <- Promise.init[Unit, Any]
            detail = (id: String) =>
                val load = if id == "1" then Kyo.unit else started.completeUnitDiscard.andThen(never.get)
                UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title")))).keyed(id)
            ui = UI.boundary.placeholder(UI.span("skel"))(UI.div(
                UI.button("go").id("go").onClick(route.set("2")),
                route.render(id => UI.div(detail(id)).id("page"))
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && el("title") != null && el("title").textContent == "title:1"))
            _     <- click("go")
            _     <- started.get
            _     <- assertEventually(Sync.defer(el("page").hasAttribute("inert")))
            _     <- fiber.interrupt
            _     <- fiber.getResult
            // An interrupted mount releases on a fiber of its own, so the release is awaited on the page.
            _ <- assertEventually(
                Sync.defer(dom.document.querySelector("[inert]") == null && dom.document.querySelector("[data-kyo-held]") == null)
            )
        yield succeed
        end for
    }

    "a keyless mount whose placeholder holds a region shows each new instance's content as its region re-renders" in {
        // The shape of uic.SelectButton inside a re-rendering region: the mount's own `.placeholder` is a boundary around it, and
        // that placeholder is itself a region. Every re-render starts a new instance, whose content must reach the page.
        for
            tick  <- Signal.initRef(0)
            other <- Signal.initRef("a")
            ui = UI.div(
                UI.button("bump").id("bump").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.render(t =>
                    UI.mounted(Kyo.lift[UI, Async](UI.span(s"body-$t").id("body")))
                        .placeholder(other.render(o => UI.span(s"waiting-$o")))
                )
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            body = Sync.defer(Maybe(el("body")).fold("")(_.textContent))
            _ <- assertEventually(Sync.defer(ready.installed).andThen(body.map(_ == "body-0")))
            _ <- click("bump")
            _ <- assertEventually(body.map(_ == "body-1"))
            _ <- click("bump")
            _ <- assertEventually(body.map(_ == "body-2"))
            _ <- click("bump")
            _ <- assertEventually(body.map(_ == "body-3"))
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield succeed
        end for
    }

    // Regression: a mount's content sits at the mount's own path, so a mount with a placeholder whose content is directly another
    // mount with a placeholder (a uic.Select a component shows behind a skeleton) put two boundaries and two instances at one path.
    // Looked up by path alone, the inner boundary found the outer one while the inner instance was not registered yet, the outer
    // showed its content, which held the inner boundary again, and the engine recursed until the heap ran out; the page showed
    // neither.
    "a mount with a placeholder whose content is a mount with a placeholder shows it, and its handler runs" in {
        for
            clicks <- Signal.initRef(0)
            choice <- Signal.initRef(0)
            inner = (id: String) =>
                UI.mounted(Sync.defer(UI.button(id).id(id).onClick(clicks.getAndUpdate(_ + 1).unit))).placeholder(UI.span("inner skeleton"))
            direct    = UI.mounted(Sync.defer(inner("direct"))).keyed("k").placeholder(UI.span("outer skeleton"))
            throughIt = UI.mounted(Sync.defer(choice.render(_ => inner("rendered")))).placeholder(UI.span("outer skeleton"))
            ready     = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div(direct, throughIt), ready)))
            _     <- assertEventually(Sync.defer(ready.installed && el("direct") != null && el("rendered") != null))
            _     <- click("direct")
            _     <- click("rendered")
            _     <- assertEventually(clicks.get.map(_ == 2))
            n     <- clicks.get
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(n == 2)
    }

    /** A loading mount with a placeholder whose content is a mount with a placeholder, directly or through a region, both made in one
      * helper: a helper taking a `Frame` makes every node in it under its caller's, as a component library's do.
      */
    private def nestedUnderOneFrame(gate: Promise[Unit, Any], selected: SignalRef[String], throughRegion: Boolean)(using Frame): UI =
        def inner: UI =
            UI.mounted(Sync.defer(UI.span("picked").id("picked")))
                .placeholder(selected.render(v => UI.span(s"inner skeleton $v")))
        UI.mounted(gate.get.andThen(if throughRegion then selected.render(_ => inner) else inner))
            .keyed("k")
            .placeholder(UI.span("outer skeleton"))
    end nestedUnderOneFrame

    private def showsNestedUnderOneFrame(throughRegion: Boolean)(using Frame, kyo.test.AssertScope) =
        for
            gate     <- Promise.init[Unit, Any]
            selected <- Signal.initRef("a")
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.div(nestedUnderOneFrame(gate, selected, throughRegion)), ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text().contains("outer skeleton")))
            _     <- gate.completeUnitDiscard
            _     <- assertEventually(Sync.defer(el("picked") != null))
            shown = text()
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(!shown.contains("skeleton"), shown)

    "a mount with a placeholder whose content is a mount with a placeholder, made under one frame, shows it" in {
        showsNestedUnderOneFrame(throughRegion = false)
    }

    "a mount with a placeholder whose content renders a mount with a region for its placeholder, made under one frame, shows it" in {
        showsNestedUnderOneFrame(throughRegion = true)
    }

    /** A tab strip of sections, the way the component demo builds its pages: a mount with a placeholder (the strip) whose content
      * keeps its panel in a keyed list row, the panel holding keyed sections that are mounts with placeholders of their own, and a
      * slow neighbour that keeps the strip waiting. `innerFirst` lets the sections publish before the strip reveals.
      */
    private def sectionsInAStrip(innerFirst: Boolean, placeholderIsBody: Boolean = false, placeholderSameAsBody: Boolean = false)(using
        Frame,
        kyo.test.AssertScope
    ) =
        for
            outerGate <- Promise.init[Unit, Any]
            innerGate <- Promise.init[Unit, Any]
            slowGate  <- Promise.init[Unit, Any]
            innerRan  <- Promise.init[Unit, Any]
            panels    <- Signal.initRef(Chunk("examples"))
            section = (n: Int) =>
                UI.mounted(innerGate.get.andThen(innerRan.completeUnitDiscard).andThen(UI.span(s"live-$n").id(s"live-$n")))
                    .keyed(s"section-$n").placeholder(UI.span(s"static-$n").id(s"static-$n"))
            body = UI.div(
                UI.div(panels.foreachKeyed(identity)(_ => UI.div(section(1), section(2)))),
                UI.mounted(slowGate.get.andThen(UI.span("slow").id("slow")))
            )
            ui = UI.div(UI.mounted(outerGate.get.andThen(body)).placeholder(
                if placeholderSameAsBody then body
                else if placeholderIsBody then UI.div(UI.span("strip-loading").id("strip-loading"), body)
                else UI.span("strip-loading").id("strip-loading")
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && (el("strip-loading") != null || el("static-1") != null)))
            _     <- outerGate.completeUnitDiscard
            _     <- (if innerFirst then innerGate.completeUnitDiscard.andThen(innerRan.get) else Kyo.unit): Unit < Async
            _     <- slowGate.completeUnitDiscard
            _     <- assertEventually(Sync.defer(el("slow") != null))
            _     <- (if innerFirst then Kyo.unit else innerGate.completeUnitDiscard): Unit < Async
            _     <- assertEventually(Sync.defer(el("live-1") != null && el("live-2") != null))
            stale <- Sync.defer(el("static-1") == null && el("static-2") == null)
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(stale, s"a section kept its placeholder: ${text()}")

    "sections that publish before the strip around them reveals show their content" in {
        sectionsInAStrip(innerFirst = true)
    }

    "sections that publish after the strip around them reveals show their content" in {
        sectionsInAStrip(innerFirst = false)
    }

    // A tab strip's static projection is the strip itself, panels and sections included, so the sections stand in the
    // placeholder before they stand in the content.
    "sections in a strip whose placeholder is its own body show their content when they publish first" in {
        sectionsInAStrip(innerFirst = true, placeholderIsBody = true)
    }

    "sections in a strip whose placeholder is its own body show their content when they publish last" in {
        sectionsInAStrip(innerFirst = false, placeholderIsBody = true)
    }

    // The tab strip exactly: its placeholder IS its body, so the reveal morphs the body over itself, and each section's slot
    // is there under its own key already. A section that published while the strip was still waiting had its paint dropped
    // with everything else below the strip, so the strip's reveal is the only paint that brings its content.
    "sections in a strip whose placeholder is exactly its body show their content when they publish first" in {
        sectionsInAStrip(innerFirst = true, placeholderSameAsBody = true)
    }

    "sections in a strip whose placeholder is exactly its body show their content when they publish last" in {
        sectionsInAStrip(innerFirst = false, placeholderSameAsBody = true)
    }

end DomBackendBoundaryTest
