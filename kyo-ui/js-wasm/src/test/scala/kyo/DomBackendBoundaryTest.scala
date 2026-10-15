package kyo

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

end DomBackendBoundaryTest
