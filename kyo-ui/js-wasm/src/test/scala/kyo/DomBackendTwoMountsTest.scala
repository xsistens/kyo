package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** Two mounts on one page each own their session: tearing one down must leave the other's listeners delivering. */
class DomBackendTwoMountsTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def text(id: String): String =
        val el = dom.document.getElementById(id)
        if el == null then "" else el.textContent

    private def setViewportWidth(width: Int): Unit =
        discard(scalajs.Dynamic.global.Object.defineProperty(
            dom.window,
            "innerWidth",
            scalajs.Dynamic.literal(value = width, configurable = true)
        ))
        val resize = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].Event)("resize")
        discard(dom.window.asInstanceOf[scalajs.Dynamic].dispatchEvent(resize))
    end setViewportWidth

    "a viewport observer keeps delivering after another mount on the page is torn down" in {
        val observing: UI =
            UI.mounted(
                for
                    cmds <- UI.commands
                    id   <- cmds.freshId
                yield UI.div(
                    UI.div.id(id)("box"),
                    UI.mounted(
                        cmds.observeViewportById(id).map(rect =>
                            rect.render {
                                case Present(r) => (UI.span(f"w=${r.viewportWidth}%.0f").id("tm-obs"): UI)
                                case Absent     => (UI.span("pending").id("tm-obs"): UI)
                            }
                        )
                    ).placeholder(UI.span("pending").id("tm-obs"))
                )
            ).placeholder(UI.span("loading").id("tm-obs"))
        for
            _ <- Sync.defer {
                dom.document.body.innerHTML = """<div id="tm-a"></div><div id="tm-b"></div>"""
                setViewportWidth(800)
            }
            readyA = new DomTestEnv.MountReady
            readyB = new DomTestEnv.MountReady
            fiberA <- Fiber.initUnscoped(Scope.run(DomBackend.mount(observing, "#tm-a", readyA)))
            _      <- assertEventually(Sync.defer(readyA.installed && text("tm-obs") == "w=800"))
            fiberB <- Fiber.initUnscoped(Scope.run(DomBackend.mount(UI.span("other").id("tm-other"), "#tm-b", readyB)))
            _      <- assertEventually(Sync.defer(readyB.installed && text("tm-other") == "other"))
            _      <- fiberB.interrupt
            _      <- fiberB.getResult
            _      <- Sync.defer(setViewportWidth(500))
            _      <- assertEventually(Sync.defer(text("tm-obs") == "w=500"), 10.seconds)
            _      <- fiberA.interrupt
            _      <- fiberA.getResult
        yield succeed
        end for
    }

end DomBackendTwoMountsTest
