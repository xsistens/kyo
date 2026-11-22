package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** On the in-browser engine a boundary's `onNotice` sink reaches a failing click handler, with the same app code as under server-push. */
class DomBackendNoticeTest extends kyo.test.Test[Any]:

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

    "a boundary's sink reaches a click handler" in {
        for
            seen <- Promise.init[String, Any]
            ui = UI.div(UI.boundary.onNotice(t => seen.completeDiscard(Result.succeed(t.getMessage)))(UI.div(
                UI.button("go").id("go").onClick(Abort.fail(new RuntimeException("click failed")))
            )))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("go") != null))
            _     <- click("go")
            got   <- seen.get
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(got == "click failed", s"the sink received: $got")
    }

end DomBackendNoticeTest
