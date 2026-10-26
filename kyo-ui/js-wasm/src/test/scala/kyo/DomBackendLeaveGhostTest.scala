package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** The SPA client's leave ghost: the body-level clone that plays a departing element's leave animation.
  *
  * jsdom performs no layout, so the source's box is given to it directly (an own `getBoundingClientRect`, which
  * `cloneNode` does not copy) and the assertions read what the ghost was told to be. How Chrome then resolves that box
  * is covered for the server-push twin in `EnterLeaveTransitionScenarioItTest`.
  */
class DomBackendLeaveGhostTest extends kyo.test.Test[Any]:

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

    "a leave ghost keeps its measured size against a class min-size and leaves max-size to the classes" in {
        // At <body> with position:fixed a percentage min-size from the source's classes resolves against the viewport:
        // a dropdown panel's min-width:100% beat the inline width and stretched the ghost to the window edge. The max
        // sizes stay unset so a leave class can still collapse the ghost through them.
        for
            visible <- Signal.initRef(true)
            ui = UI.div(
                UI.button("hide").id("ghost-hide").onClick(visible.set(false)),
                UI.when(visible)(UI.div("bye").id("ghost-box").leaveTransition("ghost-leave"))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("ghost-box") != null))
            _     <- Sync.defer {
                val rect: scalajs.Function0[scalajs.Any] =
                    () => scalajs.Dynamic.literal(left = 10.0, top = 20.0, width = 120.0, height = 30.0)
                dom.document.getElementById("ghost-box").asInstanceOf[scalajs.Dynamic].updateDynamic("getBoundingClientRect")(rect)
            }
            _   <- click("ghost-hide")
            _   <- assertEventually(Sync.defer(dom.document.querySelector("[data-kyo-ghost]") != null))
            box <- Sync.defer {
                val st = dom.document.querySelector("[data-kyo-ghost]").asInstanceOf[dom.html.Element].style
                (st.width, st.minWidth, st.maxWidth, st.height, st.minHeight, st.maxHeight, st.boxSizing)
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(box == ("120px", "0px", "", "30px", "0px", "", "border-box"), box.toString)
    }

end DomBackendLeaveGhostTest
