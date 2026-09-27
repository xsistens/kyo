package kyo

import kyo.UI.foreachKeyed
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

class DomBackendPathTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    "a command reaches a row whose key holds a quote and a backslash" in {
        val key = "a\"b\\c"
        for
            rows <- Signal.initRef(Chunk(key))
            ui = UI.div(
                UI.ul(rows.foreachKeyed(identity)(_ => UI.input.id("quoted-row"))),
                UI.button("go").id("quoted-go").onClick(UI.commands.map(_.focus(Seq("0", "0", key))))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("quoted-row") != null))
            path  <- Sync.defer(dom.document.getElementById("quoted-row").getAttribute("data-kyo-path"))
            _     <- Sync.defer {
                val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                    "click",
                    scalajs.Dynamic.literal(bubbles = true)
                )
                discard(dom.document.getElementById("quoted-go").asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
            }
            _ <- assertEventually(Sync.defer(Maybe(dom.document.activeElement).exists(_.id == "quoted-row")))
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(path == s"0.0.$key", path)
        end for
    }

end DomBackendPathTest
