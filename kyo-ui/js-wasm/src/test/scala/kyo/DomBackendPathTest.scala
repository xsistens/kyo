package kyo

import kyo.UI.foreachKeyed
import kyo.internal.DomBackend
import org.scalajs.dom

class DomBackendPathTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    "a channel patch reaches a row whose key holds a quote and a backslash" in {
        val key = "a\"b\\c"
        for
            rows <- Signal.initRef(Chunk(key))
            hot  <- Signal.initRef(false)
            ui    = UI.div(UI.ul(rows.foreachKeyed(identity)(_ => UI.input.id("quoted-row").cssClass("hot", hot: Signal[Boolean]))))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("quoted-row") != null))
            path  <- Sync.defer(dom.document.getElementById("quoted-row").getAttribute("data-kyo-path"))
            _     <- hot.set(true)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("quoted-row").classList.contains("hot")))
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(path == s"0.0.$key", path)
        end for
    }

end DomBackendPathTest
