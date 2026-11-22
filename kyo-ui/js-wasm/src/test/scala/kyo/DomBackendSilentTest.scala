package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** The SPA client forwards what the element lists in `data-kyo-ev`, so a button silent except for blur (`silentExcept`) sends the reader's
  * click nowhere, while focus leaving it still reaches its `onBlur`. The click goes first, so the order of what arrives shows whether it
  * was sent at all.
  */
class DomBackendSilentTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** The reader's `event` on the element `id`: a native click, or focus leaving it. */
    private def dispatch(id: String, event: String)(using Frame): Unit < Sync =
        Sync.defer {
            val el = dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic]
            if event == "click" then discard(el.click())
            else discard(el.dispatchEvent(scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].FocusEvent)(event)))
        }

    "a button silent except for blur sends its click nowhere and still sends the blur that leaves it" in {
        for
            heard <- AtomicRef.init(Chunk.empty[String])
            ui = UI.div(
                UI.button("Locked").id("sil-button")
                    .onClick(heard.getAndUpdate(_.append("click")).unit)
                    .onBlur(heard.getAndUpdate(_.append("blur")).unit)
                    .silentExcept(true, "blur")
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("sil-button") != null))
            _     <- dispatch("sil-button", "click")
            _     <- dispatch("sil-button", "blur")
            _     <- assertEventually(heard.get.map(_.nonEmpty))
            got   <- heard.get
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(got == Chunk("blur"), s"the client sent $got, expected only the blur")
        end for
    }

    "a text field silent except for blur still sends the text it holds when focus leaves it" in {
        for
            sent <- AtomicRef.init(Chunk.empty[String])
            ui = UI.div(
                UI.input.id("sil-input")
                    .onBlur((_, text) => sent.getAndUpdate(_.append(text)).unit)
                    .silentExcept(true, "blur")
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("sil-input") != null))
            _     <- Sync.defer {
                dom.document.getElementById("sil-input").asInstanceOf[dom.html.Input].value = "typed"
            }
            _   <- dispatch("sil-input", "blur")
            _   <- assertEventually(sent.get.map(_.nonEmpty))
            got <- sent.get
            _   <- fiber.interrupt
            _   <- fiber.getResult
        yield assert(got == Chunk("typed"), s"the blur carried $got, expected the typed text")
        end for
    }

end DomBackendSilentTest
