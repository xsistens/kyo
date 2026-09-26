package kyo

import kyo.internal.DomBackend
import org.scalajs.dom

class DomBackendChannelTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    "a channel write is on the page when set returns" in {
        for
            hot  <- Signal.initRef(false)
            text <- Signal.initRef("before")
            off  <- Signal.initRef(false)
            ui = UI.div(
                UI.span("x").id("chan-class").cssClass("hot", hot: Signal[Boolean]),
                UI.span("y").id("chan-attr").title(text: Signal[String]),
                UI.input.id("chan-bool").disabled(off: Signal[Boolean])
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("chan-class") != null))
            _     <- hot.set(true)
            _     <- text.set("after")
            _     <- off.set(true)
            seen  <- Sync.defer {
                val doc = dom.document
                (
                    doc.getElementById("chan-class").classList.contains("hot"),
                    doc.getElementById("chan-attr").getAttribute("title"),
                    doc.getElementById("chan-bool").hasAttribute("disabled")
                )
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(seen == (true, "after", true), s"class, title, disabled = $seen")
        end for
    }

    "a bound text region is written in place when set returns" in {
        // A Signal[String] in a child position is one text node: the write lands in that node, which keeps its
        // identity, and a string with markup characters arrives as literal text.
        for
            label <- Signal.initRef("before")
            ui    = UI.div(UI.span(label: Signal[String]).id("chan-text"))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("chan-text") != null))
            node  <- Sync.defer(textNodeOf("chan-text"))
            _     <- label.set("<b>&after</b>")
            seen  <- Sync.defer {
                val el = dom.document.getElementById("chan-text")
                (el.textContent, el.getElementsByTagName("b").length, textNodeOf("chan-text") eq node)
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(seen == ("<b>&after</b>", 0, true), s"text, markup elements, same node = $seen")
        end for
    }

    private def textNodeOf(id: String): dom.Node =
        val children = dom.document.getElementById(id).childNodes
        (0 until children.length).map(children(_)).find(_.nodeType == dom.Node.TEXT_NODE).orNull

end DomBackendChannelTest
