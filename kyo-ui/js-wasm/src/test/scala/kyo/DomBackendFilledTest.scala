package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** The SPA client's half of `filledClass`: the class follows whether the field holds text, as the reader types, as a
  * region is drawn again over a typed field, and as the program writes a bound ref. The server-push twin is held by
  * `FilledClassTest` in Chrome.
  */
class DomBackendFilledTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** The reader leaving `text` in the field `id`, which the browser keeps, as one input event. */
    private def typeInto(id: String, text: String)(using Frame): Unit < Sync =
        Sync.defer {
            val field = dom.document.getElementById(id)
            field.asInstanceOf[scalajs.Dynamic].value = text
            val input = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].Event)(
                "input",
                scalajs.Dynamic.literal(bubbles = true)
            )
            discard(field.asInstanceOf[scalajs.Dynamic].dispatchEvent(input))
        }

    private def has(id: String, cls: String)(using Frame): Boolean < Sync =
        Sync.defer(dom.document.getElementById(id).classList.contains(cls))

    private def valueOf(id: String)(using Frame): String < Sync =
        Sync.defer(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic].value.asInstanceOf[String])

    "typing sets the class and clearing takes it off, on the field and on an ancestor target" in {
        for
            _ <- Kyo.unit
            ui = UI.div(
                UI.input.id("fl-bare").filledClass("p-filled"),
                UI.input.id("fl-const").value("x").filledClass("p-filled"),
                UI.div.cssClass("fl-wrap").id("fl-wrap")(UI.textarea.id("fl-area").filledClass("on", ".fl-wrap"))
            )
            ready = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("fl-area") != null))
            started  <- has("fl-const", "p-filled")
            _        <- typeInto("fl-bare", "Anna")
            typed    <- has("fl-bare", "p-filled")
            _        <- typeInto("fl-bare", "")
            cleared  <- has("fl-bare", "p-filled")
            _        <- typeInto("fl-const", "")
            emptied  <- has("fl-const", "p-filled")
            _        <- typeInto("fl-area", "line")
            wrapped  <- has("fl-wrap", "on")
            _        <- typeInto("fl-area", "")
            released <- has("fl-wrap", "on")
            _        <- fiber.interrupt
            _        <- fiber.getResult
        yield assert(
            (started, typed, cleared, emptied, wrapped, released) == (true, true, false, false, true, false),
            s"started, typed, cleared, const emptied, wrapper on, wrapper off: ${(started, typed, cleared, emptied, wrapped, released)}"
        )
        end for
    }

    "a region drawn again after typing leaves the class on what the field holds" in {
        for
            n <- Signal.initRef(0)
            ui = UI.div(n.map(k =>
                UI.div(
                    UI.input.id("fr-bare").filledClass("p-filled"),
                    UI.input.id("fr-const").value("x").filledClass("p-filled"),
                    UI.span(k.toString).id("fr-n")
                )
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("fr-n") != null))
            _     <- typeInto("fr-bare", "Anna")
            _     <- typeInto("fr-const", "")
            _     <- n.set(1)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("fr-n").textContent == "1"))
            bare  <- has("fr-bare", "p-filled")
            const <- has("fr-const", "p-filled")
            tb    <- valueOf("fr-bare")
            tc    <- valueOf("fr-const")
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert((bare, const) == (tb.nonEmpty, tc.nonEmpty), s"bare filled $bare over '$tb', const filled $const over '$tc'")
        end for
    }

    "a ref the program empties takes the class off, and one it fills puts it back" in {
        for
            held <- Signal.initRef("abc")
            ui    = UI.div(UI.input.id("ff-ref").value(held).filledClass("p-filled"))
            ready = new DomTestEnv.MountReady
            fiber   <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _       <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("ff-ref") != null))
            started <- has("ff-ref", "p-filled")
            _       <- held.set("")
            _       <- assertEventually(valueOf("ff-ref").map(_.isEmpty))
            cleared <- has("ff-ref", "p-filled")
            _       <- held.set("z")
            _       <- assertEventually(valueOf("ff-ref").map(_ == "z"))
            again   <- has("ff-ref", "p-filled")
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield assert((started, cleared, again) == (true, false, true), s"started, cleared, filled: ${(started, cleared, again)}")
        end for
    }

end DomBackendFilledTest
