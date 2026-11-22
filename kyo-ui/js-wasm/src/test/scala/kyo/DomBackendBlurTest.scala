package kyo

import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** The SPA client's blur on a text field carries the text the field holds as focus leaves it, read off the element, so
  * the two-parameter `onBlur` hears what the reader typed into a field nothing binds. The server-push twin is held by `InputTest`,
  * `TextareaTest` and `NumberInputTest` in Chrome.
  */
class DomBackendBlurTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** The reader typing `text` into the field `id`, which the browser keeps, then focus leaving it. */
    private def typeAndLeave(id: String, text: String)(using Frame): Unit < Sync =
        Sync.defer {
            val field = dom.document.getElementById(id)
            field.asInstanceOf[scalajs.Dynamic].value = text
            val blur = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].FocusEvent)("blur")
            discard(field.asInstanceOf[scalajs.Dynamic].dispatchEvent(blur))
        }

    "a blur on a text field nothing binds carries what the reader typed, and the payload-free onBlur still runs" in {
        for
            heard <- AtomicRef.init(Chunk.empty[String])
            plain <- AtomicRef.init(0)
            ui = UI.div(
                UI.input.id("bl-input").onBlur(plain.getAndUpdate(_ + 1).unit).onBlur((_, v) =>
                    heard.getAndUpdate(_.append(s"input:$v")).unit
                ),
                UI.input.id("bl-const").value("x").onBlur((_, v) => heard.getAndUpdate(_.append(s"const:$v")).unit),
                UI.textarea.id("bl-area").onBlur((_, v) => heard.getAndUpdate(_.append(s"area:$v")).unit),
                UI.numberInput.id("bl-number").onBlur((_, v) => heard.getAndUpdate(_.append(s"number:$v")).unit)
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("bl-number") != null))
            _     <- typeAndLeave("bl-input", "ab")
            _     <- assertEventually(heard.get.map(_.size == 1))
            _     <- typeAndLeave("bl-const", "xy")
            _     <- assertEventually(heard.get.map(_.size == 2))
            _     <- typeAndLeave("bl-area", "line")
            _     <- assertEventually(heard.get.map(_.size == 3))
            _     <- typeAndLeave("bl-number", "42")
            _     <- assertEventually(heard.get.map(_.size == 4))
            got   <- heard.get
            ran   <- plain.get
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(
            got == Chunk("input:ab", "const:xy", "area:line", "number:42") && ran == 1,
            s"heard $got, the payload-free onBlur ran $ran times"
        )
        end for
    }

end DomBackendBlurTest
