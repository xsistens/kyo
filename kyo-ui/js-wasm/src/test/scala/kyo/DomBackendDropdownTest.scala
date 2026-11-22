package kyo

import kyo.internal.DomBackend
import org.scalajs.dom

/** A `UI.dropdown` in the SPA client works as it does in the server-push client (the dropdown block of `HtmlRenderer.clientJs`):
  * the trigger opens and closes the list, the keys move and pick, a pick reaches the dropdown's binding, and a silent dropdown
  * opens nothing and picks nothing. Each case mounts the dropdown and fires its events on the page, through the client's own
  * listeners.
  */
class DomBackendDropdownTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def el(id: String): dom.html.Element = dom.document.getElementById(id).asInstanceOf[dom.html.Element]

    private def clickOn(target: dom.Element): Unit =
        discard(target.dispatchEvent(
            scalajs.js.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.js.Dynamic].MouseEvent)(
                "click",
                scalajs.js.Dynamic.literal(bubbles = true)
            ).asInstanceOf[dom.Event]
        ))

    private def click(id: String): Unit = clickOn(el(id))

    private def key(id: String, k: String): Unit =
        discard(el(id).dispatchEvent(scalajs.js.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.js.Dynamic].KeyboardEvent)(
            "keydown",
            scalajs.js.Dynamic.literal(key = k, bubbles = true, cancelable = true)
        ).asInstanceOf[dom.Event]))

    private def open(id: String): Boolean = !el(s"$id-options").hasAttribute("hidden")

    private def highlighted(id: String): String =
        val hl = el(s"$id-options").querySelector("[data-kyo-dropdown-hl]")
        if hl == null then "" else hl.getAttribute("data-kyo-dropdown-val")

    /** `ui` mounted, `body` run once its trigger is on the page, the mount ended after. */
    private def mounted[A](ui: UI, first: String)(body: => A < (Async & Abort[Any]))(using
        Frame,
        kyo.test.AssertScope
    ): A < (Async & Abort[Any]) =
        val ready = new DomTestEnv.MountReady
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById(s"$first-trigger") != null))
            a     <- body
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield a
        end for
    end mounted

    private val options = Seq("Alpha" -> "a", "Beta" -> "b", "Gamma" -> "g")

    "a trigger click opens the list on its first option, and a second click closes it" in {
        for
            v <- Signal.initRef("a")
            r <- mounted(UI.div(UI.dropdown(options*).id("d").value(v)), "d") {
                Sync.defer {
                    click("d-trigger")
                    val opened = (open("d"), highlighted("d"))
                    click("d-trigger")
                    (opened, open("d"))
                }
            }
        yield assert(r == ((true, "a"), false), s"opened (open, highlighted), then open: $r")
    }

    "the arrows move the highlight round the list and Enter picks it: the binding takes the value and the list closes" in {
        for
            v <- Signal.initRef("a")
            r <- mounted(UI.div(UI.dropdown(options*).id("d").value(v)), "d") {
                for
                    moved <- Sync.defer {
                        click("d-trigger")
                        key("d-trigger", "ArrowDown")
                        val down = highlighted("d")
                        key("d-trigger", "ArrowUp")
                        key("d-trigger", "ArrowUp")
                        val wrapped = highlighted("d")
                        key("d-trigger", "ArrowDown")
                        key("d-trigger", "ArrowDown")
                        key("d-trigger", "Enter")
                        (down, wrapped, open("d"))
                    }
                    _ <- assertEventually(v.get.map(_ == "b"))
                yield moved
            }
        yield assert(r == ("b", "g", false), s"after ArrowDown, after two ArrowUp, open after Enter: $r")
    }

    "an option click picks it, and the binding takes the value" in {
        for
            v <- Signal.initRef("a")
            r <- mounted(UI.div(UI.dropdown(options*).id("d").value(v)), "d") {
                for
                    closed <- Sync.defer {
                        click("d-trigger")
                        val opt = el("d-options").querySelector("[data-kyo-dropdown-val='g']").asInstanceOf[dom.html.Element]
                        clickOn(opt)
                        !open("d")
                    }
                    _ <- assertEventually(v.get.map(_ == "g"))
                yield closed
            }
        yield assert(r, "the list is still open after the pick")
    }

    "Space opens a closed list, a printable key jumps to the next option starting with it, Escape closes" in {
        for
            v <- Signal.initRef("a")
            r <- mounted(UI.div(UI.dropdown(options*).id("d").value(v)), "d") {
                Sync.defer {
                    key("d-trigger", " ")
                    val spaced = open("d")
                    key("d-trigger", "g")
                    val jumped = highlighted("d")
                    key("d-trigger", "Escape")
                    (spaced, jumped, open("d"))
                }
            }
        yield assert(r == (true, "g", false), s"open after Space, highlighted after g, open after Escape: $r")
    }

    "opening one dropdown closes the other" in {
        for
            v <- Signal.initRef("a")
            w <- Signal.initRef("a")
            r <- mounted(UI.div(UI.dropdown(options*).id("d").value(v), UI.dropdown(options*).id("e").value(w)), "e") {
                Sync.defer {
                    click("d-trigger")
                    click("e-trigger")
                    (open("d"), open("e"))
                }
            }
        yield assert(r == (false, true), s"(d open, e open): $r")
    }

    "a silent or disabled dropdown opens nothing, by a click or by Space" in {
        for
            v <- Signal.initRef("a")
            r <- mounted(
                UI.div(UI.dropdown(options*).id("q").value(v).silent(true), UI.dropdown(options*).id("x").value(v).disabled(true)),
                "x"
            ) {
                Sync.defer {
                    click("q-trigger")
                    key("q-trigger", " ")
                    click("x-trigger")
                    (open("q"), open("x"))
                }
            }
        yield assert(r == (false, false), s"(silent open, disabled open): $r")
    }

    "a dropdown that turns silent while its list is open picks nothing by Enter; once it hears again, a pick does" in {
        for
            v     <- Signal.initRef("a")
            mute  <- Signal.initRef(false)
            heard <- AtomicRef.init(Chunk.empty[String])
            ui = UI.div(UI.dropdown(options*).id("d").value(v).silent(mute: Signal[Boolean])
                .onChange(x => heard.getAndUpdate(_.append(x)).unit))
            r <- mounted(ui, "d") {
                for
                    _ <- Sync.defer(click("d-trigger"))
                    _ <- mute.set(true)
                    _ <- assertEventually(Sync.defer(!el("d").hasAttribute("data-kyo-ev")))
                    // ArrowDown to Beta, then Enter: a pick of b the client must not send.
                    _ <- Sync.defer { key("d-trigger", "ArrowDown"); key("d-trigger", "Enter") }
                    _ <- mute.set(false)
                    _ <- assertEventually(Sync.defer(el("d").hasAttribute("data-kyo-ev")))
                    // Picks reach the dispatcher in order, so a sent b would be heard before this g.
                    _ <- Sync.defer {
                        click("d-trigger")
                        key("d-trigger", "g")
                        key("d-trigger", "Enter")
                    }
                    _  <- assertEventually(v.get.map(_ == "g"))
                    hs <- heard.get
                yield hs
            }
        yield assert(r == Chunk("g"), s"heard $r")
    }

end DomBackendDropdownTest
