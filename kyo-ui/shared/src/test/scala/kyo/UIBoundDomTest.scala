package kyo

import kyo.Browser.*
import kyo.UI.render
import scala.language.implicitConversions

/** `UI.Bound` in real Chrome, over the server transport: every binding (a constant, a signal, a ref) holds a native control once an edit's
  * handlers and the regions they woke have run, and a default leaves it to the reader. Trusted input through CDP, so the browser's own
  * default actions run. [[DomBackendNativeStateTest]] holds the same cases for the browser mount, under jsdom.
  */
class UIBoundDomTest extends UITest:

    private val c = Selector.id("c")

    "a tick its owner declined on a box bound to a constant in a region is taken back" in {
        withUI(Signal.initRef(false).map(model => UI.div(model.render(v => UI.checkbox.id("c").checked(v).onChange(_ => ()))))) {
            for
                _ <- Browser.click(c)
                _ <- Browser.assertNotChecked(c)
            yield succeed
        }
    }

    "a box bound to a constant with no handler is taken back" in {
        withUI(UI.div(UI.checkbox.id("c").checked(true))) {
            for
                _ <- Browser.click(c)
                _ <- Browser.assertChecked(c)
            yield succeed
        }
    }

    "a ref whose hearer writes the old value back takes the box back" in {
        withUI(Signal.initRef(false).map(model => UI.div(UI.checkbox.id("c").checked(model).onChange(on => model.set(!on))))) {
            for
                _ <- Browser.click(c)
                _ <- Browser.assertNotChecked(c)
            yield succeed
        }
    }

    "a click puts back the indeterminate state a box states" in {
        withUI(UI.div(UI.checkbox.id("c").checked(false).indeterminate(true).onChange(_ => ()))) {
            for
                _ <- Browser.click(c)
                _ <- Browser.assertNotChecked(c)
                _ <- assertEventually(Browser.eval("""document.getElementById("c").indeterminate""").map(_ == "true"))
            yield succeed
        }
    }

    "a box with a default keeps the reader's tick through a repaint around it" in {
        withUI(Signal.initRef(0).map(other =>
            UI.div(
                other.render(o => UI.div(UI.checkbox.id("c").defaultChecked(false).onChange(_ => ()), UI.span(s"o=$o").id("o"))),
                UI.button("again").id("again").onClick(other.getAndUpdate(_ + 1).unit)
            )
        )) {
            for
                _ <- Browser.click(c)
                _ <- Browser.assertChecked(c)
                _ <- Browser.click(Selector.id("again"))
                _ <- assertEventually(Browser.eval("""document.getElementById("o").textContent""").map(_ == "o=1"))
                _ <- Browser.assertChecked(c)
            yield succeed
        }
    }

    "a field bound to a constant takes back a keystroke its owner declined" in {
        withUI(UI.div(UI.input.id("t").value("held").onInput(_ => ()))) {
            for
                _ <- Browser.focus(Selector.id("t"))
                _ <- Browser.typeText("x")
                _ <- assertEventually(Browser.value(Selector.id("t")).map(_ == "held"))
            yield succeed
        }
    }

    "a field with a default keeps what the reader types" in {
        withUI(UI.div(UI.input.id("t").defaultValue("start").onInput(_ => ()), UI.button("out").id("out"))) {
            for
                _     <- Browser.focus(Selector.id("t"))
                _     <- Browser.typeText("x")
                _     <- Browser.focus(Selector.id("out"))
                typed <- Browser.value(Selector.id("t"))
            yield assert(typed == "startx" || typed == "xstart", s"the reader's text stays: $typed")
        }
    }

    "a select whose option is selected by a constant takes back a pick" in {
        withUI(UI.div(UI.select.id("s")(UI.option("A").value("a"), UI.option("B").value("b").selected(true)))) {
            for
                _ <- Browser.eval("""var s=document.getElementById("s");s.value="a";s.dispatchEvent(new Event("change",{bubbles:true}));""")
                _ <- assertEventually(Browser.value(Selector.id("s")).map(_ == "b"))
            yield succeed
        }
    }

end UIBoundDomTest
