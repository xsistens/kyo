package kyo

import kyo.Browser.*
import scala.language.implicitConversions

/** `UI.Bound.Follow` in real Chrome, over the server transport: what the browser does to a control before the event reaches the owner, and
  * that a decline takes it back. Trusted input through CDP, so the browser's own default actions run: a radio click clears its group.
  * [[DomBackendFollowTest]] holds the same cases for the browser mount, under jsdom.
  */
class UIBoundFollowDomTest extends UITest:

    private val t = Selector.id("t")

    "a keystroke its owner declined leaves the text the owner holds" in {
        withUI(Signal.initRef("held").map(held => UI.div(UI.textarea.id("t").value(held.readOnly).onInput(_ => ())))) {
            for
                _ <- Browser.focus(t)
                _ <- Browser.typeText("x")
                _ <- assertEventually(Browser.value(t).map(_ == "held"))
            yield succeed
        }
    }

    "a keystroke its owner took stays" in {
        withUI(Signal.initRef("").map(held => UI.div(UI.input.id("t").value(held.readOnly).onInput(held.set(_))))) {
            for
                _ <- Browser.focus(t)
                _ <- Browser.typeText("abc")
                _ <- assertEventually(Browser.value(t).map(_ == "abc"))
            yield succeed
        }
    }

    "an onChange-only field keeps what is typed until the commit puts it back" in {
        withUI(Signal.initRef("held").map(held =>
            UI.div(UI.input.id("t").value(held.readOnly).onChange(_ => ()), UI.button("out").id("out"))
        )) {
            for
                _     <- Browser.focus(t)
                _     <- Browser.typeText("x")
                typed <- Browser.value(t)
                _     <- Browser.focus(Selector.id("out"))
                _     <- assertEventually(Browser.value(t).map(_ == "held"))
            yield assert(typed == "heldx" || typed == "xheld", s"what was typed stayed until the commit: $typed")
        }
    }

    "a toggle its owner declined takes the box back" in {
        withUI(Signal.initRef(false).map(held => UI.div(UI.checkbox.id("c").checked(held.readOnly).onChange(_ => ())))) {
            for
                _ <- Browser.click(Selector.id("c"))
                _ <- Browser.assertNotChecked(Selector.id("c"))
            yield succeed
        }
    }

    "a toggle its owner took stays" in {
        withUI(Signal.initRef(false).map(held => UI.div(UI.checkbox.id("c").checked(held.readOnly).onChange(held.set(_))))) {
            for
                _ <- Browser.click(Selector.id("c"))
                _ <- Browser.assertChecked(Selector.id("c"))
            yield succeed
        }
    }

    "a radio its owner declined gives the group its selection back" in {
        withUI(
            for
                a <- Signal.initRef(true)
                b <- Signal.initRef(false)
            yield UI.div(
                UI.radio.id("a").name("g").checked(a.readOnly).onChange(_ => ()),
                UI.radio.id("b").name("g").checked(b.readOnly).onChange(_ => ())
            )
        ) {
            for
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertNotChecked(Selector.id("b"))
                _ <- Browser.assertChecked(Selector.id("a"))
            yield succeed
        }
    }

    "a radio with no handler, whose owner never takes it, gives the group its selection back" in {
        withUI(
            for
                a <- Signal.initRef(true)
                b <- Signal.initRef(false)
            yield UI.div(UI.radio.id("a").name("g").checked(a.readOnly), UI.radio.id("b").name("g").checked(b.readOnly))
        ) {
            for
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertNotChecked(Selector.id("b"))
                _ <- Browser.assertChecked(Selector.id("a"))
            yield succeed
        }
    }

    "a radio its owner took moves the group's selection" in {
        withUI(
            for
                held <- Signal.initRef("a")
            yield UI.div(
                UI.radio.id("a").name("g").checked(held.map(_ == "a")).onChange(_ => held.set("a")),
                UI.radio.id("b").name("g").checked(held.map(_ == "b")).onChange(_ => held.set("b"))
            )
        ) {
            for
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertChecked(Selector.id("b"))
                _ <- Browser.assertNotChecked(Selector.id("a"))
            yield succeed
        }
    }

    "a free radio beside a bound group keeps its tick" in {
        withUI(
            Signal.initRef(false).map(held =>
                UI.div(UI.radio.id("free").name("other"), UI.radio.id("b").name("g").checked(held.readOnly).onChange(_ => ()))
            )
        ) {
            for
                _ <- Browser.click(Selector.id("free"))
                _ <- Browser.assertChecked(Selector.id("free"))
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertNotChecked(Selector.id("b"))
                _ <- Browser.assertChecked(Selector.id("free"))
            yield succeed
        }
    }

    "a write to the signal reaches a field the reader already edited" in {
        withUI(
            Signal.initRef("held").map(held =>
                UI.div(UI.input.id("t").value(held.readOnly).onChange(_ => ()), UI.button("set").id("go").onClick(held.set("next")))
            )
        ) {
            for
                _ <- Browser.focus(t)
                _ <- Browser.typeText("x")
                _ <- Browser.click(Selector.id("go"))
                _ <- assertEventually(Browser.value(t).map(_ == "next"))
            yield succeed
        }
    }

    "a native select its owner declined shows what the owner holds" in {
        withUI(
            Signal.initRef("a").map(held =>
                UI.div(UI.select.id("s").value(held.readOnly).onChange(_ => ())(UI.option("A").value("a"), UI.option("B").value("b")))
            )
        ) {
            for
                _ <- Browser.eval("""var s=document.getElementById("s");s.value="b";s.dispatchEvent(new Event("change",{bubbles:true}));""")
                _ <- assertEventually(Browser.value(Selector.id("s")).map(_ == "a"))
            yield succeed
        }
    }

end UIBoundFollowDomTest
