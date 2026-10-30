package kyo

import kyo.Browser.*

class RadioTest extends UITest:

    // ---- Rendering ----

    "type radio" in {
        withUI(UI.div(UI.radio.id("r"))) {
            Browser.assertAttribute(Selector.id("r"), "type", "radio").unit
        }
    }

    "checked true" in {
        withUI(UI.div(UI.radio.checked(true).id("r"))) {
            Browser.assertChecked(Selector.id("r")).unit
        }
    }

    "name attribute" in {
        withUI(UI.div(UI.radio.name("group1").id("r"))) {
            Browser.assertAttribute(Selector.id("r"), "name", "group1").unit
        }
    }

    // ---- Group behavior ----

    "same name check one unchecks other" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.radio.name("g").id("a").onChange(v => if v then ref.set("a") else Kyo.lift(())),
                UI.radio.name("g").id("b").onChange(v => if v then ref.set("b") else Kyo.lift(())),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertText(Selector.id("v"), "a")
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertText(Selector.id("v"), "b")
            yield ()
        }
    }

    "three radios same name cycle" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.radio.name("g").id("a").onChange(v => if v then ref.set("a") else Kyo.lift(())),
                UI.radio.name("g").id("b").onChange(v => if v then ref.set("b") else Kyo.lift(())),
                UI.radio.name("g").id("c").onChange(v => if v then ref.set("c") else Kyo.lift(())),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertText(Selector.id("v"), "a")
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertText(Selector.id("v"), "b")
                _ <- Browser.click(Selector.id("c"))
                _ <- Browser.assertText(Selector.id("v"), "c")
            yield ()
        }
    }

    "different name groups independent" in {
        val app: UI < Async =
            for
                g1 <- Signal.initRef("")
                g2 <- Signal.initRef("")
            yield UI.div(
                UI.radio.name("g1").id("a1").onChange(v => if v then g1.set("a1") else Kyo.lift(())),
                UI.radio.name("g1").id("b1").onChange(v => if v then g1.set("b1") else Kyo.lift(())),
                UI.radio.name("g2").id("a2").onChange(v => if v then g2.set("a2") else Kyo.lift(())),
                g1.map(v => UI.span(s"g1:$v").id("v1")),
                g2.map(v => UI.span(s"g2:$v").id("v2"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("a1"))
                _ <- Browser.assertText(Selector.id("v1"), "g1:a1")
                _ <- Browser.click(Selector.id("a2"))
                _ <- Browser.assertText(Selector.id("v2"), "g2:a2")
                _ <- Browser.assertText(Selector.id("v1"), "g1:a1")
            yield ()
        }
    }

    // ---- Interaction ----

    "check fires onChange true" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.radio.id("r").onChange(v => ref.set(v.toString)),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    // ---- Keyboard ----

    "pressKey Enter checks" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.radio.id("r").onChange(v => ref.set(v)),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("r"), Key.Enter)
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "pressKey Space checks" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.radio.id("r").onChange(v => ref.set(v)),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("r"), Key.Space)
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    // ---- Focus ----

    "focus assertFocused" in {
        withUI(UI.div(UI.radio.id("r"))) {
            for
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.assertVisible(Selector.id("r"))
            yield ()
        }
    }

    "onFocus fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.radio.id("r").onFocus(ref.set(true)),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "onBlur fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.radio.id("r").onBlur(ref.set(true)),
                UI.button("x").id("b"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    // ---- Disabled ----

    "disabled no fire" in {
        val app: UI < Async =
            for ref <- Signal.initRef(0)
            yield UI.div(
                UI.radio.id("r").disabled(true).onChange(_ => ref.getAndUpdate(_ + 1).unit),
                ref.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.assertDisabled(Selector.id("r"))
                _ <- Browser.assertText(Selector.id("v"), "0")
            yield ()
        }
    }

    "disabled signal toggle" in {
        val app: UI < Async =
            for
                disabled <- Signal.initRef(true)
                counter  <- Signal.initRef(0)
            yield UI.div(
                disabled.map(d => UI.radio.id("r").disabled(d).onChange(_ => counter.getAndUpdate(_ + 1).unit)),
                UI.button("Enable").id("en").onClick(disabled.set(false)),
                counter.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.assertDisabled(Selector.id("r"))
                _ <- Browser.click(Selector.id("en"))
                _ <- Browser.assertEnabled(Selector.id("r"))
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.assertText(Selector.id("v"), "1")
            yield ()
        }
    }

    // ---- Reactive ----

    "checked signal" in {
        val app: UI < Async =
            for flag <- Signal.initRef(true)
            yield UI.div(
                UI.button("Toggle").id("t").onClick(flag.getAndUpdate(!_).unit),
                flag.map(v => UI.radio.checked(v).id("r"))
            )
        withUI(app) {
            for
                _ <- Browser.assertChecked(Selector.id("r"))
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertVisible(Selector.id("r"))
            yield ()
        }
    }

    // ---- Merged from CheckboxRadioTest (radio scenarios) ----

    "radio exists (merged)" in {
        withUI(UI.div(UI.radio.id("r"))) {
            Browser.assertVisible(Selector.id("r")).unit
        }
    }

    "radio onChange fires (merged)" in {
        val app: UI < Async =
            for ref <- Signal.initRef("none")
            yield UI.div(
                UI.radio.id("r").onChange(b => ref.set(b.toString)),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("r"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "radio disabled (merged)" in {
        withUI(UI.div(UI.radio.id("r").disabled(true))) {
            Browser.assertDisabled(Selector.id("r")).unit
        }
    }

    // ---- Arrow keys in a group with an inert radio ----

    /** A group of radios with the ids `ids`, the one with id `b` shaped by `mid`, each reporting its changes to a log. */
    private def group(mid: UI.Ast.Radio => UI.Ast.Radio, ids: Seq[String] = Seq("a", "b", "c")): UI < Async =
        for
            log  <- Signal.initRef("")
            done <- Signal.initRef("")
        yield
            val radios = ids.map { id =>
                val r = UI.radio.name("g").id(id).onChange(v => log.updateAndGet(_ + s"$id$v,").unit)
                if id == "b" then mid(r) else r
            }
            UI.div(
                UI.div(UI.fragment(radios*)),
                UI.button("done").id("d").onClick(done.set("done")),
                log.map(v => UI.span(v).id("log")),
                done.map(v => UI.span(v).id("done"))
            )

    /** The selected radios and the focused one, read right after the key, then the log, read once a click after the key
      * has been answered: the session answers events in order, so every change the key caused is in the log by then.
      */
    private def pressed(on: String, key: Key)(using Frame): (String, String) < (Browser & Abort[BrowserReadException]) =
        val state =
            "(() => Array.from(document.querySelectorAll('input[type=radio]')).filter(r => r.checked).map(r => r.id).join('') + ' focus ' + " +
                "(document.activeElement && document.activeElement.id))()"
        for
            _   <- Browser.press(Selector.id(on), key)
            now <- kyo.internal.BrowserEval.evalJs(state)
            _   <- Browser.click(Selector.id("d"))
            _   <- Browser.assertText(Selector.id("done"), "done")
            log <- kyo.internal.BrowserEval.evalJs("document.getElementById('log').textContent")
        yield (now, log)
        end for
    end pressed

    "an arrow walks past a disabled radio by itself: the behavior an inert one has to match" in {
        withUI(group(_.disabled(true))) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertChecked(Selector.id("a"))
                r <- pressed("a", Key.ArrowDown)
            yield assert(r == ("c focus c", "atrue,ctrue,"), s"selected and focused, log: $r")
        }
    }

    "an arrow walks past an inert radio to the next one the reader may select, and reports that one" in {
        withUI(group(_.preventActivation)) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertChecked(Selector.id("a"))
                r <- pressed("a", Key.ArrowDown)
            yield assert(r == ("c focus c", "atrue,ctrue,"), s"selected and focused, log: $r")
        }
    }

    "an arrow back walks past an inert radio the same way" in {
        withUI(group(_.preventActivation)) {
            for
                _ <- Browser.click(Selector.id("c"))
                _ <- Browser.assertChecked(Selector.id("c"))
                r <- pressed("c", Key.ArrowLeft)
            yield assert(r == ("a focus a", "ctrue,atrue,"), s"selected and focused, log: $r")
        }
    }

    "an inert radio the reader is on, and that is not selected, is left for the next one the reader may select" in {
        withUI(group(_.preventActivation)) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertChecked(Selector.id("a"))
                r <- pressed("b", Key.ArrowDown)
            yield assert(r == ("c focus c", "atrue,ctrue,"), s"selected and focused, log: $r")
        }
    }

    "an inert radio that is selected keeps the group where it is" in {
        withUI(group(_.checked(true).preventActivation)) {
            for
                _ <- Browser.assertChecked(Selector.id("b"))
                r <- pressed("b", Key.ArrowDown)
            yield assert(r == ("b focus b", ""), s"selected and focused, log: $r")
        }
    }

    "in a right-to-left group Left walks forward and Right back, by itself" in {
        withUI(group(identity).map(g => UI.div(g).jsProp("dir", "rtl"))) {
            for
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertChecked(Selector.id("b"))
                r <- pressed("b", Key.ArrowLeft)
            yield assert(r == ("c focus c", "btrue,ctrue,"), s"selected and focused, log: $r")
        }
    }

    "in a right-to-left group the walk past an inert radio keeps that direction" in {
        withUI(group(_.preventActivation, Seq("a", "b", "c", "e")).map(g => UI.div(g).jsProp("dir", "rtl"))) {
            for
                _ <- Browser.click(Selector.id("c"))
                _ <- Browser.assertChecked(Selector.id("c"))
                r <- pressed("c", Key.ArrowRight)
            yield assert(r == ("a focus a", "ctrue,atrue,"), s"selected and focused, log: $r")
        }
    }

end RadioTest
