package kyo

import kyo.Browser.*
import kyo.internal.FilledClass
import kyo.internal.HtmlRenderer

/** `filledClass`: a text field's class follows whether it holds text, set in the client.
  *
  * The static render is checked here directly; the server-push client in Chrome, through what a reader does (typing,
  * clearing) and what the page does (a region drawn again over a typed field, a ref the program writes). The SPA client's
  * twin is `DomBackendFilledTest`. The last cases hold the script both clients share to the names in
  * `kyo.internal.FilledClass`.
  */
class FilledClassTest extends UITest:

    private def html(ui: UI)(using Frame): String < Sync = HtmlRenderer.render(ui, Seq.empty)

    // ---- the static render ----

    "the static render carries the class where the starting text is not empty, on every text field" in {
        for
            input  <- html(UI.input.value("x").filledClass("p-filled"))
            area   <- html(UI.textarea.value("x").filledClass("p-filled"))
            number <- html(UI.numberInput.value("4").filledClass("p-filled"))
            pass   <- html(UI.passwordInput.value("x").filledClass("p-filled"))
        yield
            assert(input.contains("""class="p-filled"""") && input.contains("""data-kyo-filled="p-filled""""), input)
            assert(area.contains("""class="p-filled""""), area)
            assert(number.contains("""class="p-filled""""), number)
            assert(pass.contains("""class="p-filled""""), pass)
        end for
    }

    "the static render reads a ref's text, and an empty or unbound field carries the marker without the class" in {
        for
            held  <- Signal.initRef("abc")
            none  <- Signal.initRef("")
            byRef <- html(UI.input.value(held).filledClass("p-filled"))
            empty <- html(UI.input.value(none).filledClass("p-filled"))
            const <- html(UI.input.value("").filledClass("p-filled"))
            bare  <- html(UI.input.filledClass("p-filled"))
        yield
            assert(byRef.contains("""class="p-filled""""), byRef)
            Seq(empty, const, bare).foreach(h =>
                assert(!h.contains("class=") && h.contains("""data-kyo-filled="p-filled""""), h)
            )
        end for
    }

    "the class joins the field's own classes, and a masked starting text counts as the field shows it" in {
        for
            own    <- html(UI.input.cssClass("p-inputtext").value("x").filledClass("p-filled"))
            masked <- html(UI.input.inputMask("(99)").value("12").filledClass("p-filled"))
        yield
            assert(own.contains("""class="p-inputtext p-filled""""), own)
            // The class is judged on the value as the field shows it, which is the masked one the render writes.
            val shown = "value=\"([^\"]*)\"".r.findFirstMatchIn(masked).map(_.group(1)).getOrElse("")
            assert(shown.startsWith("(1") && masked.contains("""class="p-filled""""), masked)
        end for
    }

    "with an ancestor target the static render marks the field and leaves the class to the component" in {
        html(UI.input.value("x").filledClass("p-inputwrapper-filled", ".wrap")).map { h =>
            assert(h.contains("""data-kyo-filled-up="p-inputwrapper-filled"""") && h.contains("""data-kyo-filled-on=".wrap""""), h)
            assert(!h.contains("class="), h)
        }
    }

    // ---- the server-push client, in Chrome ----

    private def has(id: String, cls: String)(using Frame) =
        Browser.evalBoolean(s"document.getElementById('$id').classList.contains('$cls')")

    "typing into a field nothing binds sets the class, and clearing it takes it off" in {
        withUI(UI.div(UI.input.id("i").filledClass("p-filled"))) {
            for
                before <- has("i", "p-filled")
                _      <- Browser.fill(Selector.id("i"), "Anna")
                typed  <- has("i", "p-filled")
                _      <- Browser.fill(Selector.id("i"), "")
                clear  <- has("i", "p-filled")
            yield assert((before, typed, clear) == (false, true, false), s"before, typed, cleared: ${(before, typed, clear)}")
        }
    }

    "a field started from a constant the reader empties loses the class" in {
        withUI(UI.div(UI.input.id("i").value("x").filledClass("p-filled"))) {
            for
                before <- has("i", "p-filled")
                _      <- Browser.fill(Selector.id("i"), "")
                clear  <- has("i", "p-filled")
            yield assert((before, clear) == (true, false), s"started, cleared: ${(before, clear)}")
        }
    }

    "an ancestor target takes the class, beside one on the field itself" in {
        withUI(UI.div.cssClass("wrap").id("w")(UI.input.id("i").filledClass("p-filled").filledClass("on", ".wrap"))) {
            for
                _     <- Browser.fill(Selector.id("i"), "Anna")
                typed <- has("w", "on")
                self  <- has("i", "p-filled")
                _     <- Browser.fill(Selector.id("i"), "")
                clear <- has("w", "on")
            yield assert(
                (typed, self, clear) == (true, true, false),
                s"wrapper typed, field typed, wrapper cleared: ${(typed, self, clear)}"
            )
        }
    }

    // The region draws the field again from what the server knows: no text for the unbound one, the constant for the other.
    // The browser keeps what the reader typed in both, so the class has to follow the field, not the render.
    "a region drawn again after typing leaves the class on what the field holds" in {
        val app: UI < Async =
            for n <- Signal.initRef(0)
            yield UI.div(
                n.map(k =>
                    UI.div(
                        UI.input.id("bare").filledClass("p-filled"),
                        UI.input.id("const").value("x").filledClass("p-filled"),
                        UI.span(k.toString).id("n")
                    )
                ),
                UI.button("again").id("again").onClick(n.getAndUpdate(_ + 1).unit)
            )
        withUI(app) {
            for
                _     <- Browser.fill(Selector.id("bare"), "Anna")
                _     <- Browser.fill(Selector.id("const"), "")
                _     <- Browser.click(Selector.id("again"))
                _     <- Browser.assertText(Selector.id("n"), "1")
                bare  <- has("bare", "p-filled")
                const <- has("const", "p-filled")
                texts <- Browser.evalJson[Seq[String]]("[document.getElementById('bare').value,document.getElementById('const').value]")
            yield assert(
                (bare, const) == (texts(0).nonEmpty, texts(1).nonEmpty),
                s"bare filled $bare over '${texts(0)}', const filled $const over '${texts(1)}'"
            )
        }
    }

    "a ref the program empties takes the class off, and one it fills puts it back" in {
        val app: UI < Async =
            for held <- Signal.initRef("abc")
            yield UI.div(
                UI.input.id("i").value(held).filledClass("p-filled"),
                UI.button("clear").id("clear").onClick(held.set("")),
                UI.button("fill").id("fill").onClick(held.set("z")),
                held.map(v => UI.span(s"[$v]").id("v"))
            )
        withUI(app) {
            for
                before <- has("i", "p-filled")
                _      <- Browser.click(Selector.id("clear"))
                _      <- Browser.assertText(Selector.id("v"), "[]")
                clear  <- has("i", "p-filled")
                _      <- Browser.click(Selector.id("fill"))
                _      <- Browser.assertText(Selector.id("v"), "[z]")
                again  <- has("i", "p-filled")
            yield assert((before, clear, again) == (true, false, true), s"started, cleared, filled: ${(before, clear, again)}")
        }
    }

    // ---- one rule for both clients ----

    "the shared script reads the names the Scala client reads, and the server-push client ships it" in {
        assert(FilledClass.classAttr == "data-kyo-filled" && FilledClass.upAttr == "data-kyo-filled-up")
        assert(FilledClass.targetAttr == "data-kyo-filled-on")
        Seq(FilledClass.classAttr, FilledClass.upAttr, FilledClass.targetAttr).foreach(a =>
            assert(FilledClass.js.contains(s""""$a""""), s"the script reads $a")
        )
        assert(FilledClass.js.contains("""f.value!==""""), "the script's rule is the one `filled` states: any text")
        assert(FilledClass.filled("a") && !FilledClass.filled(""))
        assert(HtmlRenderer.clientJs("").contains(FilledClass.js), "the server-push client carries the shared script")
    }

end FilledClassTest
