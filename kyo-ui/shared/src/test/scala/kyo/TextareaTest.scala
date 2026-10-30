package kyo

import kyo.Browser.*

class TextareaTest extends UITest:

    "textarea exists" in {
        withUI(UI.div(UI.textarea.id("t"))) {
            Browser.assertVisible(Selector.id("t")).unit
        }
    }

    "textarea placeholder" in {
        withUI(UI.div(UI.textarea.placeholder("hint").id("t"))) {
            Browser.assertAttribute(Selector.id("t"), "placeholder", "hint").unit
        }
    }

    "textarea disabled" in {
        withUI(UI.div(UI.textarea.disabled(true).id("t"))) {
            Browser.assertDisabled(Selector.id("t")).unit
        }
    }

    "textarea readonly" in {
        withUI(UI.div(UI.textarea.readOnly(true).id("t"))) {
            Browser.assertAttribute(Selector.id("t"), "readonly", "").unit
        }
    }

    "textarea onInput" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "typed text")
                _ <- Browser.assertText(Selector.id("v"), "typed text")
            yield ()
        }
    }

    "textarea onChange" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onChange(v => ref.set(v)),
                ref.map(v => UI.span(s"val:$v").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "hello")
                _ <- Browser.assertText(Selector.id("v"), "val:hello")
            yield ()
        }
    }

    "fill multiline text" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(v.length.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "line1\nline2\nline3")
                _ <- Browser.assertText(Selector.id("v"), "17")
            yield ()
        }
    }

    "fill 500 chars" in {
        val longText        = "a" * 500
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(v.length.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), longText)
                _ <- Browser.assertText(Selector.id("v"), "500")
            yield ()
        }
    }

    "fill replaces previous" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "first")
                _ <- Browser.assertText(Selector.id("v"), "first")
                _ <- Browser.fill(Selector.id("t"), "second")
                _ <- Browser.assertText(Selector.id("v"), "second")
            yield ()
        }
    }

    "fill empty string clears" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(s"[$v]").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "something")
                _ <- Browser.assertText(Selector.id("v"), "[something]")
                _ <- Browser.fill(Selector.id("t"), "")
                _ <- Browser.assertText(Selector.id("v"), "[]")
            yield ()
        }
    }

    "value SignalRef fill updates ref" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").value(ref).onInput(v => ref.set(v)),
                ref.map(v => UI.span(s"val:$v").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "typed")
                _ <- Browser.assertText(Selector.id("v"), "val:typed")
            yield ()
        }
    }

    "value SignalRef external set updates display" in {
        val app: UI < Async =
            for ref <- Signal.initRef("before")
            yield UI.div(
                UI.textarea.id("t").value(ref),
                UI.button("Set").id("b").onClick(ref.set("after"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("b"))
                v <- Browser.value(Selector.id("t"))
            yield assert(v == "after", s"textarea shows '$v', not the value written to the ref")
        }
    }

    // Regression: a textarea carries its value as content, not as a `value` attribute, so a ref write
    // reached the field only while it was focused. A click on another button moves the focus away, and a
    // clear from there left the typed text on screen while every other consumer of the ref had moved on.
    "a ref write clears a textarea the user has typed into" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").value(ref),
                UI.button("Clear").id("clear").onClick(ref.set("")),
                ref.map(v => UI.span(s"[$v]").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "typed")
                _ <- Browser.assertText(Selector.id("v"), "[typed]")
                _ <- Browser.click(Selector.id("clear"))
                _ <- Browser.assertText(Selector.id("v"), "[]")
                // The ref moving is not the claim; the FIELD showing it is.
                _ <- Browser.assertValueEmpty(Selector.id("t"))
            yield ()
        }
    }

    "a ref write replaces the text of a textarea the user has typed into" in {
        // Edge whitespace, a line break and every character HTML escapes arrive exactly as written.
        val tricky          = "  a\n<b> & \"c\" 'd'  "
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").value(ref),
                UI.button("Preset").id("preset").onClick(ref.set(tricky)),
                ref.map(v => UI.span(if v == tricky then "preset" else "other").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "typed")
                _ <- Browser.click(Selector.id("preset"))
                _ <- Browser.assertText(Selector.id("v"), "preset")
                v <- Browser.value(Selector.id("t"))
            yield assert(v == tricky, s"textarea shows '$v', not the value written to the ref")
        }
    }

    "value initial assertValue" in {
        withUI(UI.div(UI.textarea.id("t").value("initial"))) {
            Browser.value(Selector.id("t")).map(v => assert(v == "initial", s"textarea shows '$v'"))
        }
    }

    // Regression: the HTML parser drops one line break right after `<textarea>`, so a value that starts with one
    // reached the field without it.
    "a value that starts with a line break keeps it" in {
        withUI(UI.div(UI.textarea.id("t").value("\nsecond line"))) {
            Browser.value(Selector.id("t")).map(v =>
                assert(v == "\nsecond line", s"textarea shows '${v.replace("\n", "\\n")}'")
            )
        }
    }

    "line breaks arrive as written, CR LF and a lone CR as the LF a browser normalizes them to" in {
        val cases = Chunk(
            "\n\nthird line"      -> "\n\nthird line",
            "ends with a break\n" -> "ends with a break\n",
            "a\n\n\nb"            -> "a\n\n\nb",
            "\r\nwindows"         -> "\nwindows",
            "a\rb"                -> "a\nb"
        )
        Kyo.foreach(cases) { (written, expected) =>
            withUI(UI.div(UI.textarea.id("t").value(written))) {
                Browser.value(Selector.id("t")).map(v => (expected, v))
            }
        }.map(_.foreach((expected, v) => assert(v == expected, s"textarea shows '${v.replace("\n", "\\n")}'")))
    }

    "onInput and onChange both fire" in {
        val app: UI < Async =
            for
                inputRef  <- Signal.initRef("")
                changeRef <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => inputRef.set(v)).onChange(v => changeRef.set(v)),
                inputRef.map(v => UI.span(s"input:$v").id("vi")),
                changeRef.map(v => UI.span(s"change:$v").id("vc"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "test")
                _ <- Browser.assertText(Selector.id("vi"), "input:test")
                _ <- Browser.assertText(Selector.id("vc"), "change:test")
            yield ()
        }
    }

    "onInput with multiline text" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(v.length.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "a\nb\nc")
                _ <- Browser.assertText(Selector.id("v"), "5")
            yield ()
        }
    }

    "focus textarea" in {
        withUI(UI.div(UI.textarea.id("t"))) {
            for
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertVisible(Selector.id("t"))
            yield ()
        }
    }

    "onFocus fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.textarea.id("t").onFocus(ref.set(true)),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "onBlur fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.textarea.id("t").onBlur(ref.set(true)),
                UI.button("other").id("other"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("t"))
                _ <- Browser.click(Selector.id("other"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "disabled signal toggle" in {
        val app: UI < Async =
            for disabled <- Signal.initRef(true)
            yield UI.div(
                UI.textarea.id("t").disabled(disabled),
                UI.button("Toggle").id("tog").onClick(disabled.getAndUpdate(!_).unit)
            )
        withUI(app) {
            for
                _ <- Browser.assertDisabled(Selector.id("t"))
                _ <- Browser.click(Selector.id("tog"))
                _ <- Browser.assertEnabled(Selector.id("t"))
                _ <- Browser.click(Selector.id("tog"))
                _ <- Browser.assertDisabled(Selector.id("t"))
            yield ()
        }
    }

    "textarea special chars in value" in {
        withUI(UI.div(UI.textarea.value("<script>alert(1)</script>").id("t"))) {
            Browser.assertVisible(Selector.id("t")).unit
        }
    }

    // ---- Multiline ----

    "fill multiline text signal has newlines" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").value(ref).onInput(v => ref.set(v)),
                ref.map(v => UI.span(s"lines:${v.count(_ == '\n') + 1}").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "line1\nline2\nline3")
                _ <- Browser.assertText(Selector.id("v"), "lines:3")
            yield ()
        }
    }

    "textarea fill then pressKey appends" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").onInput(v => ref.set(v)),
                ref.map(v => UI.span(s"sig:$v").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "hello")
                _ <- Browser.assertText(Selector.id("v"), "sig:hello")
                _ <- Browser.press(Selector.id("t"), Key('!'))
                _ <- Browser.assertText(Selector.id("v"), "sig:hello!")
            yield ()
        }
    }

    "textarea with placeholder visible when empty" in {
        withUI(UI.div(UI.textarea.id("t").placeholder("Enter notes..."))) {
            Browser.assertAttribute(Selector.id("t"), "placeholder", "Enter notes...").unit
        }
    }

    "very long text signal preserves all" in {
        val longText        = "x" * 500
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.textarea.id("t").value(ref).onInput(v => ref.set(v)),
                ref.map(v => UI.span(s"len:${v.length}").id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), longText)
                _ <- Browser.assertText(Selector.id("v"), "len:500")
            yield ()
        }
    }

    // The browser keeps the typed text until the blur carries it: nothing binds the field or hears its typing.
    "a textarea's blur reports the text typed into it, unbound and from a constant" in {
        val app: UI < Async =
            for
                bare  <- Signal.initRef("none")
                start <- Signal.initRef("none")
            yield UI.div(
                UI.textarea.id("t").onBlur((_, v) => bare.set(s"[${v.replace("\n", "|")}]")),
                UI.textarea.id("c").value("x").onBlur((_, v) => start.set(s"[$v]")),
                UI.button("other").id("other"),
                bare.map(v => UI.span(v).id("vt")),
                start.map(v => UI.span(v).id("vc"))
            )
        withUI(app) {
            for
                _ <- Browser.fill(Selector.id("t"), "line1\nline2")
                _ <- Browser.click(Selector.id("other"))
                _ <- Browser.assertText(Selector.id("vt"), "[line1|line2]")
                _ <- Browser.fill(Selector.id("c"), "xy")
                _ <- Browser.click(Selector.id("other"))
                _ <- Browser.assertText(Selector.id("vc"), "[xy]")
            yield ()
        }
    }

end TextareaTest
