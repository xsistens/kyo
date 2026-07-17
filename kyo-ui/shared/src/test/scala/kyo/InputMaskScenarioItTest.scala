package kyo

import kyo.Browser.*

/** Unit 3: declarative client-local input filtering (`inputFilter`) and masking (`inputMask`).
  *
  * Drives the server-push transport (withUI -> runHandlers -> HtmlRenderer.clientJs) in real Chrome via a
  * document-level `beforeinput` capture listener. The DomBackend SPA mirror has no browser harness and is kept in
  * sync by construction. The input reflects its (filtered/masked) value into a span through `onInput`, so the
  * assertions read the value the SERVER received — the whole point of the feature (Prime sends the masked value).
  */
class InputMaskScenarioItTest extends UITest:

    "inputFilter emits data-kyo-filter" in {
        withUI(UI.div(UI.input.id("i").inputFilter("int"))) {
            Browser.assertAttribute(Selector.id("i"), "data-kyo-filter", "int").unit
        }
    }

    "inputMask emits data-kyo-mask" in {
        withUI(UI.div(UI.input.id("i").inputMask("(999) 999-9999"))) {
            Browser.assertAttribute(Selector.id("i"), "data-kyo-mask", "(999) 999-9999").unit
        }
    }

    "filter=int drops non-digits as they are typed" in {
        val app: UI < Async =
            for got <- Signal.initRef("")
            yield UI.div(
                UI.input.id("i").inputFilter("int").onInput(v => got.set(v)),
                got.map(v => UI.span(v).id("out"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("i"), Key('a'))
                _ <- Browser.press(Selector.id("i"), Key('1'))
                _ <- Browser.press(Selector.id("i"), Key('b'))
                _ <- Browser.press(Selector.id("i"), Key('2'))
                _ <- Browser.assertText(Selector.id("out"), "12")
            yield ()
        }
    }

    "mask auto-inserts literals as digits are typed" in {
        val app: UI < Async =
            for got <- Signal.initRef("")
            yield UI.div(
                UI.input.id("i").inputMask("(999) 999").onInput(v => got.set(v)),
                got.map(v => UI.span(v).id("out"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("i"), Key('1'))
                _ <- Browser.press(Selector.id("i"), Key('2'))
                _ <- Browser.press(Selector.id("i"), Key('3'))
                _ <- Browser.press(Selector.id("i"), Key('4'))
                _ <- Browser.press(Selector.id("i"), Key('5'))
                _ <- Browser.press(Selector.id("i"), Key('6'))
                _ <- Browser.assertText(Selector.id("out"), "(123) 456")
            yield ()
        }
    }

    "mask backspace deletes the digit before the literal and collapses trailing literals" in {
        val app: UI < Async =
            for got <- Signal.initRef("")
            yield UI.div(
                UI.input.id("i").inputMask("(999) 999").onInput(v => got.set(v)),
                got.map(v => UI.span(v).id("out"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("i"), Key('1'))
                _ <- Browser.press(Selector.id("i"), Key('2'))
                _ <- Browser.press(Selector.id("i"), Key('3'))
                _ <- Browser.press(Selector.id("i"), Key('4'))
                _ <- Browser.assertText(Selector.id("out"), "(123) 4")
                _ <- Browser.press(Selector.id("i"), Key.Backspace)
                _ <- Browser.assertText(Selector.id("out"), "(123")
            yield ()
        }
    }

end InputMaskScenarioItTest
