package kyo

import kyo.Browser.*

/** Unit 3: imperative Command op. A component uses the session channel ([[kyo.UI.commands]]) to send a whitelisted verb to
  * an element: the server emits `HtmlOp.Command`, the client applies it. v1 verbs: `focus`, `scrollIntoView`. Unknown verbs
  * are ignored by the client (forward-compat) — no crash, no effect.
  *
  * Drives the server-push transport in real Chrome. The `focus` verb targets an input whose `onFocus` writes a Signal, so
  * the resulting focus move is observable in the page text (rather than only via document.activeElement).
  */
class CommandScenarioItTest extends UITest:

    private def app: UI < Async =
        for focused <- Signal.initRef("none")
        yield UI.div(
            UI.input.id("target").tabIndex(0).onFocus(focused.set("target")),
            UI.button("focus").id("btn").onClick(UI.commands.flatMap(_.focus(Seq("0")))),
            UI.button("bogus").id("btn2").onClick(UI.commands.flatMap(_.command(Seq("0"), "totallyUnknownVerb"))),
            focused.map(v => UI.span(v).id("out"))
        )

    "Command focus moves focus to the target element" in {
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("btn"))
                // The input's onFocus fired -> the Command's element.focus() actually moved DOM focus to #target.
                _ <- Browser.assertText(Selector.id("out"), "target")
            yield ()
        }
    }

    "an unknown Command verb is a no-op (no crash, focus unchanged, later commands still work)" in {
        withUI(app) {
            for
                // Unknown verb first: must not focus the target and must not break the session.
                _ <- Browser.click(Selector.id("btn2"))
                _ <- Browser.assertText(Selector.id("out"), "none")
                // A real focus command afterwards still works — proves the unknown verb did not wedge the client.
                _ <- Browser.click(Selector.id("btn"))
                _ <- Browser.assertText(Selector.id("out"), "target")
            yield ()
        }
    }

end CommandScenarioItTest
