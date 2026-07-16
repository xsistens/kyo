package kyo

import kyo.Browser.*

/** Declarative focus seeding (`focusAuto`) and focus return (`focusRestore`).
  *
  * These suites drive the server-push transport (withUI → runHandlers → HtmlRenderer.clientJs) in real
  * Chrome, the same harness the focus-trap and focus/caret-restore tests use. DomBackend's SPA-mode
  * mirror of the same logic has no browser harness (the Node test env has no DOM, matching the module
  * convention documented in UIWindowTest); it is kept in sync by construction with the clientJs
  * implementation these tests pin down.
  */
class FocusAutoScenarioItTest extends UITest:

    "focusAuto and focusRestore emit their data attributes; absent by default" in {
        val app: UI < Async = UI.div(
            UI.div("seed").tabIndex(-1).focusAuto(true).focusRestore(true).id("seeded"),
            UI.div("plain").id("plain")
        )
        withUI(app) {
            for
                _ <- Browser.assertAttribute(Selector.id("seeded"), "data-kyo-focus-auto", "1")
                _ <- Browser.assertAttribute(Selector.id("seeded"), "data-kyo-focus-restore", "1")
                _ <- Browser.assertNoAttribute(Selector.id("plain"), "data-kyo-focus-auto")
                _ <- Browser.assertNoAttribute(Selector.id("plain"), "data-kyo-focus-restore")
            yield ()
        }
    }

    "opening a panel seeds focus into the focus-auto element (winning over restore-to-trigger)" in {
        val app: UI < Async =
            for showPanel <- Signal.initRef(false)
            yield UI.div(
                UI.button("Open").id("trigger").onClick(showPanel.set(true)),
                UI.when(showPanel)(
                    UI.div(
                        UI.button("Inside").id("inside")
                    ).id("panel").tabIndex(-1).focusAuto(true)
                )
            )
        withUI(app) {
            for
                // Real click: the trigger is the focused element when the panel appears, so without
                // seeding the client's focus restore would keep focus on the trigger.
                _ <- Browser.click(Selector.id("trigger"))
                _ <- Browser.assertVisible(Selector.id("panel"))
                _ <- Browser.assertFocused(Selector.id("panel"))
            yield ()
        }
    }

    "a re-render of an already-open focus-auto element does not steal the user's focus" in {
        // The focus-auto panel is present from the initial page load (initial-mount seeding, old set
        // empty — consistent with native autofocus). Bumping the counter re-renders the WHOLE region
        // containing the panel with identical paths; the panel must NOT re-seed, and the existing
        // focus restore must keep the user's focus on the inner button they clicked.
        val app: UI < Async =
            for n <- Signal.initRef(0)
            yield UI.div(
                n.map(i =>
                    UI.div(
                        UI.button(s"Inner").id("inner"),
                        UI.div("seed").tabIndex(-1).focusAuto(true).id("panel"),
                        UI.span(s"n=$i").id("status")
                    )
                ),
                UI.button("Bump").id("bump").onClick(n.getAndUpdate(_ + 1).unit)
            )
        withUI(app) {
            for
                // Initial-mount seeding: the panel is focused on page load.
                _ <- Browser.assertFocused(Selector.id("panel"))
                // The user moves focus elsewhere inside the region.
                _ <- Browser.click(Selector.id("inner"))
                _ <- Browser.assertFocused(Selector.id("inner"))
                // Bump via a synthetic JS click so the click itself does not move focus (same technique
                // as UITestSpaReactiveInputFocusTest); the region re-renders with the same paths.
                _ <- Browser.evalDiscard("document.getElementById('bump').click()")
                _ <- Browser.assertText(Selector.id("status"), "n=1")
                // No re-seed: focus stays on the inner button (kept there by the focus restore).
                _ <- Browser.assertFocused(Selector.id("inner"))
                _ <- Browser.assertNotFocused(Selector.id("panel"))
            yield ()
        }
    }

    "closing a focus-restore panel returns focus to the previously focused element" in {
        val app: UI < Async =
            for showPanel <- Signal.initRef(false)
            yield UI.div(
                UI.button("Open").id("trigger").onClick(showPanel.set(true)),
                UI.when(showPanel)(
                    UI.div(
                        UI.button("Close").id("close").onClick(showPanel.set(false))
                    ).id("panel").tabIndex(-1).focusAuto(true).focusRestore(true)
                )
            )
        withUI(app) {
            for
                // Real click focuses the trigger; opening seeds focus into the panel and records the
                // trigger's path as the return target.
                _ <- Browser.click(Selector.id("trigger"))
                _ <- Browser.assertFocused(Selector.id("panel"))
                // Close from inside the panel: the seeded element disappears, so the sweep pops the
                // stack entry and (focusRestore was set) returns focus to the trigger.
                _ <- Browser.click(Selector.id("close"))
                _ <- Browser.assertNotExists(Selector.id("panel"))
                _ <- Browser.assertFocused(Selector.id("trigger"))
            yield ()
        }
    }

end FocusAutoScenarioItTest
