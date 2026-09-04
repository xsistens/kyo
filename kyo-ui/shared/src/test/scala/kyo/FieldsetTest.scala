package kyo

import kyo.Browser.*

/** `<fieldset>` and `<legend>`, in a real browser, asserted through the accessibility tree and the
  * disabled cascade — the two things that separate the native pair from a `div` wearing
  * `role="group"` and an `aria-label`.
  *
  * Both claims are about the BROWSER's behaviour rather than the markup, which is why they are here
  * and not in a render test: nothing about the emitted HTML tells you that Chrome computed the
  * group's name from the legend, or that a control three levels down went disabled because an
  * ancestor did.
  */
class FieldsetTest extends UITest:

    "a fieldset is a group named by its legend" in {
        withUI(UI.div(UI.fieldset(UI.legend("Local settings"), UI.input.id("i")).id("fs"))) {
            for
                _ <- Browser.assertRole(Selector.id("fs"), "group")
                // Structural, not an aria-label: nothing in the markup names this group.
                _ <- Browser.assertAccessibleName(Selector.id("fs"), "Local settings")
            yield ()
        }
    }

    // `assertDisabled` is the WRONG probe for the cascade and it is worth saying why: it reads
    // `element.disabled`, and that IDL property reflects the control's OWN content attribute only.
    // An input inside a disabled fieldset reports `disabled === false` while being, in the spec's
    // words, "actually disabled". The effective state is what `:disabled` matches, so that is what
    // these assert.
    private def disabledCount(id: String) = Selector.css(s"#$id:disabled")

    "a disabled fieldset disables the controls inside it" in {
        withUI(
            UI.div(
                UI.fieldset(UI.legend("Off"), UI.div(UI.input.id("inner"))).disabled(true).id("fs")
            )
        ) {
            // The input says nothing about being disabled, and it is nested a level deeper than the
            // fieldset's own children. No `div` can do this, with or without a role.
            Browser.assertCount(disabledCount("inner"), 1).unit
        }
    }

    "a fieldset that is not disabled leaves its controls alone" in {
        withUI(UI.div(UI.fieldset(UI.legend("On"), UI.input.id("inner")).id("fs"))) {
            for
                _ <- Browser.assertEnabled(Selector.id("inner"))
                _ <- Browser.assertCount(disabledCount("inner"), 0)
            yield ()
        }
    }

    "the disabled cascade follows a signal" in {
        val app: UI < Async =
            for off <- Signal.initRef(true)
            yield UI.div(
                UI.fieldset(UI.legend("Group"), UI.input.id("i")).disabled(off).id("fs"),
                UI.button("Enable").id("en").onClick(off.set(false))
            )
        withUI(app) {
            for
                _ <- Browser.assertCount(disabledCount("i"), 1)
                _ <- Browser.click(Selector.id("en"))
                _ <- Browser.assertCount(disabledCount("i"), 0)
            yield ()
        }
    }
end FieldsetTest
