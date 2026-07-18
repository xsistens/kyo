package kyo

import kyo.Browser.*

/** Self-addressing: a reusable component learns its own DOM identity without ever knowing its structural render path.
  *
  * The component mounts (`UI.mounted`, where `Env[Commands]` resolves at run time), mints session-unique ids via
  * [[kyo.UI.Commands.freshId]], stamps them on its OWN elements (`.id(theId)` — id is a settable prop), and drives the
  * id-addressed commands at itself: [[kyo.UI.Commands.focusId]] (`HtmlOp.CommandById` → `document.getElementById`) and
  * [[kyo.UI.Commands.requestMeasureById]] (`HtmlOp.RequestMeasureById` → `UIEvent.MeasureById` → `deliverMeasureById`).
  *
  * Drives the server-push transport in real Chrome. The focus target's `onFocus` writes a Signal, so the focus move is
  * observable in the page text; the measured box is a fixed 120x40 so its rect is deterministic. Two `freshId` calls
  * mint distinct ids (proven in the page rather than asserted structurally).
  */
class SelfAddressScenarioItTest extends UITest:

    private def app: UI < Async =
        Kyo.lift(UI.div(
            UI.mounted(
                for
                    cmds    <- UI.commands
                    inputId <- cmds.freshId
                    boxId   <- cmds.freshId
                    focused <- Signal.initRef("none")
                    info    <- Signal.initRef("pending")
                yield UI.div(
                    // The component stamps its own minted ids onto its own elements.
                    UI.input.id(inputId).tabIndex(0).onFocus(focused.set("target")),
                    UI.div.id(boxId).style(Style.width(120.px) ++ Style.height(40.px))("content"),
                    // ...and drives id-addressed commands at itself — no render path involved.
                    UI.button("focus").id("btn-focus").onClick(cmds.focusId(inputId)),
                    UI.button("measure").id("btn-measure").onClick(
                        cmds.requestMeasureById(boxId)(r =>
                            info.set(f"${r.width}%.0f x ${r.height}%.0f vp ${r.viewportWidth > 0 && r.viewportHeight > 0}")
                        )
                    ),
                    UI.span(if inputId != boxId then "distinct" else "same").id("distinct"),
                    focused.map(v => UI.span(v).id("out")),
                    info.map(v => UI.span(v).id("minfo"))
                )
            ).placeholder(UI.span("loading...").id("ph"))
        ))

    "focusId moves DOM focus to the component's own self-stamped element" in {
        withUI(app) {
            for
                // The component minted the input's id and drives focus at it by id — the onFocus fired.
                _ <- Browser.click(Selector.id("btn-focus"))
                _ <- Browser.assertText(Selector.id("out"), "target")
            yield ()
        }
    }

    "requestMeasureById delivers a plausible rect for the component's own self-stamped element" in {
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("btn-measure"))
                _ <- Browser.assertText(Selector.id("minfo"), "120 x 40 vp true")
            yield ()
        }
    }

    "two freshId calls mint distinct ids" in {
        withUI(app) {
            Browser.assertText(Selector.id("distinct"), "distinct")
        }
    }

end SelfAddressScenarioItTest
