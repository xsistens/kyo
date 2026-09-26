package kyo

import kyo.Browser.*

/** Self-addressing: a reusable component learns its own DOM identity without ever knowing its structural render path.
  * It mounts (where `Env[Commands]` resolves at run time), mints session-unique ids via [[kyo.UI.Commands.freshId]],
  * stamps them on its OWN elements (`.id(theId)`), and drives id-addressed commands at itself (focusId, requestMeasureById).
  *
  * Server-push transport in real Chrome. The focus target's `onFocus` writes a Signal (focus observable in page text);
  * the measured box is a fixed 120x40 (deterministic rect); two `freshId` calls mint distinct ids.
  */
class SelfAddressScenarioItTest extends UITest:

    private def app: UI < Async =
        Kyo.lift(UI.div(
            UI.mounted(
                for
                    cmds    <- UI.commands
                    inputId <- cmds.freshId
                    boxId   <- cmds.freshId
                    tallId  <- cmds.freshId
                    focused <- Signal.initRef("none")
                    info    <- Signal.initRef("pending")
                    batch   <- Signal.initRef("pending")
                yield UI.div(
                    UI.input.id(inputId).tabIndex(0).onFocus(focused.set("target")),
                    UI.div.id(boxId).style(Style.width(120.px) ++ Style.height(40.px))("content"),
                    UI.div.id(tallId).style(Style.width(60.px) ++ Style.height(90.px))("tall"),
                    UI.button("focus").id("btn-focus").onClick(cmds.focusId(inputId)),
                    UI.button("measure").id("btn-measure").onClick(
                        cmds.requestMeasureById(boxId).map(r =>
                            info.set(f"${r.width}%.0f x ${r.height}%.0f vp ${r.viewportWidth > 0 && r.viewportHeight > 0}")
                        )
                    ),
                    UI.button("measure all").id("btn-batch").onClick(
                        // The same id twice: one request, one reply, and both positions hold it.
                        cmds.requestMeasureByIds(Seq(boxId, tallId, boxId)).map(rs =>
                            batch.set(rs.map(r => f"${r.width}%.0fx${r.height}%.0f").mkString(" "))
                        )
                    ),
                    UI.span(if inputId != boxId then "distinct" else "same").id("distinct"),
                    focused.map(v => UI.span(v).id("out")),
                    info.map(v => UI.span(v).id("minfo")),
                    batch.map(v => UI.span(v).id("binfo"))
                )
            ).placeholder(UI.span("loading...").id("ph"))
        ))

    "focusId moves DOM focus to the component's own self-stamped element" in {
        withUI(app) {
            for
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

    "requestMeasureByIds answers every id of a batch, in the order it was asked" in {
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("btn-batch"))
                _ <- Browser.assertText(Selector.id("binfo"), "120x40 60x90 120x40")
            yield ()
        }
    }

    "two freshId calls mint distinct ids" in {
        withUI(app) {
            Browser.assertText(Selector.id("distinct"), "distinct")
        }
    }

end SelfAddressScenarioItTest
