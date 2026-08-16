package kyo

import kyo.Browser.*

/** Viewport observation exposed as a Signal. [[kyo.UI.Commands.observeViewportById]] returns a `Signal[Maybe[Rect]]` the
  * component RENDERS: the kyo-native shape for a continuous stream is a Signal, not a callback. On
  * `HtmlOp.ObserveViewportById` the client measures the element now and attaches scroll/resize listeners; each reply
  * (`UIEvent.MeasureById`) is pushed into the backing SignalRef by `deliverMeasureById`, so the rendered text tracks the
  * element's live geometry (`Absent` until the first measure lands).
  *
  * Drives the server-push transport in real Chrome. The observed box is a fixed 120x40, so the first reported size is
  * deterministic. The observe is wired from a NESTED mount so it runs AFTER the box is inserted (getElementById would miss
  * an element observed before its own insertion).
  */
class ObserveViewportScenarioItTest extends UITest:

    private def app: UI < Async =
        Kyo.lift(UI.mounted(
            for
                cmds <- UI.commands
                id   <- cmds.freshId
            yield UI.div(
                UI.div.id(id).style(Style.width(120.px) ++ Style.height(40.px))("box"),
                UI.mounted(
                    cmds.observeViewportById(id).map(rectSig =>
                        rectSig.render {
                            case Present(r) => (UI.span(f"${r.width}%.0f x ${r.height}%.0f").id("obs"): UI)
                            case Absent     => (UI.span("pending").id("obs"): UI)
                        }
                    )
                ).placeholder(UI.span("pending").id("obs"))
            )
        ).placeholder(UI.span("loading").id("obs")))

    "observeViewportById exposes the element's live geometry as a Signal (initial measure lands)" in {
        withUI(app) {
            for _ <- Browser.assertText(Selector.id("obs"), "120 x 40")
            yield ()
        }
    }

end ObserveViewportScenarioItTest
