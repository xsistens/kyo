package kyo

import kyo.Browser.*
import kyo.UI.render

/** Whether an INNER reactive region still works after the region around it re-rendered.
  *
  * This is the shape every component that keeps state under a reactive slot has:
  * `uic.ContextMenu` subscribes to its items' reactive `disabled` OUTSIDE the region that
  * watches its own open state, so a signal a menu item depends on emitting means the open
  * region is torn down and built again. If the rebuilt inner region were not subscribed, the
  * menu would set its open flag and nothing would repaint — which is exactly the symptom the
  * spotify example reported.
  */
class NestedRegionSurvivalTest extends UITest:

    private def app: UI < Async =
        for
            outer <- Signal.initRef("")
            inner <- Signal.initRef("")
        yield UI.div(
            outer.render(o => inner.render(i => UI.span(s"[$o|$i]").id("v"))),
            UI.button("outer").id("o").onClick(outer.getAndUpdate(_ + "o").unit),
            UI.button("inner").id("i").onClick(inner.getAndUpdate(_ + "i").unit)
        )

    "an inner region still updates after the outer one rebuilt it" in
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("v"), "[|]")
                _ <- Browser.click(Selector.id("i"))
                _ <- Browser.assertText(Selector.id("v"), "[|i]")
                // The outer emission rebuilds the inner region from scratch.
                _ <- Browser.click(Selector.id("o"))
                _ <- Browser.assertText(Selector.id("v"), "[o|i]")
                // And the rebuilt one has to be subscribed, or its ref writes into nothing.
                _ <- Browser.click(Selector.id("i"))
                _ <- Browser.assertText(Selector.id("v"), "[o|ii]")
                _ <- Browser.click(Selector.id("i"))
                _ <- Browser.assertText(Selector.id("v"), "[o|iii]")
            yield ()
        }

    "it survives the outer region rebuilding it repeatedly" in
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("o"))
                _ <- Browser.click(Selector.id("o"))
                _ <- Browser.click(Selector.id("o"))
                _ <- Browser.assertText(Selector.id("v"), "[ooo|]")
                _ <- Browser.click(Selector.id("i"))
                _ <- Browser.assertText(Selector.id("v"), "[ooo|i]")
            yield ()
        }

end NestedRegionSurvivalTest
