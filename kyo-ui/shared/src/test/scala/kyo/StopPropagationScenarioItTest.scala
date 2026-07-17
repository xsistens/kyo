package kyo

import kyo.Browser.*

/** Per-level event consumption via `stopPropagation(true)`.
  *
  * Default dispatch fires the handler of EVERY ancestor on the path that declared the event (bubble-through);
  * the flag makes an element that declared a handler for the arriving event's type consume it, so handlers
  * above it do not fire. The stop lives server-side in ReactiveUI.dispatchToElement (single source of truth
  * for both transports); these suites drive the server-push transport in real Chrome, like
  * FocusAutoScenarioItTest.
  */
class StopPropagationScenarioItTest extends UITest:

    "stopPropagation emits its marker attribute; absent by default" in {
        val app: UI < Async = UI.div(
            UI.div("stopper").id("s").stopPropagation(true).onKeyDown(_ => ()),
            UI.div("plain").id("plain").onKeyDown(_ => ())
        )
        withUI(app) {
            for
                _ <- Browser.assertAttribute(Selector.id("s"), "data-kyo-stop", "1")
                _ <- Browser.assertNoAttribute(Selector.id("plain"), "data-kyo-stop")
            yield ()
        }
    }

    "without the flag both nested onKeyDown handlers fire (regression pin for bubble-through)" in {
        val app: UI < Async =
            for
                aKey <- Signal.initRef("")
                bKey <- Signal.initRef("")
            yield UI.div(
                UI.div.id("a").onKeyDown(ke => aKey.set(ke.key.toString))(
                    UI.div.id("b").onKeyDown(ke => bKey.set(ke.key.toString))(
                        UI.input.id("i")
                    )
                ),
                aKey.map(v => UI.span(s"a:$v").id("av")),
                bKey.map(v => UI.span(s"b:$v").id("bv"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("i"), Key.Escape)
                _ <- Browser.assertText(Selector.id("bv"), "b:Escape")
                _ <- Browser.assertText(Selector.id("av"), "a:Escape")
            yield ()
        }
    }

    "keydown stops at the consuming inner container: only B's handler fires" in {
        val app: UI < Async =
            for
                aKey <- Signal.initRef("none")
                bKey <- Signal.initRef("none")
            yield UI.div(
                UI.div.id("a").onKeyDown(ke => aKey.set(ke.key.toString))(
                    UI.div.id("b").stopPropagation(true).onKeyDown(ke => bKey.set(ke.key.toString))(
                        UI.input.id("i")
                    )
                ),
                aKey.map(v => UI.span(s"a:$v").id("av")),
                bKey.map(v => UI.span(s"b:$v").id("bv"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("i"), Key.Escape)
                _ <- Browser.assertText(Selector.id("bv"), "b:Escape")
                // B consumed the event: A's handler never ran.
                _ <- Browser.assertText(Selector.id("av"), "a:none")
            yield ()
        }
    }

    "the flag only consumes event types the element declared: click passes through a keydown-stopper" in {
        val app: UI < Async =
            for
                aClicks <- Signal.initRef(0)
                bKey    <- Signal.initRef("none")
            yield UI.div(
                UI.div.id("a").onClick(aClicks.getAndUpdate(_ + 1).unit)(
                    UI.div.id("b").stopPropagation(true).onKeyDown(ke => bKey.set(ke.key.toString))(
                        UI.button("Go").id("btn")
                    )
                ),
                aClicks.map(n => UI.span(s"a:$n").id("av")),
                bKey.map(v => UI.span(s"b:$v").id("bv"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("btn"))
                // B declared no click handler, so its stop flag is inert for clicks: A still fires.
                _ <- Browser.assertText(Selector.id("av"), "a:1")
            yield ()
        }
    }

    "click consumption: a stopping inner handler keeps the outer onClick from firing" in {
        val app: UI < Async =
            for
                inner <- Signal.initRef(0)
                outer <- Signal.initRef(0)
            yield UI.div(
                UI.div.id("outer").onClick(outer.getAndUpdate(_ + 1).unit)(
                    UI.button("Go").id("btn").stopPropagation(true).onClick(inner.getAndUpdate(_ + 1).unit)
                ),
                inner.map(n => UI.span(s"in:$n").id("iv")),
                outer.map(n => UI.span(s"out:$n").id("ov"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("btn"))
                _ <- Browser.assertText(Selector.id("iv"), "in:1")
                _ <- Browser.assertText(Selector.id("ov"), "out:0")
            yield ()
        }
    }

    "nested overlays: Escape closes only the innermost layer" in {
        val app: UI < Async =
            for
                showOuter <- Signal.initRef(true)
                showInner <- Signal.initRef(true)
            yield UI.div(
                UI.when(showOuter)(
                    UI.div.id("overlay-outer").onKeyDown { ke =>
                        if ke.key == UI.Keyboard.Escape then showOuter.set(false)
                        else ()
                    }(
                        UI.span("outer content").id("outer-content"),
                        UI.when(showInner)(
                            UI.div.id("overlay-inner").stopPropagation(true).onKeyDown { ke =>
                                if ke.key == UI.Keyboard.Escape then showInner.set(false)
                                else ()
                            }(
                                UI.input.id("inner-input")
                            )
                        )
                    )
                ),
                UI.span("sentinel").id("sentinel")
            )
        withUI(app) {
            for
                // First Escape from inside the inner overlay: the inner layer consumes it and closes;
                // the outer overlay stays open.
                _ <- Browser.press(Selector.id("inner-input"), Key.Escape)
                _ <- Browser.assertNotExists(Selector.id("overlay-inner"))
                _ <- Browser.assertVisible(Selector.id("outer-content"))
            yield ()
        }
    }

end StopPropagationScenarioItTest
