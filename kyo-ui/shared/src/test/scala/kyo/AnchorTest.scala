package kyo

import kyo.Browser.*
import kyo.Length.*
import kyo.UI.*
import kyo.UI.Ast.*

class AnchorTest extends UITest:

    "anchor renders with text" in {
        withUI(UI.div(UI.a.href(Href.Absolute(HttpUrl.parse("https://example.com").getOrThrow))("Link").id("a"))) {
            Browser.assertText(Selector.id("a"), "Link").unit
        }
    }

    "anchor href attribute" in {
        withUI(UI.div(UI.a.href(Href.Absolute(HttpUrl.parse("https://example.com").getOrThrow)).id("a"))) {
            Browser.assertAttributeSatisfies(Selector.id("a"), "href", "ignore")(_.contains("example.com")).unit
        }
    }

    "anchor target blank" in {
        withUI(UI.div(UI.a.href(Href.Path("url"), UI.Target.Blank).id("a"))) {
            Browser.assertAttribute(Selector.id("a"), "target", "_blank").unit
        }
    }

    "anchor target self" in {
        withUI(UI.div(UI.a.href(Href.Path("url")).target(UI.Target.Self).id("a"))) {
            Browser.assertAttribute(Selector.id("a"), "target", "_self").unit
        }
    }

    "anchor target parent" in {
        withUI(UI.div(UI.a.href(Href.Path("url")).target(UI.Target.Parent).id("a"))) {
            Browser.assertAttribute(Selector.id("a"), "target", "_parent").unit
        }
    }

    "anchor target top" in {
        withUI(UI.div(UI.a.href(Href.Path("url")).target(UI.Target.Top).id("a"))) {
            Browser.assertAttribute(Selector.id("a"), "target", "_top").unit
        }
    }

    "anchor no target" in {
        withUI(UI.div(UI.a.href(Href.Path("url")).id("a"))) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("a"), "target", "no target attribute")(_.isEmpty)
            yield ()
        }
    }

    "anchor onClick" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.a.href(Href.Fragment("")).id("a").onClick(ref.set(true))("Click"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "anchor focus" in {
        withUI(UI.div(UI.a.href(Href.Fragment("")).id("a")("Link"))) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.assertVisible(Selector.id("a"))
            yield ()
        }
    }

    "anchor onFocus fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.a.href(Href.Fragment("")).id("a").onFocus(ref.set(true))("Link"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            // Windows Chrome can drop the synthetic click so the anchor never gains focus and onFocus never fires.
            // Retry click+assert together; the click is idempotent once focus lands.
            for
                _ <- Retry[BrowserReadException](Schedule.fixed(300.millis).take(15)) {
                    Browser.click(Selector.id("a")).andThen(
                        Browser.assertText(Selector.id("v"), "true", Present(Schedule.fixed(100.millis).take(2)))
                    )
                }
            yield ()
        }
    }

    "anchor onBlur fires" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.a.href(Href.Fragment("")).id("a").onBlur(ref.set(true))("Link"),
                UI.button("Other").id("b"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("a"))
                _ <- Browser.click(Selector.id("b"))
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

    "a plain anchor keeps its native navigation" in {
        // The click transport prevents an anchor's default only where a click handler was declared, so the
        // handler rather than the href decides what happens. An anchor WITHOUT one is an ordinary link and
        // has to stay one: an in-page fragment scrolls, and a cross-document path is left to the browser
        // (or, in a Scala.js app, to UILocation's interceptor). Prevent-defaulting every anchor kills both,
        // which is what a navigation built from plain links runs into.
        val app: UI < Async =
            Kyo.lift(UI.div(
                UI.a.href(Href.Fragment("mark")).id("a")("Link"),
                UI.span("target").id("mark")
            ))
        withUI(app) {
            for
                _    <- Browser.click(Selector.id("a"))
                hash <- Browser.eval("location.hash")
            yield assert(hash == "#mark")
        }
    }

    "an anchor that carries a click handler navigates nowhere" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.a.href(Href.Fragment("handled")).id("a").onClick(ref.set(true))("Link"),
                UI.span("target").id("handled"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _    <- Browser.click(Selector.id("a"))
                _    <- Browser.assertText(Selector.id("v"), "true")
                hash <- Browser.eval("location.hash")
            yield assert(hash != "#handled")
        }
    }

    "pressKey Enter on anchor fires onClick" in {
        val app: UI < Async =
            for ref <- Signal.initRef(false)
            yield UI.div(
                UI.a.href(Href.Fragment("")).id("a").onClick(ref.set(true))("Click"),
                ref.map(v => UI.span(v.toString).id("v"))
            )
        withUI(app) {
            for
                _ <- Browser.press(Selector.id("a"), Key.Enter)
                _ <- Browser.assertText(Selector.id("v"), "true")
            yield ()
        }
    }

end AnchorTest
