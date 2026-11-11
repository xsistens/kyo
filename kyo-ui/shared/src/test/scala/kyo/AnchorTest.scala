package kyo

import kyo.Browser.*
import kyo.Length.*
import kyo.UI.*
import kyo.UI.Ast.*
import kyo.internal.HtmlRenderer

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

    "a modified click on an anchor with onClick keeps the browser's default and still runs the handler" in {
        // A window listener runs after the page's delegated listener, so it sees whether that one took the
        // default; it then cancels the navigation itself so the test page stays put.
        val app: UI < Async =
            for clicks <- Signal.initRef(0)
            yield UI.div(
                UI.a.href(Href.Path("/elsewhere")).id("a").onClick(clicks.getAndUpdate(_ + 1).unit)("Link"),
                clicks.map(n => UI.span(n.toString).id("n"))
            )
        def clickPrevented(ctrl: Boolean): String =
            s"""(function(){var seen=null;function f(e){seen=e.defaultPrevented;e.preventDefault();}
               |window.addEventListener('click',f);
               |document.getElementById('a').dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true,ctrlKey:$ctrl}));
               |window.removeEventListener('click',f);return seen;})()""".stripMargin
        withUI(app) {
            for
                plain    <- Browser.evalBoolean(clickPrevented(ctrl = false))
                modified <- Browser.evalBoolean(clickPrevented(ctrl = true))
                _        <- Browser.assertText(Selector.id("n"), "2")
            yield
                assert(plain)
                assert(!modified)
            end for
        }
    }

    "download names the file an anchor saves instead of navigating to" in {
        withUI(UI.div(
            UI.a.href(Href.External("data", "text/csv;charset=utf-8,a%2Cb")).download("rows.csv").id("dl")("Save"),
            UI.a.href(Href.Path("/path")).id("plain")("Open")
        )) {
            for
                _    <- Browser.assertAttribute(Selector.id("dl"), "download", "rows.csv")
                name <- Browser.evalJson[String]("document.getElementById('dl').download")
                bare <- Browser.evalJson[String]("document.getElementById('plain').download")
            yield
                assert(name == "rows.csv")
                assert(bare.isEmpty)
        }
    }

    "a download anchor with onClick keeps the browser's default and still runs the handler" in {
        val app: UI < Async =
            for clicks <- Signal.initRef(0)
            yield UI.div(
                UI.a.href(Href.External("data", "text/plain,x")).download("x.txt").id("a")
                    .onClick(clicks.getAndUpdate(_ + 1).unit)("Save"),
                clicks.map(n => UI.span(n.toString).id("n"))
            )
        val clickPrevented =
            """(function(){var seen=null;function f(e){seen=e.defaultPrevented;e.preventDefault();}
              |window.addEventListener('click',f);
              |document.getElementById('a').dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
              |window.removeEventListener('click',f);return seen;})()""".stripMargin
        withUI(app) {
            for
                prevented <- Browser.evalBoolean(clickPrevented)
                _         <- Browser.assertText(Selector.id("n"), "1")
            yield assert(!prevented)
        }
    }

    "a blocked anchor declines the browser's default for a plain, modified and middle click" in {
        // Button draws a loading or inert link as an anchor marked aria-disabled, with a click handler that answers nothing. The
        // browser must not follow its href for any click: a plain click, a modified one (ctrl, meta, shift, alt) or a middle click
        // (auxclick, button 1, which is not a click event).
        val app: UI < Async =
            for clicks <- Signal.initRef(0)
            yield UI.div(
                UI.a.href(Href.Path("/elsewhere")).id("a").aria(AriaKey.disabled -> "true").blocked(true)
                    .onClick(clicks.getAndUpdate(_ + 1).unit)("Link"),
                clicks.map(n => UI.span(n.toString).id("n"))
            )
        def prevented(eventType: String, init: String): String =
            s"""(function(){var seen=null;function f(e){seen=e.defaultPrevented;e.preventDefault();}
               |window.addEventListener('$eventType',f);
               |document.getElementById('a').dispatchEvent(new MouseEvent('$eventType',{bubbles:true,cancelable:true,$init}));
               |window.removeEventListener('$eventType',f);return seen;})()""".stripMargin
        withUI(app) {
            for
                plain  <- Browser.evalBoolean(prevented("click", ""))
                ctrl   <- Browser.evalBoolean(prevented("click", "ctrlKey:true"))
                meta   <- Browser.evalBoolean(prevented("click", "metaKey:true"))
                shift  <- Browser.evalBoolean(prevented("click", "shiftKey:true"))
                alt    <- Browser.evalBoolean(prevented("click", "altKey:true"))
                middle <- Browser.evalBoolean(prevented("auxclick", "button:1"))
            yield
                assert(plain)
                assert(ctrl)
                assert(meta)
                assert(shift)
                assert(alt)
                assert(middle)
        }
    }

    "blocked(true) renders data-kyo-blocked, and blocked(false) renders nothing" in {
        for
            marked <- HtmlRenderer.render(UI.a.href(Href.Path("/elsewhere")).id("a").blocked(true)("Link"), Seq.empty)
            plain  <- HtmlRenderer.render(UI.a.href(Href.Path("/elsewhere")).id("a").blocked(false)("Link"), Seq.empty)
        yield
            assert(marked.contains("""data-kyo-blocked="1""""), marked)
            assert(!plain.contains("data-kyo-blocked"), plain)
    }

    "blocked is a link setter: the link builder takes it, a non-link element builder does not" in {
        // Compile-time check: blocked marks a link only, so a div or a button must not accept it.
        val onLink   = scala.compiletime.testing.typeCheckErrors("""{ given kyo.Frame = kyo.Frame.internal; kyo.UI.a.blocked(true) }""")
        val onDiv    = scala.compiletime.testing.typeCheckErrors("""{ given kyo.Frame = kyo.Frame.internal; kyo.UI.div.blocked(true) }""")
        val onButton =
            scala.compiletime.testing.typeCheckErrors("""{ given kyo.Frame = kyo.Frame.internal; kyo.UI.button.blocked(true) }""")
        assert(onLink.isEmpty, s"the link builder must take blocked: ${onLink.map(_.message)}")
        assert(onDiv.nonEmpty, "a div accepted blocked: the generic element builder still carries the setter")
        assert(onButton.nonEmpty, "a button accepted blocked: the generic element builder still carries the setter")
    }

    "an anchor that only says aria-disabled keeps the browser's default for a plain click" in {
        // aria-disabled is a hint for assistive technology. Only the marker Button draws on a blocked link declines the click, so an
        // anchor a caller marks aria-disabled still navigates.
        withUI(UI.div(UI.a.href(Href.Path("/elsewhere")).id("a").aria(AriaKey.disabled -> "true")("Link"))) {
            val plainClick =
                """(function(){var seen=null;function f(e){seen=e.defaultPrevented;e.preventDefault();}
                  |window.addEventListener('click',f);
                  |document.getElementById('a').dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
                  |window.removeEventListener('click',f);return seen;})()""".stripMargin
            for prevented <- Browser.evalBoolean(plainClick)
            yield assert(!prevented)
        }
    }

    "a middle click on an unblocked anchor keeps the browser's default" in {
        withUI(UI.div(UI.a.href(Href.Path("/elsewhere")).id("a")("Link"))) {
            val middleClick =
                """(function(){var seen=null;function f(e){seen=e.defaultPrevented;e.preventDefault();}
                  |window.addEventListener('auxclick',f);
                  |document.getElementById('a').dispatchEvent(new MouseEvent('auxclick',{bubbles:true,cancelable:true,button:1}));
                  |window.removeEventListener('auxclick',f);return seen;})()""".stripMargin
            for prevented <- Browser.evalBoolean(middleClick)
            yield assert(!prevented)
        }
    }

end AnchorTest
