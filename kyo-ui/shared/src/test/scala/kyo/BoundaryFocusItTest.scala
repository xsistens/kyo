package kyo

import kyo.Browser.*
import kyo.UI.render
import scala.language.implicitConversions

/** [[kyo.UI.boundary]] in a real browser: what a user typed and where the caret stands survive a page that loads and reveals next to
  * the field.
  *
  * The field is not bound to a signal, so what was typed lives only in the DOM: a paint that replaced the field would lose it even
  * where the client restores focus. The observation reads `document.activeElement` with a bare `Runtime.evaluate`, which moves no
  * focus, and no `focus()` runs after the reveal.
  */
class BoundaryFocusItTest extends UITest:

    override def timeout = 120.seconds

    "a field next to a page that loads keeps focus, caret and what was typed, through the reveal" in {
        for
            started <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            typed   <- AtomicRef.init("")
            app =
                for route <- Signal.initRef("1")
                yield
                    val detail = (id: String) =>
                        val load = if id == "1" then Kyo.unit else started.completeUnitDiscard.andThen(gate.get)
                        UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title")))).keyed(id)
                    UI.boundary.placeholder(UI.span("skel"))(UI.div(
                        UI.input.id("search").onInput(typed.set(_)),
                        UI.button("go").id("go").onClick(route.set("2")),
                        route.render(id => UI.div(detail(id)))
                    ))
            result <- withUI(app) {
                for
                    _ <- Browser.assertText(Selector.id("title"), "title:1")
                    // A synthetic click, which moves no focus, navigates; the next page loads until the gate opens.
                    _ <- Browser.evalDiscard("document.getElementById('go').click()")
                    _ <- started.get
                    // Typed while the next page loads, then the caret is put at 2.
                    _    <- Browser.fill(Selector.id("search"), "kyo")
                    _    <- Browser.evalDiscard("var el = document.getElementById('search'); el.focus(); el.setSelectionRange(2,2);")
                    _    <- gate.completeUnitDiscard
                    _    <- Browser.assertText(Selector.id("title"), "title:2")
                    seen <- Browser.evalJson[String](
                        "document.activeElement.id + ':' + document.activeElement.selectionStart + ':' + document.activeElement.value"
                    )
                yield seen
            }
            onServer <- typed.get
        yield
            assert(result == "search:2:kyo", s"after the reveal: $result")
            assert(onServer == "kyo", s"the server received: $onServer")
    }

    "a page that waits is inert: focus leaves it, it takes none back, and the page around it keeps working" in {
        for
            started <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            typed   <- AtomicRef.init(Chunk.empty[String])
            app =
                for route <- Signal.initRef("1")
                yield
                    val detail = (id: String) =>
                        val load    = if id == "1" then Kyo.unit else started.completeUnitDiscard.andThen(gate.get)
                        val comment = UI.textarea.id(s"comment-$id").onInput(v => typed.getAndUpdate(_.append(s"$id:$v")).unit)
                        UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title"), comment))).keyed(id)
                    UI.boundary.placeholder(UI.span("skel"))(UI.div(
                        UI.input.id("search"),
                        UI.button("go").id("go").onClick(route.set("2")),
                        // The app sets its own aria-busy, which the wait overrides and must not destroy.
                        route.render(id => UI.div(detail(id)).id("page").aria(AriaKey.busy -> "false"))
                    ))
            active = "document.activeElement.id || document.activeElement.tagName"
            value  = (id: String) => s"document.getElementById('$id').value"
            result <- withUI(app) {
                for
                    _ <- Browser.assertText(Selector.id("title"), "title:1")
                    _ <- Browser.fill(Selector.id("comment-1"), "hel")
                    _ <- Browser.evalDiscard("document.getElementById('comment-1').focus()")
                    _ <- Browser.evalDiscard("document.getElementById('go').click()")
                    _ <- started.get
                    // The page waits for issue 2: it is marked inert and busy, and the field that had focus lost it.
                    _    <- Browser.assertExists(Selector.css("#page[inert]"))
                    _    <- Browser.assertAttribute(Selector.css("#page"), "aria-busy", "true")
                    left <- Browser.evalJson[String](active)
                    // Typing on goes nowhere: real key events, as a user who keeps typing sends them.
                    _         <- Browser.typeText("lo")
                    waiting   <- Browser.evalJson[String](value("comment-1"))
                    _         <- Browser.evalDiscard("document.getElementById('comment-1').focus()")
                    refocused <- Browser.evalJson[String](active)
                    // Outside the page that waits, a field still takes focus; it gives it up again before the reveal.
                    _       <- Browser.evalDiscard("document.getElementById('search').focus()")
                    outside <- Browser.evalJson[String](active)
                    _       <- Browser.evalDiscard("document.activeElement.blur()")
                    _       <- gate.completeUnitDiscard
                    _       <- Browser.assertText(Selector.id("title"), "title:2")
                    _       <- Browser.assertNotExists(Selector.css("[inert]"))
                    _       <- Browser.assertAttribute(Selector.css("#page"), "aria-busy", "false")
                    after   <- Browser.evalJson[String](active)
                    // The revealed page is no longer inert: its field takes focus and input.
                    _         <- Browser.fill(Selector.id("comment-2"), "ok")
                    revealed2 <- Browser.evalJson[String](value("comment-2"))
                yield (left, waiting, refocused, outside, after, revealed2)
            }
            onServer <- typed.get
        yield
            val (left, waiting, refocused, outside, after, revealed2) = result
            assert(left == "BODY", s"focus while the page waits: $left")
            assert(waiting == "hel", s"the waiting field after typing on: $waiting")
            assert(refocused == "BODY", s"a field of the waiting page took focus: $refocused")
            assert(outside == "search", s"a field outside the waiting page: $outside")
            assert(after == "BODY", s"focus after the reveal: $after")
            assert(revealed2 == "ok", s"the revealed field: $revealed2")
            assert(onServer == Chunk("1:hel", "2:ok"), s"the server received: $onServer")
    }

    "navigating twice while the first page loads keeps the page inert until the last one is revealed" in {
        for
            started2 <- Promise.init[Unit, Any]
            started3 <- Promise.init[Unit, Any]
            never    <- Promise.init[Unit, Any]
            gate3    <- Promise.init[Unit, Any]
            app =
                for route <- Signal.initRef("1")
                yield
                    val detail = (id: String) =>
                        val load = id match
                            case "2" => started2.completeUnitDiscard.andThen(never.get)
                            case "3" => started3.completeUnitDiscard.andThen(gate3.get)
                            case _   => Kyo.unit
                        UI.mounted(load.andThen(UI.div(UI.span(s"title:$id").id("title")))).keyed(id)
                    UI.boundary.placeholder(UI.span("skel"))(UI.div(
                        UI.button("2").id("go2").onClick(route.set("2")),
                        UI.button("3").id("go3").onClick(route.set("3")),
                        route.render(id => UI.div(detail(id)).id("page"))
                    ))
            _ <- withUI(app) {
                for
                    _ <- Browser.assertText(Selector.id("title"), "title:1")
                    _ <- Browser.evalDiscard("document.getElementById('go2').click()")
                    _ <- started2.get
                    _ <- Browser.assertExists(Selector.css("#page[inert]"))
                    _ <- Browser.evalDiscard("document.getElementById('go3').click()")
                    _ <- started3.get
                    // Issue 2's wait ended and issue 3's began: whichever mark reached the page last, it is still held.
                    _ <- Browser.assertExists(Selector.css("#page[inert]"))
                    _ <- Browser.assertText(Selector.id("title"), "title:1")
                    _ <- gate3.completeUnitDiscard
                    _ <- Browser.assertText(Selector.id("title"), "title:3")
                    _ <- Browser.assertNotExists(Selector.css("[inert]"))
                yield ()
            }
        yield succeed
    }

end BoundaryFocusItTest
