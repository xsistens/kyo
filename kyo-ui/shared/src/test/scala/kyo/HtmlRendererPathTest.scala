package kyo

import kyo.UI.*
import kyo.UI.Ast.*
import kyo.UI.foreachKeyed
import kyo.internal.HtmlRenderer

/** Which elements render `data-kyo-path` (HtmlRenderer.carriesPath): the ones a client names to the server or the
  * server names to the client, and no others.
  */
class HtmlRendererPathTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    private def paths(html: String): Seq[String] =
        "data-kyo-path=\"([^\"]*)\"".r.findAllMatchIn(html).map(_.group(1)).toSeq

    "a plain element carries no path; a handler, an id or a control does" in {
        for
            plain   <- renderHtml(UI.div(UI.span("a"), UI.td("b"), UI.p("c")))
            handler <- renderHtml(UI.div(UI.span("a").onClick(())))
            named   <- renderHtml(UI.div(UI.span("a").id("box")))
            control <- renderHtml(UI.div(UI.input, UI.button("b"), UI.a("x"), UI.textarea))
        yield
            assert(paths(plain).isEmpty, plain)
            assert(paths(handler) == Seq("0"), handler)
            assert(paths(named) == Seq("0"), named)
            assert(paths(control) == Seq("0", "1", "2", "3"), control)
    }

    "a reactive binding or a client-side marker makes an element addressable" in {
        for
            sel    <- Signal.initRef(false)
            klass  <- renderHtml(UI.div(UI.tr.cssClass("hot", sel)))
            attr   <- renderHtml(UI.div(UI.span.title(sel.map(_.toString))))
            hidden <- renderHtml(UI.div(UI.span.hidden(sel)))
            focus  <- renderHtml(UI.div(UI.span.focusAuto(true), UI.span.tabIndex(0)))
            portal <- renderHtml(UI.div(UI.span.portal(true)))
            enter  <- renderHtml(UI.div(UI.span.enterTransition("in"), UI.span.leaveTransition("out")))
        yield
            assert(paths(klass) == Seq("0"))
            assert(paths(attr) == Seq("0"))
            assert(paths(hidden) == Seq("0"))
            assert(paths(focus) == Seq("0", "1"))
            assert(paths(portal) == Seq("0"))
            assert(paths(enter) == Seq("0", "1"))
    }

    "a keyed row root carries the key path even when the row is plain" in {
        for
            rows <- Signal.initRef(Chunk("a", "b"))
            html <- renderHtml(UI.ul(rows.foreachKeyed(identity)(v => UI.li(UI.span(v)))))
        yield assert(paths(html) == Seq("0.a", "0.b"), html)
    }

    "every SVG element carries its path" in {
        for html <- renderHtml(UI.div(Svg.svg(Svg.g(Svg.circle.r(1)))))
        yield assert(paths(html) == Seq("0", "0.0", "0.0.0"), html)
    }

end HtmlRendererPathTest
