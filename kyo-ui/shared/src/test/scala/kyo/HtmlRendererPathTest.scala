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
            klass  <- renderHtml(UI.div(UI.tr.when(sel)(_.cssClass("hot"))))
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

    "an element whose only channel is its inline style, or whose only hook is onAttached, carries its path" in {
        // A channel patches its element by path, and an attach reaches the hook by path, so neither works on an element without one.
        for
            st     <- Signal.initRef(Style.width(Length.Px(10)))
            styled <- renderHtml(UI.div(UI.span.inlineStyle(st)))
            // The typed setter is the Scala.js one (kyo.onAttached); the field is shared.
            hooked <- renderHtml(UI.div(UI.span.withAttrs(UI.span.attrs.copy(onAttached = Present(_ => Kyo.unit)))))
        yield
            assert(paths(styled) == Seq("0"), styled)
            assert(paths(hooked) == Seq("0"), hooked)
    }

    "a client-side marker set to false is unset: no path, no marker, and a later false takes an earlier true back" in {
        val off: Seq[SpanElement => SpanElement] = Seq(
            _.focusAuto(false),
            _.focusRestore(false),
            _.focusTrap(false),
            _.scrollAuto(false),
            _.stopPropagation(false),
            _.portal(false)
        )
        val takenBack: Seq[SpanElement => SpanElement] = Seq(
            _.focusAuto(true).focusAuto(false),
            _.focusRestore(true).focusRestore(false),
            _.focusTrap(true).focusTrap(false),
            _.scrollAuto(true).scrollAuto(false),
            _.stopPropagation(true).stopPropagation(false),
            _.portal(true).portal(false)
        )
        val plain = UI.span("a")
        for
            offHtml  <- renderHtml(UI.div(off.map(f => f(plain): UI)*))
            backHtml <- renderHtml(UI.div(takenBack.map(f => f(plain): UI)*))
        yield
            val attrs = (off ++ takenBack).map(f => f(plain).attrs)
            assert(
                attrs.forall(a =>
                    a.focusAuto.isEmpty && a.focusRestore.isEmpty && a.focusTrap.isEmpty && a.scrollAuto.isEmpty &&
                        a.stopPropagation.isEmpty && a.portal.isEmpty
                ),
                s"$attrs"
            )
            assert(paths(offHtml).isEmpty && paths(backHtml).isEmpty, s"$offHtml | $backHtml")
            assert(!offHtml.contains("data-kyo-") && !backHtml.contains("data-kyo-"), s"$offHtml | $backHtml")
        end for
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
