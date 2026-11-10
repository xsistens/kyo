package kyo

import kyo.internal.HtmlRenderer
import scala.language.implicitConversions

class UICssClassTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    "single cssClass renders class attribute" in {
        renderHtml(UI.div.cssClass("hero")).map { s =>
            assert(s.contains("class=\"hero\""))
        }
    }

    "multiple cssClass calls space-join in call order" in {
        renderHtml(UI.div.cssClass("a").cssClass("b").cssClass("c")).map { s =>
            assert(s.contains("class=\"a b c\""))
        }
    }

    "cssClass with a double-quote is attribute-escaped" in {
        renderHtml(UI.div.cssClass("a\"b")).map { s =>
            assert(s.contains("class=\"a&quot;b\""))
            assert(!s.contains("a\"b"))
        }
    }

    "cssClass with several names adds them after the element's classes, in order, the bytes of one call per name" in {
        for
            several <- renderHtml(UI.div.cssClass("z").cssClass("a", "b", "c"))
            chained <- renderHtml(UI.div.cssClass("z").cssClass("a").cssClass("b").cssClass("c"))
        yield assert(several == chained && several.contains("class=\"z a b c\""), s"$several | $chained")
    }

    "cssClass returns the element's own type, for an svg element and for an element of a type parameter" in {
        // An svg element is declared outside UI.Ast, where `cssClass` once typed so that no `apply` was found on it.
        def classed[E <: UI.Ast.Element { type Self = E }](el: E): E = el.cssClass("g", "h")
        val path: Svg.Path                                           = Svg.path.cssClass("p")
        val div: UI.Ast.Div                                          = classed(UI.div)
        for
            svg   <- renderHtml(Svg.svg(path))
            plain <- renderHtml(div)
        yield assert(svg.contains("class=\"p\"") && plain.contains("class=\"g h\""), s"$svg | $plain")
        end for
    }

    "cssClass coexists with id and data attributes" in {
        renderHtml(UI.div.id("x").data("k", "v").cssClass("c")).map { s =>
            assert(s.contains("id=\"x\""))
            assert(s.contains("data-k=\"v\""))
            assert(s.contains("class=\"c\""))
        }
    }

end UICssClassTest
