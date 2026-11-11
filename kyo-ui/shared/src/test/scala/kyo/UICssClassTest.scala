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
        renderHtml(UI.div.id("x").data(data"data-k" -> "v").cssClass("c")).map { s =>
            assert(s.contains("id=\"x\""))
            assert(s.contains("data-k=\"v\""))
            assert(s.contains("class=\"c\""))
        }
    }

    // ---- matching ----

    import UICssClassTest.Tone

    /** A class per case, written each of the ways a case may name one: a name, a `Present`, or `Absent` for none. */
    private def toneClass(t: Tone): String | Maybe[String] = t match
        case Tone.Hot   => "p-hot"
        case Tone.Warm  => Present("p-warm")
        case Tone.Plain => Absent

    "cssClass.matching over a constant writes the class its case names statically, the bytes of cssClass(name), and nothing for Absent" in {
        val hot = UI.div.cssClass("p-x").cssClass.matching(Tone.Hot)(toneClass)
        for
            hotHtml  <- renderHtml(hot)
            warm     <- renderHtml(UI.div.cssClass("p-x").cssClass.matching(Tone.Warm)(toneClass))
            plain    <- renderHtml(UI.div.cssClass("p-x").cssClass.matching(Tone.Plain)(toneClass))
            byName   <- renderHtml(UI.div.cssClass("p-x").cssClass("p-hot"))
            warmName <- renderHtml(UI.div.cssClass("p-x").cssClass("p-warm"))
            none     <- renderHtml(UI.div.cssClass("p-x"))
        yield
            assert(hot.attrs.reactiveClassNames.isEmpty && hot.attrs.cssClasses == Chunk("p-x", "p-hot"), s"${hot.attrs}")
            assert(hotHtml == byName && warm == warmName && plain == none, s"$hotHtml | $warm | $plain")
        end for
    }

    "cssClass.matching over a signal is one class-name channel: the bytes of a hand-written one, writing the class of the case it holds" in {
        given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
        for
            tone <- Signal.initRef(Tone.Hot)
            sig  = tone: Signal[Tone]
            el   = UI.div.cssClass("p-x").cssClass.matching(sig)(toneClass)
            hand = UI.div.cssClass("p-x").withReactiveClassName(sig.map {
                case Tone.Hot   => Present("p-hot")
                case Tone.Warm  => Present("p-warm")
                case Tone.Plain => Absent
            })
            hot      <- renderHtml(el)
            handHot  <- renderHtml(hand)
            _        <- tone.set(Tone.Warm)
            warm     <- renderHtml(el)
            _        <- tone.set(Tone.Plain)
            plain    <- renderHtml(el)
            handNone <- renderHtml(hand)
        yield
            assert(el.attrs.reactiveClassNames.size == 1 && el.attrs.cssClasses == Chunk("p-x"), s"${el.attrs}")
            assert(hot == handHot && plain == handNone, s"$hot / $handHot | $plain / $handNone")
            assert(
                hot.contains("class=\"p-x p-hot\"") && warm.contains("class=\"p-x p-warm\"") && plain.contains("class=\"p-x\""),
                s"$hot | $warm | $plain"
            )
        end for
    }

end UICssClassTest

object UICssClassTest:
    enum Tone derives CanEqual:
        case Hot, Warm, Plain
end UICssClassTest
