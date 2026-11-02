package kyo

import kyo.UI.*
import kyo.internal.HtmlRenderer

class UIChildrenTest extends kyo.test.Test[Any]:

    "a list of children renders the same bytes as the same children written out" in {
        val rows: List[UI] = List(span("a"), span("b"))
        for
            spliced <- HtmlRenderer.render(div(UI.children(rows)*), Seq.empty)
            literal <- HtmlRenderer.render(div(span("a"), span("b")), Seq.empty)
        yield assert(spliced == literal)
        end for
    }

    "an empty list gives a container with no children" in {
        for
            spliced <- HtmlRenderer.render(div(UI.children(List.empty[UI])*), Seq.empty)
            literal <- HtmlRenderer.render(div, Seq.empty)
        yield assert(spliced == literal)
        end for
    }

    "the rows keep the paths they have as literal children, which a fragment would not" in {
        def row(text: String) = span(text).onClick(Kyo.unit)
        for
            spliced  <- HtmlRenderer.render(div(UI.children(Chunk(row("a"), row("b")))*), Seq.empty)
            literal  <- HtmlRenderer.render(div(row("a"), row("b")), Seq.empty)
            fragment <- HtmlRenderer.render(div(UI.fragment(row("a"), row("b"))), Seq.empty)
        yield
            assert(spliced == literal)
            assert(fragment != literal)
        end for
    }

    "takes a list of a precise element type" in {
        val rows = Seq(span("a"))
        HtmlRenderer.render(div(UI.children(rows)*), Seq.empty).map(html => assert(html.contains("<span")))
    }

    "rejects a list of SVG nodes" in {
        typeCheckFailure("""
            import kyo.*
            UI.div(UI.children(Seq(Svg.circle))*)
        """)
    }

end UIChildrenTest
