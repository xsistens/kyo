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

    "single children, optional ones and lists mix, in order, and render as the same children written out" in {
        val label: Maybe[UI.Ast.SpanElement] = Present(span("b"))
        val none: Maybe[UI.Ast.SpanElement]  = Absent
        for
            spliced <- HtmlRenderer.render(div(UI.children(span("a"), label, none, List(span("c"), span("d")))*), Seq.empty)
            literal <- HtmlRenderer.render(div(span("a"), span("b"), span("c"), span("d")), Seq.empty)
        yield assert(spliced == literal)
        end for
    }

    "children a helper hands back are one part of another list" in {
        def icons(using Frame): UI.Children = UI.children(span("a"), span("b"))
        for
            spliced <- HtmlRenderer.render(div(UI.children(icons, span("c"))*), Seq.empty)
            literal <- HtmlRenderer.render(div(span("a"), span("b"), span("c")), Seq.empty)
        yield assert(spliced == literal)
        end for
    }

    "rejects an optional SVG node" in {
        typeCheckFailure("""
            import kyo.*
            UI.div(UI.children(Maybe(Svg.circle))*)
        """)
    }

    "rejects a list of SVG nodes" in {
        typeCheckFailure("""
            import kyo.*
            UI.div(UI.children(Seq(Svg.circle))*)
        """)
    }

end UIChildrenTest
