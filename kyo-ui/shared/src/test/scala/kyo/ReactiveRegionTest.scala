package kyo

import kyo.internal.ReactiveRegion

class ReactiveRegionTest extends kyo.test.Test[Any]:

    "HTML regions write a dot per segment, plain [0-9A-Za-z_] as is and any other unit as $ plus four hex" in {
        val region = ReactiveRegion.from(Seq("", "😀", "\"--\u0000", "user_42"), svgContext = false)
        assert(region == ReactiveRegion.HtmlRange("r..$d83d$de00.$0022$002d$002d$0000.user_42"))
        assert(ReactiveRegion.htmlId(Seq("1", "0", "0", "2", "1", "0")) == "r.1.0.0.2.1.0")
        assert(ReactiveRegion.htmlId(Seq.empty) == "r")
    }

    "the path reads back out of the id, whatever the segments hold" in {
        val paths = Seq(
            Seq.empty[String],
            Seq(""),
            Seq("", ""),
            Seq("1", "0", "0", "2"),
            Seq("a.b", "c~d", "e$f", "$r"),
            Seq("😀", " ", "--", "<!-->", "\u0000", "ÄÖÜ"),
            Seq("a-b", "user-42")
        )
        paths.foreach { path =>
            val id = ReactiveRegion.htmlId(path)
            assert(ReactiveRegion.isValidHtmlId(id), id)
            assert(ReactiveRegion.pathOf(id) == Present(path), id)
            assert(!id.exists(c => c == ' ' || c == '>' || c == '<' || c == '"' || c == '\''), id)
            assert(!id.contains("--"), id)
        }
        val nested = ReactiveRegion.RegionIdentity.root(Seq("a.b", "2")).transparent.transparent
        assert(ReactiveRegion.htmlId(nested) == "r.a$002eb.2~2")
        assert(ReactiveRegion.pathOf(ReactiveRegion.htmlId(nested)) == Present(Seq("a.b", "2")))
        assert(ReactiveRegion.baseIdOf(ReactiveRegion.htmlId(nested)) == "r.a$002eb.2")
    }

    "code units on both sides of 256 escape alike and decode back" in {
        val path = Seq("a" * 300, "\u00ff\u0100\uffff")
        val id   = ReactiveRegion.htmlId(path)
        assert(id == "r." + "a" * 300 + ".$00ff$0100$ffff")
        assert(ReactiveRegion.pathOf(id) == Present(path))
    }

    "path segment boundaries remain distinct" in {
        val joined = ReactiveRegion.from(Seq("ab"), svgContext = false)
        val split  = ReactiveRegion.from(Seq("a", "b"), svgContext = false)
        val empty  = ReactiveRegion.from(Seq("", "ab"), svgContext = false)
        assert(joined != split)
        assert(joined != empty)
        assert(split != empty)
    }

    "transparent nesting is tagged separately from application path segments" in {
        val root     = ReactiveRegion.RegionIdentity.root(Seq("nested"))
        val direct   = root.transparent
        val deeper   = direct.transparent
        val emptyKey = root.child("")
        val ids      = Seq(root, direct, deeper, emptyKey).map(ReactiveRegion.htmlId)
        assert(ids == Seq("r.nested", "r.nested~1", "r.nested~2", "r.nested."))
        assert(ids.distinct.size == ids.size)
    }

    "region id validation accepts encoded paths and tagged nesting only" in {
        val valid = Seq(
            "r",
            "r.",
            "r..",
            "r.a",
            "r.a~1",
            "r.a~10",
            "r~3",
            "r.a$002e.b_9",
            "r.$0024r"
        )
        val invalid = Seq(
            "",
            "r1",
            "ra",
            "n",
            "r.a~0",
            "r.a~01",
            "r.a~",
            "r.a~1x",
            "r.a~1~1",
            "r.a$00",
            "r.a$00zz",
            "r.a$00AB",
            "r.a b",
            "r.a-b",
            "r.a$"
        )
        assert(valid.forall(ReactiveRegion.isValidHtmlId))
        assert(invalid.forall(id => !ReactiveRegion.isValidHtmlId(id)))
    }

    "SVG regions retain their element path" in {
        assert(ReactiveRegion.from(Seq("0", "key"), svgContext = true) == ReactiveRegion.SvgElement(Seq("0", "key")))
    }

    "table content reports rows only when an actual row is present" in {
        import ReactiveRegion.TableContent
        assert(ReactiveRegion.tableContent(UI.tr(UI.td("row"))) == TableContent.Rows)
        assert(ReactiveRegion.tableContent(UI.tbody(UI.tr())) == TableContent.AuthoredSections)
        assert(ReactiveRegion.tableContent(UI.thead(UI.tr())) == TableContent.AuthoredSections)
        assert(ReactiveRegion.tableContent(UI.tfoot(UI.tr())) == TableContent.AuthoredSections)
        assert(ReactiveRegion.tableContent(UI.fragment()) == TableContent.Other)
        assert(ReactiveRegion.tableContent(UI.div("not a row")) == TableContent.Other)
        val transparent = Signal.initConst(UI.tr()).render(identity)
        assert(ReactiveRegion.tableContent(transparent) == TableContent.Transparent)
        assert(
            ReactiveRegion.tableContent(Seq(UI.tbody(UI.tr()), transparent)) == TableContent.AuthoredSections
        )
    }

end ReactiveRegionTest
