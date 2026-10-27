package kyo.uic

import kyo.*
import kyo.UI.*

/** What a page change of a paginated `uic.DataView` redraws. */
class DataViewTest extends UicTest:

    private def control(cls: String)(using Frame): UI =
        UI.mounted(Signal.initRef(0).map(_ => UI.div.cssClass(cls))).placeholder(UI.div.cssClass(cls))

    private def view(page: SignalRef[Int])(using Frame): UI =
        DataView[Int]()
            .items(1 to 30)
            .itemTemplate(i => UI.div.cssClass("row")(i.toString))
            .header(control("head-control"))
            .footer(control("foot-control"))
            .paginate(10)(page)
            .render

    "a page bound one way shows the signal's page, and a click is reported without being written" in {
        for
            held <- Signal.initRef(2)
            seen <- Signal.initRef(-1)
            ui = DataView[Int]().items(1 to 30).itemTemplate(i => UI.div.cssClass("row")(i.toString))
                .paginate(10)(held.readOnly).onPageChange(p => seen.set(p)).render
            first <- elementsWithClass(ui, "row").map(_.headOption.map(textOf))
            link  <- elementsWithClass(ui, "p-paginator-page").map(_.find(textOf(_) == "1").get)
            _     <- click(link)
            h     <- held.get
            s     <- seen.get
        yield
            assert(first == Some("21"), s"the third page starts at $first")
            assert(h == 2 && s == 0, s"the signal still holds $h, the click reported $s")
    }

    "a page change redraws the rows, not the header, the footer or the paginator" in {
        for
            page <- Signal.initRef(0)
            ui = view(page)
            head   <- regionsAboveMount(ui, "head-control")
            foot   <- regionsAboveMount(ui, "foot-control")
            pager  <- regionsAbove(ui, "p-paginator")
            rows   <- regionsAbove(ui, "row")
            before <- elementsWithClass(ui, "row").map(_.size)
            _      <- page.set(2)
            after  <- elementsWithClass(ui, "row").map(_.size)
            first  <- elementsWithClass(ui, "row").map(_.headOption.map(_.children.collect { case t: UI.Ast.Text => t.value }.mkString))
        yield
            assert(first == Some("21"), s"the third page starts at $first")
            assert(head == 0 && foot == 0, s"a control in the header sits under $head region(s), in the footer under $foot")
            assert(pager == 0, s"the paginator sits under $pager region(s), so a page change builds it again")
            assert(rows == 1, s"the rows sit under $rows region(s)")
            assert(before == 10 && after == 10, s"rows on a page: $before, then $after")
    }

end DataViewTest
