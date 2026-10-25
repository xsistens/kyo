package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.Paginator` subscribes the page and the rows per page it is drawn from. */
class PaginatorTest extends UicTest:

    "what follows the page reads the page and the rows per page in ONE region" in {
        for
            page <- Signal.initRef(1)
            rows <- Signal.initRef(10)
            ui = uic.Paginator().totalRecords(120).rows(rows).page(page).render
            links  <- regionsAbove(ui, "p-paginator-pages")
            first  <- regionsAbove(ui, "p-paginator-first")
            report <- regionsAbove(
                uic.Paginator().totalRecords(120).rows(rows).page(page).currentPageReport("{first}").render,
                "p-paginator-current"
            )
        yield assert(
            links == 1 && first == 1 && report == 1,
            s"changing the rows writes the page too; a nested region repaints the older page: links $links, first $first, report $report"
        )
    }

    "a page change or a rows change rebuilds neither dropdown, so neither starts its mount again" in {
        val both = List(
            PaginatorElement.PrevPageLink,
            PaginatorElement.PageLinks,
            PaginatorElement.NextPageLink,
            PaginatorElement.RowsPerPageDropdown,
            PaginatorElement.JumpToPageDropdown
        )
        for
            page <- Signal.initRef(1)
            rows <- Signal.initRef(10)
            fixed = uic.Paginator().totalRecords(120).rows(10).page(page).rowsPerPageOptions(Seq(10, 20)).template(both*).render
            live  = uic.Paginator().totalRecords(120).rows(rows).page(page).rowsPerPageOptions(Seq(10, 20)).template(both*).render
            rppFixed <- regionsAboveMount(fixed, "p-paginator-rpp-dropdown")
            jtpFixed <- regionsAboveMount(fixed, "p-paginator-jtp-dropdown")
            rppLive  <- regionsAboveMount(live, "p-paginator-rpp-dropdown")
            jtpLive  <- regionsAboveMount(live, "p-paginator-jtp-dropdown")
        yield
            assert(rppFixed == 0 && jtpFixed == 0, s"with the rows fixed: rpp under $rppFixed region(s), jtp under $jtpFixed")
            // The jump list's options are the pages, read as a signal, so neither a page nor a rows change rebuilds it.
            assert(rppLive == 0 && jtpLive == 0, s"with the rows bound: rpp under $rppLive region(s), jtp under $jtpLive")
        end for
    }

    "a total that changes reaches the paginator without rebuilding it" in {
        for
            page  <- Signal.initRef(0)
            total <- Signal.initRef(120)
            ui = uic.Paginator().totalRecords(total).rows(10).page(page).rowsPerPageOptions(Seq(10, 20))
                .template(PaginatorElement.PageLinks, PaginatorElement.RowsPerPageDropdown).render
            before <- elementsWithClass(ui, "p-paginator-page").map(_.size)
            _      <- total.set(25)
            after  <- elementsWithClass(ui, "p-paginator-page").map(_.size)
            rpp    <- regionsAboveMount(ui, "p-paginator-rpp-dropdown")
        yield
            assert(before == 5 && after == 3, s"page links before $before, after the total shrank to three pages $after")
            assert(rpp == 0, s"the rows-per-page dropdown sits under $rpp region(s)")
    }

    "a rows-per-page pick writes the rows and moves to the page that keeps the first record shown" in {
        // Page 4 of 10 rows starts at record 31; a page of 20 that shows it is page 2.
        for
            page <- Signal.initRef(3)
            rows <- Signal.initRef(10)
            _    <- uic.Paginator().totalRecords(120).rows(rows).page(page).rowsPerPageOptions(Seq(10, 20)).rowsPicked(20)
            r    <- rows.get
            p    <- page.get
        yield assert(r == 20 && p == 1, s"rows $r, page $p")
    }

    "the dropdowns run what a pick asks for, rather than handing it back unrun" in {
        // A `fold(())(f)` handler infers a common type of Unit and the action, so the action came back as a value and the
        // pick did nothing: the rows-per-page and the jump dropdowns were dead in the browser.
        for
            page <- Signal.initRef(3)
            rows <- Signal.initRef(10)
            pag = uic.Paginator().totalRecords(120).rows(rows).page(page).rowsPerPageOptions(Seq(10, 20))
            _ <- pag.rowsChange(Present(20))
            r <- rows.get
            _ <- pag.jumpChange(Present(4))
            p <- page.get
        yield assert(r == 20 && p == 4, s"rows $r, page $p")
    }

    "and so it does with a total that follows a signal" in {
        for
            page  <- Signal.initRef(3)
            rows  <- Signal.initRef(10)
            total <- Signal.initRef(120)
            _     <- uic.Paginator().totalRecords(total.map(identity)).rows(rows).page(page).rowsPerPageOptions(Seq(10, 20)).rowsPicked(20)
            r     <- rows.get
            p     <- page.get
        yield assert(r == 20 && p == 1, s"rows $r, page $p")
    }

end PaginatorTest
