package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.Paginator` subscribes the page and the rows per page it is drawn from, and both held to rule A
  * ([[ReactiveValueContract]]).
  */
class PaginatorTest extends ReactiveValueContract:

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

    private def pageLink(ui: UI, label: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-paginator-page").map(_.find(textOf(_) == label).getOrElse(
            throw new AssertionError(s"no page link $label")
        ))

    private def selectedPage(ui: UI)(using Frame): String < Sync =
        elementsWithClass(ui, "p-paginator-page-selected").map(_.map(textOf).mkString(","))

    "a page bound one way shows the signal, and a click reports the page and writes nothing" in {
        // The page lives somewhere else (a route, a parent): the paginator draws it and hands the reader's pick over.
        for
            held <- Signal.initRef(1)
            seen <- Signal.initRef(-1)
            ui = uic.Paginator().totalRecords(120).rows(10).page(held.readOnly).onPageChange(p => seen.set(p)).render
            before <- selectedPage(ui)
            link   <- pageLink(ui, "4")
            _      <- click(link)
            h      <- held.get
            s      <- seen.get
        yield
            assert(before == "2", s"the page the signal holds is the one drawn selected: $before")
            assert(h == 1 && s == 3, s"the signal still holds $h, the pick reported $s")
        end for
    }

    "a constant page is drawn and a click is reported" in {
        for
            seen <- Signal.initRef(-1)
            ui = uic.Paginator().totalRecords(120).rows(10).page(2).onPageChange(p => seen.set(p)).render
            before <- selectedPage(ui)
            link   <- pageLink(ui, "5")
            _      <- click(link)
            s      <- seen.get
        yield assert(before == "3" && s == 4, s"selected $before, reported $s")
    }

    "a rows-per-page pick reports the rows and the page it moved to, after writing both" in {
        // Page 4 of 10 rows starts at record 31; a page of 20 that shows it is page 2. The page moved, so it is reported
        // as a page change like any other, and both refs hold the new values by the time either callback runs.
        for
            page     <- Signal.initRef(3)
            rows     <- Signal.initRef(10)
            seenRows <- Signal.initRef((-1, -1))
            seenPage <- Signal.initRef((-1, -1))
            _        <- uic.Paginator().totalRecords(120).rows(rows).page(page).rowsPerPageOptions(Seq(10, 20))
                .onRowsChange(n => rows.get.map(r => seenRows.set((n, r))))
                .onPageChange(p => page.get.map(q => seenPage.set((p, q))))
                .rowsPicked(20)
            r <- seenRows.get
            p <- seenPage.get
        yield assert(r == (20, 20) && p == (1, 1), s"rows reported, held $r; page reported, held $p")
    }

    "rows and page bound one way: a rows-per-page pick writes neither and reports both" in {
        for
            page     <- Signal.initRef(3)
            rows     <- Signal.initRef(10)
            seenRows <- Signal.initRef(-1)
            seenPage <- Signal.initRef(-1)
            pag = uic.Paginator().totalRecords(120).rows(rows.readOnly).page(page.readOnly).rowsPerPageOptions(Seq(10, 20))
                .onRowsChange(seenRows.set).onPageChange(seenPage.set)
            _     <- pag.rowsPicked(20)
            r     <- rows.get
            p     <- page.get
            sr    <- seenRows.get
            sp    <- seenPage.get
            cards <- elementsWithClass(pag.render, "p-uic-diagnostic")
        yield
            assert(r == 10 && p == 3, s"the signals still hold rows $r, page $p")
            assert(sr == 20 && sp == 1, s"reported rows $sr, page $sp")
            assert(cards.isEmpty, s"both picks have somewhere to go: ${cards.map(textOf)}")
        end for
    }

    "a run that stops binding the page and the rows carries both on from what the paginator showed" in {
        def ranOnce(t: UI): Boolean < Sync =
            elementsWithClass(t, "p-paginator-current").map(_.headOption.exists(c => textOf(c).startsWith("run 1")))
        for
            tick <- Signal.initRef(0)
            page <- Signal.initRef(3)
            rows <- Signal.initRef(20)
            ui = UI.div(tick.render { n =>
                val base = uic.Paginator().totalRecords(200).rowsPerPageOptions(Seq(10, 20)).currentPageReport(s"run $n {rows}")
                // One placement: a conversion in each branch of an `if` would be two places, so two instances.
                val pag: uic.Paginator = if n == 0 then base.page(page).rows(rows) else base
                UI.div(pag)
            })
            (every, _, _) <- paintedAround(ui, t => elementsWithClass(t, "p-paginator").map(_.nonEmpty), ranOnce)(_ => setSettled(tick, 1))
            shownPage     <- selectedPage(every)
            report        <- elementsWithClass(every, "p-paginator-current").map(_.map(textOf))
        yield assert(
            shownPage.split(",").contains("4") && report.exists(_ == "run 1 20"),
            s"after the run that dropped both bindings: page $shownPage, report $report"
        )
        end for
    }

    // ---- rule A ----

    private def next(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-paginator-next")

    // Ten rows over 120 records is twelve pages; the next-page button goes from the first to the second.
    holds(new SlotCase[uic.Paginator, Int](
        name = "page",
        bare = Present(uic.Paginator().totalRecords(120).rows(10)),
        bind = uic.Paginator().totalRecords(120).rows(10).page(_),
        hear = (p, f) => p.onPageChange(f),
        event = "onPageChange",
        render = _.render,
        start = 0,
        next = 1,
        control = next(_),
        shows = selectedPage(_).map(_.toInt - 1),
        isFixed = Present(paginatorPagesFixed(_))
    ))

    /** The rows-per-page dropdown's trigger, which opens its panel: what a reader reaches, and what says it is fixed. */
    private def rowsTrigger(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-paginator-rpp-dropdown")

    private def opens(c: UI.Ast.Element): Boolean = declaresClick(c) || c.attrs.onKeyDown.nonEmpty

    private def readOnly(c: UI.Ast.Element): Boolean = c.attrs.ariaAttrs.get("readonly").contains("true")

    /** The rows the dropdown shows. */
    private def rowsShown(t: UI)(using Frame): Int < Sync =
        elementsWithClass(t, "p-select-label").map(ls =>
            ls.map(textOf).flatMap(_.toIntOption).headOption.getOrElse(throw new AssertionError(s"no rows shown: ${ls.map(textOf)}"))
        )

    // The page count follows the rows, so the page stays where it is: a pick of 20 rows on the first page reports no page.
    holds(new SlotCase[uic.Paginator, Int](
        name = "rows",
        bare = Present(uic.Paginator().totalRecords(120).rowsPerPageOptions(Seq(10, 20))),
        bind = uic.Paginator().totalRecords(120).rowsPerPageOptions(Seq(10, 20)).rows(_),
        hear = (p, f) => p.onRowsChange(f),
        event = "onRowsChange",
        render = _.render,
        start = 10,
        next = 20,
        control = rowsTrigger(_),
        shows = rowsShown(_),
        answers = opens,
        saysFixed = readOnly,
        // A lookup that finds no dropdown yet is not ready: the first tree published can be a page-link region alone.
        ready = Present(elementsWithClass(_, "p-paginator-rpp-dropdown").map(_.exists(declaresClick))),
        after = Seq(t => clickBubbling(t)(e => e.attrs.cssClasses.contains("p-select-option") && textOf(e) == "20"))
    ))

    "the page as first rendered draws an unbound rows dropdown the reader can use, before any mount runs" in {
        for d <- rowsTrigger(uic.Paginator().totalRecords(120).rowsPerPageOptions(Seq(10, 20)).render)
        yield assert(
            !d.attrs.cssClasses.contains("p-disabled") && !readOnly(d),
            s"an unbound rows count is the reader's: ${d.attrs.cssClasses}, ${d.attrs.ariaAttrs}"
        )
    }

    "a fixed page draws its jump list read-only: a tab stop that says it goes nowhere, and no card" in {
        val fixed = uic.Paginator().totalRecords(120).rows(10).page(2).template(PaginatorElement.JumpToPageDropdown).render
        for
            d    <- elementWithClass(fixed, "p-paginator-jtp-dropdown")
            card <- elementsWithClass(fixed, "p-uic-diagnostic")
        yield
            assert(!d.attrs.cssClasses.contains("p-disabled"), "a fixed page is not a disabled one")
            assert(readOnly(d) && d.attrs.tabIndex.contains(0) && !opens(d), s"reachable and inert: ${d.attrs.ariaAttrs}")
            assert(card.isEmpty, s"a fixed page is no mistake: ${card.map(textOf)}")
        end for
    }

end PaginatorTest
