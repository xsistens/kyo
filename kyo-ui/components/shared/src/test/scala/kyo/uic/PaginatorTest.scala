package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.Paginator` subscribes the page and the rows per page it is drawn from. */
class PaginatorTest extends UicTest:

    "the page and the rows per page are ONE region over the paginator" in {
        for
            page <- Signal.initRef(1)
            rows <- Signal.initRef(10)
            ui = uic.Paginator().totalRecords(120).rows(rows).page(page).render
            regions <- regionsAbove(ui, "p-paginator")
        yield assert(regions == 1, s"changing the rows writes the page too; a nested region repaints the older page: $regions")
    }

end PaginatorTest
