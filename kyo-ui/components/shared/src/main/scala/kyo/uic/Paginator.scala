package kyo.uic

import kyo.*
import kyo.UI.*

/** One element of a [[Paginator]]'s layout, and the vocabulary [[Paginator.template]]
  * orders them with.
  *
  * Prime names the same set in a space-separated string (`paginatorTemplate`), where a
  * misspelt name renders nothing and says nothing. These are values, so a name that is
  * not one of them does not compile, and the order is the order of the list.
  */
enum PaginatorElement derives CanEqual:
    case FirstPageLink, PrevPageLink, PageLinks, NextPageLink, LastPageLink
    case RowsPerPageDropdown, CurrentPageReport, JumpToPageInput, JumpToPageDropdown

object PaginatorElement:

    /** Prime's default layout: the five navigation elements, nothing else. */
    val default: List[PaginatorElement] =
        List(FirstPageLink, PrevPageLink, PageLinks, NextPageLink, LastPageLink)
end PaginatorElement

/** Paginator: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Paginator anatomy: `div.p-paginator.p-component[role=navigation]` >
  * `button.p-paginator-first`/`-prev` + `span.p-paginator-pages` of
  * `button.p-paginator-page[.p-paginator-page-selected]` + `-next`/`-last`,
  * then the optional template slots (the `.p-paginator-rpp-dropdown` Select,
  * the `.p-paginator-current` report, the `.p-paginator-jtp-input` field) in
  * Prime's default template order), so the extracted `@primeuix` paginator CSS
  * applies verbatim. This is the SAME markup DataTable and DataView embed
  * below their content.
  *
  * `page` and `rows` each take a constant, a signal or a ref. A ref binds two
  * way: a click writes it, then [[onPageChange]] (or [[onRowsChange]]) hears the
  * new value. A signal or a constant binds one way: the paginator draws it and only
  * reports the reader's pick, so a page that lives in a route or a parent is moved
  * there and comes back through the signal. Page links window to `pageLinkSize`
  * (Prime default 5) centered on the current page, exactly PrimeVue's boundary
  * math; like PrimeVue, the window just shifts (no ellipsis buttons: the extracted
  * sheet styles none). A rows change repositions to the page that keeps the first
  * visible record visible, and reports that page as a page change.
  */
final class Paginator private[uic] (private[uic] val s: Paginator.State) extends Node, Stated:
    import s.*

    type Self = Paginator

    /** Total number of records to page over. A `Signal[Int]` (a filtered count, say) changes the page count in place,
      * without building the paginator again.
      */
    def totalRecords(n: Int | Signal[Int]): Paginator = new Paginator(s.copy(totalV = ReactiveValue(n)))

    /** Records per page (floored at 1; the page count is `ceil(totalRecords / rows)`).
      *
      * A `SignalRef` binds two way: the [[rowsPerPageOptions]] dropdown writes the reader's pick into it. A plain
      * `Signal` binds one way: the dropdown shows it and hands the pick to [[onRowsChange]] without writing. A constant
      * is a count the reader cannot change unless [[onRowsChange]] is there to take the pick, so without one its
      * dropdown is drawn read-only. Unbound, the paginator keeps the count the reader picks itself, starting at the first
      * [[rowsPerPageOptions]] entry (at one where there are none).
      */
    def rows(n: Int | Signal[Int]): Paginator = new Paginator(s.copy(rowsSlot = rowsSlot.bind(n)))

    /** Package-internal: a host that owns the page size hands a constant one, dropping any binding a caller made, since
      * a dropdown writing a ref the host does not page by would page a list nobody is looking at.
      */
    private[uic] def fixedRows(n: Int): Paginator = new Paginator(s.copy(rowsSlot = Slot[Int]().bind(math.max(1, n))))

    /** The 0-based page index (clamped at render).
      *
      * A `SignalRef` binds two way: a click writes it, then [[onPageChange]] hears the page. A plain `Signal` or a
      * constant binds one way: the paginator draws it and a click only reports to [[onPageChange]], which is how a page
      * held in a route or a parent is moved. A one-way page with no [[onPageChange]] cannot move and is reported in a
      * card.
      */
    def page(n: Int | Signal[Int]): Paginator = new Paginator(s.copy(pageSlot = pageSlot.bind(n)))

    /** At most this many page links render, centered on the current page (Prime's
      * `pageLinkSize`, default 5); all pages render when they fit the window.
      */
    def pageLinkSize(n: Int): Paginator = new Paginator(s.copy(pageLinkSizeV = math.max(1, n)))

    /** Renders Prime's rows-per-page Select (`.p-paginator-rpp-dropdown`) after
      * the nav buttons. A pick changes the [[rows]] (as that slot binds them) and
      * repositions to the page that keeps the first visible record visible.
      */
    def rowsPerPageOptions(opts: Seq[Int]): Paginator =
        new Paginator(s.copy(rppOptionsV = opts.toList.filter(_ >= 1)))

    /** Renders Prime's current-page report (`span.p-paginator-current`) from a
      * template with the placeholders `{currentPage}` `{totalPages}` `{first}`
      * `{last}` `{rows}` `{totalRecords}` (Prime's default template is
      * `"({currentPage} of {totalPages})"`).
      */
    def currentPageReport(template: String): Paginator = new Paginator(s.copy(reportTemplateV = Present(template)))

    /** Renders Prime's jump-to-page input (`span.p-paginator-jtp-input` wrapping
      * the field): committing a number (change event) navigates to that 1-based
      * page, clamped to the valid range.
      */
    def jumpToPageInput(v: Boolean): Paginator = new Paginator(s.copy(jtpInputFlag = v))

    /** Which elements render, and in which order (Prime's `paginatorTemplate`).
      *
      * Prime names them in a space-separated string, where a misspelt name renders
      * nothing and reports nothing; here they are values, so the compiler is what
      * catches a name that is not one of them.
      *
      * The list REPLACES the layout rather than adding to it: an element it does not
      * name does not render, [[jumpToPageInput]] included, and one it names twice
      * renders twice, which is what a template is for. What an element still needs to
      * have something to show stays where it was: `CurrentPageReport` reads
      * [[currentPageReport]], and the two dropdowns read [[rowsPerPageOptions]]. An
      * element named with nothing behind it renders a card.
      *
      * Without a template the layout is Prime's default order, with the optional
      * elements appended in the order Prime appends them.
      */
    def template(elements: PaginatorElement*): Paginator = new Paginator(s.copy(templateV = Present(elements.toList)))

    /** Fired with the page the reader asked for, after a two-way [[page]] is written: a page link, a navigation button,
      * either jump control, and the page a rows-per-page pick moves to.
      */
    def onPageChange(f: Int => UI.Action)(using Frame): Paginator = new Paginator(s.copy(pageSlot = pageSlot.hear(f)))

    /** Fired with the rows per page the reader picked, after a two-way [[rows]] is written, and before the page the
      * pick moves to is reported.
      */
    def onRowsChange(f: Int => UI.Action)(using Frame): Paginator = new Paginator(s.copy(rowsSlot = rowsSlot.hear(f)))

    /** Package-internal: host components (DataTable/DataView) stamp their
      * positional class (`p-datatable-paginator-bottom`, ...) on the root.
      */
    private[uic] def hostClass(cls: String): Paginator = new Paginator(s.copy(hostClassesV = hostClassesV :+ cls))

    /** Package-internal: hosts that already render inside their own page-ref
      * subscription supply the resolved page directly (no nested subscription).
      */
    private[uic] def currentPage(cur: Int): Paginator = new Paginator(s.copy(currentPageV = Present(cur)))

    /** Package-internal: a host that owns the page (a table, a data view) hands its binding and its own page event over. An
      * event the caller set on the paginator itself stays where the host has none.
      */
    private[uic] def hostPage(v: ReactiveValue[Int], report: Maybe[Int => UI.Action]): Paginator =
        new Paginator(s.copy(pageSlot = Slot(Present(v), report.orElse(pageSlot.hearer))))

    /** Package-internal: a host that owns the page size hands its binding over, with its own event and that event's name. */
    private[uic] def hostRows(v: ReactiveValue[Int], report: Maybe[Int => UI.Action], eventName: String): Paginator =
        new Paginator(s.copy(rowsSlot = Slot(Present(v), report.orElse(rowsSlot.hearer)), rowsEventName = eventName))

    private def totalPages(total: Int, rows: Int): Int = math.max(1, (total + rows - 1) / rows)

    private def pageReading(using Frame): ReactiveValue[Int] =
        currentPageV match
            case Present(cur) => ReactiveValue.Const(cur)
            case Absent       => pageSlot.reading(0)

    private def rowsReading(using Frame): ReactiveValue[Int] = rowsSlot.reading(rowsStart).map(math.max(1, _))

    /** The rows a page holds where nothing set them, which is where an unbound paginator's own count starts too: the first
      * [[rowsPerPageOptions]] entry, so the dropdown shows a count it offers, and one where there are no options.
      */
    private def rowsStart: Int = rppOptionsV.headOption.getOrElse(1)

    /** A constant page nobody hears is fixed: the links show it and go nowhere. A host drawing the page owns its clicks. */
    private def pageFixed: Boolean = currentPageV.isEmpty && pageSlot.fixed

    private def totalReading(using Frame): ReactiveValue[Int] = totalV.map(math.max(0, _))

    /** What the rows-per-page dropdown runs on a pick of `n`. */
    private[uic] def rowsPicked(n: Int)(using Frame): UI.Action =
        pageReading.and(rowsReading.and(totalReading)).current.map { case (p, (r, t)) => changeRows(n, View(p, r, t)) }

    /** What the rows-per-page dropdown runs on a change. The fold is typed: an untyped `fold(())(f)` infers a common type of
      * Unit and the action, which hands the action back as a value and runs nothing.
      */
    private[uic] def rowsChange(using Frame): Maybe[Int] => UI.Action = _.fold[UI.Action](())(rowsPicked)

    /** What the jump-to-page dropdown runs on a change. */
    private[uic] def jumpChange(using Frame): Maybe[Int] => UI.Action = _.fold[UI.Action](())(activate)

    private[uic] def render(using Frame): UI =
        // Standing alone and unbound, the paginator keeps the page the reader goes to, and the rows they pick, itself. A
        // host that draws the page (`currentPage`) or hands its own slots on is bound, and keeps nothing of its own.
        val ownsPage = currentPageV.isEmpty && pageSlot.binding.isEmpty
        val ownsRows = rowsSlot.binding.isEmpty
        if !ownsPage && !ownsRows then renderLive
        else
            UI.mounted {
                for
                    page <- if ownsPage then pageSlot.owned(0) else Kyo.lift(pageSlot)
                    rows <- rowsSlot.owned(rowsStart)
                yield new Paginator(s.copy(pageSlot = page, rowsSlot = rows)).renderLive
            }
        end if
    end render

    private def renderLive(using Frame): UI =
        val page: ReactiveValue[Int] = pageReading
        val rows: ReactiveValue[Int] = rowsReading
        val total                    = totalReading
        // The rows and the total decide the page count; the page is read with them as ONE reading (see [[ReactiveValue.and]]):
        // changing the rows writes both, and a nested region would repaint the page it closed over.
        val shape = rows.and(total)
        val state = page.and(shape)

        // Each part that follows the page is a region of its own over that one reading, and the two dropdowns are built
        // outside it: a Select is a mount, and a region around it would start the mount again on every page change,
        // replacing the field under the reader's focus. The root and the cards follow nothing.
        def following(part: View => List[UI]): List[UI] =
            List(state.render { case (p, (r, t)) => fragment(part(View(p, r, t))*) })

        val rpp: List[UI] =
            if rppOptionsV.isEmpty then Nil
            else
                var sel = Select[Int]()
                    .options(rppOptionsV)(_.toString)
                    .extraClass("p-paginator-rpp-dropdown")
                    .selection(rows.bound)
                    .accessibleName("Rows per page")
                // A fixed count is drawn read-only: reachable and readable, with nothing to pick.
                if rowsPickable then sel = sel.onChange(rowsChange)
                else sel = sel.readonly(true)
                List(sel.render)

        // Prime's second way to reach a page directly: the same targets as the page links,
        // as a list rather than a row of buttons, which is what a hundred pages need. Its options are the pages: it
        // reads the pages the rows and the total give as a signal, and shows the page one way, so it is built once.
        val jtpDropdown: List[UI] =
            val sel = Select[Int]()
                .options(shape.map[Seq[Int]]((r, t) => (0 until totalPages(t, r)).toList).bound)(p => (p + 1).toString)
                .extraClass("p-paginator-jtp-dropdown")
                .selection(state.map((p, s) => View.clamp(p, totalPages(s._2, s._1))).bound)
                .accessibleName("Jump to page")
            List((if pageFixed then sel.readonly(true) else sel.onChange(jumpChange)).render)
        end jtpDropdown

        def elementUI(e: PaginatorElement): List[UI] = e match
            case PaginatorElement.FirstPageLink =>
                following(v => List(navButton("first", Icons.angleDoubleLeft, 0, v.cur == 0, "First Page")))
            case PaginatorElement.PrevPageLink =>
                following(v => List(navButton("prev", Icons.angleLeft, v.cur - 1, v.cur == 0, "Previous Page")))
            case PaginatorElement.PageLinks    => following(v => List(pageLinks(v)))
            case PaginatorElement.NextPageLink =>
                following(v => List(navButton("next", Icons.angleRight, v.cur + 1, v.cur >= v.pages - 1, "Next Page")))
            case PaginatorElement.LastPageLink =>
                following(v => List(navButton("last", Icons.angleDoubleRight, v.pages - 1, v.cur >= v.pages - 1, "Last Page")))
            case PaginatorElement.RowsPerPageDropdown => rpp
            case PaginatorElement.CurrentPageReport   => if reportTemplateV.isEmpty then Nil else following(report)
            case PaginatorElement.JumpToPageInput     => following(v => List(jtpField(v)))
            case PaginatorElement.JumpToPageDropdown  => jtpDropdown

        // Without a template the layout is Prime's default order with the optional
        // elements appended where Prime appends them, which is what this rendered before
        // the setter existed.
        val layout = templateV.getOrElse(
            PaginatorElement.default ++
                (if rppOptionsV.isEmpty then Nil else List(PaginatorElement.RowsPerPageDropdown)) ++
                (if reportTemplateV.isEmpty then Nil else List(PaginatorElement.CurrentPageReport)) ++
                (if !jtpInputFlag then Nil else List(PaginatorElement.JumpToPageInput))
        )

        var root = div.cssClass("p-paginator").cssClass("p-component")
        hostClassesV.foreach(c => root = root.cssClass(c))
        val children: List[UI] = cards(layout) ++ unheard(layout) ++ layout.flatMap(elementUI)
        root.role("navigation")(UI.children(children)*)
    end renderLive

    /** What the parts that follow the page are drawn from: the page clamped into the page count the rows and the total
      * give.
      */
    final private case class View(page: Int, rows: Int, total: Int):
        val pages: Int = totalPages(total, rows)
        val cur: Int   = View.clamp(page, pages)

    private object View:
        def clamp(page: Int, pages: Int): Int = math.min(math.max(page, 0), pages - 1)

    private def navButton(cls: String, glyph: IconGlyph, target: Int, isDisabled: Boolean, label: String)(using Frame): UI =
        var b = button.cssClass(s"p-paginator-$cls").jsProp("type", "button").aria("label", label)
        if isDisabled then b = b.cssClass("p-disabled").disabled(true)
        else if pageFixed then b = b.aria("disabled", "true")
        else b = b.onClick(activate(target))
        b(GlyphSvg(glyph, s"p-paginator-$cls-icon", "p-icon"))
    end navButton

    private def pageLinks(v: View)(using Frame): UI =
        // PrimeVue's calculatePageLinkBoundaries: center the window on the current
        // page, then pull it back inside the boundaries.
        val visible           = math.min(pageLinkSizeV, v.pages)
        val startRaw          = math.max(0, math.ceil(v.cur - visible / 2.0).toInt)
        val endRaw            = math.min(v.pages - 1, startRaw + visible - 1)
        val start             = math.max(0, startRaw - (pageLinkSizeV - (endRaw - startRaw + 1)))
        val buttons: List[UI] = (start to endRaw).toList.map { pi =>
            var b = button.cssClass("p-paginator-page").jsProp("type", "button").aria("label", s"Page ${pi + 1}")
            if pi == v.cur then b = b.cssClass("p-paginator-page-selected").aria("current", "page")
            else if pageFixed then b = b.aria("disabled", "true")
            else b = b.onClick(activate(pi))
            b((pi + 1).toString)
        }
        span.cssClass("p-paginator-pages")(UI.children(buttons)*)
    end pageLinks

    private def report(v: View)(using Frame): List[UI] = reportTemplateV.toList.map { t =>
        val first = if v.total > 0 then v.cur * v.rows + 1 else 0
        val last  = math.min((v.cur + 1) * v.rows, v.total)
        val text  = t
            .replace("{currentPage}", (v.cur + 1).toString)
            .replace("{totalPages}", v.pages.toString)
            .replace("{first}", first.toString)
            .replace("{last}", last.toString)
            .replace("{rows}", v.rows.toString)
            .replace("{totalRecords}", v.total.toString)
        span.cssClass("p-paginator-current")(text)
    }

    private def jtpField(v: View)(using Frame): UI =
        span.cssClass("p-paginator-jtp-input")(
            if pageFixed then Input().value((v.cur + 1).toString).accessibleName("Jump to page").readonly(true).render
            else
                Input()
                    .value((v.cur + 1).toString)
                    .accessibleName("Jump to page")
                    .onChange { s =>
                        val target = s.trim.toIntOption.map(_ - 1).getOrElse(v.cur)
                        activate(View.clamp(target, v.pages))
                    }
                    .render
        )

    /** Elements a template names with nothing behind them, and the one switch a
      * template overrules. Prime's string template says nothing about either.
      */
    private def cards(layout: List[PaginatorElement])(using Frame): List[UI] =
        if templateV.isEmpty then Nil
        else
            val empty = List(
                if !layout.contains(PaginatorElement.CurrentPageReport) || reportTemplateV.isDefined then Nil
                else List("CurrentPageReport, which reads currentPageReport"),
                if !layout.contains(PaginatorElement.RowsPerPageDropdown) || rppOptionsV.nonEmpty then Nil
                else List("RowsPerPageDropdown, which reads rowsPerPageOptions")
            ).flatten
            Diagnostics.cards("Paginator")(
                Diagnostic.naming("the template names an element with nothing behind it, so it renders nothing", empty),
                Diagnostic(
                    jtpInputFlag && !layout.contains(PaginatorElement.JumpToPageInput),
                    "the template is the layout and it does not name JumpToPageInput, so jumpToPageInput " +
                        "is not read; name it in the template or drop the flag"
                )
            ).toList
        end if
    end cards

    /** Whether a rows-per-page pick reaches anybody: a two-way binding, or [[onRowsChange]]. Unbound, the count is the
      * paginator's own, which its mount keeps, so the page as first rendered already draws the dropdown the reader will use.
      */
    private def rowsPickable: Boolean = rowsSlot.binding.isEmpty || rowsSlot.heard

    /** One-way bindings a reader's pick cannot move: a page with no [[onPageChange]], and a signal of rows behind a
      * shown rows-per-page dropdown with no [[onRowsChange]]. A host that draws the page itself (`currentPage`) owns its
      * clicks, and a constant rows count is one the reader is not meant to change, so neither is reported.
      */
    private def unheard(layout: List[PaginatorElement])(using Frame): List[UI] =
        Diagnostics.cards("Paginator")(
            Diagnostic.unheard(pageSlot, Paginator.pageName, on = currentPageV.isEmpty),
            Diagnostic.unheard(
                rowsSlot,
                Slot.Name("Paginator", "rows", "a pick", rowsEventName),
                on = rppOptionsV.nonEmpty && layout.contains(PaginatorElement.RowsPerPageDropdown)
            )
        ).toList
    end unheard

    /** A page pick: written if [[page]] binds two way, then reported. Targets are valid by construction: nav and page
      * clicks aim at rendered pages, the jump controls clamp against the render-time page count.
      */
    private def activate(target: Int)(using Frame): UI.Action =
        pageSlot.change(math.max(0, target))

    /** RPP pick: write the rows ref, then reposition to the page that keeps the
      * first visible record visible (clamped into the new range).
      */
    private def changeRows(newRows: Int, at: View)(using Frame): UI.Action =
        if newRows < 1 then ()
        else
            val newPages = totalPages(at.total, newRows)
            val target   = math.min(at.cur * at.rows / newRows, newPages - 1)
            val page     = math.max(0, target)
            // Typed: an untyped `if` over Unit and an action infers Any and hands the action back unrun.
            val movePage: UI.Action = if page == at.cur then () else activate(page)
            rowsSlot.change(newRows).andThen(movePage)
end Paginator

object Paginator:
    final private[uic] case class State(
        totalV: ReactiveValue[Int] = ReactiveValue.Const(0),
        rowsSlot: Slot[Int] = Slot(),
        pageSlot: Slot[Int] = Slot(),
        currentPageV: Maybe[Int] = Absent,
        pageLinkSizeV: Int = 5,
        rppOptionsV: List[Int] = Nil,
        reportTemplateV: Maybe[String] = Absent,
        jtpInputFlag: Boolean = false,
        templateV: Maybe[List[PaginatorElement]] = Absent,
        hostClassesV: List[String] = Nil,
        // What a host calls the rows event, so the card for a one-way rows count names the setter its caller has.
        rowsEventName: String = "onRowsChange"
    )

    private val pageName   = Slot.Name("Paginator", "page", "a click", "onPageChange")
    def apply(): Paginator = new Paginator(State())
end Paginator
