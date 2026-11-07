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
final class Paginator private[uic] (private[uic] val p: Paginator.Props) extends Node, HasProps:

    type Self = Paginator

    /** Total number of records to page over. A `Signal[Int]` (a filtered count, say) changes the page count in place,
      * without building the paginator again.
      */
    def totalRecords(n: Prop[Int]): Paginator = new Paginator(p.copy(total = n))

    /** Records per page (floored at 1; the page count is `ceil(totalRecords / rows)`).
      *
      * A `SignalRef` binds two way: the [[rowsPerPageOptions]] dropdown writes the reader's pick into it. A plain
      * `Signal` binds one way: the dropdown shows it and hands the pick to [[onRowsChange]] without writing. A constant
      * is a count the reader cannot change unless [[onRowsChange]] is there to take the pick, so without one its
      * dropdown is drawn read-only. Unbound, the paginator keeps the count the reader picks itself, starting at the first
      * [[rowsPerPageOptions]] entry (at one where there are none).
      */
    def rows(n: Prop[Int]): Paginator = new Paginator(p.copy(rows = p.rows.bind(n)))

    /** Package-internal: a host that owns the page size hands a constant one, dropping any binding a caller made, since
      * a dropdown writing a ref the host does not page by would page a list nobody is looking at.
      */
    private[uic] def fixedRows(n: Int): Paginator = new Paginator(p.copy(rows = Slot[Int]().bind(math.max(1, n))))

    /** The 0-based page index (clamped at render).
      *
      * A `SignalRef` binds two way: a click writes it, then [[onPageChange]] hears the page. A plain `Signal` or a
      * constant binds one way: the paginator draws it and a click only reports to [[onPageChange]], which is how a page
      * held in a route or a parent is moved. A one-way page with no [[onPageChange]] cannot move and is reported in a
      * card. Unbound, the paginator keeps the page the reader goes to itself, starting at the first.
      */
    def page(n: Prop[Int]): Paginator = new Paginator(p.copy(page = p.page.bind(n)))

    /** At most this many page links render, centered on the current page (Prime's
      * `pageLinkSize`, default 5); all pages render when they fit the window.
      */
    def pageLinkSize(n: Prop[Int]): Paginator = new Paginator(p.copy(pageLinkSize = n))

    /** Renders Prime's rows-per-page Select (`.p-paginator-rpp-dropdown`) after
      * the nav buttons. A pick changes the [[rows]] (as that slot binds them) and
      * repositions to the page that keeps the first visible record visible. An unbound [[rows]] starts at the first option.
      */
    def rowsPerPageOptions(opts: Seq[Int]): Paginator =
        val options = opts.toList.filter(_ >= 1)
        new Paginator(p.copy(rowsPerPageOptions = options, rows = p.rows.copy(start = Present(options.headOption.getOrElse(1)))))
    end rowsPerPageOptions

    /** Renders Prime's current-page report (`span.p-paginator-current`) from a
      * template with the placeholders `{currentPage}` `{totalPages}` `{first}`
      * `{last}` `{rows}` `{totalRecords}` (Prime's default template is
      * `"({currentPage} of {totalPages})"`).
      */
    def currentPageReport(template: Prop[String]): Paginator = new Paginator(p.copy(currentPageReport = Present(template)))

    /** Renders Prime's jump-to-page input (`span.p-paginator-jtp-input` wrapping
      * the field): committing a number (change event) navigates to that 1-based
      * page, clamped to the valid range.
      */
    def jumpToPageInput(v: Prop[Boolean]): Paginator = new Paginator(p.copy(jumpToPageInput = v))

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
    def template(elements: PaginatorElement*): Paginator = new Paginator(p.copy(template = Present(elements.toList)))

    /** Fired with the page the reader asked for, after a two-way [[page]] is written: a page link, a navigation button,
      * either jump control, and the page a rows-per-page pick moves to.
      */
    def onPageChange(f: Int => UI.Action)(using Frame): Paginator = new Paginator(p.copy(page = p.page.hear(f)))

    /** Fired with the rows per page the reader picked, after a two-way [[rows]] is written, and before the page the
      * pick moves to is reported.
      */
    def onRowsChange(f: Int => UI.Action)(using Frame): Paginator = new Paginator(p.copy(rows = p.rows.hear(f)))

    /** Package-internal: host components (DataTable/DataView) stamp their
      * positional class (`p-datatable-paginator-bottom`, ...) on the root.
      */
    private[uic] def hostClass(cls: String): Paginator = new Paginator(p.copy(hostClasses = p.hostClasses :+ cls))

    /** Package-internal: hosts that already render inside their own page-ref
      * subscription supply the resolved page directly (no nested subscription).
      */
    private[uic] def currentPage(cur: Int): Paginator = new Paginator(p.copy(currentPage = Present(cur)))

    /** Package-internal: a host that owns the page (a table, a data view) hands its binding and its own page event over. An
      * event the caller set on the paginator itself stays where the host has none.
      */
    private[uic] def hostPage(v: ReactiveValue[Int], report: Maybe[Int => UI.Action]): Paginator =
        new Paginator(p.copy(page = Slot(Present(v.bound), report.orElse(p.page.hearer))))

    /** Package-internal: a host that owns the page size hands its binding over, with its own event and that event's name. */
    private[uic] def hostRows(v: ReactiveValue[Int], report: Maybe[Int => UI.Action], eventName: String): Paginator =
        new Paginator(p.copy(rows = Slot(Present(v.bound), report.orElse(p.rows.hearer)), rowsEventName = eventName))

    /** What the rows-per-page dropdown runs on a pick of `n`. */
    private[uic] def rowsPicked(n: Int)(using Frame): UI.Action = Paginator.rowsPicked(p, n)

    /** What the rows-per-page dropdown runs on a change. */
    private[uic] def rowsChange(using Frame): Maybe[Int] => UI.Action = _.fold[UI.Action](())(rowsPicked)

    /** What the jump-to-page dropdown runs on a change. */
    private[uic] def jumpChange(using Frame): Maybe[Int] => UI.Action = _.fold[UI.Action](())(Paginator.activate(p, _))

    /** Standing alone, the paginator keeps the page the reader goes to, and the rows they pick, itself: an unbound slot is the
      * instance's own, which the engine binds where a run leaves it unbound.
      */
    private[uic] def render(using Frame): UI = UI.mountedWith(p)(Paginator.drawn)

    /** The paginator with no instance of its own, for a host (a table, a data view) that binds the page and the rows itself
      * and draws the paginator in its own region.
      */
    private[uic] def renderInPlace(using Frame): UI = Paginator.drawn(new UI.Live(p, Absent))
end Paginator

object Paginator:
    final private[uic] case class Props(
        total: Prop[Int] = 0,
        rows: Slot[Int] = Slot(start = Present(1)),
        page: Slot[Int] = Slot(start = Present(0)),
        currentPage: Maybe[Int] = Absent,
        pageLinkSize: Prop[Int] = 5,
        rowsPerPageOptions: List[Int] = Nil,
        currentPageReport: Maybe[Prop[String]] = Absent,
        jumpToPageInput: Prop[Boolean] = false,
        template: Maybe[List[PaginatorElement]] = Absent,
        hostClasses: List[String] = Nil,
        // What a host calls the rows event, so the card for a one-way rows count names the setter its caller has.
        rowsEventName: String = "onRowsChange"
    ) derives CanEqual

    private val pageName   = Slot.Name("Paginator", "page", "a click", "onPageChange")
    def apply(): Paginator = new Paginator(Props())

    private def totalPages(total: Int, rows: Int): Int = math.max(1, (total + rows - 1) / rows)

    /** The rows a run's props stand for: the binding, or where the slot starts. */
    private def rowsOf(q: Props)(using Frame): Prop[Int] = Prop.map(q.rows.value(1))(math.max(1, _))

    /** The page a run's props stand for: the page a host drew, the binding, or where the slot starts. */
    private def pageOf(q: Props): Prop[Int] = q.currentPage.fold(q.page.value(0))(c => c)

    /** A constant page nobody hears is fixed: the links show it and go nowhere. A host drawing the page owns its clicks. */
    private def pageFixed(q: Props): Boolean = q.currentPage.isEmpty && q.page.fixed

    /** Whether a rows-per-page pick reaches anybody: a two-way binding, or [[onRowsChange]]. Unbound, the count is the
      * paginator's own, so the page as first rendered already draws the dropdown the reader will use.
      */
    private def rowsPickable(q: Props): Boolean = q.rows.bound.isEmpty || q.rows.heard

    /** A page pick: written if [[page]] binds two way, then reported. Targets are valid by construction: nav and page
      * clicks aim at rendered pages, the jump controls clamp against the render-time page count.
      */
    private def activate(q: Props, target: Int)(using Frame): UI.Action = q.page.change(math.max(0, target))

    /** A rows-per-page pick of `n`, read against the page, the rows and the total as they are now. */
    private def rowsPicked(q: Props, n: Int)(using Frame): UI.Action =
        val now = Prop.zip(pageOf(q), Prop.zip(rowsOf(q), Prop.map(q.total)(math.max(0, _))))
        Prop.signal(now).current.map { case (pg, (r, t)) => changeRows(q, n, View(pg, r, t)) }

    /** RPP pick: write the rows ref, then reposition to the page that keeps the
      * first visible record visible (clamped into the new range).
      */
    private def changeRows(q: Props, newRows: Int, at: View)(using Frame): UI.Action =
        if newRows < 1 then ()
        else
            val newPages = totalPages(at.total, newRows)
            val target   = math.min(at.cur * at.rows / newRows, newPages - 1)
            val page     = math.max(0, target)
            // Typed: an untyped `if` over Unit and an action infers Any and hands the action back unrun.
            val movePage: UI.Action = if page == at.cur then () else activate(q, page)
            q.rows.change(newRows).andThen(movePage)

    /** What of the props builds the paginator and follows no page: the layout's parts, what each element needs, who hears. */
    final private case class Base(
        template: Maybe[List[PaginatorElement]],
        options: List[Int],
        report: Boolean,
        pageFixed: Boolean,
        rowsPickable: Boolean,
        pageDeaf: Boolean,
        rowsDeaf: Boolean,
        hosted: Boolean,
        rowsEventName: String,
        hostClasses: List[String]
    ) derives CanEqual:
        /** Without a template the layout is Prime's default order with the optional elements appended where Prime appends
          * them, which is what this rendered before the setter existed.
          */
        def layout(jumpInput: Boolean): List[PaginatorElement] = template.getOrElse(
            PaginatorElement.default ++
                (if options.isEmpty then Nil else List(PaginatorElement.RowsPerPageDropdown)) ++
                (if !report then Nil else List(PaginatorElement.CurrentPageReport)) ++
                (if !jumpInput then Nil else List(PaginatorElement.JumpToPageInput))
        )
    end Base

    private object Base:
        def apply(q: Props): Base =
            Base(
                q.template,
                q.rowsPerPageOptions,
                q.currentPageReport.isDefined,
                pageFixed(q),
                rowsPickable(q),
                q.page.deaf,
                q.rows.deaf,
                q.currentPage.isDefined,
                q.rowsEventName,
                q.hostClasses
            )
    end Base

    /** What the parts that follow the page are drawn from: the page clamped into the page count the rows and the total
      * give.
      */
    final private case class View(page: Int, rows: Int, total: Int):
        val pages: Int = totalPages(total, rows)
        val cur: Int   = View.clamp(page, pages)

    private object View:
        def clamp(page: Int, pages: Int): Int = math.min(math.max(page, 0), pages - 1)

    /** The paginator, drawn once over `props`, which hand it `page` and `rows` bound: to the instance's own refs where a run
      * binds nothing (carrying on from a binding a run drops), unbound where it is drawn in place and its host binds them.
      *
      * Each part that follows the page (the links, the buttons, the report, the jump field) is a region of its own over the
      * page, the rows and the total read as ONE value: changing the rows writes the page too, and a nested region would
      * repaint the page it closed over. The two dropdowns are built outside those regions: a Select is a mount, and a region
      * around it would start the mount again on every page change, replacing the field under the reader's focus. What
      * builds the layout is one `reactive` block, drawn again only when a run changes it.
      */
    private def drawn(props: UI.Live[Props])(using Frame): UI =
        val page                              = props(pageOf(_))
        val rows                              = props(rowsOf(_))
        val total                             = props(q => Prop.map(q.total)(math.max(0, _)))
        val linkSize                          = props(q => Prop.map(q.pageLinkSize)(math.max(1, _)))
        val reportOf                          = props(_.currentPageReport)
        val base                              = props(Base(_))
        val jumpInput                         = props(_.jumpToPageInput)
        def go(target: Int): UI.Action        = props.call(identity).map(activate(_, target))
        val pickRows: Maybe[Int] => UI.Action =
            _.fold[UI.Action](())(n => props.call(identity).map(rowsPicked(_, n)))
        val jump: Maybe[Int] => UI.Action = _.fold[UI.Action](())(go)

        def following(part: View => List[UI]): List[UI] =
            List(UI.reactive(fragment(part(View(page.read, rows.read, total.read))*)))

        def navButton(cls: String, glyph: IconGlyph, target: Int, isDisabled: Boolean, label: String, fixed: Boolean): UI =
            button.cssClass(s"p-paginator-$cls").jsProp("type", "button").aria("label", label)
                .when(isDisabled)(_.cssClass("p-disabled").disabled(true))
                .when(!isDisabled && fixed)(_.aria("disabled", "true"))
                .when(!isDisabled && !fixed)(_.onClick(go(target)))(GlyphSvg(glyph, s"p-paginator-$cls-icon", "p-icon"))

        def pageLinks(v: View, size: Int, fixed: Boolean): UI =
            // PrimeVue's calculatePageLinkBoundaries: center the window on the current page, then pull it back inside the
            // boundaries.
            val visible           = math.min(size, v.pages)
            val startRaw          = math.max(0, math.ceil(v.cur - visible / 2.0).toInt)
            val endRaw            = math.min(v.pages - 1, startRaw + visible - 1)
            val start             = math.max(0, startRaw - (size - (endRaw - startRaw + 1)))
            val buttons: List[UI] = (start to endRaw).toList.map { pi =>
                button.cssClass("p-paginator-page").jsProp("type", "button").aria("label", s"Page ${pi + 1}")
                    .when(pi == v.cur)(_.cssClass("p-paginator-page-selected").aria("current", "page"))
                    .when(pi != v.cur && fixed)(_.aria("disabled", "true"))
                    .when(pi != v.cur && !fixed)(_.onClick(go(pi)))((pi + 1).toString)
            }
            span.cssClass("p-paginator-pages")(UI.children(buttons)*)
        end pageLinks

        def report(v: View, t: String): UI =
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
        end report

        def jtpField(v: View, fixed: Boolean): UI =
            val field = Input().value((v.cur + 1).toString).accessibleName("Jump to page")
            span.cssClass("p-paginator-jtp-input")(
                if fixed then field.readonly(true).render
                else
                    field.onChange { s =>
                        val target = s.trim.toIntOption.map(_ - 1).getOrElse(v.cur)
                        go(View.clamp(target, v.pages))
                    }.render
            )
        end jtpField

        UI.reactive {
            val b      = base.read
            val jtp    = jumpInput.read
            val layout = b.layout(jtp)

            val rpp: List[UI] =
                if b.options.isEmpty then Nil
                else
                    val sel = Select[Int]().options(b.options)(_.toString).extraClass("p-paginator-rpp-dropdown")
                        .selection(rows).accessibleName("Rows per page")
                    // A fixed count is drawn read-only: reachable and readable, with nothing to pick.
                    List((if b.rowsPickable then sel.onChange(pickRows) else sel.readonly(true)).render)

            // Prime's second way to reach a page directly: the same targets as the page links, as a list rather than a row of
            // buttons, which is what a hundred pages need. Its options are the pages the rows and the total give, read as a
            // prop, and it shows the page one way, so it is built once.
            val jtpDropdown: List[UI] =
                val pages = Prop.map(Prop.zip(rows, total))((r, t) => (0 until totalPages(t, r)).toList: Seq[Int])
                val shown = Prop.map(Prop.zip(page, Prop.zip(rows, total)))((pg, rt) => View.clamp(pg, totalPages(rt._2, rt._1)))
                val sel   = Select[Int]().options(pages)(pg => (pg + 1).toString).extraClass("p-paginator-jtp-dropdown")
                    .selection(shown).accessibleName("Jump to page")
                List((if b.pageFixed then sel.readonly(true) else sel.onChange(jump)).render)
            end jtpDropdown

            def elementUI(e: PaginatorElement): List[UI] = e match
                case PaginatorElement.FirstPageLink =>
                    following(v => List(navButton("first", Icons.angleDoubleLeft, 0, v.cur == 0, "First Page", b.pageFixed)))
                case PaginatorElement.PrevPageLink =>
                    following(v => List(navButton("prev", Icons.angleLeft, v.cur - 1, v.cur == 0, "Previous Page", b.pageFixed)))
                case PaginatorElement.PageLinks =>
                    List(UI.reactive(pageLinks(View(page.read, rows.read, total.read), linkSize.read, b.pageFixed)))
                case PaginatorElement.NextPageLink =>
                    following(v => List(navButton("next", Icons.angleRight, v.cur + 1, v.cur >= v.pages - 1, "Next Page", b.pageFixed)))
                case PaginatorElement.LastPageLink =>
                    following(v =>
                        List(navButton("last", Icons.angleDoubleRight, v.pages - 1, v.cur >= v.pages - 1, "Last Page", b.pageFixed))
                    )
                case PaginatorElement.RowsPerPageDropdown => rpp
                case PaginatorElement.CurrentPageReport   =>
                    if !b.report then Nil
                    else
                        List(UI.reactive {
                            val v = View(page.read, rows.read, total.read)
                            reportOf.read.fold(UI.empty: UI)(report(v, _))
                        })
                case PaginatorElement.JumpToPageInput    => following(v => List(jtpField(v, b.pageFixed)))
                case PaginatorElement.JumpToPageDropdown => jtpDropdown

            // Elements a template names with nothing behind them, and the one switch a template overrules: Prime's string
            // template says nothing about either.
            val cards: List[UI] =
                if b.template.isEmpty then Nil
                else
                    val empty = List(
                        if !layout.contains(PaginatorElement.CurrentPageReport) || b.report then Nil
                        else List("CurrentPageReport, which reads currentPageReport"),
                        if !layout.contains(PaginatorElement.RowsPerPageDropdown) || b.options.nonEmpty then Nil
                        else List("RowsPerPageDropdown, which reads rowsPerPageOptions")
                    ).flatten
                    Diagnostics.cards("Paginator")(
                        Diagnostic.naming("the template names an element with nothing behind it, so it renders nothing", empty),
                        Diagnostic(
                            jtp && !layout.contains(PaginatorElement.JumpToPageInput),
                            "the template is the layout and it does not name JumpToPageInput, so jumpToPageInput " +
                                "is not read; name it in the template or drop the flag"
                        )
                    ).toList

            // One-way bindings a reader's pick cannot move: a page with no onPageChange, and a signal of rows behind a shown
            // rows-per-page dropdown with no onRowsChange. A host that draws the page itself owns its clicks, and a constant
            // rows count is one the reader is not meant to change, so neither is reported.
            val unheard: List[UI] = Diagnostics.cards("Paginator")(
                Diagnostic(!b.hosted && b.pageDeaf, pageName.advice),
                Diagnostic(
                    b.options.nonEmpty && layout.contains(PaginatorElement.RowsPerPageDropdown) && b.rowsDeaf,
                    Slot.Name("Paginator", "rows", "a pick", b.rowsEventName).advice
                )
            ).toList

            val root = b.hostClasses.foldLeft(div.cssClass("p-paginator").cssClass("p-component"))(_.cssClass(_))
            root.role("navigation")(UI.children(cards ++ unheard ++ layout.flatMap(elementUI))*)
        }
    end drawn
end Paginator
