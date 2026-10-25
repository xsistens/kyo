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
  * `page(ref)` binds the 0-based page index two-way (clamped at render);
  * clicking a target writes the ref back before firing `onPage`. Page links
  * window to `pageLinkSize` (Prime default 5) centered on the current page,
  * exactly PrimeVue's boundary math; like PrimeVue, the window just shifts (no
  * ellipsis buttons: the extracted sheet styles none). `rows` can bind to a
  * `SignalRef[Int]` so the rows-per-page dropdown (`rowsPerPageOptions`) can
  * write it back; a rows change repositions to the page that keeps the first
  * visible record visible.
  */
final case class Paginator private (
    totalV: ReactiveValue[Int] = ReactiveValue.Const(0),
    rowsV: Int = 1,
    rowsRef: Maybe[SignalRef[Int]] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    currentPageV: Maybe[Int] = Absent,
    pageLinkSizeV: Int = 5,
    rppOptionsV: List[Int] = Nil,
    reportTemplateV: Maybe[String] = Absent,
    jtpInputFlag: Boolean = false,
    templateV: Maybe[List[PaginatorElement]] = Absent,
    hostClassesV: List[String] = Nil,
    onPageF: Maybe[Int => UI.Action] = Absent
) extends Node:
    type Self = Paginator

    /** Total number of records to page over. A `Signal[Int]` (a filtered count, say) changes the page count in place,
      * without building the paginator again.
      */
    def totalRecords(n: Int | Signal[Int]): Paginator = copy(totalV = ReactiveValue(n))

    /** Records per page (the page count is `ceil(totalRecords / rows)`). */
    def rows(n: Int): Paginator = copy(rowsV = math.max(1, n))

    /** Binds the records-per-page two-way to `ref` (floored at 1): the
      * [[rowsPerPageOptions]] dropdown writes it back, and app writes re-render.
      */
    def rows(ref: SignalRef[Int]): Paginator = copy(rowsRef = Present(ref))

    /** Package-internal: a host that owns the page size hands a constant one, dropping any ref a caller bound, since a
      * dropdown writing a ref the host does not page by would page a list nobody is looking at.
      */
    private[uic] def fixedRows(n: Int): Paginator = copy(rowsV = math.max(1, n), rowsRef = Absent)

    /** Binds the 0-based page index two-way to `ref` (clamped at render). */
    def page(ref: SignalRef[Int]): Paginator = copy(pageRef = Present(ref))

    /** At most this many page links render, centered on the current page (Prime's
      * `pageLinkSize`, default 5); all pages render when they fit the window.
      */
    def pageLinkSize(n: Int): Paginator = copy(pageLinkSizeV = math.max(1, n))

    /** Renders Prime's rows-per-page Select (`.p-paginator-rpp-dropdown`) after
      * the nav buttons. Interactive only with the `rows(SignalRef[Int])` binding:
      * picking an option writes the ref and repositions to the page that keeps
      * the first visible record visible; without the binding it renders disabled.
      */
    def rowsPerPageOptions(opts: Seq[Int]): Paginator =
        copy(rppOptionsV = opts.toList.filter(_ >= 1))

    /** Renders Prime's current-page report (`span.p-paginator-current`) from a
      * template with the placeholders `{currentPage}` `{totalPages}` `{first}`
      * `{last}` `{rows}` `{totalRecords}` (Prime's default template is
      * `"({currentPage} of {totalPages})"`).
      */
    def currentPageReport(template: String): Paginator = copy(reportTemplateV = Present(template))

    /** Renders Prime's jump-to-page input (`span.p-paginator-jtp-input` wrapping
      * the field): committing a number (change event) navigates to that 1-based
      * page, clamped to the valid range.
      */
    def jumpToPageInput(v: Boolean): Paginator = copy(jtpInputFlag = v)

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
    def template(elements: PaginatorElement*): Paginator = copy(templateV = Present(elements.toList))

    /** Fired with the target page index after the ref write-back. */
    def onPage(f: Int => UI.Action)(using Frame): Paginator = copy(onPageF = Present(Callback.of(f)))

    /** Package-internal: host components (DataTable/DataView) stamp their
      * positional class (`p-datatable-paginator-bottom`, ...) on the root.
      */
    private[uic] def hostClass(cls: String): Paginator = copy(hostClassesV = hostClassesV :+ cls)

    /** Package-internal: hosts that already render inside their own page-ref
      * subscription supply the resolved page directly (no nested subscription).
      */
    private[uic] def currentPage(cur: Int): Paginator = copy(currentPageV = Present(cur))

    private def totalPages(total: Int, rows: Int): Int = math.max(1, (total + rows - 1) / rows)

    private[uic] def render(using Frame): UI =
        val page: Reading[Int] =
            currentPageV match
                case Present(cur) => Reading.Fixed(cur)
                case Absent       => pageRef.fold(Reading.Fixed(0))(Reading.Live(_))
        val rows: Reading[Int] = rowsRef.fold(Reading.Fixed(rowsV))(ref => Reading.Live(ref.map(math.max(1, _))))
        val total              = Reading.of(totalV).map(math.max(0, _))
        // The rows and the total decide the page count; the page is read with them as ONE reading (see [[Reading]]):
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
                    .selection(rows.slot)
                    .accessibleName("Rows per page")
                rowsRef match
                    case Present(_) =>
                        sel = sel.onChange(_.fold(())(n =>
                            state.signal.current.map { case (p, (r, t)) => changeRows(n, View(p, r, t)) }
                        ))
                    case Absent => sel = sel.disabled(true)
                end match
                List(sel.render)

        // Prime's second way to reach a page directly: the same targets as the page links,
        // as a list rather than a row of buttons, which is what a hundred pages need. Its options are the pages: it
        // reads the pages the rows and the total give as a signal, and shows the page one way, so it is built once.
        val jtpDropdown: List[UI] =
            List(
                Select[Int]()
                    .options(shape.map[Seq[Int]]((r, t) => (0 until totalPages(t, r)).toList).slot)(p => (p + 1).toString)
                    .extraClass("p-paginator-jtp-dropdown")
                    .selection(state.map((p, s) => View.clamp(p, totalPages(s._2, s._1))).slot)
                    .accessibleName("Jump to page")
                    .onChange(_.fold(())(activate))
                    .render
            )

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
        val children: List[UI] = cards(layout) ++ layout.flatMap(elementUI)
        root.role("navigation")(children.map(toChild)*)
    end render

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
        else b = b.onClick(activate(target))
        b(toChild(GlyphSvg(glyph, s"p-paginator-$cls-icon", "p-icon")))
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
            else b = b.onClick(activate(pi))
            b((pi + 1).toString)
        }
        span.cssClass("p-paginator-pages")(buttons.map(toChild)*)
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
            toChild(
                Input()
                    .value((v.cur + 1).toString)
                    .accessibleName("Jump to page")
                    .onChange { s =>
                        val target = s.trim.toIntOption.map(_ - 1).getOrElse(v.cur)
                        activate(View.clamp(target, v.pages))
                    }
                    .render
            )
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
            val overruled =
                if !jtpInputFlag || layout.contains(PaginatorElement.JumpToPageInput) then Nil
                else
                    List(KeyDiagnostics.card(
                        "Paginator",
                        "the template is the layout and it does not name JumpToPageInput, so jumpToPageInput " +
                            "is not read; name it in the template or drop the flag",
                        Nil
                    ))
            val bare =
                if empty.isEmpty then Nil
                else
                    List(KeyDiagnostics.card(
                        "Paginator",
                        "the template names an element with nothing behind it, so it renders nothing",
                        empty
                    ))
            bare ++ overruled
        end if
    end cards

    /** Write the bound ref (when present), then fire `onPage`. Targets are valid
      * by construction: nav/page clicks aim at rendered pages, the jump input
      * clamps against the render-time page count.
      */
    private def activate(target: Int)(using Frame): UI.Action =
        val t                = math.max(0, target)
        val write: UI.Action = pageRef match
            case Present(ref) => ref.set(t)
            case Absent       => ()
        val fire: UI.Action = onPageF match
            case Present(f) => f(t)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate

    /** RPP pick: write the rows ref, then reposition to the page that keeps the
      * first visible record visible (clamped into the new range).
      */
    private def changeRows(newRows: Int, at: View)(using Frame): UI.Action =
        if newRows < 1 then ()
        else
            val newPages             = totalPages(at.total, newRows)
            val target               = math.min(at.cur * at.rows / newRows, newPages - 1)
            val writeRows: UI.Action = rowsRef match
                case Present(ref) => ref.set(newRows)
                case Absent       => ()
            val writePage: UI.Action = pageRef match
                case Present(ref) => ref.set(math.max(0, target))
                case Absent       => ()
            for
                _ <- writeRows
                r <- writePage
            yield r
            end for
end Paginator

object Paginator:
    def apply(): Paginator = new Paginator()
