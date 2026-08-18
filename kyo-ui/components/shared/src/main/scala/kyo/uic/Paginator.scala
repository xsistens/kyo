package kyo.uic

import kyo.*
import kyo.UI.*

/** Paginator — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Paginator anatomy: `div.p-paginator.p-component[role=navigation]` >
  * `button.p-paginator-first`/`-prev` + `span.p-paginator-pages` of
  * `button.p-paginator-page[.p-paginator-page-selected]` + `-next`/`-last`,
  * then the optional template slots — the `.p-paginator-rpp-dropdown` Select,
  * the `.p-paginator-current` report, the `.p-paginator-jtp-input` field — in
  * Prime's default template order), so the extracted `@primeuix` paginator CSS
  * applies verbatim. This is the SAME markup DataTable and DataView embed
  * below their content.
  *
  * `page(ref)` binds the 0-based page index two-way (clamped at render);
  * clicking a target writes the ref back before firing `onPage`. Page links
  * window to `pageLinkSize` (Prime default 5) centered on the current page —
  * exactly PrimeVue's boundary math; like PrimeVue, the window just shifts (no
  * ellipsis buttons — the extracted sheet styles none). `rows` can bind to a
  * `SignalRef[Int]` so the rows-per-page dropdown (`rowsPerPageOptions`) can
  * write it back; a rows change repositions to the page that keeps the first
  * visible record visible.
  */
final case class Paginator private (
    totalRecordsV: Int = 0,
    rowsV: Int = 1,
    rowsRef: Maybe[SignalRef[Int]] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    currentPageV: Maybe[Int] = Absent,
    pageLinkSizeV: Int = 5,
    rppOptionsV: List[Int] = Nil,
    reportTemplateV: Maybe[String] = Absent,
    jtpInputFlag: Boolean = false,
    hostClassesV: List[String] = Nil,
    onPageF: Maybe[Int => Any < Async] = Absent
) extends Node:
    type Self = Paginator

    /** Total number of records to page over. */
    def totalRecords(n: Int): Paginator = copy(totalRecordsV = math.max(0, n))

    /** Records per page (the page count is `ceil(totalRecords / rows)`). */
    def rows(n: Int): Paginator = copy(rowsV = math.max(1, n))

    /** Binds the records-per-page two-way to `ref` (floored at 1): the
      * [[rowsPerPageOptions]] dropdown writes it back, and app writes re-render.
      */
    def rows(ref: SignalRef[Int]): Paginator = copy(rowsRef = Present(ref))

    /** Binds the 0-based page index two-way to `ref` (clamped at render). */
    def page(ref: SignalRef[Int]): Paginator = copy(pageRef = Present(ref))

    /** At most this many page links render, centered on the current page (Prime's
      * `pageLinkSize`, default 5); all pages render when they fit the window.
      */
    def pageLinkSize(n: Int): Paginator = copy(pageLinkSizeV = math.max(1, n))

    /** Renders Prime's rows-per-page Select (`.p-paginator-rpp-dropdown`) after
      * the nav buttons. Interactive only with the `rows(SignalRef[Int])` binding —
      * picking an option writes the ref and repositions to the page that keeps
      * the first visible record visible; without the binding it renders disabled
      * (the `month`-ref precedent on DatePicker).
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

    /** Fired with the target page index after the ref write-back. */
    def onPage(f: Int => Any < Async): Paginator = copy(onPageF = Present(f))

    /** Package-internal: host components (DataTable/DataView) stamp their
      * positional class (`p-datatable-paginator-bottom`, ...) on the root.
      */
    private[uic] def hostClass(cls: String): Paginator = copy(hostClassesV = hostClassesV :+ cls)

    /** Package-internal: hosts that already render inside their own page-ref
      * subscription supply the resolved page directly (no nested subscription).
      */
    private[uic] def currentPage(cur: Int): Paginator = copy(currentPageV = Present(cur))

    private def totalPages(rows: Int): Int = math.max(1, (totalRecordsV + rows - 1) / rows)

    private[uic] def render(using Frame): UI =
        def withPage(rows: Int): UI =
            currentPageV match
                case Present(cur) => body(cur, rows)
                case Absent =>
                    pageRef match
                        case Present(ref) => ref.render(body(_, rows))
                        case Absent       => body(0, rows)
        rowsRef match
            case Present(ref) => ref.render(r => withPage(math.max(1, r)))
            case Absent       => withPage(rowsV)
    end render

    private def body(page: Int, rows: Int)(using Frame): UI =
        val pages = totalPages(rows)
        val cur   = math.min(math.max(page, 0), pages - 1)

        def navButton(cls: String, glyph: IconGlyph, target: Int, isDisabled: Boolean, label: String): UI =
            var b = button.cssClass(s"p-paginator-$cls").jsProp("type", "button").aria("label", label)
            if isDisabled then b = b.cssClass("p-disabled").disabled(true)
            else b = b.onClick(activate(target))
            b(toChild(GlyphSvg(glyph, s"p-paginator-$cls-icon", "p-icon")))
        end navButton

        // PrimeVue's calculatePageLinkBoundaries: center the window on the current
        // page, then pull it back inside the boundaries.
        val visible  = math.min(pageLinkSizeV, pages)
        val startRaw = math.max(0, math.ceil(cur - visible / 2.0).toInt)
        val endRaw   = math.min(pages - 1, startRaw + visible - 1)
        val start    = math.max(0, startRaw - (pageLinkSizeV - (endRaw - startRaw + 1)))

        val pageButtons: List[UI] = (start to endRaw).toList.map { pi =>
            var b = button.cssClass("p-paginator-page").jsProp("type", "button").aria("label", s"Page ${pi + 1}")
            if pi == cur then b = b.cssClass("p-paginator-page-selected").aria("current", "page")
            else b = b.onClick(activate(pi))
            b((pi + 1).toString)
        }

        val rpp: List[UI] =
            if rppOptionsV.isEmpty then Nil
            else
                var sel = Select[Int]()
                    .options(rppOptionsV)(_.toString)
                    .extraClass("p-paginator-rpp-dropdown")
                    .current(rows.toString)
                    .accessibleName("Rows per page")
                rowsRef match
                    case Present(_) => sel = sel.onChange(v => changeRows(v, cur, rows))
                    case Absent     => sel = sel.disabled(true)
                List(sel.render)

        val report: List[UI] = reportTemplateV.toList.map { t =>
            val first = if totalRecordsV > 0 then cur * rows + 1 else 0
            val last  = math.min((cur + 1) * rows, totalRecordsV)
            val text = t
                .replace("{currentPage}", (cur + 1).toString)
                .replace("{totalPages}", pages.toString)
                .replace("{first}", first.toString)
                .replace("{last}", last.toString)
                .replace("{rows}", rows.toString)
                .replace("{totalRecords}", totalRecordsV.toString)
            span.cssClass("p-paginator-current")(text)
        }

        val jtp: List[UI] =
            if !jtpInputFlag then Nil
            else
                List(
                    span.cssClass("p-paginator-jtp-input")(
                        toChild(
                            Input()
                                .value((cur + 1).toString)
                                .accessibleName("Jump to page")
                                .onChange { s =>
                                    val target = s.trim.toIntOption.map(_ - 1).getOrElse(cur)
                                    activate(math.min(math.max(0, target), pages - 1))
                                }
                                .render
                        )
                    )
                )

        var root = div.cssClass("p-paginator").cssClass("p-component")
        hostClassesV.foreach(c => root = root.cssClass(c))
        val children: List[UI] =
            List(
                navButton("first", Icons.angleDoubleLeft, 0, cur == 0, "First Page"),
                navButton("prev", Icons.angleLeft, cur - 1, cur == 0, "Previous Page"),
                span.cssClass("p-paginator-pages")(pageButtons.map(toChild)*),
                navButton("next", Icons.angleRight, cur + 1, cur >= pages - 1, "Next Page"),
                navButton("last", Icons.angleDoubleRight, pages - 1, cur >= pages - 1, "Last Page")
            ) ++ rpp ++ report ++ jtp
        root.role("navigation")(children.map(toChild)*)
    end body

    /** Write the bound ref (when present), then fire `onPage`. Targets are valid
      * by construction: nav/page clicks aim at rendered pages, the jump input
      * clamps against the render-time page count.
      */
    private def activate(target: Int)(using Frame): Any < Async =
        val t = math.max(0, target)
        val write: Any < Async = pageRef match
            case Present(ref) => ref.set(t)
            case Absent       => ()
        val fire: Any < Async = onPageF match
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
    private def changeRows(selected: String, cur: Int, oldRows: Int)(using Frame): Any < Async =
        selected.trim.toIntOption match
            case Some(newRows) if newRows >= 1 =>
                val newPages = totalPages(newRows)
                val target   = math.min(cur * oldRows / newRows, newPages - 1)
                val writeRows: Any < Async = rowsRef match
                    case Present(ref) => ref.set(newRows)
                    case Absent       => ()
                val writePage: Any < Async = pageRef match
                    case Present(ref) => ref.set(math.max(0, target))
                    case Absent       => ()
                for
                    _ <- writeRows
                    r <- writePage
                yield r
                end for
            case _ => ()
end Paginator

object Paginator:
    def apply(): Paginator = new Paginator()
