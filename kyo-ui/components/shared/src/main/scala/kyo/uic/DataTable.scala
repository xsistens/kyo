package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** Horizontal alignment of one [[Column]] (applied to the header cell and every
  * body cell of the column via `.p-uic-dt-center` / `.p-uic-dt-end`).
  */
enum ColumnAlign derives CanEqual:
    case Start, Center, End

/** One column of a [[DataTable]] — a typed, hand-authored carrier: a `header`
  * label (also the column's identity in the `sort` spec), an optional plain-text
  * projection (used for cell text AND the global filter), an optional `body`
  * template rendering arbitrary UI per row, an optional `sortBy` ordering (which
  * makes the header clickable when the table has a `sort` ref), and an `align`.
  *
  * {{{
  * Column[Product]("Name")(_.name).sortBy(_.name)
  * Column[Product]("Price").body(p => span(fmt(p))).sortBy(_.price).align(ColumnAlign.End)
  * }}}
  */
final case class Column[A] private (
    headerV: String,
    textF: Maybe[A => String] = Absent,
    bodyF: Maybe[A => UI] = Absent,
    orderingV: Maybe[Ordering[A]] = Absent,
    alignV: ColumnAlign = ColumnAlign.Start
):
    /** Custom cell content, replacing (or standing in for) the text projection. */
    def body(f: A => UI): Column[A] = copy(bodyF = Present(f))

    /** Makes the column sortable by the projected key (header clicks cycle
      * ascending → descending → unsorted when the table has a `sort` ref).
      */
    def sortBy[B](f: A => B)(using ord: Ordering[B]): Column[A] =
        copy(orderingV = Present(Ordering.by(f)))

    def align(v: ColumnAlign): Column[A] = copy(alignV = v)
end Column

object Column:
    /** A column rendering (and filtering by) the plain-text projection. */
    def apply[A](header: String)(text: A => String): Column[A] =
        new Column[A](header, textF = Present(text))

    /** A column without a text projection — give it a [[Column.body]] template.
      * (No text projection also means the global filter cannot match it.)
      */
    def apply[A](header: String): Column[A] = new Column[A](header)
end Column

/** DataTable — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DataTable anatomy: `div.p-datatable.p-component[.p-datatable-hoverable]
  * [.p-datatable-striped][.p-datatable-gridlines][.p-datatable-sm|-lg]` >
  * `div.p-datatable-table-container` > `table.p-datatable-table` with
  * `th.p-datatable-header-cell[.p-datatable-sortable-column][.p-datatable-column-sorted]`
  * headers (each a `div.p-datatable-column-header-content` >
  * `span.p-datatable-column-title` + `span.p-datatable-sort-icon`) over body rows
  * carrying `.p-row-even`/`.p-row-odd`, `.p-datatable-selectable-row`, and
  * `.p-datatable-row-selected`, plus an embedded `div.p-paginator`), so the
  * extracted `@primeuix` datatable + paginator CSS applies.
  *
  * The header and body rows sit in real `thead.p-datatable-thead` and
  * `tbody.p-datatable-tbody` row groups, which is what the extracted sheet's
  * row, cell, hover, selection, striping and gridline rules are scoped to.
  *
  * Rows are TYPED and every behavior is pure `(data, ui-state refs) → markup`,
  * computed server-side at render:
  *   - `sort(ref)` — the table SORTS the rows itself. A header click cycles
  *     ascending → descending → removed (removable-sort semantics); clicking a
  *     DIFFERENT column appends to the spec, so multi-sort needs no modifier key
  *     (the spec is ordered: first entry = primary key).
  *   - `globalFilter(ref)` — case-insensitive contains-match over the columns'
  *     text projections.
  *   - `paginate(size)(pageRef)` — slices the (filtered, sorted) rows and renders
  *     the embedded Prime paginator; the page ref is 0-based and clamped.
  *   - `selectionMode` + `selected(ref)` — `Single`/`Multiple` select on row
  *     click; `Checkbox` renders Prime's checkbox column (`Radio` follows
  *     single-select semantics without a radio column for now).
  *   - `expanded(ref)` + `rowExpansionTemplate` — an expander-button column is
  *     auto-added; expanded rows are followed by a full-colspan
  *     `tr.p-datatable-row-expansion`.
  *
  * `rowKey` supplies the stable id behind selection/expansion and the
  * `onRowClick` payload. It is REQUIRED in practice whenever any of those is
  * bound: without it rows fall back to their position in the ORIGINAL rows list,
  * which survives sorting and filtering (both computed here) but not a data
  * change — reorder the rows and every selection, expansion and click payload
  * re-associates with a different record, silently. A table that binds identity
  * without a key therefore renders a loud `.p-uic-key-error` card above itself
  * rather than shipping that failure to production data.
  */
final case class DataTable[A] private (
    rowsV: List[A] = Nil,
    rowKeyF: Maybe[A => String] = Absent,
    cols: List[Column[A]] = Nil,
    sortRef: Maybe[SignalRef[List[(String, Boolean)]]] = Absent,
    filterRef: Maybe[SignalRef[String]] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    expandedRef: Maybe[SignalRef[Set[String]]] = Absent,
    expansionF: Maybe[A => UI] = Absent,
    stripedFlag: Boolean = false,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyMessageV: Maybe[TextValue] = Absent,
    onRowClickF: Maybe[String => Any < Async] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent
) extends Node:
    type Self = DataTable[A]

    /** Appends data rows. */
    def rows(rs: Seq[A]): DataTable[A] = copy(rowsV = rowsV ++ rs.toList)

    /** Stable row identity — the selection/expansion key and the `onRowClick`
      * payload. Required for reliable selection/expansion.
      */
    def rowKey(f: A => String): DataTable[A] = copy(rowKeyF = Present(f))

    /** Appends columns. */
    def columns(cs: Column[A]*): DataTable[A] = copy(cols = cols ++ cs.toList)

    /** Binds the ordered sort spec two-way: `(column header, ascending)` entries,
      * first = primary. Header clicks cycle asc → desc → removed; clicks on other
      * columns append (modifier-free multi-sort).
      */
    def sort(ref: SignalRef[List[(String, Boolean)]]): DataTable[A] = copy(sortRef = Present(ref))

    /** Binds the global filter query: case-insensitive contains-match over the
      * columns' text projections.
      */
    def globalFilter(ref: SignalRef[String]): DataTable[A] = copy(filterRef = Present(ref))

    /** Slices the rows into pages of `size` and renders the embedded paginator;
      * `ref` holds the 0-based page index (clamped at render).
      */
    def paginate(size: Int)(ref: SignalRef[Int]): DataTable[A] =
        copy(pageSizeV = Present(math.max(1, size)), pageRef = Present(ref))

    /** Selection semantics: `Single`/`Multiple` select on row click, `Checkbox`
      * via Prime's checkbox column; `None` (default) leaves rows inert.
      */
    def selectionMode(v: SelectionMode): DataTable[A] = copy(selectionModeV = v)

    /** Binds selection two-way to `ref` (a set of [[rowKey]] ids). */
    def selected(ref: SignalRef[Set[String]]): DataTable[A] = copy(selectedRef = Present(ref))

    /** Binds row expansion two-way to `ref` (a set of [[rowKey]] ids); pair with
      * [[rowExpansionTemplate]].
      */
    def expanded(ref: SignalRef[Set[String]]): DataTable[A] = copy(expandedRef = Present(ref))

    /** Content of the full-colspan expansion row below an expanded data row; setting
      * it auto-adds the expander-button column.
      */
    def rowExpansionTemplate(f: A => UI): DataTable[A] = copy(expansionF = Present(f))

    /** Zebra striping (`.p-datatable-striped` + `.p-row-odd` rows). */
    def stripedRows(v: Boolean): DataTable[A] = copy(stripedFlag = v)

    /** Cell borders on every edge (`.p-datatable-gridlines`). */
    def showGridlines(v: Boolean): DataTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-datatable-sm` / default / `.p-datatable-lg` cell paddings. */
    def size(v: Size): DataTable[A] = copy(sizeV = v)

    /** Text of the full-width `tr.p-datatable-empty-message` row shown when no rows
      * survive filtering.
      */
    def emptyMessage(v: String): DataTable[A] = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): DataTable[A] = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    /** Fired with the row key after any selection write from a row click. */
    def onRowClick(f: String => Any < Async): DataTable[A] = copy(onRowClickF = Present(f))

    /** `aria-label` for the table. */
    def accessibleName(v: String): DataTable[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): DataTable[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** `aria-labelledby` id reference for the table. */
    def accessibleNameRef(v: String): DataTable[A] = copy(accNameRefV = Present(v))

    // ---- render ----

    private def keyOf(a: A): String =
        rowKeyF match
            case Present(f) => f(a)
            case Absent     => rowsV.indexOf(a).toString

    private def rowClickSelects: Boolean =
        selectionModeV == SelectionMode.Single || selectionModeV == SelectionMode.Multiple ||
            selectionModeV == SelectionMode.Radio

    private def checkboxColumn: Boolean = selectionModeV == SelectionMode.Checkbox

    private def expanderColumn: Boolean = expansionF.isDefined

    private def rowInteractive: Boolean = rowClickSelects || onRowClickF.isDefined

    /** Renders through whichever ui-state refs are bound (nested reactive nodes
      * render through in SSR).
      */
    private def withRef[T](ref: Maybe[SignalRef[T]], fallback: T)(k: T => UI)(using Frame): UI =
        ref match
            case Present(r) => r.render(k)
            case Absent     => k(fallback)

    private[uic] def render(using Frame): UI =
        withRef(sortRef, List.empty[(String, Boolean)]) { sort =>
            withRef(filterRef, "") { query =>
                withRef(pageRef, 0) { page =>
                    withRef(selectedRef, Set.empty[String]) { sel =>
                        withRef(expandedRef, Set.empty[String]) { exp =>
                            body(sort, query, page, sel, exp)
                        }
                    }
                }
            }
        }

    private def body(
        sort: List[(String, Boolean)],
        query: String,
        page: Int,
        sel: Set[String],
        exp: Set[String]
    )(using Frame): UI =
        // 1. Global filter: contains-match over the columns' text projections.
        val filtered =
            if query.isEmpty then rowsV
            else
                val q = query.toLowerCase
                rowsV.filter(a => cols.exists(c => c.textF.exists(f => f(a).toLowerCase.contains(q))))

        // 2. Sort: apply the spec back-to-front through stable sorts, so the first
        //    entry ends up the primary key.
        val sorted = sort.reverse.foldLeft(filtered) { case (rs, (key, asc)) =>
            cols.find(_.headerV == key).flatMap(_.orderingV.toOption) match
                case Some(ord) => rs.sorted(using if asc then ord else ord.reverse)
                case None      => rs
        }

        // 3. Paginate: clamp the 0-based page, slice, and embed the standalone
        //    Paginator (resolved page passed directly — the table already renders
        //    inside its own page-ref subscription).
        val (paged, paginatorUI) = pageSizeV match
            case Present(size) =>
                val totalPages = math.max(1, (sorted.size + size - 1) / size)
                val cur        = math.min(math.max(page, 0), totalPages - 1)
                var pag = Paginator()
                    .totalRecords(sorted.size)
                    .rows(size)
                    .currentPage(cur)
                    .hostClass("p-datatable-paginator-bottom")
                pageRef.foreach(ref => pag = pag.page(ref))
                (sorted.slice(cur * size, cur * size + size), List(pag.render))
            case Absent => (sorted, Nil)

        val colCount = cols.length + (if checkboxColumn then 1 else 0) + (if expanderColumn then 1 else 0)

        val headRow: UI =
            val expanderTh: List[UI] = if expanderColumn then List(th.cssClass("p-datatable-header-cell")) else Nil
            val checkboxTh: List[UI] = if checkboxColumn then List(th.cssClass("p-datatable-header-cell")) else Nil
            val colThs: List[UI]     = cols.map(c => headerCell(c, sort))
            tr((expanderTh ++ checkboxTh ++ colThs).map(toChild)*)
        end headRow

        val bodyRows: List[UI] =
            if paged.isEmpty then
                List(
                    tr.cssClass("p-datatable-empty-message")(
                        emptyMessageV.getOrElse(TextValue.Const("No records found")) match
                            case TextValue.Const(t) => td.colspan(math.max(colCount, 1))(t): UI
                            case TextValue.Dyn(s)   => s.render(t => td.colspan(math.max(colCount, 1))(t))
                    )
                )
            else paged.zipWithIndex.flatMap((a, i) => dataRow(a, i, sel, exp, colCount))

        var tbl = table.cssClass("p-datatable-table")
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => tbl = tbl.aria("labelledby", v))
        val tableEl: UI = tbl(
            toChild(thead.cssClass("p-datatable-thead")(toChild(headRow))),
            toChild(tbody.cssClass("p-datatable-tbody")(bodyRows.map(toChild)*))
        )

        var root = div.cssClass("p-datatable").cssClass("p-component")
        // Prime: hoverable whenever a selection mode is set (checkbox included) or
        // rows react to clicks.
        if selectionModeV != SelectionMode.None || onRowClickF.isDefined then
            root = root.cssClass("p-datatable-hoverable")
        if stripedFlag then root = root.cssClass("p-datatable-striped")
        if gridlinesFlag then root = root.cssClass("p-datatable-gridlines")
        sizeV match
            case Size.Small  => root = root.cssClass("p-datatable-sm")
            case Size.Large  => root = root.cssClass("p-datatable-lg")
            case Size.Normal => ()
        end match
        root(
            (rowKeyCard ++ ((div.cssClass("p-datatable-table-container")(toChild(tableEl)): UI) :: paginatorUI)).map(toChild)*
        )
    end body

    /** The loud card rendered above the table when per-row IDENTITY is in use but
      * [[rowKey]] is unset (see [[KeyDiagnostics]]).
      *
      * The fallback key is the row's position, and position is not an identity: it
      * is stable under sorting and filtering (both are computed over the original
      * list here) but not under a data change. Reorder the underlying rows and every
      * selection, expansion and click payload silently re-associates with a
      * different record. The component cannot tell a static list from a live one, so
      * the moment identity is actually consumed the key stops being optional.
      */
    private def rowKeyCard(using Frame): List[UI] =
        val usesIdentity = selectedRef.isDefined || expandedRef.isDefined || onRowClickF.isDefined
        if rowKeyF.isDefined || !usesIdentity then Nil
        else
            List(KeyDiagnostics.card(
                "DataTable",
                "selection, expansion or onRowClick is bound but rowKey is unset, so rows are keyed by position " +
                    "and a data change re-associates that state with the wrong record; set rowKey",
                Nil
            ))
        end if
    end rowKeyCard

    /** One sortable/plain header cell with Prime's header-content anatomy. */
    private def headerCell(c: Column[A], sort: List[(String, Boolean)])(using Frame): UI =
        val sortable = c.orderingV.isDefined && sortRef.isDefined
        val active   = sort.find(_._1 == c.headerV)

        var cell = th.cssClass("p-datatable-header-cell")
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell.cssClass("p-datatable-sortable-column").tabIndex(0).onClick(toggleSort(c.headerV))
        active.foreach { (_, asc) =>
            cell = cell
                .cssClass("p-datatable-column-sorted")
                .aria("sort", if asc then "ascending" else "descending")
        }

        val sortIcon: List[UI] =
            if !sortable then Nil
            else
                val glyph = active match
                    case Some((_, true))  => Icons.sortAmountUpAlt
                    case Some((_, false)) => Icons.sortAmountDown
                    case None             => Icons.sortAlt
                List(GlyphSvg(glyph, "p-datatable-sort-icon"))

        cell(
            div.cssClass("p-datatable-column-header-content")(
                ((span.cssClass("p-datatable-column-title")(c.headerV): UI) :: sortIcon).map(toChild)*
            )
        )
    end headerCell

    /** Header click: asc → desc → removed on the clicked column; clicking another
      * column APPENDS its spec (modifier-free multi-sort).
      */
    private def toggleSort(key: String)(using Frame): Any < Async =
        sortRef match
            case Present(ref) =>
                ref.getAndUpdate { cur =>
                    cur.find(_._1 == key) match
                        case Some((_, true))  => cur.map(s => if s._1 == key then (key, false) else s)
                        case Some((_, false)) => cur.filterNot(_._1 == key)
                        case None             => cur :+ (key, true)
                }
            case Absent => ()

    /** One data row (plus its expansion row while expanded). */
    private def dataRow(a: A, index: Int, sel: Set[String], exp: Set[String], colCount: Int)(using Frame): List[UI] =
        val id    = keyOf(a)
        val isSel = sel.contains(id)
        val isExp = exp.contains(id)

        val expanderTd: List[UI] =
            if !expanderColumn then Nil
            else
                val glyph = if isExp then Icons.chevronDown else Icons.chevronRight
                List(
                    td(
                        button
                            .cssClass("p-datatable-row-toggle-button")
                            .jsProp("type", "button")
                            .aria("expanded", isExp.toString)
                            .aria("label", if isExp then "Row Collapse" else "Row Expand")
                            .onClick(toggleExpand(id))(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon")))
                    )
                )

        // Checkbox selection reuses Prime's checkbox anatomy (as Tree does).
        val checkboxTd: List[UI] =
            if !checkboxColumn then Nil
            else
                var cb = div.cssClass("p-checkbox").cssClass("p-component").aria("hidden", "true")
                if isSel then cb = cb.cssClass("p-checkbox-checked")
                cb = cb.onClick(toggleSelect(id))
                val icon: List[UI] = if isSel then List(GlyphSvg(Icons.check, "p-checkbox-icon")) else Nil
                List(td(cb(toChild(div.cssClass("p-checkbox-box")(icon.map(toChild)*)))))

        val dataTds: List[UI] = cols.map { c =>
            var cell = td
            c.alignV match
                case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
                case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
                case ColumnAlign.Start  => ()
            end match
            val content: HtmlChildVal = c.bodyF match
                case Present(f) => toChild(f(a))
                case Absent     => toChild(stringToUI(c.textF.map(_(a)).getOrElse("")))
            cell(content)
        }

        var row = tr.cssClass(if index % 2 == 0 then "p-row-even" else "p-row-odd")
        if rowClickSelects then row = row.cssClass("p-datatable-selectable-row")
        if isSel then row = row.cssClass("p-datatable-row-selected")
        if selectionModeV != SelectionMode.None then row = row.aria("selected", isSel.toString)
        if rowInteractive then row = row.tabIndex(0).onClick(activate(id))
        val rowEl: UI = row((expanderTd ++ checkboxTd ++ dataTds).map(toChild)*)

        val expansionRow: List[UI] =
            if isExp then
                expansionF.toList.map { f =>
                    tr.cssClass("p-datatable-row-expansion")(
                        td.colspan(math.max(colCount, 1))(toChild(f(a)))
                    )
                }
            else Nil

        rowEl :: expansionRow
    end dataRow

    private def toggleExpand(id: String)(using Frame): Any < Async =
        expandedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()

    private def toggleSelect(id: String)(using Frame): Any < Async =
        selectedRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case Absent       => ()

    /** Clicking a row updates the bound selection set (per the mode), then fires `onRowClick`. */
    private def activate(id: String)(using Frame): Any < Async =
        val setSelection: Any < Async = (selectedRef, selectionModeV) match
            case (Present(ref), SelectionMode.Single | SelectionMode.Radio) =>
                ref.getAndUpdate(cur => if cur == Set(id) then Set.empty else Set(id))
            case (Present(ref), SelectionMode.Multiple) =>
                ref.getAndUpdate(cur => if cur.contains(id) then cur - id else cur + id)
            case _ => ()
        val fireClick: Any < Async = onRowClickF match
            case Present(f) => f(id)
            case Absent     => ()
        for
            _ <- setSelection
            r <- fireClick
        yield r
        end for
    end activate
end DataTable

object DataTable:
    def apply[A](): DataTable[A] = new DataTable[A]()
