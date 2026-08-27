package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** DataTable — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DataTable anatomy: `div.p-datatable.p-component[.p-datatable-hoverable]
  * [.p-datatable-striped][.p-datatable-gridlines][.p-datatable-sm|-lg]` >
  * `div.p-datatable-table-container` > `table.p-datatable-table` with
  * `th.p-datatable-header-cell[.p-datatable-sortable-column][.p-datatable-column-sorted]`
  * headers (each a `div.p-datatable-column-header-content` >
  * `span.p-datatable-column-title` + `span.p-datatable-sort-icon` + the
  * multi-sort `span.p-datatable-sort-badge`) over body rows
  * carrying `.p-row-even`/`.p-row-odd`, `.p-datatable-selectable-row`, and
  * `.p-datatable-row-selected`, plus an embedded `div.p-paginator`), so the
  * extracted `@primeuix` datatable + paginator CSS applies.
  *
  * The header and body rows sit in real `thead.p-datatable-thead` and
  * `tbody.p-datatable-tbody` row groups, the `thead` holding one `tr` per level of
  * [[headerGroup]] nesting, which is what the extracted sheet's
  * row, cell, hover, selection, striping and gridline rules are scoped to.
  * Columns carrying a [[Column.footer]] add a `tfoot.p-datatable-tfoot` summary
  * row; `header`/`footer` are the two slots outside the table
  * (`div.p-datatable-header` above it, `div.p-datatable-footer` below the
  * paginator).
  *
  * Rows are TYPED and every behavior is pure `(data, ui-state refs) → markup`,
  * computed server-side at render:
  *   - `sort(ref)`: the table SORTS the rows itself, from an ordered [[SortKey]]
  *     spec whose first sorting entry is the primary key. An entry names its column
  *     by path, the labels of the [[headerGroup]]s around it followed by its own
  *     header, so a column in no group is named by its header alone and one header
  *     under two groups still names two different columns. What a plain header click does
  *     depends on how many columns sort. With one, it owns the whole cycle: ascending,
  *     descending, off. With several, it only REVERSES the clicked column, in place, so a
  *     spec built up over several clicks cannot lose a key because one header was clicked
  *     once too often; there, switching a column off belongs to Ctrl or Cmd. A plain
  *     click on a column that is not sorting makes it the single key the reader
  *     controls either way: entries they cannot reach, a column carrying
  *     `sortable(false)` or a path naming no column here, keep their slots ahead of
  *     it, since a click may only clear what a click could put back.
  *     Ctrl or Cmd click is the multi-key control: it adds a column, or advances the one
  *     already there WITHOUT moving it (ascending, descending, and, while
  *     [[removableSort]] is on, unsorted while keeping its slot). Holding the slot is
  *     what makes a mis-click cheap, since the next Ctrl click on the same header
  *     restores the column at the rank it had. While two or more columns sort, each
  *     sorted header carries its 1-based rank in a `.p-datatable-sort-badge`, which is
  *     the only thing that says which key wins.
  *   - `globalFilter(ref)` — case-insensitive contains-match over the columns'
  *     text projections.
  *   - `paginate(size)(pageRef)` — slices the (filtered, sorted) rows and renders
  *     the embedded Prime paginator; the page ref is 0-based and clamped.
  *   - `selectionMode` + `selected(ref)` — `Single`/`Multiple` select on row
  *     click; `Checkbox` renders Prime's checkbox column, whose header carries the
  *     binary select-all over every row that survived the global filter (`Radio`
  *     follows single-select semantics without a radio column for now).
  *   - `expanded(ref)` + `rowExpansionTemplate` — an expander-button column is
  *     auto-added; expanded rows are followed by a full-colspan
  *     `tr.p-datatable-row-expansion`.
  *   - `groupBy(levels)`: one nested level per argument, outermost first. Each
  *     run of consecutive rows sharing a level's key becomes a group, headed by a
  *     `tr.p-datatable-row-group-header` and optionally closed by a summary row;
  *     `expandedGroups(ref)` makes them collapsible, keyed by [[GroupPath]].
  *   - `headerGroup(label)(columns)`: a labelled cell spanning the columns written
  *     under it, adding one header row per level of nesting. The spans are read off
  *     that tree rather than declared, so a header of any depth cannot fall out of
  *     step with the columns beneath it.
  *   - `Column.rowSpan(key)`: the other way to show a key, where that column's cells merge
  *     across their run, clipped by the merged columns left of them and by the
  *     innermost group, so two spans can never cross.
  *   - `loading(flag)`: a spinner over `.p-datatable-mask` covers the table.
  *   - `scrollHeight(css)`: caps and scrolls the container, pinning the row
  *     groups to its edges.
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
    cols: List[ColumnTree[A]] = Nil,
    sortRef: Maybe[SignalRef[List[SortKey]]] = Absent,
    filterRef: Maybe[SignalRef[String]] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    selectionModeV: SelectionMode = SelectionMode.None,
    selectedRef: Maybe[SignalRef[Set[String]]] = Absent,
    expandedRef: Maybe[SignalRef[Set[String]]] = Absent,
    expansionF: Maybe[A => UI] = Absent,
    groupsV: List[RowGroup[A]] = Nil,
    expandedGroupsRef: Maybe[SignalRef[Set[GroupPath]]] = Absent,
    removableSortFlag: Boolean = true,
    stripedFlag: Boolean = false,
    gridlinesFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    emptyContentV: Maybe[EmptyContent] = Absent,
    onRowClickF: Maybe[String => Any < Async] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    headerV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    scrollHeightV: Maybe[String] = Absent
) extends Node:
    type Self = DataTable[A]

    /** Appends data rows. */
    def rows(rs: Seq[A]): DataTable[A] = copy(rowsV = rowsV ++ rs.toList)

    /** Stable row identity — the selection/expansion key and the `onRowClick`
      * payload. Required for reliable selection/expansion.
      */
    def rowKey(f: A => String): DataTable[A] = copy(rowKeyF = Present(f))

    /** Appends columns. Each argument is authored against the table's row type, so
      * [[column]] needs no type argument of its own, and each may be a single [[column]]
      * or a [[headerGroup]] spanning several, which is what makes the header multi-row.
      */
    def columns(cs: HeaderOf[A]*): DataTable[A] =
        given ColumnScope[A] = new ColumnScope[A]()
        copy(cols = cols ++ cs.map(c => (c: ColumnTree[A])).toList)
    end columns

    /** The real columns, left to right, with any [[headerGroup]] flattened away. The
      * body, the footer and the filter are functions of these alone; only the header
      * reads the tree.
      */
    private lazy val leafCols: List[Column[A, FlatOnly]] = cols.flatMap(_.leaves)

    /** The same columns paired with the path that identifies them in the sort spec. */
    private lazy val leafPaths: List[(List[String], Column[A, FlatOnly])] = ColumnTree.leafPaths(cols)

    /** Binds the ordered sort spec two-way: [[SortKey]] entries, the first sorting one
      * being the primary key.
      *
      * A plain header click sorts by that column alone. A click holding Ctrl (or Cmd)
      * adds the column to the spec, or advances the one already there WITHOUT moving it:
      * ascending, descending, and (while [[removableSort]] is on) unsorted while keeping
      * its slot. That last state is what makes an accidental click cheap, since one more
      * click on the same header puts the column back at the rank it had.
      */
    def sort(ref: SignalRef[List[SortKey]]): DataTable[A] = copy(sortRef = Present(ref))

    /** Whether a header click cycle reaches `Unsorted` (default) or stops at ascending
      * and descending, which is Prime's default.
      */
    def removableSort(v: Boolean): DataTable[A] = copy(removableSortFlag = v)

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

    /** Groups the rows, one nested level per argument, outermost first. Every run of
      * CONSECUTIVE rows sharing a level's key becomes a group of that level, and the
      * levels below it split that group further.
      *
      * Grouping reads the order the table is about to render in, it does not impose one,
      * so pair it with a [[sort]] spec that leads with the same projections (or with rows
      * that already arrive ordered). Group a table by one key while it sorts by another
      * and the same key legitimately heads several runs, which is what the row order says.
      *
      * Each argument is authored against the table's row type, so [[group]] needs no type
      * argument of its own, and carries its own header row, summary row and visibility.
      */
    def groupBy(gs: GroupOf[A]*): DataTable[A] =
        given GroupScope[A] = new GroupScope[A]()
        copy(groupsV = groupsV ++ gs.map(g => (g: RowGroup[A])).toList)
    end groupBy

    /** Makes the groups collapsible and binds the set of EXPANDED paths two-way, so an
      * empty set starts with every group collapsed. Each level's header row grows a toggle
      * button; a collapsed group hides everything nested inside it, its own summary row
      * included, and keeps its slice of the page, since [[paginate]] slices rows before
      * they are grouped.
      *
      * The currency is [[GroupPath]] rather than a key, because a key alone does not
      * identify a group below the outermost level: the same brand occurs under every
      * category that sells one, and collapsing it in one place must not collapse it in the
      * others. A level with [[RowGroup.showHeader]] off has no toggle and stays open.
      */
    def expandedGroups(ref: SignalRef[Set[GroupPath]]): DataTable[A] = copy(expandedGroupsRef = Present(ref))

    /** Zebra striping (`.p-datatable-striped` + `.p-row-odd` rows). */
    def stripedRows(v: Boolean): DataTable[A] = copy(stripedFlag = v)

    /** Cell borders on every edge (`.p-datatable-gridlines`). */
    def showGridlines(v: Boolean): DataTable[A] = copy(gridlinesFlag = v)

    /** Size: `.p-datatable-sm` / default / `.p-datatable-lg` cell paddings. */
    def size(v: Size): DataTable[A] = copy(sizeV = v)

    /** Content of the full-width `tr.p-datatable-empty-message` row shown when no rows
      * survive filtering.
      */
    def emptyContent(v: String): DataTable[A] = copy(emptyContentV = Present(EmptyContent.const(v)))

    /** Reactive text: re-renders the empty slot in place on signal emission. */
    def emptyContent(sig: Signal[String]): DataTable[A] = copy(emptyContentV = Present(EmptyContent.dyn(sig)))

    /** Arbitrary UI for the empty state: an icon over a line of explanation and the
      * button that creates the first record, rendered in the same slot the text would
      * occupy.
      */
    def emptyContent(ui: UI): DataTable[A] = copy(emptyContentV = Present(EmptyContent.ui(ui)))

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

    /** Toolbar slot above the table (`div.p-datatable-header`), the place for a
      * global-filter input, a title, or action buttons.
      */
    def header(ui: UI): DataTable[A] = copy(headerV = Present(ui))

    /** Slot below the table and the paginator (`div.p-datatable-footer`). For per
      * column summary cells use [[Column.footer]], which renders a real `tfoot`.
      */
    def footer(ui: UI): DataTable[A] = copy(footerV = Present(ui))

    /** Busy state: a spinner over a dimming mask (`.p-datatable-mask`) covers the
      * table while data is being fetched.
      */
    def loading(v: Boolean): DataTable[A] = copy(loadingV = Present(BoolValue.Const(v)))

    /** Reactive busy state, bound to the data-fetch in-flight signal; the mask toggles
      * in its own sub-region without re-rendering the rows.
      */
    def loading(sig: Signal[Boolean]): DataTable[A] = copy(loadingV = Present(BoolValue.Dyn(sig)))

    /** Caps the table container at a CSS length and scrolls it, pinning the `thead`
      * (and the `tfoot`, when columns carry footers) to the container edges.
      */
    def scrollHeight(v: String): DataTable[A] = copy(scrollHeightV = Present(v))

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
        withSortableFlags { flags =>
            withRef(sortRef, List.empty[SortKey]) { sort =>
                withRef(filterRef, "") { query =>
                    withRef(pageRef, 0) { page =>
                        withRef(selectedRef, Set.empty[String]) { sel =>
                            withRef(expandedRef, Set.empty[String]) { exp =>
                                withRef(expandedGroupsRef, Set.empty[GroupPath]) { groups =>
                                    body(sort, query, page, sel, exp, groups, flags)
                                }
                            }
                        }
                    }
                }
            }
        }

    /** Resolves every reactive [[Column.sortable]] flag to a plain boolean before the table
      * builds, one nested subscription per signal-backed column.
      *
      * The flag reaches the class, the tab stop, the icon and the click handler of one
      * header cell, so patching it in place would mean keeping four channels in step. The
      * table already re-renders its whole subtree whenever a bound signal moves, sort,
      * filter, page, selection, expansion, groups, and this is the seventh of those: the
      * cost is a sort click's worth of render, and the cell code reads a plain boolean.
      */
    private def withSortableFlags(build: Map[List[String], Boolean] => UI)(using Frame): UI =
        val reactive = leafPaths.flatMap((p, c) => c.sortableSig.toList.map(sig => (p, sig)))
        def loop(rest: List[(List[String], Signal[Boolean])], acc: Map[List[String], Boolean]): UI =
            rest match
                case Nil              => build(acc)
                case (p, sig) :: tail => sig.render(b => loop(tail, acc + (p -> b)))
        loop(reactive, Map.empty)
    end withSortableFlags

    /** This column's resolved sortable flag: the reactive ones are in `flags`, the rest
      * carry theirs statically.
      */
    private def sortableFlag(c: Column[A, FlatOnly], path: List[String], flags: Map[List[String], Boolean]): Boolean =
        flags.getOrElse(path, c.sortableConst)

    private def body(
        sort: List[SortKey],
        query: String,
        page: Int,
        sel: Set[String],
        exp: Set[String],
        openGroups: Set[GroupPath],
        flags: Map[List[String], Boolean]
    )(using Frame): UI =
        // 1. Global filter: contains-match over the columns' text projections.
        val filtered =
            if query.isEmpty then rowsV
            else
                val q = query.toLowerCase
                rowsV.filter(a => leafCols.exists(c => c.textF.exists(f => f(a).toLowerCase.contains(q))))

        // 2. Sort: apply the SORTING entries back-to-front through stable sorts, so the
        //    first one ends up the primary key. Unsorted entries hold a slot in the
        //    priority order and contribute nothing here.
        val sorted = SortKey.sorting(sort).reverse.foldLeft(filtered) { (rs, k) =>
            leafPaths.find(_._1 == k.path).flatMap(_._2.orderingV.toOption) match
                case Some(ord) => rs.sorted(using if k.direction == SortDirection.Ascending then ord else ord.reverse)
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

        // The paths whose headers the reader can actually click, which is what both click
        // transitions may clear. Everything else in the spec is the caller's to keep.
        val interactive: Set[List[String]] =
            if sortRef.isEmpty then Set.empty
            else leafPaths.collect { case (p, c) if c.isSortable(sortableFlag(c, p, flags)) => p }.toSet

        val colCount = leafCols.length + (if checkboxColumn then 1 else 0) + (if expanderColumn then 1 else 0)

        // One tr per header level. The leading expander and checkbox cells belong to the
        // top row and reach down through every other one, so they line up with a column
        // whatever depth the header has.
        val headRows: List[UI] =
            val matrix = ColumnTree.spans(cols)
            val depth  = math.max(matrix.length, 1)
            val leading: List[UI] =
                val expanderTh: List[UI] =
                    if !expanderColumn then Nil
                    else
                        var cell = th.cssClass("p-datatable-header-cell")
                        if depth > 1 then cell = cell.rowspan(depth)
                        List(cell)
                val checkboxTh: List[UI] = if checkboxColumn then List(selectAllCell(sorted, sel, depth)) else Nil
                expanderTh ++ checkboxTh
            end leading
            val rows = if matrix.isEmpty then List(Nil) else matrix
            rows.zipWithIndex.map { (cells, i) =>
                val ths = cells.map(headerSpanCell(_, sort, flags, interactive))
                tr((if i == 0 then leading ++ ths else ths).map(toChild)*)
            }
        end headRows

        val bodyRows: List[UI] =
            if paged.isEmpty then
                List(
                    tr.cssClass("p-datatable-empty-message")(
                        toChild(EmptyContent.render(emptyContentV, "No records found")(c =>
                            td.colspan(math.max(colCount, 1))(c)
                        ))
                    )
                )
            else groupSegments(paged.zipWithIndex, groupsV, Nil, sel, exp, openGroups, colCount)

        // The footer aggregates over the FILTERED rows, not the visible page: a
        // column total that changed when the reader turned the page would be wrong.
        val footGroup: List[UI] =
            if !leafCols.exists(_.hasFooter) then Nil
            else
                val leadingTds: List[UI] = List.fill(colCount - leafCols.length)(td)
                val footRow: UI          = tr((leadingTds ++ leafCols.map(footerCell(_, sorted))).map(toChild)*)
                List(tfoot.cssClass("p-datatable-tfoot")(toChild(footRow)))

        var tbl = table.cssClass("p-datatable-table")
        if scrollHeightV.isDefined then tbl = tbl.cssClass("p-datatable-scrollable-table")
        accNameV match
            case Present(TextValue.Const(v)) => tbl = tbl.aria("label", v)
            case Present(TextValue.Dyn(s))   => tbl = tbl.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => tbl = tbl.aria("labelledby", v))
        val tableEl: UI = tbl(
            (List[UI](
                thead.cssClass("p-datatable-thead")(headRows.map(toChild)*),
                tbody.cssClass("p-datatable-tbody")(bodyRows.map(toChild)*)
            ) ++ footGroup).map(toChild)*
        )

        var container = div.cssClass("p-datatable-table-container")
        scrollHeightV.foreach(h => container = container.style(_.maxHeight(CssValue.length(h))))
        val containerEl: UI = container(toChild(tableEl))

        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-datatable-header")(toChild(h)))
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-datatable-footer")(toChild(f)))

        var root = div.cssClass("p-datatable").cssClass("p-component")
        // Prime: hoverable whenever a selection mode is set (checkbox included) or
        // rows react to clicks.
        if selectionModeV != SelectionMode.None || onRowClickF.isDefined then
            root = root.cssClass("p-datatable-hoverable")
        if stripedFlag then root = root.cssClass("p-datatable-striped")
        if gridlinesFlag then root = root.cssClass("p-datatable-gridlines")
        if scrollHeightV.isDefined then root = root.cssClass("p-datatable-scrollable")
        sizeV match
            case Size.Small  => root = root.cssClass("p-datatable-sm")
            case Size.Large  => root = root.cssClass("p-datatable-lg")
            case Size.Normal => ()
        end match
        root(
            (rowKeyCard ++ headerCards(
                sort,
                flags
            ) ++ loadingMask ++ headerSlot ++ (containerEl :: paginatorUI) ++ footerSlot).map(toChild)*
        )
    end body

    /** The busy mask: Prime's absolute `.p-datatable-mask` composed with the dimming
      * `.p-overlay-mask` (the mask's own `position: fixed` loses to the datatable
      * rule), holding the spinner. The DataView precedent.
      */
    private def loadingMask(using Frame): List[UI] =
        def maskDiv: UI =
            div
                .cssClass("p-datatable-mask")
                .cssClass("p-overlay-mask")(
                    toChild(ProgressSpinner().size(Size.Small).accessibleName("Loading").render)
                )
        loadingV match
            case Present(BoolValue.Const(true))  => List(maskDiv)
            case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then maskDiv else UI.empty))
            case Present(BoolValue.Const(false)) => Nil
            case Absent                          => Nil
        end match
    end loadingMask

    /** One `tfoot` cell. Columns without a footer still render an empty `td` so the
      * footer row keeps the column grid.
      */
    private def footerCell(c: Column[A, FlatOnly], inFilter: List[A])(using Frame): UI =
        var cell = td
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        val content: List[UI] = (c.footerTextV, c.footerF) match
            case (Present(t), _) => List(span.cssClass("p-datatable-column-footer")(t))
            case (_, Present(f)) => List(span.cssClass("p-datatable-column-footer")(toChild(f(inFilter))))
            case _               => Nil
        cell(content.map(toChild)*)
    end footerCell

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

    /** The loud cards for the ways a column tree and a sort spec mislead at render time
      * rather than at compile time (see [[KeyDiagnostics]]).
      *
      * A group holding no column occupies nothing, so it disappears from the header
      * instead of reporting itself. A spec entry that matches no sortable column does
      * nothing at all, which covers a mistyped part, a column that never got a `sortBy`,
      * and a path missing the group labels above it. And two sortable columns reachable
      * by the SAME path cannot be told apart by a spec at all: the table sorts by
      * whichever sits left and lights both headers up.
      */
    private def headerCards(sort: List[SortKey], flags: Map[List[String], Boolean])(using Frame): List[UI] =
        def show(path: List[String]): String = path.mkString(" / ")
        val empties                          = ColumnTree.emptyGroups(cols)
        val sortable                         = leafPaths.filter(_._2.orderingV.isDefined).map(_._1)
        val unknown                          = if sortRef.isEmpty then Nil else sort.map(_.path).filterNot(sortable.contains).map(show)
        val ambiguous =
            if sortRef.isEmpty then Nil else KeyDiagnostics.duplicates(sortable.map(show))
        // A column asked to be sortable with nothing to sort by: the flag decides whether the
        // reader may change the spec, and an ordering is what a change would act on.
        val noOrdering = leafPaths.collect {
            case (p, c) if c.orderingV.isEmpty && c.sortableV.isDefined && sortableFlag(c, p, flags) => show(p)
        }
        val emptyCard =
            if empties.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "a headerGroup holds no column, so it renders nothing; give it columns or drop it",
                    empties
                ))
        val unknownCard =
            if unknown.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "the sort spec names a column this table cannot sort; a path is the group labels around " +
                        "the column followed by its header, and the column needs a sortBy",
                    unknown
                ))
        val ambiguousCard =
            if ambiguous.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "two sortable columns share a path, so no spec can tell them apart; rename one or put " +
                        "them under different headerGroups",
                    ambiguous
                ))
        val noOrderingCard =
            if noOrdering.isEmpty then Nil
            else
                List(KeyDiagnostics.card(
                    "DataTable",
                    "sortable(true) on a column with no ordering has nothing to sort by; add sortBy, or drop " +
                        "the flag",
                    noOrdering
                ))
        emptyCard ++ unknownCard ++ ambiguousCard ++ noOrderingCard
    end headerCards

    /** The checkbox column's header cell: Prime's select-all, binary (no partial
      * state), checked while every row that survived the global filter is selected.
      *
      * It works on the FILTERED rows across every page, not the visible slice, which
      * is the only reading of "all" that stays stable while the reader pages through.
      * Rows the filter removed keep whatever selection they had, so the write is an
      * add-or-remove of the visible keys rather than a replacement of the whole set:
      * narrowing the filter, select-alling, then widening it again must not silently
      * drop what was selected before.
      */
    private def selectAllCell(inFilter: List[A], sel: Set[String], rows: Int)(using Frame): UI =
        val keys        = inFilter.map(keyOf)
        val allSelected = keys.nonEmpty && keys.forall(sel.contains)
        val toggle: Any < Async = selectedRef match
            case Present(ref) => ref.getAndUpdate(cur => if allSelected then cur -- keys else cur ++ keys)
            case Absent       => ()
        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        cell(
            toChild(
                CheckBox()
                    .checked(allSelected)
                    .accessibleName("Select All")
                    .onChange(_ => toggle)
                    .render
            )
        )
    end selectAllCell

    /** One cell of the header matrix: a leaf renders its column header, a group a plain
      * title cell as wide as the leaves beneath it.
      */
    private def headerSpanCell(
        sp: ColumnTree.HeaderSpan[A],
        sort: List[SortKey],
        flags: Map[List[String], Boolean],
        interactive: Set[List[String]]
    )(using Frame): UI =
        sp.node.asColumn match
            case Present(c) =>
                headerCell(c, sp.path, sort, sp.rowspan, sortableFlag(c, sp.path, flags), interactive)
            case Absent =>
                var cell = th.cssClass("p-datatable-header-cell")
                if sp.colspan > 1 then cell = cell.colspan(sp.colspan)
                cell(
                    div.cssClass("p-datatable-column-header-content")(
                        toChild(span.cssClass("p-datatable-column-title")(sp.node.label))
                    )
                )
        end match
    end headerSpanCell

    /** One sortable/plain header cell with Prime's header-content anatomy, reaching down
      * `rows` header rows so an ungrouped column lines up with a grouped one.
      */
    private def headerCell(
        c: Column[A, FlatOnly],
        path: List[String],
        sort: List[SortKey],
        rows: Int,
        flag: Boolean,
        interactive: Set[List[String]]
    )(using Frame): UI =
        val sortable  = c.isSortable(flag) && sortRef.isDefined
        val sortingKs = SortKey.sorting(sort)
        val rank      = sortingKs.indexWhere(_.path == path)
        val direction = sort.find(_.path == path).map(_.direction).getOrElse(SortDirection.Unsorted)

        var cell = th.cssClass("p-datatable-header-cell")
        if rows > 1 then cell = cell.rowspan(rows)
        c.alignV match
            case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
            case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
            case ColumnAlign.Start  => ()
        end match
        if sortable then
            cell = cell.cssClass("p-datatable-sortable-column").tabIndex(0).onClick(e => toggleSort(path, e, interactive))
        if direction.isSorting then
            cell = cell
                .cssClass("p-datatable-column-sorted")
                .aria("sort", if direction == SortDirection.Ascending then "ascending" else "descending")
        end if

        // The neutral icon is the affordance and the directional one is state, so a column
        // the reader may not re-sort still says which way it currently sorts.
        val sortIcon: List[UI] =
            if !sortable && !direction.isSorting then Nil
            else
                val glyph = direction match
                    case SortDirection.Ascending  => Icons.sortAmountUpAlt
                    case SortDirection.Descending => Icons.sortAmountDown
                    case SortDirection.Unsorted   => Icons.sortAlt
                List(GlyphSvg(glyph, "p-datatable-sort-icon"))

        // Sorting by one column needs no ordinal; sorting by several does, because the
        // spec is ordered and the icons alone cannot say which key is primary.
        val sortBadge: List[UI] =
            if rank < 0 || sortingKs.length < 2 then Nil
            else
                List(
                    Badge((rank + 1).toString)
                        .size(Size.Small)
                        .hostClass("p-datatable-sort-badge")
                        .render
                )

        cell(
            div.cssClass("p-datatable-column-header-content")(
                ((span.cssClass("p-datatable-column-title")(c.headerV): UI) :: (sortIcon ++ sortBadge)).map(toChild)*
            )
        )
    end headerCell

    /** Header click. Plain: sort by this column alone, advancing it when it is already
      * the only key so a plain click still cycles. Ctrl or Cmd: add the column, or
      * advance the one already in the spec IN PLACE, which is what lets an accidental
      * click be undone by the next one.
      */
    private def toggleSort(path: List[String], e: MouseEvent, interactive: Set[List[String]])(using
        Frame
    ): Any < Async =
        sortRef match
            case Present(ref) =>
                val multi = e.modifiers.ctrl || e.modifiers.meta
                ref.getAndUpdate(cur =>
                    if multi then SortKey.cycle(cur, path, removableSortFlag, interactive.contains)
                    else SortKey.plain(cur, path, removableSortFlag, interactive.contains)
                )
            case Absent => ()

    private def isExpanded(a: A, exp: Set[String]): Boolean = expansionF.isDefined && exp.contains(keyOf(a))

    /** How many table rows one data row contributes: itself, plus its expansion row while
      * it is open. What a merged cell has to span is rows, not records.
      */
    private def trCount(a: A, exp: Set[String]): Int = if isExpanded(a, exp) then 2 else 1

    /** Renders one slice of rows under the remaining grouping levels: each level splits the
      * slice into runs, wraps every run in its header and summary rows, and hands the run
      * down. With no levels left the rows render directly.
      *
      * `path` is the chain of keys taken so far, which is what identifies a group at any
      * depth and therefore what [[expandedGroups]] holds.
      */
    private def groupSegments(
        rows: List[(A, Int)],
        levels: List[RowGroup[A]],
        path: List[String],
        sel: Set[String],
        exp: Set[String],
        openGroups: Set[GroupPath],
        colCount: Int
    )(using Frame): List[UI] =
        levels match
            case Nil => leafRows(rows, sel, exp, colCount)
            case level :: rest =>
                RowGroup.runs(rows)((a, _) => level.keyF(a)).flatMap { (key, run) =>
                    val groupPath = GroupPath(path :+ key)
                    val groupRows = run.map(_._1)
                    // A level without a header row has nowhere to put a toggle, so it can
                    // never be collapsed: collapsing it would hide its rows with no way
                    // back.
                    val collapsible = expandedGroupsRef.isDefined && level.showHeaderFlag
                    val open        = !collapsible || openGroups.contains(groupPath)

                    val headerRow: List[UI] =
                        if !level.showHeaderFlag then Nil
                        else List(groupHeaderRow(level, groupPath, groupRows, colCount, collapsible, open))
                    val innerRows: List[UI] =
                        if !open then Nil
                        else groupSegments(run, rest, groupPath.keys, sel, exp, openGroups, colCount)
                    val footerRow: List[UI] =
                        if !open then Nil
                        else
                            level.footerF.toList.map(f =>
                                tr.cssClass("p-datatable-row-group-footer")(
                                    toChild(td.colspan(math.max(colCount, 1))(toChild(f(groupPath, groupRows))))
                                )
                            )
                    headerRow ++ innerRows ++ footerRow
                }
    end groupSegments

    /** The innermost slice: the data rows themselves, carrying whatever merged cells the
      * [[Column.rowSpan]] columns resolve to over exactly this slice.
      */
    private def leafRows(rows: List[(A, Int)], sel: Set[String], exp: Set[String], colCount: Int)(using
        Frame
    ): List[UI] =
        val spans = spanCells(rows.map(_._1), exp)
        rows.zip(spans).flatMap((row, cells) => dataRow(row._1, row._2, sel, exp, colCount, cells))
    end leafRows

    /** Resolves the merged cells of one slice: for each row, which of the marked columns it
      * heads a span of, and which it is covered by.
      *
      * The columns are taken left to right, and each one splits the blocks the previous one
      * produced rather than the whole slice. That nesting is what keeps two spans from
      * crossing, which the browser would render as a broken grid, and it is why the marked
      * columns read outer to inner in column order.
      */
    private def spanCells(rows: List[A], exp: Set[String]): List[Map[Int, SpanCell]] =
        val merged = leafCols.zipWithIndex.collect { case (c, i) if c.rowSpanEq.isDefined => (i, c.rowSpanEq.get) }
        if merged.isEmpty then List.fill(rows.size)(Map.empty)
        else
            val indexed                                                = rows.toVector
            val start: (Map[Int, Map[Int, SpanCell]], List[List[Int]]) = (Map.empty, List(rows.indices.toList))
            val (assigned, _) = merged.foldLeft(start) { case ((acc, blocks), (col, same)) =>
                val split = blocks.flatMap(block => RowGroup.blocks(block)((x, y) => same(indexed(x), indexed(y))))
                val next = split.foldLeft(acc) { (byRow, run) =>
                    val spanned = run.map(pos => trCount(indexed(pos), exp)).sum
                    run.zipWithIndex.foldLeft(byRow) { case (m, (pos, offset)) =>
                        val cell = if offset == 0 then SpanCell.Head(spanned) else SpanCell.Covered
                        m.updated(pos, m.getOrElse(pos, Map.empty) + (col -> cell))
                    }
                }
                (next, split)
            }
            rows.indices.toList.map(pos => assigned.getOrElse(pos, Map.empty))
        end if
    end spanCells

    /** One level's header row: Prime's `tr.p-datatable-row-group-header` over one
      * full-width cell, the collapse toggle ahead of the content while the level is
      * collapsible. Without a [[RowGroup.header]] template the cell shows the key alone.
      */
    private def groupHeaderRow(
        level: RowGroup[A],
        path: GroupPath,
        rows: List[A],
        colCount: Int,
        collapsible: Boolean,
        open: Boolean
    )(using Frame): UI =
        val toggle: List[UI] =
            if !collapsible then Nil
            else
                val glyph = if open then Icons.chevronDown else Icons.chevronRight
                List(
                    button
                        .cssClass("p-datatable-row-toggle-button")
                        .jsProp("type", "button")
                        .aria("expanded", open.toString)
                        .aria("label", if open then "Row Group Collapse" else "Row Group Expand")
                        .onClick(toggleGroup(path))(toChild(GlyphSvg(glyph, "p-datatable-row-toggle-icon")))
                )
        val content: UI = level.headerF match
            case Present(f) => f(path, rows)
            case Absent     => stringToUI(path.key)
        tr.cssClass("p-datatable-row-group-header")(
            toChild(td.colspan(math.max(colCount, 1))((toggle :+ content).map(toChild)*))
        )
    end groupHeaderRow

    private def toggleGroup(path: GroupPath)(using Frame): Any < Async =
        expandedGroupsRef match
            case Present(ref) => ref.getAndUpdate(cur => if cur.contains(path) then cur - path else cur + path)
            case Absent       => ()

    /** One data row (plus its expansion row while expanded). */
    private def dataRow(
        a: A,
        index: Int,
        sel: Set[String],
        exp: Set[String],
        colCount: Int,
        spans: Map[Int, SpanCell]
    )(using Frame): List[UI] =
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

        val dataTds: List[UI] = leafCols.zipWithIndex.flatMap { (c, i) =>
            val cellSpan = spans.get(i)
            if cellSpan.contains(SpanCell.Covered) then Nil
            else
                var cell = td
                cellSpan match
                    // A run of one row spans nothing; `rowspan="1"` is the default and
                    // stating it would only make the markup say something it already says.
                    case Some(SpanCell.Head(n)) if n > 1 => cell = cell.rowspan(n)
                    case _                               => ()
                end match
                c.alignV match
                    case ColumnAlign.Center => cell = cell.cssClass("p-uic-dt-center")
                    case ColumnAlign.End    => cell = cell.cssClass("p-uic-dt-end")
                    case ColumnAlign.Start  => ()
                end match
                val content: HtmlChildVal = c.bodyF match
                    case Present(f) => toChild(f(a))
                    case Absent     => toChild(stringToUI(c.textF.map(_(a)).getOrElse("")))
                List(cell(content))
            end if
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
