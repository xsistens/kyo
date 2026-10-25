package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.uic.form.FieldError

/** Which regions a DataTable draws its MOUNTS under.
  *
  * A mount starts again whenever a region above it repaints, and while it starts kyo-ui shows an inert snapshot that no
  * click or key reaches. So a mount the table holds around its table (the paginator's dropdowns, whatever the page put in
  * the header slot) sits under no region of the table at all: none of them shows the sort, the page turn, the search,
  * the filters, the widths or an edit. Every table here binds all of those, so a region reading any of them above a
  * mount shows up in the count. The behaviour that follows checks that what the reader sees did not move with it.
  */
class DataTableRegionsTest extends UicTest:

    final case class Item(id: String, name: String, price: Int) derives CanEqual

    private val five = (1 to 5).toList.map(i => Item(i.toString, ('A' + i - 1).toChar.toString, i * 10))

    /** Every piece of state the table reads, bound, so a region over any of it is a region the count sees. */
    final private case class Bound(
        rows: SignalRef[Seq[Item]],
        sort: SignalRef[List[SortKey]],
        query: SignalRef[String],
        filters: SignalRef[Map[List[String], ColumnFilter]],
        widths: SignalRef[Map[List[String], Double]],
        page: SignalRef[Int],
        editing: SignalRef[Maybe[CellPath[Item]]],
        err: SignalRef[Maybe[(CellKey, FieldError)]],
        draft: SignalRef[String],
        menus: Map[List[String], SignalRef[Boolean]]
    )

    private def bound(editing: Maybe[CellPath[Item]] = Absent)(using Frame): Bound < Sync =
        for
            rows    <- Signal.initRef[Seq[Item]](five)
            sort    <- Signal.initRef(List.empty[SortKey])
            query   <- Signal.initRef("")
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            widths  <- Signal.initRef(Map.empty[List[String], Double])
            page    <- Signal.initRef(0)
            edit    <- Signal.initRef(editing)
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            draft   <- Signal.initRef("")
            menus   <- Kyo.foreach(List(List("Name"), List("Price")))(p => Signal.initRef(false).map(p -> _))
        yield Bound(rows, sort, query, filters, widths, page, edit, err, draft, menus.toMap)

    /** The table over `b`, paged unless `paged` is off: a test that measures a Select of its own turns the paginator,
      * whose dropdowns are Selects too, off.
      */
    private def table(b: Bound, paged: Boolean = true)(using Frame): DataTable[Item] =
        val t = uic.DataTable[Item]().rows(b.rows).rowKey(_.id).columns(
            uic.column("Name")(_.name).sortBy(_.name).filterBy
                .editableAs(CellType.of(five.map(_.name))(identity))(_.name)((i, v) => i.copy(name = v)),
            uic.column("Price")(_.price.toString).sortBy(_.price).filterBy(_.price)
        ).sort(b.sort).globalFilter(b.query).columnFilters(b.filters).columnWidths(b.widths).editingCell(b.editing)
        if !paged then t
        else
            t.paginate(2)(b.page).paginator(_.rowsPerPageOptions(Seq(2, 4)).template(
                PaginatorElement.PageLinks,
                PaginatorElement.RowsPerPageDropdown,
                PaginatorElement.JumpToPageDropdown
            ))
        end if
    end table

    private def wired(t: DataTable[Item], b: Bound)(using Frame): UI =
        t.wired("t", Map(List("Name") -> b.draft), b.err, _ => (), b.menus)

    "the paginator's dropdowns sit under no region of the table" in {
        for
            b <- bound()
            ui = wired(table(b), b)
            rpp <- regionsAboveMount(ui, "p-paginator-rpp-dropdown")
            jtp <- regionsAboveMount(ui, "p-paginator-jtp-dropdown")
        yield
            assert(rpp == 0, s"the rows-per-page dropdown sits under $rpp region(s)")
            // The jump list's options ARE the pages, which it reads as a signal of options: built once all the same.
            assert(jtp == 0, s"the jump dropdown sits under $jtp region(s)")
        end for
    }

    "what the page puts in the header slot sits under no region of the table" in {
        for
            b <- bound()
            ui = wired(table(b).header(uic.MultiSelect[String]().options(Seq("a", "b")): UI), b)
            header <- regionsAboveMount(ui, "p-multiselect")
        yield assert(header == 0, s"the header slot's control sits under $header region(s)")
    }

    "an open filter menu of the filter row is drawn in one region over its open state and its column's filter" in {
        // The host reads both, so the panel is drawn directly (host-gated, under the column's panel id) rather than in a
        // mount of its own that a repaint of the host would start again.
        for
            b <- bound()
            _ <- b.menus(List("Name")).set(true)
            ui = wired(table(b), b)
            menu <- regionsAbove(ui, "p-datatable-filter-overlay")
        yield assert(menu == 1, s"the open mode list sits under $menu region(s); one reads its open state and its column's filter")
    }

    "an open filter menu of the menu display sits under its own open state and draft alone" in {
        for
            b     <- bound()
            draft <- Signal.initRef(ColumnFilter(List(FilterRule("", MatchMode.Contains))))
            _     <- b.menus(List("Name")).set(true)
            ui = table(b, paged = false).filterDisplay(FilterDisplay.Menu).wired(
                "t",
                Map(List("Name") -> b.draft),
                b.err,
                _ => (),
                b.menus,
                filterDrafts = Map(List("Name") -> draft)
            )
            rule <- regionsAboveMount(ui, "p-select")
        yield assert(rule == 1, s"a rule's mode Select sits under $rule region(s); one is the menu's own open state and draft")
    }

    "an open Select editor sits under its own row alone" in {
        for
            b <- bound(Present(CellPath(five(1), List("Name"))))
            ui = wired(table(b, paged = false), b)
            editor <- regionsAboveMount(ui, "p-select")
        yield assert(editor == 1, s"the editor sits under $editor region(s); one is the keyed row list it is a row of")
    }

    "the rows-per-page dropdown is live and decides how many rows a page holds" in {
        // With the size a constant the paginator had nothing to write its pick into and drew the dropdown disabled; a
        // size bound to a ref is what the dropdown writes and what the table pages by.
        for
            b    <- bound()
            size <- Signal.initRef(2)
            ui = uic.DataTable[Item]().rows(b.rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(size)(b.page)
                .paginator(_.rowsPerPageOptions(Seq(2, 4)).template(PaginatorElement.RowsPerPageDropdown))
                .wired("t", Map.empty, b.err, _ => ())
            rpp    <- elementWithClass(ui, "p-paginator-rpp-dropdown")
            before <- bodyNames(ui)
            _      <- size.set(4)
            after  <- bodyNames(ui)
        yield
            assert(!rpp.attrs.cssClasses.contains("p-disabled"), "the dropdown takes a pick")
            assert(before == List("A", "B") && after == List("A", "B", "C", "D"), s"two rows, then four: $before / $after")
        end for
    }

    "an open Select editor in a windowed body sits under its own row alone" in {
        for
            b      <- bound(Present(CellPath(five(1), List("Name"))))
            scroll <- Signal.initRef(0.0)
            ui = table(b, paged = false).scrollHeight("200px").scrollRows(40, overscan = 0)
                .wired("t", Map(List("Name") -> b.draft), b.err, _ => (), b.menus, scroll = Present(scroll))
            editor <- regionsAboveMount(ui, "p-select")
        yield assert(editor == 1, s"the editor sits under $editor region(s); one is the keyed window it is a row of")
    }

    "an open Select editor in a frozen row sits under its own row alone" in {
        for
            b      <- bound(Present(CellPath(Item("9", "Z", 90), List("Name"))))
            pinned <- Signal.initRef[Seq[Item]](Seq(Item("9", "Z", 90)))
            top    <- Signal.initRef(Absent: Maybe[Int])
            ui = table(b, paged = false).frozenRows(pinned)
                .wired("t", Map(List("Name") -> b.draft), b.err, _ => (), b.menus, headTop = Present(top))
            editors <- regionsAboveMount(ui, "p-select")
        yield assert(editors == 1, s"the editor sits under $editors region(s); one is the keyed list of the frozen rows")
    }

    "a bound column layout is the one region a mount sits under beside its own" in {
        // The column order and visibility ARE the layout: a mount in a column that moves belongs to the new layout, so the
        // table draws its container again when they change. That is the contract, and this pins it.
        for
            b     <- bound(Present(CellPath(five(1), List("Name"))))
            order <- Signal.initRef(List(List("Price"), List("Name")))
            ui = table(b, paged = false).columnOrder(order).wired("t", Map(List("Name") -> b.draft), b.err, _ => (), b.menus)
            editor <- regionsAboveMount(ui, "p-select")
        yield assert(editor == 2, s"the editor sits under $editor region(s): the layout's and its row list's")
    }

    // ---- behaviour that must not move while the regions do ----------------------------------------------------------

    /** One node's own elements, with the reactive, fragment and list layers resolved away. */
    private def resolve(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        node match
            case e: UI.Ast.Element       => Chunk(e)
            case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(resolve)
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(resolve).map(_.flatten)
            case f: UI.Ast.Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map { items =>
                            Kyo.foreach(items.zipWithIndex) { (item, i) => resolve(render(i, item)) }.map(_.flatten)
                        }
                )
            case k: UI.Ast.KeyedChild[?] => resolve(k.child)
            case _                       => Chunk.empty

    /** The first cell's text of every body row, which is the Name column here. */
    private def bodyNames(ui: UI)(using Frame): List[String] < Sync =
        elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-datatable-tbody")))
            .flatMap(bs => Kyo.foreach(bs)(b => Kyo.foreach(b.children)(resolve).map(_.flatten)).map(_.flatten))
            .map(_.flatMap(_.children.collect { case c: UI.Ast.Element => c }.headOption))
            .map(_.flatMap(_.children.collect { case t: UI.Ast.Text => t.value }).toList)

    private def html(ui: UI)(using Frame): String < Async = UI.runRender(ui).take(1).run.map(_.mkString)

    "an empty table says so and still offers its paginator" in {
        for
            b <- bound()
            _ <- b.rows.set(Seq.empty)
            ui = wired(table(b), b)
            out <- html(ui)
        yield
            assert(out.contains("No records found"), "the empty message")
            assert(out.contains("p-paginator"), "the paginator stays")
        end for
    }

    "a filter that leaves fewer rows than the page shown shows the last page there is" in {
        for
            b <- bound()
            _ <- b.page.set(2)
            ui = wired(table(b), b)
            before <- bodyNames(ui)
            _      <- b.query.set("A")
            after  <- bodyNames(ui)
        yield
            assert(before == List("E"), s"the third page holds E: $before")
            assert(after == List("A"), s"one row is left, on the only page there is: $after")
        end for
    }

    "a sort while an edit is open keeps the edit on its row" in {
        for
            b <- bound(Present(CellPath(five(1), List("Name"))))
            _ <- b.draft.set("C")
            ui = wired(table(b), b)
            _       <- b.sort.set(List(SortKey(List("Name"), SortDirection.Descending)))
            editing <- b.editing.get
            draft   <- b.draft.get
            out     <- html(ui)
        yield
            assert(editing == Present(CellPath(five(1), List("Name"))), s"the same cell is open: $editing")
            assert(draft == "C", s"with what was typed: $draft")
            assert(out.contains("p-select"), "and its editor is drawn")
        end for
    }

    "a filter while an edit is open keeps the edit while its row is shown" in {
        for
            b <- bound(Present(CellPath(five(1), List("Name"))))
            ui = wired(table(b), b)
            _       <- b.query.set("B")
            editing <- b.editing.get
            names   <- bodyNames(ui)
            out     <- html(ui)
        yield
            assert(editing == Present(CellPath(five(1), List("Name"))), s"the same cell is open: $editing")
            assert(out.contains("p-select"), s"and its editor is drawn over the one row left: $names")
        end for
    }

    "a width change leaves a filter's text where the reader typed it" in {
        for
            b <- bound()
            _ <- b.filters.set(Map(List("Name") -> ColumnFilter("B", MatchMode.Contains)))
            ui = wired(table(b), b)
            _     <- b.widths.set(Map(List("Name") -> 180.0))
            ins   <- elements(ui).map(_.collect { case i: UI.Ast.TextInput => i })
            names <- bodyNames(ui)
        yield
            assert(ins.exists(_.value.exists { case UI.Bound.Const(v) => v == "B"; case _ => false }), "the filter input still holds B")
            assert(names == List("B"), s"and still filters by it: $names")
        end for
    }

end DataTableRegionsTest
