package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.uic.form.FieldError

/** DataTable's BEHAVIOUR: what its handlers do when a key reaches them. GoldenRenderTest
  * pins the markup, `GridNavTest` the key map, and this file the wiring between the two,
  * which is where the two of them can agree and the table still do the wrong thing.
  *
  * The handlers are pulled straight off the rendered tree, so what runs here is what the
  * client would run. Nothing is simulated except the keystroke itself.
  */
class DataTableTest extends UicTest:

    final case class Item(id: String, name: String, price: Int) derives CanEqual

    private val items = List(Item("1", "A", 10), Item("2", "B", 20))

    /** Every element of a rendered tree, with the reactive nodes resolved to their current
      * content. Reactive is a subscription boundary, not a node the client sees, so a walk
      * that stopped there would miss everything a table renders inside its own refs.
      */
    private def elements(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        node match
            case e: UI.Ast.Element =>
                Kyo.foreach(e.children)(elements).map(cs => Chunk(e) ++ cs.flatten)
            case r: UI.Ast.Reactive[?] =>
                r.signal.current(using r.frame).map(elements)
            case f: UI.Ast.Fragment[?] =>
                Kyo.foreach(f.children)(elements).map(_.flatten)
            case k: UI.Ast.KeyedChild[?] => elements(k.child)
            // A mount is shown as its placeholder, which is what the golden renderer does
            // and what an overlay's own reposition mount wraps its panel in.
            case m: UI.Ast.Mounted =>
                m.placeholderUI match
                    case Present(ui) => elements(ui)
                    case Absent      => Chunk.empty
            case _ => Chunk.empty

    private def cellWithId(node: UI, id: String)(using Frame): UI.Ast.Element < Sync =
        elements(node).map(_.find(_.attrs.identifier.contains(id)).getOrElse(
            throw new AssertionError(s"no element with id $id")
        ))

    private def press(cell: UI.Ast.Element, key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none)(
        using Frame
    ): Any < Async =
        cell.attrs.onKeyDown match
            case Present(f) => f(UI.KeyboardEvent(key, mods, cell.attrs.identifier))
            case Absent     => throw new AssertionError("the cell declares no key handler")

    private def elementWithClass(node: UI, cls: String)(using Frame): UI.Ast.Element < Sync =
        elements(node).map(_.find(_.attrs.cssClasses.contains(cls)).getOrElse(
            throw new AssertionError(s"no element with class $cls")
        ))

    /** The click handler is an EFFECT stored in a `Maybe`, so it is taken out by hand: a
      * match over it infers `Any` and lands inert, which is the trap the module documents.
      */
    private def click(el: UI.Ast.Element)(using Frame): Any < Async =
        if el.attrs.onClick.isEmpty then throw new AssertionError("the element declares no click handler")
        else el.attrs.onClick.get

    /** A table whose two editable columns are Name and Price, wired to refs the test owns
      * so it can read back what a keystroke did. `focus` records instead of commanding.
      */
    private def wire(using Frame) =
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath])
            name    <- Signal.initRef("")
            price   <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            focused <- Signal.initRef(List.empty[String])
        yield
            val table = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                uic.column("Price")(_.price.toString)
                    .editableAs(uic.CellType.int.validate(kyo.uic.form.Validator.min(1)))(_.price)((i, v) => i.copy(price = v))
            ).editingCell(editing)
            val ui = table.wired(
                "t",
                Map(List("Name") -> name, List("Price") -> price),
                err,
                id => focused.getAndUpdate(_ :+ id)
            )
            (ui, rows, editing, name, price, err, focused)

    "Enter on a resting cell opens it, seeded from the row" in {
        for
            (ui, _, editing, name, _, _, _) <- wire
            cell                            <- cellWithId(ui, "t-c0-0")
            _                               <- press(cell, UI.Keyboard.Enter)
            open                            <- editing.get
            draft                           <- name.get
        yield
            assert(open == Present(CellPath("1", List("Name"))))
            assert(draft == "A", "the draft opens on what the cell showed")
    }

    "a printable key opens the cell ON that character" in {
        for
            (ui, _, editing, name, _, _, _) <- wire
            cell                            <- cellWithId(ui, "t-c1-0")
            _                               <- press(cell, UI.Keyboard.Char('z'))
            open                            <- editing.get
            draft                           <- name.get
        yield
            assert(open == Present(CellPath("2", List("Name"))))
            assert(draft == "z", "and not on the row's value, which typing replaces")
    }

    // The bug this file was written for: the editor IS the focused element, so a commit
    // that removes it without handing focus back leaves the cursor nowhere and the next
    // arrow key does nothing.
    "Enter commits and hands focus back to the cell" in {
        for
            (ui, rows, editing, name, _, _, focused) <- wire
            cell                                     <- cellWithId(ui, "t-c0-0")
            _                                        <- press(cell, UI.Keyboard.Enter)
            _                                        <- name.set("Ada")
            reopened                                 <- cellWithId(ui, "t-c0-0")
            _                                        <- press(reopened, UI.Keyboard.Enter)
            open                                     <- editing.get
            stored                                   <- rows.get
            seen                                     <- focused.get
        yield
            assert(open == Absent, "the cell closed")
            assert(stored.head.name == "Ada", "the write went through the column")
            assert(seen == List("t-c0-0"), "and focus went back to the cell the editor was in")
    }

    "Escape discards and hands focus back too" in {
        for
            (ui, rows, editing, name, _, _, focused) <- wire
            cell                                     <- cellWithId(ui, "t-c0-0")
            _                                        <- press(cell, UI.Keyboard.Enter)
            _                                        <- name.set("Ada")
            reopened                                 <- cellWithId(ui, "t-c0-0")
            _                                        <- press(reopened, UI.Keyboard.Escape)
            open                                     <- editing.get
            stored                                   <- rows.get
            seen                                     <- focused.get
        yield
            assert(open == Absent)
            assert(stored.head.name == "A", "nothing was written")
            assert(seen == List("t-c0-0"))
    }

    // A cell that refuses still has the reader in it, so moving focus would take them out
    // of the value they are fixing.
    "a refused commit keeps the cell open and does NOT move focus" in {
        for
            (ui, rows, editing, _, price, err, focused) <- wire
            cell                                        <- cellWithId(ui, "t-c0-1")
            _                                           <- press(cell, UI.Keyboard.Enter)
            _                                           <- price.set("0")
            reopened                                    <- cellWithId(ui, "t-c0-1")
            _                                           <- press(reopened, UI.Keyboard.Enter)
            open                                        <- editing.get
            standing                                    <- err.get
            stored                                      <- rows.get
            seen                                        <- focused.get
        yield
            assert(open == Present(CellPath("1", List("Price"))), "the cell stayed open")
            assert(standing.exists((c, e) => c == CellPath("1", List("Price")) && e.code == "min"))
            assert(stored.head.price == 10, "and nothing was written")
            assert(seen.isEmpty, "focus stayed in the editor")
    }

    "an arrow moves the cursor by asking the client to focus the next cell" in {
        for
            (ui, _, editing, _, _, _, focused) <- wire
            cell                               <- cellWithId(ui, "t-c0-0")
            _                                  <- press(cell, UI.Keyboard.ArrowDown)
            _                                  <- press(cell, UI.Keyboard.ArrowRight)
            open                               <- editing.get
            seen                               <- focused.get
        yield
            assert(seen == List("t-c1-0", "t-c0-1"), "one focus command per key, no re-render")
            assert(open == Absent, "and moving opens nothing")
    }

    "Tab commits and carries the edit into the next editable cell, leaving focus to the browser" in {
        for
            (ui, rows, editing, name, price, _, focused) <- wire
            cell                                         <- cellWithId(ui, "t-c0-0")
            _                                            <- press(cell, UI.Keyboard.Enter)
            _                                            <- name.set("Ada")
            reopened                                     <- cellWithId(ui, "t-c0-0")
            _                                            <- press(reopened, UI.Keyboard.Tab)
            open                                         <- editing.get
            stored                                       <- rows.get
            draft                                        <- price.get
            seen                                         <- focused.get
        yield
            assert(stored.head.name == "Ada", "what Tab left is committed")
            assert(open == Present(CellPath("1", List("Price"))), "and what it lands on is open")
            assert(draft == "10", "seeded from that cell's own row")
            assert(seen.isEmpty, "no focus command: the browser's tab order already moved it")
    }

    /** The text inputs of the rendered tree, in render order. The handler is on the input
      * node itself and not in `attrs`, since only a text input has one.
      */
    private def inputs(node: UI)(using Frame): Chunk[UI.Ast.TextInput] < Sync =
        elements(node).map(_.collect { case i: UI.Ast.TextInput => i })

    private def type_(i: UI.Ast.TextInput, text: String)(using Frame): Any < Async =
        i.onInput match
            case Present(f) => f(text)
            case Absent     => throw new AssertionError("the input declares no handler")

    /** A table filtered by two columns, one text and one that compares. */
    private def filtered(using Frame) =
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            menus   <- Kyo.foreach(List(List("Name"), List("Price")))(p => Signal.initRef(false).map(p -> _))
        yield
            val table = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).filterBy,
                uic.column("Price")(_.price.toString).filterBy(_.price)
            ).columnFilters(filters)
            val ui = table.wired("t", Map.empty, err, _ => (), menus.toMap)
            (ui, filters, menus.toMap)

    /** The row names the table currently renders, read off its body cells. */
    private def bodyNames(node: UI)(using Frame): Chunk[String] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-tbody")).flatMap(_.children.collect {
            case r: UI.Ast.Element => r
        })).map(_.flatMap(_.children.collect { case c: UI.Ast.Element => c }.headOption))
            .map(_.flatMap(_.children.collect { case t: UI.Ast.Text => t.value }))

    "typing into a filter cell writes the column's own filter and narrows the rows" in {
        for
            (ui, filters, _) <- filtered
            fields           <- inputs(ui)
            _                <- type_(fields.head, "A")
            spec             <- filters.get
            names            <- bodyNames(ui)
        yield
            assert(spec == Map(List("Name") -> ColumnFilter("A", MatchMode.Contains)), "the text column starts on contains")
            assert(names == Chunk("A"), "and the table narrows to what matches")
    }

    // The comparison is over the VALUE: read as text, 20 does not come before 15.
    "a column that compares reads the query as a value" in {
        for
            (ui, filters, _) <- filtered
            fields           <- inputs(ui)
            _                <- type_(fields(1), "20")
            spec             <- filters.get
            equal            <- bodyNames(ui)
            _                <- filters.set(Map(List("Price") -> ColumnFilter("15", MatchMode.Less)))
            below            <- bodyNames(ui)
        yield
            assert(fields.size == 2, "one input per filterable column")
            assert(spec == Map(List("Price") -> ColumnFilter("20", MatchMode.Equals)), "a column that compares starts on equals")
            assert(equal == Chunk("B"))
            assert(below == Chunk("A"), "and a seeded mode is read the same way")
    }

    // A filter the table cannot apply is not a filter: the rows stay and the input says so.
    "a query the column cannot read keeps every row and marks its own input" in {
        for
            (ui, filters, _) <- filtered
            fields           <- inputs(ui)
            _                <- type_(fields(1), "nope")
            names            <- bodyNames(ui)
            after            <- inputs(ui)
            spec             <- filters.get
        yield
            assert(names == Chunk("A", "B"), "the table is not emptied behind a typo")
            assert(after(1).attrs.cssClasses.contains("p-invalid"), "and the input carries the refusal")
            assert(spec(List("Price")).query == "nope", "what was typed is still what is there")
    }

    "picking a mode rewrites that column's filter and closes the menu" in {
        for
            (ui, filters, menus) <- filtered
            fields               <- inputs(ui)
            _                    <- type_(fields.head, "A")
            _                    <- menus(List("Name")).set(true)
            open                 <- elements(ui)
            starts = open.find(e =>
                e.attrs.cssClasses.contains("p-datatable-filter-constraint") &&
                    e.children.collect { case t: UI.Ast.Text => t.value }.contains("Starts with")
            ).getOrElse(throw new AssertionError("the menu shows no modes"))
            _     <- click(starts)
            spec  <- filters.get
            still <- menus(List("Name")).get
        yield
            assert(spec == Map(List("Name") -> ColumnFilter("A", MatchMode.StartsWith)), "the query survives the mode")
            assert(!still, "and picking one closes the menu")
    }

    private def hasCell(node: UI, id: String)(using Frame): Boolean < Sync =
        elements(node).map(_.exists(_.attrs.identifier.contains(id)))

    // A column that is not on the screen is not in the grid either: the cursor addresses
    // cells by position, so the position has to mean what the reader sees.
    "hiding a column renumbers the grid the keyboard moves over" in {
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath])
            name    <- Signal.initRef("")
            price   <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            shown   <- Signal.initRef(true)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                uic.column("Note")(_ => "note").visible(shown),
                uic.column("Price")(_.price.toString).editable(_.price)((i, v) => i.copy(price = v))
            ).editingCell(editing).wired("t", Map(List("Name") -> name, List("Price") -> price), err, _ => ())
            wide      <- cellWithId(ui, "t-c0-1")
            _         <- press(wide, UI.Keyboard.Enter)
            overNote  <- editing.get
            third     <- hasCell(ui, "t-c0-2")
            _         <- shown.set(false)
            narrow    <- cellWithId(ui, "t-c0-1")
            _         <- press(narrow, UI.Keyboard.Enter)
            overPrice <- editing.get
            gone      <- hasCell(ui, "t-c0-2")
        yield
            assert(third, "three columns are three cells")
            assert(overNote == Absent, "and the second one carries no pipeline, so Enter does nothing")
            assert(!gone, "hiding one leaves two")
            assert(overPrice == Present(CellPath("1", List("Price"))), "and the second one is now the price")
    }

    // The same defect on the mouse path. Focus cannot go to the button that replaces the
    // one just pressed: that element is new, and a self-command resolves its id the moment
    // it lands, which can be before the insert. It goes to a cell, which was already there.
    "the row editor's save button hands focus back to the row" in {
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Set("1"))
            name    <- Signal.initRef("Ada")
            price   <- Signal.initRef("10")
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            focused <- Signal.initRef(List.empty[String])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                uic.column("Price")(_.price.toString).editable(_.price)((i, v) => i.copy(price = v))
            ).editingRows(editing).wired(
                "t",
                Map(List("Name") -> name, List("Price") -> price),
                err,
                id => focused.getAndUpdate(_ :+ id)
            )
            save  <- elementWithClass(ui, "p-datatable-row-editor-save")
            _     <- click(save)
            open  <- editing.get
            store <- rows.get
            seen  <- focused.get
        yield
            assert(open == Set.empty[String], "pressing it closes the row")
            assert(store.head.name == "Ada", "writing the drafts through the columns")
            assert(seen == List("t-c0-0"), "and focus lands on the row's first editable cell")
    }

    /** A table whose three columns the reader may resize, with the header measurement
      * stubbed: what the grab reads is the width the browser would report, and the test
      * supplies it so the arithmetic is the only thing under test.
      */
    private def sized(widths: Map[String, Double], pinMiddle: Boolean = false)(using Frame) =
        for
            rows <- Signal.initRef[Seq[Item]](items)
            cols <- Signal.initRef(Map.empty[List[String], Double])
            err  <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            grab <- Signal.initRef(Absent: Maybe[ColumnGrab])
        yield
            val base = uic.DataTable[Item]().rows(rows).rowKey(_.id)
            val table =
                if pinMiddle then
                    base.columns(
                        uic.column("Name")(_.name),
                        uic.column("Price")(_.price.toString).resizable(false),
                        uic.column("Id")(_.id)
                    ).columnWidths(cols)
                else
                    base.columns(
                        uic.column("Name")(_.name),
                        uic.column("Price")(_.price.toString),
                        uic.column("Id")(_.id)
                    ).columnWidths(cols)
            // The stub answers by header id: `t-h0` is the first leaf column's cell.
            val measure = (id: String) =>
                UI.Rect(0, 0, widths.getOrElse(id, 0.0), 30, 1000, 800): UI.Rect < Async
            (table.wired("t", Map.empty, err, _ => (), Map.empty, measure, Present(grab)), cols, grab)
        end for
    end sized

    private def pointerAt(x: Double): UI.PointerEvent = UI.PointerEvent(x, 0, 0, 0, 6, 30, 1, Absent)

    private val mouseAt: UI.MouseEvent = UI.MouseEvent(Absent, UI.Modifiers.none)

    private def resizers(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-column-resizer")))

    private def drag(handle: UI.Ast.Element, from: Double, to: Double)(using Frame): Any < Async =
        (handle.attrs.onPointerDown, handle.attrs.onPointerMove) match
            case (Present(down), Present(move)) => down(pointerAt(from)).andThen(move(pointerAt(to)))
            case _                              => throw new AssertionError("the handle declares no drag")

    // The whole point of moving a boundary rather than sizing a column: the pair trades
    // width, so the table stays as wide as the space it was given.
    "dragging a boundary writes both columns and keeps their total" in {
        for
            (ui, cols, _) <- sized(Map("t-h0" -> 200, "t-h1" -> 100))
            handles       <- resizers(ui)
            _             <- drag(handles.head, 200, 240)
            widths        <- cols.get
        yield
            assert(widths == Map(List("Name") -> 240.0, List("Price") -> 60.0))
            assert(widths.values.sum == 300.0, "and the two of them still cover what they covered")
    }

    // Computed from the grab and not from the last frame: a pointer that outruns the
    // render posts the moves it made, and a per-frame delta would lose the ones between.
    "a drag follows the pointer from where it was grabbed" in {
        for
            (ui, cols, _) <- sized(Map("t-h0" -> 200, "t-h1" -> 100))
            handles       <- resizers(ui)
            down = handles.head.attrs.onPointerDown.getOrElse(throw new AssertionError("no grab"))
            move = handles.head.attrs.onPointerMove.getOrElse(throw new AssertionError("no drag"))
            _      <- down(pointerAt(200))
            _      <- move(pointerAt(220))
            _      <- move(pointerAt(250))
            widths <- cols.get
        yield assert(widths == Map(List("Name") -> 250.0, List("Price") -> 50.0), "the second move is absolute, not cumulative")
    }

    "a drag past the end of a column stops at the minimum" in {
        for
            (ui, cols, _) <- sized(Map("t-h0" -> 200, "t-h1" -> 100))
            handles       <- resizers(ui)
            _             <- drag(handles.head, 200, 900)
            widths        <- cols.get
        yield
            assert(widths == Map(List("Name") -> 285.0, List("Price") -> 15.0), "the neighbour keeps the floor")
            assert(widths.values.sum == 300.0, "and the total is still preserved")
    }

    // A boundary belongs to two columns, so one of them refusing takes it away.
    "a boundary needs a resizable column on both sides, and the last column has none" in {
        for
            (plain, _, _)  <- sized(Map.empty)
            (locked, _, _) <- sized(Map.empty, pinMiddle = true)
            all            <- resizers(plain)
            fewer          <- resizers(locked)
        yield
            assert(all.size == 2, "three columns are two boundaries")
            assert(fewer.isEmpty, "pinning the middle one takes both of its boundaries")
    }

    // The width has to reach the column, and a cell cannot carry it: a header cell of a
    // grouped table spans several columns and cannot name a width for any of them.
    "the widths render as a colgroup, one col per rendered column" in {
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            shown <- Signal.initRef(true)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).width(220),
                uic.column("Note")(_ => "note").width(80).visible(shown),
                uic.column("Price")(_.price.toString)
            ).render
            wide   <- elements(ui).map(_.collect { case c: UI.Ast.Col => c })
            _      <- shown.set(false)
            narrow <- elements(ui).map(_.collect { case c: UI.Ast.Col => c })
        yield
            assert(wide.size == 3, "one per column, sized or not")
            assert(wide.count(_.attrs.uiStyle.props.nonEmpty) == 2, "and only the two that have a width carry one")
            assert(narrow.size == 2, "a hidden column takes its col with it")
    }

    "a bound width map nothing can write into is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            cols <- Signal.initRef(Map(List("Nmae") -> 200.0))
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).columnWidths(cols).render
            cards <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-uic-key-error")))
            text = cards.flatMap(_.children.collect { case t: UI.Ast.Text => t.value }).mkString(" ")
        yield
            assert(cards.size == 2, "one column is no boundary, and the entry names no column")
            assert(text.contains("no boundary is draggable"))
            assert(text.contains("Nmae"), "the unknown path is named")
    }

    /** The cells of one column, header and body together, taken by position rather than by
      * class: what is under test is which of them hold and where, and a cell that has
      * stopped holding is exactly a cell with no class left to look up.
      */
    private def cellsAt(node: UI, index: Int)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.collect {
            case r: UI.Ast.Tr => r
        }.flatMap(r => r.children.collect { case e: UI.Ast.Element => e }.lift(index)))

    private def held(el: UI.Ast.Element): Boolean = el.attrs.cssClasses.contains("p-datatable-frozen-column")

    private def offsets(cells: Chunk[UI.Ast.Element]): List[Style.Prop] =
        cells.toList.flatMap(_.attrs.uiStyle.props.filter {
            case _: Style.Prop.Left | _: Style.Prop.Right => true
            case _                                        => false
        }).distinct

    private def cards(node: UI)(using Frame): String < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-uic-key-error"))
            .flatMap(_.children.collect { case t: UI.Ast.Text => t.value }).mkString(" "))

    // The whole arithmetic in one table: a frozen column holds at the distance the columns
    // between it and the edge take up, so the first sits on the edge and the second stands
    // off it by exactly the first one's width.
    "a frozen column holds at the width of the columns before it" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).width(220).frozen(true),
                uic.column("Price")(_.price.toString).width(120).frozen(true),
                uic.column("Id")(_.id)
            ).render
            first  <- cellsAt(ui, 0)
            second <- cellsAt(ui, 1)
            third  <- cellsAt(ui, 2)
        yield
            assert(first.forall(held) && second.forall(held), "header and body cells alike")
            assert(!third.exists(held), "the free column scrolls")
            assert(offsets(first) == List(Style.Prop.Left(0.px)))
            assert(offsets(second) == List(Style.Prop.Left(220.px)), "the width of the one before it")
    }

    // The table's own columns stand between a frozen one and the edge, so they hold too,
    // and their width is part of the offset. It is a variable rather than a number because
    // the width the col carries and the width the offset counts have to be one quantity.
    "the checkbox column holds with the frozen ones, and its width is in their offset" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).selectionMode(SelectionMode.Checkbox).columns(
                uic.column("Name")(_.name).width(220).frozen(true),
                uic.column("Price")(_.price.toString).width(120).frozen(true),
                uic.column("Id")(_.id)
            ).render
            lead   <- cellsAt(ui, 0)
            name   <- cellsAt(ui, 1)
            price  <- cellsAt(ui, 2)
            widths <- elements(ui).map(_.collect { case c: UI.Ast.Col => c })
        yield
            assert(lead.forall(held), "the checkbox column is between the frozen one and the edge")
            assert(offsets(lead) == List(Style.Prop.Left(0.px)))
            assert(offsets(name) == List(Style.Prop.Left(Length.Calc("var(--p-uic-dt-select-width)"))))
            assert(
                offsets(price) == List(Style.Prop.Left(Length.Calc("var(--p-uic-dt-select-width) + 220px"))),
                "past the checkbox column and the column before it"
            )
            assert(
                widths.head.attrs.uiStyle.props == Seq(Style.Prop.Width(Length.Calc("var(--p-uic-dt-select-width)"))),
                "and the col carries the same quantity, so the two cannot drift"
            )
    }

    "a column frozen against the end holds at the width of the ones after it" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString).width(150).frozen(FrozenEdge.End),
                uic.column("Id")(_.id).width(100).frozen(FrozenEdge.End)
            ).render
            free   <- cellsAt(ui, 0)
            middle <- cellsAt(ui, 1)
            last   <- cellsAt(ui, 2)
        yield
            assert(!free.exists(held))
            assert(offsets(last) == List(Style.Prop.Right(0.px)), "the last column sits on the trailing edge")
            assert(offsets(middle) == List(Style.Prop.Right(100.px)), "and the one before it stands off by its width")
    }

    // All or nothing: an offset is a sum over the columns in front of a cell, so one that
    // cannot contribute its width makes every offset behind it wrong, and a wrong offset
    // is a column parked over the middle of the table.
    "a frozen column with no width freezes nothing and names itself" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).frozen(true),
                uic.column("Price")(_.price.toString).width(120)
            ).render
            first <- cellsAt(ui, 0)
            text  <- cards(ui)
        yield
            assert(!first.exists(held), "nothing holds")
            assert(text.contains("has no width"))
            assert(text.contains("Name"), "and the column is named")
    }

    "a free column between a frozen one and its edge freezes nothing" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).width(220),
                uic.column("Price")(_.price.toString).width(120).frozen(true)
            ).render
            second <- cellsAt(ui, 1)
            text   <- cards(ui)
        yield
            assert(!second.exists(held))
            assert(text.contains("free column between it and its edge"))
            assert(text.contains("Price"))
    }

    // One header cell cannot half scroll, so a group has to agree with itself.
    "a header group over a frozen and a free column is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.headerGroup("Item")(
                    uic.column("Name")(_.name).width(220).frozen(true),
                    uic.column("Price")(_.price.toString).width(120)
                )
            ).render
            first <- cellsAt(ui, 0)
            text  <- cards(ui)
        yield
            assert(!first.exists(held), "the group disagrees with itself, so the table freezes nothing")
            assert(text.contains("do not agree where they belong"))
            assert(text.contains("Item"))
    }

    // A width the reader dragged is the width the offsets are computed from, since both
    // read the same bound map on the same render.
    "a dragged width moves the column stuck behind it" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            cols <- Signal.initRef(Map(List("Name") -> 200.0))
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).frozen(true),
                uic.column("Price")(_.price.toString).width(120).frozen(true),
                uic.column("Id")(_.id)
            ).columnWidths(cols).render
            before <- cellsAt(ui, 1).map(offsets)
            _      <- cols.set(Map(List("Name") -> 260.0))
            after  <- cellsAt(ui, 1).map(offsets)
        yield
            assert(before == List(Style.Prop.Left(200.px)), "the bound width is what the offset counts")
            assert(after == List(Style.Prop.Left(260.px)), "and it follows the drag that wrote it")
    }

    /** A table whose columns the reader may drag, with the header measurement stubbed:
      * every leaf header cell is 100 wide, so `t-h0` starts at 0, `t-h1` at 100, and an
      * absolute x of 250 is the middle of the third column.
      */
    private def movable(
        pin: Boolean = false,
        freeze: Boolean = false,
        group: Boolean = false,
        hide: Boolean = false
    )(using Frame) =
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            order <- Signal.initRef(List.empty[List[String]])
            sort  <- Signal.initRef(List.empty[SortKey])
            shown <- Signal.initRef(!hide)
            err   <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            move  <- Signal.initRef(Absent: Maybe[ColumnDrag])
        yield
            val base = uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort)
            val table =
                if group then
                    base.columns(
                        uic.headerGroup("G")(
                            uic.column("Name")(_.name).sortBy(_.name),
                            uic.column("Price")(_.price.toString)
                        ),
                        uic.column("Id")(_.id)
                    )
                else if pin then
                    base.columns(
                        uic.column("Name")(_.name).sortBy(_.name),
                        uic.column("Price")(_.price.toString).reorderable(false),
                        uic.column("Id")(_.id)
                    )
                else if freeze then
                    base.columns(
                        uic.column("Name")(_.name).sortBy(_.name).width(100).frozen(true),
                        uic.column("Price")(_.price.toString),
                        uic.column("Id")(_.id)
                    )
                else
                    base.columns(
                        uic.column("Name")(_.name).sortBy(_.name),
                        uic.column("Price")(_.price.toString).visible(shown),
                        uic.column("Id")(_.id)
                    )
            val measure = (id: String) =>
                val i = id.drop(id.indexOf("-h") + 2).toInt
                UI.Rect(i * 100.0, 0, 100, 30, 1000, 800): UI.Rect < Async
            (
                table.columnOrder(order).wired("t", Map.empty, err, _ => (), Map.empty, measure, Absent, Present(move)),
                order,
                sort
            )
        end for
    end movable

    /** Grabs a header cell and lets go somewhere else, the three events a real pointer
      * posts in that order.
      */
    private def dragHeader(node: UI, id: String, from: Double, to: Double)(using Frame): Any < Async =
        for
            cell <- cellWithId(node, id)
            down = cell.attrs.onPointerDown.getOrElse(throw new AssertionError("the header declares no grab"))
            move = cell.attrs.onPointerMove.getOrElse(throw new AssertionError("the header declares no drag"))
            up   = cell.attrs.onPointerUp.getOrElse(throw new AssertionError("the header declares no release"))
            _ <- down(pointerAt(from))
            _ <- move(pointerAt(to))
            r <- up(pointerAt(to))
        yield r

    "dragging a header past another writes the new order" in {
        for
            (ui, order, _) <- movable()
            _              <- dragHeader(ui, "t-h0", 50, 210)
            moved          <- order.get
        yield assert(moved == List(List("Price"), List("Name"), List("Id")), "the grabbed column lands where it was dropped")
    }

    // A header that sorts is also a header that can be dragged, so what tells the two
    // apart is whether the pointer travelled at all.
    "a press that does not travel leaves the order alone and still sorts" in {
        for
            (ui, order, sort) <- movable()
            cell              <- cellWithId(ui, "t-h0")
            down = cell.attrs.onPointerDown.getOrElse(throw new AssertionError("no grab"))
            up   = cell.attrs.onPointerUp.getOrElse(throw new AssertionError("no release"))
            _     <- down(pointerAt(50))
            _     <- up(pointerAt(50))
            still <- order.get
            _     <- cell.attrs.onClickEvt.getOrElse(throw new AssertionError("no click"))(mouseAt)
            keys  <- sort.get
        yield
            assert(still.isEmpty, "nothing was written")
            assert(keys.map(_.path) == List(List("Name")), "the press was the click that sorts")
    }

    // The browser still owes the cell the click that ends the press, and a header that
    // sorts would answer a gesture the reader did not make.
    "the click that ends a drag does not sort, and the next one does" in {
        for
            (ui, _, sort) <- movable()
            _             <- dragHeader(ui, "t-h0", 50, 210)
            // Name is the second column now, which is the whole point: the columns moved
            // under the pointer that was still holding them.
            cell <- cellWithId(ui, "t-h1")
            clickIt = cell.attrs.onClickEvt.getOrElse(throw new AssertionError("the header declares no click"))
            _     <- clickIt(mouseAt)
            after <- sort.get
            // Freshly off the tree, as the browser has it: swallowing the click cleared the
            // state, and the re-render that followed handed the cell a handler that sorts.
            again <- cellWithId(ui, "t-h1")
            _     <- again.attrs.onClickEvt.getOrElse(throw new AssertionError("no click"))(mouseAt)
            later <- sort.get
        yield
            assert(after.isEmpty, "the click that ended the drag is swallowed")
            assert(later.map(_.path) == List(List("Name")), "and the one after it sorts")
    }

    // A pinned column keeps its INDEX and not merely its neighbours, so every drop that
    // would carry it one place along is taken away rather than quietly moving it.
    "a column that refuses to be reordered takes away the drops that would move it" in {
        for
            (pinned, _, _) <- movable(pin = true)
            first          <- cellWithId(pinned, "t-h0")
            last           <- cellWithId(pinned, "t-h2")
        yield
            assert(!first.attrs.cssClasses.contains("p-datatable-reorderable-column"), "nowhere to go past the pin")
            assert(!last.attrs.cssClasses.contains("p-datatable-reorderable-column"))
    }

    "a drop that would leave a frozen column adrift is not offered" in {
        for
            (ui, order, _) <- movable(freeze = true)
            _              <- dragHeader(ui, "t-h1", 150, 10)
            still          <- order.get
            _              <- dragHeader(ui, "t-h1", 150, 310)
            moved          <- order.get
        yield
            assert(still.isEmpty, "nothing may pass in front of the frozen column")
            assert(moved == List(List("Name"), List("Id"), List("Price")), "and behind it everything still moves")
    }

    // The header is a tree, and a boundary inside another group would ask one cell to sit
    // in two places at once.
    "a column moves among its own siblings and no further" in {
        for
            (ui, order, _) <- movable(group = true)
            _              <- dragHeader(ui, "t-h0", 50, 310)
            outside        <- order.get
            _              <- dragHeader(ui, "t-h0", 50, 210)
            inside         <- order.get
        yield
            assert(outside.isEmpty, "a boundary past the group is not one this column may land on")
            assert(
                inside == List(List("G", "Price"), List("G", "Name"), List("Id")),
                "and the far end of its own group is"
            )
    }

    // The order a drop writes is the whole authored list, so a column the reader is
    // hiding comes back where it was rather than behind everything that moved.
    "a hidden column keeps its place in the order a drag writes" in {
        for
            (ui, order, _) <- movable(hide = true)
            _              <- dragHeader(ui, "t-h0", 50, 190)
            moved          <- order.get
        yield assert(moved == List(List("Price"), List("Id"), List("Name")), "the hidden Price keeps its own place")
    }

    "the order names a column this table does not have" in {
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            order <- Signal.initRef(List(List("Nmae")))
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).columnOrder(order).render
            text <- cards(ui)
        yield
            assert(text.contains("names a column this table does not have"))
            assert(text.contains("Nmae"))
    }

    "moveBlock lifts a block out and puts it back in front of what it was dropped on" in {
        val xs = List("a", "b", "c", "d")
        assert(DataTable.moveBlock(xs, 0, 1, 3) == List("b", "c", "a", "d"), "forwards, past what it passed")
        assert(DataTable.moveBlock(xs, 3, 4, 1) == List("a", "d", "b", "c"), "backwards, in front of the anchor")
        assert(DataTable.moveBlock(xs, 0, 2, 4) == List("c", "d", "a", "b"), "a block of two travels together")
        assert(DataTable.moveBlock(xs, 1, 2, 1) == xs, "a drop where it already is changes nothing")
        assert(DataTable.moveBlock(xs, 1, 2, 2) == xs, "and neither does the boundary behind it")
    }

    "holdsPinned refuses a move that shifts a column which would not be moved" in {
        val free = List(true, false, true)
        assert(!DataTable.holdsPinned(free, 0, 1, 2), "the pinned middle would slide to the front")
        assert(DataTable.holdsPinned(List(true, true, false), 0, 1, 2), "here the pinned one keeps index 2")
    }

end DataTableTest
