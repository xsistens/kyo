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

    /** What the reader typed, delivered the way the client delivers it.
      *
      * The other edit tests write the draft ref by hand, which proves what the table does
      * with a draft and nothing about how one gets there. This goes through the field's own
      * input-time channel, which is the half that was broken: a field carrying neither
      * writes nothing until it loses focus, so Enter's keydown commits a draft that still
      * holds the seed.
      *
      * Both channels count, because both are input-time on the client: a declared
      * `onInput`, and a `value` bound to a ref, which the renderer turns into the same
      * event (`hasSignalRefValue`).
      */
    private def typeInto(cell: UI.Ast.Element, text: String)(using Frame): Any < Async =
        elements(cell).map { els =>
            els.collectFirst {
                case t: UI.Ast.TextInput if t.onInput.isDefined         => t.onInput.get
                case t: UI.Ast.TextInput if refBound(t.value).isDefined => refBound(t.value).get.set
            }.getOrElse(throw new AssertionError("the open editor writes nothing as it is typed into"))
        }.map(_(text))

    private def refBound(v: Maybe[UI.Bound[String]]): Maybe[SignalRef[String]] =
        v match
            case Present(UI.Bound.Ref(ref)) => Present(ref)
            case _                          => Absent

    "a text editor writes into the draft as it is typed" in {
        for
            (ui, _, _, name, _, _, _) <- wire
            cell                      <- elementWithId(ui, "t-c0-0")
            _                         <- press(cell, UI.Keyboard.Enter)
            open                      <- elementWithId(ui, "t-c0-0")
            _                         <- typeInto(open, "Ada")
            draft                     <- name.get
        yield assert(draft == "Ada")
    }

    // The number editor reported only on `change`, which the browser fires on blur or on
    // Enter's DEFAULT action, both of them after the keydown the table commits on. So the
    // commit read the seed, found it unchanged, wrote nothing and closed: a valid edit was
    // dropped and a refused one showed no message, since nothing was ever refused.
    "a number editor writes into the draft as it is typed" in {
        for
            (ui, _, _, _, price, _, _) <- wire
            cell                       <- elementWithId(ui, "t-c0-1")
            _                          <- press(cell, UI.Keyboard.Enter)
            open                       <- elementWithId(ui, "t-c0-1")
            _                          <- typeInto(open, "42")
            draft                      <- price.get
        yield assert(draft == "42")
    }

    "a number cell commits what was typed into it, and refuses what a rule rejects" in {
        for
            (ui, rows, editing, _, _, err, _) <- wire
            cell                              <- elementWithId(ui, "t-c0-1")
            _                                 <- press(cell, UI.Keyboard.Enter)
            open                              <- elementWithId(ui, "t-c0-1")
            _                                 <- typeInto(open, "42")
            reopened                          <- elementWithId(ui, "t-c0-1")
            _                                 <- press(reopened, UI.Keyboard.Enter)
            stored                            <- rows.get
            _                                 <- press(cell, UI.Keyboard.Enter)
            open2                             <- elementWithId(ui, "t-c0-1")
            _                                 <- typeInto(open2, "0")
            reopened2                         <- elementWithId(ui, "t-c0-1")
            _                                 <- press(reopened2, UI.Keyboard.Enter)
            stillOpen                         <- editing.get
            refused                           <- err.get
        yield
            assert(stored.head.price == 42, "the typed value reached the row")
            assert(stillOpen == Present(CellPath("1", List("Price"))), "the refused cell stayed open")
            assert(refused.exists((_, e) => e.code == "min"), "with the rule's own error on it")
    }

    // Moving the editor is a commit like any other. Opening the next cell without one drops
    // whatever was typed into the last, silently, which is what a reader clicking from cell
    // to cell does all day.
    "clicking another cell commits the one that is open" in {
        for
            (ui, rows, editing, _, _, _, _) <- wire
            cell                            <- elementWithId(ui, "t-c0-0")
            _                               <- press(cell, UI.Keyboard.Enter)
            open                            <- elementWithId(ui, "t-c0-0")
            _                               <- typeInto(open, "Ada")
            other                           <- elementWithId(ui, "t-c1-0")
            _                               <- click(other)
            stored                          <- rows.get
            now                             <- editing.get
        yield
            assert(stored.head.name == "Ada", "what was typed reached the row")
            assert(now == Present(CellPath("2", List("Name"))), "and the editor moved on")
    }

    "clicking another cell while the open one refuses leaves the editor where it is" in {
        for
            (ui, rows, editing, _, _, err, _) <- wire
            cell                              <- elementWithId(ui, "t-c0-1")
            _                                 <- press(cell, UI.Keyboard.Enter)
            open                              <- elementWithId(ui, "t-c0-1")
            _                                 <- typeInto(open, "0")
            other                             <- elementWithId(ui, "t-c1-0")
            _                                 <- click(other)
            stored                            <- rows.get
            now                               <- editing.get
            refused                           <- err.get
        yield
            assert(stored.head.price == 10, "nothing was written")
            assert(now == Present(CellPath("1", List("Price"))), "the refusing cell kept the editor")
            assert(refused.exists((_, e) => e.code == "min"), "and shows why")
    }

    // Tab commits and moves on, which is right until the commit is refused: then moving on
    // opens the next cell, and opening a cell clears the error, so the refusal was invisible
    // and the edit was gone.
    "Tab past a refused commit leaves the editor where it is, with the error showing" in {
        for
            (ui, rows, editing, _, _, err, focused) <- wire
            cell                                    <- elementWithId(ui, "t-c0-1")
            _                                       <- press(cell, UI.Keyboard.Enter)
            open                                    <- elementWithId(ui, "t-c0-1")
            _                                       <- typeInto(open, "0")
            reopened                                <- elementWithId(ui, "t-c0-1")
            _                                       <- press(reopened, UI.Keyboard.Tab)
            stored                                  <- rows.get
            now                                     <- editing.get
            refused                                 <- err.get
            seen                                    <- focused.get
        yield
            assert(stored.head.price == 10, "nothing was written")
            assert(now == Present(CellPath("1", List("Price"))), "the editor stayed on the value being fixed")
            assert(refused.exists((_, e) => e.code == "min"), "and the error survived")
            // The cell would be the editor's container, so landing there is landing outside
            // the field: the reader would have to tab back IN to fix what they were told
            // about. Focus goes to the id the editor stamped.
            assert(seen == List("t-e0-1"), "and focus went back into the editor, not onto its cell")
    }

    "Enter on a resting cell opens it, seeded from the row" in {
        for
            (ui, _, editing, name, _, _, _) <- wire
            cell                            <- elementWithId(ui, "t-c0-0")
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
            cell                            <- elementWithId(ui, "t-c1-0")
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
            cell                                     <- elementWithId(ui, "t-c0-0")
            _                                        <- press(cell, UI.Keyboard.Enter)
            _                                        <- name.set("Ada")
            reopened                                 <- elementWithId(ui, "t-c0-0")
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
            cell                                     <- elementWithId(ui, "t-c0-0")
            _                                        <- press(cell, UI.Keyboard.Enter)
            _                                        <- name.set("Ada")
            reopened                                 <- elementWithId(ui, "t-c0-0")
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
            cell                                        <- elementWithId(ui, "t-c0-1")
            _                                           <- press(cell, UI.Keyboard.Enter)
            _                                           <- price.set("0")
            reopened                                    <- elementWithId(ui, "t-c0-1")
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
            cell                               <- elementWithId(ui, "t-c0-0")
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
            cell                                         <- elementWithId(ui, "t-c0-0")
            _                                            <- press(cell, UI.Keyboard.Enter)
            _                                            <- name.set("Ada")
            reopened                                     <- elementWithId(ui, "t-c0-0")
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
    /** One node's own elements, with a reactive or fragment layer resolved away: a windowed
      * body wraps its rows in one, since the rows are what a scroll replaces and the row
      * group is what has to stay put.
      */
    private def resolve(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        node match
            case e: UI.Ast.Element       => Chunk(e)
            case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(resolve)
            case f: UI.Ast.Fragment[?]   => Kyo.foreach(f.children)(resolve).map(_.flatten)
            case k: UI.Ast.KeyedChild[?] => resolve(k.child)
            case _                       => Chunk.empty

    private def rowsUnder(e: UI.Ast.Element)(using Frame): Chunk[UI.Ast.Element] < Sync =
        Kyo.foreach(e.children)(resolve).map(_.flatten)

    private def bodyNames(node: UI)(using Frame): Chunk[String] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-tbody")))
            .flatMap(bs => Kyo.foreach(bs)(rowsUnder).map(_.flatten))
            .map(_.flatMap(_.children.collect { case c: UI.Ast.Element => c }.headOption))
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

    "and a mode can be picked with the keyboard, since the list is a row of tab stops" in {
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
            _    <- press(starts, UI.Keyboard.Space)
            spec <- filters.get
        yield assert(spec == Map(List("Name") -> ColumnFilter("A", MatchMode.StartsWith)))
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
            wide      <- elementWithId(ui, "t-c0-1")
            _         <- press(wide, UI.Keyboard.Enter)
            overNote  <- editing.get
            third     <- hasCell(ui, "t-c0-2")
            _         <- shown.set(false)
            narrow    <- elementWithId(ui, "t-c0-1")
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
            // The stub answers by header id: `t-h0` is the first leaf column's cell. A grab
            // asks for every id it needs in one call, so the stub answers a list.
            val measure = (ids: Seq[String]) =>
                Chunk.from(ids.map(id => UI.Rect(0, 0, widths.getOrElse(id, 0.0), 30, 1000, 800))): Chunk[UI.Rect] < Async
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
            val measure = (ids: Seq[String]) =>
                Chunk.from(ids.map { id =>
                    val i = id.drop(id.indexOf("-h") + 2).toInt
                    UI.Rect(i * 100.0, 0, 100, 30, 1000, 800)
                }): Chunk[UI.Rect] < Async
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
            cell <- elementWithId(node, id)
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
            cell              <- elementWithId(ui, "t-h0")
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
            cell <- elementWithId(ui, "t-h1")
            clickIt = cell.attrs.onClickEvt.getOrElse(throw new AssertionError("the header declares no click"))
            _     <- clickIt(mouseAt)
            after <- sort.get
            // Freshly off the tree, as the browser has it: swallowing the click cleared the
            // state, and the re-render that followed handed the cell a handler that sorts.
            again <- elementWithId(ui, "t-h1")
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
            first          <- elementWithId(pinned, "t-h0")
            last           <- elementWithId(pinned, "t-h2")
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

    // ---- lazily loaded rows ----

    /** Two rows in an order no sort would put them in, so a table that sorted them would
      * say so by moving them.
      */
    private val unsorted = List(Item("2", "B", 20), Item("1", "A", 10))

    /** The page numbers the paginator offers, and which of them is the current one. */
    private def pages(node: UI)(using Frame): (List[String], List[String]) < Sync =
        elements(node).map { all =>
            val buttons                  = all.filter(_.attrs.cssClasses.contains("p-paginator-page")).toList
            def label(e: UI.Ast.Element) = e.children.collect { case t: UI.Ast.Text => t.value }.mkString
            (buttons.map(label), buttons.filter(_.attrs.cssClasses.contains("p-paginator-page-selected")).map(label))
        }

    "a lazily loaded table renders the rows in the order it was given them" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            sort <- Signal.initRef(List(SortKey("Name", SortDirection.Ascending)))
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort).columns(
                uic.column("Name")(_.name).sortBy(_.name)
            )
            here  <- bodyNames(base.render)
            there <- bodyNames(base.lazyRows(2).render)
        yield
            assert(here == Chunk("A", "B"), "the same spec sorts a table that owns its rows")
            assert(there == Chunk("B", "A"), "and moves nothing in one whose rows arrive sorted")
    }

    "a lazily loaded table filters nothing it was given" in {
        for
            rows  <- Signal.initRef[Seq[Item]](unsorted)
            query <- Signal.initRef("A")
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).globalFilter(query).columns(
                uic.column("Name")(_.name)
            )
            here  <- bodyNames(base.render)
            there <- bodyNames(base.lazyRows(2).render)
        yield
            assert(here == Chunk("A"), "a table that owns its rows answers the query itself")
            assert(there == Chunk("B", "A"), "and one whose rows arrive filtered leaves the query to whoever read it")
    }

    // The query is the server's to read, so the input cannot be told it is unreadable: a
    // number column asked for "abc" would mark itself in a table that reads its own.
    "a column filter of a lazily loaded table narrows nothing and marks nothing" in {
        for
            rows    <- Signal.initRef[Seq[Item]](unsorted)
            filters <- Signal.initRef(Map(List("Price") -> ColumnFilter("abc", MatchMode.Equals)))
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString).filterBy(_.price)
            ).columnFilters(filters)
            here  = base.wired("t", Map.empty, err, _ => ())
            there = base.lazyRows(2).wired("t", Map.empty, err, _ => ())
            hereRows  <- bodyNames(here)
            hereMark  <- elements(here).map(_.exists(_.attrs.cssClasses.contains("p-invalid")))
            thereRows <- bodyNames(there)
            thereMark <- elements(there).map(_.exists(_.attrs.cssClasses.contains("p-invalid")))
        yield
            assert(hereRows == Chunk("B", "A"), "a query neither table can use empties neither of them")
            assert(hereMark, "the table that reads its own says the query is not a price")
            assert(thereRows == Chunk("B", "A"))
            assert(!thereMark, "and the one that reads none of them says nothing about it")
    }

    "the paginator of a lazily loaded table counts the pages the total says exist" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            page <- Signal.initRef(0)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).paginate(2)(page).lazyRows(7).render
            (offered, current) <- pages(ui)
            shown              <- bodyNames(ui)
        yield
            assert(offered == List("1", "2", "3", "4"), "seven rows of two make four pages")
            assert(current == List("1"))
            assert(shown == Chunk("B", "A"), "and the page it was given is the page it renders")
    }

    "a page past the end of a lazily loaded table clamps to the last one" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            page <- Signal.initRef(9)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).paginate(2)(page).lazyRows(7).render
            (_, current) <- pages(ui)
        yield assert(current == List("4"), "the total is what says where the pages stop")
    }

    "a total that arrives with its page repaints the paginator" in {
        for
            rows  <- Signal.initRef[Seq[Item]](unsorted)
            page  <- Signal.initRef(0)
            total <- Signal.initRef(Total.Known(7): Total)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).paginate(2)(page).lazyRows(total).render
            (before, _) <- pages(ui)
            _           <- total.set(Total.Known(3))
            (after, _)  <- pages(ui)
        yield
            assert(before == List("1", "2", "3", "4"))
            assert(after == List("1", "2"), "a query that matched fewer rows offers fewer pages")
    }

    // The ordering is not what sorts a prepared table, so the column has to say it sorts;
    // the default cannot be yes, or every column would offer a sort nobody asked for.
    "a column of a lazily loaded table sorts once it says so" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            sort <- Signal.initRef(List.empty[SortKey])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort).lazyRows(2).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString).sortable(true),
                uic.column("Id")(_.id).sortBy(_.id)
            ).render
            heads <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-datatable-header-cell")).toList)
            _     <- heads(1).attrs.onClickEvt.getOrElse(throw new AssertionError("the header declares no click"))(mouseAt)
            spec  <- sort.get
        yield
            val offers = heads.map(_.attrs.cssClasses.contains("p-datatable-sortable-column"))
            assert(offers == List(false, true, true), "the flag says so, and so does an ordering nothing reads")
            assert(spec == List(SortKey("Price", SortDirection.Ascending)), "and the click writes the spec")
    }

    "sortable with nothing to sort by is a mistake only where the table sorts" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            sort <- Signal.initRef(List(SortKey("Price", SortDirection.Ascending)))
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString).sortable(true)
            )
            here  <- cards(base.render)
            there <- cards(base.lazyRows(2).render)
        yield
            assert(here.contains("nothing to sort by"), "a table that sorts needs the ordering")
            assert(here.contains("cannot sort"), "and cannot sort by the spec entry either")
            assert(there == "", "a table that sorts elsewhere needs neither")
    }

    "a sort spec of a lazily loaded table naming no column is still reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            sort <- Signal.initRef(List(SortKey("Nmae", SortDirection.Ascending)))
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort).lazyRows(2).columns(
                uic.column("Name")(_.name).sortable(true)
            ).render)
        yield
            assert(text.contains("Nmae"))
            assert(text.contains("sortBy or sortable(true)"), "which is what marks a column of such a table")
    }

    "more rows than a page holds is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            page <- Signal.initRef(0)
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).paginate(1)(page).lazyRows(7).render)
        yield
            assert(text.contains("more rows than one page holds"))
            assert(text.contains("2 rows over a page of 1"))
    }

    "more rows than the total says exist is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).lazyRows(1).render)
        yield
            assert(text.contains("more rows than the total it says exist"))
            assert(text.contains("2 rows out of a total of 1"))
    }

    // ---- rows from a RowSource ----

    /** The first non-empty window a source published, which is the barrier a test needs
      * before it renders: taking twice would race a fetch that finished first.
      */
    private def served[A](ch: Channel[Seq[A]])(using Frame): Seq[A] < (Async & Abort[Closed]) =
        ch.take.map(v => if v.nonEmpty then v else served(ch))

    private val five = (1 to 5).toList.map(i => Item(i.toString, ('A' + i - 1).toChar.toString, i * 10))

    "a bound source fills the rows, the total and the paginator in one call" in {
        for
            query <- Signal.initRef("a")
            src <- RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            seen <- Channel.init[Seq[Item]](16)
            _    <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _    <- served(seen)
            ui = uic.DataTable[Item]().rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).source(src).render
            names              <- bodyNames(ui)
            (offered, current) <- pages(ui)
        yield
            assert(names == Chunk("A", "B"), "the first page of the source")
            assert(offered == List("1", "2", "3"), "five rows of two, counted off the source's total")
            assert(current == List("1"))
    }

    // A cursor knows what follows, not how much. The paginator then grows a page at a
    // time rather than counting out pages nothing will ever fill.
    "an unknown total offers one page more while anything follows" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            page <- Signal.initRef(1)
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).paginate(2)(page)
            (more, _) <- pages(base.lazyRows(Total.Unknown(true)).render)
            (last, _) <- pages(base.lazyRows(Total.Unknown(false)).render)
        yield
            assert(more == List("1", "2", "3"), "two rows on page 2 and something after them")
            assert(last == List("1", "2"), "and nothing after them makes page 2 the last")
    }

    // A computed row list is a Signal and not a SignalRef: nothing can write into a
    // `map` of a query state, so the read-only binding is the only honest one for it.
    "rows bound to a read-only signal are read, and re-read when it emits" in {
        for
            source <- Signal.initRef[Seq[Item]](items)
            // A derived signal — exactly what a query state or a paginated
            // connection hands a caller, and not something anyone can write into.
            derived = source.map(_.filter(_.price >= 20))
            table = uic.DataTable[Item]().rows(derived).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            )
            before <- bodyNames(table.render)
            _      <- source.set(items :+ Item("3", "C", 30))
            after  <- bodyNames(table.render)
        yield
            assert(before == Chunk("B"), "the projection is what the table shows")
            assert(after == Chunk("B", "C"), "and a later emission is picked up")
    }

    "rows bound twice are reported, since only one binding is read" in {
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            query <- Signal.initRef("a")
            src <- RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).source(src).render)
        yield
            assert(text.contains("bound twice"))
            assert(text.contains("source") && text.contains("rows(ref)"), "and both bindings are named")
    }

    "a signal binding and a seq binding are reported the same way, and named apart from a source" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            text <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).rows(rows.readOnly).render)
        yield
            assert(text.contains("bound twice"))
            assert(text.contains("rows(signal)") && text.contains("rows(seq)"))
            assert(!text.contains("source"), "no RowSource is bound, so nothing may claim one is")
    }

    // ---- a windowed body ----

    /** A hundred rows, more than any viewport in these tests can hold. */
    private val hundred = (1 to 100).toList.map(i => Item(i.toString, s"R$i", i))

    /** Five rows fit the viewport (200 / 40), and with no overscan the window at rest is
      * rows 0 to 5 plus the one the arithmetic reaches into.
      */
    private def windowed(using Frame): DataTable[Item] =
        uic.DataTable[Item]().rowKey(_.id).columns(uic.column("Name")(_.name))
            .scrollHeight("200px").scrollRows(40, overscan = 0)

    private def bodyEl(node: UI)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(node, "p-datatable-tbody")

    private def bodyTrs(node: UI)(using Frame): List[UI.Ast.Element] < Sync =
        bodyEl(node).flatMap(rowsUnder).map(_.toList)

    private def heightOf(e: UI.Ast.Element): Maybe[Length] =
        Maybe.fromOption(e.attrs.uiStyle.props.collect { case Style.Prop.Height(v) => v }.headOption)

    /** The two spacers, ahead of the window and behind it, by the height each holds. */
    private def spacers(node: UI)(using Frame): List[Maybe[Length]] < Sync =
        bodyTrs(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-virtualscroller-spacer")).map(heightOf))

    private def isSlotRow(e: UI.Ast.Element): Boolean =
        e.children.collect { case c: UI.Ast.Element => c }
            .flatMap(_.children.collect { case c: UI.Ast.Element => c })
            .exists(_.attrs.cssClasses.contains("p-skeleton"))

    /** What the window says about itself, which the LEADING spacer carries: the row group
      * outlives every scroll, so nothing on it could say where the window sits.
      */
    private def windowAttrs(node: UI)(using Frame): (Maybe[String], Maybe[String]) < Sync =
        bodyTrs(node).map(_.head).map(e =>
            (Maybe.fromOption(e.attrs.dataAttrs.get("uic-vs-first")), Maybe.fromOption(e.attrs.dataAttrs.get("uic-vs-count")))
        )

    "a windowed table draws the rows in view and holds the rest of the list in a spacer" in {
        for
            rows   <- Signal.initRef[Seq[Item]](hundred)
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.rows(rows).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            names   <- bodyNames(ui)
            pad     <- spacers(ui)
            attrs   <- windowAttrs(ui)
            body    <- bodyEl(ui)
            offered <- pages(ui).map(_._1)
        yield
            assert(names == Chunk("R1", "R2", "R3", "R4", "R5", "R6"), "the six rows the arithmetic reaches")
            assert(pad == List(Present(0.px), Present(3760.px)), "and one spacer holding the ninety-four behind them")
            // A floor under the table while its rows are rewritten: they go in document
            // order, so a leading spacer shrinking before the trailing one grows would
            // collapse it under the scroll position for an instant.
            assert(heightOf(body) == Present(4000.px), "the body is as tall as the whole list")
            assert(attrs == (Present("0"), Present("6")))
            assert(offered.isEmpty, "a windowed table scrolls instead of paginating")
    }

    // The scroll position is a row index in disguise, and the stripe follows the row it
    // belongs to rather than its place in the window.
    "a scroll moves the window, and the rows keep the stripe their own index gives them" in {
        for
            rows   <- Signal.initRef[Seq[Item]](hundred)
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.rows(rows).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            _     <- scroll.set(400.0)
            names <- bodyNames(ui)
            pad   <- spacers(ui)
            trs   <- bodyTrs(ui)
            attrs <- windowAttrs(ui)
        yield
            assert(names == Chunk("R11", "R12", "R13", "R14", "R15", "R16"), "ten rows down")
            assert(pad == List(Present(400.px), Present(3360.px)), "held apart by what is above and below")
            assert(attrs == (Present("10"), Present("6")))
            val first = trs.filterNot(_.attrs.cssClasses.contains("p-datatable-virtualscroller-spacer")).head
            assert(first.attrs.cssClasses.contains("p-row-even"), "row ten is an even row wherever it is drawn")
            assert(heightOf(first) == Present(40.px), "and says how tall it is, since it is placed by counting")
    }

    "a windowed source is asked for the range the scroll is about to draw" in {
        for
            query  <- Signal.initRef("a")
            src    <- RowSource.init(query, pageSize = 6)((_, o, l) => (hundred.slice(o, o + l), Total.Known(100)))
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.source(src).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            vs <- elementWithClass(ui, "p-virtualscroller")
            _ <- vs.attrs.onScrollPos.getOrElse(throw new AssertionError("the scroller reports no scroll"))(
                UI.ScrollPositionEvent(400.0, 0.0, Absent)
            )
            at  <- scroll.get
            ask <- src.demand.get
        yield
            assert(at == 400.0, "the position the browser reported")
            assert(ask == RowSource.Demand(10, 6), "and the rows that position puts on the screen")
    }

    // Between a scroll and the window it asked for there is a fetch, and the rows on the
    // screen are the ones the source published, at the offset it published them at.
    "a row the source has not reached is drawn as a row of slots the same height" in {
        for
            query  <- Signal.initRef("a")
            src    <- RowSource.init(query, pageSize = 6)((_, o, l) => (hundred.slice(o, o + l), Total.Known(100)))
            seen   <- Channel.init[Seq[Item]](16)
            _      <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _      <- served(seen)
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.source(src).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            here  <- bodyNames(ui)
            _     <- scroll.set(400.0)
            trs   <- bodyTrs(ui)
            names <- bodyNames(ui)
            pad   <- spacers(ui)
        yield
            assert(here == Chunk("R1", "R2", "R3", "R4", "R5", "R6"), "the range the source served")
            assert(names.isEmpty, "none of which is in the window the scroll moved to")
            val drawn = trs.filterNot(_.attrs.cssClasses.contains("p-datatable-virtualscroller-spacer"))
            assert(drawn.size == 6 && drawn.forall(isSlotRow), "so the window is six slots")
            assert(drawn.forall(t => heightOf(t) == Present(40.px)), "each as tall as the row it stands in for")
            assert(pad == List(Present(400.px), Present(3360.px)), "and the list is as long as it was")
    }

    // Infinite scrolling is the total saying it does not know, and it reaches one screen
    // past whatever has been served so far.
    "an unknown total leaves a screen to scroll into while anything follows" in {
        for
            query <- Signal.initRef("a")
            src <- RowSource.init(query, pageSize = 6)((_, o, l) =>
                (hundred.slice(o, o + l), Total.Unknown(o + l < hundred.size))
            )
            seen   <- Channel.init[Seq[Item]](16)
            _      <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _      <- served(seen)
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.source(src).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            pad <- spacers(ui)
        yield
            // Six rows are drawn, at forty each, and the spacer holds the rest of the
            // reach: eleven rows in all, which is what has been served plus one screen.
            assert(pad == List(Present(0.px), Present(200.px)), "the five rows there are still to scroll into")
    }

    "a scroll height in no unit the window can read leaves every row rendered" in {
        for
            rows <- Signal.initRef[Seq[Item]](hundred)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .scrollHeight("40vh").scrollRows(40).render
            text  <- cards(ui)
            names <- bodyNames(ui)
        yield
            assert(text.contains("scrollRows") && text.contains("40vh"), "the length it could not read is named")
            assert(names.size == 100, "and the table renders every row rather than a wrong window")
    }

    "rows of their own between the data rows leave every row rendered" in {
        for
            rows <- Signal.initRef[Seq[Item]](hundred)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .groupBy(uic.RowGroup((i: Item) => if i.price > 50 then "high" else "low"))
                .scrollHeight("200px").scrollRows(40).render
            text <- cards(ui)
            trs  <- bodyTrs(ui)
        yield
            assert(text.contains("row grouping"), "the level that renders rows of its own is named")
            val data = trs.count(t => t.attrs.cssClasses.contains("p-row-even") || t.attrs.cssClasses.contains("p-row-odd"))
            assert(data == 100, "and the table renders every row")
            assert(!trs.exists(_.attrs.cssClasses.contains("p-datatable-virtualscroller-spacer")), "with no window")
    }

    "a page size a windowed table will not read is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](hundred)
            page <- Signal.initRef(0)
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(10)(page).scrollHeight("200px").scrollRows(40).render)
        yield assert(text.contains("scrolls instead of paginating") && text.contains("10 rows"))
    }

    "a windowed table does not navigate, and says so" in {
        for
            rows <- Signal.initRef[Seq[Item]](hundred)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
            ).scrollHeight("200px").scrollRows(40).render
            text <- cards(ui)
            all  <- elements(ui)
        yield
            assert(text.contains("navigation is off"))
            assert(!all.exists(_.attrs.cssClasses.contains("p-uic-dt-nav")), "and the cursor's own class is gone")
    }

    // ---- rows that hold under the header ----

    private def groups(node: UI)(using Frame): List[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-tbody")).toList)

    private def namesIn(e: UI.Ast.Element)(using Frame): List[String] < Sync =
        rowsUnder(e).map(_.toList.flatMap(_.children.collect { case c: UI.Ast.Element => c }.headOption)
            .flatMap(_.children.collect { case t: UI.Ast.Text => t.value }))

    private val pinned = List(Item("9", "Total", 30))

    "frozen rows render in their own group above the body, and hold at the header's height" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            err  <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            top  <- Signal.initRef(Absent: Maybe[Int])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .scrollHeight("200px").frozenRows(pinned)
                .wired("t", Map.empty, err, _ => (), headTop = Present(top))
            before <- groups(ui)
            atRest <- namesIn(before.head)
            _      <- top.set(Present(41))
            after  <- groups(ui)
        yield
            assert(before.size == 2, "the pinned rows are a row group of their own")
            assert(before.head.attrs.cssClasses.contains("p-datatable-frozen-tbody"), "and it comes first")
            assert(
                before.head.attrs.cssClasses.contains("p-datatable-tbody"),
                "carrying the body class too, or the cells lose their styling"
            )
            assert(atRest == List("Total"), "holding the rows it was given")
            assert(heightOf(before.head).isEmpty, "before the header is measured it sits in flow")
            assert(
                after.head.attrs.uiStyle.props.collect { case Style.Prop.Top(v) => v } == List(41.px),
                "and once it is measured, it holds at that height"
            )
    }

    "the body rows keep their own group, and the cursor does not step into the frozen one" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            err  <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            top  <- Signal.initRef(Absent: Maybe[Int])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
            ).scrollHeight("200px").frozenRows(pinned)
                .wired("t", Map.empty, err, _ => (), headTop = Present(top))
            gs   <- groups(ui)
            held <- namesIn(gs.head)
            body <- namesIn(gs(1))
            all  <- elements(ui)
        yield
            assert(held == List("Total") && body == List("A", "B"), "each group holds its own rows")
            val stops = all.filter(_.attrs.tabIndex.contains(0)).flatMap(_.attrs.identifier)
            assert(stops == Chunk("t-c0-0", "t-c1-0"), "only the scrolling rows are in the keyboard grid")
    }

    "a frozen row that is in the body as well is reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .scrollHeight("200px").frozenRows(List(items.head)).render)
        yield
            assert(text.contains("in the body as well"))
            assert(text.contains("1"), "and the key that is in both is named")
    }

    "frozen rows with nothing to hold against are reported" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .frozenRows(pinned).render)
        yield assert(text.contains("scroll container") && text.contains("scrollHeight"))
    }

    // ---- rows the caller keeps out of the selection ----

    private def checkboxIn(row: UI.Ast.Element)(using Frame): UI.Ast.Element < Sync =
        elements(row).map(_.find(_.attrs.cssClasses.contains("p-checkbox")).getOrElse(
            throw new AssertionError("the row renders no checkbox")
        ))

    "a click that passed through a control in the row is that control's, not the row's" in {
        // Following a link in a cell, pressing the expander, starting a row edit: each is a click the
        // reader aimed at something, and a row that also selected on it would be answering a question
        // nobody asked. The control cannot decline the click for the row — one that navigates natively
        // declares no kyo handler, so it has no `stopPropagation` to set.
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).render
            trs        <- bodyTrs(ui)
            _          <- clickViaControl(trs.head)
            viaControl <- sel.get
            _          <- click(trs.head)
            plain      <- sel.get
        yield
            assert(viaControl.isEmpty, "the control's click does not also select")
            assert(plain == Set("1"), "and a click on the row itself still does")
    }

    "the keyboard path is the row's, whatever the row contains" in {
        // `onControl` answers where a POINTER went; Enter on the row is the row's, and the row is
        // what holds the tab stop.
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).render
            trs    <- bodyTrs(ui)
            _      <- press(trs.head, UI.Keyboard.Enter)
            picked <- sel.get
        yield assert(picked == Set("1"))
    }

    "a row the predicate rejects stays out of the selection when it is clicked" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel)
                .selectableWhen(_.id != "2").render
            trs <- bodyTrs(ui)
            _   <- click(trs(1))
            no  <- sel.get
            _   <- click(trs.head)
            yes <- sel.get
        yield
            assert(no == Set.empty, "the rejected row writes nothing")
            assert(yes == Set("1"), "and the accepted one still selects")
            assert(
                !trs(1).attrs.cssClasses.contains("p-datatable-selectable-row"),
                "a row that cannot be picked does not offer the pointer that says it can"
            )
            assert(trs.head.attrs.cssClasses.contains("p-datatable-selectable-row"))
    }

    "select-all passes over the rows the predicate rejects" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selected(sel)
                .selectableWhen(_.id != "2").render
            box <- elements(ui).map(_.collect { case c: UI.Ast.Checkbox => c }.head)
            _ <- box.onChange match
                case Present(f) => f(true)
                case Absent     => throw new AssertionError("the header box declares no change handler")
            all <- sel.get
        yield assert(all == Set("1"), "the header box selects what may be selected and nothing else")
    }

    "the checkbox of a rejected row is disabled and inert" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selected(sel)
                .selectableWhen(_.id != "2").render
            trs <- bodyTrs(ui)
            ok  <- checkboxIn(trs.head)
            no  <- checkboxIn(trs(1))
        yield
            assert(ok.attrs.onClick.isDefined && !ok.attrs.cssClasses.contains("p-disabled"))
            assert(no.attrs.onClick.isEmpty, "there is no handler to reach, not a handler that refuses")
            assert(no.attrs.cssClasses.contains("p-disabled"), "and it says so")
    }

    // ---- selected: the three bindings the union carries ----

    "a constant selection paints, and nothing can move it" in {
        for
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(Set("2")).onRowClick(_ => ()).render
            trs <- bodyTrs(ui)
            _   <- click(trs.head)
            // Re-rendered from the same constant: a click had nowhere to go, so nothing changed.
            after <- bodyTrs(ui)
        yield
            assert(after(1).attrs.cssClasses.contains("p-datatable-row-selected"))
            assert(!after.head.attrs.cssClasses.contains("p-datatable-row-selected"))
            assert(after(1).attrs.ariaAttrs.get("selected").contains("true"))
    }

    "a one-way selection follows its source and is never written back" in {
        // The shape a caller owning the state elsewhere binds: the table paints what the signal
        // says, the click goes out through onRowClick, and the new value comes back in through the
        // signal. One circle, one direction.
        for
            src    <- Signal.initRef(Set("1"))
            clicks <- Signal.initRef(List.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple)
                .selected(src.map(identity))
                .onRowClick(k => clicks.getAndUpdate(_ :+ k).unit).render
            trs   <- bodyTrs(ui)
            _     <- click(trs(1))
            after <- src.get
            heard <- clicks.get
            _     <- src.set(Set("2"))
            moved <- bodyTrs(ui)
        yield
            assert(after == Set("1"), "the table did not write to a signal it was only given to read")
            assert(heard == List("2"), "and the click still reached the caller")
            assert(moved(1).attrs.cssClasses.contains("p-datatable-row-selected"))
            assert(!moved.head.attrs.cssClasses.contains("p-datatable-row-selected"))
    }

    "a SignalRef still binds two ways" in {
        // The regression nail: the union dispatches on the RUNTIME class, and ReactiveVariable
        // IS-A Dyn, so the wrong match order would silently downgrade every existing caller.
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).render
            trs    <- bodyTrs(ui)
            _      <- click(trs(1))
            picked <- sel.get
        yield assert(picked == Set("2"))
    }

    "a one-way selection still reports what it holds, and restore leaves it alone" in {
        for
            src <- Signal.initRef(Set("1", "2"))
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(src.map(identity)).onRowClick(_ => ())
            st           <- table.state
            _            <- table.restore(uic.TableState(selected = Set("9")))
            afterRestore <- src.get
        yield
            assert(st.selected == Set("1", "2"), "reading needs no write access")
            assert(afterRestore == Set("1", "2"), "and restoring has nowhere to write")
    }

    "a right-click over a one-way selection is still told what is selected" in {
        for
            src  <- Signal.initRef(Set("1"))
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(src.map(identity))
                .onRowClick(_ => ())
                .onRowContextMenu(t => seen.set(Present(t))).render
            trs <- bodyTrs(ui)
            _ <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            told <- seen.get
        yield assert(told == Present(uic.RowContext(items(1), List(items.head))))
    }

    "a checkbox column over a one-way selection has nothing to write, and says so" in {
        for
            src <- Signal.initRef(Set.empty[String])
            text <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selected(src.map(identity)).render)
        yield assert(text.contains("checkbox column has nothing to write"))
    }

    "a one-way selection with no onRowClick cannot move at all, and says so" in {
        for
            src <- Signal.initRef(Set.empty[String])
            inert <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(src.map(identity)).render)
            wired <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(src.map(identity))
                .onRowClick(_ => ()).render)
            twoWay <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(src).render)
        yield
            assert(inert.contains("a row click has nowhere to go"))
            assert(!wired.contains("a row click has nowhere to go"), "an onRowClick is the outlet")
            assert(!twoWay.contains("a row click has nowhere to go"), "and so is a writable ref")
    }

    // ---- metaKeySelection: the modifier ruleset, and the range it brings ----

    /** A range needs more than the two rows the rest of this file gets by with. */
    private val four =
        List(Item("1", "A", 10), Item("2", "B", 20), Item("3", "C", 30), Item("4", "D", 40))

    /** A table with modifier selection, through the mount seam it needs for its anchor. */
    private def modifierTable(sel: SignalRef[Set[String]], rows: Seq[Item] = four)(using Frame) =
        for
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            anchor <- Signal.initRef(Absent: Maybe[String])
        yield
            val t = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).metaKeySelection(true)
            (t.wired("t", Map.empty, err, _ => (), anchor = Present(anchor)), anchor)

    private def clickWith(el: UI.Ast.Element, mods: UI.Modifiers)(using Frame): Any < Async =
        el.attrs.onClickEvt match
            case Present(f) => f(UI.MouseEvent(el.attrs.identifier, mods))
            case Absent     => throw new AssertionError("the row declares no typed click handler")

    "with metaKeySelection a plain click replaces the selection instead of adding to it" in {
        for
            sel     <- Signal.initRef(Set.empty[String])
            (ui, _) <- modifierTable(sel)
            trs     <- bodyTrs(ui)
            _       <- click(trs.head)
            first   <- sel.get
            _       <- click(trs(1))
            second  <- sel.get
            // Prime: a plain click on an ALREADY picked row collapses to it rather than clearing,
            // which is what keeps "click, then shift-click" a range every time.
            _     <- click(trs(1))
            again <- sel.get
        yield
            assert(first == Set("1"))
            assert(second == Set("2"), "the second click replaced rather than added")
            assert(again == Set("2"), "and a plain click never clears the row it lands on")
    }

    "Ctrl or Cmd toggles the row into and out of the selection" in {
        for
            sel      <- Signal.initRef(Set.empty[String])
            (ui, _)  <- modifierTable(sel)
            trs      <- bodyTrs(ui)
            _        <- click(trs.head)
            _        <- clickWith(trs(1), UI.Modifiers(ctrl = true))
            withCtrl <- sel.get
            _        <- clickWith(trs(2), UI.Modifiers(meta = true))
            withMeta <- sel.get
            _        <- clickWith(trs(1), UI.Modifiers(meta = true))
            removed  <- sel.get
        yield
            assert(withCtrl == Set("1", "2"), "Ctrl adds")
            assert(withMeta == Set("1", "2", "3"), "and so does Cmd — it is meta OR ctrl, never one")
            assert(removed == Set("1", "3"), "a modified click on a picked row takes it out")
    }

    "Shift selects the range from the last row picked without it" in {
        for
            sel     <- Signal.initRef(Set.empty[String])
            (ui, _) <- modifierTable(sel)
            trs     <- bodyTrs(ui)
            _       <- click(trs.head)
            _       <- clickWith(trs(2), UI.Modifiers(shift = true))
            down    <- sel.get
            // The anchor does NOT move with a shift-click, so narrowing the range is the same
            // gesture repeated rather than a walk back.
            _      <- clickWith(trs(1), UI.Modifiers(shift = true))
            narrow <- sel.get
        yield
            assert(down == Set("1", "2", "3"), "inclusive, in the reader's order")
            assert(narrow == Set("1", "2"), "and re-measured from the same anchor")
    }

    "a range runs upwards as readily as down, and an anchorless shift picks one row" in {
        for
            sel      <- Signal.initRef(Set.empty[String])
            (ui, _)  <- modifierTable(sel)
            trs      <- bodyTrs(ui)
            _        <- clickWith(trs(1), UI.Modifiers(shift = true))
            noAnchor <- sel.get
            _        <- click(trs(2))
            _        <- clickWith(trs.head, UI.Modifiers(shift = true))
            up       <- sel.get
        yield
            assert(noAnchor == Set("2"), "the first shift-click is just a pick, and sets the anchor")
            assert(up == Set("1", "2", "3"))
    }

    "a range steps over the rows the predicate rejects" in {
        for
            sel    <- Signal.initRef(Set.empty[String])
            err    <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            anchor <- Signal.initRef(Absent: Maybe[String])
            ui = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).metaKeySelection(true)
                .selectableWhen(_.id != "2")
                .wired("t", Map.empty, err, _ => (), anchor = Present(anchor))
            trs    <- bodyTrs(ui)
            _      <- click(trs.head)
            _      <- clickWith(trs(2), UI.Modifiers(shift = true))
            picked <- sel.get
        yield assert(picked == Set("1", "3"), "a rejected row is not dragged in by a range")
    }

    "without metaKeySelection the modifiers mean nothing, and no anchor is owned" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).render
            trs   <- bodyTrs(ui)
            _     <- click(trs.head)
            _     <- clickWith(trs(1), UI.Modifiers(shift = true))
            after <- sel.get
        yield
            // The default table is not mounted at all, which is the whole reason the anchor is
            // tied to the opt-in: `bodyTrs` reaching real rows here proves there is no mount.
            assert(after == Set("1", "2"), "shift is just another click while the flag is off")
    }

    "a selection restriction over a table with no selection is reported" in {
        for
            text <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name)).selectableWhen(_.id != "2").render)
        yield assert(text.contains("selectableWhen") && text.contains("selectionMode"))
    }

    // ---- classes the caller puts on a row ----

    "a row carries the caller's classes beside the ones the table gives it" in {
        for
            sel <- Signal.initRef(Set("1"))
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selected(sel).stripedRows(true)
                .rowClasses(i => if i.price > 15 then Seq("dear", "flag") else Nil).render
            trs <- bodyTrs(ui)
        yield
            assert(!trs.head.attrs.cssClasses.contains("dear"), "the cheap row is given nothing")
            assert(trs(1).attrs.cssClasses.containsSlice(Seq("dear", "flag")), "both classes, in order")
            assert(
                trs.head.attrs.cssClasses.contains("p-datatable-row-selected") &&
                    trs.head.attrs.cssClasses.contains("p-row-even"),
                "and the table's own classes still say what they said"
            )
    }

    "an empty class name is not a class" in {
        val ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
            .rowClasses(_ => Seq("")).render
        for trs <- bodyTrs(ui)
        yield assert(trs.forall(!_.attrs.cssClasses.contains("")))
    }

    // ---- a viewport sized by its parent ----

    "flex scroll makes a scroll container with no height of its own" in {
        val flex  = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name)).flexScroll(true)
        val sized = flex.scrollHeight("200px")
        for
            root    <- elements(flex.render).map(_.head)
            capped  <- elements(sized.render).map(_.head)
            body    <- elementWithClass(flex.render, "p-datatable-table-container")
            reports <- cards(sized.render)
        yield
            assert(root.attrs.cssClasses.containsSlice(Seq("p-datatable-scrollable", "p-datatable-flex-scrollable")))
            assert(body.attrs.uiStyle.props.isEmpty, "the height comes from the parent, not from a style here")
            assert(
                !capped.attrs.cssClasses.contains("p-datatable-flex-scrollable"),
                "a stated length is the one the table can read, so it is the one that wins"
            )
            assert(reports.contains("flexScroll") && reports.contains("200px"), "and the other is reported")
        end for
    }

    "frozen rows hold against a flex viewport, and a windowed body cannot be measured against one" in {
        for
            reports <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .flexScroll(true).frozenRows(pinned).render)
            windowed <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .flexScroll(true).scrollRows(40).render)
        yield
            assert(reports.isEmpty, "a flex viewport is a scroll container, which is all a frozen row holds against")
            assert(windowed.contains("at layout time"), "and a window needs a number, which layout has not produced yet")
    }

    // ---- a drag that moves a column instead of a boundary ----

    /** The same three columns, each with a width of its own, resizing in `Expand`. */
    private def expanding(mode: uic.ColumnResizeMode, lastWidth: Maybe[Double] = Present(100.0))(using Frame) =
        for
            cols <- Signal.initRef(Map.empty[List[String], Double])
            err  <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            grab <- Signal.initRef(Absent: Maybe[ColumnGrab])
        yield
            val base = uic.DataTable[Item]().rows(items).rowKey(_.id)
            val withCols = lastWidth match
                case Present(w) => base.columns(
                        uic.column("Name")(_.name).width(100),
                        uic.column("Price")(_.price.toString).width(100),
                        uic.column("Id")(_.id).width(w)
                    )
                case Absent => base.columns(
                        uic.column("Name")(_.name).width(100),
                        uic.column("Price")(_.price.toString).width(100),
                        uic.column("Id")(_.id)
                    )
            val table = withCols.columnWidths(cols).columnResizeMode(mode)
            val measure = (ids: Seq[String]) =>
                Chunk.from(ids.map(id =>
                    UI.Rect(0, 0, Map("t-h0" -> 100.0, "t-h1" -> 100.0, "t-h2" -> 100.0).getOrElse(id, 0.0), 30, 1000, 800)
                )): Chunk[UI.Rect] < Async
            (table.wired("t", Map.empty, err, _ => (), Map.empty, measure, Present(grab)), cols)
        end for
    end expanding

    "an expanding drag writes the column it grabbed and leaves the others alone" in {
        for
            (ui, cols) <- expanding(uic.ColumnResizeMode.Expand)
            handles    <- resizers(ui)
            _          <- drag(handles.head, 200, 240)
            widths     <- cols.get
        yield assert(widths == Map(List("Name") -> 140.0), "one column moved, and nothing gave the width back")
    }

    "an expanding drag stops at the same floor a fitting one does" in {
        for
            (ui, cols) <- expanding(uic.ColumnResizeMode.Expand)
            handles    <- resizers(ui)
            _          <- drag(handles.head, 200, 0)
            widths     <- cols.get
        yield assert(widths == Map(List("Name") -> 15.0))
    }

    "the last column keeps its handle where it has nothing to trade with" in {
        for
            (expand, _) <- expanding(uic.ColumnResizeMode.Expand)
            (fit, _)    <- expanding(uic.ColumnResizeMode.Fit)
            wide        <- resizers(expand)
            paired      <- resizers(fit)
            tbl         <- elementWithClass(expand, "p-datatable-resizable-table")
            fitted      <- elementWithClass(fit, "p-datatable-resizable-table")
            root        <- elements(expand).map(_.head)
        yield
            assert(wide.size == 3, "every resizable column, the last one included")
            assert(paired.size == 2, "against the two boundaries fitting has")
            assert(!tbl.attrs.cssClasses.contains("p-datatable-resizable-table-fit"))
            assert(fitted.attrs.cssClasses.contains("p-datatable-resizable-table-fit"))
            assert(
                tbl.attrs.uiStyle.props.collect { case Style.Prop.Width(v) => v } == Seq(300.0.px),
                "and the table states the width its columns add up to"
            )
            assert(fitted.attrs.uiStyle.props.collect { case Style.Prop.Width(v) => v }.isEmpty)
            assert(root.attrs.cssClasses.contains("p-datatable-scrollable"), "a table that outgrows its container scrolls")
    }

    "a column with no width sends an expanding table back to fitting" in {
        for
            (ui, _)  <- expanding(uic.ColumnResizeMode.Expand, lastWidth = Absent)
            handles  <- resizers(ui)
            tbl      <- elementWithClass(ui, "p-datatable-resizable-table")
            reported <- cards(ui)
        yield
            assert(handles.size == 2, "the boundaries fitting has, and no handle on the last column")
            assert(tbl.attrs.cssClasses.contains("p-datatable-resizable-table-fit"))
            assert(tbl.attrs.uiStyle.props.collect { case Style.Prop.Width(v) => v }.isEmpty, "no width it cannot add up")
            assert(reported.contains("every column") && reported.contains("Id"), "and the column with none is named")
    }

    // ---- rows the reader drags to another place ----

    private val trio = List(Item("1", "A", 10), Item("2", "B", 20), Item("3", "C", 30))

    private def pointerAtY(y: Double): UI.PointerEvent = UI.PointerEvent(0, y, 0, 0, 200, 40, 1, Absent)

    /** Three rows forty pixels tall, stacked from the top of the viewport, with a place to
      * park the drag and a record of what a completed one handed over.
      */
    private def draggableRows(build: uic.DataTable[Item] => uic.DataTable[Item] = identity)(using Frame) =
        for
            rows  <- Signal.initRef[Seq[Item]](trio)
            err   <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            drag  <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            moves <- Signal.initRef(List.empty[(Int, Int, List[String])])
            table = build(
                uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true)
                    .onRowReorder(m => moves.getAndUpdate(_ :+ (m.from, m.to, m.rows.map(_.name).toList)))
            )
            measureAll = (ids: Seq[String]) =>
                Chunk.from(ids.zipWithIndex.map((_, i) => UI.Rect(0, i * 40, 200, 40, 1000, 800))): Chunk[UI.Rect] < Async
        yield
            val ui = table.wired(
                "t",
                Map.empty,
                err,
                _ => (),
                Map.empty,
                measureAll,
                Absent,
                Absent,
                Absent,
                Absent,
                Present(drag)
            )
            (ui, rows, moves, drag)
        end for
    end draggableRows

    private def grips(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-reorderable-row-handle")))

    private def dragRow(grip: UI.Ast.Element, from: Double, to: Double)(using Frame): Any < Async =
        (grip.attrs.onPointerDown, grip.attrs.onPointerMove, grip.attrs.onPointerUp) match
            case (Present(down), Present(move), Present(up)) =>
                down(pointerAtY(from)).andThen(move(pointerAtY(to))).andThen(up(pointerAtY(to)))
            case _ => throw new AssertionError("the grip declares no drag")

    "a completed row drag writes the list and hands it over, indexed in the list" in {
        for
            (ui, rows, moves, _) <- draggableRows()
            handles              <- grips(ui)
            _                    <- dragRow(handles.head, 10, 95)
            after                <- rows.get
            fired                <- moves.get
        yield
            assert(after.map(_.name) == Seq("B", "A", "C"), "the first row landed after the second")
            assert(fired == List((0, 1, List("B", "A", "C"))), "and the event says where it came from and went")
    }

    "a drop where the row already was writes nothing" in {
        for
            (ui, rows, moves, _) <- draggableRows()
            handles              <- grips(ui)
            // The boundary just below the grabbed row is the row's own place.
            _     <- dragRow(handles.head, 10, 44)
            after <- rows.get
            fired <- moves.get
        yield
            assert(after.map(_.name) == Seq("A", "B", "C"))
            assert(fired.isEmpty, "and nothing is reported as a change")
    }

    "the line shows on the row a drop would land beside" in {
        for
            (ui, _, _, drag) <- draggableRows()
            handles          <- grips(ui)
            down = handles.head.attrs.onPointerDown.getOrElse(throw new AssertionError("no grab"))
            move = handles.head.attrs.onPointerMove.getOrElse(throw new AssertionError("no drag"))
            _    <- down(pointerAtY(10))
            _    <- move(pointerAtY(78))
            held <- drag.get
            trs  <- bodyTrs(ui)
        yield
            assert(held.map(_.target) == Present(2), "the nearest boundary is the one below the second row")
            assert(trs.head.attrs.cssClasses.contains("p-uic-dt-dragging"), "the grabbed row says it is travelling")
            assert(trs(2).attrs.cssClasses.contains("p-datatable-dragpoint-top"), "and the line sits above the third")
            assert(trs.forall(!_.attrs.cssClasses.contains("p-datatable-dragpoint-bottom")))
    }

    "a press that never travels moves nothing" in {
        for
            (ui, rows, moves, drag) <- draggableRows()
            handles                 <- grips(ui)
            _                       <- dragRow(handles.head, 10, 12)
            after                   <- rows.get
            fired                   <- moves.get
            held                    <- drag.get
        yield
            assert(after.map(_.name) == Seq("A", "B", "C") && fired.isEmpty)
            assert(held == Absent, "and the drag is cleared rather than parked")
    }

    "an order the list does not hold takes the grips away and says which" in {
        for
            sort          <- Signal.initRef(List(uic.SortKey.ascending("Name")))
            (ui, _, _, _) <- draggableRows(_.sort(sort))
            handles       <- grips(ui)
            reported      <- cards(ui)
        yield
            assert(handles.forall(_.attrs.onPointerDown.isEmpty), "the column renders, the drag does not")
            assert(handles.size == 3, "the anatomy is the same either way")
            assert(reported.contains("a sort spec"))
    }

    "reorderable rows with nowhere to write are reported" in {
        for
            reported <- cards(uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(uic.column("Name")(_.name))
                .reorderableRows(true).render)
        yield assert(reported.contains("rows(ref)") && reported.contains("onRowReorder"))
    }

    // ---- the row a context menu was opened on ----

    "a right-click marks the row it landed on and tells the caller, without selecting it" in {
        for
            sel  <- Signal.initRef(Set.empty[String])
            ctx  <- Signal.initRef(Absent: Maybe[String])
            seen <- Signal.initRef(List.empty[Item])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel)
                .contextMenuRow(ctx).onRowContextMenu(t => seen.getAndUpdate(_ :+ t.row))
            trs <- bodyTrs(table.render)
            _ <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            on     <- ctx.get
            picked <- sel.get
            told   <- seen.get
            after  <- bodyTrs(table.render)
        yield
            assert(on == Present("2") && told == List(items(1)))
            assert(picked.isEmpty, "acting on a row is not selecting it")
            assert(after(1).attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"))
            assert(!after.head.attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"))
    }

    "the right-click hands over the selected ROWS beside the one it landed on" in {
        for
            sel  <- Signal.initRef(Set("1"))
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel)
                .onRowContextMenu(t => seen.set(Present(t)))
            trs <- bodyTrs(table.render)
            // Row 2 is NOT in the selection: the pair is what lets a menu tell the two cases apart.
            _ <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            outside <- seen.get
            _       <- sel.set(Set("1", "2"))
            _ <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            inside <- seen.get
        yield
            assert(outside == Present(uic.RowContext(items(1), List(items.head))))
            assert(!outside.exists(t => t.selected.contains(t.row)), "a right-click outside the selection")
            assert(inside == Present(uic.RowContext(items(1), items)))
            assert(inside.exists(t => t.selected.contains(t.row)), "a right-click inside it")
    }

    "the selection handed over is the rows, in the table's own order, filtering aside" in {
        for
            sel  <- Signal.initRef(Set("2", "1"))
            qry  <- Signal.initRef("B")
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selected(sel).globalFilter(qry)
                .onRowContextMenu(t => seen.set(Present(t)))
            trs <- bodyTrs(table.render)
            _ <- trs.head.attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            told <- seen.get
        yield
            // One row is on the screen, both are selected: the menu is asked about the selection,
            // not about the page.
            assert(trs.size == 1)
            assert(told == Present(uic.RowContext(items(1), items)))
    }

    "a table with no context binding leaves the browser's own menu alone" in {
        for trs <- bodyTrs(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name)).render)
        yield assert(trs.forall(_.attrs.onContextMenu.isEmpty))
    }

    // ---- the table as data ----

    "csv writes a heading row and one line per row, from the columns that export" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name),
            uic.column("Price")(_.price.toString).exportHeader("Price (EUR)"),
            uic.column("Id")(_.id).exportable(false)
        )
        assert(table.csv(items) == "Name,Price (EUR)\nA,10\nB,20")
    }

    "a column with no text of its own exports what exportAs gives it" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).exportAs(i => i.name.toLowerCase),
            uic.column("Tag").body(i => UI.span(i.name)).exportAs(_.price.toString),
            uic.column("Blank").body(i => UI.span(i.name))
        )
        assert(table.csv(items.take(1)) == "Name,Tag,Blank\na,10,")
    }

    "a field that holds the separator, a quote or a line break is quoted" in {
        assert(uic.DataTable.csvField("plain", ",") == "plain")
        assert(uic.DataTable.csvField("a,b", ",") == "\"a,b\"")
        assert(uic.DataTable.csvField("say \"hi\"", ",") == "\"say \"\"hi\"\"\"")
        assert(uic.DataTable.csvField("two\nlines", ",") == "\"two\nlines\"")
        assert(uic.DataTable.csvField("a,b", ";") == "a,b", "the separator is what matters, not the comma")
    }

    "the effectful csv exports what the reader is looking at, across every page" in {
        for
            sort  <- Signal.initRef(List(uic.SortKey("Price", uic.SortDirection.Descending)))
            query <- Signal.initRef("")
            page  <- Signal.initRef(0)
            table = uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString).sortBy(_.price)
            ).sort(sort).globalFilter(query).paginate(1)(page)
            all <- table.csv
            _   <- query.set("B")
            one <- table.csv
        yield
            assert(all == "Name,Price\nC,30\nB,20\nA,10", "sorted as rendered, and not sliced to the page")
            assert(one == "Name,Price\nB,20", "and the global filter narrows it the same way")
    }

    "csv writes the columns in the order the reader put them in" in {
        for
            order <- Signal.initRef(List(List("Price"), List("Name")))
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).columnOrder(order)
            out <- table.csv
        yield assert(out == "Price,Name\n10,A\n20,B")
    }

    "a separator of its own joins the fields, and is what decides the quoting" in {
        def table(sep: String) = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).exportAs(i => s"${i.name},x"),
            uic.column("Price")(_.price.toString)
        ).csvSeparator(sep)
        assert(table(",").csv(items.take(1)) == "Name,Price\n\"A,x\",10")
        assert(
            table(";").csv(items.take(1)) == "Name;Price\nA,x;10",
            "a comma is just a character once the separator is a semicolon"
        )
    }

    // ---- bringing a reader back where they were ----

    "the table's state reads every bound ref, and restoring writes them back" in {
        for
            sort   <- Signal.initRef(List.empty[uic.SortKey])
            query  <- Signal.initRef("")
            page   <- Signal.initRef(0)
            sel    <- Signal.initRef(Set.empty[String])
            widths <- Signal.initRef(Map.empty[List[String], Double])
            table = uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(
                uic.column("Name")(_.name).sortBy(_.name),
                uic.column("Price")(_.price.toString)
            ).sort(sort).globalFilter(query).paginate(2)(page)
                .selectionMode(SelectionMode.Multiple).selected(sel).columnWidths(widths)
            fresh <- table.state
            _     <- sort.set(List(uic.SortKey.ascending("Name")))
            _     <- query.set("b")
            _     <- page.set(1)
            _     <- sel.set(Set("2"))
            _     <- widths.set(Map(List("Name") -> 240.0))
            saved <- table.state
            _     <- table.restore(fresh)
            back  <- table.state
        yield
            assert(fresh == uic.TableState(), "an untouched table is the default state")
            assert(saved.sort == List(uic.SortKey.ascending("Name")))
            assert(saved.globalFilter == "b" && saved.page == 1 && saved.selected == Set("2"))
            assert(saved.columnWidths == Map(List("Name") -> 240.0))
            assert(back == fresh, "and restoring puts every one of them back")
    }

    "a state field whose ref is not bound reads as its default and is not restored" in {
        for
            sort <- Signal.initRef(List(uic.SortKey.ascending("Name")))
            table = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name).sortBy(_.name)).sort(sort)
            saved <- table.state
            _     <- table.restore(saved.copy(page = 7, selected = Set("1"), globalFilter = "x"))
            after <- table.state
        yield
            assert(saved.page == 0 && saved.selected.isEmpty, "nothing bound, nothing read")
            assert(after == saved, "and nothing written, since there is nowhere for it to go")
            assert(after.sort == List(uic.SortKey.ascending("Name")), "the one ref that is bound survives")
    }

    // ---- picking cells instead of rows ----

    private def cellsIn(node: UI)(using Frame): List[UI.Ast.Element] < Sync =
        bodyTrs(node).flatMap(trs => Kyo.foreach(trs)(rowsUnder).map(_.flatten.toList))

    "a click picks the cell it landed on, and Multiple keeps the ones before it" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).selectionMode(SelectionMode.Multiple).selectedCells(picked)
            tds   <- cellsIn(table.render)
            _     <- click(tds.head)
            _     <- click(tds(3))
            both  <- picked.get
            _     <- click(tds.head)
            one   <- picked.get
            after <- cellsIn(table.render)
        yield
            assert(both == Set(CellPath("1", List("Name")), CellPath("2", List("Price"))))
            assert(one == Set(CellPath("2", List("Price"))), "a second click on a picked cell takes it back")
            assert(after(3).attrs.cssClasses.contains("p-uic-dt-cell-selected"))
            assert(!after.head.attrs.cssClasses.contains("p-uic-dt-cell-selected"))
            assert(after.head.attrs.ariaAttrs.get("selected").contains("false"))
    }

    "Single replaces the set rather than adding to it" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).selectionMode(SelectionMode.Single).selectedCells(picked)
            tds  <- cellsIn(table.render)
            _    <- click(tds.head)
            _    <- click(tds(3))
            only <- picked.get
        yield assert(only == Set(CellPath("2", List("Price"))))
    }

    "a cell of a row the predicate rejects cannot be picked" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).selectableWhen(_.id != "2")
            tds <- cellsIn(table.render)
        yield
            assert(tds.head.attrs.onClick.isDefined)
            assert(tds(1).attrs.onClick.isEmpty, "no handler to reach, not a handler that refuses")
    }

    "a click that two bindings both claim is reported, and editing keeps it" in {
        for
            picked  <- Signal.initRef(Set.empty[CellPath])
            editing <- Signal.initRef(Absent: Maybe[CellPath])
            rows    <- Signal.initRef(Set.empty[String])
            clash <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).editingCell(editing)
                .onCellValueChanged(_ => ()).render)
            withRows <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).selected(rows).render)
            modeless <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selectedCells(picked).render)
        yield
            assert(clash.contains("editingCell") && clash.contains("selectedCells"))
            assert(withRows.contains("selected, which picks rows"))
            assert(modeless.contains("Single or Multiple") && modeless.contains("Checkbox"))
    }

    // ---- several conditions on one column ----

    /** A menu-display table over the three items, with the panel's open state and its
      * draft owned here so a test can open it and read what it holds.
      */
    private def filterMenu(using Frame) =
        for
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            err     <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            open    <- Signal.initRef(false)
            draft   <- Signal.initRef(ColumnFilter.empty(MatchMode.Contains))
        yield
            val table = uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(
                uic.column("Name")(_.name).filterBy
            ).columnFilters(filters).filterDisplay(uic.FilterDisplay.Menu)
            def ui = table.wired(
                "t",
                Map.empty,
                err,
                _ => (),
                Map(List("Name") -> open),
                filterDrafts = Map(List("Name") -> draft)
            )
            (ui, filters, open, draft)
        end for
    end filterMenu

    private def buttonWith(node: UI, cls: String)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(node, cls)

    /** All the text an element holds, however deeply, which is how a button is found by
      * its label rather than by a class the sheet does not give it.
      */
    private def textOf(e: UI.Ast.Element)(using Frame): String =
        def walk(u: UI): String = u match
            case t: UI.Ast.Text     => t.value
            case el: UI.Ast.Element => el.children.map(walk).mkString
            case _                  => ""
        e.children.map(walk).mkString
    end textOf

    "the funnel is in the header cell and there is no filter row" in {
        for
            (ui, _, _, _) <- filterMenu
            all           <- elements(ui)
        yield
            assert(all.exists(_.attrs.cssClasses.contains("p-datatable-popover-filter")))
            assert(!all.exists(_.attrs.cssClasses.contains("p-datatable-inline-filter")), "one display or the other")
    }

    "opening the menu seeds the draft from the filter the table is running" in {
        for
            (ui, filters, open, draft) <- filterMenu
            _                          <- filters.set(Map(List("Name") -> ColumnFilter("B", MatchMode.StartsWith)))
            funnel                     <- buttonWith(ui, "p-datatable-column-filter-button")
            _                          <- click(funnel)
            seeded                     <- draft.get
            isOpen                     <- open.get
        yield
            assert(isOpen, "the click opens it")
            assert(seeded == ColumnFilter("B", MatchMode.StartsWith), "on what is applied, not on what was abandoned")
    }

    "typing in the menu narrows nothing until Apply, and then narrows by every rule" in {
        for
            (ui, filters, open, draft) <- filterMenu
            _                          <- open.set(true)
            _ <- draft.set(ColumnFilter(
                List(FilterRule("A", MatchMode.Contains), FilterRule("B", MatchMode.Contains)),
                uic.FilterOperator.Or
            ))
            beforeApply <- filters.get
            shown       <- bodyNames(ui)
            apply       <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-button")))
            _           <- click(apply.find(b => textOf(b).contains("Apply")).getOrElse(throw new AssertionError("no Apply")))
            after       <- filters.get
            closed      <- open.get
            names       <- bodyNames(ui)
        yield
            assert(beforeApply.isEmpty, "the draft is not the filter")
            assert(shown.toList == List("A", "B", "C"), "and the table is still showing everything")
            assert(after(List("Name")).rules.sizeIs == 2 && after(List("Name")).operator == uic.FilterOperator.Or)
            assert(!closed, "applying closes the panel")
            assert(names.toList == List("A", "B"), "Match Any keeps a row either rule keeps")
    }

    "Match All keeps only the rows every rule keeps" in {
        for
            (ui, filters, _, _) <- filterMenu
            _ <- filters.set(Map(List("Name") -> ColumnFilter(
                List(FilterRule("A", MatchMode.Contains), FilterRule("B", MatchMode.Contains)),
                uic.FilterOperator.And
            )))
            trs <- bodyTrs(ui)
        yield assert(trs.sizeIs == 1 && trs.head.attrs.cssClasses.contains("p-datatable-empty-message"))
    }

    "Add Rule and Remove Rule move the draft, not the filter" in {
        for
            (ui, filters, open, draft) <- filterMenu
            _                          <- open.set(true)
            add                        <- buttonWith(ui, "p-datatable-filter-add-rule-button")
            _                          <- click(add)
            two                        <- draft.get
            remove                     <- buttonWith(ui, "p-datatable-filter-remove-rule-button")
            _                          <- click(remove)
            one                        <- draft.get
            untouched                  <- filters.get
        yield
            assert(two.rules.sizeIs == 2 && two.rules.forall(_.query.isEmpty))
            assert(one.rules.sizeIs == 1)
            assert(untouched.isEmpty, "neither button applies anything")
    }

    "Clear takes the column out of the filters and empties the draft" in {
        for
            (ui, filters, open, draft) <- filterMenu
            _                          <- filters.set(Map(List("Name") -> ColumnFilter("A", MatchMode.Contains)))
            _                          <- draft.set(ColumnFilter("A", MatchMode.Contains))
            _                          <- open.set(true)
            buttons                    <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-button")))
            _      <- click(buttons.find(b => textOf(b).contains("Clear")).getOrElse(throw new AssertionError("no Clear")))
            after  <- filters.get
            left   <- draft.get
            closed <- open.get
        yield
            assert(after.isEmpty && left == ColumnFilter("", MatchMode.Contains))
            assert(!closed)
    }

    "an applied draft that asks for nothing takes the column out rather than leaving an empty entry" in {
        for
            (ui, filters, open, draft) <- filterMenu
            _                          <- filters.set(Map(List("Name") -> ColumnFilter("A", MatchMode.Contains)))
            _                          <- open.set(true)
            _                          <- draft.set(ColumnFilter("   ", MatchMode.Contains))
            buttons                    <- elements(ui).map(_.filter(_.attrs.cssClasses.contains("p-button")))
            _     <- click(buttons.find(b => textOf(b).contains("Apply")).getOrElse(throw new AssertionError("no Apply")))
            after <- filters.get
        yield assert(after.isEmpty)
    }

    "a filter row given several conditions shows the first and says so" in {
        for
            filters <- Signal.initRef(Map(List("Name") -> ColumnFilter(
                List(FilterRule("A", MatchMode.Contains), FilterRule("B", MatchMode.Contains))
            )))
            err <- Signal.initRef(Absent: Maybe[(CellPath, FieldError)])
            ui = uic.DataTable[Item]().rows(trio).rowKey(_.id)
                .columns(uic.column("Name")(_.name).filterBy).columnFilters(filters)
                .wired("t", Map.empty, err, _ => ())
            reported <- cards(ui)
        yield assert(reported.contains("several conditions") && reported.contains("FilterDisplay.Menu"))
    }

    // ---- the tab stops a table makes itself ----
    //
    // A `th` and a `tr` are not controls, so a table that gives them a tab stop owes them the
    // keys too. Without these the reader could reach every sortable header and every selectable
    // row and operate none of them.

    "a sortable header sorts on Enter and on Space, and carries the modifiers over" in {
        for
            sort <- Signal.initRef(List.empty[uic.SortKey])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name).sortBy(_.name), uic.column("Price")(_.price.toString).sortBy(_.price))
                .sort(sort).render
            headers    <- elementsWithClass(ui, "p-datatable-sortable-column")
            _          <- press(headers(0), UI.Keyboard.Enter)
            afterEnter <- sort.get
            _          <- press(headers(0), UI.Keyboard.Space)
            afterSpace <- sort.get
            _          <- press(headers(1), UI.Keyboard.Enter, UI.Modifiers(ctrl = true))
            afterMulti <- sort.get
        yield
            assert(afterEnter.map(_.column) == List("Name"), "Enter sorts by that column")
            assert(afterSpace.head.direction != afterEnter.head.direction, "Space cycles it, as a second click would")
            assert(afterMulti.map(_.column) == List("Name", "Price"), "Ctrl+Enter adds one, as Ctrl+click does")
    }

    "a selectable row selects on Enter and on Space" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selected(sel).render
            rows       <- elementsWithClass(ui, "p-datatable-selectable-row")
            _          <- press(rows(0), UI.Keyboard.Enter)
            afterEnter <- sel.get
            _          <- press(rows(1), UI.Keyboard.Space)
            afterSpace <- sel.get
        yield
            assert(afterEnter == Set("1"), "Enter picks the row it was pressed on")
            assert(afterSpace == Set("1", "2"), "and Space picks the next one")
    }

    "a key that is not an activation leaves the selection alone" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selected(sel).render
            rows  <- elementsWithClass(ui, "p-datatable-selectable-row")
            _     <- press(rows(0), UI.Keyboard.ArrowDown)
            after <- sel.get
        yield assert(after.isEmpty)
    }

end DataTableTest
