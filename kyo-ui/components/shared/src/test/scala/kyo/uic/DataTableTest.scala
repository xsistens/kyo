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
        wireWith(uic.CellType.int.validate(kyo.uic.form.Validator.min(1)))

    /** [[wire]] with the Price column's cell type, rules included, chosen by the test. */
    private def wireWith(priceType: CellType[Int])(using Frame) =
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            name    <- Signal.initRef("")
            price   <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            focused <- Signal.initRef(List.empty[String])
        yield
            val table = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                uic.column("Price")(_.price.toString)
                    .editableAs(priceType)(_.price)((i, v) => i.copy(price = v))
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
            // The way the runtime dispatches an input event: the bound ref is written first, then onInput is told.
            els.collectFirst {
                case t: UI.Ast.TextInput if t.onInput.isDefined || refBound(t.value).isDefined =>
                    (s: String) => refBound(t.value).fold[UI.Action](())(_.set(s)).andThen(t.onInput.fold[UI.Action](())(f => f(s)))
            }.getOrElse(throw new AssertionError("the open editor writes nothing as it is typed into"))
        }.map(f => handled(f(text)))

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
            // The cell reopened on the row the first commit wrote, so that is the row it holds.
            assert(stillOpen == Present(CellPath(Item("1", "A", 42), List("Price"))), "the refused cell stayed open")
            assert(refused.exists((_, e) => e.code == "min"), "with the rule's own error on it")
    }

    // A commit is the cell's submit: a rule that waits for a blur in a form field runs here like any other.
    "a commit runs every rule of the cell type, whatever activation it waits for" in {
        import kyo.uic.form.{Activation, Validator}
        for
            runs                        <- AtomicInt.init(0)
            (ui, rows, _, _, _, err, _) <- wireWith(
                uic.CellType.int.validate(
                    Validator.async[Int](v =>
                        runs.incrementAndGet.map(_ => (if v > 100 then Present(FieldError("too-dear")) else Absent): Maybe[FieldError])
                    ).on(Activation.Blur) and Validator.min(1).on(Activation.Submit)
                )
            )
            cell     <- elementWithId(ui, "t-c0-1")
            _        <- press(cell, UI.Keyboard.Enter)
            open     <- elementWithId(ui, "t-c0-1")
            _        <- typeInto(open, "0")
            reopened <- elementWithId(ui, "t-c0-1")
            _        <- press(reopened, UI.Keyboard.Enter)
            refused  <- err.get
            n        <- runs.get
            stored   <- rows.get
        yield
            assert(n == 1, s"the rule on(Blur) ran at the commit: $n")
            assert(refused.exists((_, e) => e.code == "min"), s"and so did the one on(Submit), which refused: $refused")
            assert(stored.head.price == 10, "so nothing was written")
        end for
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
            assert(now == Present(CellPath(items(1), List("Name"))), "and the editor moved on")
    }

    "a click inside the open cell leaves what was typed in it" in {
        // A number field's spinner, or a click to place the caret, lands on the open cell too. It opened the cell
        // again and seeded it from the row, so every spinner step snapped back to the value the cell started with.
        for
            (ui, _, editing, _, price, _, _) <- wire
            cell                             <- elementWithId(ui, "t-c0-1")
            _                                <- press(cell, UI.Keyboard.Enter)
            open                             <- elementWithId(ui, "t-c0-1")
            _                                <- typeInto(open, "42")
            again                            <- elementWithId(ui, "t-c0-1")
            _                                <- click(again)
            draft                            <- price.get
            now                              <- editing.get
        yield
            assert(draft == "42", s"the draft kept what was typed: $draft")
            assert(now == Present(CellPath(items(0), List("Price"))), "and the cell is still the open one")
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
            assert(now == Present(CellPath(items(0), List("Price"))), "the refusing cell kept the editor")
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
            assert(now == Present(CellPath(items(0), List("Price"))), "the editor stayed on the value being fixed")
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
            assert(open == Present(CellPath(items(0), List("Name"))))
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
            assert(open == Present(CellPath(items(1), List("Name"))))
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
            assert(open == Present(CellPath(items(0), List("Price"))), "the cell stayed open")
            assert(standing.exists((c, e) => c == CellKey("1", List("Price")) && e.code == "min"))
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
            assert(open == Present(CellPath(Item("1", "Ada", 10), List("Price"))), "and what it lands on is open")
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
            case Present(f) => handled(f(text))
            case Absent     => throw new AssertionError("the input declares no handler")

    /** A table filtered by two columns, one text and one that compares. */
    private def filtered(using Frame) =
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            menus   <- Kyo.foreach(List(List("Name"), List("Price")))(p => Signal.initRef(false).map(p -> _))
        yield
            val table = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).filterBy,
                uic.column("Price")(_.price.toString).filterBy(_.price)
            ).columnFilters(filters)
            val ui = table.wired("t", Map.empty, err, _ => (), menus.toMap)
            (ui, filters, menus.toMap)

    private def nameFilter(q: String) = Map(List("Name") -> ColumnFilter(q, MatchMode.Contains))

    /** A table whose column filters are bound as `bind` says, wired the way the mount wires it. */
    private def filteredBy(bind: uic.DataTable[Item] => uic.DataTable[Item])(using Frame) =
        for
            err   <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            typed <- Signal.initRef(Absent: Maybe[String])
            menus <- Signal.initRef(false)
        yield bind(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name).filterBy))
            .wired("t", Map.empty, err, _ => (), Map(List("Name") -> menus), filterTyped = Map(List("Name") -> typed))

    private def blur(i: UI.Ast.TextInput)(using Frame): Any < Async =
        i.attrs.onBlur match
            case Present(eff) => handled(eff)
            case Absent       => throw new AssertionError("the input declares no blur handler")

    private def shownQuery(ui: UI)(using Frame): String < Sync =
        inputs(ui).map(_.head.value match
            case Present(UI.Bound.Const(v)) => v
            case other                      => s"not a constant: $other")

    // The filters live somewhere else (a route, a parent): the table filters by them and hands what the reader types over.
    "column filters bound one way filter by the signal, and typing is reported without being written" in {
        for
            held   <- Signal.initRef(nameFilter("A"))
            seen   <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            ui     <- filteredBy(_.columnFilters(held.readOnly).onColumnFiltersChange(seen.set))
            before <- bodyNames(ui)
            field  <- inputs(ui).map(_.head)
            _      <- type_(field, "B")
            h      <- held.get
            s      <- seen.get
        yield
            assert(before == Chunk("A"), s"the rows follow the signal: $before")
            assert(h == nameFilter("A") && s == nameFilter("B"), s"held $h, reported $s")
    }

    "two-way column filters are written before the table reports them" in {
        for
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            seen    <- Signal.initRef((Map.empty[List[String], ColumnFilter], Map.empty[List[String], ColumnFilter]))
            ui      <- filteredBy(_.columnFilters(filters).onColumnFiltersChange(m => filters.get.map(h => seen.set((m, h)))))
            field   <- inputs(ui).map(_.head)
            _       <- type_(field, "B")
            s       <- seen.get
        yield assert(s == (nameFilter("B"), nameFilter("B")), s"reported, held: $s")
    }

    // An owner that writes back later than the reader types (through a route, say) answers the first keystroke after
    // the second. The field keeps what the reader typed: the late answer must not take a keystroke back.
    "an owner that answers late does not take back what the reader typed" in {
        for
            held  <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            ui    <- filteredBy(_.columnFilters(held.readOnly).onColumnFiltersChange(_ => ()))
            field <- inputs(ui).map(_.head)
            _     <- type_(field, "a")
            _     <- type_(field, "ab")
            _     <- held.set(nameFilter("a"))
            late  <- shownQuery(ui)
        yield assert(late == "ab", s"the late answer to the first keystroke shows $late")
    }

    // The mode menu reads the same filter the field shows: a mode picked while the owner has not caught up with the
    // reader carries what the reader typed, not the owner's older answer.
    "a mode picked over a one-way filter the owner has not caught up with keeps what the reader typed" in {
        for
            held  <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            seen  <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            err   <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            typed <- Signal.initRef(Absent: Maybe[String])
            menu  <- Signal.initRef(false)
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name).filterBy)
                .columnFilters(held.readOnly).onColumnFiltersChange(seen.set)
                .wired("t", Map.empty, err, _ => (), Map(List("Name") -> menu), filterTyped = Map(List("Name") -> typed))
            field <- inputs(ui).map(_.head)
            _     <- type_(field, "a")
            _     <- type_(field, "ab")
            _     <- held.set(nameFilter("a"))
            _     <- menu.set(true)
            open  <- elements(ui)
            starts = open.find(e =>
                e.attrs.cssClasses.contains("p-datatable-filter-constraint") &&
                    e.children.collect { case t: UI.Ast.Text => t.value }.contains("Starts with")
            ).getOrElse(throw new AssertionError("the menu shows no modes"))
            _ <- click(starts)
            s <- seen.get
        yield assert(s == Map(List("Name") -> ColumnFilter("ab", MatchMode.StartsWith)), s"the pick reported $s")
    }

    "once the reader leaves a one-way filter field it shows what the owner holds" in {
        for
            held   <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            ui     <- filteredBy(_.columnFilters(held.readOnly).onColumnFiltersChange(_ => ()))
            field  <- inputs(ui).map(_.head)
            _      <- type_(field, "ab")
            _      <- held.set(nameFilter("ab"))
            field2 <- inputs(ui).map(_.head)
            _      <- blur(field2)
            _      <- held.set(nameFilter("x"))
            after  <- shownQuery(ui)
        yield assert(after == "x", s"the owner's value after the reader left: $after")
    }

    "column filters bound one way with nothing to hear the reader say so" in {
        for
            held  <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            deaf  <- filteredBy(_.columnFilters(held.readOnly)).map(elementsWithClass(_, "p-uic-key-error"))
            heard <- filteredBy(_.columnFilters(held.readOnly).onColumnFiltersChange(_ => ())).map(elementsWithClass(_, "p-uic-key-error"))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onColumnFiltersChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty, s"somebody hears: ${heard.map(textOf)}")
    }

    "restore writes two-way column filters and leaves one-way ones to their owner" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name).filterBy)
        for
            twoWay <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            oneWay <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            _      <- table.columnFilters(twoWay).restore(TableState(columnFilters = nameFilter("B")))
            _ <- table.columnFilters(oneWay.readOnly).onColumnFiltersChange(_ => ()).restore(TableState(columnFilters = nameFilter("B")))
            t <- twoWay.get
            o <- oneWay.get
        yield assert(t == nameFilter("B") && o.isEmpty, s"two-way $t, one-way $o")
        end for
    }

    private def expandable(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
            .rowExpansionTemplate(i => span(s"more ${i.name}"))

    private def expansionTexts(ui: UI)(using Frame): Chunk[String] < Sync =
        elementsWithClass(ui, "p-datatable-row-expansion").map(_.map(textOf))

    // Which rows are open lives somewhere else (a route, a parent): the table opens those and hands a toggle over.
    "rows expanded one way follow the signal, and a toggle is reported without being written" in {
        for
            held <- Signal.initRef(Set(items(1)))
            seen <- Signal.initRef(Set.empty[Item])
            ui = expandable.expanded(held.readOnly).onExpandedChange(seen.set).render
            open    <- expansionTexts(ui)
            toggles <- elementsWithClass(ui, "p-datatable-row-toggle-button")
            _       <- click(toggles(0))
            h       <- held.get
            s       <- seen.get
        yield
            assert(open == Chunk("more B"), s"the open rows follow the signal: $open")
            assert(h == Set(items(1)) && s == Set(items(0), items(1)), s"held $h, reported $s")
        end for
    }

    "two-way expansion is written before the table reports it" in {
        for
            exp  <- Signal.initRef(Set.empty[Item])
            seen <- Signal.initRef((Set.empty[Item], Set.empty[Item]))
            ui = expandable.expanded(exp).onExpandedChange(next => exp.get.map(h => seen.set((next, h)))).render
            toggles <- elementsWithClass(ui, "p-datatable-row-toggle-button")
            _       <- click(toggles(1))
            s       <- seen.get
        yield assert(s == (Set(items(1)), Set(items(1))), s"reported, held: $s")
    }

    "expansion bound one way with nothing to hear a toggle says so" in {
        for
            held  <- Signal.initRef(Set.empty[Item])
            deaf  <- elementsWithClass(expandable.expanded(held.readOnly).render, "p-uic-key-error")
            heard <- elementsWithClass(expandable.expanded(held.readOnly).onExpandedChange(_ => ()).render, "p-uic-key-error")
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onExpandedChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty, s"somebody hears: ${heard.map(textOf)}")
    }

    private def grouped(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
            .groupBy(uic.RowGroup((i: Item) => i.name))

    "groups expanded one way follow the signal, and a toggle is reported without being written" in {
        for
            held <- Signal.initRef(Set(GroupPath(List("B"))))
            seen <- Signal.initRef(Set.empty[GroupPath])
            ui = grouped.expandedGroups(held.readOnly).onExpandedGroupsChange(seen.set).render
            shown   <- bodyNames(ui)
            toggles <- elementsWithClass(ui, "p-datatable-row-toggle-button")
            _       <- click(toggles(0))
            h       <- held.get
            s       <- seen.get
            deaf    <- elementsWithClass(grouped.expandedGroups(held.readOnly).render, "p-uic-key-error")
        yield
            // The body reads both group headers, A and B, and the rows of the open group alone.
            assert(shown == Chunk("A", "B", "B"), s"only the open group shows its rows: $shown")
            assert(h == Set(GroupPath(List("B"))), s"the signal still holds $h")
            assert(s == Set(GroupPath(List("A")), GroupPath(List("B"))), s"the toggle reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onExpandedGroupsChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "restore writes two-way expansion and leaves one-way expansion to its owner" in {
        for
            twoWay  <- Signal.initRef(Set.empty[Item])
            oneWay  <- Signal.initRef(Set.empty[Item])
            gTwoWay <- Signal.initRef(Set.empty[GroupPath])
            gOneWay <- Signal.initRef(Set.empty[GroupPath])
            state = TableState(expanded = Set("1"), expandedGroups = Set(GroupPath(List("A"))))
            _ <- expandable.expanded(twoWay).restore(state, k => Maybe.fromOption(items.find(_.id == k)))
            _ <- expandable.expanded(oneWay.readOnly).onExpandedChange(_ => ()).restore(state, k => Maybe.fromOption(items.find(_.id == k)))
            _ <- grouped.expandedGroups(gTwoWay).restore(state)
            _ <- grouped.expandedGroups(gOneWay.readOnly).onExpandedGroupsChange(_ => ()).restore(state)
            t <- twoWay.get
            o <- oneWay.get
            gt <- gTwoWay.get
            go <- gOneWay.get
        yield
            assert(t == Set(items(0)) && o.isEmpty, s"rows: two-way $t, one-way $o")
            assert(gt == Set(GroupPath(List("A"))) && go.isEmpty, s"groups: two-way $gt, one-way $go")
    }

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
            case f: UI.Ast.Foreach[?, ?] =>
                // A keyed row list is the same kind of layer as the reactive one above: what the rows
                // currently are is what the assertions are about.
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map { items =>
                            Kyo.foreach(items.zipWithIndex) { (item, i) => resolve(render(i, item)) }.map(_.flatten)
                        }
                )
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
    "the table is ONE region over edit state, column order, flags, sort, filter and widths, and its root none" in {
        // Starting an edit writes the error and the edited cell, a column drop the order and the drag: a region
        // nested per signal repaints the table against the values the inner one closed over. The root used to sit
        // under that region too, with the paginator and the header and footer slots in it; those follow none of the
        // state and may hold mounts, so the root is built once and the table container is the one region.
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            name    <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            shown   <- Signal.initRef(true)
            canSort <- Signal.initRef(true)
            sort    <- Signal.initRef(List.empty[uic.SortKey])
            query   <- Signal.initRef("")
            widths  <- Signal.initRef(Map.empty[List[String], Double])
            order   <- Signal.initRef(List(List("Price"), List("Name")))
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)).sortable(canSort),
                uic.column("Price")(_.price.toString).visible(shown)
            ).editingCell(editing).sort(sort).globalFilter(query).columnWidths(widths).columnOrder(order)
                .wired("t", Map(List("Name") -> name), err, _ => ())
            regions <- regionsAbove(ui, "p-datatable-table-container")
            root    <- regionsAbove(ui, "p-datatable")
        yield
            assert(regions == 1, s"the table sits under $regions regions; nested ones repaint older state")
            assert(root == 0, s"the root sits under $root regions")
        end for
    }

    "hiding a column renumbers the grid the keyboard moves over" in {
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            name    <- Signal.initRef("")
            price   <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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
            assert(overPrice == Present(CellPath(items(0), List("Price"))), "and the second one is now the price")
    }

    // The same defect on the mouse path. Focus cannot go to the button that replaces the
    // one just pressed: that element is new, and a self-command resolves its id the moment
    // it lands, which can be before the insert. It goes to a cell, which was already there.
    "the row editor's save button hands focus back to the row" in {
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Set(items(0)))
            name    <- Signal.initRef("Ada")
            price   <- Signal.initRef("10")
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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
            assert(open == Set.empty[Item], "pressing it closes the row")
            assert(store.head.name == "Ada", "writing the drafts through the columns")
            assert(seen == List("t-c0-0"), "and focus lands on the row's first editable cell")
    }

    // ---- the editing state under every binding ----

    /** Two editable columns over `items`, through the mount seam that gives the editor its drafts. */
    private def editable(bind: uic.DataTable[Item] => uic.DataTable[Item])(using Frame): UI < Sync =
        for
            name  <- Signal.initRef("")
            price <- Signal.initRef("")
            err   <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
        yield bind(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
            uic.column("Price")(_.price.toString).editable(_.price)((i, v) => i.copy(price = v))
        ).onCellValueChanged(_ => ()).onRowValueChanged(_ => ()))
            .wired("t", Map(List("Name") -> name, List("Price") -> price), err, _ => ())

    private def editableBare(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
        ).onCellValueChanged(_ => ()).onRowValueChanged(_ => ())

    "a constant editing cell nobody hears is fixed: its editor shows, and no other cell opens" in {
        for
            ui    <- editable(_.editingCell(Present(CellPath(items(0), List("Name")))))
            open  <- elementWithId(ui, "t-c0-0")
            other <- elementWithId(ui, "t-c1-0")
            card  <- cards(ui)
        yield
            assert(open.attrs.cssClasses.contains("p-cell-editing"), "the constant cell shows its editor")
            assert(!declaresClick(other), "a fixed editing cell opens no other")
            assert(card.isEmpty, s"a fixed editing cell is not a mistake: $card")
        end for
    }

    "an editing cell bound one way is followed, and a click reported without being written" in {
        for
            held <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            seen <- Signal.initRef(List.empty[Maybe[CellPath[Item]]])
            ui   <- editable(_.editingCell(held.readOnly).onEditingCellChange(c => seen.getAndUpdate(_ :+ c).unit))
            cell <- elementWithId(ui, "t-c1-0")
            _    <- click(cell)
            h    <- held.get
            told <- seen.get
            deaf <- cards(editableBare.editingCell(held.readOnly).render)
        yield
            assert(h == Absent && told == List(Present(CellPath(items(1), List("Name")))), s"held $h, reported $told")
            assert(deaf.contains("onEditingCellChange"), s"nobody hears: $deaf")
        end for
    }

    "an editing cell ref is written before the click is reported" in {
        for
            held <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            seen <- Signal.initRef(List.empty[(Maybe[CellPath[Item]], Maybe[CellPath[Item]])])
            ui   <- editable(_.editingCell(held).onEditingCellChange(c => held.get.map(h => seen.getAndUpdate(_ :+ (c, h)).unit)))
            cell <- elementWithId(ui, "t-c1-0")
            _    <- click(cell)
            told <- seen.get
        yield
            val one = Present(CellPath(items(1), List("Name")))
            assert(told == List((one, one)), s"reported, held: $told")
    }

    "editingCell() with no ref keeps the open cell itself, so a click opens the editor" in {
        val table = editableBare.editingCell()
        for
            card  <- cards(table.render)
            after <- publishedAfterReady(
                table.render,
                t => elementsWithClass(t, "p-editable-column").map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-cell-editing").map(_.nonEmpty)
            )(t => elementsWithClass(t, "p-editable-column").map(cs => click(cs.head)))
        yield
            assert(after.nonEmpty, "the table never opened the cell the reader clicked")
            assert(card.isEmpty, s"an editing cell the table keeps itself: $card")
        end for
    }

    "constant editing rows nobody hears are fixed: their editors show, and no button changes them" in {
        for
            ui    <- editable(_.editingRows(Set(items(0))))
            save  <- elementWithClass(ui, "p-datatable-row-editor-save")
            inits <- elementsWithClass(ui, "p-datatable-row-editor-init")
            card  <- cards(ui)
        yield
            assert(save.attrs.ariaAttrs.get("disabled").contains("true"), "the open row's save is reachable and inert")
            assert(inits.nonEmpty && inits.forall(_.attrs.ariaAttrs.get("disabled").contains("true")), "no row opens")
            assert(card.isEmpty, s"fixed editing rows are not a mistake: $card")
        end for
    }

    "editing rows bound one way are followed, and a row opened is reported without being written" in {
        for
            held  <- Signal.initRef(Set.empty[Item])
            seen  <- Signal.initRef(List.empty[Set[Item]])
            ui    <- editable(_.editingRows(held.readOnly).onEditingRowsChange(r => seen.getAndUpdate(_ :+ r).unit))
            inits <- elementsWithClass(ui, "p-datatable-row-editor-init")
            _     <- click(inits(1))
            h     <- held.get
            told  <- seen.get
            deaf  <- cards(editableBare.editingRows(held.readOnly).render)
        yield
            assert(h.isEmpty && told == List(Set(items(1))), s"held $h, reported $told")
            assert(deaf.contains("onEditingRowsChange"), s"nobody hears: $deaf")
        end for
    }

    "an editing rows ref is written before the row opened is reported" in {
        for
            held  <- Signal.initRef(Set.empty[Item])
            seen  <- Signal.initRef(List.empty[(Set[Item], Set[Item])])
            ui    <- editable(_.editingRows(held).onEditingRowsChange(r => held.get.map(h => seen.getAndUpdate(_ :+ (r, h)).unit)))
            inits <- elementsWithClass(ui, "p-datatable-row-editor-init")
            _     <- click(inits(1))
            told  <- seen.get
        yield assert(told == List((Set(items(1)), Set(items(1)))), s"reported, held: $told")
    }

    "editingRows() with no ref keeps the open rows itself, so the row editor opens a row" in {
        val table = editableBare.editingRows()
        for
            card  <- cards(table.render)
            after <- publishedAfterReady(
                table.render,
                t => elementsWithClass(t, "p-datatable-row-editor-init").map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-datatable-row-editor-save").map(_.nonEmpty)
            )(t => elementsWithClass(t, "p-datatable-row-editor-init").map(bs => click(bs.head)))
        yield
            assert(after.nonEmpty, "the table never opened the row the reader asked for")
            assert(card.isEmpty, s"editing rows the table keeps itself: $card")
        end for
    }

    /** A table whose three columns the reader may resize, with the header measurement
      * stubbed: what the grab reads is the width the browser would report, and the test
      * supplies it so the arithmetic is the only thing under test.
      */
    private def sized(
        widths: Map[String, Double],
        pinMiddle: Boolean = false,
        bind: (uic.DataTable[Item], SignalRef[Map[List[String], Double]]) => uic.DataTable[Item] = (t, c) => t.columnWidths(c)
    )(using Frame) =
        for
            rows <- Signal.initRef[Seq[Item]](items)
            cols <- Signal.initRef(Map.empty[List[String], Double])
            err  <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            grab <- Signal.initRef(Absent: Maybe[ColumnGrab])
        yield
            val base  = uic.DataTable[Item]().rows(rows).rowKey(_.id)
            val table =
                if pinMiddle then
                    base.columns(
                        uic.column("Name")(_.name),
                        uic.column("Price")(_.price.toString).resizable(false),
                        uic.column("Id")(_.id)
                    )
                else
                    base.columns(
                        uic.column("Name")(_.name),
                        uic.column("Price")(_.price.toString),
                        uic.column("Id")(_.id)
                    )
            // The stub answers by header id: `t-h0` is the first leaf column's cell. A grab
            // asks for every id it needs in one call, so the stub answers a list.
            val measure = (ids: Seq[String]) =>
                Chunk.from(ids.map(id => UI.Rect(0, 0, widths.getOrElse(id, 0.0), 30, 1000, 800))): Chunk[UI.Rect] < Async
            (bind(table, cols).wired("t", Map.empty, err, _ => (), Map.empty, measure, Present(grab)), cols, grab)
        end for
    end sized

    private def pointerAt(x: Double): UI.PointerEvent = UI.PointerEvent(x, 0, 0, 0, 6, 30, 1, Absent)

    private val mouseAt: UI.MouseEvent = UI.MouseEvent(Absent, UI.Modifiers.none)

    private def resizers(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-column-resizer")))

    private def drag(handle: UI.Ast.Element, from: Double, to: Double)(using Frame): Any < Async =
        (handle.attrs.onPointerDown, handle.attrs.onPointerMove) match
            case (Present(down), Present(move)) => handled(down(pointerAt(from)).andThen(move(pointerAt(to))))
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
        hide: Boolean = false,
        bindOrder: (uic.DataTable[Item], SignalRef[List[List[String]]]) => uic.DataTable[Item] = (t, o) => t.columnOrder(o)
    )(using Frame) =
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            order <- Signal.initRef(List.empty[List[String]])
            sort  <- Signal.initRef(List.empty[SortKey])
            shown <- Signal.initRef(!hide)
            err   <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            move  <- Signal.initRef(Absent: Maybe[ColumnDrag])
        yield
            val base  = uic.DataTable[Item]().rows(rows).rowKey(_.id).sort(sort)
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
                bindOrder(table, order).wired("t", Map.empty, err, _ => (), Map.empty, measure, Absent, Present(move)),
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
            _ <- handled(down(pointerAt(from)))
            _ <- handled(move(pointerAt(to)))
            r <- handled(up(pointerAt(to)))
        yield r

    // The layout lives somewhere else (a saved view, a parent): the table lays the columns out by it and hands a drag over.
    "column widths bound one way are reported on every move of a drag and not written" in {
        for
            seen          <- Signal.initRef(Map.empty[List[String], Double])
            (ui, cols, _) <-
                sized(Map("t-h0" -> 200, "t-h1" -> 100), bind = (t, c) => t.columnWidths(c.readOnly).onColumnWidthsChange(seen.set))
            handles <- resizers(ui)
            _       <- drag(handles.head, 200, 240)
            held    <- cols.get
            s       <- seen.get
        yield assert(held.isEmpty && s == Map(List("Name") -> 240.0, List("Price") -> 60.0), s"held $held, reported $s")
    }

    "two-way column widths are written before the table reports them" in {
        for
            seen          <- Signal.initRef((Map.empty[List[String], Double], Map.empty[List[String], Double]))
            (ui, cols, _) <- sized(
                Map("t-h0" -> 200, "t-h1" -> 100),
                bind = (t, c) => t.columnWidths(c).onColumnWidthsChange(m => c.get.map(h => seen.set((m, h))))
            )
            handles <- resizers(ui)
            _       <- drag(handles.head, 200, 240)
            s       <- seen.get
        yield assert(s._1 == s._2 && s._1.nonEmpty, s"reported, held: $s")
    }

    "a column order bound one way is reported on a drop and not written" in {
        for
            seen           <- Signal.initRef(List.empty[List[String]])
            (ui, order, _) <- movable(bindOrder = (t, o) => t.columnOrder(o.readOnly).onColumnOrderChange(seen.set))
            _              <- dragHeader(ui, "t-h0", 50, 210)
            held           <- order.get
            s              <- seen.get
        yield assert(held.isEmpty && s == List(List("Price"), List("Name"), List("Id")), s"held $held, reported $s")
    }

    "a layout bound one way with nothing to hear the reader says so" in {
        for
            (widthsUi, _, _) <- sized(Map("t-h0" -> 200, "t-h1" -> 100), bind = (t, c) => t.columnWidths(c.readOnly))
            (orderUi, _, _)  <- movable(bindOrder = (t, o) => t.columnOrder(o.readOnly))
            widthCards       <- elementsWithClass(widthsUi, "p-uic-key-error")
            orderCards       <- elementsWithClass(orderUi, "p-uic-key-error")
        yield
            assert(widthCards.map(textOf).mkString.contains("onColumnWidthsChange"), s"widths: ${widthCards.map(textOf)}")
            assert(orderCards.map(textOf).mkString.contains("onColumnOrderChange"), s"order: ${orderCards.map(textOf)}")
    }

    "a constant global filter filters, and restore leaves one-way layout and query to their owner" in {
        val table =
            uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name), uic.column("Price")(_.price.toString))
        for
            shown  <- bodyNames(table.globalFilter("B").render)
            query  <- Signal.initRef("")
            widths <- Signal.initRef(Map.empty[List[String], Double])
            order  <- Signal.initRef(List.empty[List[String]])
            state = TableState(globalFilter = "A", columnWidths = Map(List("Name") -> 90.0), columnOrder = List(List("Price")))
            _ <- table.globalFilter(query.readOnly).columnWidths(widths.readOnly).onColumnWidthsChange(_ => ())
                .columnOrder(order.readOnly).onColumnOrderChange(_ => ()).restore(state)
            q <- query.get
            w <- widths.get
            o <- order.get
        yield
            assert(shown == Chunk("B"), s"a constant query filters: $shown")
            assert(q == "" && w.isEmpty && o.isEmpty, s"query $q, widths $w, order $o")
        end for
    }

    "dragging a header past another writes the new order" in {
        for
            (ui, order, _) <- movable()
            _              <- dragHeader(ui, "t-h0", 50, 210)
            moved          <- order.get
        yield assert(moved == List(List("Price"), List("Name"), List("Id")), "the grabbed column lands where it was dropped")
    }

    // The header is drawn once per column layout, and a drop computes the new order from the order that layout shows
    // (`effectiveOrder`), so a second drop builds on the first rather than on the authored order.
    "a second drop keeps the first" in {
        for
            (ui, order, _) <- movable()
            _              <- dragHeader(ui, "t-h0", 50, 210)
            first          <- order.get
            _              <- dragHeader(ui, "t-h2", 250, 10)
            second         <- order.get
        yield
            assert(first == List(List("Price"), List("Name"), List("Id")), s"the first drop: $first")
            assert(second == List(List("Id"), List("Price"), List("Name")), s"the second, on top of it: $second")
        end for
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
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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

    private def pageLinkOf(node: UI, label: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(node, "p-paginator-page").map(_.find(textOf(_) == label).getOrElse(
            throw new AssertionError(s"no page link $label")
        ))

    // The page lives somewhere else (a route, a parent): the table pages by it, and a page click is handed over.
    "a page bound one way pages by the signal, and a page click is reported without being written" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            held <- Signal.initRef(1)
            seen <- Signal.initRef(-1)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(2)(held.readOnly).lazyRows(7).onPageChange(p => seen.set(p)).render
            (_, current) <- pages(ui)
            link         <- pageLinkOf(ui, "3")
            _            <- click(link)
            h            <- held.get
            s            <- seen.get
        yield
            assert(current == List("2"), s"the page the signal holds is the one shown: $current")
            assert(h == 1 && s == 2, s"the signal still holds $h, the click reported $s")
    }

    "a two-way page is written before the table reports it" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            page <- Signal.initRef(0)
            seen <- Signal.initRef((-1, -1))
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(2)(page).lazyRows(7).onPageChange(p => page.get.map(held => seen.set((p, held)))).render
            link <- pageLinkOf(ui, "4")
            _    <- click(link)
            s    <- seen.get
        yield assert(s == (3, 3), s"reported, held: $s")
    }

    "a page size bound one way takes its picks through onPageSizeChange, and without it says so" in {
        for
            rows <- Signal.initRef[Seq[Item]](unsorted)
            size <- Signal.initRef(2)
            page <- Signal.initRef(0)
            base = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(size.readOnly)(page).lazyRows(7).paginator(_.rowsPerPageOptions(Seq(2, 4)))
            heard    <- elementsWithClass(base.onPageSizeChange(_ => ()).render, "p-uic-key-error")
            deaf     <- elementsWithClass(base.render, "p-uic-key-error")
            dropdown <- elementWithClass(base.onPageSizeChange(_ => ()).render, "p-paginator-rpp-dropdown")
        yield
            assert(heard.isEmpty, s"a size with somewhere to go: ${heard.map(textOf)}")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onPageSizeChange"), s"a size nobody hears: ${deaf.map(textOf)}")
            assert(!dropdown.attrs.cssClasses.contains("p-disabled"), "a size someone hears is one the reader can pick")
    }

    "restore writes a two-way page and leaves a one-way page to its owner" in {
        for
            rows   <- Signal.initRef[Seq[Item]](unsorted)
            twoWay <- Signal.initRef(0)
            oneWay <- Signal.initRef(0)
            table = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name)).lazyRows(7)
            _ <- table.paginate(2)(twoWay).restore(TableState(page = 3))
            _ <- table.paginate(2)(oneWay.readOnly).onPageChange(_ => ()).restore(TableState(page = 3))
            t <- twoWay.get
            o <- oneWay.get
        yield assert(t == 3 && o == 0, s"two-way $t, one-way $o")
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

    /** The first window of `n` rows a source published. */
    private def servedOf[A](ch: Channel[Seq[A]], n: Int)(using Frame): Seq[A] < (Async & Abort[Closed]) =
        ch.take.map(v => if v.size == n then v else servedOf(ch, n))

    /** Waits for a source's loading flag to come down, which is its answer, empty or not. */
    private def answered(ch: Channel[Boolean])(using Frame): Unit < (Async & Abort[Closed]) =
        ch.take.map(loading => if loading then answered(ch) else ())

    private val five = (1 to 5).toList.map(i => Item(i.toString, ('A' + i - 1).toChar.toString, i * 10))

    "a bound source fills the rows, the total and the paginator in one call" in {
        for
            query <- Signal.initRef("a")
            src   <- RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            _    <- src.run
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

    // The table binds a source's size the way `paginate` binds a ref, so the rows-per-page dropdown writes it and the
    // table and the source page by the same number.
    "a source with a bound page size pages by what it holds" in {
        for
            query <- Signal.initRef("a")
            size  <- Signal.initRef(2)
            src   <- RowSource.init(query, pageSize = size) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            _    <- src.run
            seen <- Channel.init[Seq[Item]](16)
            _    <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _    <- served(seen)
            ui = uic.DataTable[Item]().rowKey(_.id).columns(uic.column("Name")(_.name)).source(src).render
            _                  <- size.set(4)
            _                  <- servedOf(seen, 4)
            names              <- bodyNames(ui)
            (offered, current) <- pages(ui)
        yield
            assert(names == Chunk("A", "B", "C", "D"), s"a page of four, not $names")
            assert(offered == List("1", "2"), "five rows of four are two pages")
            assert(current == List("1"))
    }

    // A page opens a source the way it opens a ref and hands it to the table, which runs it in its own mount: no mount
    // of the page's own, and no copy of the table as that mount's placeholder. Until the table is mounted, its static
    // render is the table loading, which is the placeholder the page used to copy by hand.
    "a table over a source runs it in its own mount" in {
        for
            query <- Signal.initRef("a")
            src   <- (RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }: RowSource[String, Item] < Sync)
            seen <- Channel.init[Seq[Item]](16)
            _    <- Fiber.init(src.rows.observe(v => seen.put(v)))
            ui = uic.DataTable[Item]().rowKey(_.id).columns(uic.column("Name")(_.name)).source(src).render
            before <- bodyTrs(ui).map(_.size)
            busy   <- src.loading.current
            names  <- Scope.run {
                for
                    _ <- live(ui)
                    _ <- served(seen)
                    n <- bodyNames(ui)
                yield n
            }
        yield
            assert(busy && before == 2, s"unmounted, the table holds the place of a page of two: $before rows, loading $busy")
            assert(names == Chunk("A", "B"), s"mounted, it ran the source: $names")
    }

    // Before its first answer a source holds no rows and is loading, which is not the same as a
    // query that matched nothing: the table used to say "No records found" under its mask for as
    // long as the first page took, then grew by every row that arrived.
    "a page still loading is not reported as empty" in {
        for
            query <- Signal.initRef("a")
            gate  <- Promise.init[Unit, Any]
            src   <- RowSource.init(query, pageSize = 3) { (_, offset, limit) =>
                gate.get.andThen((five.slice(offset, offset + limit), Total.Known(five.size)))
            }
            ui = uic.DataTable[Item]().rowKey(_.id).columns(uic.column("Name")(_.name)).source(src).render
            trs <- bodyTrs(ui)
            _   <- gate.completeUnitDiscard
        yield
            assert(trs.nonEmpty, "the body holds the place of the page it is waiting for")
            assert(
                !trs.exists(_.attrs.cssClasses.contains("p-datatable-empty-message")),
                "nothing has been asked yet, so nothing can have been found missing"
            )
    }

    /** A lazily loaded page of `size` rows, still loading, holding `rows` from before. */
    private def loadingPage(rows: Seq[Item], size: Int, page: Int, total: Total)(using Frame): UI < Sync =
        for
            sizeRef <- Signal.initRef(size)
            pageRef <- Signal.initRef(page)
        yield uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
            .paginate(sizeRef)(pageRef).lazyRows(total).loading(true).render

    /** How many of `trs` hold a skeleton rather than a row. */
    private def skeletonRows(trs: List[UI.Ast.Element])(using Frame): Int < Sync =
        Kyo.foreach(trs)(tr => elements(tr).map(_.exists(_.attrs.cssClasses.contains("p-skeleton")))).map(_.count(identity))

    // The reader asked for more rows per page. Until the bigger page arrives the table holds the rows it
    // has and the place of the rest, so it does not grow by them when they land.
    "a page that grows while it loads keeps its rows and holds the place of the rest" in {
        for
            ui    <- loadingPage(five.take(3), size = 5, page = 0, total = Total.Known(10))
            names <- bodyNames(ui)
            trs   <- bodyTrs(ui)
            held  <- skeletonRows(trs)
        yield
            assert(names == Chunk("A", "B", "C"), "the rows it has stay")
            assert(trs.size == 5 && held == 2, s"two places held for the rows to come, got ${trs.size} rows, $held held")
    }

    "a page that shrinks while it loads shows only the rows it will have" in {
        for
            ui    <- loadingPage(five, size = 3, page = 0, total = Total.Known(10))
            names <- bodyNames(ui)
        yield assert(names == Chunk("A", "B", "C"), "the rows past the new page size belong to no page the reader asked for")
    }

    "the last page holds the place of only the rows that remain" in {
        for
            ui   <- loadingPage(Nil, size = 3, page = 3, total = Total.Known(10))
            trs  <- bodyTrs(ui)
            held <- skeletonRows(trs)
        yield assert(trs.size == 1 && held == 1, s"ten rows leave one for the fourth page of three, got ${trs.size}")
    }

    /** The rows standing in under `loadingContent`, and the height their content is laid over. */
    private def loadingParts(trs: List[UI.Ast.Element])(using Frame): (Int, List[Maybe[Length]]) < Sync =
        Kyo.foreach(trs)(tr => elements(tr)).map { inside =>
            val rows = trs.count(_.attrs.cssClasses.contains("p-uic-datatable-loading-row"))
            val over = inside.flatten.filter(_.attrs.cssClasses.contains("p-uic-datatable-loading-content")).map(heightOf).toList
            (rows, over)
        }

    "loading text stands over the place of the rows that are coming, as tall as they will be" in {
        for
            sizeRef <- Signal.initRef(3)
            pageRef <- Signal.initRef(0)
            ui = uic.DataTable[Item]().rows(Seq.empty[Item]).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(sizeRef)(pageRef).lazyRows(Total.Known(10)).loading(true)
                .loadingContent("Loading products").render
            trs          <- bodyTrs(ui)
            (rows, over) <- loadingParts(trs)
            text         <- elements(ui).map(_.flatMap(_.children.collect { case t: UI.Ast.Text => t.value }))
        yield
            assert(rows == 3, "one row per row that is coming, so the height is theirs")
            assert(over == List(Present(Length.Calc("3 * 100%"))), "and the text once, over all three")
            assert(text.count(_ == "Loading products") == 1)
            assert(!trs.exists(_.attrs.cssClasses.contains("p-datatable-empty-message")))
    }

    "loading content of the caller's own stands over the rows still missing from a growing page" in {
        for
            sizeRef <- Signal.initRef(5)
            pageRef <- Signal.initRef(0)
            table = uic.DataTable[Item]().rows(five.take(3)).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(sizeRef)(pageRef).lazyRows(Total.Known(10)).loading(true)
                .loadingContent(UI.span("one moment").id("mine")).render
            names        <- bodyNames(table)
            trs          <- bodyTrs(table)
            (rows, over) <- loadingParts(trs)
            mine         <- elements(table).map(_.count(_.attrs.identifier.contains("mine")))
        yield
            assert(names == Chunk("A", "B", "C"), "the rows it has stay")
            assert(rows == 2 && over == List(Present(Length.Calc("2 * 100%"))), "the caller's content over the two to come")
            assert(mine == 1, "rendered once")
    }

    /** What one table shows now: its rows' names, how many rows stand in under `loadingContent`, how
      * tall the content over them is, and the texts it holds.
      */
    private def loadingState(ui: UI)(using Frame): (Chunk[String], Int, List[Maybe[Length]], Chunk[String]) < Sync =
        for
            names        <- bodyNames(ui)
            trs          <- bodyTrs(ui)
            (rows, over) <- loadingParts(trs)
            texts        <- Kyo.foreach(trs)(tr => elements(tr)).map(_.flatten.flatMap(_.children.collect { case t: UI.Ast.Text =>
                t.value
            }.filterNot(_ == "\u00a0")))
        yield (names, rows, over, Chunk.from(texts))

    "the place held for a loading page follows the page size, the rows held and the flag as they change" in {
        for
            sizeRef <- Signal.initRef(3)
            pageRef <- Signal.initRef(0)
            rows    <- Signal.initRef(Seq.empty[Item])
            loading <- Signal.initRef(true)
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(sizeRef)(pageRef).lazyRows(Total.Known(10)).loading(loading)
                .loadingContent(n => UI.span(s"$n coming")).render
            first  <- loadingState(ui)
            _      <- sizeRef.set(5)
            grown  <- loadingState(ui)
            _      <- rows.set(five.take(2))
            held   <- loadingState(ui)
            _      <- sizeRef.set(1)
            shrunk <- loadingState(ui)
            _      <- loading.set(false)
            done   <- loadingState(ui)
        yield
            assert(first == (Chunk(), 3, List(Present(Length.Calc("3 * 100%"))), Chunk("3 coming")), first.toString)
            assert(grown == (Chunk(), 5, List(Present(Length.Calc("5 * 100%"))), Chunk("5 coming")), grown.toString)
            assert(held == (Chunk("A", "B"), 3, List(Present(Length.Calc("3 * 100%"))), Chunk("A", "B", "3 coming")), held.toString)
            assert(shrunk == (Chunk("A"), 0, Nil, Chunk("A")), shrunk.toString)
            assert(done._2 == 0 && done._3.isEmpty && !done._4.exists(_.contains("coming")), done.toString)
    }

    "loadingRow puts the caller's own row in place of each row that is coming" in {
        for
            sizeRef <- Signal.initRef(3)
            pageRef <- Signal.initRef(0)
            ui = uic.DataTable[Item]().rows(Seq.empty[Item]).rowKey(_.id).columns(uic.column("Name")(_.name))
                .paginate(sizeRef)(pageRef).lazyRows(Total.Known(10)).loading(true)
                .loadingRow((i, n) => UI.span(s"row ${i + 1} of $n")).render
            (_, rows, over, texts) <- loadingState(ui)
            trs                    <- bodyTrs(ui)
            held                   <- skeletonRows(trs)
        yield
            assert(texts == Chunk("row 1 of 3", "row 2 of 3", "row 3 of 3"))
            assert(trs.size == 3 && held == 0, "the caller's rows and nothing of the table's own")
            assert(!trs.exists(_.attrs.cssClasses.contains("p-uic-datatable-loading-over")) && over.isEmpty)
    }

    /** How many busy masks `ui` holds now. */
    private def masks(ui: UI)(using Frame): Int < Sync =
        elements(ui).map(_.count(_.attrs.cssClasses.contains("p-datatable-mask")))

    private def maskTable(mask: Boolean | Signal[Boolean])(using Frame): UI =
        uic.DataTable[Item]().rows(Seq.empty[Item]).rowKey(_.id).columns(uic.column("Name")(_.name))
            .loading(true).loadingMask(mask).render

    "loadingMask(false) keeps the mask off while the rows load, and the place and the busy flag on" in {
        for
            off  <- masks(maskTable(false))
            on   <- masks(maskTable(true))
            trs  <- bodyTrs(maskTable(false))
            busy <- elements(maskTable(false)).map(_.filter(_.attrs.cssClasses.contains("p-datatable-table"))
                .flatMap(_.attrs.ariaAttrs.get("busy").toList))
            held <- skeletonRows(trs)
        yield
            assert(off == 0, "no mask")
            assert(on == 1, "the default and an explicit true both mask")
            assert(held == 1 && busy == Chunk("true"), "the place of the rows and aria-busy stay")
    }

    // Content of the caller's own already says how the wait looks, and the mask's spinner landed on top
    // of it. So setting it switches the mask off unless the mask is asked for as well.
    "loading content of the caller's own leaves the mask off unless it is asked for" in {
        def table(using Frame) =
            uic.DataTable[Item]().rows(Seq.empty[Item]).rowKey(_.id).columns(uic.column("Name")(_.name)).loading(true)
        for
            text    <- masks(table.loadingContent("Fetching").render)
            counted <- masks(table.loadingContent(n => UI.span(s"$n")).render)
            rows    <- masks(table.loadingRow((i, _) => UI.span(s"$i")).render)
            asked   <- masks(table.loadingContent("Fetching").loadingMask(true).render)
            plain   <- masks(table.render)
        yield
            assert((text, counted, rows) == (0, 0, 0), "no spinner over the caller's own content")
            assert(asked == 1, "unless the mask is asked for")
            assert(plain == 1, "the skeletons keep Prime's mask")
        end for
    }

    "a mask bound to a signal comes and goes with it" in {
        for
            ref <- Signal.initRef(true)
            ui = maskTable(ref)
            first  <- masks(ui)
            _      <- ref.set(false)
            hidden <- masks(ui)
            _      <- ref.set(true)
            back   <- masks(ui)
        yield assert((first, hidden, back) == (1, 0, 1))
    }

    "a table whose rows are on their way says it is busy" in {
        for
            busy <- elements(uic.DataTable[Item]().rows(Seq.empty[Item]).rowKey(_.id)
                .columns(uic.column("Name")(_.name)).loading(true).render)
                .map(_.filter(_.attrs.cssClasses.contains("p-datatable-table")).flatMap(_.attrs.ariaAttrs.get("busy").toList))
        yield assert(busy == Chunk("true"))
    }

    "a source that answered no rows is reported as empty" in {
        for
            query <- Signal.initRef("a")
            src   <- RowSource.init(query, pageSize = 3) { (_, _, _) =>
                (Seq.empty[Item], Total.Known(0))
            }
            _    <- src.run
            done <- Channel.init[Boolean](16)
            _    <- Fiber.init(src.loading.observe(v => done.put(v)))
            _    <- answered(done)
            ui = uic.DataTable[Item]().rowKey(_.id).columns(uic.column("Name")(_.name)).source(src).render
            trs <- bodyTrs(ui)
        yield assert(trs.sizeIs == 1 && trs.head.attrs.cssClasses.contains("p-datatable-empty-message"))
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
            // A derived signal: exactly what a query state or a paginated
            // connection hands a caller, and not something anyone can write into.
            derived = source.map(_.filter(_.price >= 20))
            table   = uic.DataTable[Item]().rows(derived).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            )
            before <- bodyNames(table.render)
            _      <- source.set(items :+ Item("3", "C", 30))
            after  <- bodyNames(table.render)
        yield
            assert(before == Chunk("B"), "the projection is what the table shows")
            assert(after == Chunk("B", "C"), "and a later emission is picked up")
    }

    "a rowKey that repeats is reported, since it costs the selection and the reuse both" in {
        // Two rows the projection cannot tell apart. Selecting one selects both, and the body's
        // keyed list refuses to reuse anything and rebuilds every emission, neither of which
        // shows on the page, and the second only became a cost once the body was keyed.
        val twins = List(Item("1", "A", 10), Item("2", "A", 20), Item("3", "B", 30))
        for text <- cards(uic.DataTable[Item]().rows(twins).rowKey(_.name).columns(
                uic.column("Name")(_.name)
            ).onRowClick(_ => ()).render)
        yield
            assert(text.contains("rowKey repeats"))
            assert(text.contains("A"), "and the repeated key is named")
        end for
    }

    "a rowKey that is unique is not reported" in {
        for text <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).onRowClick(_ => ()).render)
        yield assert(!text.contains("rowKey repeats"))
    }

    "rows bound twice are reported, since only one binding is read" in {
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            query <- Signal.initRef("a")
            src   <- RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            text <- cards(uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).source(src).render)
        yield
            assert(text.contains("bound twice"))
            assert(text.contains("source") && text.contains("rows(ref)"), "and both bindings are named")
    }

    "rows beside a source are named by their form, and rows set twice are no mistake" in {
        for
            rows  <- Signal.initRef[Seq[Item]](items)
            query <- Signal.initRef("a")
            src   <- RowSource.init(query, pageSize = 2) { (_, offset, limit) =>
                (five.slice(offset, offset + limit), Total.Known(five.size))
            }
            beside <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).source(src).render)
            twice <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name)
            ).rows(rows.readOnly).render)
        yield
            assert(beside.contains("bound twice") && beside.contains("source") && beside.contains("rows(seq)"))
            assert(!twice.contains("bound twice"), s"the second rows replaces the first: $twice")
        end for
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

    // A window over a source that has not answered has nothing to scroll over yet, and used to
    // say "No records found" for it, then grew to the height of its viewport.
    "a window still loading holds its rows' place rather than reporting them missing" in {
        for
            query <- Signal.initRef("a")
            gate  <- Promise.init[Unit, Any]
            src   <- RowSource.init(query, pageSize = 6)((_, o, l) =>
                gate.get.andThen((hundred.slice(o, o + l), Total.Known(100)))
            )
            trs <- bodyTrs(windowed.source(src).render)
            _   <- gate.completeUnitDiscard
        yield
            assert(!trs.exists(_.attrs.cssClasses.contains("p-datatable-empty-message")), "nothing was found missing")
            assert(
                trs.count(tr => heightOf(tr) == Present(40.px)) == 5,
                "the five rows the viewport holds, at the height they will have"
            )
    }

    private def bodyEl(node: UI)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(node, "p-datatable-tbody")

    private def bodyTrs(node: UI)(using Frame): List[UI.Ast.Element] < Sync =
        bodyEl(node).flatMap(rowsUnder).map(_.toList)

    private def heightOf(e: UI.Ast.Element): Maybe[Length] =
        Maybe.fromOption(e.attrs.uiStyle.props.collect { case Style.Prop.Height(v) => v }.headOption)

    /** The style `e` shows now: its static style, then its inline-style channel's value. A row group's height and a frozen
      * group's offset follow the state through that channel, since the group itself is not drawn again for them.
      */
    private def styleNow(e: UI.Ast.Element)(using Frame): Style < Sync =
        e.attrs.reactiveStyle match
            case Present(sig) => sig.current.map(e.attrs.uiStyle ++ _)
            case Absent       => Kyo.lift(e.attrs.uiStyle)

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
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.rows(rows).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            names   <- bodyNames(ui)
            pad     <- spacers(ui)
            attrs   <- windowAttrs(ui)
            body    <- bodyEl(ui)
            tall    <- styleNow(body).map(_.props.collect { case Style.Prop.Height(v) => v })
            offered <- pages(ui).map(_._1)
        yield
            assert(names == Chunk("R1", "R2", "R3", "R4", "R5", "R6"), "the six rows the arithmetic reaches")
            assert(pad == List(Present(0.px), Present(3760.px)), "and one spacer holding the ninety-four behind them")
            // A floor under the table while its rows are rewritten: they go in document
            // order, so a leading spacer shrinking before the trailing one grows would
            // collapse it under the scroll position for an instant.
            assert(tall == Seq(4000.px), s"the body is as tall as the whole list: $tall")
            assert(attrs == (Present("0"), Present("6")))
            assert(offered.isEmpty, "a windowed table scrolls instead of paginating")
    }

    // The scroll position is a row index in disguise, and the stripe follows the row it
    // belongs to rather than its place in the window.
    "a scroll moves the window, and the rows keep the stripe their own index gives them" in {
        for
            rows   <- Signal.initRef[Seq[Item]](hundred)
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.source(src).wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            vs <- elementWithClass(ui, "p-virtualscroller")
            _  <- vs.attrs.onScrollPos.getOrElse(throw new AssertionError("the scroller reports no scroll"))(
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
            _      <- src.run
            seen   <- Channel.init[Seq[Item]](16)
            _      <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _      <- served(seen)
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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

    "rows a window has not reached stand under the caller's loading content, a run at a time" in {
        for
            query  <- Signal.initRef("a")
            src    <- RowSource.init(query, pageSize = 6)((_, o, l) => (hundred.slice(o, o + l), Total.Known(100)))
            _      <- src.run
            seen   <- Channel.init[Seq[Item]](16)
            _      <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _      <- served(seen)
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            scroll <- Signal.initRef(0.0)
            ui = windowed.source(src).loadingContent("Loading rows").wired("t", Map.empty, err, _ => (), scroll = Present(scroll))
            _            <- scroll.set(400.0)
            trs          <- bodyTrs(ui)
            (rows, over) <- loadingParts(trs)
        yield
            val drawn = trs.filterNot(_.attrs.cssClasses.contains("p-datatable-virtualscroller-spacer"))
            assert(rows == 6 && drawn.size == 6, "the six rows of the window, none of them a skeleton row")
            assert(drawn.forall(t => heightOf(t) == Present(40.px)), "each as tall as the row it stands in for")
            assert(over == List(Present(Length.Calc("6 * 100%"))), "and the text once, over the whole run")
    }

    // Infinite scrolling is the total saying it does not know, and it reaches one screen
    // past whatever has been served so far.
    "an unknown total leaves a screen to scroll into while anything follows" in {
        for
            query <- Signal.initRef("a")
            src   <- RowSource.init(query, pageSize = 6)((_, o, l) =>
                (hundred.slice(o, o + l), Total.Unknown(o + l < hundred.size))
            )
            _      <- src.run
            seen   <- Channel.init[Seq[Item]](16)
            _      <- Fiber.init(src.rows.observe(v => seen.put(v)))
            _      <- served(seen)
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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
            err  <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            top  <- Signal.initRef(Absent: Maybe[Int])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .scrollHeight("200px").frozenRows(pinned)
                .wired("t", Map.empty, err, _ => (), headTop = Present(top))
            before <- groups(ui)
            atRest <- namesIn(before.head)
            restAt <- styleNow(before.head).map(_.props.collect { case Style.Prop.Top(v) => v })
            _      <- top.set(Present(41))
            after  <- groups(ui)
            heldAt <- styleNow(after.head).map(_.props.collect { case Style.Prop.Top(v) => v })
        yield
            assert(before.size == 2, "the pinned rows are a row group of their own")
            assert(before.head.attrs.cssClasses.contains("p-datatable-frozen-tbody"), "and it comes first")
            assert(
                before.head.attrs.cssClasses.contains("p-datatable-tbody"),
                "carrying the body class too, or the cells lose their styling"
            )
            assert(atRest == List("Total"), "holding the rows it was given")
            assert(heightOf(before.head).isEmpty && restAt.isEmpty, "before the header is measured it sits in flow")
            assert(heldAt == List(41.px), s"and once it is measured, it holds at that height: $heldAt")
    }

    "the body rows keep their own group, and the cursor does not step into the frozen one" in {
        for
            rows <- Signal.initRef[Seq[Item]](items)
            err  <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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
        // nobody asked. The control cannot decline the click for the row: one that navigates natively
        // declares no kyo handler, so it has no `stopPropagation` to set.
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).render
            trs        <- bodyTrs(ui)
            _          <- clickViaControl(trs.head)
            viaControl <- sel.get
            _          <- click(trs.head)
            plain      <- sel.get
        yield
            assert(viaControl.isEmpty, "the control's click does not also select")
            assert(plain == rowsWith("1"), "and a click on the row itself still does")
    }

    "the keyboard path is the row's, whatever the row contains" in {
        // `onControl` answers where a POINTER went; Enter on the row is the row's, and the row is
        // what holds the tab stop.
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).render
            trs    <- bodyTrs(ui)
            _      <- press(trs.head, UI.Keyboard.Enter)
            picked <- sel.get
        yield assert(picked == rowsWith("1"))
    }

    "a row the predicate rejects stays out of the selection when it is clicked" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel)
                .selectableWhen(_.id != "2").render
            trs <- bodyTrs(ui)
            _   <- click(trs(1))
            no  <- sel.get
            _   <- click(trs.head)
            yes <- sel.get
        yield
            assert(no == Set.empty, "the rejected row writes nothing")
            assert(yes == rowsWith("1"), "and the accepted one still selects")
            assert(
                !trs(1).attrs.cssClasses.contains("p-datatable-selectable-row"),
                "a row that cannot be picked does not offer the pointer that says it can"
            )
            assert(trs.head.attrs.cssClasses.contains("p-datatable-selectable-row"))
    }

    "select-all passes over the rows the predicate rejects" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selection(sel)
                .selectableWhen(_.id != "2").render
            box <- elements(ui).map(_.collect { case c: UI.Ast.Checkbox => c }.head)
            _   <- box.onChange match
                case Present(f) => f(true)
                case Absent     => throw new AssertionError("the header box declares no change handler")
            all <- sel.get
        yield assert(all == rowsWith("1"), "the header box selects what may be selected and nothing else")
    }

    "the checkbox of a rejected row is disabled and inert" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selection(sel)
                .selectableWhen(_.id != "2").render
            trs <- bodyTrs(ui)
            ok  <- checkboxIn(trs.head)
            no  <- checkboxIn(trs(1))
        yield
            assert(ok.attrs.onClick.isDefined && !ok.attrs.cssClasses.contains("p-disabled"))
            assert(no.attrs.onClick.isEmpty, "there is no handler to reach, not a handler that refuses")
            assert(no.attrs.cssClasses.contains("p-disabled"), "and it says so")
    }

    // ---- selection: the three bindings the union carries ----

    "a constant selection paints, and nothing can move it" in {
        for
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(rowsWith("2")).onRowClick(_ => ()).render
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
            src    <- Signal.initRef(rowsWith("1"))
            clicks <- Signal.initRef(List.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple)
                .selection(src.map(identity))
                .onRowClick(k => clicks.getAndUpdate(_ :+ k).unit).render
            trs   <- bodyTrs(ui)
            _     <- click(trs(1))
            after <- src.get
            heard <- clicks.get
            _     <- src.set(rowsWith("2"))
            moved <- bodyTrs(ui)
        yield
            assert(after == rowsWith("1"), "the table did not write to a signal it was only given to read")
            assert(heard == List(items(1)), "and the click still reached the caller")
            assert(moved(1).attrs.cssClasses.contains("p-datatable-row-selected"))
            assert(!moved.head.attrs.cssClasses.contains("p-datatable-row-selected"))
    }

    "a SignalRef still binds two ways" in {
        // The regression nail: the union dispatches on the RUNTIME class, and ReactiveVariable
        // IS-A Dyn, so the wrong match order would silently downgrade every existing caller.
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).render
            trs    <- bodyTrs(ui)
            _      <- click(trs(1))
            picked <- sel.get
        yield assert(picked == rowsWith("2"))
    }

    "a one-way selection still reports what it holds, and restore leaves it alone" in {
        for
            src <- Signal.initRef(rowsWith("1", "2"))
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity)).onRowClick(_ => ())
            st           <- table.state
            _            <- table.restore(uic.TableState(selected = Set("9")))
            afterRestore <- src.get
        yield
            assert(st.selected == Set("1", "2"), "reading needs no write access")
            assert(afterRestore == rowsWith("1", "2"), "and restoring has nowhere to write")
    }

    "a right-click over a one-way selection is still told what is selected" in {
        for
            src  <- Signal.initRef(rowsWith("1"))
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity))
                .onRowClick(_ => ())
                .onContextMenuRowChange(t => seen.set(Present(t))).render
            trs <- bodyTrs(ui)
            _   <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            told <- seen.get
        yield assert(told == Present(uic.RowContext(items(1), List(items.head))))
    }

    "a checkbox column over a one-way selection has nothing to write, and says so" in {
        for
            src  <- Signal.initRef(Set.empty[Item])
            text <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selection(src.map(identity)).render)
        yield assert(text.contains("checkbox column has nothing to write"))
    }

    "a one-way selection with no onRowClick cannot move at all, and says so" in {
        for
            src   <- Signal.initRef(Set.empty[Item])
            inert <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity)).render)
            wired <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity))
                .onRowClick(_ => ()).render)
            twoWay <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src).render)
        yield
            assert(inert.contains("a row click has nowhere to go"))
            assert(!wired.contains("a row click has nowhere to go"), "an onRowClick is the outlet")
            assert(!twoWay.contains("a row click has nowhere to go"), "and so is a writable ref")
    }

    // ---- metaKeySelection: the modifier ruleset, and the range it brings ----

    /** A range needs more than the two rows the rest of this file gets by with. */
    private val four =
        List(Item("1", "A", 10), Item("2", "B", 20), Item("3", "C", 30), Item("4", "D", 40))

    /** The rows with these ids, which is what a selection holds. Every fixture here numbers its
      * rows the same way, so [[four]] answers for all of them.
      */
    private def rowsWith(ids: String*): Set[Item] = ids.map(id => four.find(_.id == id).get).toSet

    /** A table with modifier selection, through the mount seam it needs for its anchor. */
    private def modifierTable(sel: SignalRef[Set[Item]], rows: Seq[Item] = four)(using Frame) =
        for
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            anchor <- Signal.initRef(Absent: Maybe[Item])
        yield
            val t = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).metaKeySelection(true)
            (t.wired("t", Map.empty, err, _ => (), anchor = Present(uic.ReactiveValue[Maybe[Item]](anchor))), anchor)

    private def clickWith(el: UI.Ast.Element, mods: UI.Modifiers)(using Frame): Any < Async =
        el.attrs.onClickEvt match
            case Present(f) => handled(f(UI.MouseEvent(el.attrs.identifier, mods)))
            case Absent     => throw new AssertionError("the row declares no typed click handler")

    "with metaKeySelection a plain click replaces the selection instead of adding to it" in {
        for
            sel     <- Signal.initRef(Set.empty[Item])
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
            assert(first == rowsWith("1"))
            assert(second == rowsWith("2"), "the second click replaced rather than added")
            assert(again == rowsWith("2"), "and a plain click never clears the row it lands on")
    }

    "Ctrl or Cmd toggles the row into and out of the selection" in {
        for
            sel      <- Signal.initRef(Set.empty[Item])
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
            assert(withCtrl == rowsWith("1", "2"), "Ctrl adds")
            assert(withMeta == rowsWith("1", "2", "3"), "and so does Cmd: it is meta OR ctrl, never one")
            assert(removed == rowsWith("1", "3"), "a modified click on a picked row takes it out")
    }

    "Shift selects the range from the last row picked without it" in {
        for
            sel     <- Signal.initRef(Set.empty[Item])
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
            assert(down == rowsWith("1", "2", "3"), "inclusive, in the reader's order")
            assert(narrow == rowsWith("1", "2"), "and re-measured from the same anchor")
    }

    "a range runs upwards as readily as down, and an anchorless shift picks one row" in {
        for
            sel      <- Signal.initRef(Set.empty[Item])
            (ui, _)  <- modifierTable(sel)
            trs      <- bodyTrs(ui)
            _        <- clickWith(trs(1), UI.Modifiers(shift = true))
            noAnchor <- sel.get
            _        <- click(trs(2))
            _        <- clickWith(trs.head, UI.Modifiers(shift = true))
            up       <- sel.get
        yield
            assert(noAnchor == rowsWith("2"), "the first shift-click is just a pick, and sets the anchor")
            assert(up == rowsWith("1", "2", "3"))
    }

    "a range steps over the rows the predicate rejects" in {
        for
            sel    <- Signal.initRef(Set.empty[Item])
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            anchor <- Signal.initRef(Absent: Maybe[Item])
            ui = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).metaKeySelection(true)
                .selectableWhen(_.id != "2")
                .wired("t", Map.empty, err, _ => (), anchor = Present(uic.ReactiveValue[Maybe[Item]](anchor)))
            trs    <- bodyTrs(ui)
            _      <- click(trs.head)
            _      <- clickWith(trs(2), UI.Modifiers(shift = true))
            picked <- sel.get
        yield assert(picked == rowsWith("1", "3"), "a rejected row is not dragged in by a range")
    }

    "without metaKeySelection the modifiers mean nothing, and no anchor is owned" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).render
            trs   <- bodyTrs(ui)
            _     <- click(trs.head)
            _     <- clickWith(trs(1), UI.Modifiers(shift = true))
            after <- sel.get
        yield
            // The default table is not mounted at all, which is the whole reason the anchor is
            // tied to the opt-in: `bodyTrs` reaching real rows here proves there is no mount.
            assert(after == rowsWith("1", "2"), "shift is just another click while the flag is off")
    }

    "onSelectionChange carries what the table worked out, which a row key cannot" in {
        for
            src    <- Signal.initRef(Set.empty[Item])
            heard  <- Signal.initRef(List.empty[Set[Item]])
            err    <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            anchor <- Signal.initRef(Absent: Maybe[Item])
            ui = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple)
                // One way in, and the callback is the only way out: the shape a selection whose
                // truth lives in a cache has.
                .selection(src.map(identity))
                .metaKeySelection(true)
                .onSelectionChange(s => heard.getAndUpdate(_ :+ s).unit)
                .wired("t", Map.empty, err, _ => (), anchor = Present(uic.ReactiveValue[Maybe[Item]](anchor)))
            trs <- bodyTrs(ui)
            _   <- click(trs.head)
            // The table computed a range off its own anchor; the caller could not have.
            _   <- clickWith(trs(2), UI.Modifiers(shift = true))
            got <- heard.get
            end <- src.get
        yield
            assert(got == List(rowsWith("1"), rowsWith("1", "2", "3")))
            assert(end.isEmpty, "and it wrote to nothing, because nothing writable was bound")
    }

    "a bound ref and the callback both hear it, and hear the same thing" in {
        // `applySelection` writes then tells, so a caller may bind either or both and the two can
        // never disagree about what the selection became. Every selection path goes through it.
        for
            sel   <- Signal.initRef(Set.empty[Item])
            heard <- Signal.initRef(List.empty[Set[Item]])
            ui = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel)
                .onSelectionChange(s => heard.getAndUpdate(_ :+ s).unit).render
            trs   <- bodyTrs(ui)
            _     <- click(trs.head)
            _     <- click(trs(1))
            got   <- heard.get
            inRef <- sel.get
        yield
            assert(got == List(rowsWith("1"), rowsWith("1", "2")))
            assert(inRef == got.last, "the ref and the callback ended on the same value")
    }

    "a one-way selection with modifiers and only a row key to report with is reported" in {
        for
            src  <- Signal.initRef(Set.empty[Item])
            text <- cards(uic.DataTable[Item]().rows(four).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity))
                .metaKeySelection(true).onRowClick(_ => ()).render)
            quiet <- cards(uic.DataTable[Item]().rows(four).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(src.map(identity))
                .metaKeySelection(true).onSelectionChange(_ => ()).render)
        yield
            assert(text.contains("cannot express a range or a replacement"))
            assert(!quiet.contains("cannot express a range or a replacement"))
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
            sel <- Signal.initRef(rowsWith("1"))
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selection(sel).stripedRows(true)
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
    /** Whether `el` carries `name` now: statically, or through a class channel whose signal holds true. Whether a table
      * fits or expands follows its widths, so where the widths are bound the table says so through a channel.
      */
    private def classNow(el: UI.Ast.Element, name: String)(using Frame): Boolean < Sync =
        el.attrs.reactiveClasses.get(name) match
            case Some(sig) => sig.current
            case None      => Kyo.lift(el.attrs.cssClasses.contains(name))

    /** The widths `el` states now: its static style's, then its inline-style channel's, which wins where both have one. */
    private def widthsNow(el: UI.Ast.Element)(using Frame): Seq[Length] < Sync =
        val static = el.attrs.uiStyle.props.collect { case Style.Prop.Width(v) => v }
        el.attrs.reactiveStyle match
            case Present(sig) => sig.current.map(st => static ++ st.props.collect { case Style.Prop.Width(v) => v })
            case Absent       => Kyo.lift(static)
    end widthsNow

    private def expanding(mode: uic.ColumnResizeMode, lastWidth: Maybe[Double] = Present(100.0))(using Frame) =
        for
            cols <- Signal.initRef(Map.empty[List[String], Double])
            err  <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            grab <- Signal.initRef(Absent: Maybe[ColumnGrab])
        yield
            val base     = uic.DataTable[Item]().rows(items).rowKey(_.id)
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
            val table   = withCols.columnWidths(cols).columnResizeMode(mode)
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
            tblFit      <- classNow(tbl, "p-datatable-resizable-table-fit")
            fittedFit   <- classNow(fitted, "p-datatable-resizable-table-fit")
            tblWidths   <- widthsNow(tbl)
            fitWidths   <- widthsNow(fitted)
            // The class follows the widths in place (a class channel on the root), since whether an expanding table
            // states its width depends on them; it is on while its signal says so.
            scrolls <- root.attrs.reactiveClasses.get("p-datatable-scrollable") match
                case Some(sig) => sig.current
                case None      => Kyo.lift(root.attrs.cssClasses.contains("p-datatable-scrollable"))
        yield
            assert(wide.size == 3, "every resizable column, the last one included")
            assert(paired.size == 2, "against the two boundaries fitting has")
            assert(!tblFit)
            assert(fittedFit)
            assert(tblWidths == Seq(300.0.px), s"and the table states the width its columns add up to: $tblWidths")
            assert(fitWidths.isEmpty)
            assert(scrolls, "a table that outgrows its container scrolls")
        end for
    }

    "a column the reader has sized gives an expanding table the last column's grip back" in {
        // Expanding needs every column sized; one sized only by a drag counts as soon as its width is in the bound map, and
        // the header has to follow the map for it, since the header is no longer drawn again for every width.
        for
            (ui, cols) <- expanding(uic.ColumnResizeMode.Expand, lastWidth = Absent)
            before     <- resizers(ui)
            _          <- cols.set(Map(List("Id") -> 120.0))
            after      <- resizers(ui)
            last       <- elementWithId(ui, "t-h2")
            lastOn     <- classNow(last, "p-datatable-resizable-column")
        yield
            assert(before.size == 2, s"fitting, while the last column has no width: ${before.size}")
            assert(after.size == 3, s"expanding once the map sizes it, the last column's grip included: ${after.size}")
            assert(lastOn, "and its header says it resizes")
        end for
    }

    "a column the spec can sort by says how it sorts, none included, statically and live alike" in {
        // Prime (HeaderCell.ariaSort) puts aria-sort on every sortable column, `none` while it does not sort, and on no
        // other. A column locked against the reader's clicks still says how the spec sorts it.
        for
            sort <- Signal.initRef(List.empty[uic.SortKey])
            err  <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            t = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name).sortBy(_.name),
                uic.column("Price")(_.price.toString),
                uic.column("Id")(_.id).sortBy(_.id).sortable(false)
            ).sort(sort)
            html <- renderHtml(t.render)
            wired = t.wired("t", Map.empty, err, _ => ())
            live    <- elementsWithClass(wired, "p-datatable-header-cell")
            liveNow <- Kyo.foreach(live)(c =>
                c.attrs.reactiveAttrs.get("aria-sort") match
                    case Some(sig) => sig.current.map(Some(_))
                    case None      => Kyo.lift(c.attrs.ariaAttrs.get("sort"))
            )
        yield
            assert("""aria-sort="none"""".r.findAllMatchIn(html).size == 2, "the static page: none on Name and Id, nothing on Price")
            assert(liveNow.toList == List(Some("none"), None, Some("none")), s"live: $liveNow")
        end for
    }

    "a column with no width sends an expanding table back to fitting" in {
        for
            (ui, _)  <- expanding(uic.ColumnResizeMode.Expand, lastWidth = Absent)
            handles  <- resizers(ui)
            tbl      <- elementWithClass(ui, "p-datatable-resizable-table")
            reported <- cards(ui)
            fits     <- classNow(tbl, "p-datatable-resizable-table-fit")
            widths   <- widthsNow(tbl)
        yield
            assert(handles.size == 2, "the boundaries fitting has, and no handle on the last column")
            assert(fits)
            assert(widths.isEmpty, "no width it cannot add up")
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
            drag  <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            moves <- Signal.initRef(List.empty[(Int, Int, List[String])])
            table = build(
                uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true)
                    .onRowReorder(m => moves.getAndUpdate(_ :+ (m.from, m.to, m.rows.map(_.name).toList)))
            )
            ui <- wiredDrag(table, drag)
        yield (ui, rows, moves, drag)

    /** `table` through the mount seam a drag needs: three rows forty pixels tall, and `drag` to park it in. */
    private def wiredDrag(table: uic.DataTable[Item], drag: SignalRef[Maybe[uic.RowDrag]])(using Frame): UI < Sync =
        Signal.initRef(Absent: Maybe[(CellKey, FieldError)]).map { err =>
            val measureAll = (ids: Seq[String]) =>
                Chunk.from(ids.zipWithIndex.map((_, i) => UI.Rect(0, i * 40, 200, 40, 1000, 800))): Chunk[UI.Rect] < Async
            table.wired(
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
        }

    private def grips(node: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(node).map(_.filter(_.attrs.cssClasses.contains("p-datatable-reorderable-row-handle")))

    private def dragRow(grip: UI.Ast.Element, from: Double, to: Double)(using Frame): Any < Async =
        (grip.attrs.onPointerDown, grip.attrs.onPointerMove, grip.attrs.onPointerUp) match
            case (Present(down), Present(move), Present(up)) =>
                handled(down(pointerAtY(from)).andThen(move(pointerAtY(to))).andThen(up(pointerAtY(to))))
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

    // ---- the rows under every binding ----

    /** Two editable columns through the mount seam that gives the editors their drafts, with no rows and nobody
      * listening: `bind` supplies the rows, the editing state and whoever hears.
      */
    private def editableOver(bind: uic.DataTable[Item] => uic.DataTable[Item])(using Frame) =
        for
            name  <- Signal.initRef("")
            price <- Signal.initRef("")
            err   <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
        yield
            val ui = bind(uic.DataTable[Item]().rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                uic.column("Price")(_.price.toString).editable(_.price)((i, v) => i.copy(price = v))
            )).wired("t", Map(List("Name") -> name, List("Price") -> price), err, _ => ())
            (ui, name, price)

    /** Opens the first row's Name cell, types `text` and commits it with Enter. */
    private def commitFirstName(ui: UI, text: String)(using Frame): Any < Async =
        for
            cell  <- elementWithId(ui, "t-c0-0")
            _     <- press(cell, UI.Keyboard.Enter)
            open  <- elementWithId(ui, "t-c0-0")
            _     <- typeInto(open, text)
            again <- elementWithId(ui, "t-c0-0")
            r     <- press(again, UI.Keyboard.Enter)
        yield r

    "rows set twice show the second list, since a row list is set rather than appended" in {
        for trs <- bodyTrs(uic.DataTable[Item]().rows(items).rows(trio).rowKey(_.id).columns(uic.column("Name")(_.name)).render)
        yield assert(trs.size == 3, s"${trs.size} rows")
    }

    "constant rows nobody hears are fixed: no cell opens, no row opens, and no card" in {
        for
            cellEd         <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            rowEd          <- Signal.initRef(Set.empty[Item])
            (byCell, _, _) <- editableOver(_.rows(items).editingCell(cellEd))
            (byRow, _, _)  <- editableOver(_.rows(items).editingRows(rowEd))
            cell           <- elementWithId(byCell, "t-c0-0")
            inits          <- elementsWithClass(byRow, "p-datatable-row-editor-init")
            cardC          <- cards(byCell)
            cardR          <- cards(byRow)
        yield
            assert(!declaresClick(cell) && !cell.attrs.cssClasses.contains("p-editable-column"), "a fixed row list opens no cell")
            assert(inits.nonEmpty && inits.forall(_.attrs.ariaAttrs.get("disabled").contains("true")), "and no row")
            assert(cardC.isEmpty && cardR.isEmpty, s"fixed rows are not a mistake: $cardC / $cardR")
        end for
    }

    "constant rows nobody hears keep their grips and drag nothing, with no card" in {
        for
            drag <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            ui   <- wiredDrag(
                uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true),
                drag
            )
            handles  <- grips(ui)
            reported <- cards(ui)
        yield
            assert(handles.size == 3 && handles.forall(_.attrs.onPointerDown.isEmpty), "the grips show and drag nothing")
            assert(reported.isEmpty, s"fixed rows are not a mistake: $reported")
    }

    "rows bound one way are followed, and a commit is reported through onRowsChange without being written" in {
        for
            held       <- Signal.initRef[Seq[Item]](items)
            seen       <- Signal.initRef(List.empty[Seq[Item]])
            ed         <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            (ui, _, _) <- editableOver(_.rows(held.readOnly).editingCell(ed).onRowsChange(rs => seen.getAndUpdate(_ :+ rs).unit))
            _          <- commitFirstName(ui, "Ada")
            h          <- held.get
            s          <- seen.get
        yield
            assert(h == items, s"a one-way list is not written: $h")
            assert(s == List(Seq(items(0).copy(name = "Ada"), items(1))), s"reported $s")
    }

    "a one-way row list nobody hears is reported in a card that names onRowsChange" in {
        for
            held       <- Signal.initRef[Seq[Item]](trio)
            ed         <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            drag       <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            (ui, _, _) <- editableOver(_.rows(held.readOnly).editingCell(ed))
            moved      <- wiredDrag(
                uic.DataTable[Item]().rows(held.readOnly).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true),
                drag
            )
            edits <- cards(ui)
            drops <- cards(moved)
        yield
            assert(edits.contains("onRowsChange"), s"an edit nobody hears: $edits")
            assert(drops.contains("onRowsChange"), s"a drop nobody hears: $drops")
    }

    "a row list ref is written before onRowsChange hears it" in {
        for
            held       <- Signal.initRef[Seq[Item]](items)
            seen       <- Signal.initRef((Seq.empty[Item], Seq.empty[Item]))
            ed         <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            (ui, _, _) <- editableOver(_.rows(held).editingCell(ed).onRowsChange(rs => held.get.map(now => seen.set((rs, now)))))
            _          <- commitFirstName(ui, "Ada")
            s          <- seen.get
        yield
            val after = Seq(items(0).copy(name = "Ada"), items(1))
            assert(s == (after, after), s"reported, held: $s")
    }

    "rows heard only through onRowsChange offer the grips, and a drop reports the new list" in {
        for
            seen <- Signal.initRef(List.empty[Seq[String]])
            drag <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            ui   <- wiredDrag(
                uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true).onRowsChange(rs => seen.getAndUpdate(_ :+ rs.map(_.name)).unit),
                drag
            )
            handles <- grips(ui)
            _       <- dragRow(handles.head, 10, 95)
            s       <- seen.get
        yield assert(s == List(Seq("B", "A", "C")), s"reported $s")
    }

    "rows heard only through onRowValueChanged can be edited, with no card" in {
        for
            rowEd      <- Signal.initRef(Set.empty[Item])
            (ui, _, _) <- editableOver(_.rows(items).editingRows(rowEd).onRowValueChanged(_ => ()))
            inits      <- elementsWithClass(ui, "p-datatable-row-editor-init")
            reported   <- cards(ui)
        yield
            assert(inits.nonEmpty && inits.forall(declaresClick), "the rows open")
            assert(reported.isEmpty, s"onRowValueChanged hears a row edit: $reported")
    }

    "a drop on rows heard only for edits reaches nobody: constant rows drag nothing, and a one-way list says so" in {
        for
            held  <- Signal.initRef[Seq[Item]](trio)
            dragC <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            dragS <- Signal.initRef(Absent: Maybe[uic.RowDrag])
            fixed <- wiredDrag(
                uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true).onCellValueChanged(_ => ()),
                dragC
            )
            oneWay <- wiredDrag(
                uic.DataTable[Item]().rows(held.readOnly).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .reorderableRows(true).onCellValueChanged(_ => ()),
                dragS
            )
            handles  <- grips(fixed)
            reported <- cards(oneWay)
        yield
            assert(handles.forall(_.attrs.onPointerDown.isEmpty), "onCellValueChanged hears no drop")
            assert(reported.contains("onRowsChange"), s"a drop nobody hears: $reported")
        end for
    }

    "an edit on rows heard only for drops reaches nobody: constant rows open no cell, and a one-way list says so" in {
        for
            held           <- Signal.initRef[Seq[Item]](items)
            edC            <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            edS            <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            (fixed, _, _)  <- editableOver(_.rows(items).editingCell(edC).onRowReorder(_ => ()))
            (oneWay, _, _) <- editableOver(_.rows(held.readOnly).editingCell(edS).onRowReorder(_ => ()))
            cell           <- elementWithId(fixed, "t-c0-0")
            reported       <- cards(oneWay)
        yield
            assert(!declaresClick(cell), "onRowReorder hears no edit")
            assert(reported.contains("onRowsChange"), s"an edit nobody hears: $reported")
        end for
    }

    "a cell edit on rows heard only through onRowValueChanged reaches nobody, since a cell commit fires no row event" in {
        for
            held           <- Signal.initRef[Seq[Item]](items)
            edC            <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            edS            <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            (fixed, _, _)  <- editableOver(_.rows(items).editingCell(edC).onRowValueChanged(_ => ()))
            (oneWay, _, _) <- editableOver(_.rows(held.readOnly).editingCell(edS).onRowValueChanged(_ => ()))
            cell           <- elementWithId(fixed, "t-c0-0")
            reported       <- cards(oneWay)
        yield
            assert(!declaresClick(cell), "onRowValueChanged hears no cell commit")
            assert(reported.contains("onRowsChange"), s"a cell commit nobody hears: $reported")
        end for
    }

    "a row whose second column refuses lands nowhere, not half written" in {
        for
            held              <- Signal.initRef[Seq[Item]](items)
            cells             <- Signal.initRef(List.empty[String])
            rowEd             <- Signal.initRef(Set(items(0)))
            (ui, name, price) <-
                editableOver(_.rows(held).editingRows(rowEd).onCellValueChanged(c => cells.getAndUpdate(_ :+ c.after.name).unit))
            _    <- name.set("Ada")
            _    <- price.set("nope")
            save <- elementWithClass(ui, "p-datatable-row-editor-save")
            _    <- click(save)
            h    <- held.get
            c    <- cells.get
            open <- rowEd.get
        yield
            assert(h == items, s"the Name column's write was kept: $h")
            assert(c.isEmpty, s"and reported: $c")
            assert(open == Set(items(0)), "the row stays open on its error")
        end for
    }

    // ---- the row a context menu was opened on ----

    "a right-click marks the row it landed on and tells the caller, without selecting it" in {
        for
            sel  <- Signal.initRef(Set.empty[Item])
            ctx  <- Signal.initRef(Absent: Maybe[Item])
            seen <- Signal.initRef(List.empty[Item])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel)
                .contextMenuRow(ctx).onContextMenuRowChange(t => seen.getAndUpdate(_ :+ t.row))
            trs <- bodyTrs(table.render)
            _   <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            on     <- ctx.get
            picked <- sel.get
            told   <- seen.get
            after  <- bodyTrs(table.render)
        yield
            assert(on == Present(items(1)) && told == List(items(1)))
            assert(picked.isEmpty, "acting on a row is not selecting it")
            assert(after(1).attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"))
            assert(!after.head.attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"))
    }

    "a constant context row nobody hears is fixed: marked, and the right-click left to the browser" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
            .contextMenuRow(Present(items(1)))
        for
            trs  <- bodyTrs(table.render)
            card <- cards(table.render)
        yield
            assert(trs(1).attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"), "the constant row is marked")
            assert(trs.forall(_.attrs.onContextMenu.isEmpty), "a fixed row takes no right-click")
            assert(card.isEmpty, s"a fixed context row is not a mistake: $card")
        end for
    }

    "a context row bound one way is followed, and a right-click reported without being written" in {
        for
            held <- Signal.initRef(Absent: Maybe[Item])
            seen <- Signal.initRef(List.empty[Item])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .contextMenuRow(held.readOnly).onContextMenuRowChange(t => seen.getAndUpdate(_ :+ t.row).unit)
            _      <- held.set(Present(items.head))
            before <- bodyTrs(table.render)
            _      <- before(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            h    <- held.get
            told <- seen.get
            deaf <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .contextMenuRow(held.readOnly).render)
        yield
            assert(before.head.attrs.cssClasses.contains("p-datatable-contextmenu-row-selected"), "the signal's row is marked")
            assert(h == Present(items.head) && told == List(items(1)), s"held $h, reported $told")
            assert(deaf.contains("onContextMenuRowChange"), s"nobody hears: $deaf")
        end for
    }

    "a context row ref is written before the right-click is reported" in {
        for
            ctx  <- Signal.initRef(Absent: Maybe[Item])
            seen <- Signal.initRef(List.empty[(Item, Maybe[Item])])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .contextMenuRow(ctx).onContextMenuRowChange(t => ctx.get.map(c => seen.getAndUpdate(_ :+ (t.row, c)).unit))
            trs <- bodyTrs(table.render)
            _   <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            told <- seen.get
        yield assert(told == List((items(1), Present(items(1)))), s"reported, held: $told")
    }

    "the right-click hands over the selected ROWS beside the one it landed on" in {
        for
            sel  <- Signal.initRef(rowsWith("1"))
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel)
                .onContextMenuRowChange(t => seen.set(Present(t)))
            trs <- bodyTrs(table.render)
            // Row 2 is NOT in the selection: the pair is what lets a menu tell the two cases apart.
            _ <- trs(1).attrs.onContextMenu match
                case Present(h) => h
                case Absent     => throw new AssertionError("the row declares no context handler")
            outside <- seen.get
            _       <- sel.set(rowsWith("1", "2"))
            _       <- trs(1).attrs.onContextMenu match
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
            sel  <- Signal.initRef(rowsWith("2", "1"))
            qry  <- Signal.initRef("B")
            seen <- Signal.initRef(Absent: Maybe[uic.RowContext[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).globalFilter(qry)
                .onContextMenuRowChange(t => seen.set(Present(t)))
            trs <- bodyTrs(table.render)
            _   <- trs.head.attrs.onContextMenu match
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
            sel    <- Signal.initRef(Set.empty[Item])
            widths <- Signal.initRef(Map.empty[List[String], Double])
            table = uic.DataTable[Item]().rows(trio).rowKey(_.id).columns(
                uic.column("Name")(_.name).sortBy(_.name),
                uic.column("Price")(_.price.toString)
            ).sort(sort).globalFilter(query).paginate(2)(page)
                .selectionMode(SelectionMode.Multiple).selection(sel).columnWidths(widths)
            fresh <- table.state
            _     <- sort.set(List(uic.SortKey.ascending("Name")))
            _     <- query.set("b")
            _     <- page.set(1)
            _     <- sel.set(rowsWith("2"))
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
            picked <- Signal.initRef(Set.empty[CellPath[Item]])
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
            assert(both == Set(CellPath(items(0), List("Name")), CellPath(items(1), List("Price"))))
            assert(one == Set(CellPath(items(1), List("Price"))), "a second click on a picked cell takes it back")
            assert(after(3).attrs.cssClasses.contains("p-uic-dt-cell-selected"))
            assert(!after.head.attrs.cssClasses.contains("p-uic-dt-cell-selected"))
            assert(after.head.attrs.ariaAttrs.get("selected").contains("false"))
    }

    "a constant cell set nobody hears is fixed: picked cells show, and a click picks nothing" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name),
            uic.column("Price")(_.price.toString)
        ).selectionMode(SelectionMode.Multiple).selectedCells(Set(CellPath(items(1), List("Price"))))
        for
            tds  <- cellsIn(table.render)
            card <- cards(table.render)
        yield
            assert(tds(3).attrs.cssClasses.contains("p-uic-dt-cell-selected"), "the constant cell shows picked")
            assert(tds.forall(td => !declaresClick(td)), "a fixed set takes no click")
            assert(card.isEmpty, s"a fixed cell set is not a mistake: $card")
        end for
    }

    "a cell set bound one way is followed, and a click reported without being written" in {
        for
            held <- Signal.initRef(Set(CellPath(items(0), List("Name"))))
            seen <- Signal.initRef(List.empty[Set[CellPath[Item]]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).selectionMode(SelectionMode.Multiple).selectedCells(held.readOnly)
                .onSelectedCellsChange(c => seen.getAndUpdate(_ :+ c).unit)
            tds  <- cellsIn(table.render)
            _    <- click(tds(3))
            h    <- held.get
            told <- seen.get
            deaf <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selectedCells(held.readOnly).render)
        yield
            assert(tds.head.attrs.cssClasses.contains("p-uic-dt-cell-selected"), "the signal's cell shows picked")
            assert(h == Set(CellPath(items(0), List("Name"))), s"the signal is not written: $h")
            assert(told == List(Set(CellPath(items(0), List("Name")), CellPath(items(1), List("Price")))), s"reported $told")
            assert(deaf.contains("onSelectedCellsChange"), s"nobody hears: $deaf")
        end for
    }

    "a cell set ref is written before the click is reported" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath[Item]])
            seen   <- Signal.initRef(List.empty[(Set[CellPath[Item]], Set[CellPath[Item]])])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Single).selectedCells(picked)
                .onSelectedCellsChange(c => picked.get.map(p => seen.getAndUpdate(_ :+ (c, p)).unit))
            tds  <- cellsIn(table.render)
            _    <- click(tds(1))
            told <- seen.get
        yield
            val one = Set(CellPath(items(1), List("Name")))
            assert(told == List((one, one)), s"reported, held: $told")
    }

    // A table with a selection mode and no row binding keeps a row selection of its own, which it allocates in a mount.
    // That selection must not count as a row binding: it switched cell selection off in every live table.
    "a live table picks cells into a bound ref, with no row selection bound" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Single).selectedCells(picked)
            // A published region may be the row stream alone, with no table around it, so the mark is looked for anywhere.
            after <- publishedAfterReady(
                table.render,
                t => cellsIn(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-uic-dt-cell-selected").map(_.nonEmpty)
            )(t => cellsIn(t).map(tds => click(tds(1))))
            held <- picked.get
        yield
            assert(held == Set(CellPath(items(1), List("Name"))), s"picked $held")
            assert(after.nonEmpty, "the table never showed the cell the reader picked")
    }

    "selectedCells() with no ref keeps the picked cells itself" in {
        val table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
            .selectionMode(SelectionMode.Single).selectedCells()
        for
            card <- cards(table.render)
            // A published region may be the row stream alone, with no table around it, so the mark is looked for anywhere.
            after <- publishedAfterReady(
                table.render,
                t => cellsIn(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-uic-dt-cell-selected").map(_.nonEmpty)
            )(t => cellsIn(t).map(tds => click(tds(1))))
        yield
            assert(after.nonEmpty, "the table never showed the cell the reader picked")
            assert(card.isEmpty, s"cells the table keeps itself: $card")
        end for
    }

    "Single replaces the set rather than adding to it" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Price")(_.price.toString)
            ).selectionMode(SelectionMode.Single).selectedCells(picked)
            tds  <- cellsIn(table.render)
            _    <- click(tds.head)
            _    <- click(tds(3))
            only <- picked.get
        yield assert(only == Set(CellPath(items(1), List("Price"))))
    }

    "a cell of a row the predicate rejects cannot be picked" in {
        for
            picked <- Signal.initRef(Set.empty[CellPath[Item]])
            table = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).selectableWhen(_.id != "2")
            tds <- cellsIn(table.render)
        yield
            assert(tds.head.attrs.onClick.isDefined)
            assert(tds(1).attrs.onClick.isEmpty, "no handler to reach, not a handler that refuses")
    }

    "a click that two bindings both claim is reported, and editing keeps it" in {
        for
            picked  <- Signal.initRef(Set.empty[CellPath[Item]])
            editing <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            rows    <- Signal.initRef(Set.empty[Item])
            clash   <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).editingCell(editing)
                .onCellValueChanged(_ => ()).render)
            withRows <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selectedCells(picked).selection(rows).render)
            modeless <- cards(uic.DataTable[Item]().rows(items).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Checkbox).selectedCells(picked).render)
        yield
            assert(clash.contains("editingCell") && clash.contains("selectedCells"))
            assert(withRows.contains("selection, which picks rows"))
            assert(modeless.contains("Single or Multiple") && modeless.contains("Checkbox"))
    }

    // ---- several conditions on one column ----

    /** A menu-display table over the three items, with the panel's open state and its
      * draft owned here so a test can open it and read what it holds.
      */
    private def filterMenu(using Frame) =
        for
            filters <- Signal.initRef(Map.empty[List[String], ColumnFilter])
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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

    "the open filter menu is ONE region over the open state and the draft" in {
        // Opening writes the draft and the open state together; a draft region nested in the Overlay's own drew
        // the menu from a draft it closed over.
        for
            (ui, _, open, _) <- filterMenu
            _                <- open.set(true)
            funnel           <- regionsAbove(ui, "p-datatable-popover-filter")
            menu             <- regionsAbove(ui, "p-datatable-filter-buttonbar")
        yield assert(menu == funnel + 1, s"the menu sits ${menu - funnel} regions below the funnel's span; one reads both")
    }

    "the funnel is in the header cell and there is no filter row" in {
        for
            (ui, _, _, _) <- filterMenu
            all           <- elements(ui)
        yield
            assert(all.exists(_.attrs.cssClasses.contains("p-datatable-popover-filter")))
            assert(!all.exists(_.attrs.cssClasses.contains("p-datatable-inline-filter")), "one display or the other")
    }

    "the menu's operator and match-mode dropdowns write the draft they are picked in" in {
        // Both handlers were `fold(())(f)`, which hands the action back as a value: a pick in either dropdown did nothing.
        val two = ColumnFilter(List(FilterRule("A", MatchMode.Contains), FilterRule("B", MatchMode.Contains)), uic.FilterOperator.And)
        val dt  = uic.DataTable[String]().rows(List("A")).columns(uic.column("Name")(identity))
        for
            draft <- Signal.initRef(two)
            _     <- dt.operatorPicked(draft, two)(Present(uic.FilterOperator.Or))
            op    <- draft.get
            _     <- dt.modePicked(draft, two, 1)(Present(MatchMode.StartsWith))
            mode  <- draft.get
        yield
            assert(op.operator == uic.FilterOperator.Or, s"the operator pick: ${op.operator}")
            assert(mode.rules(1).mode == MatchMode.StartsWith, s"the second rule's mode pick: ${mode.rules(1).mode}")
        end for
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
            _                          <- draft.set(ColumnFilter(
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
            _                   <- filters.set(Map(List("Name") -> ColumnFilter(
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
            err <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
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

    private def sortedTable(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id)
            .columns(uic.column("Name")(_.name).sortBy(_.name), uic.column("Price")(_.price.toString).sortBy(_.price))

    // The sort lives somewhere else (a route, a parent): the table sorts by it and hands a header press over.
    "a sort bound one way orders the rows by the signal, and a header press is reported without being written" in {
        val byNameDown = List(uic.SortKey("Name", uic.SortDirection.Descending))
        for
            held <- Signal.initRef(byNameDown)
            seen <- Signal.initRef(List.empty[uic.SortKey])
            ui = sortedTable.sort(held.readOnly).onSortChange(seen.set).render
            shown   <- bodyNames(ui)
            headers <- elementsWithClass(ui, "p-datatable-sortable-column")
            _       <- press(headers(1), UI.Keyboard.Enter)
            h       <- held.get
            s       <- seen.get
        yield
            assert(shown == Chunk("B", "A"), s"the rows follow the signal: $shown")
            assert(h == byNameDown, s"the signal still holds $h")
            assert(s.map(_.column) == List("Price"), s"the press reported $s")
        end for
    }

    "a two-way sort is written before the table reports it" in {
        for
            sort <- Signal.initRef(List.empty[uic.SortKey])
            seen <- Signal.initRef((List.empty[uic.SortKey], List.empty[uic.SortKey]))
            ui = sortedTable.sort(sort).onSortChange(next => sort.get.map(held => seen.set((next, held)))).render
            headers <- elementsWithClass(ui, "p-datatable-sortable-column")
            _       <- press(headers(0), UI.Keyboard.Enter)
            s       <- seen.get
        yield assert(s._1.map(_.column) == List("Name") && s._1 == s._2, s"reported, held: $s")
    }

    "a sort bound one way with nothing to hear a press says so, and a constant one with onSortChange sorts" in {
        for
            held  <- Signal.initRef(List.empty[uic.SortKey])
            seen  <- Signal.initRef(List.empty[uic.SortKey])
            deaf  <- elementsWithClass(sortedTable.sort(held.readOnly).render, "p-uic-key-error")
            heard <- elementsWithClass(sortedTable.sort(held.readOnly).onSortChange(_ => ()).render, "p-uic-key-error")
            fixed = sortedTable.sort(List.empty[uic.SortKey]).onSortChange(seen.set).render
            headers <- elementsWithClass(fixed, "p-datatable-sortable-column")
            _       <- press(headers(0), UI.Keyboard.Enter)
            s       <- seen.get
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onSortChange"), s"a sort nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty, s"a sort with somewhere to go: ${heard.map(textOf)}")
            assert(headers.size == 2 && s.map(_.column) == List("Name"), s"a constant sort reports a press: $s")
        end for
    }

    "restore writes a two-way sort and leaves a one-way sort to its owner" in {
        val restored = List(uic.SortKey("Price", uic.SortDirection.Ascending))
        for
            twoWay <- Signal.initRef(List.empty[uic.SortKey])
            oneWay <- Signal.initRef(List.empty[uic.SortKey])
            _      <- sortedTable.sort(twoWay).restore(TableState(sort = restored))
            _      <- sortedTable.sort(oneWay.readOnly).onSortChange(_ => ()).restore(TableState(sort = restored))
            t      <- twoWay.get
            o      <- oneWay.get
        yield assert(t == restored && o.isEmpty, s"two-way $t, one-way $o")
        end for
    }

    "a selectable row selects on Enter and on Space" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selection(sel).render
            rows       <- elementsWithClass(ui, "p-datatable-selectable-row")
            _          <- press(rows(0), UI.Keyboard.Enter)
            afterEnter <- sel.get
            _          <- press(rows(1), UI.Keyboard.Space)
            afterSpace <- sel.get
        yield
            assert(afterEnter == rowsWith("1"), "Enter picks the row it was pressed on")
            assert(afterSpace == rowsWith("1", "2"), "and Space picks the next one")
    }

    "a key that is not an activation leaves the selection alone" in {
        for
            sel <- Signal.initRef(Set.empty[Item])
            ui = uic.DataTable[Item]().rows(items).rowKey(_.id)
                .columns(uic.column("Name")(_.name))
                .selectionMode(uic.SelectionMode.Multiple).selection(sel).render
            rows  <- elementsWithClass(ui, "p-datatable-selectable-row")
            _     <- press(rows(0), UI.Keyboard.ArrowDown)
            after <- sel.get
        yield assert(after.isEmpty)
    }

    // ── the anchor the caller owns ──────────────────────────────────────────────────

    /** The app shape that found this: modifier selection, a bound anchor, rendered through
      * `render` rather than the `wired` seam, so the anchor takes the route a live table takes.
      */
    private def rangingTable(sel: SignalRef[Set[Item]], anchor: SignalRef[Maybe[Item]])(using Frame): UI =
        uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
            .selectionMode(SelectionMode.Multiple).selection(sel).metaKeySelection(true)
            .selectionAnchor(anchor).render

    "an anchor the caller bound survives the table being rendered again; a minted one cannot" in {
        // Two independent renders of the same table is what a page re-render produces, and it is
        // where a minted anchor is lost: it lives in the table's mount, whose state ends with the
        // region enclosing it. Bound, the range is measured across the gap.
        for
            sel    <- Signal.initRef(Set.empty[Item])
            anchor <- Signal.initRef(Absent: Maybe[Item])
            trs1   <- bodyTrs(rangingTable(sel, anchor))
            _      <- click(trs1.head)
            held   <- anchor.get
            trs2   <- bodyTrs(rangingTable(sel, anchor))
            _      <- clickWith(trs2(2), UI.Modifiers(shift = true))
            got    <- sel.get
        yield
            assert(held.contains(four.head), "the plain click moved the anchor the caller holds")
            assert(got == rowsWith("1", "2", "3"), "and the next render measured its range from there")
    }

    private def rangingOver(sel: SignalRef[Set[Item]])(using Frame) =
        uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
            .selectionMode(SelectionMode.Multiple).selection(sel).metaKeySelection(true)

    "a constant anchor nobody hears is fixed: a shift-click ranges from it, and a plain click leaves it" in {
        for
            sel  <- Signal.initRef(Set.empty[Item])
            trs  <- bodyTrs(rangingOver(sel).selectionAnchor(Present(four(1))).render)
            _    <- click(trs.head)
            _    <- clickWith(trs(3), UI.Modifiers(shift = true))
            got  <- sel.get
            card <- cards(rangingOver(sel).selectionAnchor(Present(four(1))).render)
        yield
            assert(got == rowsWith("2", "3", "4"), s"the range starts at the constant row: $got")
            assert(card.isEmpty, s"a fixed anchor is not a mistake: $card")
        end for
    }

    "an anchor bound one way is ranged from, and a plain click reported without being written" in {
        for
            sel  <- Signal.initRef(Set.empty[Item])
            held <- Signal.initRef(Present(four(1)): Maybe[Item])
            seen <- Signal.initRef(List.empty[Maybe[Item]])
            table = rangingOver(sel).selectionAnchor(held.readOnly).onSelectionAnchorChange(a => seen.getAndUpdate(_ :+ a).unit)
            trs  <- bodyTrs(table.render)
            _    <- clickWith(trs(3), UI.Modifiers(shift = true))
            got  <- sel.get
            _    <- click(trs.head)
            h    <- held.get
            told <- seen.get
            deaf <- cards(rangingOver(sel).selectionAnchor(held.readOnly).render)
        yield
            assert(got == rowsWith("2", "3", "4"), s"the range starts at the signal's row: $got")
            assert(h == Present(four(1)) && told == List(Present(four.head)), s"held $h, reported $told")
            assert(deaf.contains("onSelectionAnchorChange"), s"nobody hears: $deaf")
        end for
    }

    "an anchor ref is written before the plain click is reported" in {
        for
            sel    <- Signal.initRef(Set.empty[Item])
            anchor <- Signal.initRef(Absent: Maybe[Item])
            seen   <- Signal.initRef(List.empty[(Maybe[Item], Maybe[Item])])
            table = rangingOver(sel).selectionAnchor(anchor)
                .onSelectionAnchorChange(a => anchor.get.map(h => seen.getAndUpdate(_ :+ (a, h)).unit))
            trs  <- bodyTrs(table.render)
            _    <- click(trs(2))
            told <- seen.get
        yield assert(told == List((Present(four(2)), Present(four(2)))), s"reported, held: $told")
    }

    "a bound anchor is the only state a ranging table needed, so it stays a pure render" in {
        for
            sel    <- Signal.initRef(Set.empty[Item])
            anchor <- Signal.initRef(Absent: Maybe[Item])
        yield rangingTable(sel, anchor) match
            case m: Ast.Mounted => fail("a bound anchor should not force a mount")
            case _              => succeed
    }

    "an unbound ranging table still mints its anchor in a mount, as it always did" in {
        for sel <- Signal.initRef(Set.empty[Item])
        yield
            val ui = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
                .selectionMode(SelectionMode.Multiple).selection(sel).metaKeySelection(true).render
            ui match
                case _: Ast.Mounted => succeed
                case other          => fail(s"a table that mints its anchor renders as a mount; got: $other")
    }

    // ── the mount's identity ────────────────────────────────────────────────────────
    //
    // Everything behind this mount is state the table owns and no caller can hand back: the
    // shift anchor, the column grab and drag, the row drag, the scroll offset, the measured
    // frozen-header height. Keyless, the mount's effect re-runs on every emission of whatever
    // region encloses the table, and all of it is re-allocated, so a shift-click reads an
    // anchor ref that no click ever wrote, which is what a range failing on a live page looks
    // like. The three cases below pin the identity that stops it.

    /** A table that owns state, so it renders through the mount rather than as a plain tree. */
    private def stateful(id: Maybe[String])(using Frame): UI =
        val t = uic.DataTable[Item]().rows(four).rowKey(_.id).columns(uic.column("Name")(_.name))
            .selectionMode(SelectionMode.Multiple).metaKeySelection(true)
            .onSelectionChange(_ => ()).selection(Set.empty[Item])
        (id match
            case Present(v) => t.id(v)
            case Absent     => t
        ).render
    end stateful

    "a table the caller named keeps its instance, and an unnamed one stays as it was" in {
        // `key` is `Maybe[Any]` and Any is not comparable under strict equality, so the keys are
        // held against each other by their rendered form rather than by ==.
        (stateful(Present("t1")), stateful(Absent)) match
            case (n: Ast.Mounted, a: Ast.Mounted) =>
                assert(n.key.map(_.toString).contains("t1"), "the id is the key")
                assert(a.key.isEmpty, "and no id means no key, as before")
            case other => fail(s"a stateful DataTable renders as a mount; got: $other")
        end match
    }

    "two renders of the same named table carry the same key, which is the whole point" in {
        // A re-render builds the value again from scratch; this is that, and the key has to
        // survive it or the instance behind the mount does not.
        (stateful(Present("t1")), stateful(Present("t1"))) match
            case (x: Ast.Mounted, y: Ast.Mounted) =>
                assert(x.key.map(_.toString) == y.key.map(_.toString))
                assert(x.key.isDefined)
            case other => fail(s"a stateful DataTable renders as a mount; got: $other")
        end match
    }

    "two named tables on one page take two distinct keys" in {
        (stateful(Present("t1")), stateful(Present("t2"))) match
            case (x: Ast.Mounted, y: Ast.Mounted) =>
                assert(x.key.map(_.toString) != y.key.map(_.toString))
            case other => fail(s"a stateful DataTable renders as a mount; got: $other")
        end match
    }

    "a commit whose onCellValueChanged fails still writes the row and closes the editor" in {
        for
            rows    <- Signal.initRef[Seq[Item]](items)
            editing <- Signal.initRef(Absent: Maybe[CellPath[Item]])
            name    <- Signal.initRef("")
            err     <- Signal.initRef(Absent: Maybe[(CellKey, FieldError)])
            ui = uic.DataTable[Item]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
            ).editingCell(editing).onCellValueChanged(_ => Abort.fail("rejected"))
                .wired("t", Map(List("Name") -> name), err, _ => ())
            cell  <- elementWithId(ui, "t-c0-0")
            _     <- press(cell, UI.Keyboard.Enter)
            open  <- elementWithId(ui, "t-c0-0")
            _     <- typeInto(open, "Ada")
            again <- elementWithId(ui, "t-c0-0")
            _     <- press(again, UI.Keyboard.Enter)
            after <- rows.get
            still <- editing.get
        yield assert(after.head.name == "Ada" && still.isEmpty, s"the table finished its own work; editing=$still")
    }

    private def bare(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).sortBy(_.name),
            uic.column("Price")(_.price.toString)
        )

    "constants nobody hears are fixed: no header sorts, no row picks, no toggle opens, and no card" in {
        val fixed = bare.sort(List.empty[uic.SortKey]).selectionMode(uic.SelectionMode.Single).selection(Set(items(0)))
            .expanded(Set(items(0))).rowExpansionTemplate(i => span(i.name))
        for
            headers <- elementsWithClass(fixed.render, "p-datatable-sortable-column")
            rows    <- elementsWithClass(fixed.render, "p-datatable-selectable-row")
            toggles <- elementsWithClass(fixed.render, "p-datatable-row-toggle-button")
            card    <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(headers.isEmpty, "a fixed sort offers no header")
            assert(rows.isEmpty, "a fixed selection offers no row")
            assert(toggles.nonEmpty && toggles.forall(t => !declaresClick(t)), "a fixed expansion opens nothing")
            assert(card.isEmpty, s"fixed constants: ${card.map(textOf)}")
        end for
    }

    "an unbound sort is the table's own, so a header press sorts" in {
        // The header's sort state rides the attribute channel, which no region publishes.
        for
            sorted <- attrPatchedAfter(bare.render, t => elementsWithClass(t, "p-datatable-sortable-column").map(_.exists(declaresClick)))(
                t => elementWithClass(t, "p-datatable-sortable-column").map(click)
            )((name, value) => name == "aria-sort" && value == "ascending")
        yield assert(sorted, "the table never sorted by the header the reader pressed")
    }

    "an unbound selection is the table's own, so a row press picks the row" in {
        for
            after <- publishedAfterReady(
                bare.selectionMode(uic.SelectionMode.Single).render,
                t => elementsWithClass(t, "p-datatable-selectable-row").map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-datatable-row-selected").map(_.nonEmpty)
            )(t => elementsWithClass(t, "p-datatable-selectable-row").map(rs => click(rs.head)))
        yield assert(after.nonEmpty, "the table never showed the row the reader picked")
    }

    "an unbound expansion is the table's own, so a toggle opens the row" in {
        for
            after <- publishedAfterReady(
                bare.rowExpansionTemplate(i => span(i.name)).render,
                t => elementsWithClass(t, "p-datatable-row-toggle-button").map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-datatable-row-expansion").map(_.nonEmpty)
            )(t => elementsWithClass(t, "p-datatable-row-toggle-button").map(ts => click(ts.head)))
        yield assert(after.nonEmpty, "the table never opened the row the reader toggled")
    }

    "a column that filters with nothing bound keeps the filters itself, and says nothing" in {
        for card <- elementsWithClass(bare.columns(uic.column("Name")(_.name).filterBy).render, "p-uic-key-error")
        yield assert(card.isEmpty, s"an unbound filter is the table's own: ${card.map(textOf)}")
    }

    private def threeColumns(using Frame) =
        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name),
            uic.column("Price")(_.price.toString),
            uic.column("Id")(_.id)
        )

    // The drag itself measures the columns, which this harness does not answer; that path is the two-way ref's, held above.
    "columnWidths() with no ref turns the boundary drag on, keeps the widths itself, and says nothing" in {
        val table = threeColumns.columnWidths()
        for
            static <- resizers(table.render)
            card   <- elementsWithClass(table.render, "p-uic-key-error")
            live   <- publishedAfterReady(table.render, t => resizers(t).map(_.exists(_.attrs.onPointerDown.isDefined)))()
            off    <- resizers(threeColumns.render)
        yield
            assert(static.size == 2, s"the page as first rendered already shows both boundaries: ${static.size}")
            assert(live.nonEmpty, "the mounted table offers a grab on its boundaries")
            assert(card.isEmpty, s"widths the table keeps itself: ${card.map(textOf)}")
            assert(off.isEmpty, "a table nobody asked for the drag offers none")
        end for
    }

    "columnOrder() with no ref turns the header drag on, keeps the order itself, and says nothing" in {
        val table         = threeColumns.columnOrder()
        def movers(t: UI) = elementsWithClass(t, "p-datatable-reorderable-column")
        for
            static <- movers(table.render)
            card   <- elementsWithClass(table.render, "p-uic-key-error")
            live   <- publishedAfterReady(table.render, t => movers(t).map(_.exists(_.attrs.onPointerDown.isDefined)))()
            off    <- movers(threeColumns.render)
        yield
            assert(static.size == 3, s"the page as first rendered already marks every header: ${static.size}")
            assert(live.nonEmpty, "the mounted table offers a grab on its headers")
            assert(card.isEmpty, s"an order the table keeps itself: ${card.map(textOf)}")
            assert(off.isEmpty, "a table nobody asked for the drag offers none")
        end for
    }

    "expandedGroups() with no ref keeps the open groups itself, so a toggle opens the group" in {
        val table          = grouped.expandedGroups()
        def toggles(t: UI) = elementsWithClass(t, "p-datatable-row-toggle-button")
        for
            closed <- bodyNames(table.render)
            card   <- elementsWithClass(table.render, "p-uic-key-error")
            after  <- publishedAfterReady(
                table.render,
                t => toggles(t).map(_.exists(declaresClick)),
                until = t =>
                    for
                        ts    <- toggles(t)
                        names <- bodyNames(t)
                    yield ts.exists(_.attrs.ariaAttrs.get("expanded").contains("true")) || names.count(_ == "A") == 2
            )(t => toggles(t).map(ts => click(ts.head)))
        yield
            assert(closed == Chunk("A", "B"), s"every group starts collapsed: $closed")
            assert(after.nonEmpty, "the table never opened the group the reader toggled")
            assert(card.isEmpty, s"groups the table keeps itself: ${card.map(textOf)}")
        end for
    }

    "the last call wins between a ref and the table's own" in {
        for
            held  <- Signal.initRef(Map(List("Name") -> 120.0))
            card  <- elementsWithClass(threeColumns.columnWidths(held.readOnly).columnWidths().render, "p-uic-key-error")
            bound <- elementsWithClass(threeColumns.columnWidths().columnWidths(held.readOnly).render, "p-uic-key-error")
        yield
            assert(card.isEmpty, s"the table's own replaces the one-way binding: ${card.map(textOf)}")
            assert(
                bound.map(textOf).mkString.contains("onColumnWidthsChange"),
                s"the binding replaces the table's own: ${bound.map(textOf)}"
            )
        end for
    }

end DataTableTest
