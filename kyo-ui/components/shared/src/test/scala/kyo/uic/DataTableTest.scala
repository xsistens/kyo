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
            case _                       => Chunk.empty

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

end DataTableTest
