package kyo.uic

import kyo.*
import kyo.UI.*

/** The tab stops [[TreeTable]] makes itself.
  *
  * A `th` and a `tr` are not controls, so a table that gives them a tab stop owes them the keys
  * too. Without these the reader could reach every sortable header and every selectable row and
  * operate none of them.
  */
class TreeTableTest extends UicTest:

    final private case class Row(id: String, name: String) derives CanEqual

    // Two roots rather than a parent and a child: a collapsed subtree draws no row, and the
    // second row is what shows that a key acts on the row it was pressed on.
    private val roots = List(uic.TreeTableNode(Row("r1", "Ada")), uic.TreeTableNode(Row("r2", "Grace")))

    private def table(using Frame) =
        uic.TreeTable[Row]().nodes(roots*).rowKey(_.id).columns(uic.column("Name")(_.name).sortBy(_.name))

    "a sortable header sorts on Enter and on Space, and carries the modifiers over" in {
        for
            sort <- Signal.initRef(List.empty[uic.SortKey])
            ui = table.sort(sort).render
            header     <- elementWithClass(ui, "p-treetable-sortable-column")
            _          <- press(header, UI.Keyboard.Enter)
            afterEnter <- sort.get
            _          <- press(header, UI.Keyboard.Space)
            afterSpace <- sort.get
        yield
            assert(afterEnter.map(_.column) == List("Name"), "Enter sorts by that column")
            assert(afterSpace.head.direction != afterEnter.head.direction, "Space cycles it, as a second click would")
    }

    "a selectable row selects on Enter and on Space" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = table.selectionMode(uic.SelectionMode.Multiple).selected(sel).render
            rows       <- elementsWithClass(ui, "p-treetable-selectable-row")
            _          <- press(rows(0), UI.Keyboard.Enter)
            afterEnter <- sel.get
            _          <- press(rows(1), UI.Keyboard.Space)
            afterSpace <- sel.get
        yield
            assert(afterEnter == Set("r1"), "Enter picks the row it was pressed on")
            assert(afterSpace == Set("r1", "r2"), "and Space picks the next one")
    }

    "a key that is not an activation leaves the selection alone" in {
        for
            sel <- Signal.initRef(Set.empty[String])
            ui = table.selectionMode(uic.SelectionMode.Multiple).selected(sel).render
            rows  <- elementsWithClass(ui, "p-treetable-selectable-row")
            _     <- press(rows(0), UI.Keyboard.ArrowDown)
            after <- sel.get
        yield assert(after.isEmpty)
    }

    // ---- the treegrid cursor ----
    //
    // A treegrid is ONE tab stop, and the arrows are what reach the rest of it. The cursor sits on
    // ROWS rather than cells, which the ARIA pattern allows and which is what this table can do:
    // it has no cell-level operation to move a cursor to. The semantics are TreeNav's, unchanged
    // from Tree, so only the wiring is asserted here.

    private val family = List(
        uic.TreeTableNode(
            Row("ada", "Ada"),
            children = List(uic.TreeTableNode(Row("grace", "Grace")), uic.TreeTableNode(Row("alan", "Alan")))
        ),
        uic.TreeTableNode(Row("barbara", "Barbara"))
    )

    private def tree(using Frame) =
        uic.TreeTable[Row]().nodes(family*).rowKey(_.id).columns(uic.column("Name")(_.name))

    /** The wired rows, in screen order: a row carries no class of its own, so its stamped id is
      * what names it.
      */
    private def navRows(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(ui).map(_.filter(_.attrs.identifier.exists(_.startsWith("tt-r"))))

    /** Presses `key` on the row at `at`, returning where focus went and what is open afterwards. */
    private def drive(at: Int, key: UI.Keyboard, open: Set[String] = Set.empty)(using
        Frame
    ): (Maybe[String], Set[String]) < Async =
        for
            expanded <- Signal.initRef(open)
            moved    <- Signal.initRef(Absent: Maybe[String])
            ui = tree.expanded(expanded).wired("tt", Absent, id => moved.set(Present(id)))
            rows  <- navRows(ui)
            _     <- press(rows(at), key)
            to    <- moved.get
            after <- expanded.get
        yield (to, after)

    "a collapsed subtree contributes no rows, so the cursor walks what is on the screen" in {
        for
            expanded <- Signal.initRef(Set.empty[String])
            shut = tree.expanded(expanded).wired("tt", Absent, _ => ())
            closedRows <- navRows(shut)
            _          <- expanded.set(Set("ada"))
            openRows   <- navRows(tree.expanded(expanded).wired("tt", Absent, _ => ()))
        yield
            assert(closedRows.size == 2, "two roots while everything is shut")
            assert(openRows.size == 4, "and the two children join them when the first root opens")
    }

    "ArrowDown walks to the next visible row" in
        drive(0, UI.Keyboard.ArrowDown).map((to, _) => assert(to == Present("tt-r1")))

    "ArrowUp walks back" in
        drive(1, UI.Keyboard.ArrowUp).map((to, _) => assert(to == Present("tt-r0")))

    "ArrowRight opens a closed parent without moving off it" in
        drive(0, UI.Keyboard.ArrowRight).map { (to, open) =>
            assert(open == Set("ada"), "the parent opens")
            assert(to == Absent, "and the cursor stays on it, so the second press can step in")
        }

    "ArrowRight again steps into the children it just showed" in
        drive(0, UI.Keyboard.ArrowRight, open = Set("ada")).map((to, _) => assert(to == Present("tt-r1")))

    "ArrowLeft closes an open parent" in
        drive(0, UI.Keyboard.ArrowLeft, open = Set("ada")).map((_, open) => assert(open.isEmpty))

    "ArrowLeft from a child climbs to its parent" in
        drive(1, UI.Keyboard.ArrowLeft, open = Set("ada")).map((to, open) =>
            assert(to == Present("tt-r0") && open == Set("ada"), "it climbs rather than closing anything")
        )

    "Home and End reach the ends of what is visible" in {
        for
            (home, _) <- drive(3, UI.Keyboard.Home, open = Set("ada"))
            (end, _)  <- drive(0, UI.Keyboard.End, open = Set("ada"))
        yield assert(home == Present("tt-r0") && end == Present("tt-r3"))
    }

    "Enter selects the row and opens a parent with it" in {
        for
            expanded <- Signal.initRef(Set.empty[String])
            sel      <- Signal.initRef(Set.empty[String])
            ui = tree.expanded(expanded).selectionMode(uic.SelectionMode.Multiple).selected(sel)
                .wired("tt", Absent, _ => ())
            rows   <- navRows(ui)
            _      <- press(rows(0), UI.Keyboard.Enter)
            picked <- sel.get
            open   <- expanded.get
        yield assert(picked == Set("ada") && open == Set("ada"))
    }

    "the treegrid is one tab stop, and the arrows are what reach the rest" in {
        for
            expanded <- Signal.initRef(Set("ada"))
            ui       <- Kyo.lift(tree.expanded(expanded).wired("tt", Absent, _ => ()))
            rows     <- navRows(ui)
        yield
            assert(rows.head.attrs.tabIndex.contains(0), "the first row takes the tab stop")
            assert(rows.tail.forall(_.attrs.tabIndex.contains(-1)), "and the rest are out of the Tab order")
            assert(rows.forall(_.attrs.onKeyDown.isDefined), "every row answers the arrows")
    }

    "the tab stop follows the cursor, so a Tab away and back returns where the reader stood" in {
        for
            expanded <- Signal.initRef(Set("ada"))
            cursor   <- Signal.initRef(Absent: Maybe[String])
            ui = tree.expanded(expanded).wired("tt", Present(cursor), _ => ())
            rows  <- navRows(ui)
            _     <- press(rows(0), UI.Keyboard.ArrowDown)
            at    <- cursor.get
            after <- navRows(ui)
        yield
            assert(at == Present("grace"), "the arrow leaves the cursor on the row it moved to")
            assert(after(1).attrs.tabIndex.contains(0), "which is the row that now holds the tab stop")
            assert(after(0).attrs.tabIndex.contains(-1), "and the first row has given it up")
    }

    "the tab stop starts on the chosen row, so a table opens where the choice is" in {
        for
            expanded <- Signal.initRef(Set("ada"))
            chosen   <- Signal.initRef(Set("alan"))
            cursor   <- Signal.initRef(Absent: Maybe[String])
            ui = tree.expanded(expanded).selectionMode(uic.SelectionMode.Single).selected(chosen)
                .wired("tt", Present(cursor), _ => ())
            rows <- navRows(ui)
        yield
            assert(rows(2).attrs.tabIndex.contains(0), "the selected row is where the keyboard picks up")
            assert(rows(0).attrs.tabIndex.contains(-1), "not the first row, which the reader did not choose")
    }

    "a click seeds the cursor, so the keyboard carries on from the row the pointer chose" in {
        for
            expanded <- Signal.initRef(Set("ada"))
            cursor   <- Signal.initRef(Absent: Maybe[String])
            ui = tree.expanded(expanded).wired("tt", Present(cursor), _ => ())
            rows <- navRows(ui)
            _    <- click(rows(2))
            at   <- cursor.get
        yield assert(at == Present("alan"), "a table with nothing to select still follows the pointer")
    }

    "a treegrid that takes several rows says so" in {
        for
            chosen <- Signal.initRef(Set.empty[String])
            multi  = tree.selectionMode(uic.SelectionMode.Multiple).selected(chosen).wired("tt", Absent, _ => ())
            single = tree.selectionMode(uic.SelectionMode.Single).selected(chosen).wired("tt", Absent, _ => ())
            m <- elementWithClass(multi, "p-treetable-table")
            s <- elementWithClass(single, "p-treetable-table")
        yield
            assert(m.attrs.ariaAttrs.get("multiselectable").contains("true"))
            assert(s.attrs.ariaAttrs.get("multiselectable").isEmpty, "one row at a time is the default and says nothing")
    }

end TreeTableTest
