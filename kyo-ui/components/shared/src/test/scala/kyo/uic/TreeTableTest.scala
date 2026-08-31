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
            ui = tree.expanded(expanded).wired("tt", id => moved.set(Present(id)))
            rows  <- navRows(ui)
            _     <- press(rows(at), key)
            to    <- moved.get
            after <- expanded.get
        yield (to, after)

    "a collapsed subtree contributes no rows, so the cursor walks what is on the screen" in {
        for
            expanded <- Signal.initRef(Set.empty[String])
            shut = tree.expanded(expanded).wired("tt", _ => ())
            closedRows <- navRows(shut)
            _          <- expanded.set(Set("ada"))
            openRows   <- navRows(tree.expanded(expanded).wired("tt", _ => ()))
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
                .wired("tt", _ => ())
            rows   <- navRows(ui)
            _      <- press(rows(0), UI.Keyboard.Enter)
            picked <- sel.get
            open   <- expanded.get
        yield assert(picked == Set("ada") && open == Set("ada"))
    }

    "the treegrid is one tab stop, and the arrows are what reach the rest" in {
        for
            expanded <- Signal.initRef(Set("ada"))
            ui       <- Kyo.lift(tree.expanded(expanded).wired("tt", _ => ()))
            rows     <- navRows(ui)
        yield
            assert(rows.head.attrs.tabIndex.contains(0), "the first row takes the tab stop")
            assert(rows.tail.forall(_.attrs.tabIndex.contains(-1)), "and the rest are out of the Tab order")
            assert(rows.forall(_.attrs.onKeyDown.isDefined), "every row answers the arrows")
    }

end TreeTableTest
