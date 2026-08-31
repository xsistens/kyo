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

end TreeTableTest
