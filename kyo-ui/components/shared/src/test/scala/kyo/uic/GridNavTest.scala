package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers
import kyo.uic.GridNav.EditOp
import kyo.uic.GridNav.Grid
import kyo.uic.GridNav.Pos
import kyo.uic.GridNav.Step

/** The pure key map behind the table's cell grid. What the steps turn into (a focus
  * command, an open editor, a committed value) is pinned in GoldenRenderTest and in the
  * demo.
  */
class GridNavTest extends UicTest:

    private val none  = Modifiers.none
    private val ctrl  = Modifiers(ctrl = true)
    private val shift = Modifiers(shift = true)

    /** Three rows, three columns, the middle column editable everywhere. */
    private val grid = Grid(rows = 3, cols = 3, editable = p => p.col == 1, page = 2)

    private def key(k: Keyboard, at: Pos = Pos(1, 1), editing: Boolean = false, mods: Modifiers = none) =
        GridNav.onKey(grid, at, editing, k, mods)

    "arrows move one cell" in {
        assert(key(Keyboard.ArrowUp) == Present(Step(focus = Present(Pos(0, 1)))))
        assert(key(Keyboard.ArrowDown) == Present(Step(focus = Present(Pos(2, 1)))))
        assert(key(Keyboard.ArrowLeft) == Present(Step(focus = Present(Pos(1, 0)))))
        assert(key(Keyboard.ArrowRight) == Present(Step(focus = Present(Pos(1, 2)))))
    }

    // A wrap would move the reader somewhere they did not aim at, so an edge absorbs.
    "an arrow at the edge stays where it is" in {
        assert(key(Keyboard.ArrowUp, at = Pos(0, 1)) == Present(Step(focus = Present(Pos(0, 1)))))
        assert(key(Keyboard.ArrowLeft, at = Pos(1, 0)) == Present(Step(focus = Present(Pos(1, 0)))))
        assert(key(Keyboard.ArrowDown, at = Pos(2, 2)) == Present(Step(focus = Present(Pos(2, 2)))))
        assert(key(Keyboard.ArrowRight, at = Pos(2, 2)) == Present(Step(focus = Present(Pos(2, 2)))))
    }

    "Home and End are the row's ends, and the grid's under ctrl" in {
        assert(key(Keyboard.Home) == Present(Step(focus = Present(Pos(1, 0)))))
        assert(key(Keyboard.End) == Present(Step(focus = Present(Pos(1, 2)))))
        assert(key(Keyboard.Home, mods = ctrl) == Present(Step(focus = Present(Pos(0, 0)))))
        assert(key(Keyboard.End, mods = ctrl) == Present(Step(focus = Present(Pos(2, 2)))))
    }

    "a page is a page of rows, clamped" in {
        assert(key(Keyboard.PageDown, at = Pos(0, 1)) == Present(Step(focus = Present(Pos(2, 1)))))
        assert(key(Keyboard.PageUp, at = Pos(2, 1)) == Present(Step(focus = Present(Pos(0, 1)))))
        assert(key(Keyboard.PageDown, at = Pos(2, 1)) == Present(Step(focus = Present(Pos(2, 1)))), "clamped at the end")
    }

    "Enter and F2 open an editable cell, and do nothing over one that is not" in {
        assert(key(Keyboard.Enter) == Present(Step(edit = EditOp.Open(Absent))))
        assert(key(Keyboard.F2) == Present(Step(edit = EditOp.Open(Absent))))
        assert(key(Keyboard.Enter, at = Pos(1, 0)) == Absent, "a column with no pipeline keeps the key")
        assert(key(Keyboard.F2, at = Pos(1, 2)) == Absent)
    }

    "a printable key opens the editor on that character" in {
        assert(key(Keyboard.Char('x')) == Present(Step(edit = EditOp.Open(Present("x")))))
        assert(key(Keyboard.Space) == Present(Step(selectRow = true)), "Space selects the row instead")
        assert(key(Keyboard.Backspace) == Present(Step(edit = EditOp.Open(Present("")))), "Backspace opens it empty")
        assert(key(Keyboard.Delete) == Present(Step(edit = EditOp.Open(Present("")))))
    }

    // A chord is a command, not text: Ctrl-C over a cell must reach the browser.
    "a chord is not typing" in {
        assert(key(Keyboard.Char('c'), mods = ctrl) == Absent)
        assert(key(Keyboard.Char('x'), mods = Modifiers(alt = true)) == Absent)
        assert(key(Keyboard.Char('v'), mods = Modifiers(meta = true)) == Absent)
    }

    "a printable key over a cell that cannot open keeps the key" in {
        assert(key(Keyboard.Char('x'), at = Pos(1, 0)) == Absent)
    }

    // The caret and the number stepper own the arrows while a cell is open; a grid that
    // stole them would make an editor unusable.
    "while editing, only Enter, Escape and Tab are the grid's" in {
        assert(key(Keyboard.Enter, editing = true) == Present(Step(edit = EditOp.Commit)))
        assert(key(Keyboard.Escape, editing = true) == Present(Step(edit = EditOp.Cancel)))
        assert(key(Keyboard.ArrowLeft, editing = true) == Absent)
        assert(key(Keyboard.ArrowDown, editing = true) == Absent)
        assert(key(Keyboard.Home, editing = true) == Absent)
        assert(key(Keyboard.Char('x'), editing = true) == Absent)
    }

    "Tab commits and carries the edit to the next editable cell, without moving focus itself" in {
        assert(
            key(Keyboard.Tab, at = Pos(0, 1), editing = true) ==
                Present(Step(focus = Present(Pos(1, 1)), edit = EditOp.Commit, moveFocus = false))
        )
        assert(
            key(Keyboard.Tab, at = Pos(1, 1), editing = true, mods = shift) ==
                Present(Step(focus = Present(Pos(0, 1)), edit = EditOp.Commit, moveFocus = false))
        )
        assert(
            key(Keyboard.Tab, at = Pos(2, 1), editing = true) == Present(Step(edit = EditOp.Commit, moveFocus = false)),
            "the last editable cell commits and lets Tab leave the table"
        )
        assert(key(Keyboard.Tab) == Absent, "and a resting cell never sees it: the browser moves on")
    }

    "nextEditable reads in row order, wrapping into the next row" in {
        assert(GridNav.nextEditable(grid, Pos(0, 1), backwards = false) == Present(Pos(1, 1)))
        assert(GridNav.nextEditable(grid, Pos(0, 0), backwards = false) == Present(Pos(0, 1)))
        assert(GridNav.nextEditable(grid, Pos(2, 1), backwards = false) == Absent, "nothing after the last one")
        assert(GridNav.nextEditable(grid, Pos(0, 1), backwards = true) == Absent, "nor before the first")
        assert(GridNav.nextEditable(grid, Pos(2, 2), backwards = true) == Present(Pos(2, 1)))
    }

    "a grid with no editable cell has no next one" in {
        val flat = Grid(rows = 2, cols = 2, editable = _ => false, page = 1)
        assert(GridNav.nextEditable(flat, Pos(0, 0), backwards = false) == Absent)
        assert(GridNav.onKey(flat, Pos(0, 0), editing = false, Keyboard.Enter, none) == Absent)
        assert(
            GridNav.onKey(flat, Pos(0, 0), editing = false, Keyboard.ArrowDown, none) ==
                Present(Step(focus = Present(Pos(1, 0)))),
            "but it still navigates"
        )
    }

    // A column nobody may land on absorbs no key: the cursor steps over it, so a locked
    // column between two others never becomes a wall.
    "a non-navigable column is stepped over, not landed on" in {
        val locked                     = Grid(rows = 2, cols = 3, editable = _ => true, page = 1, navigable = p => p.col != 1)
        def move(k: Keyboard, at: Pos) = GridNav.onKey(locked, at, editing = false, k, none)
        assert(move(Keyboard.ArrowRight, Pos(0, 0)) == Present(Step(focus = Present(Pos(0, 2)))))
        assert(move(Keyboard.ArrowLeft, Pos(0, 2)) == Present(Step(focus = Present(Pos(0, 0)))))
        assert(move(Keyboard.End, Pos(0, 0)) == Present(Step(focus = Present(Pos(0, 2)))))
        assert(move(Keyboard.Home, Pos(0, 2)) == Present(Step(focus = Present(Pos(0, 0)))))
        // Vertical movement keeps the column, which a locked column never is.
        assert(move(Keyboard.ArrowDown, Pos(0, 0)) == Present(Step(focus = Present(Pos(1, 0)))))
    }

    "a locked column at the edge leaves the cursor where it is" in {
        val locked = Grid(rows = 1, cols = 2, editable = _ => true, page = 1, navigable = p => p.col == 0)
        assert(
            GridNav.onKey(locked, Pos(0, 0), editing = false, Keyboard.ArrowRight, none) ==
                Present(Step(focus = Present(Pos(0, 0))))
        )
        assert(
            GridNav.onKey(locked, Pos(0, 0), editing = false, Keyboard.End, none) ==
                Present(Step(focus = Present(Pos(0, 0)))),
            "End scans back to the last column anyone may land on"
        )
    }

    "an empty grid answers nothing" in {
        val empty = Grid(rows = 0, cols = 3, editable = _ => true, page = 1)
        assert(GridNav.onKey(empty, Pos(0, 0), editing = false, Keyboard.ArrowDown, none) == Absent)
        assert(GridNav.nextEditable(empty, Pos(0, 0), backwards = false) == Absent)
    }

end GridNavTest
