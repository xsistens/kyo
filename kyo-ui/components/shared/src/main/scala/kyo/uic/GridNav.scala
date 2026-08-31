package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers

/** Pure keyboard-navigation state machine for [[DataTable]]'s cell grid, in the shape
  * `MenuNav` and `MegaNav` already use for the menu family: it takes the grid's shape, the
  * cursor and one key, and returns the [[GridNav.Step]] the host maps onto focus and
  * editing. No DOM, no effects, no component.
  *
  * The cursor is a position and not an identity, because that is what a reader means by
  * "the cell below this one". Which row a position denotes is the host's business, and it
  * counts the rows that are actually on the screen: a collapsed group renders none of its
  * rows, so a cursor stepping by paged index would land on a row nobody can see.
  *
  * `Absent` means the key is not ours, so the browser keeps it. That is the whole reason
  * arrows do nothing while a cell is open: the caret and the number stepper own them, and
  * a grid that stole them would make an editor unusable.
  */
private[uic] object GridNav:

    /** A cell by position: row among the VISIBLE rows, column among the leaf columns. */
    final case class Pos(row: Int, col: Int) derives CanEqual

    /** The grid a key is read against. `editable` answers per cell, so a column gated by
      * `Column.editableWhen` is skipped for the rows it rejects and not for the others;
      * `navigable` answers per column, and a column that answers false is stepped over
      * rather than landed on.
      */
    final case class Grid(
        rows: Int,
        cols: Int,
        editable: Pos => Boolean,
        page: Int,
        navigable: Pos => Boolean = _ => true
    )

    /** What one key does to the edit that is open, or to the one that is not. */
    enum EditOp derives CanEqual:
        case Keep
        case Open(seed: Maybe[String])
        case Commit
        case Cancel
    end EditOp

    /** The outcome of one key: where the cursor goes, what happens to the edit, and
      * whether the row is being selected.
      *
      * `focus` is `Absent` when the cursor stays where it is. Tab is the one key that
      * moves the cursor without asking for focus to be MOVED, since the browser is already
      * doing that: the tab stops are the editable cells, so its order and this one agree.
      */
    final case class Step(
        focus: Maybe[Pos] = Absent,
        edit: EditOp = EditOp.Keep,
        selectRow: Boolean = false,
        moveFocus: Boolean = true
    ) derives CanEqual

    /** The next editable cell in reading order, wrapping into the next row and stopping at
      * the ends of the grid. `backwards` is Shift-Tab.
      */
    def nextEditable(g: Grid, at: Pos, backwards: Boolean): Maybe[Pos] =
        val total = g.rows * g.cols
        val start = at.row * g.cols + at.col
        val step  = if backwards then -1 else 1
        @annotation.tailrec
        def loop(i: Int): Maybe[Pos] =
            if i < 0 || i >= total then Absent
            else
                val p = Pos(i / g.cols, i % g.cols)
                if g.editable(p) then Present(p) else loop(i + step)
        if g.cols <= 0 then Absent else loop(start + step)
    end nextEditable

    /** Reads one key against the grid. `editing` is whether the cursor's own cell is open. */
    def onKey(g: Grid, at: Pos, editing: Boolean, key: Keyboard, mods: Modifiers): Maybe[Step] =
        if g.rows <= 0 || g.cols <= 0 then Absent
        else if editing then whileEditing(g, at, key, mods)
        else whileResting(g, at, key, mods)

    private def whileEditing(g: Grid, at: Pos, key: Keyboard, mods: Modifiers): Maybe[Step] =
        key match
            case Keyboard.Enter  => Present(Step(edit = EditOp.Commit))
            case Keyboard.Escape => Present(Step(edit = EditOp.Cancel))
            // Tab commits and carries the edit into the next editable cell, which is where
            // the browser is taking focus anyway. Nothing is prevented, so nothing has to
            // be prevented synchronously: the tab stops ARE the editable cells.
            case Keyboard.Tab =>
                nextEditable(g, at, mods.shift) match
                    case Present(next) =>
                        Present(Step(focus = Present(next), edit = EditOp.Commit, moveFocus = false))
                    case Absent => Present(Step(edit = EditOp.Commit, moveFocus = false))
            case _ => Absent

    private def whileResting(g: Grid, at: Pos, key: Keyboard, mods: Modifiers): Maybe[Step] =
        // A vertical move keeps the column, so it needs no search: a column is navigable
        // for every row or for none. A horizontal one steps over the columns that are not,
        // and stays put when there is nothing past them.
        def move(p: Pos): Maybe[Step]     = Present(Step(focus = Present(clamp(g, p))))
        def across(dir: Int): Maybe[Step] = Present(Step(focus = Present(seek(g, at, dir))))
        key match
            case Keyboard.ArrowUp    => move(at.copy(row = at.row - 1))
            case Keyboard.ArrowDown  => move(at.copy(row = at.row + 1))
            case Keyboard.ArrowLeft  => across(-1)
            case Keyboard.ArrowRight => across(1)
            // Ctrl or Cmd takes Home and End to the ends of the GRID, as a spreadsheet
            // does; on their own they are the ends of the row.
            case Keyboard.Home =>
                if mods.ctrl || mods.meta then move(edge(g, Pos(0, 0), 1)) else move(edge(g, at.copy(col = 0), 1))
            case Keyboard.End =>
                if mods.ctrl || mods.meta then move(edge(g, Pos(g.rows - 1, g.cols - 1), -1))
                else move(edge(g, at.copy(col = g.cols - 1), -1))
            case Keyboard.PageUp   => move(at.copy(row = at.row - g.page))
            case Keyboard.PageDown => move(at.copy(row = at.row + g.page))
            case Keyboard.Enter | Keyboard.F2 =>
                if g.editable(at) then Present(Step(edit = EditOp.Open(Absent))) else Absent
            // A printable key opens the editor ON that character, which is what makes
            // typing over a cell replace it. Backspace and Delete open it empty.
            case Keyboard.Backspace | Keyboard.Delete =>
                if g.editable(at) then Present(Step(edit = EditOp.Open(Present("")))) else Absent
            // Space is the one printable key that does not, because in a grid it is the
            // selection key: that is what AG Grid does and what the ARIA grid pattern says,
            // and it is the reason Enter and Space part ways here while they mean the same
            // thing on every button-shaped row in the library. A reader who wants a leading
            // space in a cell opens the editor with Enter or F2 first.
            case Keyboard.Space => Present(Step(selectRow = true))
            case k =>
                k.charValue match
                    case Present(c) if !mods.ctrl && !mods.alt && !mods.meta && g.editable(at) =>
                        Present(Step(edit = EditOp.Open(Present(c))))
                    case _ => Absent
        end match
    end whileResting

    /** Cells outside the grid do not exist, so a key that would leave it stops at the edge.
      * AG Grid does the same: an arrow at a boundary has no effect rather than wrapping,
      * since a wrap would move the reader somewhere they did not aim at.
      */
    private def clamp(g: Grid, p: Pos): Pos =
        Pos(math.min(math.max(p.row, 0), g.rows - 1), math.min(math.max(p.col, 0), g.cols - 1))

    /** The nearest navigable column from `from` in direction `dir`, or `from` itself when
      * there is none past the locked ones: a column nobody may land on absorbs no key, it
      * is passed over, and running out of columns leaves the cursor where it started
      * rather than on the last cell it stepped across.
      */
    private def seek(g: Grid, from: Pos, dir: Int): Pos =
        @annotation.tailrec
        def loop(at: Pos): Pos =
            val next = at.copy(col = at.col + dir)
            if next.col < 0 || next.col >= g.cols then from
            else if g.navigable(next) then next
            else loop(next)
        end loop
        loop(from)
    end seek

    /** The first navigable column at or after `from`, scanning by `dir`. */
    @annotation.tailrec
    private def edge(g: Grid, from: Pos, dir: Int): Pos =
        if g.navigable(from) then from
        else
            val next = from.copy(col = from.col + dir)
            if next.col < 0 || next.col >= g.cols then from else edge(g, next, dir)

end GridNav
