package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.uic.MenuNav.OpenOp
import kyo.uic.MenuNav.Step

/** Pure keyboard-navigation state machine for [[MegaMenu]] — a 2D grid, not the
  * nested tree [[MenuNav]] models, so it gets its own rules. Focus is a path:
  * `Nil` = none, `List(r)` = root `r` on the bar, `List(r, c, i)` = the `i`-th
  * navigable item of column `c` in root `r`'s panel. Returns a [[MenuNav.Step]]
  * (`OpenOp.OpenTo(List(r))` opens root r's panel, `Close` closes it) the host
  * maps onto its signals. Unit-tested directly (`MegaNavTest`).
  *
  * Model: on the bar, Left/Right move roots (horizontal) and Down opens the
  * focused root's panel onto its first item; in a panel, Up/Down move within a
  * column (wrapping), Left/Right move between columns (clamped, index clamped to
  * the new column), Home/End jump, Enter/Space activate, and Escape closes the
  * panel back to the bar (then clears at the bar).
  */
private[uic] object MegaNav:

    private def rootIdxs(items: List[MegaMenuItem]): List[Int] =
        items.zipWithIndex.collect { case (it, i) if !it.disabledFlag => i }

    private def hasPanel(items: List[MegaMenuItem], r: Int): Boolean =
        r >= 0 && r < items.size && items(r).columnsV.nonEmpty

    private def colCount(items: List[MegaMenuItem], r: Int): Int =
        if r >= 0 && r < items.size then items(r).columnsV.size else 0

    /** Navigable [[MenuItem]]s of column `c` in root `r` (groups flattened,
      * separators and disabled rows dropped).
      */
    private[uic] def colItems(items: List[MegaMenuItem], r: Int, c: Int): List[MenuItem] =
        if r >= 0 && r < items.size && c >= 0 && c < items(r).columnsV.size then
            items(r).columnsV(c).flatMap(_.itemsV).filter(it => !it.separatorFlag && !it.disabledFlag)
        else Nil

    private def moveRoot(items: List[MegaMenuItem], r: Int, dir: Int): Int =
        val rs = rootIdxs(items)
        if rs.isEmpty then r
        else
            val idx  = rs.indexOf(r)
            val next = if idx < 0 then 0 else (idx + dir + rs.size) % rs.size
            rs(next)
        end if
    end moveRoot

    private def firstRoot(items: List[MegaMenuItem]): Int = rootIdxs(items).headOption.getOrElse(0)
    private def lastRoot(items: List[MegaMenuItem]): Int  = rootIdxs(items).lastOption.getOrElse(0)

    /** Enter root `r`'s panel at column 0, item 0 (`Absent` if it has no navigable
      * first-column item).
      */
    private def openPanel(items: List[MegaMenuItem], r: Int): Maybe[Step] =
        if hasPanel(items, r) && colItems(items, r, 0).nonEmpty then
            Present(Step(focus = List(r, 0, 0), open = OpenOp.OpenTo(List(r))))
        else Absent

    def onKey(
        items: List[MegaMenuItem],
        horizontal: Boolean,
        focus: List[Int],
        key: Keyboard
    ): Maybe[Step] =
        import Keyboard.*

        if focus.isEmpty then
            key match
                case ArrowRight | ArrowDown | Home => Present(Step(List(firstRoot(items))))
                case ArrowLeft | ArrowUp           => Present(Step(List(firstRoot(items))))
                case End                           => Present(Step(List(lastRoot(items))))
                case _                             => Absent
        else if focus.size == 1 then
            val r = focus.head
            key match
                case ArrowRight if horizontal => Present(Step(List(moveRoot(items, r, +1)), OpenOp.Close))
                case ArrowLeft if horizontal  => Present(Step(List(moveRoot(items, r, -1)), OpenOp.Close))
                case ArrowDown | ArrowRight   => openPanel(items, r).orElse(Absent)
                case ArrowUp | ArrowLeft      => Absent
                case Home                     => Present(Step(List(firstRoot(items)), OpenOp.Close))
                case End                      => Present(Step(List(lastRoot(items)), OpenOp.Close))
                case Enter | Space =>
                    openPanel(items, r).orElse(Present(Step(List(r), activate = true)))
                case Escape => Present(Step(Nil, OpenOp.Close, dismiss = true))
                case _      => Absent
            end match
        else
            // panel: focus == List(r, c, i)
            val r            = focus.head
            val c            = focus(1)
            val i            = focus(2)
            def col(cc: Int) = colItems(items, r, cc)
            def clampI(cc: Int, want: Int): Int =
                val n = col(cc).size
                if n == 0 then 0 else math.max(0, math.min(n - 1, want))
            key match
                case ArrowDown =>
                    val n = col(c).size
                    if n == 0 then Absent else Present(Step(List(r, c, (i + 1) % n)))
                case ArrowUp =>
                    val n = col(c).size
                    if n == 0 then Absent else Present(Step(List(r, c, (i - 1 + n) % n)))
                case ArrowRight =>
                    val cc = math.min(colCount(items, r) - 1, c + 1)
                    Present(Step(List(r, cc, clampI(cc, i))))
                case ArrowLeft =>
                    val cc = math.max(0, c - 1)
                    Present(Step(List(r, cc, clampI(cc, i))))
                case Home          => Present(Step(List(r, c, 0)))
                case End           => Present(Step(List(r, c, math.max(0, col(c).size - 1))))
                case Enter | Space => Present(Step(focus, activate = true))
                case Escape        => Present(Step(List(r), OpenOp.Close))
                case _             => Absent
            end match
        end if
    end onKey
end MegaNav
