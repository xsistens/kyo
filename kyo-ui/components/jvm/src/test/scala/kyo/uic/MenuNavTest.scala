package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.uic.MenuNav.OpenOp
import kyo.uic.MenuNav.Orientation
import kyo.uic.MenuNav.Step

/** Pure-logic assertions for the [[MenuNav]] WAI-ARIA keyboard state machine —
  * no DOM, no effects (the semantics every menu-family component maps onto its
  * focus + submenu-open signals).
  */
class MenuNavTest extends UicTest:

    // File ▸ (New, Open, Recent ▸ (R1, R2)) | Edit ▸ (Undo, [sep], Redo) | Help
    private val tree: List[MenuItem] = List(
        MenuItem("File").items(
            MenuItem("New"),
            MenuItem("Open"),
            MenuItem("Recent").items(MenuItem("R1"), MenuItem("R2"))
        ),
        MenuItem("Edit").items(
            MenuItem("Undo"),
            MenuItem.separator,
            MenuItem("Redo")
        ),
        MenuItem("Help")
    )

    // File | Disabled(off) | Help — for skip-over
    private val withDisabled: List[MenuItem] = List(
        MenuItem("File"),
        MenuItem("Off").disabled(true),
        MenuItem("Help")
    )

    private def vKey(focus: List[Int], key: Keyboard) =
        MenuNav.onKey(tree, Orientation.Vertical, focus, key)
    private def hKey(focus: List[Int], key: Keyboard) =
        MenuNav.onKey(tree, Orientation.Horizontal, focus, key)

    "ArrowDown from nothing lands on the first row" in assert(
        vKey(Nil, Keyboard.ArrowDown) == Present(Step(focus = List(0)))
    )

    "ArrowUp from nothing lands on the last row" in assert(
        vKey(Nil, Keyboard.ArrowUp) == Present(Step(focus = List(2)))
    )

    "ArrowDown advances and wraps at the end" in assert(
        (vKey(List(0), Keyboard.ArrowDown) == Present(Step(focus = List(1)))) &&
            (vKey(List(2), Keyboard.ArrowDown) == Present(Step(focus = List(0))))
    )

    "ArrowUp retreats and wraps at the start" in assert(
        (vKey(List(1), Keyboard.ArrowUp) == Present(Step(focus = List(0)))) &&
            (vKey(List(0), Keyboard.ArrowUp) == Present(Step(focus = List(2))))
    )

    "navigation skips disabled rows" in assert(
        {
            val down = MenuNav.onKey(withDisabled, Orientation.Vertical, List(0), Keyboard.ArrowDown)
            down == Present(Step(focus = List(2))) // 1 (disabled) skipped
        }
    )

    "Home/End jump within the current level" in assert(
        {
            // inside File's submenu (Recent has children but is still navigable)
            val home = vKey(List(0, 2), Keyboard.Home)
            val end  = vKey(List(0, 0), Keyboard.End)
            (home == Present(Step(focus = List(0, 0)))) && (end == Present(Step(focus = List(0, 2))))
        }
    )

    "Enter on a leaf requests activation" in assert(
        vKey(List(2), Keyboard.Enter) == Present(Step(focus = List(2), activate = true))
    )

    "Enter on a parent opens its submenu and lands on the first child" in assert(
        vKey(List(0), Keyboard.Enter) == Present(Step(focus = List(0, 0), open = OpenOp.OpenTo(List(0, 0))))
    )

    "ArrowRight opens a submenu (vertical)" in assert(
        vKey(List(0), Keyboard.ArrowRight) == Present(Step(focus = List(0, 0), open = OpenOp.OpenTo(List(0, 0))))
    )

    "ArrowRight on a leaf is inert in a vertical menu" in assert(
        vKey(List(2), Keyboard.ArrowRight) == Absent
    )

    "ArrowLeft climbs one submenu level, closing that submenu" in assert(
        {
            // from Recent's child back to Recent, keeping File's submenu open
            val deep = vKey(List(0, 2, 0), Keyboard.ArrowLeft)
            // from File's child back to File, closing everything
            val shallow = vKey(List(0, 0), Keyboard.ArrowLeft)
            (deep == Present(Step(focus = List(0, 2), open = OpenOp.OpenTo(List(0))))) &&
            (shallow == Present(Step(focus = List(0), open = OpenOp.Close)))
        }
    )

    "ArrowLeft at the root of a vertical menu is inert" in assert(
        vKey(List(1), Keyboard.ArrowLeft) == Absent
    )

    "Escape climbs one level, then dismisses at the root" in assert(
        {
            val inSub = vKey(List(0, 0), Keyboard.Escape)
            val atTop = vKey(List(0), Keyboard.Escape)
            (inSub == Present(Step(focus = List(0), open = OpenOp.Close))) &&
            (atTop == Present(Step(focus = Nil, open = OpenOp.Close, dismiss = true)))
        }
    )

    // ---- menubar (horizontal root) ----

    "horizontal root: Left/Right move between roots and close submenus" in assert(
        (hKey(List(0), Keyboard.ArrowRight) == Present(Step(focus = List(1), open = OpenOp.Close))) &&
            (hKey(List(0), Keyboard.ArrowLeft) == Present(Step(focus = List(2), open = OpenOp.Close)))
    )

    "horizontal root: ArrowDown opens the focused root's submenu" in assert(
        hKey(List(0), Keyboard.ArrowDown) == Present(Step(focus = List(0, 0), open = OpenOp.OpenTo(List(0, 0))))
    )

    "horizontal root: ArrowUp opens the submenu on its last row" in assert(
        hKey(List(0), Keyboard.ArrowUp) == Present(Step(focus = List(0, 2), open = OpenOp.OpenTo(List(0, 2))))
    )

    "menubar submenu: ArrowLeft crosses to the previous root and opens it" in assert(
        // depth-2 in Edit's submenu, Left → File root opened on its first child
        hKey(List(1, 0), Keyboard.ArrowLeft) == Present(Step(focus = List(0, 0), open = OpenOp.OpenTo(List(0, 0))))
    )

    "menubar submenu: ArrowRight on a leaf crosses to the next root and opens it" in assert(
        // Edit ▸ Undo is a leaf; Right → Help root (no children) focused, subs closed
        hKey(List(1, 0), Keyboard.ArrowRight) == Present(Step(focus = List(2), open = OpenOp.Close))
    )
end MenuNavTest
