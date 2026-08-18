package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.uic.MenuNav.OpenOp
import kyo.uic.MenuNav.Step

/** Pure-logic assertions for the [[MegaNav]] grid state machine. */
class MegaNavTest extends UicTest:

    // Furniture ▸ [col0: Living(Chairs,Sofas)+Kitchen(Tables) | col1: Bedroom(Beds)]
    // Electronics ▸ [col0: Audio(Speakers)]
    // Contact (no panel)
    private val items: List[MegaMenuItem] = List(
        MegaMenuItem("Furniture")
            .column(
                MenuGroup("Living").items(MenuItem("Chairs"), MenuItem("Sofas")),
                MenuGroup("Kitchen").items(MenuItem("Tables"))
            )
            .column(MenuGroup("Bedroom").items(MenuItem("Beds"))),
        MegaMenuItem("Electronics").column(MenuGroup("Audio").items(MenuItem("Speakers"))),
        MegaMenuItem("Contact")
    )

    private def k(focus: List[Int], key: Keyboard) = MegaNav.onKey(items, horizontal = true, focus, key)

    private def lbl(it: MenuItem): String = it.labelV match
        case TextValue.Const(t) => t
        case _                  => "?"

    "column flattening drops headings, keeps navigable items in order" in assert(
        (MegaNav.colItems(items, 0, 0).map(lbl) == List("Chairs", "Sofas", "Tables")) &&
            (MegaNav.colItems(items, 0, 1).map(lbl) == List("Beds"))
    )

    "from nothing, a movement key focuses the first root" in assert(
        (k(Nil, Keyboard.ArrowRight) == Present(Step(List(0)))) &&
            (k(Nil, Keyboard.ArrowDown) == Present(Step(List(0))))
    )

    "bar: Left/Right move roots and close panels, wrapping" in assert(
        (k(List(0), Keyboard.ArrowRight) == Present(Step(List(1), OpenOp.Close))) &&
            (k(List(2), Keyboard.ArrowRight) == Present(Step(List(0), OpenOp.Close))) &&
            (k(List(0), Keyboard.ArrowLeft) == Present(Step(List(2), OpenOp.Close)))
    )

    "bar: ArrowDown opens the focused root's panel at (0,0)" in assert(
        k(List(0), Keyboard.ArrowDown) == Present(Step(List(0, 0, 0), OpenOp.OpenTo(List(0))))
    )

    "bar: a column-less root has no panel to open" in assert(
        k(List(2), Keyboard.ArrowDown) == Absent
    )

    "bar: Enter opens a panel, or activates a column-less root" in assert(
        (k(List(0), Keyboard.Enter) == Present(Step(List(0, 0, 0), OpenOp.OpenTo(List(0))))) &&
            (k(List(2), Keyboard.Enter) == Present(Step(List(2), activate = true)))
    )

    "panel: Up/Down move within the column, wrapping" in assert(
        (k(List(0, 0, 0), Keyboard.ArrowDown) == Present(Step(List(0, 0, 1)))) &&
            (k(List(0, 0, 2), Keyboard.ArrowDown) == Present(Step(List(0, 0, 0)))) &&
            (k(List(0, 0, 0), Keyboard.ArrowUp) == Present(Step(List(0, 0, 2))))
    )

    "panel: Right/Left move between columns, clamping the row index" in assert(
        // col0 has 3 items, col1 has 1 — moving right from row 2 clamps to row 0
        (k(List(0, 0, 2), Keyboard.ArrowRight) == Present(Step(List(0, 1, 0)))) &&
            (k(List(0, 1, 0), Keyboard.ArrowLeft) == Present(Step(List(0, 0, 0)))) &&
            // at the right edge, Right stays put
            (k(List(0, 1, 0), Keyboard.ArrowRight) == Present(Step(List(0, 1, 0))))
    )

    "panel: Home/End jump within the column" in assert(
        (k(List(0, 0, 1), Keyboard.Home) == Present(Step(List(0, 0, 0)))) &&
            (k(List(0, 0, 1), Keyboard.End) == Present(Step(List(0, 0, 2))))
    )

    "panel: Enter activates the item; Escape returns to the bar" in assert(
        (k(List(0, 0, 1), Keyboard.Enter) == Present(Step(List(0, 0, 1), activate = true))) &&
            (k(List(0, 0, 1), Keyboard.Escape) == Present(Step(List(0), OpenOp.Close)))
    )

    "bar: Escape dismisses" in assert(
        k(List(0), Keyboard.Escape) == Present(Step(Nil, OpenOp.Close, dismiss = true))
    )
end MegaNavTest
