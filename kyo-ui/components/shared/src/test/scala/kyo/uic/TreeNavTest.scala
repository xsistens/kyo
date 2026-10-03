package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.uic.TreeNav.Op
import kyo.uic.TreeNav.Row
import kyo.uic.TreeNav.Step

/** Pure-logic assertions for the [[TreeNav]] WAI-ARIA tree keyboard state machine — no DOM,
  * no effects (the semantics [[Tree]] and [[TreeSelect]] map onto their focus and expansion
  * state).
  */
class TreeNavTest extends UicTest:

    // Documents (open) ▸ (Work, Home (closed) ▸ …) | Pictures (closed) | Notes (leaf)
    private val rows = List(
        Row(0, "documents", expandable = true, expanded = true),
        Row(1, "work", expandable = false, expanded = false),
        Row(1, "home", expandable = true, expanded = false),
        Row(0, "pictures", expandable = true, expanded = false),
        Row(0, "notes", expandable = false, expanded = false)
    )

    private def key(focus: Int, k: Keyboard) = TreeNav.onKey(rows, focus, k)

    "ArrowDown from nothing lands on the first row" in assert(
        key(-1, Keyboard.ArrowDown) == Present(Step(0))
    )

    "ArrowUp from nothing lands on the last visible row" in assert(
        key(-1, Keyboard.ArrowUp) == Present(Step(4))
    )

    "ArrowDown walks into an open node's children" in assert(
        key(0, Keyboard.ArrowDown) == Present(Step(1))
    )

    "ArrowDown leaves the last child for the next root" in assert(
        key(2, Keyboard.ArrowDown) == Present(Step(3))
    )

    "ArrowDown holds at the last row" in assert(
        key(4, Keyboard.ArrowDown) == Present(Step(4))
    )

    "ArrowUp holds at the first row" in assert(
        key(0, Keyboard.ArrowUp) == Present(Step(0))
    )

    "ArrowRight on a closed node opens it and stays put" in assert(
        key(3, Keyboard.ArrowRight) == Present(Step(3, Op.Expand("pictures")))
    )

    "ArrowRight on an open node walks into its first child" in assert(
        key(0, Keyboard.ArrowRight) == Present(Step(1))
    )

    "ArrowRight on a leaf is not ours" in assert(
        key(4, Keyboard.ArrowRight) == Absent
    )

    "ArrowLeft on an open node closes it and stays put" in assert(
        key(0, Keyboard.ArrowLeft) == Present(Step(0, Op.Collapse("documents")))
    )

    "ArrowLeft on a child climbs to its parent" in assert(
        key(1, Keyboard.ArrowLeft) == Present(Step(0))
    )

    "ArrowLeft on a closed node climbs rather than closing what is already closed" in assert(
        key(2, Keyboard.ArrowLeft) == Present(Step(0))
    )

    "ArrowLeft on a root with nothing to close is not ours" in assert(
        key(4, Keyboard.ArrowLeft) == Absent
    )

    "Home and End reach the ends" in assert(
        key(2, Keyboard.Home) == Present(Step(0)) && key(2, Keyboard.End) == Present(Step(4))
    )

    "Enter selects a leaf and opens nothing" in assert(
        key(1, Keyboard.Enter) == Present(Step(1, activate = true))
    )

    "Space selects a leaf too" in assert(
        key(1, Keyboard.Space) == Present(Step(1, activate = true))
    )

    "Enter on a closed parent selects it and opens it" in assert(
        key(2, Keyboard.Enter) == Present(Step(2, Op.Expand("home"), activate = true))
    )

    "Space on an open parent selects it and closes it" in assert(
        key(0, Keyboard.Space) == Present(Step(0, Op.Collapse("documents"), activate = true))
    )

    "Enter on nothing is not ours" in assert(
        key(-1, Keyboard.Enter) == Absent
    )

    "Escape clears the highlight and asks the host to close" in assert(
        key(2, Keyboard.Escape) == Present(Step(-1, dismiss = true))
    )

    "a key the tree does not use stays with the browser" in assert(
        key(2, Keyboard.Tab) == Absent
    )

    "an empty tree has nowhere to go" in assert(
        TreeNav.onKey(Nil, -1, Keyboard.ArrowDown) == Absent &&
            TreeNav.onKey(Nil, -1, Keyboard.End) == Absent
    )

end TreeNavTest
