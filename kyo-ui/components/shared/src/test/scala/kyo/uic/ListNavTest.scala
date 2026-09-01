package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.uic.ListNav.Step

/** Pure-logic assertions for the [[ListNav]] roving-focus state machine: no DOM, no effects
  * (the semantics [[Menu]], [[Listbox]] and [[Accordion]] map onto their own focus state).
  */
class ListNavTest extends UicTest:

    // Rows 0..5 where 1 is a heading and 4 is disabled: the highlight lands on 0, 2, 3, 5.
    private val navigable = List(0, 2, 3, 5)

    private def key(focus: Int, k: Keyboard, wrap: Boolean = true) =
        ListNav.onKey(navigable, focus, k, wrap)

    "ArrowDown from nothing lands on the first navigable row" in assert(
        key(-1, Keyboard.ArrowDown) == Present(Step(0))
    )

    "ArrowUp from nothing lands on the last navigable row" in assert(
        key(-1, Keyboard.ArrowUp) == Present(Step(5))
    )

    "ArrowDown skips the rows the keyboard cannot land on" in assert(
        key(0, Keyboard.ArrowDown) == Present(Step(2))
    )

    "ArrowUp skips them the other way" in assert(
        key(5, Keyboard.ArrowUp) == Present(Step(3))
    )

    "ArrowDown wraps at the end when the host wraps" in assert(
        key(5, Keyboard.ArrowDown) == Present(Step(0))
    )

    "ArrowUp wraps at the start when the host wraps" in assert(
        key(0, Keyboard.ArrowUp) == Present(Step(5))
    )

    "ArrowDown holds at the end when the host does not wrap" in assert(
        key(5, Keyboard.ArrowDown, wrap = false) == Present(Step(5))
    )

    "ArrowUp holds at the start when the host does not wrap" in assert(
        key(0, Keyboard.ArrowUp, wrap = false) == Present(Step(0))
    )

    "Home and End reach the ends from anywhere" in assert(
        key(3, Keyboard.Home) == Present(Step(0)) && key(3, Keyboard.End) == Present(Step(5))
    )

    "Enter activates the highlighted row" in assert(
        key(3, Keyboard.Enter) == Present(Step(3, activate = true))
    )

    "Space activates it too" in assert(
        key(3, Keyboard.Space) == Present(Step(3, activate = true))
    )

    "Enter on nothing is not ours, so the browser keeps it" in assert(
        key(-1, Keyboard.Enter) == Absent
    )

    "Enter on a row the keyboard skips is not ours either" in assert(
        key(4, Keyboard.Enter) == Absent
    )

    "Escape clears the highlight and asks the host to close" in assert(
        key(3, Keyboard.Escape) == Present(Step(-1, dismiss = true))
    )

    "a key the list does not use stays with the browser" in assert(
        key(3, Keyboard.Tab) == Absent
    )

    "a focus that stopped being navigable moves to a real row" in assert(
        key(4, Keyboard.ArrowDown) == Present(Step(0)) && key(4, Keyboard.ArrowUp) == Present(Step(5))
    )

    "an empty list has nowhere to go" in assert(
        ListNav.onKey(Nil, -1, Keyboard.ArrowDown, wrap = true) == Present(Step(-1)) &&
            ListNav.onKey(Nil, -1, Keyboard.Home, wrap = true) == Absent
    )

    // ---- orientation ----

    private def hkey(focus: Int, k: Keyboard) =
        ListNav.onKey(navigable, focus, k, wrap = true, ListNav.Orientation.Horizontal)

    "a horizontal list moves on the horizontal arrows" in assert(
        hkey(-1, Keyboard.ArrowRight) == Present(Step(0)) &&
            hkey(-1, Keyboard.ArrowLeft) == Present(Step(5)) &&
            hkey(0, Keyboard.ArrowRight) == Present(Step(2))
    )

    "a horizontal list leaves the vertical arrows to the browser" in assert(
        hkey(0, Keyboard.ArrowDown) == Absent && hkey(0, Keyboard.ArrowUp) == Absent
    )

    "a vertical list leaves the horizontal arrows to the browser" in assert(
        key(0, Keyboard.ArrowLeft) == Absent && key(0, Keyboard.ArrowRight) == Absent
    )

    private def bkey(focus: Int, k: Keyboard) =
        ListNav.onKey(navigable, focus, k, wrap = true, ListNav.Orientation.Both)

    "a radio group answers all four arrows, since that is what a radio group does" in assert(
        bkey(0, Keyboard.ArrowRight) == Present(Step(2)) &&
            bkey(0, Keyboard.ArrowDown) == Present(Step(2)) &&
            bkey(2, Keyboard.ArrowLeft) == Present(Step(0)) &&
            bkey(2, Keyboard.ArrowUp) == Present(Step(0)),
        "Down and Right are one movement, Up and Left the other"
    )

    "Home and End have no axis, so both orientations reach the ends" in assert(
        hkey(2, Keyboard.Home) == Present(Step(0)) && hkey(2, Keyboard.End) == Present(Step(5))
    )

    "activation and dismissal have no axis either" in assert(
        hkey(2, Keyboard.Enter) == Present(Step(2, activate = true)) &&
            hkey(2, Keyboard.Space) == Present(Step(2, activate = true)) &&
            hkey(2, Keyboard.Escape) == Present(Step(-1, dismiss = true))
    )

end ListNavTest
