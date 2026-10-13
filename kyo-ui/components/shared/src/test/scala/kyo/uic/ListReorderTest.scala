package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers

/** The pure move machine [[OrderList]] and [[PickList]] share.
  *
  * The plain vertical arrows are deliberately not here: they belong to the list, where they move
  * the highlight, and a column whose arrows reordered instead would leave a reader no way to reach
  * the row they meant to move. The modifier is what says carry.
  */
class ListReorderTest extends UicTest:

    private val plain = Modifiers.none
    private val cmd   = Modifiers.none.copy(meta = true)
    private val ctrl  = Modifiers.none.copy(ctrl = true)
    private val right = Present(Keyboard.ArrowRight)

    private def key(k: Keyboard, mods: Modifiers = plain, toward: Maybe[Keyboard] = Absent) =
        ListReorder.onKey(k, mods, toward)

    "a plain arrow is the list's, so the reader can still get to the row they mean" in {
        assert(key(Keyboard.ArrowUp) == Absent)
        assert(key(Keyboard.ArrowDown) == Absent)
    }

    "a carried arrow moves the selection one position" in {
        assert(key(Keyboard.ArrowUp, cmd) == Present(ListReorder.Move.Step(down = false)))
        assert(key(Keyboard.ArrowDown, ctrl) == Present(ListReorder.Move.Step(down = true)))
    }

    "and with Shift it goes all the way to the end" in {
        assert(key(Keyboard.ArrowUp, cmd.copy(shift = true)) == Present(ListReorder.Move.Edge(down = false)))
        assert(key(Keyboard.ArrowDown, ctrl.copy(shift = true)) == Present(ListReorder.Move.Edge(down = true)))
    }

    "Shift alone carries nothing, since it is the modifier that says carry" in {
        assert(key(Keyboard.ArrowUp, plain.copy(shift = true)) == Absent)
    }

    "the arrow pointing at the other column transfers the selection into it" in {
        assert(key(Keyboard.ArrowRight, plain, right) == Present(ListReorder.Move.Out(all = false)))
        assert(key(Keyboard.ArrowRight, cmd, right) == Present(ListReorder.Move.Out(all = true)), "and carried, the lot")
    }

    "the arrow pointing away from it is nobody's" in {
        assert(key(Keyboard.ArrowLeft, plain, right) == Absent)
    }

    "a one-column list has nowhere to send anything, so no horizontal arrow is its own" in {
        assert(key(Keyboard.ArrowRight) == Absent)
        assert(key(Keyboard.ArrowLeft, cmd) == Absent)
    }

    "everything else belongs to the list and then to the browser" in {
        assert(key(Keyboard.Space) == Absent, "Space selects the highlighted row")
        assert(key(Keyboard.Home, cmd) == Absent)
        assert(key(Keyboard.Tab, cmd, right) == Absent)
    }

    "the algorithms are Prime's: a selected block keeps its order and stops at the edge" in {
        val xs = List("a", "b", "c", "d")
        assert(ListReorder.moveUp(xs, Set("c", "d").contains) == List("a", "c", "d", "b"))
        assert(ListReorder.moveUp(xs, Set("a").contains) == xs, "already at the top")
        assert(ListReorder.moveDown(xs, Set("a").contains) == List("b", "a", "c", "d"))
        assert(ListReorder.moveTop(xs, Set("c").contains) == List("c", "a", "b", "d"))
        assert(ListReorder.moveBottom(xs, Set("a", "b").contains) == List("c", "d", "a", "b"))
    }

end ListReorderTest
