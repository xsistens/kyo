package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers

/** The reorder and transfer vocabulary [[OrderList]] and [[PickList]] share: PrimeVue's exact move
  * algorithms over an ordered list with a selected subset, and the pure key machine that says which
  * of them a chord asks for.
  *
  * Its own file because it belongs to both hosts rather than to either, and because a pure machine
  * in this module is tested directly (`ListReorderTest`), which needs a source file to be named
  * after.
  */
private[uic] object ListReorder:

    /** What a column roves: the highlight index, and the id it is announced through.
      *
      * Both are minted by the host's mount, because a component cannot learn its own render path
      * and `aria-activedescendant` needs an id to point at. They travel together since neither is
      * any use without the other.
      */
    final case class Cursor(highlight: SignalRef[Int], id: String)

    /** Each selected item swaps with its predecessor, scanning top-down; a selected
      * item already at the top stops the pass (Prime's break).
      */
    def moveUp[A](xs: List[A], isSel: A => Boolean): List[A] =
        val buf  = xs.toBuffer
        var i    = 0
        var stop = false
        while i < buf.length && !stop do
            if isSel(buf(i)) then
                if i == 0 then stop = true
                else
                    val tmp = buf(i - 1)
                    buf(i - 1) = buf(i)
                    buf(i) = tmp
            end if
            i += 1
        end while
        buf.toList
    end moveUp

    /** Each selected item swaps with its successor, scanning bottom-up; a selected
      * item already at the bottom stops the pass (Prime's break).
      */
    def moveDown[A](xs: List[A], isSel: A => Boolean): List[A] =
        val buf  = xs.toBuffer
        var i    = buf.length - 1
        var stop = false
        while i >= 0 && !stop do
            if isSel(buf(i)) then
                if i == buf.length - 1 then stop = true
                else
                    val tmp = buf(i + 1)
                    buf(i + 1) = buf(i)
                    buf(i) = tmp
            end if
            i -= 1
        end while
        buf.toList
    end moveDown

    /** Selected items move to the front, keeping their relative order. */
    def moveTop[A](xs: List[A], isSel: A => Boolean): List[A] =
        val (sel, rest) = xs.partition(isSel)
        sel ++ rest

    /** Selected items move to the back, keeping their relative order. */
    def moveBottom[A](xs: List[A], isSel: A => Boolean): List[A] =
        val (sel, rest) = xs.partition(isSel)
        rest ++ sel

    /** What a chord asks of the column the reader is standing in. */
    enum Move derives CanEqual:
        /** The selected rows move one position. */
        case Step(down: Boolean)

        /** The selected rows move to the top or the bottom of the column. */
        case Edge(down: Boolean)

        /** The column's contents leave for the other one: the selected rows, or all of them. */
        case Out(all: Boolean)
    end Move

    /** Reads one chord for a column whose other side lies in the `toward` direction (`Absent` for a
      * one-column list, which has nowhere to send anything).
      *
      * The plain vertical arrows are NOT here, and deliberately: they belong to the list, where they
      * move the highlight, and a column whose arrows reordered instead would leave a reader no way
      * to reach the row they meant to move. So the modifier is what says carry. Ctrl or Cmd with an
      * arrow moves the selection one position, Shift with it sends the selection to the end, and the
      * horizontal arrows, which a vertical list has no use for, transfer: the one pointing at the
      * other column sends the selection, and with Ctrl or Cmd it sends the whole column.
      *
      * `Absent` means the chord is not ours, so the list sees it next and then the browser.
      */
    def onKey(key: Keyboard, mods: Modifiers, toward: Maybe[Keyboard]): Maybe[Move] =
        val carry = mods.ctrl || mods.meta
        key match
            case Keyboard.ArrowUp if carry   => Present(if mods.shift then Move.Edge(false) else Move.Step(false))
            case Keyboard.ArrowDown if carry => Present(if mods.shift then Move.Edge(true) else Move.Step(true))
            case k =>
                toward match
                    case Present(t) if t == k => Present(Move.Out(carry))
                    case _                    => Absent
        end match
    end onKey
end ListReorder
