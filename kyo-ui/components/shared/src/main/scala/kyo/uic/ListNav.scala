package kyo.uic

import kyo.*
import kyo.UI.Keyboard

/** Pure roving-focus state machine for the flat lists: [[Menu]]'s rows, [[Listbox]]'s options
  * and [[Accordion]]'s headers. It takes the positions a highlight may land on, the position
  * it is on now, and one key, and returns the [[ListNav.Step]] the host maps onto its own
  * focus state. No DOM and no effects, so the semantics are unit-tested directly
  * (`ListNavTest`), the shape [[MenuNav]] and [[GridNav]] already use.
  *
  * `navigable` carries POSITIONS rather than a count, because every one of these lists holds
  * rows the keyboard must skip: a separator, a group heading, a disabled option. The host
  * builds that list once while it lays its rows out, and a focus of `-1` means nothing is
  * highlighted yet, which is the state a list renders in before the reader has touched it.
  *
  * `wrap` is one of the two things the hosts disagree about, and it is not a preference: the
  * ARIA menu pattern wraps and Prime's menus wrap with it, while Prime's listbox stops at the
  * ends. The other is `orientation`, for the hosts whose rows sit in a row rather than a
  * column. A host passes what its own pattern says.
  *
  * `Absent` means the key is not ours, so the host leaves it to the browser. That is what
  * keeps Tab, typing into a filter header, and a screen reader's own keys working.
  */
private[uic] object ListNav:

    /** Which pair of arrows moves the highlight.
      *
      * The other pair is left to the browser rather than aliased onto the same movement: in a
      * horizontal tablist ArrowDown belongs to the page, and a tablist that swallowed it would
      * take scrolling away from a reader who is already looking past it. Home and End reach the
      * ends either way, since neither has an axis.
      *
      * `Both` is for the one pattern that asks for all four: a radio group moves on Down and
      * Right alike, and on Up and Left alike, however its buttons are laid out. That is what a
      * reader gets from native radios without anyone writing it ([[Rating]] rides on exactly
      * that), so a group built from buttons has to answer the same keys.
      */
    enum Orientation derives CanEqual:
        case Vertical, Horizontal, Both

    /** The outcome of one key press. `focus` is the new highlight position (`-1` for none),
      * `activate` asks the host to run the focused row's action, and `dismiss` asks it to
      * close the popup it lives in.
      */
    final case class Step(
        focus: Int,
        activate: Boolean = false,
        dismiss: Boolean = false
    ) derives CanEqual

    /** The position `steps` away from `focus`, along the navigable ones.
      *
      * A focus that is not itself navigable (a row that just went disabled under a live
      * update) is treated as before the list rather than as a member of it, so the next key
      * lands on a real row instead of nowhere.
      */
    private def move(navigable: List[Int], focus: Int, dir: Int, wrap: Boolean): Int =
        if navigable.isEmpty then focus
        else
            navigable.indexOf(focus) match
                case -1 => if dir > 0 then navigable.head else navigable.last
                case cur =>
                    val next = cur + dir
                    if next >= 0 && next < navigable.size then navigable(next)
                    else if wrap then navigable((next + navigable.size) % navigable.size)
                    else focus
    end move

    def onKey(
        navigable: List[Int],
        focus: Int,
        key: Keyboard,
        wrap: Boolean,
        orientation: Orientation = Orientation.Vertical
    ): Maybe[Step] =
        import Keyboard.*
        val forward = orientation match
            case Orientation.Vertical   => List(ArrowDown)
            case Orientation.Horizontal => List(ArrowRight)
            case Orientation.Both       => List(ArrowDown, ArrowRight)
        val back = orientation match
            case Orientation.Vertical   => List(ArrowUp)
            case Orientation.Horizontal => List(ArrowLeft)
            case Orientation.Both       => List(ArrowUp, ArrowLeft)
        key match
            case k if forward.contains(k) => Present(Step(move(navigable, focus, +1, wrap)))
            case k if back.contains(k)    => Present(Step(move(navigable, focus, -1, wrap)))
            case Home                     => Maybe.fromOption(navigable.headOption).map(Step(_))
            case End                      => Maybe.fromOption(navigable.lastOption).map(Step(_))
            case Enter | Space =>
                if navigable.contains(focus) then Present(Step(focus, activate = true)) else Absent
            case Escape => Present(Step(focus = -1, dismiss = true))
            case _      => Absent
        end match
    end onKey
end ListNav
