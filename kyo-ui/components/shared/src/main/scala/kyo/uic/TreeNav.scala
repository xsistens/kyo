package kyo.uic

import kyo.*
import kyo.UI.Keyboard

/** Pure WAI-ARIA tree keyboard state machine shared by [[Tree]] and the panel of
  * [[TreeSelect]]. It takes the rows as they STAND ON THE SCREEN, the row the highlight is
  * on, and one key, and returns the [[TreeNav.Step]] the host maps onto its focus and
  * expansion state. No DOM and no effects, so the semantics are unit-tested directly
  * (`TreeNavTest`), the shape [[MenuNav]] and [[GridNav]] already use.
  *
  * The rows are FLAT and hold only what is visible, which is the whole reason this reads
  * simply: a collapsed node contributes one row and its children none, so Down is the next
  * index and the hierarchy survives in nothing but each row's `depth`. That also makes the
  * two questions a tree asks cheap to answer: a row's first child is the row after it, and
  * its parent is the nearest earlier row that sits shallower.
  *
  * Nothing wraps. The ARIA tree pattern stops at the ends, unlike the menu family, because a
  * tree is a place rather than a ring: a reader who holds Down expects to arrive at the last
  * row and stay there.
  *
  * One deliberate step past the pattern: Enter and Space open or close a parent as well as
  * selecting it. ARIA gives that to the arrows alone, which reads well for a tree the reader
  * only walks through, and badly for one whose folders are the point.
  *
  * `Absent` means the key is not ours, so the host leaves it to the browser.
  */
private[uic] object TreeNav:

    /** One row of the tree as the reader sees it: how deep it sits, the key its host
      * addresses it by, and whether it can open and is open.
      */
    final case class Row(depth: Int, key: String, expandable: Boolean, expanded: Boolean) derives CanEqual

    /** What a key does to the expansion state, which the host owns and this does not read
      * back: `Expand` and `Collapse` name the row's own key, so a host holding a set of open
      * ids adds or removes exactly that one.
      */
    enum Op derives CanEqual:
        case Keep
        case Expand(key: String)
        case Collapse(key: String)
    end Op

    /** The outcome of one key press. `focus` is the new highlighted row (`-1` for none),
      * `activate` asks the host to select the focused row, and `dismiss` asks it to close the
      * panel it lives in, which is what [[TreeSelect]] does with Escape and a bare [[Tree]]
      * ignores.
      */
    final case class Step(
        focus: Int,
        op: Op = Op.Keep,
        activate: Boolean = false,
        dismiss: Boolean = false
    ) derives CanEqual

    /** The nearest earlier row that sits shallower, which is the parent of `focus`. */
    private def parentOf(rows: List[Row], focus: Int): Int =
        val depth = rows(focus).depth
        rows.take(focus).lastIndexWhere(_.depth < depth)

    /** Whether the row after `focus` is its first child rather than its next sibling. */
    private def hasVisibleChild(rows: List[Row], focus: Int): Boolean =
        focus + 1 < rows.size && rows(focus + 1).depth > rows(focus).depth

    def onKey(rows: List[Row], focus: Int, key: Keyboard): Maybe[Step] =
        import Keyboard.*
        val on = focus >= 0 && focus < rows.size
        key match
            case ArrowDown =>
                if rows.isEmpty then Absent
                else Present(Step(if on then math.min(focus + 1, rows.size - 1) else 0))
            case ArrowUp =>
                if rows.isEmpty then Absent
                else Present(Step(if on then math.max(focus - 1, 0) else rows.size - 1))

            // Right opens what is closed and then walks into it, which is the two-press move
            // the ARIA pattern describes: the first press shows the children, the second
            // enters them.
            case ArrowRight if on =>
                val row = rows(focus)
                if row.expandable && !row.expanded then Present(Step(focus, Op.Expand(row.key)))
                else if hasVisibleChild(rows, focus) then Present(Step(focus + 1))
                else Absent

            // Left is its mirror: close what is open, and from a row that has nothing to
            // close, climb to the parent.
            case ArrowLeft if on =>
                val row = rows(focus)
                if row.expandable && row.expanded then Present(Step(focus, Op.Collapse(row.key)))
                else
                    parentOf(rows, focus) match
                        case -1     => Absent
                        case parent => Present(Step(parent))
                end if

            case Home => if rows.isEmpty then Absent else Present(Step(0))
            case End  => if rows.isEmpty then Absent else Present(Step(rows.size - 1))

            // Enter and Space select, and on a parent they open or close it as well. The ARIA
            // pattern leaves that to the arrows alone, which is a keyboard for a tree that is
            // only ever navigated; a tree whose parents are the interesting rows wants the key
            // under the finger to do the obvious thing to the row it is on.
            case Enter | Space =>
                if !on then Absent
                else
                    val row = rows(focus)
                    val op =
                        if !row.expandable then Op.Keep
                        else if row.expanded then Op.Collapse(row.key)
                        else Op.Expand(row.key)
                    Present(Step(focus, op, activate = true))
            case Escape => Present(Step(focus = -1, dismiss = true))
            case _      => Absent
        end match
    end onKey
end TreeNav
