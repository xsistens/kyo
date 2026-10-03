package kyo.uic

import kyo.*
import kyo.UI.Keyboard
import kyo.UI.Modifiers

/** Pure keyboard-navigation state machine for [[DatePicker]]'s three grids, in the shape
  * [[ListNav]], [[MenuNav]] and [[GridNav]] already use: it takes one key and returns the step
  * the host maps onto its cursor. No DOM and no effects, so the semantics are unit-tested
  * directly (`CalendarNavTest`).
  *
  * It is NOT [[GridNav]], and the three differences are the reason. A calendar is a continuous
  * sequence that happens to be laid out in rows, so ArrowRight on a Saturday belongs on the next
  * Sunday, where a table's cursor stops at the edge of its row. PageUp and PageDown mean one
  * month, not one screenful of rows. And Enter and Space both pick the day, where a table's grid
  * gives Enter to the editor and Space to the row selection.
  *
  * The step is expressed in the VIEW'S OWN UNIT: a day in the day grid, a month in the month
  * grid, a year in the year grid. That is what lets one machine serve all three, and it keeps
  * every piece of date arithmetic in the host, which already owns it.
  */
private[uic] object CalendarNav:

    /** What one key asks the cursor to do. */
    enum Step derives CanEqual:
        /** Move by `cells` in the view's own unit, crossing whatever boundary that implies. */
        case Move(cells: Int)

        /** The first or last cell of the row the cursor is on: a week in the day grid. */
        case RowStart
        case RowEnd

        /** One page: a month in the day grid, a year in the month grid, a decade in the year
          * grid. `large` is the Shift form, which is the next unit up.
          */
        case Page(delta: Int, large: Boolean)

        /** Pick the cell the cursor is on. */
        case Activate

        /** Close the calendar. */
        case Dismiss
    end Step

    /** Reads one key against a grid `perRow` cells wide.
      *
      * `Absent` means the key is not ours, so the browser keeps it. A `perRow` of zero or less
      * is a grid with no rows to move along, so the plain vertical arrows are left alone too.
      *
      * Ctrl or Cmd turns an arrow into a page, the way it carries a row in [[ListReorder]]: the
      * horizontal pair turns pages (a month in the day grid), the vertical pair turns the unit
      * above them (a year), and Shift multiplies the step, by four sideways and by ten upward.
      * Up is forward in time for the carried pair, where the plain ArrowUp walks back through the
      * grid: the two are different gestures and the carried one reads as a dial rather than as a
      * cursor. The page keys keep their own meaning beside them.
      */
    def onKey(key: Keyboard, mods: Modifiers, perRow: Int): Maybe[Step] =
        import Keyboard.*
        val carry = mods.ctrl || mods.meta
        key match
            case ArrowLeft if carry      => Present(Step.Page(if mods.shift then -4 else -1, large = false))
            case ArrowRight if carry     => Present(Step.Page(if mods.shift then 4 else 1, large = false))
            case ArrowUp if carry        => Present(Step.Page(if mods.shift then 10 else 1, large = true))
            case ArrowDown if carry      => Present(Step.Page(if mods.shift then -10 else -1, large = true))
            case ArrowLeft               => Present(Step.Move(-1))
            case ArrowRight              => Present(Step.Move(1))
            case ArrowUp if perRow > 0   => Present(Step.Move(-perRow))
            case ArrowDown if perRow > 0 => Present(Step.Move(perRow))
            case Home                    => Present(Step.RowStart)
            case End                     => Present(Step.RowEnd)
            case PageUp                  => Present(Step.Page(-1, mods.shift))
            case PageDown                => Present(Step.Page(1, mods.shift))
            case Enter | Space           => Present(Step.Activate)
            case Escape                  => Present(Step.Dismiss)
            case _                       => Absent
        end match
    end onKey
end CalendarNav
