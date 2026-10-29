package kyo.uic

import kyo.*
import kyo.UI.*

/** A component a page can hand to a popup host as the control that opens it.
  *
  * The host says the popup's state on the control the reader reaches: that it opens a popup and of which kind
  * (`aria-haspopup`), whether the popup is open (`aria-expanded`, following the state), and, where the state is a constant
  * nobody hears, that the control changes nothing (`aria-disabled`). A component knows which of its elements that is, so
  * it carries the state itself rather than the host guessing at its markup. [[Button]] is one.
  *
  * A host takes `UI.Ast.Interactive | PopupTrigger`. Any element that takes handlers and a tab stop qualifies too, since
  * that element is the control; one that is no control of its own (a `span`, a `div`) is made one, with a button role and
  * a tab stop. What has no such element at its root (a bare reactive region) does not compile, because the host would have
  * nowhere to say the state; wrapped in an element it is that element's content.
  *
  * Only this module implements it: the member is package-private, so the set of components that can carry a popup's
  * state is the set that does so correctly.
  */
trait PopupTrigger:

    /** This component as the control that opens a popup of kind `popup`, saying `expanded` and, when `fixed`, that it
      * changes nothing.
      */
    private[uic] def asPopupTrigger(popup: String, expanded: String | Signal[String], fixed: Boolean)(using Frame): UI
end PopupTrigger

private[uic] object PopupTrigger:

    /** The trigger as the host draws it, and whether it is a native control: one whose keys the browser turns into its
      * click, so the host must not answer them a second time.
      */
    def stamped(
        trigger: UI.Ast.Interactive | PopupTrigger,
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean
    )(using Frame): (UI, Boolean) =
        trigger match
            case t: PopupTrigger       => (t.asPopupTrigger(popup, expanded, fixed), true)
            case e: UI.Ast.Interactive =>
                val native = isNative(e)
                // A tab stop first, while the element is still known to take one.
                val reachable: UI.Ast.Element = if !native && e.attrs.tabIndex.isEmpty then e.tabIndex(0) else e
                var el: UI.Ast.Element        = reachable.aria("haspopup", popup)
                el = expanded match
                    case s: String                    => el.aria("expanded", s)
                    case s: Signal[String] @unchecked => el.aria("expanded", s)
                if fixed then el = el.aria("disabled", "true")
                if !native && el.attrs.role.isEmpty then el = el.role("button")
                (el, native)

    /** A control the browser activates by key itself. */
    private def isNative(e: UI.Ast.Interactive): Boolean =
        e match
            case _: UI.Ast.Button | _: UI.Ast.Input | _: UI.Ast.Select | _: UI.Ast.Textarea => true
            case a: UI.Ast.Anchor                                                           => a.href.isDefined
            case _                                                                          => false

end PopupTrigger
