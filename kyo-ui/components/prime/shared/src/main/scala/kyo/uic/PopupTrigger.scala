package kyo.uic

import kyo.*
import kyo.UI.*

/** A component a page can hand to a popup host as the control that opens it.
  *
  * The host says the popup's state on the control the reader reaches: that it opens a popup and of which kind
  * (`aria-haspopup`), whether the popup is open (`aria-expanded`, following the state), and, where the state is a constant
  * nobody hears, that the control changes nothing (`aria-disabled`). A component knows which of its elements that is, so
  * it carries the state itself rather than the host guessing at its markup. [[Button]] and [[Link]] are.
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
      * changes nothing. `activate` is what a key asks for where the component is no native control (see
      * [[PopupTrigger.element]]); a native one answers its keys with its click, which the host already takes.
      */
    private[uic] def asPopupTrigger(
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI
end PopupTrigger

private[uic] object PopupTrigger:

    /** `aria-expanded` for a host's open slot: a channel where the slot follows a signal, the constant as it stands, and
      * closed where nothing is bound.
      */
    def expanded(open: Maybe[ReactiveValue[Boolean]])(using Frame): String | Signal[String] =
        open.flatMap(_.dyn).fold[String | Signal[String]](open.flatMap(_.const).getOrElse(false).toString)(_.map(_.toString))

    /** The trigger of a host that leaves its handlers to the caller (an [[Overlay]] anchors a panel and toggles
      * nothing): the popup's kind and whether it is open, said on the trigger, and nothing that would make it a control
      * the host does not wire.
      */
    def announced(trigger: UI.Ast.Interactive | PopupTrigger, popup: String, expanded: String | Signal[String])(using
        Frame
    ): UI =
        trigger match
            case t: PopupTrigger       => t.asPopupTrigger(popup, expanded, fixed = false, activate = Absent)
            case e: UI.Ast.Interactive =>
                val el = e.aria(Aria.haspopup -> popup)
                expanded match
                    case s: String                    => el.aria(Aria.expanded -> s)
                    case s: Signal[String] @unchecked => el.aria(Aria.expanded -> s)
        end match
    end announced

    /** The trigger as the host draws it. The host takes the click around it, which reaches it from the trigger and from
      * anything inside; `activate` is what a key asks for on a trigger that is no control of its own (Absent where the
      * state is fixed). That key is taken on the trigger itself only (`onKeyDownSelf`): a key pressed in a control inside
      * it is that control's, and a native control's Enter is already its click.
      */
    def stamped(
        trigger: UI.Ast.Interactive | PopupTrigger,
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI =
        trigger match
            case t: PopupTrigger       => t.asPopupTrigger(popup, expanded, fixed, activate)
            case e: UI.Ast.Interactive => element(e, popup, expanded, fixed, activate)

    /** One element as a popup's trigger: the state said on it, and, where it is no native control, a button role, a tab
      * stop and Enter and Space taken on the element itself.
      */
    def element(
        e: UI.Ast.Interactive,
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI.Ast.Element =
        val native                                           = isNative(e)
        val activation: Maybe[UI.KeyboardEvent => UI.Action] =
            if native then Absent
            else
                activate.map(act =>
                    evt =>
                        evt.key match
                            case UI.Keyboard.Enter | UI.Keyboard.Space => act
                            case _                                     => Kyo.unit
                )
        // A key handler the page put on its own trigger still runs, before the activation.
        val keys: Maybe[UI.KeyboardEvent => UI.Action] =
            (e.attrs.onKeyDownSelf, activation) match
                case (Present(own), Present(act)) => Present(evt => own(evt).andThen(act(evt)))
                case (own, act)                   => act.orElse(own)
        // A tab stop and the keys first, in one write, while the element is still known to take both.
        val reachable: UI.Ast.Element =
            if native then e
            else
                e.withAttrs(e.attrs.copy(
                    tabIndex = e.attrs.tabIndex.orElse(Present(0)),
                    onKeyDownSelf = keys
                ))
        var el: UI.Ast.Element = reachable.aria(Aria.haspopup -> popup)
        el = expanded match
            case s: String                    => el.aria(Aria.expanded -> s)
            case s: Signal[String] @unchecked => el.aria(Aria.expanded -> s)
        if fixed then el = el.aria(Aria.disabled -> "true")
        if !native && el.attrs.role.isEmpty then el = el.role("button")
        el
    end element

    /** A control the browser activates by key itself. */
    private def isNative(e: UI.Ast.Interactive): Boolean =
        e match
            case _: UI.Ast.Button | _: UI.Ast.Input | _: UI.Ast.Select | _: UI.Ast.Textarea => true
            case a: UI.Ast.Anchor                                                           => a.href.isDefined
            case _                                                                          => false

end PopupTrigger
