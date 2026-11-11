package kyo.uic

import kyo.*
import kyo.UI.*

/** A form control's validity slots (`invalid`, `invalidMessage`), resolved once per render into what the control
  * stamps on its own elements and the message row that follows it.
  *
  * The control is built ONCE, whatever its validity does. A reactive slot never re-renders it:
  *   - [[mark]] (or [[markClass]] / [[markAria]], where Prime's sheet puts the two on different elements) sets
  *     `.p-invalid` as a class channel and `aria-invalid` as an attribute channel, both patched in place;
  *   - [[follow]] appends the `div.p-uic-invalid-message` row (a kyo extension; Prime leaves the message to the form
  *     layer) as a region of its own AFTER the control, so only the row appears and disappears.
  *
  * A control rebuilt by its validity replaces the element the reader is in: the keystroke that turns a field invalid
  * lands in a node that is already gone, and focus and caret go with it. That is the reason there is no way here to
  * hand the control a plain `Boolean` snapshot of a reactive slot.
  *
  * The red state is the explicit `invalid`, constant or signal, where one is set, else the presence of the reactive
  * message.
  * Without any reactive slot everything stays constant: `.p-invalid` and `aria-invalid="true"` are plain attributes
  * and the row is a sibling only when the control is invalid and has a message, exactly the static markup. A reactive
  * red state writes `aria-invalid="false"` while valid, which ARIA reads the same as no attribute: kyo-ui's attribute
  * channel patches a value, it never removes the attribute.
  *
  * A mounted control places [[follow]] inside its mount, so the static markup keeps the row inside the mount's host.
  * Nothing here is a region around the mount, so a validity change never re-runs it and an open panel stays open.
  */
private[uic] enum FieldValidity:
    case Fixed(isRed: Boolean, message: Maybe[String])
    case Live(redSig: Signal[Boolean], redFromMessage: Boolean, message: Maybe[Signal[Maybe[String]]])

    /** The red state as one of kyo-ui's `Boolean | Signal[Boolean]` setters takes it, for a control that hands it on to
      * the control it is built from (Password's input, InputOtp's cells).
      */
    def red: Boolean | Signal[Boolean] = this match
        case Fixed(r, _)   => r
        case Live(s, _, _) => s

    /** `.p-invalid` and `aria-invalid` on the one element that carries both. */
    def mark[E <: UI.Ast.Element { type Self <: E }](el: E)(using Frame): E = markAria(markClass(el))

    /** `.p-invalid` alone, for a control whose sheet hooks the class on a different element than the one that takes
      * `aria-invalid`.
      */
    def markClass[E <: UI.Ast.Element { type Self <: E }](el: E): E = el.when(red)(_.cssClass("p-invalid"))

    /** `aria-invalid` alone (see [[markClass]]). */
    def markAria[E <: UI.Ast.Element { type Self <: E }](el: E)(using Frame): E = this match
        case Fixed(true, _) => el.aria(AriaKey.invalid -> true)
        case Fixed(_, _)    => el
        case Live(s, _, _)  => el.aria(AriaKey.invalid -> s)

    /** The control followed by its message row. */
    def follow(control: UI)(using Frame): UI = this match
        case Fixed(r, message) =>
            message match
                case Present(m) if r => fragment(control, FieldValidity.row(m))
                case _               => control
        case Live(redSig, fromMessage, messageSig) =>
            // Wildcards rather than `case Absent`: `Maybe` is opaque, so the exhaustivity checker cannot see that
            // Present/Absent covers it, and -Werror turns the false warning into a failure.
            messageSig match
                case Present(message) if fromMessage => fragment(control, message.render(FieldValidity.rowIf(true, _)))
                case Present(message)                =>
                    fragment(control, redSig.combineLatest(message).render((r, m) => FieldValidity.rowIf(r, m)))
                case _ => control
end FieldValidity

private[uic] object FieldValidity:

    /** What a control holds of its validity, as its setters stored it (the slots [[HasValidity]] offers). The one field
      * `validityV` of a control's state; [[apply]] resolves it per render.
      */
    final case class Slots(
        invalid: Maybe[BoolValue] = Absent,
        message: Maybe[String] = Absent,
        messageDyn: Maybe[Signal[Maybe[String]]] = Absent
    )

    object Slots:
        /** A control with nothing set, shared so that a control nobody marks stores nothing of its own for it. */
        val empty: Slots = Slots()

    /** Resolves a control's validity slots. */
    def apply(slots: Slots)(using Frame): FieldValidity =
        val Slots(invalid, message, messageDyn) = slots
        (invalid.dynSig, messageDyn) match
            case (Present(red), Present(msg)) => Live(red, redFromMessage = false, Present(msg))
            case (Present(red), _)            =>
                Live(red, redFromMessage = false, message.map(m => Signal.initConst[Maybe[String]](Present(m))))
            // A constant `invalid` is as explicit as a signal one, so it decides the red state over the message too.
            case (_, Present(msg)) if invalid.isDefined =>
                Live(Signal.initConst(invalid.constTrue), redFromMessage = false, Present(msg))
            case (_, Present(msg)) => Live(msg.map(_.isDefined), redFromMessage = true, Present(msg))
            case _                 => Fixed(invalid.constTrue, message)
        end match
    end apply

    private def row(m: String)(using Frame): UI = div.cssClass("p-uic-invalid-message")(m)

    private def rowIf(red: Boolean, message: Maybe[String])(using Frame): UI =
        message match
            case Present(m) if red => row(m)
            case _                 => UI.empty
end FieldValidity
