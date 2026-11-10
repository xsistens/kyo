package kyo.uic

import kyo.*

/** The validation vocabulary every control that holds a user-supplied value carries: `invalid`, `invalidMessage` and the id the
  * form layer addresses. Each [[FormControl]] has it, and so do the controls that are validated but bound by no form field
  * ([[MultiDatePicker]], [[RangeDatePicker]]). Each preserves the concrete `Self` type (Input, TextArea, Slider, ...) at the call
  * site, with no opaque wrapper.
  *
  * The trait carries the validation setters ITSELF rather than declaring them. The vocabulary used to be unevenly distributed
  * (some controls had the pair, some had `invalid(Boolean)` alone, some had neither), so a user who learned it on `Input` found
  * half of it on the next control and none on the third, with no rule saying which. Defining them here makes half-presence
  * unrepresentable AND spells the behaviour once: a control cannot forget a member, and cannot describe one differently from its
  * neighbour.
  *
  * A control supplies the one-line reader and writer below instead of the setters: its state holds the slots as one `validityV`
  * ([[FieldValidity.Slots]]), and `type Self = TextArea` fixes the return type, so a control opts in by adding the trait to its
  * `extends` clause plus those two lines.
  *
  * `size` and `variant` are deliberately NOT here. They are design-system facts, not validation: the PrimeOne sheet defines
  * `.p-*-sm`/`-lg` and `.p-variant-filled` only for the field-shaped controls, so requiring them would mean inventing CSS for a
  * Slider or a Rating that Prime does not style that way.
  */
trait HasValidity extends Node, HasElementId:
    /** The slots as they are. Implemented as `validityV`. */
    private[uic] def validitySlots: FieldValidity.Slots

    /** Stores the slots. Implemented as `new TextArea(p.copy(validityV = v))`. */
    private[uic] def withValidity(v: FieldValidity.Slots): Self

    /** Marks the control invalid (`.p-invalid` + `aria-invalid`). A `Signal[Boolean]` toggles the
      * invalid state on emission. Either form is the explicit override of the message-derived red
      * default: once `invalid` is set, it alone decides whether the control is red and whether a
      * message row shows.
      */
    final def invalid(v: Boolean | Signal[Boolean]): Self = withValidity(validitySlots.copy(invalid = Present(ReactiveValue(v))))

    /** Message rendered below the control while it is invalid (`div.p-uic-invalid-message`). */
    final def invalidMessage(v: String): Self = withValidity(validitySlots.copy(message = Present(v)))

    /** Reactive message: `Present` shows the row and (by default) turns the control red,
      * `Absent` clears both. This is what `bind` wires to the field's gated message.
      *
      * Separate from the constant setter rather than folded into a union, because the two carry
      * DIFFERENT element types: a constant message is always shown, while the reactive one is an
      * optional message that can also clear the row. `String | Signal[Maybe[String]]` would be a
      * heterogeneous union, not the `A | Signal[A]` shape the rest of the module uses.
      */
    final def invalidMessage(sig: Signal[Maybe[String]]): Self = withValidity(validitySlots.copy(messageDyn = Present(sig)))

    // `id` comes from HasElementId. The form layer stamps each field's minted id there at
    // `bind` time so focus-first-invalid can address the control's focusable element
    // (`Commands.focusId`); pair it with `Label.forId` off `form.FormField.domId`. That the
    // id lands on the FOCUSABLE element rather than the root is the field-shaped half of the
    // trait's contract, and the reason the slot started here.
end HasValidity

/** A control a form field binds, over the value type `A` the field holds: `FormControl[String]` for the text fields,
  * `FormControl[Boolean]` for the boolean ones, `FormControl[Double]` for the numeric ones, `FormControl[Maybe[A]]` for a single
  * choice or a date that may stay empty, `FormControl[Set[A]]` for several, `FormControl[Seq[UI.FilePayload]]` for picked files.
  *
  * `bind(field: FormField[A])` takes exactly the field of that type, so the value type alone picks the binding: a field of
  * another type is reported against the field the control binds. A control says how the form writes it in two package-internal
  * members, each one line over its own setters, whether `Setters` generated them or the class wrote them: the field's ref,
  * through the setter that binds the value (`value`, `checked`, `selection`), and the blur, through `onBlur`. A single choice
  * also binds a field that always holds an option, through `selection` over the option type, and a numeric control's whole-number
  * constraint is a setter the number binding asks for: both through [[HasSetter]].
  */
trait FormControl[A] extends HasValidity:
    /** Binds the field's ref, two-way. Implemented as the setter that binds the value: `value(ref)`, `checked(ref)` or
      * `selection(ref)`.
      */
    private[uic] def bindRef(ref: SignalRef[A])(using Frame): Self

    /** Hears focus loss. Implemented as `onBlur(f)`. */
    private[uic] def hearBlur(f: A => UI.Action)(using Frame): Self
end FormControl
