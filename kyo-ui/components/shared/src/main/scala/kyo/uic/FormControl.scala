package kyo.uic

import kyo.*
import scala.annotation.targetName

/** The validation vocabulary every control that holds a user-supplied value carries,
  * and the surface the form-validation layer binds against: [[TextFormControl]] /
  * [[BooleanFormControl]] / [[NumberFormControl]] / [[MultiSelectFormControl]] /
  * [[FileFormControl]] each add their value binding and blur trigger on top, and each
  * preserves the concrete `Self` type (Input, TextArea, Slider, ...) at the call site
  * — no opaque wrapper.
  *
  * The trait carries the validation setters ITSELF rather than declaring them. The
  * vocabulary used to be unevenly distributed — some controls had the pair, some had
  * `invalid(Boolean)` alone, some had neither — so a user who learned it on `Input`
  * found half of it on the next control and none on the third, with no rule saying
  * which. Defining them here makes half-presence unrepresentable AND spells the
  * behaviour once: a control cannot forget a member, and cannot describe one
  * differently from its neighbour.
  *
  * A control supplies the three one-line WRITERS below instead of the setters; every
  * control already stores the slots under the same three types, and `type Self = Input`
  * fixes the return type, so a control opts in by adding the trait to its `extends`
  * clause plus those writers.
  *
  * `size` and `variant` are deliberately NOT here. They are design-system facts, not
  * validation: the PrimeOne sheet defines `.p-*-sm`/`-lg` and `.p-variant-filled`
  * only for the field-shaped controls, so requiring them would mean inventing CSS for
  * a Slider or a Rating that Prime does not style that way.
  */
trait FormControl extends Node, HasElementId:
    /** Stores the resolved invalid slot. Implemented as `copy(invalidV = v)`. */
    private[uic] def withInvalid(v: Maybe[BoolValue]): Self

    /** Stores the constant invalid message. Implemented as `copy(invalidMsgV = v)`. */
    private[uic] def withInvalidMessage(v: Maybe[String]): Self

    /** Stores the reactive invalid message. Implemented as `copy(invalidMsgDynV = v)`. */
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Self

    /** Marks the control invalid (`.p-invalid` + `aria-invalid`). A `Signal[Boolean]` toggles the
      * invalid state on emission, and is the explicit override of the message-derived red default.
      */
    final def invalid(v: Boolean | Signal[Boolean]): Self = withInvalid(Present(ReactiveValue(v)))

    /** Message rendered below the control while it is invalid (`div.p-uic-invalid-message`). */
    final def invalidMessage(v: String): Self = withInvalidMessage(Present(v))

    /** Reactive message: `Present` shows the row and (by default) turns the control red,
      * `Absent` clears both. This is what `bind` wires to the field's gated message.
      *
      * Separate from the constant setter rather than folded into a union, because the two carry
      * DIFFERENT element types: a constant message is always shown, while the reactive one is an
      * optional message that can also clear the row. `String | Signal[Maybe[String]]` would be a
      * heterogeneous union, not the `A | Signal[A]` shape the rest of the module uses.
      */
    final def invalidMessage(sig: Signal[Maybe[String]]): Self = withInvalidMessageDyn(Present(sig))

    // `id` comes from HasElementId. The form layer stamps each field's minted id there at
    // `bind` time so focus-first-invalid can address the control's focusable element
    // (`Commands.focusId`); pair it with `Label.forId` off `form.FormField.domId`. That the
    // id lands on the FOCUSABLE element rather than the root is the field-shaped half of the
    // trait's contract, and the reason the slot started here.
end FormControl

/** A `String`-valued text field: two-way value binding plus a focus-loss trigger. */
trait TextFormControl extends FormControl:
    def value(ref: SignalRef[String]): Self
    def onBlur(f: String => Any < Async): Self

/** A `Boolean`-valued field (checkbox / switch / radio): checked binding plus
  * focus-loss. `onBlur` reports the field's current checked state.
  */
trait BooleanFormControl extends FormControl:
    def checked(ref: SignalRef[Boolean]): Self
    def onBlur(f: Boolean => Any < Async): Self

/** A `Double`-valued numeric control (InputNumber, Slider, Knob, Rating): two-way value
  * binding, a focus-loss trigger reporting the current number, and a whole-number
  * constraint that the form layer flips on for whole-valued (`Int`/`Long`) number
  * fields.
  *
  * `integer` is a CONSTRAINT, not a keystroke filter, which is why every member of the
  * family can honour it: InputNumber masks decimal entry, Slider and Knob round the
  * committed value, and Rating already only produces whole stars, so the constraint is
  * satisfied by construction there.
  *
  * The value binding is the union every value slot in the module takes: a constant, a
  * writable `SignalRef[Double]` (two-way, which is what `bind` passes), or any other
  * `Signal[Double]` (one-way). The `@targetName` is for [[Rating]], whose own `Int` star
  * binding erases to the same JVM signature as this `Double` one; the call site still reads
  * `value(...)`.
  */
trait NumberFormControl extends FormControl:
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): Self
    def onBlur(f: Double => Any < Async): Self
    def integer(v: Boolean): Self
end NumberFormControl

/** A multi-selection field bound to the set of selected option keys (MultiSelect /
  * TreeSelect): two-way value binding plus a focus-loss trigger reporting the
  * current selection. The value type is `Set[String]` — the option *keys*, not the
  * option model `A`, which is exactly what the control persists.
  *
  * The two members carry a `@targetName`: `SignalRef[Set[String]]` and
  * `Set[String] => ?` erase to the same JVM signatures as [[TextFormControl]]'s
  * `SignalRef[String]` and `String => ?`, and a control that offers BOTH arities
  * ([[SelectButton]], single or `multiple(true)`) has to implement both. The call site
  * still reads `value(...)` / `onBlur(...)`; only the JVM name differs.
  */
trait MultiSelectFormControl extends FormControl:
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): Self
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): Self
end MultiSelectFormControl

/** A file-picking control ([[FileUpload]]) bound to the picked files' metadata. The
  * value type is `Seq[kyo.UI.FilePayload]` — name, size, MIME type and text content of
  * each selected file, which is what a validator needs to check a size cap or an
  * extension, and what a submit handler needs to upload.
  */
trait FileFormControl extends FormControl:
    def value(ref: SignalRef[Seq[UI.FilePayload]]): Self
    def onBlur(f: Seq[UI.FilePayload] => Any < Async): Self
