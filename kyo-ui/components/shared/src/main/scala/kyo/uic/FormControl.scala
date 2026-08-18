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
  * The trait carries the CONSTANT setters as well as the reactive ones on purpose.
  * The vocabulary used to be unevenly distributed — some controls had the pair, some
  * had `invalid(Boolean)` alone, some had neither — so a user who learned it on
  * `Input` found half of it on the next control and none on the third, with no rule
  * saying which. Declaring all four here makes half-presence unrepresentable: a
  * control that cannot answer them cannot claim the trait, and a control that claims
  * it cannot forget one.
  *
  * `size` and `variant` are deliberately NOT here. They are design-system facts, not
  * validation: the PrimeOne sheet defines `.p-*-sm`/`-lg` and `.p-variant-filled`
  * only for the field-shaped controls, so requiring them would mean inventing CSS for
  * a Slider or a Rating that Prime does not style that way.
  *
  * Every member is satisfied by the control's existing `copy`-based setters
  * (`type Self = Input` fixes the return type), so a control opts in by adding the
  * trait to its `extends` clause.
  */
trait FormControl extends Node:
    /** Marks the control invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Self

    /** Reactive validity — the bound signal toggles the invalid state on emission. */
    def invalid(sig: Signal[Boolean]): Self

    /** Message rendered below the control while it is invalid (`div.p-uic-invalid-message`). */
    def invalidMessage(v: String): Self

    /** Reactive message: `Present` shows the row and (by default) turns the control red,
      * `Absent` clears both. This is what `bind` wires to the field's gated message.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Self

    /** Native element `id`. The form layer stamps each field's minted id here at
      * `bind` time so focus-first-invalid can address the control's focusable element
      * (`Commands.focusId`); pair it with `Label.forId` off [[form.FormField.domId]].
      */
    def id(v: String): Self
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
  */
trait NumberFormControl extends FormControl:
    def value(ref: SignalRef[Double]): Self
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
