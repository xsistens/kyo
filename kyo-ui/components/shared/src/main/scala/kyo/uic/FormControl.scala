package kyo.uic

import kyo.*

/** Reactive-error surface shared by every migrated form field. The form-validation
  * layer binds against [[TextFormControl]] / [[BooleanFormControl]] so it can wire a
  * field's value, validity, message, and blur trigger WHILE preserving the concrete
  * `Self` type (Input, TextArea, ...) at the call site — no opaque wrapper.
  *
  * Every member is already satisfied by the field's existing `copy`-based setters
  * (`type Self = Input` fixes the return type), so a field opts in simply by adding
  * the trait to its `extends` clause.
  */
trait FormControl extends Node:
    def invalid(sig: Signal[Boolean]): Self
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

/** A `Double`-valued numeric field (InputNumber): two-way value binding, a focus-loss
  * trigger reporting the field's current number, and an integer-only mask that the form
  * layer flips on for whole-valued (`Int`/`Long`) number fields.
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
  */
trait MultiSelectFormControl extends FormControl:
    def value(ref: SignalRef[Set[String]]): Self
    def onBlur(f: Set[String] => Any < Async): Self
