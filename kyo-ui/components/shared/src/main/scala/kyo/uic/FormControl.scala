package kyo.uic

import kyo.*
import scala.annotation.targetName

/** The validation vocabulary every control that holds a user-supplied value carries,
  * and the surface the form-validation layer binds against: [[TextFormControl]] /
  * [[BooleanFormControl]] / [[NumberFormControl]] / [[ChoiceFormControl]] / [[MultiChoiceFormControl]] /
  * [[OptionalFormControl]] / [[FileFormControl]] each add their value binding and blur trigger on top, and each
  * preserves the concrete `Self` type (Input, TextArea, Slider, ...) at the call site,
  * with no opaque wrapper.
  *
  * The trait carries the validation setters ITSELF rather than declaring them. The
  * vocabulary used to be unevenly distributed (some controls had the pair, some had
  * `invalid(Boolean)` alone, some had neither), so a user who learned it on `Input`
  * found half of it on the next control and none on the third, with no rule saying
  * which. Defining them here makes half-presence unrepresentable AND spells the
  * behaviour once: a control cannot forget a member, and cannot describe one
  * differently from its neighbour.
  *
  * A control supplies the one-line reader and writer below instead of the setters: its state
  * holds the slots as one `validityV` ([[FieldValidity.Slots]]), and `type Self = Input` fixes
  * the return type, so a control opts in by adding the trait to its `extends` clause plus those
  * two lines.
  *
  * `size` and `variant` are deliberately NOT here. They are design-system facts, not
  * validation: the PrimeOne sheet defines `.p-*-sm`/`-lg` and `.p-variant-filled`
  * only for the field-shaped controls, so requiring them would mean inventing CSS for
  * a Slider or a Rating that Prime does not style that way.
  */
trait FormControl extends Node, HasElementId:
    /** The slots as they are. Implemented as `validityV`. */
    private[uic] def validitySlots: FieldValidity.Slots

    /** Stores the slots. Implemented as `new Input(s.copy(validityV = v))`. */
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
end FormControl

/** A `String`-valued text field: the value binding plus a focus-loss trigger. The binding is the union every value slot in
  * the module takes: a constant, a writable `SignalRef[String]` (two-way, which is what `bind` passes), or any other
  * `Signal[String]` (one-way).
  */
trait TextFormControl extends FormControl:
    def value(v: String | Signal[String]): Self
    def onBlur(f: String => UI.Action)(using Frame): Self

/** A choice of one among typed options ([[Select]], [[CascadeSelect]], a single [[SelectButton]]): the option itself, over
  * `A` where the field always holds one or over `Maybe[A]` where it may stay empty, plus a focus-loss trigger that reports
  * the held option (`Absent` for none).
  *
  * `selection` takes the module's union: a `SignalRef` binds two-way, any other `Signal` or a constant one-way. `bind`
  * passes the field's own ref, so a form always binds two-way.
  *
  * The field's value type is the option type, so `bind` takes a `FormField[A]` or a `FormField[Maybe[A]]` and a rule judges
  * the option rather than a key standing in for it. The `@targetName` is there because both bindings erase to one signature;
  * the call site reads `selection(...)` either way.
  */
trait ChoiceFormControl[A] extends FormControl:
    def selection(v: A | Signal[A]): Self
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): Self
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): Self
end ChoiceFormControl

/** A choice of several among typed options ([[MultiSelect]], [[MultiSelectButton]], [[Listbox]], [[TreeSelect]]): the set of chosen options
  * (a ref binds two way, a signal or a constant one way, as in [[ChoiceFormControl]]), plus a focus-loss trigger that
  * reports it. `bind` takes a `FormField[Set[A]]` and hands its ref over, so a bound field is two-way.
  *
  * The members carry a `@targetName` because `SignalRef[Set[A]]` and `Set[A] => ?` erase to the same signatures as
  * [[ChoiceFormControl]]'s members, so a control may carry both traits without a clash.
  */
trait MultiChoiceFormControl[A] extends FormControl:
    @targetName("selectionSet")
    def selection(v: Set[A] | Signal[Set[A]]): Self
    @targetName("onBlurSet")
    def onBlur(f: Set[A] => UI.Action)(using Frame): Self
end MultiChoiceFormControl

/** A control holding an optional value of the app's own type ([[DatePicker]], whose value type is its kind): the value bound
  * two-way as `Maybe[A]`, `Absent` while nothing is picked, plus a focus-loss trigger that reports it.
  *
  * The field's value type is `Maybe[A]`, so `bind` takes a `FormField[Maybe[A]]` and a rule judges the typed value itself,
  * with `Validator.chosen` as its `required` and `Validator.ifPresent` to state a rule over the value without making it
  * required. There is no second, primitive field behind it: the control reads and writes the field's own ref.
  */
trait OptionalFormControl[A] extends FormControl:
    def value(v: Maybe[A] | Signal[Maybe[A]]): Self
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): Self
end OptionalFormControl

/** A `Boolean`-valued field (checkbox / switch / radio): checked binding plus
  * focus-loss. `onBlur` reports the field's current checked state. The binding takes a
  * constant, a writable `SignalRef[Boolean]` (two-way, which is what `bind` passes), or any
  * other `Signal[Boolean]` (one-way).
  */
trait BooleanFormControl extends FormControl:
    def checked(v: Boolean | Signal[Boolean]): Self
    def onBlur(f: Boolean => UI.Action)(using Frame): Self

object BooleanFormControl:
    /** A component written by hand has `checked` as its member, so generic code reaches it the way it reaches a generated one. */
    given checked[C <: BooleanFormControl { type Self <: C }]: HasSetter[C, "checked", UI.Prop[Boolean]] with
        def apply(c: C, v: UI.Prop[Boolean])(using Frame): C = c.checked(v)

    /** [[checked]] for `onBlur`. */
    given onBlur[C <: BooleanFormControl { type Self <: C }]: HasSetter[C, "onBlur", Boolean => UI.Action] with
        def apply(c: C, f: Boolean => UI.Action)(using Frame): C = c.onBlur(f)
end BooleanFormControl

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
    def onBlur(f: Double => UI.Action)(using Frame): Self
    def integer(v: Boolean): Self
end NumberFormControl

/** A file-picking control ([[FileUpload]]) bound to the picked files' metadata. The
  * value type is `Seq[kyo.UI.FilePayload]`: name, size, MIME type and text content of
  * each selected file, which is what a validator needs to check a size cap or an
  * extension, and what a submit handler needs to upload.
  */
trait FileFormControl extends FormControl:
    def value(v: Seq[UI.FilePayload] | Signal[Seq[UI.FilePayload]]): Self
    def onBlur(f: Seq[UI.FilePayload] => UI.Action)(using Frame): Self
