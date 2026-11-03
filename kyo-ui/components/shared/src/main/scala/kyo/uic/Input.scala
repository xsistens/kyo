package kyo.uic

import kyo.*
import kyo.UI.*

/** Input type variants: selects which concrete kyo text-input factory backs the
  * field, so the native `type` attribute is real (email keyboards on mobile,
  * password masking, numeric spinners, ...). Hand-authored: it belongs to the
  * hand-written [[Input]] control.
  */
enum InputType derives CanEqual:
    case Text, Email, Number, Password, Tel, URL

    private[uic] def token: String = this.toString
end InputType

/** Text input: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputText: the component IS the bare `<input class="p-inputtext">`, no
  * wrapper element). Composes the kyo `<input>` element, so two-way binding
  * (`value(SignalRef)` → `Bound.Ref`), `onInput`/`onChange`, and the
  * caret-preserving morph path are all inherited from kyo-ui, not rebuilt. Any
  * other `Signal[String]` binds one way: the field shows it, and an edit only
  * reports through `onInput`/`onChange`.
  * Validation is Prime's `.p-invalid` model: `invalid(Boolean)` plus the
  * optional `invalidMessage(String)` row below the field.
  */
final case class Input private (
    valueSlot: Slot[String] = Slot(),
    placeholderText: Maybe[TextValue] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Maybe[BoolValue] = Absent,
    requiredFlag: Maybe[Boolean] = Absent,
    nameV: Maybe[String] = Absent,
    maxLengthV: Maybe[Int] = Absent,
    typeV: InputType = InputType.Text,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    accDescV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[String => UI.Action] = Absent,
    onBlurF: Maybe[String => UI.Action] = Absent,
    inputFilterV: Maybe[String] = Absent,
    inputMaskV: Maybe[String] = Absent,
    extraClassesV: List[String] = Nil,
    idV: Maybe[String] = Absent,
    focusAutoFlag: Boolean = false,
    unheardAsV: Slot.Name = Input.valueName("Input", "onInput or onChange"),
    filledUpV: Maybe[(String, String)] = Absent
) extends Node, TextFormControl, HasPlaceholder, HasAccessibleNameRef, HasAccessibleDescription:
    type Self = Input

    /** Package-internal class hook: wrappers (FloatLabel/IftaLabel) stamp Prime's
      * state classes (`p-filled`) onto the field they wrap.
      */
    private[uic] def extraClass(cls: String): Input = copy(extraClassesV = extraClassesV :+ cls)

    /** Package-internal: the component and the events the unheard card names, for a host whose field this is and whose
      * own events are what hear it ([[InputMask]]).
      */
    private[uic] def unheardAs(component: String, events: String): Input = copy(unheardAsV = Input.valueName(component, events))

    /** Package-internal: the class a host's wrapper takes while this field holds text, and the selector that finds the
      * wrapper (kyo-ui's `filledClass(name, closest)`), for a host whose stylesheet marks its wrapper ([[Password]]).
      */
    private[uic] def filledOn(cls: String, closest: String): Input = copy(filledUpV = Present((cls, closest)))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Input = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Seeds focus onto this field when a re-render inserts it into the DOM for the first
      * time, kyo-ui's `focusAuto`. A field that appears in place of something else, a cell
      * that swaps its text for an editor being the case this exists for, is focusable but
      * not focused: the element is new, so nothing carried focus into it, and a handler
      * that fired before the insert cannot reach it either. The flag is declarative for
      * that reason, and it is why an imperative `focusId` is the wrong tool here.
      *
      * At most one focus-auto element is seeded per patch, so several fields appearing
      * together (a row of editors) hand focus to the first.
      */
    def focusAuto(v: Boolean): Input = copy(focusAutoFlag = v)

    /** The value. A `SignalRef` binds TWO-WAY: an edit writes it, and its changes update the field. Any other signal
      * binds ONE-WAY: the field shows it, and an edit writes nothing and only reports through [[onInput]] and
      * [[onChange]], so the app writes its own state and the field follows; with neither, a card says the reader cannot
      * change it. A constant is the starting text, which the reader edits freely. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): Input = copy(valueSlot = valueSlot.bind(v))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): Input = copy(placeholderText = v)

    /** Disabled, constant or reactive: a `Signal[Boolean]` toggles the native `disabled` attribute IN
      * PLACE via kyo-ui's boolean-attribute channel (no re-render, so caret/focus survive), e.g. disable
      * until another field is valid; a plain `Boolean` sets it statically.
      */
    def disabled(v: Boolean | Signal[Boolean]): Input = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `readonly` (focusable but not editable), constant or reactive: a `Signal[Boolean]` toggles
      * it IN PLACE via the boolean-attribute channel (no re-render, so caret/focus survive).
      */
    def readonly(v: Boolean | Signal[Boolean]): Input = copy(readonlyFlag = Present(ReactiveValue(v)))

    /** Marks the field required (native constraint + `aria-required`). */
    def required(v: Boolean): Input = copy(requiredFlag = Present(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): Input = copy(nameV = Present(v))

    /** Native `maxlength`: caps the number of characters. */
    def maxLength(n: Int): Input = copy(maxLengthV = Present(math.max(0, n)))

    /** Selects the native input type; defaults to [[InputType.Text]]. */
    def inputType(t: InputType): Input = copy(typeV = t)

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): Input = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): Input = copy(variantV = v)

    /** Spans the full width of its container (`.p-inputtext-fluid`). */
    def fluid(v: Boolean): Input = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): Input                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): Input                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Input = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Input = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Input = copy(accNameRefV = v)

    private[uic] def withAccessibleDescription(v: Maybe[TextValue]): Input = copy(accDescV = v)

    def onInput(f: String => UI.Action)(using Frame): Input  = copy(valueSlot = valueSlot.hear(f))
    def onChange(f: String => UI.Action)(using Frame): Input = copy(onChangeF = Present(Callback.of(f)), valueSlot = valueSlot.heardToo)

    /** Fires on focus loss (native `blur`) with the text the field holds as focus
      * leaves it, which kyo-ui's blur carries (its two-parameter `onBlur`): what the reader typed,
      * also when nothing binds the field or it started from a constant. Unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => UI.Action)(using Frame): Input = copy(onBlurF = Present(Callback.of(f)))

    /** Client-local keystroke filter: `"int"` keeps only digits, `"decimal"` also
      * allows one separator, any other string is a whitelist charset. The client
      * rejects disallowed `beforeinput` keystrokes BEFORE they reach the field, so
      * (unlike a server-side `takeRight` reject) an invalid character never becomes
      * visible. The already-filtered value is what reaches `onInput`/the ref.
      *
      * The string stays this library's surface; kyo takes a typed
      * [[kyo.UI.InputFilter]], which `Input.asInputFilter` maps onto.
      */
    def inputFilter(pattern: String): Input = copy(inputFilterV = Present(pattern))

    /** Client-local input mask (the fork's `inputMask`), e.g. `"(999) 999-9999"`:
      * `9` = digit, `a` = letter, `*` = alphanumeric; every other character is a
      * literal placeholder. The client formats the field as the user types and
      * the MASKED value is what reaches `onInput`/`onChange`/the ref. Prefer the
      * dedicated [[InputMask]] component for phone/date/ssn fields.
      */
    def inputMask(mask: String): Input = copy(inputMaskV = Present(mask))

    private[uic] def render(using Frame): UI =
        val validity  = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val built: UI = typeV match
            case InputType.Text =>
                buildField(
                    withConstraints(input),
                    _.placeholder(_),
                    _.onInput(_),
                    _.onChange(_),
                    validity
                )
            case InputType.Email =>
                buildField(
                    withConstraints(emailInput),
                    _.placeholder(_),
                    _.onInput(_),
                    _.onChange(_),
                    validity
                )
            case InputType.Number =>
                buildField(numberInput, _.placeholder(_), _.onInput(_), _.onChange(_), validity)
            case InputType.Password =>
                buildField(
                    withConstraints(passwordInput),
                    _.placeholder(_),
                    _.onInput(_),
                    _.onChange(_),
                    validity
                )
            case InputType.Tel =>
                buildField(
                    withConstraints(telInput),
                    _.placeholder(_),
                    _.onInput(_),
                    _.onChange(_),
                    validity
                )
            case InputType.URL =>
                buildField(
                    withConstraints(urlInput),
                    _.placeholder(_),
                    _.onInput(_),
                    _.onChange(_),
                    validity
                )
        Diagnostics.after(validity.follow(built))(unheardAsV.component)(
            Diagnostic.unheard(valueSlot, unheardAsV, on = !readonlyFlag.constTrue && !disabledFlag.constTrue)
        )
    end render

    /** Applies the shared attribute chain to a concrete kyo text-input factory. The
      * F-bounded refinement (`Self <: T`) keeps every `Self`-returning setter
      * chainable across the six concrete input types selected by [[InputType]]; the
      * three setters kyo declares only on the concrete classes (not on the
      * `TextInput` trait) arrive as lambdas. The bound intersects `Interactive` because
      * focus seeding lives there and not on `TextInput`, while every concrete input class
      * is both.
      */
    private def buildField[T <: (UI.Ast.TextInput & UI.Ast.Interactive) { type Self <: T }](
        base: T,
        setPlaceholder: (T, String) => T,
        setOnInput: (T, String => UI.Action) => T,
        setOnChange: (T, String => UI.Action) => T,
        validity: FieldValidity
    )(using Frame): UI =
        var b: T = base.cssClass("p-inputtext").cssClass("p-component")
        extraClassesV.foreach(c => b = b.cssClass(c))
        idV.foreach(v => b = b.id(v))
        if focusAutoFlag then b = b.focusAuto(true)
        sizeV match
            case Size.Small  => b = b.cssClass("p-inputtext-sm")
            case Size.Large  => b = b.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then b = b.cssClass("p-variant-filled")
        b = validity.mark(b)
        if fluidFlag then b = b.cssClass("p-inputtext-fluid")
        // Prime's `p-filled` follows the text the field holds, which the client knows and a render does not (an unbound
        // field's text, the reader's edit of a starting one): kyo-ui toggles it there, the static render from the start.
        b = b.filledClass("p-filled")
        filledUpV.foreach((cls, closest) => b = b.filledClass(cls, closest))
        // A two-way ref is kyo-ui's own binding. A one-way signal is shown in place, with no region around the field, so a keystroke renders
        // nothing: kyo-ui puts back an edit its owner did not take. A constant is the starting text.
        valueSlot.bound.foreach(v => b = b.value(v))
        // A constant placeholder joins the attribute chain via the concrete factory's
        // String setter; a reactive one uses kyo-ui's placeholder patch channel (in-place
        // setAttribute, no re-render), declared on the shared TextInput bound.
        b = placeholderText match
            case Present(TextValue.Const(v)) => setPlaceholder(b, v)
            case Present(TextValue.Dyn(sig)) => b.placeholder(sig)
            case Absent                      => b
        b = disabledFlag.foldFlag(b)(b.disabled(_))
        b = readonlyFlag.foldFlag(b)(b.readOnly(_))
        if requiredFlag.getOrElse(false) then b = b.aria("required", "true").jsProp("required", "true")
        b = nameV.map(v => b.jsProp("name", v)).getOrElse(b)
        b = maxLengthV.map(n => b.jsProp("maxLength", n.toString)).getOrElse(b)
        b = accNameV match
            case Present(TextValue.Const(v)) => b.aria("label", v)
            case Present(TextValue.Dyn(s))   => b.aria("label", s)
            case Absent                      => b
        b = accNameRefV.map(v => b.aria("labelledby", v)).getOrElse(b)
        b = accDescV match
            case Present(TextValue.Const(v)) => b.aria("description", v)
            case Present(TextValue.Dyn(s))   => b.aria("description", s)
            case Absent                      => b
        b = valueSlot.hearer.map(f => setOnInput(b, f)).getOrElse(b)
        b = onChangeF.map(f => setOnChange(b, f)).getOrElse(b)
        // The blur carries the text the field holds as focus leaves it: what the reader typed, whatever binds the field.
        b = onBlurF.map(f => b.onBlur((_, text) => f(text))).getOrElse(b)
        b
    end buildField

    /** Applies the declarative typing constraints, which kyo puts on `ConstrainedInput`
      * rather than `TextInput`: `NumberInput` deliberately does not mix the trait in,
      * because a `number` field reads its value as the empty string while the content is
      * not a valid number, so a character-level filter cannot see what it is filtering.
      * Kept out of [[buildField]] so the Number branch still type-checks; it constrains
      * its content through `type`/`min`/`max`/`step` instead.
      */
    private def withConstraints[T <: UI.Ast.ConstrainedInput { type Self <: T }](base: T): T =
        var b = base
        b = inputFilterV.map(p => b.inputFilter(Input.asInputFilter(p))).getOrElse(b)
        b = inputMaskV.map(m => b.inputMask(m)).getOrElse(b)
        b
    end withConstraints
end Input

object Input:
    def apply(): Input = new Input()

    private[uic] def valueName(component: String, events: String) = Slot.Name(component, "value", "a change", events)

    /** Maps this library's filter string onto kyo's typed [[kyo.UI.InputFilter]]. `"int"` is
      * `Digits` (digits only; kyo's set carries no sign character), `"decimal"` is `Decimal`,
      * and anything else is taken as an explicit whitelist charset.
      */
    private[uic] def asInputFilter(pattern: String): UI.InputFilter =
        pattern match
            case "int"     => UI.InputFilter.Digits
            case "decimal" => UI.InputFilter.Decimal
            case chars     => UI.InputFilter.Allowed(chars)
end Input
