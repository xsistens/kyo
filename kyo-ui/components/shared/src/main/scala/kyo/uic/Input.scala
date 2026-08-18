package kyo.uic

import kyo.*
import kyo.UI.*

/** Input type variants — selects which concrete kyo text-input factory backs the
  * field, so the native `type` attribute is real (email keyboards on mobile,
  * password masking, numeric spinners, ...). Hand-authored: it belongs to the
  * hand-written [[Input]] control.
  */
enum InputType derives CanEqual:
    case Text, Email, Number, Password, Tel, URL

    private[uic] def token: String = this.toString
end InputType

/** Text input — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputText: the component IS the bare `<input class="p-inputtext">`, no
  * wrapper element). Composes the kyo `<input>` element, so two-way binding
  * (`value(SignalRef)` → `Bound.Ref`), `onInput`/`onChange`, and the
  * caret-preserving morph path are all inherited from kyo-ui, not rebuilt.
  * Validation is Prime's `.p-invalid` model: `invalid(Boolean)` plus the
  * optional `invalidMessage(String)` row below the field.
  */
final case class Input private (
    valueBinding: Maybe[Input.Value] = Absent,
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
    onInputF: Maybe[String => Any < Async] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    inputFilterV: Maybe[String] = Absent,
    inputMaskV: Maybe[String] = Absent,
    extraClassesV: List[String] = Nil,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl:
    type Self = Input

    /** Package-internal class hook: wrappers (FloatLabel/IftaLabel) stamp Prime's
      * state classes (`p-filled`) onto the field they wrap.
      */
    private[uic] def extraClass(cls: String): Input = copy(extraClassesV = extraClassesV :+ cls)

    /** Native element `id` — pair with `Label.forId` / `FloatLabel.forId`. */
    def id(v: String): Input = copy(idV = Present(v))

    /** Sets a constant value. */
    def value(v: String): Input = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref`: edits write back into the ref, ref changes update the input. */
    def value(ref: SignalRef[String]): Input = copy(valueBinding = Present(Input.Value.Ref(ref)))

    def placeholder(v: String): Input = copy(placeholderText = Present(TextValue.Const(v)))

    /** Reactive placeholder that tracks `sig` — patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render/re-mount of the field) on each emission
      * (e.g. a locale-driven `I18n.t` leaf).
      */
    def placeholder(sig: Signal[String]): Input = copy(placeholderText = Present(TextValue.Dyn(sig)))

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

    /** Native `maxlength` — caps the number of characters. */
    def maxLength(n: Int): Input = copy(maxLengthV = Present(math.max(0, n)))

    /** Selects the native input type; defaults to [[InputType.Text]]. */
    def inputType(t: InputType): Input = copy(typeV = t)

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): Input = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): Input = copy(variantV = v)

    /** Spans the full width of its container (`.p-inputtext-fluid`). */
    def fluid(v: Boolean): Input = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Input = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): Input = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): Input = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf or a validation error signal).
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Input = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): Input = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render), e.g. a locale-driven `I18n.t` leaf.
      */
    def accessibleName(sig: Signal[String]): Input = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): Input = copy(accNameRefV = Present(v))

    /** Accessible description → `aria-description`. */
    def accessibleDescription(v: String): Input = copy(accDescV = Present(TextValue.Const(v)))

    /** Reactive accessible description — `aria-description` patched IN PLACE via kyo-ui's
      * attribute channel (`setAttribute`, no re-render).
      */
    def accessibleDescription(sig: Signal[String]): Input = copy(accDescV = Present(TextValue.Dyn(sig)))

    def onInput(f: String => Any < Async): Input  = copy(onInputF = Present(f))
    def onChange(f: String => Any < Async): Input = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the field's current value — unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): Input = copy(onBlurF = Present(f))

    /** Client-local keystroke filter: `"int"` keeps only digits, `"decimal"` also
      * allows one separator, any other string is a whitelist charset. The client
      * rejects disallowed `beforeinput` keystrokes BEFORE they reach the field, so —
      * unlike a server-side `takeRight` reject — an invalid character never becomes
      * visible. The already-filtered value is what reaches `onInput`/the ref.
      *
      * The string stays this library's surface; kyo takes a typed
      * [[kyo.UI.InputFilter]], which [[asInputFilter]] maps onto.
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
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
        val field: UI = typeV match
            case InputType.Text =>
                buildField(withConstraints(input), _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
            case InputType.Email =>
                buildField(withConstraints(emailInput), _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
            case InputType.Number => buildField(numberInput, _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
            case InputType.Password =>
                buildField(withConstraints(passwordInput), _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
            case InputType.Tel =>
                buildField(withConstraints(telInput), _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
            case InputType.URL =>
                buildField(withConstraints(urlInput), _.placeholder(_), _.onInput(_), _.onChange(_), (bb, e) => bb.onBlur(e))
        FieldInvalid.withMessage(field, invalidV.constTrue, invalidMsgV)
    end renderStatic

    /** Applies the shared attribute chain to a concrete kyo text-input factory. The
      * F-bounded refinement (`Self <: T`) keeps every `Self`-returning setter
      * chainable across the six concrete input types selected by [[InputType]]; the
      * four setters kyo declares only on the concrete classes (not on the
      * `TextInput` trait) arrive as lambdas.
      */
    private def buildField[T <: UI.Ast.TextInput { type Self <: T }](
        base: T,
        setPlaceholder: (T, String) => T,
        setOnInput: (T, String => Any < Async) => T,
        setOnChange: (T, String => Any < Async) => T,
        setOnBlur: (T, Any < (Async & Sync)) => T
    )(using Frame): UI =
        var b: T = base.cssClass("p-inputtext").cssClass("p-component")
        extraClassesV.foreach(c => b = b.cssClass(c))
        idV.foreach(v => b = b.id(v))
        sizeV match
            case Size.Small  => b = b.cssClass("p-inputtext-sm")
            case Size.Large  => b = b.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then b = b.cssClass("p-variant-filled")
        if invalidV.constTrue then b = b.cssClass("p-invalid").aria("invalid", "true")
        if fluidFlag then b = b.cssClass("p-inputtext-fluid")
        valueBinding match
            case Present(Input.Value.Const(v)) => b = b.value(v)
            case Present(Input.Value.Ref(r))   => b = b.value(r)
            case Absent                        => ()
        end match
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
        b = onInputF.map(f => setOnInput(b, f)).getOrElse(b)
        b = onChangeF.map(f => setOnChange(b, f)).getOrElse(b)
        // onBlur carries no payload, so the current value is read from the bound ref
        // and the prepared effect is handed to the concrete factory's onBlur (passed
        // in like onInput/onChange, since neither is on the TextInput bound).
        b = onBlurF match
            case Present(f) =>
                val eff = valueBinding match
                    case Present(Input.Value.Ref(r))   => r.use(f)
                    case Present(Input.Value.Const(v)) => f(v)
                    case Absent                        => f("")
                setOnBlur(b, eff)
            case Absent => b
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

    /** Maps this library's filter string onto kyo's typed [[kyo.UI.InputFilter]]. `"int"` is
      * `Digits` (digits only — kyo's set carries no sign character), `"decimal"` is `Decimal`,
      * and anything else is taken as an explicit whitelist charset.
      */
    private[uic] def asInputFilter(pattern: String): UI.InputFilter =
        pattern match
            case "int"     => UI.InputFilter.Digits
            case "decimal" => UI.InputFilter.Decimal
            case chars     => UI.InputFilter.Allowed(chars)

    /** Const-or-ref carrier for the pending `value`. kyo's `UI.Ast.Bound` is the
      * AST-side equivalent, but its cases are only constructed via the element
      * setters, so the builder keeps its own until render time.
      */
    private[uic] enum Value:
        case Const(v: String)
        case Ref(ref: SignalRef[String])
end Input
