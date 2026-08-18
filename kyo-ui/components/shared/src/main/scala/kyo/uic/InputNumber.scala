package kyo.uic

import kyo.*
import kyo.UI.*

/** InputNumber button layout (Prime's `buttonLayout`): `Stacked` (default —
  * the up/down pair docked inside the field's end), `Horizontal` (decrement |
  * field | increment in a row) or `Vertical` (increment above, decrement
  * below a centered field).
  */
enum InputNumberButtonLayout derives CanEqual:
    case Stacked, Horizontal, Vertical

    private[uic] def token: String = this match
        case InputNumberButtonLayout.Stacked    => "stacked"
        case InputNumberButtonLayout.Horizontal => "horizontal"
        case InputNumberButtonLayout.Vertical   => "vertical"
end InputNumberButtonLayout

/** InputNumber — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputNumber anatomy: `span.p-inputnumber.p-component.p-inputwrapper` >
  * `input.p-inputnumber-input.p-inputtext` plus, with `showButtons(true)`, the
  * spin buttons `.p-inputnumber-button.p-inputnumber-increment-button` /
  * `-decrement-button` — stacked inside a `.p-inputnumber-button-group`,
  * horizontal/vertical as ordered siblings), so the extracted `@primeuix`
  * inputnumber CSS applies verbatim.
  *
  * The field is a REAL `<input type=number>` (kyo's `numberInput`): `min`/
  * `max`/`step` are native constraints, and edits deliver a `Double` through
  * kyo's numeric change payload. `value(SignalRef[Double])` binds two-way:
  * typing (on commit) and the spin buttons write the clamped value back before
  * firing `onChange`. Spinning steps `value ± step` clamped into `[min, max]`.
  *
  * Deliberate deviation from Prime: locale/currency/thousand-separator
  * FORMATTING while typing is keystroke-bound client JS in Prime (its field is
  * `type=text` plus a formatter); a server-driven component keeps the native
  * number field instead — `prefix`/`suffix` render as STATIC adornment spans
  * (`.p-uic-inputnumber-prefix`/`-suffix`, kyo extension) beside the field,
  * never inside the typed value, and no live digit-grouping mask is applied.
  */
final case class InputNumber private (
    valueBinding: Maybe[InputNumber.Value] = Absent,
    minV: Maybe[Double] = Absent,
    maxV: Maybe[Double] = Absent,
    stepV: Double = 1.0,
    showButtonsFlag: Boolean = false,
    buttonLayoutV: InputNumberButtonLayout = InputNumberButtonLayout.Stacked,
    prefixV: Maybe[String] = Absent,
    suffixV: Maybe[String] = Absent,
    placeholderText: Maybe[TextValue] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    idV: Maybe[String] = Absent,
    onChangeF: Maybe[Double => Any < Async] = Absent,
    onBlurF: Maybe[Double => Any < Async] = Absent,
    integerFlag: Boolean = false
) extends Node, NumberFormControl:
    type Self = InputNumber

    /** Sets a constant value. */
    def value(v: Double): InputNumber = copy(valueBinding = Present(InputNumber.Value.Const(v)))

    /** Binds two-way to `ref`: edits and spin clicks write the clamped value back,
      * ref changes re-render the field.
      */
    def value(ref: SignalRef[Double]): InputNumber = copy(valueBinding = Present(InputNumber.Value.Ref(ref)))

    /** Lower bound — native `min` plus the clamp floor for spins and commits. */
    def min(v: Double): InputNumber = copy(minV = Present(v))

    /** Upper bound — native `max` plus the clamp ceiling for spins and commits. */
    def max(v: Double): InputNumber = copy(maxV = Present(v))

    /** Spin/arrow increment (native `step`; Prime default 1). */
    def step(v: Double): InputNumber = copy(stepV = if v <= 0 then 1.0 else v)

    /** Shows the increment/decrement buttons (Prime's `showButtons`). */
    def showButtons(v: Boolean): InputNumber = copy(showButtonsFlag = v)

    /** Button placement — `Stacked` (default), `Horizontal` or `Vertical`. */
    def buttonLayout(v: InputNumberButtonLayout): InputNumber = copy(buttonLayoutV = v)

    /** Static adornment BEFORE the field (kyo extension `.p-uic-inputnumber-prefix`
      * — see the class doc: Prime's in-value formatting is deliberately not ported).
      */
    def prefix(v: String): InputNumber = copy(prefixV = Present(v))

    /** Static adornment AFTER the field (`.p-uic-inputnumber-suffix`). */
    def suffix(v: String): InputNumber = copy(suffixV = Present(v))

    def placeholder(v: String): InputNumber = copy(placeholderText = Present(TextValue.Const(v)))

    /** Reactive placeholder that tracks `sig` — patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render of the field) on each emission.
      */
    def placeholder(sig: Signal[String]): InputNumber = copy(placeholderText = Present(TextValue.Dyn(sig)))

    /** Disables the field + spin buttons; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): InputNumber = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `readonly` — focusable but not editable; the spin buttons stop too. */
    def readonly(v: Boolean): InputNumber = copy(readonlyFlag = v)

    /** Native `name` for HTML form participation. */
    def name(v: String): InputNumber = copy(nameV = Present(v))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field. */
    def size(v: Size): InputNumber = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is default. */
    def variant(v: FieldVariant): InputNumber = copy(variantV = v)

    /** Spans the full container width (`.p-inputnumber-fluid`). */
    def fluid(v: Boolean): InputNumber = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): InputNumber = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)`. */
    def invalidMessage(v: String): InputNumber = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): InputNumber = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): InputNumber = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label` on the input. */
    def accessibleName(v: String): InputNumber = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): InputNumber = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Native element `id` — pair with `Label.forId` / `FloatLabel.forId`. */
    def id(v: String): InputNumber = copy(idV = Present(v))

    /** Restricts entry to whole numbers: the client keystroke filter (`inputFilter("int")`)
      * rejects the decimal separator before it reaches the field, so the bound value stays
      * integral. The form layer turns this on automatically for `Int`/`Long` number fields.
      */
    def integer(v: Boolean): InputNumber = copy(integerFlag = v)

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => Any < Async): InputNumber = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the field's current number — unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: Double => Any < Async): InputNumber = copy(onBlurF = Present(f))

    private def interactive: Boolean = !disabledFlag.constTrue && !readonlyFlag

    private def clamp(v: Double): Double =
        val lo = minV.getOrElse(Double.NegativeInfinity)
        val hi = maxV.getOrElse(Double.PositiveInfinity)
        math.max(lo, math.min(hi, v))
    end clamp

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
        valueBinding match
            case Present(InputNumber.Value.Ref(ref)) => ref.render(v => body(Present(v), Present(ref)))
            case Present(InputNumber.Value.Const(v)) => body(Present(v), Absent)
            case Absent                              => body(Absent, Absent)

    private def commit(next: Double, ref: Maybe[SignalRef[Double]])(using Frame): Any < Async =
        val write: Any < Async = ref match
            case Present(r) => r.set(next)
            case Absent     => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end commit

    private def body(value: Maybe[Double], ref: Maybe[SignalRef[Double]])(using Frame): UI =
        // === the native number field ============================================
        var in = numberInput
            .cssClass("p-inputnumber-input")
            .cssClass("p-inputtext")
            .cssClass("p-component")
        idV.foreach(v => in = in.id(v))
        sizeV match
            case Size.Small  => in = in.cssClass("p-inputtext-sm")
            case Size.Large  => in = in.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then in = in.cssClass("p-variant-filled")
        if invalidV.constTrue then in = in.cssClass("p-invalid").aria("invalid", "true")
        minV.foreach(v => in = in.min(v))
        maxV.foreach(v => in = in.max(v))
        // Integer mode constrains the field through `step` rather than an input filter: kyo
        // deliberately keeps inputFilter/inputMask off NumberInput, because a `number` field
        // reads its value as the empty string while the content is not a valid number, so a
        // character-level filter cannot see what it is filtering. A whole-number `step` is the
        // native mechanism — the spinner and validation both round to it. (The value model
        // stays Double; a whole one.)
        in = in.step(if integerFlag then stepV.max(1.0).round.toDouble else stepV)
        value.foreach(v => in = in.value(InputNumber.format(v)))
        placeholderText match
            case Present(TextValue.Const(v)) => in = in.placeholder(v)
            case Present(TextValue.Dyn(sig)) => in = in.placeholder(sig)
            case Absent                      => ()
        end match
        if disabledFlag.constTrue then in = in.disabled(true)
        if readonlyFlag then in = in.readOnly(true)
        nameV.foreach(v => in = in.jsProp("name", v))
        accNameV match
            case Present(TextValue.Const(v)) => in = in.aria("label", v)
            case Present(TextValue.Dyn(s))   => in = in.aria("label", s)
            case Absent                      => ()
        end match
        // Typed commit: the number field delivers a Double, clamped into [min, max]
        // before the write-back (the browser only ENFORCES min/max on form submit).
        if interactive then in = in.onChangeNumeric(d => commit(clamp(d), ref))
        // onBlur carries no payload, so the current number is read from the bound ref
        // (or the resolved current value when unbound) and handed to the native blur.
        if interactive then
            onBlurF.foreach { f =>
                in = in.onBlur(ref match
                    case Present(r) => r.use(f)
                    case Absent     => f(value.getOrElse(0.0)))
            }
        end if

        // === spin buttons =======================================================
        def spin(dir: Int): Any < Async =
            val cur = value.getOrElse(clamp(0.0))
            commit(clamp(cur + dir * stepV), ref)

        def spinButton(dir: Int, cls: String, glyph: IconGlyph): UI =
            var b = button
                .cssClass("p-inputnumber-button")
                .cssClass(cls)
                .jsProp("type", "button")
                .tabIndex(-1)
                .aria("hidden", "true")
            if !interactive then b = b.disabled(true)
            else b = b.onClick(spin(dir))
            b(toChild(GlyphSvg(glyph, "p-icon")))
        end spinButton

        val inc = spinButton(+1, "p-inputnumber-increment-button", Icons.angleUp)
        val dec = spinButton(-1, "p-inputnumber-decrement-button", Icons.angleDown)

        val buttons: List[UI] =
            if !showButtonsFlag then Nil
            else
                buttonLayoutV match
                    case InputNumberButtonLayout.Stacked =>
                        List(span.cssClass("p-inputnumber-button-group")(toChild(inc), toChild(dec)))
                    case InputNumberButtonLayout.Horizontal | InputNumberButtonLayout.Vertical =>
                        List(inc, dec) // visual order via the sheet's `order` rules

        // === adornments (kyo extension — see class doc) =========================
        val pre  = prefixV.toList.map(t => span.cssClass("p-uic-inputnumber-prefix")(t): UI)
        val post = suffixV.toList.map(t => span.cssClass("p-uic-inputnumber-suffix")(t): UI)

        // === wrapper root =======================================================
        var el = span
            .cssClass("p-inputnumber")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
        if value.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if showButtonsFlag then el = el.cssClass(s"p-inputnumber-${buttonLayoutV.token}")
        if fluidFlag then el = el.cssClass("p-inputnumber-fluid")
        if invalidV.constTrue then el = el.cssClass("p-invalid")

        val root = el((pre ++ ((in: UI) :: buttons) ++ post).map(toChild)*)
        FieldInvalid.withMessage(root, invalidV.constTrue, invalidMsgV)
    end body
end InputNumber

object InputNumber:
    def apply(): InputNumber = new InputNumber()

    /** Renders a Double for the native field: integral values without the
      * trailing `.0` (matching what a user would type), fractional as-is.
      */
    private[uic] def format(v: Double): String =
        if v.isWhole && math.abs(v) < 1e15 then v.toLong.toString else v.toString

    /** Const-or-ref carrier for the pending `value` (the Input pattern, Double-typed). */
    private[uic] enum Value:
        case Const(v: Double)
        case Ref(ref: SignalRef[Double])
end InputNumber
