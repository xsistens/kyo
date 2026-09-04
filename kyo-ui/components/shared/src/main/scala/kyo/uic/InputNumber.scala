package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

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
    integerFlag: Boolean = false,
    focusAutoFlag: Boolean = false
) extends Node, NumberFormControl, HasPlaceholder, HasAccessibleName:
    type Self = InputNumber

    /** The field's value, in any of the three bindings a value slot holds. A constant renders it
      * statically. A writable `SignalRef[Double]` binds TWO-WAY: edits and spin clicks write the
      * clamped value back, and ref changes re-render the field. Any other `Signal[Double]` binds
      * one-way, so the field tracks it and edits write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Double]` does
      * not opt out of write-back; pass `ref.readOnly` for that. See [[text]] for the binding that
      * carries what was typed rather than what parses.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): InputNumber = copy(valueBinding = Present(InputNumber.Value(v)))

    /** Binds the field's raw TEXT two-way, for a caller that needs what was typed rather
      * than a number.
      *
      * A `number` field reads its value as the empty string while its content is not a
      * valid number, so `-`, `4.` and `1e` are all states a `Double` cannot represent and
      * [[value]] cannot report. They matter to anything that parses the text itself, a
      * [[DataTable]] cell draft above all: bound this way the field writes on every
      * keystroke through kyo's own two-way channel, so a commit reads what is on the screen
      * instead of what was last valid.
      *
      * It is the binding OR [[value]], not both. With a text binding the field carries
      * exactly what the reader typed: the numeric change is not normalised back into it,
      * and the spin buttons step the text they find.
      */
    def text(ref: SignalRef[String]): InputNumber = copy(valueBinding = Present(InputNumber.Value.Text(ref)))

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

    private[uic] def withPlaceholder(v: Maybe[TextValue]): InputNumber = copy(placeholderText = v)

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

    private[uic] def withInvalid(v: Maybe[BoolValue]): InputNumber                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): InputNumber                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): InputNumber = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): InputNumber = copy(accNameV = v)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): InputNumber = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Restricts entry to whole numbers: the client keystroke filter (`inputFilter("int")`)
      * rejects the decimal separator before it reaches the field, so the bound value stays
      * integral. The form layer turns this on automatically for `Int`/`Long` number fields.
      */
    def integer(v: Boolean): InputNumber = copy(integerFlag = v)

    /** Seeds focus onto this field when a re-render inserts it into the DOM for the first
      * time, kyo-ui's `focusAuto`. Same contract as `Input.focusAuto`: the element has to
      * be NEW for the seed to fire, which is what makes it the right tool for a field that
      * appears in place of something else.
      */
    def focusAuto(v: Boolean): InputNumber = copy(focusAutoFlag = v)

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
            case Present(InputNumber.Value.Dyn(sig)) => sig.render(v => body(Present(v), Absent))
            case Present(InputNumber.Value.Const(v)) => body(Present(v), Absent)
            // No `render` here on purpose: a text binding is kyo's own two-way channel on
            // the field, so the value travels without a subscription. Re-rendering the
            // field on each keystroke would rewrite what is being typed into it.
            case Present(InputNumber.Value.Text(ref)) => body(Absent, Absent, Present(ref))
            case Absent                               => body(Absent, Absent)

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

    private def body(
        value: Maybe[Double],
        ref: Maybe[SignalRef[Double]],
        text: Maybe[SignalRef[String]] = Absent
    )(using Frame): UI =
        // === the native number field ============================================
        var in = numberInput
            .cssClass("p-inputnumber-input")
            .cssClass("p-inputtext")
            .cssClass("p-component")
        idV.foreach(v => in = in.id(v))
        if focusAutoFlag then in = in.focusAuto(true)
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
        text match
            case Present(t) => in = in.value(t)
            case Absent     => value.foreach(v => in = in.value(InputNumber.format(v)))
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
        // before the write-back (the browser only ENFORCES min/max on form submit). A
        // text-bound field is left out of it: its content is what the reader typed, and
        // normalising a Double back into it would rewrite `4.` to `4` under them.
        if interactive && text.isEmpty then in = in.onChangeNumeric(d => commit(clamp(d), ref))
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
            text match
                // A text-bound field holds the current value only as text, so the step is
                // taken from what is in it, and the result is written back as text.
                case Present(t) =>
                    t.get.map { cur =>
                        val next = clamp(cur.toDoubleOption.getOrElse(0.0) + dir * stepV)
                        t.set(InputNumber.format(next)).andThen(onChangeF match
                            case Present(f) => f(next)
                            case Absent     => ())
                    }
                case Absent => commit(clamp(value.getOrElse(clamp(0.0)) + dir * stepV), ref)

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

    /** How the field's number is bound. `Const`/`Ref`/`Dyn` mirror the three cases of
      * [[ReactiveValue]]; `Text` is this control's own, and the reason the slot is a separate
      * enum rather than a `ReactiveValue[Double]`: it binds the raw string instead.
      */
    private[uic] enum Value:
        case Const(v: Double)
        case Ref(ref: SignalRef[Double])
        case Dyn(sig: Signal[Double])
        case Text(ref: SignalRef[String])
    end Value

    /** The `Const`/`Ref`/`Dyn` case matching the union a value setter takes. A writable
      * `SignalRef` binds two-way, any other signal one-way; `SignalRef` is tested first because
      * it IS a `Signal`.
      */
    private[uic] def Value(v: Double | Signal[Double]): Value = v match
        case r: SignalRef[Double] @unchecked => Value.Ref(r)
        case s: Signal[Double] @unchecked    => Value.Dyn(s)
        case d: Double                       => Value.Const(d)
end InputNumber
