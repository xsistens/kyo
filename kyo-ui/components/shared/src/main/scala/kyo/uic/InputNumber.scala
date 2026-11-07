package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** InputNumber button layout (Prime's `buttonLayout`): `Stacked` (default:
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

/** InputNumber: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputNumber anatomy: `span.p-inputnumber.p-component.p-inputwrapper` >
  * `input.p-inputnumber-input.p-inputtext` plus, with `showButtons(true)`, the
  * spin buttons `.p-inputnumber-button.p-inputnumber-increment-button` /
  * `-decrement-button`, stacked inside a `.p-inputnumber-button-group`,
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
  * number field instead: `prefix`/`suffix` render as STATIC adornment spans
  * (`.p-uic-inputnumber-prefix`/`-suffix`, kyo extension) beside the field,
  * never inside the typed value, and no live digit-grouping mask is applied.
  */
final class InputNumber private[uic] (private[uic] val p: InputNumber.Props) extends Node, NumberFormControl, HasPlaceholder,
      HasAccessibleName, HasProps:
    import p.*

    type Self = InputNumber

    /** The field's value, in any of the three bindings a value slot holds. A constant nobody hears is
      * fixed, drawn read-only; one [[onChange]] hears is reported and the field painted back to the
      * constant. A writable `SignalRef[Double]` binds TWO-WAY: edits and spin clicks write the
      * clamped value back, and ref changes re-render the field. Any other `Signal[Double]` binds
      * one-way, so the field tracks it and edits write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Double]` does
      * not opt out of write-back; pass `ref.readOnly` for that. See [[text]] for the binding that
      * carries what was typed rather than what parses.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): InputNumber =
        new InputNumber(p.copy(numberSlot = numberSlot.bind(v), textSlot = textSlot.unbound))

    /** Binds the field's raw TEXT, for a caller that needs what was typed rather than a number. A `SignalRef` binds two
      * way and [[onTextChange]] hears every keystroke; a plain `Signal` binds one way, the field showing it and painted back
      * after each report; a constant is the starting text the field keeps. A signal nothing hears is reported in a card.
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
    def text(v: String | Signal[String]): InputNumber =
        new InputNumber(p.copy(textSlot = textSlot.bind(v), numberSlot = numberSlot.unbound))

    /** Fired with the raw text after every keystroke and spin of a [[text]]-bound field, after a two-way text is written. */
    def onTextChange(f: String => UI.Action)(using Frame): InputNumber = new InputNumber(p.copy(textSlot = textSlot.hear(f)))

    /** Lower bound: native `min` plus the clamp floor for spins and commits. */
    def min(v: Double): InputNumber = new InputNumber(p.copy(minV = Present(v)))

    /** Upper bound: native `max` plus the clamp ceiling for spins and commits. */
    def max(v: Double): InputNumber = new InputNumber(p.copy(maxV = Present(v)))

    /** Spin/arrow increment (native `step`; Prime default 1). */
    def step(v: Double): InputNumber = new InputNumber(p.copy(stepV = if v <= 0 then 1.0 else v))

    /** Shows the increment/decrement buttons (Prime's `showButtons`). */
    def showButtons(v: Boolean): InputNumber = new InputNumber(p.copy(showButtonsFlag = v))

    /** Button placement: `Stacked` (default), `Horizontal` or `Vertical`. */
    def buttonLayout(v: InputNumberButtonLayout): InputNumber = new InputNumber(p.copy(buttonLayoutV = v))

    /** Static adornment BEFORE the field (kyo extension `.p-uic-inputnumber-prefix`;
      * see the class doc: Prime's in-value formatting is deliberately not ported).
      */
    def prefix(v: String): InputNumber = new InputNumber(p.copy(prefixV = Present(v)))

    /** Static adornment AFTER the field (`.p-uic-inputnumber-suffix`). */
    def suffix(v: String): InputNumber = new InputNumber(p.copy(suffixV = Present(v)))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): InputNumber = new InputNumber(p.copy(placeholderText = v))

    /** Disables the field + spin buttons; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): InputNumber = new InputNumber(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** Native `readonly`: focusable but not editable; the spin buttons stop too. */
    def readonly(v: Boolean): InputNumber = new InputNumber(p.copy(readonlyFlag = v))

    /** Native `name` for HTML form participation. */
    def name(v: String): InputNumber = new InputNumber(p.copy(nameV = Present(v)))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field. */
    def size(v: Size): InputNumber = new InputNumber(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is default. */
    def variant(v: FieldVariant): InputNumber = new InputNumber(p.copy(variantV = v))

    /** Spans the full container width (`.p-inputnumber-fluid`). */
    def fluid(v: Boolean): InputNumber = new InputNumber(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new InputNumber(p.copy(validityV = v))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new InputNumber(p.copy(accessV = v))

    /** Restricts entry to whole numbers: the client keystroke filter (`inputFilter("int")`)
      * rejects the decimal separator before it reaches the field, so the bound value stays
      * integral. The form layer turns this on automatically for `Int`/`Long` number fields.
      */
    def integer(v: Boolean): InputNumber = new InputNumber(p.copy(integerFlag = v))

    /** Seeds focus onto this field when a re-render inserts it into the DOM for the first
      * time, kyo-ui's `focusAuto`. Same contract as `Input.focusAuto`: the element has to
      * be NEW for the seed to fire, which is what makes it the right tool for a field that
      * appears in place of something else.
      */
    def focusAuto(v: Boolean): InputNumber = new InputNumber(p.copy(focusAutoFlag = v))

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => UI.Action)(using Frame): InputNumber = new InputNumber(p.copy(numberSlot = numberSlot.hear(f)))

    /** Fires on focus loss (native `blur`) with the field's current number: bound to
      * a ref, the number the ref holds (committed, clamped); otherwise the number in
      * the text the field holds as focus leaves it, `0` for a text that is no number,
      * clamped into the bounds where the field is bound to a number (a constant or a
      * one-way signal) and as typed where it keeps text (unbound, or bound to a text).
      * Unlike `onChange` it fires even when the value was not edited. The
      * form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: Double => UI.Action)(using Frame): InputNumber = new InputNumber(p.copy(onBlurF = Present(Callback.of(f))))

    private def interactive: Boolean = !disabledFlag.constTrue && !readonlyFlag

    private def clamp(v: Double): Double =
        val lo = minV.getOrElse(Double.NegativeInfinity)
        val hi = maxV.getOrElse(Double.PositiveInfinity)
        math.max(lo, math.min(hi, v))
    end clamp

    private[uic] def render(using Frame): UI =
        // A constant nobody hears is fixed: drawn read-only. Unbound, the field keeps what the reader types and spins itself,
        // through the same text channel a text binding uses, so it starts empty; a constant text is where it starts.
        if numberSlot.fixed && !readonlyFlag then new InputNumber(p.copy(readonlyFlag = true)).render
        else if numberSlot.binding.isDefined then renderLive
        else Slot.owningFrom(textSlot, unbound = "")(s => new InputNumber(this.p.copy(textSlot = s)).renderLive)

    private def renderLive(using Frame): UI =
        val validity = FieldValidity(validityV)
        val field    =
            validity.follow(BoolValue.reactive(disabledFlag)(d => new InputNumber(p.copy(disabledFlag = d)).renderResolved(validity)))
        // A value bound one way that nothing hears: every change the reader makes is put back.
        val quiet = readonlyFlag || disabledFlag.constTrue
        Diagnostics.after(field)("InputNumber")(
            Diagnostic.unheard(numberSlot, InputNumber.valueName, on = !quiet),
            Diagnostic.unheard(textSlot, InputNumber.textName, on = !quiet)
        )
    end renderLive

    private def renderResolved(validity: FieldValidity)(using Frame): UI =
        if numberSlot.binding.isDefined then
            // Bound one way, the native field follows the signal, so kyo-ui puts back a number the owner does not take. A constant
            // somebody hears is the same, and a constant nobody hears is fixed (drawn read-only).
            Slot.drawField(numberSlot, unbound = 0.0)(f =>
                body(
                    Present(f.value),
                    validity,
                    native = Slot.mapNative[Double, String](f.native)(InputNumber.format) match
                        case s: Signal[String] @unchecked => Present(s)
                        case _                            => Absent
                )
            )
        else
            textSlot.binding match
                // No `render` here on purpose: a text binding is kyo's own two-way channel on the field, so the value travels
                // without a subscription. Re-rendering the field on each keystroke would rewrite what is being typed into it.
                case Present(ReactiveVariable(ref)) => body(Absent, validity, Present(ref))
                // Text bound one way: the native field follows it, so kyo-ui puts back text the owner does not take.
                case Present(ReactiveValue.Dyn(sig)) =>
                    sig.render(t => body(Absent, validity, textShown = Present(t), native = Present(sig)))
                // The static projection of a starting text: the mount takes it over as the field's own.
                case Present(ReactiveValue.Const(t)) => body(Absent, validity, textShown = Present(t))
                case _                               => body(Absent, validity)
    end renderResolved

    private def body(
        value: Maybe[Double],
        validity: FieldValidity,
        text: Maybe[SignalRef[String]] = Absent,
        // Text shown but not written: a one-way text binding, or a starting text before the mount takes it over.
        textShown: Maybe[String] = Absent,
        // What the native field follows, where the binding is one way.
        native: Maybe[Signal[String]] = Absent
    )(using Frame): UI =
        // === the native number field ============================================
        // The wrapper's `p-inputwrapper-filled` follows the text the number field holds, toggled by kyo-ui's client, so a
        // field the reader empties (from a constant, or keeping text) does not keep a class no render takes off.
        var in = numberInput
            .cssClass("p-inputnumber-input")
            .cssClass("p-inputtext")
            .cssClass("p-component")
            .filledClass("p-inputwrapper-filled", ".p-inputnumber")
        accessV.idV.foreach(v => in = in.id(v))
        if focusAutoFlag then in = in.focusAuto(true)
        sizeV match
            case Size.Small  => in = in.cssClass("p-inputtext-sm")
            case Size.Large  => in = in.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then in = in.cssClass("p-variant-filled")
        in = validity.mark(in)
        minV.foreach(v => in = in.min(v))
        maxV.foreach(v => in = in.max(v))
        // Integer mode constrains the field through `step` rather than an input filter: kyo
        // deliberately keeps inputFilter/inputMask off NumberInput, because a `number` field
        // reads its value as the empty string while the content is not a valid number, so a
        // character-level filter cannot see what it is filtering. A whole-number `step` is the
        // native mechanism: the spinner and validation both round to it. (The value model
        // stays Double; a whole one.)
        in = in.step(if integerFlag then stepV.max(1.0).round.toDouble else stepV)
        (native, text, textShown) match
            case (Present(n), _, _) => in = in.value(n)
            case (_, Present(t), _) => in = in.value(t)
            case (_, _, Present(t)) => in = in.value(t)
            case _                  => value.foreach(v => in = in.value(InputNumber.format(v)))
        end match
        // A text binding hears every keystroke: a ref is written by the field itself, so only the report is left; text
        // shown one way is reported, and kyo-ui puts the field back to what its owner holds.
        def reportText(t: String): UI.Action = textSlot.hearer.fold[UI.Action](())(f => f(t))
        if interactive && text.isDefined then in = in.onInput(reportText)
        if interactive && textShown.isDefined then in = in.onInput(reportText)
        placeholderText match
            case Present(TextValue.Const(v)) => in = in.placeholder(v)
            case Present(TextValue.Dyn(sig)) => in = in.placeholder(sig)
            case Absent                      => ()
        end match
        if disabledFlag.constTrue then in = in.disabled(true)
        if readonlyFlag then in = in.readOnly(true)
        nameV.foreach(v => in = in.jsProp("name", v))
        accessV.nameV match
            case Present(TextValue.Const(v)) => in = in.aria("label", v)
            case Present(TextValue.Dyn(s))   => in = in.aria("label", s)
            case Absent                      => ()
        end match
        // Typed commit: the number field delivers a Double, clamped into [min, max]
        // before the write-back (the browser only ENFORCES min/max on form submit). A
        // text-bound field is left out of it: its content is what the reader typed, and
        // normalising a Double back into it would rewrite `4.` to `4` under them.
        if interactive && text.isEmpty && textShown.isEmpty then in = in.onChangeNumeric(d => numberSlot.change(clamp(d)))
        // A field bound to a ref holds the number it committed, clamped, so the blur reports the ref as focus leaves.
        // Anywhere else only the field holds what the reader typed (a commit with no ref writes nowhere, and a one-way
        // owner's repaint can land after the blur), so the blur reports the text the field carries, read the way a spin
        // reads a text (no number is 0). Bound to a number it is clamped as a commit is; a text binding keeps the text.
        if interactive then
            onBlurF.foreach { f =>
                in = numberSlot.writable match
                    case Present(r)           => in.onBlur(r.use(f))
                    case _ if value.isDefined => in.onBlur((_, t) => f(clamp(t.toDoubleOption.getOrElse(0.0))))
                    case _                    => in.onBlur((_, t) => f(t.toDoubleOption.getOrElse(0.0)))
            }
        end if

        // === spin buttons =======================================================
        def spin(dir: Int): UI.Action =
            text match
                // A text-bound field holds the current value only as text, so the step is
                // taken from what is in it, and the result is written back as text.
                case Present(t) =>
                    t.get.map { cur =>
                        val next = clamp(cur.toDoubleOption.getOrElse(0.0) + dir * stepV)
                        textSlot.change(InputNumber.format(next)).andThen(numberSlot.hearer.fold[UI.Action](())(f => f(next)))
                    }
                case Absent =>
                    textShown match
                        // Shown one way: the step is taken from the owner's text and only reported.
                        case Present(cur) =>
                            val next = clamp(cur.toDoubleOption.getOrElse(0.0) + dir * stepV)
                            reportText(InputNumber.format(next)).andThen(numberSlot.hearer.fold[UI.Action](())(f => f(next)))
                        case Absent => numberSlot.change(clamp(value.getOrElse(clamp(0.0)) + dir * stepV))

        def spinButton(dir: Int, cls: String, glyph: IconGlyph): UI =
            var b = button
                .cssClass("p-inputnumber-button")
                .cssClass(cls)
                .jsProp("type", "button")
                .tabIndex(-1)
                .aria("hidden", "true")
            if !interactive then b = b.disabled(true)
            else b = b.onClick(spin(dir))
            b(GlyphSvg(glyph, "p-icon"))
        end spinButton

        val inc = spinButton(+1, "p-inputnumber-increment-button", Icons.angleUp)
        val dec = spinButton(-1, "p-inputnumber-decrement-button", Icons.angleDown)

        val buttons: List[UI] =
            if !showButtonsFlag then Nil
            else
                buttonLayoutV match
                    case InputNumberButtonLayout.Stacked =>
                        List(span.cssClass("p-inputnumber-button-group")(inc, dec))
                    case InputNumberButtonLayout.Horizontal | InputNumberButtonLayout.Vertical =>
                        List(inc, dec) // visual order via the sheet's `order` rules

        // === adornments (kyo extension, see class doc) ==========================
        val pre  = prefixV.toList.map(t => span.cssClass("p-uic-inputnumber-prefix")(t): UI)
        val post = suffixV.toList.map(t => span.cssClass("p-uic-inputnumber-suffix")(t): UI)

        // === wrapper root =======================================================
        var el = span
            .cssClass("p-inputnumber")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
        // The wrapper's starting class: from the number, or from the text the field keeps (a text ref through a class
        // channel, so a write by the program reaches it too). The client keeps it right from the field's own text after.
        el = text match
            case Present(t) => el.cssClass("p-inputwrapper-filled", t.map(_.nonEmpty))
            case Absent     => el.cssClass("p-inputwrapper-filled", value.nonEmpty || textShown.exists(_.nonEmpty))
        if showButtonsFlag then el = el.cssClass(s"p-inputnumber-${buttonLayoutV.token}")
        if fluidFlag then el = el.cssClass("p-inputnumber-fluid")
        el = validity.markClass(el)

        el(UI.children(pre ++ ((in: UI) :: buttons) ++ post)*)
    end body
end InputNumber

object InputNumber:
    final private[uic] case class Props(
        numberSlot: Slot[Double] = Slot(),
        textSlot: Slot[String] = Slot(),
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
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        accessV: Accessibility = Accessibility.none,
        onBlurF: Maybe[Double => UI.Action] = Absent,
        integerFlag: Boolean = false,
        focusAutoFlag: Boolean = false
    )

    def apply(): InputNumber = new InputNumber(Props())

    /** Renders a Double for the native field: integral values without the
      * trailing `.0` (matching what a user would type), fractional as-is.
      */
    private[uic] def format(v: Double): String =
        if v.isWhole && math.abs(v) < 1e15 then v.toLong.toString else v.toString

    private val valueName = Slot.Name("InputNumber", "value", "a change", "onChange")
    private val textName  = Slot.Name("InputNumber", "text", "a change", "onTextChange")
end InputNumber
