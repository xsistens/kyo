package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Rating — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Rating anatomy: `div.p-rating.p-component[.p-readonly][.p-disabled]` > N
  * `div.p-rating-option[.p-rating-option-active]`, each holding a hidden native
  * radio (`span.p-hidden-accessible` > `input[type=radio]`) plus the star glyph
  * `.p-rating-icon` — filled `.p-rating-on-icon` up to the current value,
  * outline `.p-rating-off-icon` beyond it), so the extracted `@primeuix`
  * rating CSS applies verbatim.
  *
  * Clicking option i sets the value to i+1; clicking the CURRENT value clears
  * to 0 — Prime's cancel-on-same-value semantics. Prime v10's separate cancel
  * icon was retired in the PrimeOne sheet (no `.p-rating-cancel` classes ship),
  * so it is intentionally not ported. `value(SignalRef)` binds two-way: clicks
  * write back into the ref before firing the user `onChange`.
  *
  * The hidden radios are PrimeVue's anatomy: they carry the checked state,
  * the per-star `aria-label`, and native keyboard semantics; `name(...)` groups
  * them and submits the value with HTML forms. Prime derives a default name
  * from a per-instance attr selector — a pure server render has no such
  * uniqueness source, so the `name` attribute is only emitted when set.
  * `onIcon`/`offIcon` swap the star glyphs for any [[IconGlyph]].
  *
  * A [[NumberFormControl]], so `uic.Rating().bind(field)` wires value, validity,
  * message and the blur trigger in one call. The family speaks `Double`; a Rating
  * rounds on the way in and writes whole stars on the way out, which is exactly what
  * `NumberFormControl.integer` asks for and why that setter has nothing left to do
  * here.
  */
final case class Rating private (
    valueBinding: Maybe[ReactiveValue[Int]] = Absent,
    doubleRef: Maybe[SignalRef[Double]] = Absent,
    starsV: Int = 5,
    readonlyFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    nameV: Maybe[String] = Absent,
    onIconV: Maybe[IconGlyph] = Absent,
    offIconV: Maybe[IconGlyph] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Int => Any < Async] = Absent,
    onBlurDoubleF: Maybe[Double => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, NumberFormControl:
    type Self = Rating

    /** Native `id` on the option group — pair with `Label.forId`; the form layer stamps the
      * bound field's id here so focus-first-invalid can target it.
      */
    def id(v: String): Rating = copy(idV = Present(v))

    /** Sets a constant value (0 = nothing selected). */
    def value(v: Int): Rating = copy(valueBinding = Present(ReactiveValue.Const(v)))

    /** Binds two-way to `ref`: clicks write the new value back, ref changes re-render.
      *
      * `@targetName` because the `Double` overload below is the [[NumberFormControl]]
      * member and both erase to `value(SignalRef)`; the JVM name is the only thing that
      * differs, the call site still reads `value(...)`.
      */
    @targetName("valueIntRef")
    def value(ref: SignalRef[Int]): Rating = copy(valueBinding = Present(ReactiveVariable(ref)))

    /** Binds to a one-way DERIVED signal: the stars track it read-only (clicks write
      * nowhere). Prefer this over an artificial `SignalRef` when the value is computed.
      */
    def value(sig: Signal[Int]): Rating = copy(valueBinding = Present(ReactiveValue.Dyn(sig)))

    /** Binds two-way to a `Double` ref — the [[NumberFormControl]] shape, so a Rating can
      * carry a `Form.numberField`. The stars remain whole: the ref is read rounded and
      * written as a whole number, which is what [[integer]] asks of this family and what a
      * star count is anyway.
      */
    def value(ref: SignalRef[Double]): Rating = copy(doubleRef = Present(ref))

    /** Number of stars (Prime default 5). */
    def stars(n: Int): Rating = copy(starsV = math.max(1, n))

    /** `readonly` — the stars keep their look but stop reacting (`.p-readonly`). */
    def readonly(v: Boolean): Rating = copy(readonlyFlag = v)

    /** Disables the control (`.p-disabled` dimming + no interaction); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Rating = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `name` on the hidden per-option radios — groups them and makes the
      * value participate in HTML form submission.
      */
    def name(v: String): Rating = copy(nameV = Present(v))

    /** Glyph for the ACTIVE options (Prime default: the filled star). */
    def onIcon(glyph: IconGlyph): Rating = copy(onIconV = Present(glyph))

    /** Glyph for the options beyond the value (Prime default: the outline star). */
    def offIcon(glyph: IconGlyph): Rating = copy(offIconV = Present(glyph))

    /** Accessible name → `aria-label` on the root. */
    def accessibleName(v: String): Rating = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Rating = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Marks the rating invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Rating = copy(invalidV = Present(BoolValue.Const(v)))

    /** Reactive validity: the bound signal toggles the invalid state on emission. */
    def invalid(sig: Signal[Boolean]): Rating = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Message rendered below the stars while the rating is invalid (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): Rating = copy(invalidMsgV = Present(v))

    /** Reactive invalid message — `Present` shows the row and (by default) marks the
      * rating invalid; `Absent` clears both.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Rating = copy(invalidMsgDynV = Present(sig))

    /** Accepted for the [[NumberFormControl]] contract and satisfied by construction: a
      * star count is always whole, so the constraint has nothing left to enforce here.
      * Returns this unchanged rather than storing a flag that could never be false in
      * effect.
      */
    def integer(v: Boolean): Rating = this

    /** Fired with the NEW value (0 on cancel) after the ref write-back. */
    def onChange(f: Int => Any < Async): Rating = copy(onChangeF = Present(f))

    /** Fires on focus loss with the current star count — the validation layer's Blur
      * trigger, in the `Double` shape the number family speaks.
      */
    def onBlur(f: Double => Any < Async): Rating = copy(onBlurDoubleF = Present(f))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    private[uic] def render(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderDisabledResolved
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderDisabledResolved
                )

    private def renderDisabledResolved(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        doubleRef match
            // The Double binding wins when present: it is the form layer's, and a control
            // bound to a field has no second source of truth.
            case Present(dref) => dref.render(d => body(math.rint(d).toInt, Absent))
            case Absent =>
                valueBinding match
                    case Present(ReactiveVariable(ref))  => ref.render(v => body(v, Present(ref)))
                    case Present(ReactiveValue.Dyn(sig)) => sig.render(v => body(v, Absent))
                    case Present(ReactiveValue.Const(v)) => body(v, Absent)
                    case Absent                          => body(0, Absent)

    private def body(value: Int, ref: Maybe[SignalRef[Int]])(using Frame): UI =
        val options: List[UI] = (1 to starsV).toList.map { i =>
            val active = i <= value
            var opt    = div.cssClass("p-rating-option")
            if active then opt = opt.cssClass("p-rating-option-active")
            if interactive then opt = opt.onClick(activate(i, value, ref))

            // PrimeVue's hidden radio: real checked state, per-star aria-label, and
            // native keyboard semantics (arrows move within the name group, Space
            // selects — selecting the current value clears, Prime's cancel path).
            var hidden = radio
                .checked(i == value)
                .jsProp("value", i.toString)
                .aria("label", if i == 1 then "1 star" else s"$i stars")
            nameV.foreach(n => hidden = hidden.name(n))
            if !interactive then hidden = hidden.disabled(true)
            if readonlyFlag then hidden = hidden.aria("readonly", "true")
            if interactive then hidden = hidden.onChange(_ => activate(i, value, ref))
            val hiddenSlot: UI = span.cssClass("p-hidden-accessible")(toChild(hidden: UI))

            val icon: UI =
                if active then GlyphSvg(onIconV.getOrElse(Icons.starFill), "p-rating-icon", "p-rating-on-icon")
                else GlyphSvg(offIconV.getOrElse(Icons.star), "p-rating-icon", "p-rating-off-icon")
            opt(toChild(hiddenSlot), toChild(icon))
        }

        var el = div.cssClass("p-rating").cssClass("p-component")
        idV.foreach(v => el = el.id(v))
        if readonlyFlag then el = el.cssClass("p-readonly")
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        onBlurDoubleF.foreach { f =>
            el = el.onBlur(doubleRef match
                case Present(r) => r.use(d => f(math.rint(d)))
                case Absent     => f(value.toDouble))
        }
        FieldInvalid.withMessage(el(options.map(toChild)*), invalidV.constTrue, invalidMsgV)
    end body

    /** Prime's select semantics: picking the current value clears to 0. */
    private def activate(i: Int, current: Int, ref: Maybe[SignalRef[Int]])(using Frame): Any < Async =
        val next = if i == current then 0 else i
        val write: Any < Async = (ref, doubleRef) match
            case (Present(r), _)       => r.set(next)
            case (Absent, Present(dr)) => dr.set(next.toDouble)
            case _                     => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end Rating

object Rating:
    def apply(): Rating = new Rating()
