package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Slider — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Slider anatomy: `div.p-slider.p-component.p-slider-horizontal|-vertical` >
  * `span.p-slider-range` (the value fill) + `span.p-slider-handle`), so the
  * extracted `@primeuix` slider CSS applies verbatim.
  *
  * Interaction is a REAL `<input type=range>` (kyo's `rangeInput`) stretched
  * invisibly over the track (`.p-uic-slider-native`, kyo extension): dragging
  * and the native keyboard (arrows/Home/End) work for free, and the browser
  * delivers the committed value as a Double which is written back into the
  * bound `value(SignalRef[Double])` before `onChange` fires. The Prime divs
  * are the VISUAL layer only — the range fill and handle position are computed
  * server-side from the bound value ((value − min) / (max − min)), so a ref
  * write from anywhere moves the slider.
  *
  * A [[NumberFormControl]], so `uic.Slider().bind(field)` wires value, validity,
  * message, the blur trigger and (for a whole-valued field) the [[integer]]
  * constraint in one call.
  *
  * Honest limits under server rendering: the native `change` event fires when
  * a drag COMMITS (pointer release) — the visual fill follows a drag on
  * release, not continuously (keyboard steps commit per keypress and track
  * live). Prime's dual-handle `range` mode is not ported: it would need two
  * overlapping native inputs fighting for the same pointer surface — deferred
  * rather than approximated.
  */
final case class Slider private (
    valueBinding: Maybe[ReactiveValue[Double]] = Absent,
    minV: Double = 0.0,
    maxV: Double = 100.0,
    stepV: Double = 1.0,
    integerFlag: Boolean = false,
    orientationV: Orientation = Orientation.Horizontal,
    disabledFlag: Maybe[BoolValue] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Double => Any < Async] = Absent,
    onBlurF: Maybe[Double => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, NumberFormControl, HasAccessibleName:
    type Self = Slider

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Slider = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** The slider's value, in any of the three bindings a value slot holds. A constant renders the
      * fill/handle statically. A writable `SignalRef[Double]` binds TWO-WAY: drags and keys write the
      * committed value back, and ref changes move the fill and handle. Any other `Signal[Double]` binds
      * one-way, so the fill and handle track it and drags write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Double]` does not
      * opt out of write-back; pass `ref.readOnly` for that. A derived signal is already one-way and is
      * the better answer when the value is computed rather than user-edited.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): Slider = copy(valueBinding = Present(ReactiveValue(v)))

    /** Lower bound (Prime default 0). */
    def min(v: Double): Slider = copy(minV = v)

    /** Upper bound (Prime default 100). */
    def max(v: Double): Slider = copy(maxV = v)

    /** Drag/keyboard increment (Prime default 1). */
    def step(v: Double): Slider = copy(stepV = if v <= 0 then 1.0 else v)

    /** Constrains committed values to whole numbers: the value written back is rounded,
      * and the native input steps in whole units. The form layer flips this on for a
      * whole-valued (`Int`/`Long`) [[kyo.uic.form.NumberField]], so a slider bound to one
      * cannot hand it a fraction.
      */
    def integer(v: Boolean): Slider = copy(integerFlag = v)

    /** Track orientation — `.p-slider-horizontal` (default) or `.p-slider-vertical`. */
    def orientation(v: Orientation): Slider = copy(orientationV = v)

    /** Disables the control (`.p-disabled` + native disabled input); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Slider = copy(disabledFlag = Present(ReactiveValue(v)))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Slider = copy(accNameV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): Slider                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): Slider                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Slider = copy(invalidMsgDynV = v)

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => Any < Async): Slider = copy(onChangeF = Present(f))

    /** Fires on focus loss with the slider's current value — unlike onChange it fires even
      * when the handle was not moved. The validation layer's Blur trigger.
      */
    def onBlur(f: Double => Any < Async): Slider = copy(onBlurF = Present(f))

    /** Clamps into range, and rounds to a whole number under [[integer]]. */
    private def clamp(v: Double): Double =
        val bounded = math.max(minV, math.min(maxV, v))
        if integerFlag then math.rint(bounded) else bounded

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
        valueBinding match
            case Present(ReactiveVariable(ref))  => ref.render(v => body(v, Present(ref)))
            case Present(ReactiveValue.Dyn(sig)) => sig.render(v => body(v, Absent))
            case Present(ReactiveValue.Const(v)) => body(v, Absent)
            case Absent                          => body(minV, Absent)

    private def body(raw: Double, ref: Maybe[SignalRef[Double]])(using Frame): UI =
        val value = clamp(raw)
        val pct =
            if maxV <= minV then 0.0
            else (value - minV) / (maxV - minV) * 100.0
        val horizontal = orientationV == Orientation.Horizontal

        // === visual layer (Prime's divs, positions computed server-side) ========
        val range =
            if horizontal then span.cssClass("p-slider-range").style(_.width(pct.pct))
            else span.cssClass("p-slider-range").style(_.height(pct.pct))
        val handle =
            val base = span.cssClass("p-slider-handle").aria("hidden", "true")
            if horizontal then base.style(_.left(pct.pct)) else base.style(_.bottom(pct.pct))

        // === interaction layer: the invisible native range input ================
        var in = rangeInput
            .cssClass("p-uic-slider-native")
            .min(minV)
            .max(maxV)
            .step(if integerFlag then math.max(1.0, math.rint(stepV)) else stepV)
            .value(value)
        idV.foreach(v => in = in.id(v))
        accNameV match
            case Present(TextValue.Const(v)) => in = in.aria("label", v)
            case Present(TextValue.Dyn(s))   => in = in.aria("label", s)
            case Absent                      => ()
        end match
        if disabledFlag.constTrue then in = in.disabled(true)
        else
            in = in.onChange { d =>
                val next = clamp(d)
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
            }
        end if
        // Blur fires even without a drag — the validation layer's Blur trigger. Reads the
        // ref LIVE where there is one, so it reports the value at blur time.
        onBlurF.foreach { f =>
            in = in.onBlur(ref match
                case Present(r) => r.use(v => f(clamp(v)))
                case Absent     => f(value))
        }

        var el = div
            .cssClass("p-slider")
            .cssClass("p-component")
            .cssClass(if horizontal then "p-slider-horizontal" else "p-slider-vertical")
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        FieldInvalid.withMessage(
            el(toChild(range), toChild(handle), toChild(in: UI)),
            invalidV.constTrue,
            invalidMsgV
        )
    end body
end Slider

object Slider:
    def apply(): Slider = new Slider()
