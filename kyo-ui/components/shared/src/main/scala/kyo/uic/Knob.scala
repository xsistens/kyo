package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Knob: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Knob
  * anatomy: `div.p-knob.p-component` > `svg[viewBox=0 0 100 100]` with the
  * `.p-knob-range` background arc, the `.p-knob-value` value arc and the
  * `.p-knob-text` center label), so the extracted `@primeuix` knob CSS applies
  * verbatim. The arc paths use Prime's own dial math (radius 40 around
  * (50, 50), 240° sweep from 4π/3 to −π/3) computed server-side from the bound
  * value: pure geometry, no client script.
  *
  * Interaction is keyboard AND pointer-drag. Keyboard (Prime's slider keys on
  * the focusable svg): ArrowUp/ArrowRight step up, ArrowDown/ArrowLeft step
  * down, PageUp/PageDown jump ±10 steps, Home/End go to min/max. Pointer
  * drag: pressing/dragging the dial maps the pointer angle
  * (`atan2` around the dial center, in Prime's 240°-gap geometry) to the value,
  * snapped to `step`; the bottom dead-zone clamps to the nearest end. Each
  * writes the clamped value into the bound `value(SignalRef[Double])` before
  * `onChange` fires. The drag handler sits on the stable outer div while the svg
  * is pointer-events:none, so per-frame re-render never detaches the
  * capture.
  *
  * A [[NumberFormControl]], so `uic.Knob().bind(field)` wires value, validity,
  * message, the blur trigger and (for a whole-valued field) the [[integer]]
  * constraint in one call.
  *
  * Colors default to the knob tokens (`--p-knob-value-background`,
  * `--p-knob-range-background`, `--p-knob-text-color`) and follow the theme;
  * `valueColor`/`rangeColor`/`textColor` accept any CSS color or `var()`.
  */
final case class Knob private (
    valueBinding: Maybe[ReactiveValue[Double]] = Absent,
    minV: Double = 0.0,
    maxV: Double = 100.0,
    stepV: Double = 1.0,
    sizeV: Int = 100,
    strokeWidthV: Double = 14.0,
    showValueFlag: Boolean = true,
    valueTemplateV: Maybe[Double => String] = Absent,
    valueColorV: Maybe[String] = Absent,
    rangeColorV: Maybe[String] = Absent,
    textColorV: Maybe[String] = Absent,
    readonlyFlag: Boolean = false,
    integerFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Double => UI.Action] = Absent,
    onBlurF: Maybe[Double => UI.Action] = Absent,
    idV: Maybe[String] = Absent
) extends Node, NumberFormControl, HasAccessibleName:
    type Self = Knob

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Knob = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** The knob's value, in any of the three bindings a value slot holds. A writable `SignalRef[Double]`
      * binds TWO-WAY: drags and keyboard steps write the clamped value back, and ref changes redraw the
      * arc. Any other `Signal[Double]`, or a constant, binds ONE-WAY: the arc shows it, and a drag or a
      * step writes nothing and only reports through [[onChange]]; a signal nobody hears gets a card.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Double]`
      * does not opt out of write-back; pass `ref.readOnly` for that. A derived signal is already
      * one-way and is the better answer when the value is computed rather than user-edited.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): Knob = copy(valueBinding = Present(ReactiveValue(v)))

    /** Lower bound (Prime default 0). */
    def min(v: Double): Knob = copy(minV = v)

    /** Upper bound (Prime default 100). */
    def max(v: Double): Knob = copy(maxV = v)

    /** Keyboard increment (Prime default 1). */
    def step(v: Double): Knob = copy(stepV = if v <= 0 then 1.0 else v)

    /** Constrains committed values to whole numbers: every keyboard step and drag lands on
      * a rounded value. The form layer flips this on for a whole-valued (`Int`/`Long`)
      * [[kyo.uic.form.NumberField]], so a knob bound to one cannot hand it a fraction.
      */
    def integer(v: Boolean): Knob = copy(integerFlag = v)

    /** Rendered size in px (width = height; Prime default 100). */
    def size(px: Int): Knob = copy(sizeV = math.max(20, px))

    /** Arc stroke width in viewBox units (Prime default 14). */
    def strokeWidth(v: Double): Knob = copy(strokeWidthV = math.max(1.0, v))

    /** Shows the center value label (Prime default true). */
    def showValue(v: Boolean): Knob = copy(showValueFlag = v)

    /** Formats the center label from the current value. */
    def valueTemplate(f: Double => String): Knob = copy(valueTemplateV = Present(f))

    /** Value-arc color: any CSS color or `var()` (default the knob value token). */
    def valueColor(v: String): Knob = copy(valueColorV = Present(v))

    /** Background-arc color (default the knob range token). */
    def rangeColor(v: String): Knob = copy(rangeColorV = Present(v))

    /** Center-label color (default the knob text token). */
    def textColor(v: String): Knob = copy(textColorV = Present(v))

    /** Keeps the look but stops reacting (no keyboard writes). */
    def readonly(v: Boolean): Knob = copy(readonlyFlag = v)

    /** Disables the control (`.p-disabled`, unfocusable); a `Signal[Boolean]` toggles it
      * reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Knob = copy(disabledFlag = Present(ReactiveValue(v)))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Knob = copy(accNameV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): Knob                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): Knob                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Knob = copy(invalidMsgDynV = v)

    /** Fired with the NEW (clamped) value after the ref write-back, or with the value asked for where the knob is bound one way. */
    def onChange(f: Double => UI.Action)(using Frame): Knob = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss with the knob's current value; unlike onChange it fires even
      * when the dial was not moved. The validation layer's Blur trigger.
      */
    def onBlur(f: Double => UI.Action)(using Frame): Knob = copy(onBlurF = Present(Callback.of(f)))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    /** Clamps into range, and rounds to a whole number under [[integer]]. */
    private def clamp(v: Double): Double =
        val bounded = math.max(minV, math.min(maxV, v))
        if integerFlag then math.rint(bounded) else bounded

    private[uic] def render(using Frame): UI =
        // A constant nobody hears is fixed: drawn read-only. Unbound, the knob keeps where the reader turns it itself.
        if ReactiveValue.fixed(valueBinding, onChangeF) && !readonlyFlag then copy(readonlyFlag = true).render
        else if valueBinding.isEmpty then ReactiveValue.owned(minV)(own => copy(valueBinding = Present(own)).renderLive)(renderLive)
        else renderLive

    private def renderLive(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val field    = validity.follow(BoolValue.reactive(disabledFlag)(d => copy(disabledFlag = d).renderResolved(validity)))
        // A signal nobody hears: every change the reader makes is painted away again.
        if ReactiveValue.deaf(valueBinding, onChangeF) && !readonlyFlag && !disabledFlag.constTrue
        then fragment(field, Diagnostics.unheard("Knob", "value", "onChange"))
        else field
    end renderLive

    private def renderResolved(validity: FieldValidity)(using Frame): UI =
        valueBinding match
            case Present(ReactiveValue.Dyn(sig)) => turnable(sig, validity)
            case Present(ReactiveValue.Const(v)) => turnable(Signal.initConst(v), validity)
            case Absent                          => staticRoot(minV, validity)
        end match
    end renderResolved

    /** Pointer-drag + keyboard over whatever the value is bound to. The drag handler sits on the STABLE outer div, never
      * re-rendered, while the reactive svg dial inside is pointer-events:none, so the pointer always lands on the div whose
      * rect is the dial's coordinate space. A drag maps the pointer to an angle and so to a value on its own, regardless
      * of the render-time one; [[commit]] writes it where the binding is two-way and reports it either way.
      */
    private def turnable(value: Signal[Double], validity: FieldValidity)(using Frame): UI =
        val reactiveSvg: UI = value.render(v => dialSvg(clamp(v), Present(value)))
        var root            = shellRoot(validity)
        if interactive then
            val onDrag = (e: PointerEvent) => commit(clamp(Knob.valueFromPointer(e.x, e.y, e.rectW, e.rectH, minV, maxV, stepV)))
            root = root.onPointerDown(onDrag).onPointerMove(onDrag)
        end if
        root(reactiveSvg)
    end turnable

    /** The root box shared by the interactive and static paths. */
    private def shellRoot(validity: FieldValidity)(using Frame): Ast.Div =
        var root = div.cssClass("p-knob").cssClass("p-component")
        if disabledFlag.constTrue then root = root.cssClass("p-disabled")
        validity.mark(root)
    end shellRoot

    private def staticRoot(value: Double, validity: FieldValidity)(using Frame): UI =
        shellRoot(validity)(dialSvg(value, Absent))

    /** A value the reader asked for: written where the binding is two-way, then reported to [[onChange]]. */
    private def commit(next: Double)(using Frame): UI.Action =
        ReactiveValue.change(valueBinding, next, onChangeF)

    /** The reactive svg dial (arcs + label + role=slider + keyboard). It carries
      * `p-uic-knob-dial` (pointer-events:none) so the drag pointer lands on the
      * stable outer div, and stays keyboard-focusable (Tab) with the full
      * arrow/Page/Home/End key set.
      */
    private def dialSvg(value: Double, live: Maybe[Signal[Double]])(using Frame): UI =
        val paint = (spec: Maybe[String], token: String) =>
            spec.flatMap(Style.Color.parse).getOrElse(Style.Color.variable(token))

        val rangeArc = Svg.path
            .cssClass("p-knob-range")
            .d(Svg.PathData.raw(Knob.rangePath))
            .stroke(paint(rangeColorV, "p-knob-range-background"))
            .strokeWidth(Svg.SvgLength.User(strokeWidthV))
            .strokeLinecap(Svg.StrokeLinecap.Round)
        val valueArc = Svg.path
            .cssClass("p-knob-value")
            .d(Svg.PathData.raw(Knob.valuePath(value, minV, maxV)))
            .stroke(paint(valueColorV, "p-knob-value-background"))
            .strokeWidth(Svg.SvgLength.User(strokeWidthV))
            .strokeLinecap(Svg.StrokeLinecap.Round)

        val label = valueTemplateV.map(_(value)).getOrElse(InputNumber.format(value))
        val text  = Svg.text
            .cssClass("p-knob-text")
            .x(50)
            .y(57)
            .textAnchor(Svg.TextAnchor.Middle)
            .fill(paint(textColorV, "p-knob-text-color"))(label)

        var dial = Svg.svg
            .cssClass("p-uic-knob-dial")
            .viewBox(Svg.ViewBox(0, 0, 100, 100))
            .width(sizeV)
            .height(sizeV)
            .role("slider")
            .aria("valuemin", InputNumber.format(minV))
            .aria("valuemax", InputNumber.format(maxV))
            .aria("valuenow", InputNumber.format(value))
        accNameV match
            case Present(TextValue.Const(v)) => dial = dial.aria("label", v)
            case Present(TextValue.Dyn(s))   => dial = dial.aria("label", s)
            case Absent                      => ()
        end match
        idV.foreach(v => dial = dial.id(v))
        if readonlyFlag then dial = dial.aria("readonly", "true")
        if !disabledFlag.constTrue then dial = dial.tabIndex(0)
        // Blur fires even without a turn: the validation layer's Blur trigger. Reads the
        // binding LIVE where there is one, so it reports the value at blur time.
        onBlurF.foreach { f =>
            dial = dial.onBlur(live match
                case Present(r) => r.current.map(v => f(clamp(v)))
                case Absent     => f(value))
        }
        if interactive then
            dial = dial.preventScrollKeys.onKeyDown { e =>
                e.key match
                    case Keyboard.ArrowUp | Keyboard.ArrowRight  => commit(clamp(value + stepV))
                    case Keyboard.ArrowDown | Keyboard.ArrowLeft => commit(clamp(value - stepV))
                    case Keyboard.PageUp                         => commit(clamp(value + 10 * stepV))
                    case Keyboard.PageDown                       => commit(clamp(value - 10 * stepV))
                    case Keyboard.Home                           => commit(minV)
                    case Keyboard.End                            => commit(maxV)
                    case _                                       => ()
            }
        end if

        val children: List[Svg.SvgChild] =
            if showValueFlag then List(rangeArc, valueArc, text) else List(rangeArc, valueArc)
        dial(children*)
    end dialSvg
end Knob

object Knob:
    def apply(): Knob = new Knob()

    // === Prime's dial math (pure) =============================================
    // 240° sweep: zero at 4π/3 (lower left), full at −π/3 (lower right), radius
    // 40 around the viewBox center (50, 50). Ported from PrimeVue's Knob.

    private val radius     = 40.0
    private val mid        = 50.0
    private val minRadians = 4.0 * math.Pi / 3.0
    private val maxRadians = -math.Pi / 3.0

    private def mapRange(x: Double, inMin: Double, inMax: Double, outMin: Double, outMax: Double): Double =
        if inMax == inMin then outMin else (x - inMin) * (outMax - outMin) / (inMax - inMin) + outMin

    /** Locale-independent short decimal (Double.toString after 3-digit rounding). */
    private def n(v: Double): String =
        val r = math.rint(v * 1000) / 1000
        if r.isWhole then r.toLong.toString else r.toString

    private def pointAt(radians: Double): (Double, Double) =
        (mid + radius * math.cos(radians), mid - radius * math.sin(radians))

    /** Inverse of the dial math: the value for a pointer at local (x, y) inside a
      * (w × h) surface. Converts the screen vector from the center into Prime's
      * y-up angle, unwraps it into the `[maxRadians, minRadians]` sweep window
      * (the bottom gap clamps to the nearest end), maps it back to the value
      * range, and snaps to `step`.
      */
    private[uic] def valueFromPointer(x: Double, y: Double, w: Double, h: Double, min: Double, max: Double, step: Double): Double =
        val cx  = w / 2.0
        val cy  = h / 2.0
        var rad = math.atan2(cy - y, x - cx) // screen y-down → math y-up
        if rad < maxRadians then rad += 2 * math.Pi
        val clamped = math.max(maxRadians, math.min(minRadians, rad))
        val raw     = mapRange(clamped, minRadians, maxRadians, min, max)
        val st      = if step <= 0 then 1.0 else step
        val snapped = min + math.round((raw - min) / st) * st
        math.max(min, math.min(max, snapped))
    end valueFromPointer

    /** The full background arc (`.p-knob-range`). */
    private[uic] def rangePath: String =
        val (minX, minY) = pointAt(minRadians)
        val (maxX, maxY) = pointAt(maxRadians)
        s"M ${n(minX)} ${n(minY)} A ${n(radius)} ${n(radius)} 0 1 1 ${n(maxX)} ${n(maxY)}"
    end rangePath

    /** The value arc (`.p-knob-value`) from the zero anchor to the value angle. */
    private[uic] def valuePath(value: Double, min: Double, max: Double): String =
        val zeroRadians =
            if min > 0 && max > 0 then mapRange(min, min, max, minRadians, maxRadians)
            else mapRange(0, min, max, minRadians, maxRadians)
        val valueRadians     = mapRange(value, min, max, minRadians, maxRadians)
        val (zeroX, zeroY)   = pointAt(zeroRadians)
        val (valueX, valueY) = pointAt(valueRadians)
        val largeArc         = if math.abs(zeroRadians - valueRadians) < math.Pi then 0 else 1
        val sweep            = if valueRadians > zeroRadians then 0 else 1
        s"M ${n(zeroX)} ${n(zeroY)} A ${n(radius)} ${n(radius)} 0 $largeArc $sweep ${n(valueX)} ${n(valueY)}"
    end valuePath
end Knob
