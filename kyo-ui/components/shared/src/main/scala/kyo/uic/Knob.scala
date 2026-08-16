package kyo.uic

import kyo.*
import kyo.UI.*

/** Knob — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Knob
  * anatomy: `div.p-knob.p-component` > `svg[viewBox=0 0 100 100]` with the
  * `.p-knob-range` background arc, the `.p-knob-value` value arc and the
  * `.p-knob-text` center label), so the extracted `@primeuix` knob CSS applies
  * verbatim. The arc paths use Prime's own dial math (radius 40 around
  * (50, 50), 240° sweep from 4π/3 to −π/3) computed server-side from the bound
  * value — pure geometry, no client script.
  *
  * Interaction is keyboard AND pointer-drag. Keyboard (Prime's slider keys on
  * the focusable svg): ArrowUp/ArrowRight step up, ArrowDown/ArrowLeft step
  * down, PageUp/PageDown jump ±10 steps, Home/End go to min/max. Pointer
  * (mounted6 drag): pressing/dragging the dial maps the pointer angle
  * (`atan2` around the dial center, in Prime's 240°-gap geometry) to the value,
  * snapped to `step`; the bottom dead-zone clamps to the nearest end. Each
  * writes the clamped value into the bound `value(SignalRef[Double])` before
  * `onChange` fires. The drag handler sits on the stable outer div while the svg
  * is pointer-events:none (gotcha #1), so per-frame re-render never detaches the
  * capture.
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
    disabledFlag: Maybe[BoolValue] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Double => Any < Async] = Absent
) extends Node:
    type Self = Knob

    /** Sets a constant value (renders the dial statically). */
    def value(v: Double): Knob = copy(valueBinding = Present(ReactiveValue.Const(v)))

    /** Binds two-way to `ref`: keyboard steps write the clamped value back, ref
      * changes redraw the arc.
      */
    def value(ref: SignalRef[Double]): Knob = copy(valueBinding = Present(ReactiveVariable(ref)))

    /** Binds to a one-way DERIVED signal: the arc tracks it read-only (no drag/keyboard
      * write-back). Prefer this over an artificial `SignalRef` when the value is computed.
      */
    def value(sig: Signal[Double]): Knob = copy(valueBinding = Present(ReactiveValue.Dyn(sig)))

    /** Lower bound (Prime default 0). */
    def min(v: Double): Knob = copy(minV = v)

    /** Upper bound (Prime default 100). */
    def max(v: Double): Knob = copy(maxV = v)

    /** Keyboard increment (Prime default 1). */
    def step(v: Double): Knob = copy(stepV = if v <= 0 then 1.0 else v)

    /** Rendered size in px (width = height; Prime default 100). */
    def size(px: Int): Knob = copy(sizeV = math.max(20, px))

    /** Arc stroke width in viewBox units (Prime default 14). */
    def strokeWidth(v: Double): Knob = copy(strokeWidthV = math.max(1.0, v))

    /** Shows the center value label (Prime default true). */
    def showValue(v: Boolean): Knob = copy(showValueFlag = v)

    /** Formats the center label from the current value. */
    def valueTemplate(f: Double => String): Knob = copy(valueTemplateV = Present(f))

    /** Value-arc color — any CSS color or `var()` (default the knob value token). */
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

    /** Accessible name → `aria-label` on the svg dial. */
    def accessibleName(v: String): Knob = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Knob = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => Any < Async): Knob = copy(onChangeF = Present(f))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    private def clamp(v: Double): Double = math.max(minV, math.min(maxV, v))

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        valueBinding match
            case Present(ReactiveVariable(ref)) =>
                // Pointer-drag (mounted6) + keyboard. The drag handler sits on the STABLE
                // outer div (gotcha #1) — never re-rendered — while the reactive svg dial
                // inside is pointer-events:none, so the pointer always lands on the div
                // whose rect is the dial's coordinate space. The handler reads ref.get
                // LIVE, so a drag onto any angle sets that angle regardless of the
                // render-time value.
                val reactiveSvg: UI = ref.render(v => dialSvg(clamp(v), Present(ref)))
                var root            = div.cssClass("p-knob").cssClass("p-component")
                if disabledFlag.constTrue then root = root.cssClass("p-disabled")
                if interactive then
                    val onDrag = (e: PointerEvent) =>
                        commit(Knob.valueFromPointer(e.x, e.y, e.rectW, e.rectH, minV, maxV, stepV), Present(ref))
                    root = root.onPointerDown(onDrag).onPointerMove(onDrag)
                end if
                root(toChild(reactiveSvg))
            case Present(ReactiveValue.Dyn(sig)) => sig.render(v => staticRoot(clamp(v)))
            case Present(ReactiveValue.Const(v)) => staticRoot(clamp(v))
            case Absent                          => staticRoot(minV)

    private def staticRoot(value: Double)(using Frame): UI =
        var root = div.cssClass("p-knob").cssClass("p-component")
        if disabledFlag.constTrue then root = root.cssClass("p-disabled")
        root(toChild(dialSvg(value, Absent)))
    end staticRoot

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

    /** The reactive svg dial (arcs + label + role=slider + keyboard). It carries
      * `p-uic-knob-dial` (pointer-events:none) so the drag pointer lands on the
      * stable outer div, and stays keyboard-focusable (Tab) with the full
      * arrow/Page/Home/End key set.
      */
    private def dialSvg(value: Double, ref: Maybe[SignalRef[Double]])(using Frame): UI =
        val paint = (spec: Maybe[String], token: String) =>
            spec.flatMap(CssValue.color).getOrElse(Style.Color.variable(token))

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
        val text = Svg.text
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
        if readonlyFlag then dial = dial.aria("readonly", "true")
        if !disabledFlag.constTrue then dial = dial.tabIndex(0)
        if interactive then
            dial = dial.preventScrollKeys.onKeyDown { e =>
                e.key match
                    case Keyboard.ArrowUp | Keyboard.ArrowRight  => commit(clamp(value + stepV), ref)
                    case Keyboard.ArrowDown | Keyboard.ArrowLeft => commit(clamp(value - stepV), ref)
                    case Keyboard.PageUp                         => commit(clamp(value + 10 * stepV), ref)
                    case Keyboard.PageDown                       => commit(clamp(value - 10 * stepV), ref)
                    case Keyboard.Home                           => commit(minV, ref)
                    case Keyboard.End                            => commit(maxV, ref)
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
