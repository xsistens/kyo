package kyo.uic

import kyo.*
import kyo.UI.*

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
    orientationV: Orientation = Orientation.Horizontal,
    disabledFlag: Maybe[BoolValue] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Double => Any < Async] = Absent
) extends Node:
    type Self = Slider

    /** Sets a constant value (renders the fill/handle statically). */
    def value(v: Double): Slider = copy(valueBinding = Present(ReactiveValue.Const(v)))

    /** Binds two-way to `ref`: drags/keys write the committed value back, ref
      * changes move the fill and handle.
      */
    def value(ref: SignalRef[Double]): Slider = copy(valueBinding = Present(ReactiveVariable(ref)))

    /** Binds to a one-way DERIVED signal: the fill/handle track it read-only (drags
      * write nowhere). Prefer this over an artificial `SignalRef` when the value is
      * computed, not user-edited.
      */
    def value(sig: Signal[Double]): Slider = copy(valueBinding = Present(ReactiveValue.Dyn(sig)))

    /** Lower bound (Prime default 0). */
    def min(v: Double): Slider = copy(minV = v)

    /** Upper bound (Prime default 100). */
    def max(v: Double): Slider = copy(maxV = v)

    /** Drag/keyboard increment (Prime default 1). */
    def step(v: Double): Slider = copy(stepV = if v <= 0 then 1.0 else v)

    /** Track orientation — `.p-slider-horizontal` (default) or `.p-slider-vertical`. */
    def orientation(v: Orientation): Slider = copy(orientationV = v)

    /** Disables the control (`.p-disabled` + native disabled input); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Slider = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Accessible name → `aria-label` on the native range input. */
    def accessibleName(v: String): Slider = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Slider = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => Any < Async): Slider = copy(onChangeF = Present(f))

    private def clamp(v: Double): Double = math.max(minV, math.min(maxV, v))

    private[uic] def render(using Frame): UI =
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
            .step(stepV)
            .value(value)
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

        var el = div
            .cssClass("p-slider")
            .cssClass("p-component")
            .cssClass(if horizontal then "p-slider-horizontal" else "p-slider-vertical")
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        el(toChild(range), toChild(handle), toChild(in: UI))
    end body
end Slider

object Slider:
    def apply(): Slider = new Slider()
