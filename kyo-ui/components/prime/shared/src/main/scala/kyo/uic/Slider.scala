package kyo.uic

import kyo.*
import kyo.UI.*

/** Slider: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Slider anatomy: `div.p-slider.p-component.p-slider-horizontal|-vertical` >
  * `span.p-slider-range` (the value fill) + `span.p-slider-handle`), so the
  * extracted `@primeuix` slider CSS applies verbatim.
  *
  * Interaction is a REAL `<input type=range>` (kyo's `rangeInput`) stretched
  * invisibly over the track (`.p-uic-slider-native`, kyo extension): dragging
  * and the native keyboard (arrows/Home/End) work for free, and the browser
  * delivers the committed value as a Double which is written back into the
  * bound `value(SignalRef[Double])` before `onChange` fires. The Prime divs
  * are the VISUAL layer only: the range fill and handle position are computed
  * server-side from the bound value ((value − min) / (max − min)), so a ref
  * write from anywhere moves the slider.
  *
  * A `FormControl[Double]`, so `uic.Slider().bind(field)` wires value, validity,
  * message, the blur trigger and (for a whole-valued field) the [[integer]]
  * constraint in one call.
  *
  * Honest limits under server rendering: the native `change` event fires when
  * a drag COMMITS (pointer release), so the visual fill follows a drag on
  * release, not continuously (keyboard steps commit per keypress and track
  * live). Prime's dual-handle `range` mode is not ported: it would need two
  * overlapping native inputs fighting for the same pointer surface, so it is deferred
  * rather than approximated.
  */
final class Slider private[uic] (private[uic] val p: Slider.Props) extends Node, FormControl[Double], HasAccessibleName, HasProps:
    import p.*

    type Self = Slider

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Slider(p.copy(access = v))

    /** The slider's value, in any of the three bindings a value slot holds. A constant nobody hears is
      * fixed, drawn read-only; one [[onChange]] hears is reported and the thumb put back where the
      * constant holds it. A writable `SignalRef[Double]` binds TWO-WAY: drags and keys write the
      * committed value back, and ref changes move the fill and handle. Any other `Signal[Double]` binds
      * one-way, so the fill and handle track it and drags write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Double]` does not
      * opt out of write-back; pass `ref.readOnly` for that. A derived signal is already one-way and is
      * the better answer when the value is computed rather than user-edited.
      */
    def value(v: Double | Signal[Double]): Slider = new Slider(p.copy(valueSlot = valueSlot.bind(v)))

    /** Lower bound (Prime default 0). */
    def min(v: Double): Slider = new Slider(p.copy(minV = v))

    /** Upper bound (Prime default 100). */
    def max(v: Double): Slider = new Slider(p.copy(maxV = v))

    /** Drag/keyboard increment (Prime default 1). */
    def step(v: Double): Slider = new Slider(p.copy(stepV = if v <= 0 then 1.0 else v))

    /** Constrains committed values to whole numbers: the value written back is rounded,
      * and the native input steps in whole units. The form layer flips this on for a
      * whole-valued (`Int`/`Long`) [[kyo.uic.form.NumberField]], so a slider bound to one
      * cannot hand it a fraction.
      */
    def integer(v: Boolean): Slider = new Slider(p.copy(integerFlag = v))

    /** Track orientation: `.p-slider-horizontal` (default) or `.p-slider-vertical`. */
    def orientation(v: Orientation): Slider = new Slider(p.copy(orientationV = v))

    /** Disables the control (`.p-disabled` + native disabled input); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Slider = new Slider(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** `readonly`: the slider keeps its value and its normal look and stays reachable, so a reader can land on it and read
      * it, and no drag or key moves it. The browser's own move is declined in the client, like a readonly CheckBox.
      */
    def readonly(v: Boolean): Slider = new Slider(p.copy(readonlyFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                    = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self            = new Slider(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Double])(using Frame): Slider  = value(ref)
    private[uic] def hearBlur(f: Double => UI.Action)(using Frame): Slider = onBlur(f)

    /** Fired with the NEW (clamped) value after the ref write-back. */
    def onChange(f: Double => UI.Action)(using Frame): Slider = new Slider(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fires on focus loss with the slider's current value; unlike onChange it fires even
      * when the handle was not moved. The validation layer's Blur trigger.
      */
    def onBlur(f: Double => UI.Action)(using Frame): Slider = new Slider(p.copy(onBlurF = Present(Callback.of(f))))

    /** Clamps into range, and rounds to a whole number under [[integer]]. */
    private def clamp(v: Double): Double =
        val bounded = math.max(minV, math.min(maxV, v))
        if integerFlag then math.rint(bounded) else bounded

    private[uic] def render(using Frame): UI =
        // A constant nobody hears is fixed: drawn read-only. Unbound, the field keeps what the reader does itself.
        if valueSlot.fixed && !readonlyFlag then new Slider(p.copy(readonlyFlag = true)).render
        else Controlled.owning(valueSlot, start = minV)(s => new Slider(this.p.copy(valueSlot = s)).renderLive)

    private def renderLive(using Frame): UI =
        val validity = FieldValidity(validityV)
        val field    = validity.follow(BoolValue.reactive(disabledFlag)(d => new Slider(p.copy(disabledFlag = d)).renderResolved(validity)))
        // A signal nobody hears: every change the reader makes is put back.
        Diagnostics.after(field)("Slider")(Diagnostic.unheard(valueSlot, Slider.valueName, on = !readonlyFlag && !disabledFlag.constTrue))
    end renderLive

    // Bound one way the native thumb follows the signal, so kyo-ui puts back a drag the owner does not take; the visual layer is
    // drawn from it. A constant somebody hears is the same, and a constant nobody hears is fixed (drawn read-only).
    private def renderResolved(validity: FieldValidity)(using Frame): UI =
        Controlled.drawField(valueSlot, unbound = minV)(f => body(f.value, validity, Controlled.mapNative[Double, Double](f.native)(clamp)))

    private def body(raw: Double, validity: FieldValidity, native: Double | Signal[Double])(using Frame): UI =
        val value = clamp(raw)
        val pct   =
            if maxV <= minV then 0.0
            else (value - minV) / (maxV - minV) * 100.0
        val horizontal = orientationV == Orientation.Horizontal

        // === visual layer (Prime's divs, positions computed server-side) ========
        val range =
            if horizontal then span.cssClass("p-slider-range").style(_.width(pct.pct))
            else span.cssClass("p-slider-range").style(_.height(pct.pct))
        val handle =
            val base = span.cssClass("p-slider-handle").aria(Aria.hidden -> "true")
            if horizontal then base.style(_.left(pct.pct)) else base.style(_.bottom(pct.pct))

        // === interaction layer: the invisible native range input ================
        var in = rangeInput
            .cssClass("p-uic-slider-native")
            .min(minV)
            .max(maxV)
            .step(if integerFlag then math.max(1.0, math.rint(stepV)) else stepV)
            .value(native)
        access.id.foreach(v => in = in.id(v))
        access.name.foreach(v => in = in.aria(Aria.label -> v))
        if disabledFlag.constTrue then in = in.disabled(true)
        else if readonlyFlag then in = in.aria(Aria.readonly -> "true").preventActivation
        else
            in = in.onChange(d => valueSlot.change(clamp(d)))
        end if
        // Blur fires even without a drag: the validation layer's Blur trigger. Reads the
        // ref LIVE where there is one, so it reports the value at blur time.
        onBlurF.foreach { f =>
            in = in.onBlur(valueSlot.writable match
                case Present(r) => r.use(v => f(clamp(v)))
                case _          => f(value))
        }

        var el = div
            .cssClass("p-slider")
            .cssClass("p-component")
            .cssClass(if horizontal then "p-slider-horizontal" else "p-slider-vertical")
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        validity.mark(el)(range, handle, in: UI)
    end body
end Slider

object Slider:
    /** How a typed number field turns on the whole-number constraint: through `integer` ([[HasSetter]]). */
    given integer: HasSetter[Slider, "integer", Boolean] with
        def apply(c: Slider, v: Boolean)(using Frame): Slider = c.integer(v)

    final private[uic] case class Props(
        valueSlot: Controlled[Double] = Controlled(),
        minV: Double = 0.0,
        maxV: Double = 100.0,
        stepV: Double = 1.0,
        integerFlag: Boolean = false,
        orientationV: Orientation = Orientation.Horizontal,
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onBlurF: Maybe[Double => UI.Action] = Absent
    )

    def apply(): Slider = new Slider(Props())

    private val valueName = Controlled.Name("Slider", "value", "a change", "onChange")
end Slider
