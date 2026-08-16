package kyo.uic

import kyo.*
import kyo.UI.*

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
  */
final case class Rating private (
    valueBinding: Maybe[ReactiveValue[Int]] = Absent,
    starsV: Int = 5,
    readonlyFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    nameV: Maybe[String] = Absent,
    onIconV: Maybe[IconGlyph] = Absent,
    offIconV: Maybe[IconGlyph] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Int => Any < Async] = Absent
) extends Node:
    type Self = Rating

    /** Sets a constant value (0 = nothing selected). */
    def value(v: Int): Rating = copy(valueBinding = Present(ReactiveValue.Const(v)))

    /** Binds two-way to `ref`: clicks write the new value back, ref changes re-render. */
    def value(ref: SignalRef[Int]): Rating = copy(valueBinding = Present(ReactiveVariable(ref)))

    /** Binds to a one-way DERIVED signal: the stars track it read-only (clicks write
      * nowhere). Prefer this over an artificial `SignalRef` when the value is computed.
      */
    def value(sig: Signal[Int]): Rating = copy(valueBinding = Present(ReactiveValue.Dyn(sig)))

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

    /** Fired with the NEW value (0 on cancel) after the ref write-back. */
    def onChange(f: Int => Any < Async): Rating = copy(onChangeF = Present(f))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
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
        if readonlyFlag then el = el.cssClass("p-readonly")
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        el(options.map(toChild)*)
    end body

    /** Prime's select semantics: picking the current value clears to 0. */
    private def activate(i: Int, current: Int, ref: Maybe[SignalRef[Int]])(using Frame): Any < Async =
        val next = if i == current then 0 else i
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
    end activate
end Rating

object Rating:
    def apply(): Rating = new Rating()
