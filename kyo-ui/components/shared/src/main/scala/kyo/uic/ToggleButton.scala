package kyo.uic

import kyo.*
import kyo.UI.*

/** ToggleButton — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ToggleButton anatomy: `button.p-togglebutton.p-component
  * [.p-togglebutton-checked][.p-invalid][.p-togglebutton-sm|-lg]` >
  * `span.p-togglebutton-content` > [icon `.p-togglebutton-icon`] +
  * `span.p-togglebutton-label`), so the extracted `@primeuix` togglebutton CSS
  * applies verbatim.
  *
  * The label follows Prime's semantics: `onLabel`/`offLabel` default to
  * "Yes"/"No"; when BOTH are cleared the label span renders a non-breaking
  * space so the content box keeps its height (Prime renders `\u00A0`).
  * `checked(SignalRef)` binds two-way exactly like CheckBox: clicks write the
  * toggled value back into the ref before firing the user `onChange`.
  */
final case class ToggleButton private (
    checkedBinding: Maybe[ToggleButton.Checked] = Absent,
    onLabelV: Maybe[TextValue] = Present(TextValue.Const("Yes")),
    offLabelV: Maybe[TextValue] = Present(TextValue.Const("No")),
    onIconV: Maybe[IconGlyph] = Absent,
    offIconV: Maybe[IconGlyph] = Absent,
    sizeV: Size = Size.Normal,
    fluidFlag: Boolean = false,
    disabledFlag: Boolean = false,
    readonlyFlag: Boolean = false,
    invalidFlag: Boolean = false,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Boolean => Any < Async] = Absent,
    contentV: Maybe[UI] = Absent
) extends Node:
    type Self = ToggleButton

    /** Package-internal content slot: SelectButton's `itemTemplate` replaces the
      * icon+label pair inside `.p-togglebutton-content` with arbitrary UI (the
      * kyo analogue of Prime's default slot).
      */
    private[uic] def content(ui: UI): ToggleButton = copy(contentV = Present(ui))

    /** Sets a constant checked state. */
    def checked(v: Boolean): ToggleButton = copy(checkedBinding = Present(ToggleButton.Checked.Const(v)))

    /** Binds two-way to `ref`: clicks write the toggled value back, ref changes re-render. */
    def checked(ref: SignalRef[Boolean]): ToggleButton = copy(checkedBinding = Present(ToggleButton.Checked.Ref(ref)))

    /** Label while checked (Prime default "Yes"). */
    def onLabel(v: String): ToggleButton = copy(onLabelV = Present(TextValue.Const(v)))

    /** Reactive checked-state label tracking `sig` — re-renders in place on emission. */
    def onLabel(sig: Signal[String]): ToggleButton = copy(onLabelV = Present(TextValue.Dyn(sig)))

    /** Label while unchecked (Prime default "No"). */
    def offLabel(v: String): ToggleButton = copy(offLabelV = Present(TextValue.Const(v)))

    /** Reactive unchecked-state label tracking `sig` — re-renders in place on emission. */
    def offLabel(sig: Signal[String]): ToggleButton = copy(offLabelV = Present(TextValue.Dyn(sig)))

    /** Drops both labels — the label span renders a non-breaking space (icon-only look). */
    def noLabels: ToggleButton = copy(onLabelV = Absent, offLabelV = Absent)

    /** Icon while checked (`.p-togglebutton-icon`). */
    def onIcon(glyph: IconGlyph): ToggleButton = copy(onIconV = Present(glyph))

    /** Icon while unchecked (`.p-togglebutton-icon`). */
    def offIcon(glyph: IconGlyph): ToggleButton = copy(offIconV = Present(glyph))

    /** Size: `.p-togglebutton-sm` / default / `.p-togglebutton-lg`. */
    def size(v: Size): ToggleButton = copy(sizeV = v)

    /** Spans the full width of its container (`.p-togglebutton-fluid`). */
    def fluid(v: Boolean): ToggleButton = copy(fluidFlag = v)

    def disabled(v: Boolean): ToggleButton = copy(disabledFlag = v)

    /** `readonly` — interaction is blocked (no toggle on click), but unlike
      * `disabled` the button keeps its normal look and stays focusable.
      */
    def readonly(v: Boolean): ToggleButton = copy(readonlyFlag = v)

    /** Marks the button invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): ToggleButton = copy(invalidFlag = v)

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): ToggleButton = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): ToggleButton = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): ToggleButton = copy(accNameRefV = Present(v))

    /** Fired with the NEW checked value after a toggle (and after the ref write-back). */
    def onChange(f: Boolean => Any < Async): ToggleButton = copy(onChangeF = Present(f))

    private[uic] def render(using Frame): UI =
        checkedBinding match
            case Present(ToggleButton.Checked.Ref(ref)) => ref.render(b => body(b, Present(handlerFor(ref))))
            case Present(ToggleButton.Checked.Const(v)) => body(v, onChangeF)
            case Absent                                 => body(false, onChangeF)

    /** Two-way handler: write the toggled value into `ref`, then run the user callback. */
    private def handlerFor(ref: SignalRef[Boolean])(using Frame): Boolean => Any < Async =
        nb =>
            onChangeF match
                case Present(f) =>
                    for
                        _ <- ref.set(nb)
                        r <- f(nb)
                    yield r
                case Absent => ref.set(nb)

    private def body(isChecked: Boolean, onChg: Maybe[Boolean => Any < Async])(using Frame): UI =
        var el = button.cssClass("p-togglebutton").cssClass("p-component")
        if isChecked then el = el.cssClass("p-togglebutton-checked")
        if invalidFlag then el = el.cssClass("p-invalid").aria("invalid", "true")
        sizeV match
            case Size.Small               => el = el.cssClass("p-togglebutton-sm").cssClass("p-inputfield-sm")
            case Size.Large | Size.XLarge => el = el.cssClass("p-togglebutton-lg").cssClass("p-inputfield-lg")
            case Size.Normal              => ()
        end match
        if fluidFlag then el = el.cssClass("p-togglebutton-fluid")
        el = el.jsProp("type", "button").aria("pressed", isChecked.toString)
        accNameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => el = el.aria("labelledby", v))
        if disabledFlag then el = el.disabled(true)
        // readonly keeps the normal look and focusability, but never toggles.
        else if !readonlyFlag then onChg.foreach(f => el = el.onClick(f(!isChecked)))

        val contentChildren: List[UI] = contentV match
            case Present(ui) => List(ui)
            case Absent =>
                val iconSlot: List[UI] =
                    (if isChecked then onIconV else offIconV).toList.map(g => GlyphSvg(g, "p-togglebutton-icon"))
                // Prime: the current-state label, or a non-breaking space when no labels are set.
                val labelUI: UI = (if isChecked then onLabelV else offLabelV) match
                    case Present(TextValue.Const(t)) => span.cssClass("p-togglebutton-label")(t): UI
                    case Present(TextValue.Dyn(s))   => s.render(t => span.cssClass("p-togglebutton-label")(t))
                    case _                           => span.cssClass("p-togglebutton-label")("\u00A0"): UI
                iconSlot :+ labelUI

        el(span.cssClass("p-togglebutton-content")(contentChildren.map(toChild)*))
    end body
end ToggleButton

object ToggleButton:
    def apply(): ToggleButton = new ToggleButton()

    /** Const-or-ref carrier for the pending `checked` state (the CheckBox pattern). */
    private[uic] enum Checked:
        case Const(v: Boolean)
        case Ref(ref: SignalRef[Boolean])
end ToggleButton
