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
  * toggled value back into the ref before firing the user `onChange`. It is a
  * [[BooleanFormControl]] for the same reason, so `uic.ToggleButton().bind(field)`
  * wires value, validity, message and the blur trigger in one call.
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
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Boolean => Any < Async] = Absent,
    onBlurF: Maybe[Boolean => Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    contentV: Maybe[UI] = Absent
) extends Node, BooleanFormControl:
    type Self = ToggleButton

    /** Native `id` on the button — pair with `Label.forId`; the form layer stamps the
      * bound field's id here so focus-first-invalid can target it.
      */
    def id(v: String): ToggleButton = copy(idV = Present(v))

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
    def invalid(v: Boolean): ToggleButton = copy(invalidV = Present(BoolValue.Const(v)))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): ToggleButton = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Message rendered below the button while it is invalid (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): ToggleButton = copy(invalidMsgV = Present(v))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * button red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): ToggleButton = copy(invalidMsgDynV = Present(sig))

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

    /** Fires on focus loss with the button's current checked state — unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => Any < Async): ToggleButton = copy(onBlurF = Present(f))

    private[uic] def render(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
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
        idV.foreach(v => el = el.id(v))
        if isChecked then el = el.cssClass("p-togglebutton-checked")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        sizeV match
            case Size.Small  => el = el.cssClass("p-togglebutton-sm").cssClass("p-inputfield-sm")
            case Size.Large  => el = el.cssClass("p-togglebutton-lg").cssClass("p-inputfield-lg")
            case Size.Normal => ()
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
        // Blur fires even without a toggle — the validation layer's Blur trigger. Reads the
        // ref LIVE (the CheckBox shape), so it reports the state at blur time.
        onBlurF.foreach { f =>
            el = el.onBlur(checkedBinding match
                case Present(ToggleButton.Checked.Ref(r))   => r.use(f)
                case Present(ToggleButton.Checked.Const(v)) => f(v)
                case Absent                                 => f(false))
        }

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

        FieldInvalid.withMessage(
            el(span.cssClass("p-togglebutton-content")(contentChildren.map(toChild)*)),
            invalidV.constTrue,
            invalidMsgV
        )
    end body
end ToggleButton

object ToggleButton:
    def apply(): ToggleButton = new ToggleButton()

    /** Const-or-ref carrier for the pending `checked` state (the CheckBox pattern). */
    private[uic] enum Checked:
        case Const(v: Boolean)
        case Ref(ref: SignalRef[Boolean])
end ToggleButton
