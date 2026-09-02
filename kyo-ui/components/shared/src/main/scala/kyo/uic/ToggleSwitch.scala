package kyo.uic

import kyo.*
import kyo.UI.*

/** On/off toggle — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ToggleSwitch anatomy: `div.p-toggleswitch` > hidden native
  * `input.p-toggleswitch-input[role=switch]` + visual `div.p-toggleswitch-slider`
  * with the sliding `div.p-toggleswitch-handle`). The kyo
  * `<input type="checkbox">` provides the real boolean state, so the change
  * event and disabled state are inherited. Prime has no on/off state text —
  * place a plain `span` next to the switch instead. `handleIcon` fills Prime's
  * handle slot with a per-state glyph riding inside the sliding handle. Two-way
  * binding mirrors [[CheckBox]].
  */
final case class ToggleSwitch private (
    checkedBinding: Maybe[CheckBox.Checked] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Maybe[Boolean] = Absent,
    requiredFlag: Maybe[Boolean] = Absent,
    nameV: Maybe[String] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    tooltipV: Maybe[TextValue] = Absent,
    handleIconsV: Maybe[(IconGlyph, IconGlyph)] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Boolean => Any < Async] = Absent,
    onBlurF: Maybe[Boolean => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, BooleanFormControl, HasTooltip, HasAccessibleNameRef:
    type Self = ToggleSwitch

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): ToggleSwitch = copy(idV = v)

    /** Sets a constant on/off state. */
    def checked(v: Boolean): ToggleSwitch = copy(checkedBinding = Present(CheckBox.Checked.Const(v)))

    /** Binds two-way to `ref`: toggles write back into the ref, ref changes update the switch. */
    def checked(ref: SignalRef[Boolean]): ToggleSwitch = copy(checkedBinding = Present(CheckBox.Checked.Ref(ref)))

    /** Disables the switch. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the
      * root's `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): ToggleSwitch = copy(disabledFlag = Present(ReactiveValue(v)))

    /** `readonly` — the switch reports `aria-readonly` and refuses to change, but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): ToggleSwitch = copy(readonlyFlag = Present(v))

    /** Marks the switch required (`aria-required` + native constraint). */
    def required(v: Boolean): ToggleSwitch = copy(requiredFlag = Present(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): ToggleSwitch = copy(nameV = Present(v))

    private[uic] def withInvalid(v: Maybe[BoolValue]): ToggleSwitch                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): ToggleSwitch                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): ToggleSwitch = copy(invalidMsgDynV = v)

    private[uic] def withTooltip(v: Maybe[TextValue]): ToggleSwitch = copy(tooltipV = v)

    /** Prime's handle slot: a glyph rendered inside the sliding
      * `.p-toggleswitch-handle` — `checked` while on, `unchecked` while off.
      */
    def handleIcon(checked: IconGlyph, unchecked: IconGlyph): ToggleSwitch =
        copy(handleIconsV = Present((checked, unchecked)))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): ToggleSwitch = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): ToggleSwitch = copy(accNameRefV = v)

    def onChange(f: Boolean => Any < Async): ToggleSwitch = copy(onChangeF = Present(f))

    /** Fires on focus loss with the switch's current state — unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => Any < Async): ToggleSwitch = copy(onBlurF = Present(f))

    private[uic] def render(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
        checkedBinding match
            case Present(CheckBox.Checked.Ref(ref)) => ref.render(b => body(b, Present(handlerFor(ref))))
            case Present(CheckBox.Checked.Const(v)) => body(v, onChangeF)
            case Absent                             => body(false, onChangeF)

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
        val isReadonly = readonlyFlag.getOrElse(false)

        var box = checkbox
            .cssClass("p-toggleswitch-input")
            .role("switch")
            .checked(isChecked)
            .aria("checked", isChecked.toString)
        box = idV.map(v => box.id(v)).getOrElse(box)
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        if invalidV.constTrue then box = box.aria("invalid", "true")
        if isReadonly then box = box.aria("readonly", "true")
        box = accNameV match
            case Present(TextValue.Const(v)) => box.aria("label", v)
            case Present(TextValue.Dyn(s))   => box.aria("label", s)
            case Absent                      => box
        box = accNameRefV.map(v => box.aria("labelledby", v)).getOrElse(box)
        // readonly keeps the switch FOCUSABLE and refuses to change: the client declines the
        // browser's own toggle (`preventActivation`) and the handler is not wired. Disabling the
        // native input instead took it out of the tab order, which made readonly and disabled the
        // same thing to a keyboard. Only `disabled` is native. See [[CheckBox]] for the reasoning.
        if isReadonly then box = box.preventActivation
        box = disabledFlag.foldFlag(box)(box.disabled(_))
        if !isReadonly then box = onChg.map(box.onChange(_)).getOrElse(box)
        if !isReadonly then
            box = onBlurF match
                case Present(f) =>
                    box.onBlur(checkedBinding match
                        case Present(CheckBox.Checked.Ref(r))   => r.use(f)
                        case Present(CheckBox.Checked.Const(v)) => f(v)
                        case Absent                             => f(false))
                case Absent => box
        end if

        // Prime's handle slot: the current-state glyph rides inside the handle
        // (the `.p-uic-*` remainder sizes it below the handle box).
        val handleContent: List[UI] = handleIconsV.toList.map { (on, off) =>
            GlyphSvg(if isChecked then on else off, "p-uic-toggleswitch-handle-icon")
        }
        val slider = div.cssClass("p-toggleswitch-slider")(
            toChild(div.cssClass("p-toggleswitch-handle")(handleContent.map(toChild)*): UI)
        )

        var root = div.cssClass("p-toggleswitch").cssClass("p-component")
        if isChecked then root = root.cssClass("p-toggleswitch-checked")
        root = disabledFlag.foldFlag(root)(root.cssClass("p-disabled", _))
        if invalidV.constTrue then root = root.cssClass("p-invalid")
        root = tooltipV match
            case Present(TextValue.Const(v)) => root.jsProp("title", v)
            case Present(TextValue.Dyn(s))   => root.title(s)
            case Absent                      => root

        FieldInvalid.withMessage(root(List[UI](box, slider).map(toChild)*), invalidV.constTrue, invalidMsgV)
    end body
end ToggleSwitch

object ToggleSwitch:
    def apply(): ToggleSwitch = new ToggleSwitch()
