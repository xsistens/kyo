package kyo.uic

import kyo.*
import kyo.UI.*

/** Radio button — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * RadioButton anatomy: `div.p-radiobutton` > hidden native
  * `input.p-radiobutton-input` + visual `div.p-radiobutton-box` with the always
  * present `div.p-radiobutton-icon` dot that scales in when checked). The kyo
  * `<input type="radio">` provides `name` grouping, the change event, and the
  * disabled state. When `text` is set, the control plus a trailing text span are
  * wrapped in a `<label>` like [[CheckBox]]. Two-way binding mirrors [[CheckBox]].
  */
final case class RadioButton private (
    labelText: Maybe[String] = Absent,
    groupName: Maybe[String] = Absent,
    checkedBinding: Maybe[CheckBox.Checked] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Maybe[Boolean] = Absent,
    requiredFlag: Maybe[Boolean] = Absent,
    valueV: Maybe[String] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Boolean => Any < Async] = Absent,
    onBlurF: Maybe[Boolean => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, BooleanFormControl:
    type Self = RadioButton

    /** Native `id` on the radio input — pair with `Label.forId`; the form layer
      * stamps the bound field's id here so focus-first-invalid can target the input.
      */
    def id(v: String): RadioButton = copy(idV = Present(v))

    def text(v: String): RadioButton = copy(labelText = Present(v))

    /** Radio group name — all buttons sharing a `name` are mutually exclusive. */
    def name(v: String): RadioButton = copy(groupName = Present(v))

    /** Sets a constant checked state. */
    def checked(v: Boolean): RadioButton = copy(checkedBinding = Present(CheckBox.Checked.Const(v)))

    /** Binds two-way to `ref`: selection writes back into the ref, ref changes update the button. */
    def checked(ref: SignalRef[Boolean]): RadioButton = copy(checkedBinding = Present(CheckBox.Checked.Ref(ref)))

    def disabled(v: Boolean): RadioButton = copy(disabledFlag = Present(BoolValue.Const(v)))

    /** Reactive disabled — the native input's `disabled` attribute and the root's `.p-disabled` class
      * both toggle IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(sig: Signal[Boolean]): RadioButton = copy(disabledFlag = Present(BoolValue.Dyn(sig)))

    /** `readonly` — interaction is blocked (`aria-readonly`), but unlike
      * `disabled` the button keeps its normal look.
      */
    def readonly(v: Boolean): RadioButton = copy(readonlyFlag = Present(v))

    /** Marks the button required (`aria-required` + native constraint). */
    def required(v: Boolean): RadioButton = copy(requiredFlag = Present(v))

    /** Native `value` submitted with the form when selected. */
    def value(v: String): RadioButton = copy(valueV = Present(v))

    /** Size: `.p-radiobutton-sm` / default / `.p-radiobutton-lg`. */
    def size(v: Size): RadioButton = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): RadioButton = copy(variantV = v)

    /** Marks the button invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): RadioButton = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the control while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): RadioButton = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): RadioButton = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * button red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): RadioButton = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): RadioButton = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): RadioButton = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): RadioButton = copy(accNameRefV = Present(v))

    def onChange(f: Boolean => Any < Async): RadioButton = copy(onChangeF = Present(f))

    /** Fires on focus loss with the button's current checked state — unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => Any < Async): RadioButton = copy(onBlurF = Present(f))

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

        var box = radio.cssClass("p-radiobutton-input").checked(isChecked)
        box = idV.map(v => box.id(v)).getOrElse(box)
        box = groupName.map(box.name(_)).getOrElse(box)
        box = valueV.map(v => box.jsProp("value", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        if invalidV.constTrue then box = box.aria("invalid", "true")
        if isReadonly then box = box.aria("readonly", "true")
        box = accNameV match
            case Present(TextValue.Const(v)) => box.aria("label", v)
            case Present(TextValue.Dyn(s))   => box.aria("label", s)
            case Absent                      => box
        box = accNameRefV.map(v => box.aria("labelledby", v)).getOrElse(box)
        // readonly blocks interaction by disabling the native input; only `disabled`
        // also gets the `.p-disabled` visual treatment.
        if isReadonly then box = box.disabled(true)
        else box = disabledFlag.foldFlag(box)(box.disabled(_))
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

        // The dot icon is always present — Prime's CSS scales it 0.1 → 1 on checked.
        val boxDiv = div.cssClass("p-radiobutton-box")(toChild(div.cssClass("p-radiobutton-icon"): UI))

        var root = div.cssClass("p-radiobutton").cssClass("p-component")
        if isChecked then root = root.cssClass("p-radiobutton-checked")
        sizeV match
            case Size.Small  => root = root.cssClass("p-radiobutton-sm")
            case Size.Large  => root = root.cssClass("p-radiobutton-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then root = root.cssClass("p-variant-filled")
        if invalidV.constTrue then root = root.cssClass("p-invalid")
        root = disabledFlag.foldFlag(root)(root.cssClass("p-disabled", _))

        val control: UI = root(List[UI](box, boxDiv).map(toChild)*)
        val labelled: UI = labelText match
            case Present(t) => label.cssClass("p-uic-control-label")(List[UI](control, span(t)).map(toChild)*)
            case Absent     => control
        FieldInvalid.withMessage(labelled, invalidV.constTrue, invalidMsgV)
    end body
end RadioButton

object RadioButton:
    def apply(): RadioButton             = new RadioButton()
    def apply(text: String): RadioButton = new RadioButton(labelText = Present(text))
