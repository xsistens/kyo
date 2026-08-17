package kyo.uic

import kyo.*
import kyo.UI.*

/** Checkbox — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Checkbox anatomy: `div.p-checkbox` > hidden native `input.p-checkbox-input`
  * + visual `div.p-checkbox-box` with the check/minus SVG icon). The kyo
  * `<input type="checkbox">` provides the real boolean input, so the change
  * event and disabled state are inherited, not rebuilt. When `text` is set, the
  * control plus a trailing text span are wrapped in a `<label>` so clicking the
  * text toggles the box natively.
  *
  * Two-way binding follows Input's const-or-ref carrier idea: `checked(SignalRef)`
  * renders reactively so the checked class and the native `checked` attribute both
  * track the current value, and edits write back into the ref before firing the
  * user `onChange`.
  */
final case class CheckBox private (
    labelText: Maybe[String] = Absent,
    checkedBinding: Maybe[CheckBox.Checked] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Maybe[Boolean] = Absent,
    requiredFlag: Maybe[Boolean] = Absent,
    displayOnlyFlag: Maybe[Boolean] = Absent,
    indeterminateFlag: Maybe[Boolean] = Absent,
    nameV: Maybe[String] = Absent,
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
    type Self = CheckBox

    /** Native `id` on the checkbox input — pair with `Label.forId`; the form layer
      * stamps the bound field's id here so focus-first-invalid can target the box.
      */
    def id(v: String): CheckBox = copy(idV = Present(v))

    def text(v: String): CheckBox = copy(labelText = Present(v))

    /** Sets a constant checked state. */
    def checked(v: Boolean): CheckBox = copy(checkedBinding = Present(CheckBox.Checked.Const(v)))

    /** Binds two-way to `ref`: toggles write back into the ref, ref changes update the box. */
    def checked(ref: SignalRef[Boolean]): CheckBox = copy(checkedBinding = Present(CheckBox.Checked.Ref(ref)))

    def disabled(v: Boolean): CheckBox = copy(disabledFlag = Present(BoolValue.Const(v)))

    /** Reactive disabled — the native input's `disabled` attribute and the root's `.p-disabled` class
      * both toggle IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(sig: Signal[Boolean]): CheckBox = copy(disabledFlag = Present(BoolValue.Dyn(sig)))

    /** `readonly` — interaction is blocked (`aria-readonly`), but unlike
      * `disabled` the box keeps its normal look.
      */
    def readonly(v: Boolean): CheckBox = copy(readonlyFlag = Present(v))

    /** Marks the box required (`aria-required` + native constraint). */
    def required(v: Boolean): CheckBox = copy(requiredFlag = Present(v))

    /** Purely informational: non-interactive (`.p-uic-display-only` — kyo
      * extension, Prime has no displayOnly), without the disabled dimming.
      */
    def displayOnly(v: Boolean): CheckBox = copy(displayOnlyFlag = Present(v))

    /** Visual tri-state: `aria-checked="mixed"` + the minus icon in the box +
      * the native DOM `indeterminate` property (distinct from `checked`).
      */
    def indeterminate(v: Boolean): CheckBox = copy(indeterminateFlag = Present(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): CheckBox = copy(nameV = Present(v))

    /** Native `value` submitted with the form when checked (browser default "on"). */
    def value(v: String): CheckBox = copy(valueV = Present(v))

    /** Size: `.p-checkbox-sm` / default / `.p-checkbox-lg`. */
    def size(v: Size): CheckBox = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CheckBox = copy(variantV = v)

    /** Marks the box invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): CheckBox = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the control while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): CheckBox = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): CheckBox = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * box red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): CheckBox = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): CheckBox = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): CheckBox = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): CheckBox = copy(accNameRefV = Present(v))

    def onChange(f: Boolean => Any < Async): CheckBox = copy(onChangeF = Present(f))

    /** Fires on focus loss with the box's current checked state — unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => Any < Async): CheckBox = copy(onBlurF = Present(f))

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

    /** Two-way handler: write the new value into `ref`, then run the user callback. */
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
        val isReadonly    = readonlyFlag.getOrElse(false)
        val isDisplayOnly = displayOnlyFlag.getOrElse(false)
        val isMixed       = indeterminateFlag.getOrElse(false)
        // readonly/displayOnly block interaction by disabling the native input; only
        // `disabled` also gets the `.p-disabled` visual treatment.
        val blocked = isReadonly || isDisplayOnly

        var box = checkbox.cssClass("p-checkbox-input").checked(isChecked)
        box = idV.map(v => box.id(v)).getOrElse(box)
        if isMixed then box = box.indeterminate(true).aria("checked", "mixed")
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        box = valueV.map(v => box.jsProp("value", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        if invalidV.constTrue then box = box.aria("invalid", "true")
        if isReadonly then box = box.aria("readonly", "true")
        box = accNameV match
            case Present(TextValue.Const(v)) => box.aria("label", v)
            case Present(TextValue.Dyn(s))   => box.aria("label", s)
            case Absent                      => box
        box = accNameRefV.map(v => box.aria("labelledby", v)).getOrElse(box)
        if blocked then box = box.disabled(true)
        else box = disabledFlag.foldFlag(box)(box.disabled(_))
        if !blocked then box = onChg.map(box.onChange(_)).getOrElse(box)
        if !blocked then
            box = onBlurF match
                case Present(f) =>
                    box.onBlur(checkedBinding match
                        case Present(CheckBox.Checked.Ref(r))   => r.use(f)
                        case Present(CheckBox.Checked.Const(v)) => f(v)
                        case Absent                             => f(false))
                case Absent => box
        end if

        // Prime treats indeterminate as visually unchecked: the minus icon on the
        // neutral box wins over the check icon and the checked coloring.
        val icon: List[UI] =
            if isMixed then List(GlyphSvg(Icons.minus, "p-checkbox-icon"))
            else if isChecked then List(GlyphSvg(Icons.check, "p-checkbox-icon"))
            else Nil
        val boxDiv = div.cssClass("p-checkbox-box")(icon.map(toChild)*)

        var root = div.cssClass("p-checkbox").cssClass("p-component")
        if isChecked && !isMixed then root = root.cssClass("p-checkbox-checked")
        sizeV match
            case Size.Small  => root = root.cssClass("p-checkbox-sm")
            case Size.Large  => root = root.cssClass("p-checkbox-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then root = root.cssClass("p-variant-filled")
        if invalidV.constTrue then root = root.cssClass("p-invalid")
        root = disabledFlag.foldFlag(root)(root.cssClass("p-disabled", _))
        if isDisplayOnly then root = root.cssClass("p-uic-display-only")

        val control: UI = root(List[UI](box, boxDiv).map(toChild)*)
        val labelled: UI = labelText match
            case Present(t) => label.cssClass("p-uic-control-label")(List[UI](control, span(t)).map(toChild)*)
            case Absent     => control
        FieldInvalid.withMessage(labelled, invalidV.constTrue, invalidMsgV)
    end body
end CheckBox

object CheckBox:
    def apply(): CheckBox             = new CheckBox()
    def apply(text: String): CheckBox = new CheckBox(labelText = Present(text))

    /** Const-or-ref carrier for the pending `checked` state — the boolean analogue of
      * `Input.Value`. `Ref` establishes two-way binding at render time.
      */
    private[uic] enum Checked:
        case Const(v: Boolean)
        case Ref(ref: SignalRef[Boolean])
end CheckBox
