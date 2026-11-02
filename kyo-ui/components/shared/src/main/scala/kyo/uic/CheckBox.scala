package kyo.uic

import kyo.*
import kyo.UI.*

/** Checkbox: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Checkbox anatomy: `div.p-checkbox` > hidden native `input.p-checkbox-input`
  * + visual `div.p-checkbox-box` with the check/minus SVG icon). The kyo
  * `<input type="checkbox">` provides the real boolean input, so the change
  * event and disabled state are inherited, not rebuilt. When `text` is set, the
  * control plus a trailing text span are wrapped in a `<label>` so clicking the
  * text toggles the box natively.
  *
  * `checked` takes the union every value slot in the module takes: a `SignalRef` binds two-way (a toggle writes it,
  * then `onChange` hears the new state), any other signal or a constant binds one way (the box shows it, and a toggle
  * only reports to `onChange`). The checked class and the native `checked` attribute follow the binding either way.
  */
final case class CheckBox private (
    labelText: Maybe[String] = Absent,
    checkedBinding: Maybe[BoolValue] = Absent,
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
    onChangeF: Maybe[Boolean => UI.Action] = Absent,
    onBlurF: Maybe[Boolean => UI.Action] = Absent,
    idV: Maybe[String] = Absent,
    tabbableFlag: Boolean = true
) extends Node, BooleanFormControl, HasAccessibleNameRef:
    type Self = CheckBox

    /** Takes the box out of the tab order while it stays a real, announced checkbox a pointer can
      * still work. For a box that lives INSIDE a widget whose keyboard runs from somewhere else
      * ([[MultiSelect]]'s select-all header): the key it would receive there reaches that other
      * element too, by bubbling, and gets read a second time. Not public, because a checkbox
      * standing on its own must be reachable.
      */
    private[uic] def tabbable(v: Boolean): CheckBox = copy(tabbableFlag = v)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): CheckBox = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    def text(v: String): CheckBox = copy(labelText = Present(v))

    /** The checked state. A `SignalRef` binds TWO-WAY: a toggle writes it, and its changes update the box. Any other
      * signal, or a constant, binds ONE-WAY: the box shows it, and a toggle writes nothing and only reports through
      * [[onChange]], so the app writes its own state and the box follows. `ref.readOnly` opts a ref out. With no
      * [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): CheckBox = copy(checkedBinding = Present(ReactiveValue(v)))

    /** Disables the box. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the root's
      * `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): CheckBox = copy(disabledFlag = Present(ReactiveValue(v)))

    /** `readonly`: the box reports `aria-readonly` and refuses to change, but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): CheckBox = copy(readonlyFlag = Present(v))

    /** Marks the box required (`aria-required` + native constraint). */
    def required(v: Boolean): CheckBox = copy(requiredFlag = Present(v))

    /** Purely informational: non-interactive (`.p-uic-display-only`, a kyo
      * extension; Prime has no displayOnly), without the disabled dimming.
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

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CheckBox = copy(variantV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): CheckBox                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): CheckBox                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): CheckBox = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): CheckBox = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): CheckBox = copy(accNameRefV = v)

    def onChange(f: Boolean => UI.Action)(using Frame): CheckBox = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss with the box's current checked state. Unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): CheckBox = copy(onBlurF = Present(Callback.of(f)))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val field    = validity.follow(checkedBinding match
            // Unbound, the box keeps its own state, which starts off: a ref it owns for as long as it is shown.
            case Absent =>
                ReactiveValue.owned(false)(own => copy(checkedBinding = Present(own)).drawn(validity))(drawn(validity))
            case _ => drawn(validity))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        val inert = readonlyFlag.getOrElse(false) || displayOnlyFlag.getOrElse(false) || disabledFlag.constTrue
        val deaf  = checkedBinding.exists(b => b.dyn.isDefined && b.writable.isEmpty) && !ReactiveValue.heard(checkedBinding, onChangeF)
        if deaf && !inert then fragment(field, KeyDiagnostics.unheard("CheckBox", "checked", "onChange")) else field
    end render

    /** The box over its binding. A toggle is wired only where it reaches somebody: a two-way binding, or onChange. Bound
      * one way, or to a constant somebody hears, it paints the box again after the report, so a toggle the owner does not
      * take is taken back (ReactiveValue.repainting). A constant nobody hears is fixed: drawn read-only, reachable and
      * readable, with nothing to toggle.
      */
    private def drawn(validity: FieldValidity)(using Frame): UI =
        def toggle(repaint: UI.Action): Maybe[Boolean => UI.Action] =
            if ReactiveValue.heard(checkedBinding, onChangeF) then
                Present(b => ReactiveValue.change(checkedBinding, b, onChangeF).andThen(repaint))
            else Absent
        checkedBinding match
            case Present(ReactiveVariable(ref))  => ref.render(b => body(b, toggle(()), validity))
            case Present(ReactiveValue.Dyn(sig)) => ReactiveValue.repainting(sig)((b, repaint) => body(b, toggle(repaint), validity))
            case Present(ReactiveValue.Const(v)) if ReactiveValue.heard(checkedBinding, onChangeF) =>
                ReactiveValue.repainting(Signal.initConst(v))((b, repaint) => body(b, toggle(repaint), validity))
            case Present(ReactiveValue.Const(v)) => copy(readonlyFlag = Present(true)).body(v, Absent, validity)
            case Absent                          => body(false, toggle(()), validity)
        end match
    end drawn

    private def body(isChecked: Boolean, onChg: Maybe[Boolean => UI.Action], validity: FieldValidity)(using Frame): UI =
        val isReadonly    = readonlyFlag.getOrElse(false)
        val isDisplayOnly = displayOnlyFlag.getOrElse(false)
        val isMixed       = indeterminateFlag.getOrElse(false)
        // readonly and displayOnly keep the box FOCUSABLE and refuse to change: the client
        // declines the browser's own toggle (`preventActivation`) and the handler is simply not
        // wired. Disabling the native input instead took the box out of the tab order, which made
        // readonly indistinguishable from disabled to a keyboard and to a screen reader, and left
        // its `aria-readonly` on an element already reported as disabled. Only `disabled` is
        // native, and only `disabled` gets the `.p-disabled` visual treatment.
        val blocked = isReadonly || isDisplayOnly

        var box = checkbox.cssClass("p-checkbox-input").checked(isChecked)
        if !tabbableFlag then box = box.tabIndex(-1)
        box = idV.map(v => box.id(v)).getOrElse(box)
        if isMixed then box = box.indeterminate(true).aria("checked", "mixed")
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        box = valueV.map(v => box.jsProp("value", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        box = validity.markAria(box)
        if isReadonly then box = box.aria("readonly", "true")
        box = accNameV match
            case Present(TextValue.Const(v)) => box.aria("label", v)
            case Present(TextValue.Dyn(s))   => box.aria("label", s)
            case Absent                      => box
        box = accNameRefV.map(v => box.aria("labelledby", v)).getOrElse(box)
        if blocked then box = box.preventActivation
        box = disabledFlag.foldFlag(box)(box.disabled(_))
        if !blocked then box = onChg.map(box.onChange(_)).getOrElse(box)
        if !blocked then
            box = onBlurF match
                case Present(f) =>
                    box.onBlur(checkedBinding.fold(f(false))(_.current.map(f)))
                case Absent => box
        end if

        // Prime treats indeterminate as visually unchecked: the minus icon on the
        // neutral box wins over the check icon and the checked coloring.
        val icon: List[UI] =
            if isMixed then List(GlyphSvg(Icons.minus, "p-checkbox-icon"))
            else if isChecked then List(GlyphSvg(Icons.check, "p-checkbox-icon"))
            else Nil
        val boxDiv = div.cssClass("p-checkbox-box")(UI.children(icon)*)

        var root = div.cssClass("p-checkbox").cssClass("p-component")
        if isChecked && !isMixed then root = root.cssClass("p-checkbox-checked")
        sizeV match
            case Size.Small  => root = root.cssClass("p-checkbox-sm")
            case Size.Large  => root = root.cssClass("p-checkbox-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then root = root.cssClass("p-variant-filled")
        root = validity.markClass(root)
        root = disabledFlag.foldFlag(root)(root.cssClass("p-disabled", _))
        if isDisplayOnly then root = root.cssClass("p-uic-display-only")

        val control: UI = root(UI.children(List[UI](box, boxDiv))*)
        labelText match
            case Present(t) => label.cssClass("p-uic-control-label")(UI.children(List[UI](control, span(t)))*)
            case Absent     => control
    end body
end CheckBox

object CheckBox:
    def apply(): CheckBox             = new CheckBox()
    def apply(text: String): CheckBox = new CheckBox(labelText = Present(text))
end CheckBox
