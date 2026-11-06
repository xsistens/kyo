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
final class CheckBox private[uic] (private[uic] val p: CheckBox.Props) extends Node, BooleanFormControl, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = CheckBox

    /** Takes the box out of the tab order while it stays a real, announced checkbox a pointer can
      * still work. For a box that lives INSIDE a widget whose keyboard runs from somewhere else
      * ([[MultiSelect]]'s select-all header): the key it would receive there reaches that other
      * element too, by bubbling, and gets read a second time. Not public, because a checkbox
      * standing on its own must be reachable.
      */
    private[uic] def tabbable(v: Boolean): CheckBox = new CheckBox(p.copy(tabbableFlag = v))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new CheckBox(p.copy(accessV = v))

    def text(v: String): CheckBox = new CheckBox(p.copy(labelText = Present(v)))

    /** The checked state. A `SignalRef` binds TWO-WAY: a toggle writes it, and its changes update the box. Any other
      * signal, or a constant, binds ONE-WAY: the box shows it, and a toggle writes nothing and only reports through
      * [[onChange]], so the app writes its own state and the box follows. `ref.readOnly` opts a ref out. With no
      * [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): CheckBox = new CheckBox(p.copy(checkedSlot = checkedSlot.bind(v)))

    /** Disables the box. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the root's
      * `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): CheckBox = new CheckBox(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** `readonly`: the box reports `aria-readonly` and refuses to change, but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): CheckBox = new CheckBox(p.copy(readonlyFlag = Present(v)))

    /** Marks the box required (`aria-required` + native constraint). */
    def required(v: Boolean): CheckBox = new CheckBox(p.copy(requiredFlag = Present(v)))

    /** Purely informational: non-interactive (`.p-uic-display-only`, a kyo
      * extension; Prime has no displayOnly), without the disabled dimming.
      */
    def displayOnly(v: Boolean): CheckBox = new CheckBox(p.copy(displayOnlyFlag = Present(v)))

    /** Visual tri-state: `aria-checked="mixed"` + the minus icon in the box +
      * the native DOM `indeterminate` property (distinct from `checked`).
      */
    def indeterminate(v: Boolean): CheckBox = new CheckBox(p.copy(indeterminateFlag = Present(v)))

    /** Native `name` for HTML form participation. */
    def name(v: String): CheckBox = new CheckBox(p.copy(nameV = Present(v)))

    /** Native `value` submitted with the form when checked (browser default "on"). */
    def value(v: String): CheckBox = new CheckBox(p.copy(valueV = Present(v)))

    /** Size: `.p-checkbox-sm` / default / `.p-checkbox-lg`. */
    def size(v: Size): CheckBox = new CheckBox(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CheckBox = new CheckBox(p.copy(variantV = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new CheckBox(p.copy(validityV = v))

    def onChange(f: Boolean => UI.Action)(using Frame): CheckBox = new CheckBox(p.copy(checkedSlot = checkedSlot.hear(f)))

    /** Fires on focus loss with the box's current checked state. Unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): CheckBox = new CheckBox(p.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(validityV)
        // Unbound, the box keeps its own state, which starts off: a ref it owns for as long as it is shown.
        val field = validity.follow(Slot.owning(
            checkedSlot,
            start = false
        )(s => new CheckBox(this.p.copy(checkedSlot = s)).drawn(validity)))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        val inert = readonlyFlag.getOrElse(false) || displayOnlyFlag.getOrElse(false) || disabledFlag.constTrue
        Diagnostics.after(field)("CheckBox")(Diagnostic.unheard(checkedSlot, CheckBox.checkedName, on = !inert))
    end render

    /** The box over its binding. A toggle is wired only where it reaches somebody: a two-way binding, or onChange. Bound
      * one way, or to a constant somebody hears, the native box follows it, so kyo-ui takes back a toggle the owner does not
      * take (`UI.Bound.Follow`). A constant nobody hears is fixed: drawn read-only, reachable and
      * readable, with nothing to toggle.
      */
    private def drawn(validity: FieldValidity)(using Frame): UI =
        Slot.drawField(checkedSlot, unbound = false)(f =>
            (if f.fixed then new CheckBox(p.copy(readonlyFlag = Present(true))) else this).body(f.value, f.native, f.edit, validity)
        )

    private def body(
        isChecked: Boolean,
        native: Boolean | Signal[Boolean],
        onChg: Maybe[Boolean => UI.Action],
        validity: FieldValidity
    )(using
        Frame
    ): UI =
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

        var box = checkbox.cssClass("p-checkbox-input").checked(native)
        if !tabbableFlag then box = box.tabIndex(-1)
        box = accessV.idV.map(v => box.id(v)).getOrElse(box)
        if isMixed then box = box.indeterminate(true).aria("checked", "mixed")
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        box = valueV.map(v => box.jsProp("value", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        box = validity.markAria(box)
        if isReadonly then box = box.aria("readonly", "true")
        box = accessV.nameV match
            case Present(TextValue.Const(v)) => box.aria("label", v)
            case Present(TextValue.Dyn(s))   => box.aria("label", s)
            case Absent                      => box
        box = accessV.nameRefV.map(v => box.aria("labelledby", v)).getOrElse(box)
        if blocked then box = box.preventActivation
        box = disabledFlag.foldFlag(box)(box.disabled(_))
        if !blocked then box = onChg.map(box.onChange(_)).getOrElse(box)
        if !blocked then
            box = onBlurF match
                case Present(f) =>
                    box.onBlur(checkedSlot.current(false).map(f))
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
    final private[uic] case class Props(
        labelText: Maybe[String] = Absent,
        checkedSlot: Slot[Boolean] = Slot(),
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Maybe[Boolean] = Absent,
        requiredFlag: Maybe[Boolean] = Absent,
        displayOnlyFlag: Maybe[Boolean] = Absent,
        indeterminateFlag: Maybe[Boolean] = Absent,
        nameV: Maybe[String] = Absent,
        valueV: Maybe[String] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        accessV: Accessibility = Accessibility.none,
        onBlurF: Maybe[Boolean => UI.Action] = Absent,
        tabbableFlag: Boolean = true
    )

    private val checkedName = Slot.Name("CheckBox", "checked", "a change", "onChange")

    def apply(): CheckBox             = new CheckBox(Props())
    def apply(text: String): CheckBox = new CheckBox(Props(labelText = Present(text)))
end CheckBox
