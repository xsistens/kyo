package kyo.uic

import kyo.*
import kyo.UI.*

/** Radio button: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * RadioButton anatomy: `div.p-radiobutton` > hidden native
  * `input.p-radiobutton-input` + visual `div.p-radiobutton-box` with the always
  * present `div.p-radiobutton-icon` dot that scales in when checked). The kyo
  * `<input type="radio">` provides `name` grouping, the change event, and the
  * disabled state. When `text` is set, the control plus a trailing text span are
  * wrapped in a `<label>` like [[CheckBox]]. The `checked` binding mirrors [[CheckBox]].
  */
final class RadioButton private[uic] (private[uic] val p: RadioButton.Props) extends Node, FormControl[Boolean], HasAccessibleNameRef,
      HasProps:
    import p.*

    type Self = RadioButton

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new RadioButton(p.copy(access = v))

    def text(v: String): RadioButton = new RadioButton(p.copy(labelText = Present(v)))

    /** Radio group name: all buttons sharing a `name` are mutually exclusive. */
    def name(v: String): RadioButton = new RadioButton(p.copy(groupName = Present(v)))

    /** The checked state. A `SignalRef` binds TWO-WAY: a selection writes it, and its changes update the button. Any other
      * signal, or a constant, binds ONE-WAY: the button shows it, and a selection writes nothing and only reports through
      * [[onChange]], so the app writes its own state and the button follows. `ref.readOnly` opts a ref out. With no
      * [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): RadioButton = new RadioButton(p.copy(checkedSlot = checkedSlot.bind(v)))

    /** Disables the button. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the
      * root's `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): RadioButton = new RadioButton(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** `readonly`: the button refuses to be selected, but unlike `disabled` it keeps its normal look AND its place in
      * the tab order: a reader can land on it and read its state, and every key and click that would select it runs
      * into nothing. A radio has no readonly state, so it says so with `aria-disabled`.
      */
    def readonly(v: Boolean): RadioButton = new RadioButton(p.copy(readonlyFlag = Present(v)))

    /** Marks the button required (`aria-required` + native constraint). */
    def required(v: Boolean): RadioButton = new RadioButton(p.copy(requiredFlag = Present(v)))

    /** Native `value` submitted with the form when selected. */
    def value(v: String): RadioButton = new RadioButton(p.copy(valueV = Present(v)))

    /** Size: `.p-radiobutton-sm` / default / `.p-radiobutton-lg`. */
    def size(v: Size): RadioButton = new RadioButton(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): RadioButton = new RadioButton(p.copy(variantV = v))

    private[uic] def validitySlots: FieldValidity.Slots                          = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self                  = new RadioButton(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Boolean])(using Frame): RadioButton  = checked(ref)
    private[uic] def hearBlur(f: Boolean => UI.Action)(using Frame): RadioButton = onBlur(f)

    def onChange(f: Boolean => UI.Action)(using Frame): RadioButton = new RadioButton(p.copy(checkedSlot = checkedSlot.hear(f)))

    /** Fires on focus loss with the button's current checked state; unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): RadioButton = new RadioButton(p.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(validityV)
        // One instance per place: unbound, the box keeps its own state, which starts off, in a ref the instance owns, and a host
        // region that runs again hands the instance the run's props instead of starting it over.
        val field = validity.follow(Controlled.instance(p)(q => new RadioButton(q).drawn(validity)))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        val inert = readonlyFlag.getOrElse(false) || disabledFlag.constTrue
        Diagnostics.after(field)("RadioButton")(Diagnostic.unheard(checkedSlot, RadioButton.checkedName, on = !inert))
    end render

    /** The box over its binding. A toggle is wired only where it reaches somebody: a two-way binding, or onChange. Bound,
      * the native box shows the binding, and kyo-ui takes back a toggle the owner does not take (see [[kyo.UI.Bound]]). A constant nobody hears is fixed: drawn read-only, reachable and
      * readable, with nothing to toggle.
      */
    private def drawn(validity: FieldValidity)(using Frame): UI =
        Controlled.drawField(checkedSlot, unbound = false)(f =>
            (if f.fixed then new RadioButton(p.copy(readonlyFlag = Present(true))) else this).body(f.value, f.native, f.edit, validity)
        )

    private def body(
        isChecked: Boolean,
        native: Boolean | Signal[Boolean],
        onChg: Maybe[Boolean => UI.Action],
        validity: FieldValidity
    )(using
        Frame
    ): UI =
        val isReadonly = readonlyFlag.getOrElse(false)

        var box = radio.cssClass("p-radiobutton-input").checked(native)
        box = access.id.map(v => box.id(v)).getOrElse(box)
        box = groupName.map(box.name(_)).getOrElse(box)
        box = valueV.map(v => box.jsProp("value", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria(Aria.required -> "true")
        box = validity.markAria(box)
        if isReadonly then box = box.aria(Aria.disabled -> "true")
        box = access.name match
            case Present(v) => box.aria(Aria.label -> v)
            case _          => box
        box = access.nameRef.map(v => box.aria(Aria.labelledby -> v)).getOrElse(box)
        // readonly keeps the radio FOCUSABLE and refuses to select it: the client declines the
        // browser's own selection (`preventActivation`) and the handler is not wired. Disabling the
        // native input instead took it out of the tab order (see CheckBox). Only `disabled` is
        // native, and only `disabled` gets the `.p-disabled` visual treatment.
        if isReadonly then box = box.preventActivation
        box = disabledFlag.foldFlag(box)(box.disabled(_))
        if !isReadonly then box = onChg.map(box.onChange(_)).getOrElse(box)
        if !isReadonly then
            box = onBlurF match
                case Present(f) =>
                    box.onBlur(checkedSlot.current(false).map(f))
                case Absent => box
        end if

        // The dot icon is always present; Prime's CSS scales it 0.1 → 1 on checked.
        val boxDiv = div.cssClass("p-radiobutton-box")(div.cssClass("p-radiobutton-icon"): UI)

        var root = div.cssClass("p-radiobutton").cssClass("p-component")
        if isChecked then root = root.cssClass("p-radiobutton-checked")
        sizeV match
            case Size.Small  => root = root.cssClass("p-radiobutton-sm")
            case Size.Large  => root = root.cssClass("p-radiobutton-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then root = root.cssClass("p-variant-filled")
        root = validity.markClass(root)
        root = disabledFlag.foldFlag(root)(on => root.when(on)(_.cssClass("p-disabled")))

        val control: UI = root(UI.children(List[UI](box, boxDiv))*)
        labelText match
            case Present(t) => label.cssClass("p-uic-control-label")(UI.children(List[UI](control, span(t)))*)
            case Absent     => control
    end body
end RadioButton

object RadioButton:
    final private[uic] case class Props(
        labelText: Maybe[String] = Absent,
        groupName: Maybe[String] = Absent,
        checkedSlot: Controlled[Boolean] = Controlled(start = Present(false)),
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Maybe[Boolean] = Absent,
        requiredFlag: Maybe[Boolean] = Absent,
        valueV: Maybe[String] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onBlurF: Maybe[Boolean => UI.Action] = Absent
    ) derives CanEqual

    private val checkedName = Controlled.Name("RadioButton", "checked", "a change", "onChange")

    def apply(): RadioButton             = new RadioButton(Props())
    def apply(text: String): RadioButton = new RadioButton(Props(labelText = Present(text)))
end RadioButton
