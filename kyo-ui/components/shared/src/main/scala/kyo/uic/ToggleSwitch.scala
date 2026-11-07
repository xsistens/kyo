package kyo.uic

import kyo.*
import kyo.UI.*

/** On/off toggle: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ToggleSwitch anatomy: `div.p-toggleswitch` > hidden native
  * `input.p-toggleswitch-input[role=switch]` + visual `div.p-toggleswitch-slider`
  * with the sliding `div.p-toggleswitch-handle`). The kyo
  * `<input type="checkbox">` provides the real boolean state, so the change
  * event and disabled state are inherited. Prime has no on/off state text;
  * place a plain `span` next to the switch instead. `handleIcon` fills Prime's
  * handle slot with a per-state glyph riding inside the sliding handle. The `checked`
  * binding mirrors [[CheckBox]].
  */
final class ToggleSwitch private[uic] (private[uic] val p: ToggleSwitch.Props) extends Node, BooleanFormControl, HasTooltip,
      HasAccessibleNameRef, HasProps:
    import p.*

    type Self = ToggleSwitch

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new ToggleSwitch(p.copy(access = v))

    /** The checked state. A `SignalRef` binds TWO-WAY: a toggle writes it, and its changes update the switch. Any other
      * signal, or a constant, binds ONE-WAY: the switch shows it, and a toggle writes nothing and only reports through
      * [[onChange]], so the app writes its own state and the switch follows. `ref.readOnly` opts a ref out. With no
      * [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): ToggleSwitch = new ToggleSwitch(p.copy(checkedSlot = checkedSlot.bind(v)))

    /** Disables the switch. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the
      * root's `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): ToggleSwitch = new ToggleSwitch(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** `readonly`: the switch reports `aria-readonly` and refuses to change, but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): ToggleSwitch = new ToggleSwitch(p.copy(readonlyFlag = Present(v)))

    /** Marks the switch required (`aria-required` + native constraint). */
    def required(v: Boolean): ToggleSwitch = new ToggleSwitch(p.copy(requiredFlag = Present(v)))

    /** Native `name` for HTML form participation. */
    def name(v: String): ToggleSwitch = new ToggleSwitch(p.copy(nameV = Present(v)))

    private[uic] def validitySlots: FieldValidity.Slots             = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self     = new ToggleSwitch(p.copy(validityV = v))
    private[uic] def withTooltip(v: Maybe[TextValue]): ToggleSwitch = new ToggleSwitch(p.copy(tooltipV = v))

    /** Prime's handle slot: a glyph rendered inside the sliding
      * `.p-toggleswitch-handle`, `checked` while on, `unchecked` while off.
      */
    def handleIcon(checked: IconGlyph, unchecked: IconGlyph): ToggleSwitch =
        new ToggleSwitch(p.copy(handleIconsV = Present((checked, unchecked))))

    def onChange(f: Boolean => UI.Action)(using Frame): ToggleSwitch = new ToggleSwitch(p.copy(checkedSlot = checkedSlot.hear(f)))

    /** Fires on focus loss with the switch's current state. Unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): ToggleSwitch = new ToggleSwitch(p.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(validityV)
        // Unbound, the box keeps its own state, which starts off: a ref it owns for as long as it is shown.
        val field = validity.follow(Slot.owning(
            checkedSlot,
            start = false
        )(s => new ToggleSwitch(this.p.copy(checkedSlot = s)).drawn(validity)))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        val inert = readonlyFlag.getOrElse(false) || disabledFlag.constTrue
        Diagnostics.after(field)("ToggleSwitch")(Diagnostic.unheard(checkedSlot, ToggleSwitch.checkedName, on = !inert))
    end render

    /** The box over its binding. A toggle is wired only where it reaches somebody: a two-way binding, or onChange. Bound
      * one way, or to a constant somebody hears, the native box follows it, so kyo-ui takes back a toggle the owner does not
      * take (`UI.Bound.Follow`). A constant nobody hears is fixed: drawn read-only, reachable and
      * readable, with nothing to toggle.
      */
    private def drawn(validity: FieldValidity)(using Frame): UI =
        Slot.drawField(checkedSlot, unbound = false)(f =>
            (if f.fixed then new ToggleSwitch(p.copy(readonlyFlag = Present(true))) else this).body(f.value, f.native, f.edit, validity)
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

        var box = checkbox
            .cssClass("p-toggleswitch-input")
            .role("switch")
            .checked(native)
            .aria("checked", isChecked.toString)
        box = access.id.map(v => box.id(v)).getOrElse(box)
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        box = validity.markAria(box)
        if isReadonly then box = box.aria("readonly", "true")
        box = access.name match
            case Present(v) => box.aria("label", v)
            case _          => box
        box = access.nameRef.map(v => box.aria("labelledby", v)).getOrElse(box)
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
                    box.onBlur(checkedSlot.current(false).map(f))
                case Absent => box
        end if

        // Prime's handle slot: the current-state glyph rides inside the handle
        // (the `.p-uic-*` remainder sizes it below the handle box).
        val handleContent: List[UI] = handleIconsV.toList.map { (on, off) =>
            GlyphSvg(if isChecked then on else off, "p-uic-toggleswitch-handle-icon")
        }
        val slider = div.cssClass("p-toggleswitch-slider")(
            div.cssClass("p-toggleswitch-handle")(UI.children(handleContent)*): UI
        )

        var root = div.cssClass("p-toggleswitch").cssClass("p-component")
        if isChecked then root = root.cssClass("p-toggleswitch-checked")
        root = disabledFlag.foldFlag(root)(root.cssClass("p-disabled", _))
        root = validity.markClass(root)
        root = tooltipV match
            case Present(TextValue.Const(v)) => root.jsProp("title", v)
            case Present(TextValue.Dyn(s))   => root.title(s)
            case Absent                      => root

        root(UI.children(List[UI](box, slider))*)
    end body
end ToggleSwitch

object ToggleSwitch:
    final private[uic] case class Props(
        checkedSlot: Slot[Boolean] = Slot(),
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Maybe[Boolean] = Absent,
        requiredFlag: Maybe[Boolean] = Absent,
        nameV: Maybe[String] = Absent,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        tooltipV: Maybe[TextValue] = Absent,
        handleIconsV: Maybe[(IconGlyph, IconGlyph)] = Absent,
        access: Accessibility = Accessibility.none,
        onBlurF: Maybe[Boolean => UI.Action] = Absent
    )

    private val checkedName = Slot.Name("ToggleSwitch", "checked", "a change", "onChange")

    def apply(): ToggleSwitch = new ToggleSwitch(Props())
end ToggleSwitch
