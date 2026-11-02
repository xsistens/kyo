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
final case class ToggleSwitch private (
    checkedBinding: Maybe[BoolValue] = Absent,
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
    onChangeF: Maybe[Boolean => UI.Action] = Absent,
    onBlurF: Maybe[Boolean => UI.Action] = Absent,
    idV: Maybe[String] = Absent
) extends Node, BooleanFormControl, HasTooltip, HasAccessibleNameRef:
    type Self = ToggleSwitch

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): ToggleSwitch = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** The checked state. A `SignalRef` binds TWO-WAY: a toggle writes it, and its changes update the switch. Any other
      * signal, or a constant, binds ONE-WAY: the switch shows it, and a toggle writes nothing and only reports through
      * [[onChange]], so the app writes its own state and the switch follows. `ref.readOnly` opts a ref out. With no
      * [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): ToggleSwitch = copy(checkedBinding = Present(ReactiveValue(v)))

    /** Disables the switch. A `Signal[Boolean]` toggles the native input's `disabled` attribute and the
      * root's `.p-disabled` class IN PLACE via the boolean-attribute and class channels (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): ToggleSwitch = copy(disabledFlag = Present(ReactiveValue(v)))

    /** `readonly`: the switch reports `aria-readonly` and refuses to change, but
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
      * `.p-toggleswitch-handle`, `checked` while on, `unchecked` while off.
      */
    def handleIcon(checked: IconGlyph, unchecked: IconGlyph): ToggleSwitch =
        copy(handleIconsV = Present((checked, unchecked)))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): ToggleSwitch = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): ToggleSwitch = copy(accNameRefV = v)

    def onChange(f: Boolean => UI.Action)(using Frame): ToggleSwitch = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss with the switch's current state. Unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): ToggleSwitch = copy(onBlurF = Present(Callback.of(f)))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val field    = validity.follow(checkedBinding match
            // Unbound, the box keeps its own state, which starts off: a ref it owns for as long as it is shown.
            case Absent =>
                ReactiveValue.owned(false)(own => copy(checkedBinding = Present(own)).drawn(validity))(drawn(validity))
            case _ => drawn(validity))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        val inert = readonlyFlag.getOrElse(false) || disabledFlag.constTrue
        val deaf  = checkedBinding.exists(b => b.dyn.isDefined && b.writable.isEmpty) && !ReactiveValue.heard(checkedBinding, onChangeF)
        if deaf && !inert then fragment(field, KeyDiagnostics.unheard("ToggleSwitch", "checked", "onChange")) else field
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
        val isReadonly = readonlyFlag.getOrElse(false)

        var box = checkbox
            .cssClass("p-toggleswitch-input")
            .role("switch")
            .checked(isChecked)
            .aria("checked", isChecked.toString)
        box = idV.map(v => box.id(v)).getOrElse(box)
        box = nameV.map(v => box.jsProp("name", v)).getOrElse(box)
        if requiredFlag.getOrElse(false) then box = box.jsProp("required", "true").aria("required", "true")
        box = validity.markAria(box)
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
                    box.onBlur(checkedBinding.fold(f(false))(_.current.map(f)))
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
    def apply(): ToggleSwitch = new ToggleSwitch()
