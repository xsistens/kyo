package kyo.uic

import kyo.*
import kyo.UI.*

/** ToggleButton: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ToggleButton anatomy: `button.p-togglebutton.p-component
  * [.p-togglebutton-checked][.p-invalid][.p-togglebutton-sm|-lg]` >
  * `span.p-togglebutton-content` > [icon `.p-togglebutton-icon`] +
  * `span.p-togglebutton-label`), so the extracted `@primeuix` togglebutton CSS
  * applies verbatim.
  *
  * The label follows Prime's semantics: `onLabel`/`offLabel` default to
  * "Yes"/"No"; when BOTH are cleared the label span renders a non-breaking
  * space so the content box keeps its height (Prime renders `\u00A0`).
  * `checked` binds exactly like CheckBox: a `SignalRef` two-way (a click writes the
  * toggled value back before firing `onChange`), anything else one way. It is a
  * [[BooleanFormControl]] for the same reason, so `uic.ToggleButton().bind(field)`
  * wires value, validity, message and the blur trigger in one call.
  */
final class ToggleButton private[uic] (private[uic] val s: ToggleButton.State) extends Node, BooleanFormControl, HasAccessibleNameRef,
      Stated:
    import s.*

    type Self = ToggleButton

    /** The wiring [[SelectButton]] applies to one option of a SINGLE-select group.
      *
      * A choice of one out of several is a radio group, whatever it is built from, and that is
      * what this button then reports: `role="radio"` with `aria-checked` rather than a toggle
      * button's `aria-pressed`, one tab stop for the whole group, and the group's own key handler
      * to move within it. Package-internal, because a toggle button standing on its own is a
      * toggle button and has none of this.
      */
    private[uic] def asRadioOption(tabbable: Boolean, onKey: KeyboardEvent => UI.Action): ToggleButton =
        new ToggleButton(s.copy(radioV = Present(ToggleButton.RadioOption(tabbable, onKey))))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new ToggleButton(s.copy(accessV = v))

    /** Package-internal content slot: SelectButton's `itemTemplate` replaces the
      * icon+label pair inside `.p-togglebutton-content` with arbitrary UI (the
      * kyo analogue of Prime's default slot).
      */
    private[uic] def content(ui: UI): ToggleButton = new ToggleButton(s.copy(contentV = Present(ui)))

    /** The checked state. A `SignalRef` binds TWO-WAY: a click writes the toggled value, and its changes re-render.
      * Any other signal, or a constant, binds ONE-WAY: the button shows it, and a click writes nothing and only
      * reports through [[onChange]], so the app writes its own state and the button follows. `ref.readOnly` opts a
      * ref out. With no [[onChange]], a card says the reader cannot change it.
      */
    def checked(v: Boolean | Signal[Boolean]): ToggleButton = new ToggleButton(s.copy(checkedSlot = checkedSlot.bind(v)))

    /** Label while checked (Prime default "Yes"). A `Signal[String]` re-renders it in place on emission.
      */
    def onLabel(v: String | Signal[String]): ToggleButton = new ToggleButton(s.copy(onLabelV = Present(ReactiveValue(v))))

    /** Label while unchecked (Prime default "No"). A `Signal[String]` re-renders it in place on emission.
      */
    def offLabel(v: String | Signal[String]): ToggleButton = new ToggleButton(s.copy(offLabelV = Present(ReactiveValue(v))))

    /** Drops both labels: the label span renders a non-breaking space (icon-only look). */
    def noLabels: ToggleButton = new ToggleButton(s.copy(onLabelV = Absent, offLabelV = Absent))

    /** Icon while checked (`.p-togglebutton-icon`). */
    def onIcon(glyph: IconGlyph): ToggleButton = new ToggleButton(s.copy(onIconV = Present(glyph)))

    /** Icon while unchecked (`.p-togglebutton-icon`). */
    def offIcon(glyph: IconGlyph): ToggleButton = new ToggleButton(s.copy(offIconV = Present(glyph)))

    /** Size: `.p-togglebutton-sm` / default / `.p-togglebutton-lg`. */
    def size(v: Size): ToggleButton = new ToggleButton(s.copy(sizeV = v))

    /** Spans the full width of its container (`.p-togglebutton-fluid`). */
    def fluid(v: Boolean): ToggleButton = new ToggleButton(s.copy(fluidFlag = v))

    def disabled(v: Boolean): ToggleButton = new ToggleButton(s.copy(disabledFlag = v))

    /** `readonly`: the button never toggles and reports `aria-disabled`, which is what
      * `role="button"` has to say it with (there is no readonly state for that role), but
      * unlike `disabled` it keeps its normal look AND its place in the tab order: a reader can
      * land on it and read its value, and every key and click that would change it runs into
      * nothing.
      */
    def readonly(v: Boolean): ToggleButton = new ToggleButton(s.copy(readonlyFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new ToggleButton(s.copy(validityV = v))

    /** Fired with the NEW checked value after a toggle (and after the ref write-back). */
    def onChange(f: Boolean => UI.Action)(using Frame): ToggleButton = new ToggleButton(s.copy(checkedSlot = checkedSlot.hear(f)))

    /** Fires on focus loss with the button's current checked state; unlike onChange it
      * fires even when the value was not toggled. The validation layer's Blur trigger.
      */
    def onBlur(f: Boolean => UI.Action)(using Frame): ToggleButton = new ToggleButton(s.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(validityV)
        // Unbound, the button keeps its own state, which starts unpressed: a ref it owns for as long as it is shown.
        val field = validity.follow(Slot.owning(
            checkedSlot,
            start = false
        )(s => new ToggleButton(this.s.copy(checkedSlot = s)).drawn(validity)))
        // Only a signal nobody hears is a mistake worth a card: a constant nobody hears is fixed, and drawn so.
        Diagnostics.after(field)("ToggleButton")(Diagnostic.unheard(
            checkedSlot,
            ToggleButton.checkedName,
            on = !readonlyFlag && !disabledFlag
        ))
    end render

    /** The button over its binding. A click is wired only where it reaches somebody: a two-way binding, or onChange. A
      * constant nobody hears is fixed: drawn read-only, reachable and readable, with nothing to press. The button draws
      * its state itself, so a click its owner does not take leaves nothing to paint back.
      */
    private def drawn(validity: FieldValidity)(using Frame): UI =
        Slot.drawField(checkedSlot, unbound = false)(f =>
            (if f.fixed then new ToggleButton(s.copy(readonlyFlag = true)) else this).body(f.value, f.edit, validity)
        )

    private def body(isChecked: Boolean, onChg: Maybe[Boolean => UI.Action], validity: FieldValidity)(using Frame): UI =
        var el = button.cssClass("p-togglebutton").cssClass("p-component")
        accessV.idV.foreach(v => el = el.id(v))
        if isChecked then el = el.cssClass("p-togglebutton-checked")
        el = validity.mark(el)
        sizeV match
            case Size.Small  => el = el.cssClass("p-togglebutton-sm").cssClass("p-inputfield-sm")
            case Size.Large  => el = el.cssClass("p-togglebutton-lg").cssClass("p-inputfield-lg")
            case Size.Normal => ()
        end match
        if fluidFlag then el = el.cssClass("p-togglebutton-fluid")
        el = el.jsProp("type", "button")
        // One state per role: a radio reports `aria-checked`, a toggle button `aria-pressed`, and
        // a control carrying both tells a screen reader two different stories about itself.
        radioV match
            case Present(r) =>
                el = el.role("radio").aria("checked", isChecked.toString).tabIndex(if r.tabbable then 0 else -1)
                if !disabledFlag && !readonlyFlag then el = el.onKeyDown(r.onKey)
            case Absent => el = el.aria("pressed", isChecked.toString)
        end match
        accessV.nameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accessV.nameRefV.foreach(v => el = el.aria("labelledby", v))
        if disabledFlag then el = el.disabled(true)
        // readonly keeps the normal look and focusability, but never toggles. It says so as
        // `aria-disabled`, not `aria-readonly`: this is a `role="button"`, and that role has no
        // readonly state to report. Without a click handler the browser's own activation reaches
        // nothing, so there is nothing to decline either.
        else if readonlyFlag then el = el.aria("disabled", "true")
        else onChg.foreach(f => el = el.onClick(f(!isChecked)))
        end if
        // Blur fires even without a toggle: the validation layer's Blur trigger. Reads the
        // ref LIVE (the CheckBox shape), so it reports the state at blur time.
        onBlurF.foreach { f =>
            el = el.onBlur(checkedSlot.current(false).map(f))
        }

        val contentChildren: List[UI] = contentV match
            case Present(ui) => List(ui)
            case Absent      =>
                val iconSlot: List[UI] =
                    (if isChecked then onIconV else offIconV).toList.map(g => GlyphSvg(g, "p-togglebutton-icon"))
                // Prime: the current-state label, or a non-breaking space when no labels are set.
                val labelUI: UI = (if isChecked then onLabelV else offLabelV) match
                    case Present(TextValue.Const(t)) => span.cssClass("p-togglebutton-label")(t): UI
                    case Present(TextValue.Dyn(s))   => span.cssClass("p-togglebutton-label")(s): UI
                    case _                           => span.cssClass("p-togglebutton-label")("\u00A0"): UI
                iconSlot :+ labelUI

        el(span.cssClass("p-togglebutton-content")(UI.children(contentChildren)*))
    end body
end ToggleButton

object ToggleButton:
    final private[uic] case class State(
        checkedSlot: Slot[Boolean] = Slot(),
        onLabelV: Maybe[TextValue] = Present(TextValue.Const("Yes")),
        offLabelV: Maybe[TextValue] = Present(TextValue.Const("No")),
        onIconV: Maybe[IconGlyph] = Absent,
        offIconV: Maybe[IconGlyph] = Absent,
        sizeV: Size = Size.Normal,
        fluidFlag: Boolean = false,
        disabledFlag: Boolean = false,
        readonlyFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        accessV: Accessibility = Accessibility.none,
        onBlurF: Maybe[Boolean => UI.Action] = Absent,
        contentV: Maybe[UI] = Absent,
        radioV: Maybe[ToggleButton.RadioOption] = Absent
    )

    def apply(): ToggleButton = new ToggleButton(State())

    private val checkedName = Slot.Name("ToggleButton", "checked", "a change", "onChange")

    /** What a single-select [[SelectButton]] option is, beyond a toggle button: whether it holds
      * the group's one tab stop, and the group's key handler for moving within it.
      */
    final private[uic] case class RadioOption(tabbable: Boolean, onKey: UI.KeyboardEvent => UI.Action)
end ToggleButton
