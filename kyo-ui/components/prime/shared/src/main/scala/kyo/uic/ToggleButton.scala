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
  * toggled value back before firing `onChange`), anything else one way. It binds a
  * `Boolean` form field like CheckBox through its generated `checked` and `onBlur`, so `uic.ToggleButton().bind(field)`
  * wires value, validity, message and the blur trigger in one call.
  */
final class ToggleButton private[uic] (private[uic] val p: ToggleButton.Props)
    extends Component(Setters[ToggleButton.Props, ToggleButton]), Node, FormControl[Boolean], HasAccessibleNameRef:

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
        new ToggleButton(p.copy(radio = Present(ToggleButton.RadioOption(tabbable, onKey))))

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new ToggleButton(p.copy(access = v))

    /** Package-internal content slot: SelectButton's `itemTemplate` replaces the
      * icon+label pair inside `.p-togglebutton-content` with arbitrary UI (the
      * kyo analogue of Prime's default slot).
      */
    private[uic] def content(ui: UI): ToggleButton = new ToggleButton(p.copy(content = Present(ui)))

    /** Drops both labels: the label span renders a non-breaking space (icon-only look). */
    def noLabels: ToggleButton = new ToggleButton(p.copy(onLabel = Opt.unset, offLabel = Opt.unset))

    private[uic] def validitySlots: FieldValidity.Slots                           = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self                   = new ToggleButton(p.copy(validity = v))
    private[uic] def bindRef(ref: SignalRef[Boolean])(using Frame): ToggleButton  = this.checked(ref)
    private[uic] def hearBlur(f: Boolean => UI.Action)(using Frame): ToggleButton = this.onBlur(f)

    /** One instance per place, drawn once: it holds the state the reader gives an unbound button, and a host region that runs
      * again hands it the run's props instead of starting it over (see [[ToggleButton.drawn]]).
      */
    private[uic] def render(using Frame): UI =
        UI.mountedWith(p)(ToggleButton.drawn)

    /** The button with no instance of its own, for a host that binds `checked` itself and draws its buttons in its own region
      * (SelectButton): nothing of it is left to keep across that region's runs, and no run follows, so every prop is read as
      * it is.
      */
    private[uic] def renderInPlace(using Frame): UI = ToggleButton.drawn(new UI.Live(p, Absent))
end ToggleButton

object ToggleButton:
    /** What a toggle button holds. Each field is a setter of its name (`uic.ToggleButton().size(Size.Small)`), which takes a
      * constant or a signal where the field is a `Prop`, the value itself where it is an `Opt` and the function itself for a callback;
      * `checked` binds and `onChange` (`@eventName`) hears it; `content` is written by hand, and the fields marked `@noSetter` by
      * hosts and the shared traits.
      */
    final private[uic] case class Props(
        /** The checked state. A `SignalRef` binds TWO-WAY: a click writes the toggled value, and its changes re-render. Any other
          * signal, or a constant, binds ONE-WAY: the button shows it, and a click writes nothing and only reports through `onChange`,
          * so the app writes its own state and the button follows. `ref.readOnly` opts a ref out. With no `onChange`, a card says the
          * reader cannot change it. Its event `onChange` fires with the NEW checked value after a toggle (and after the ref
          * write-back).
          */
        @eventName("onChange") checked: Controlled[Boolean] = Controlled(start = Present(false)),
        /** Label while checked (Prime default "Yes"). A signal patches the label in place. */
        onLabel: Opt[Prop[String]] = "Yes",
        /** Label while unchecked (Prime default "No"). A signal patches the label in place. */
        offLabel: Opt[Prop[String]] = "No",
        /** Icon while checked (`.p-togglebutton-icon`). */
        onIcon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** Icon while unchecked (`.p-togglebutton-icon`). */
        offIcon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** Size: `.p-togglebutton-sm` / default / `.p-togglebutton-lg`, a signal toggling the classes in place. */
        size: Prop[Size] = Size.Normal,
        /** Spans the full width of its container (`.p-togglebutton-fluid`), a signal toggling the class in place. */
        fluid: Prop[Boolean] = false,
        /** Native `disabled`: the button takes no focus and no click. A signal patches it in place. */
        disabled: Prop[Boolean] = false,
        /** `readonly`: the button never toggles and reports `aria-disabled`, which is what `role="button"` has to say it with
          * (there is no readonly state for that role), but unlike `disabled` it keeps its normal look AND its place in the tab
          * order: a reader can land on it and read its value, and every key and click that would change it runs into nothing.
          * A signal patches it in place.
          */
        readonly: Prop[Boolean] = false,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        /** Fires on focus loss with the button's current checked state; unlike `onChange` it fires even when the value was not
          * toggled. The validation layer's Blur trigger.
          */
        onBlur: Maybe[Boolean => UI.Action] = Absent,
        content: Maybe[UI] = Absent,
        @noSetter radio: Maybe[ToggleButton.RadioOption] = Absent
    ) derives CanEqual

    def apply(): ToggleButton = new ToggleButton(Props())

    private val checkedName = Controlled.Name("ToggleButton", "checked", "a change", "onChange")

    /** What of the props builds the button and has no channel of its own, read as one value: no hearer is in it, so a run
      * that only brings new hearers leaves it equal and draws nothing again.
      *
      * @param editable
      *   a click reaches somebody: the slot binds two way or somebody hears it
      * @param fixed
      *   a constant nobody hears, drawn read-only
      * @param unheard
      *   a signal bound one way that nobody hears
      * @param blurs
      *   somebody hears focus leaving
      */
    final private[uic] case class Base(
        editable: Boolean,
        fixed: Boolean,
        unheard: Boolean,
        blurs: Boolean,
        validity: FieldValidity.Slots,
        access: Accessibility,
        radio: Maybe[RadioOption]
    ) derives CanEqual

    private object Base:
        /** The base of `q`, as the engine hands it: an unbound `checked` is bound to the instance's own ref. */
        def apply(q: Props): Base =
            Base(q.checked.heard, q.checked.fixed, q.checked.deaf, q.onBlur.isDefined, q.validity, q.access, q.radio)
    end Base

    /** The button, drawn once over `props`, which hand it `checked` bound: to the instance's own ref where a run binds nothing
      * (carrying on from the binding when a run drops it), unbound where the button is drawn in place and its host binds it.
      *
      * Each place reads the field it shows: the checked state, the size, the width, `disabled` and `readonly` reach the element
      * through its class, `aria-*` and attribute channels, the labels and icons through the content's region, so a run that
      * changes one of them patches it alone. What builds the element (the [[Base]]) is one `reactive` block, drawn again only
      * when a run changes it. A click, a key and a blur read the run's flags, binding and hearers when they come, so they write
      * that run's ref and reach that run's `onChange`. A constant nobody hears is drawn read-only, reachable and readable, with
      * nothing to press.
      */
    private def drawn(props: UI.Live[Props])(using Frame): UI =
        val checked           = props(_.checked.value(false))
        val size              = props(_.size)
        val base              = props(Base(_))
        val disabled          = props(_.disabled)
        val readonly          = props(_.readonly)
        val toggle: UI.Action = props.call(_.checked).map(s => s.current(false).map(v => s.change(!v)))
        // Blur fires even without a toggle: the validation layer's Blur trigger, told the state at blur time.
        val blur: UI.Action = props.call(identity).map(q => q.onBlur.fold(Kyo.unit: UI.Action)(f => q.checked.current(false).map(f)))

        UI.reactive {
            // SelectButton's `itemTemplate` replaces the icon and label; otherwise the current state's, or a non-breaking space
            // where no labels are set (Prime).
            lazy val predefinedContent = UI.reactive {
                val on    = checked.read
                val shown = if on then props(_.onLabel).read else props(_.offLabel).read
                val glyph = if on then props(_.onIcon).read else props(_.offIcon).read
                fragment((glyph.map(GlyphSvg(_, "p-togglebutton-icon")).toList :+
                    (span.cssClass("p-togglebutton-label")(shown.getOrElse(" ")): UI))*)
            }
            val content: UI = props(_.content).read.getOrElse(predefinedContent)
            // Only a signal nobody hears is a mistake worth a card, and only while the reader could press: a constant nobody
            // hears is fixed, and drawn so. Its own block, so the flags it reads stay channels of the button.
            lazy val unheardCard = UI.reactive {
                fragment(Diagnostics.cards("ToggleButton")(Diagnostic(!readonly.read && !disabled.read, checkedName.advice))*)
            }

            val b        = base.read
            val off      = disabled.read
            val inert    = readonly.read || b.fixed
            val validity = FieldValidity(b.validity)
            val marked   = validity.mark(
                button.cssClass("p-togglebutton").cssClass("p-component")
                    .when(b.access.id)(_.id(_))
                    .when(checked)(_.cssClass("p-togglebutton-checked"))
            )
            val shaped = marked
                .cssClass.matching(size) {
                    case Size.Small  => "p-togglebutton-sm"
                    case Size.Normal => Absent
                    case Size.Large  => "p-togglebutton-lg"
                }
                .cssClass.matching(size) {
                    case Size.Small  => "p-inputfield-sm"
                    case Size.Normal => Absent
                    case Size.Large  => "p-inputfield-lg"
                }
                .when(props(_.fluid))(_.cssClass("p-togglebutton-fluid")).jsProp("type", "button")
            // One state per role: a radio reports `aria-checked`, a toggle button `aria-pressed`, and
            // a control carrying both tells a screen reader two different stories about itself.
            val roled = b.radio match
                case Present(r) =>
                    shaped.role("radio").aria("checked", checked.read.toString).tabIndex(if r.tabbable then 0 else -1)
                        .onKeyDown(r.onKey)
                case Absent => shaped.aria("pressed", checked.read.toString)
            // readonly keeps the normal look and focusability, but never toggles. It says so as
            // `aria-disabled`, not `aria-readonly`: this is a `role="button"`, and that role has no
            // readonly state to report. Disabled or readonly, the button is silent: its handlers check nothing, it lists
            // no event while locked, and a change patches that in place.
            val el = roled
                .aria("label", b.access.name)
                .aria("labelledby", b.access.nameRef)
                .disabled(off)
                .silent(inert)
                .ariaFlag("disabled", !off && inert)
                .when(b.editable)(_.onClick(toggle))
                .when(b.blurs)(_.onBlur(blur))
            val field = validity.follow(el(span.cssClass("p-togglebutton-content")(content)))
            if b.unheard then fragment(field, unheardCard) else field
        }
    end drawn

    /** What a single-select [[SelectButton]] option is, beyond a toggle button: whether it holds
      * the group's one tab stop, and the group's key handler for moving within it.
      */
    final private[uic] case class RadioOption(tabbable: Boolean, onKey: UI.KeyboardEvent => UI.Action)
end ToggleButton
