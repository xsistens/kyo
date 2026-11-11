package kyo.uic

import kyo.*
import kyo.UI.*

/** Input type variants: selects which concrete kyo text-input factory backs the field, so the native `type` attribute is real
  * (email keyboards on mobile, password masking, numeric spinners, ...).
  */
enum InputType derives CanEqual:
    case Text, Email, Number, Password, Tel, URL

    /** The free-text type kyo-ui writes for this one. `Number` has none: a number field is drawn by its own factory and never
      * reads it, so it answers `Text`.
      */
    private[uic] def textType: UI.TextType = this match
        case InputType.Text     => UI.TextType.Text
        case InputType.Email    => UI.TextType.Email
        case InputType.Password => UI.TextType.Password
        case InputType.Tel      => UI.TextType.Tel
        case InputType.URL      => UI.TextType.Url
        case InputType.Number   => UI.TextType.Text
end InputType

/** Text input: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's InputText: the component IS the bare
  * `<input class="p-inputtext">`, no wrapper element). Composes the kyo `<input>` element, so two-way binding
  * (`value(SignalRef)` to `Bound.Ref`), `onInput`/`onChange`, and the caret-preserving morph path are all inherited from kyo-ui,
  * not rebuilt.
  *
  * The text is the reader's: unbound, the browser keeps what was typed, and a constant is where it starts, which the instance
  * holds from there, so a host region that runs again keeps the reader's text. Any other `Signal[String]` binds one way: the
  * field shows it, and an edit only reports through `onInput`/`onChange`. Validation is Prime's `.p-invalid` model: `invalid`
  * plus the optional `invalidMessage` row below the field.
  */
final class Input private[uic] (private[uic] val p: Input.Props) extends Component(Setters[Input.Props, Input]), Node,
      FormControl[String],
      HasPlaceholder, HasAccessibleNameRef, HasAccessibleDescription:

    type Self = Input

    /** Package-internal class hook: wrappers (FloatLabel/IftaLabel) stamp Prime's state classes (`p-filled`) onto the field they
      * wrap.
      */
    private[uic] def extraClass(cls: String): Input = new Input(p.copy(extraClasses = p.extraClasses :+ cls))

    /** Package-internal: the component and the events the unheard card names, for a host whose field this is and whose own events
      * are what hear it ([[InputMask]]).
      */
    private[uic] def unheardAs(component: String, events: String): Input =
        new Input(p.copy(unheardAs = Input.valueName(component, events)))

    /** Package-internal: the class a host's wrapper takes while this field holds text, and the selector that finds the wrapper
      * (kyo-ui's `filledClass(name, closest)`), for a host whose stylesheet marks its wrapper ([[Password]]).
      */
    private[uic] def filledOn(cls: String, closest: String): Input = new Input(p.copy(filledUp = Present((cls, closest))))

    /** Package-internal: the field's type follows `sig` through kyo-ui's `textType` channel, in place of [[Input.Props.inputType]],
      * for a host whose type moves between free-text types alone ([[Password]]'s reveal): it can never cross to `Number`, so it
      * needs no number flag and the field sits under no region.
      */
    private[uic] def revealing(sig: Signal[UI.TextType]): Input = new Input(p.copy(revealedAs = Present(sig)))

    private[uic] def accessibility: Accessibility                             = p.access
    private[uic] def withAccessibility(v: Accessibility): Self                = new Input(p.copy(access = v))
    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): Input           = new Input(p.copy(placeholder = v))
    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): Input = new Input(p.copy(description = v))
    private[uic] def validitySlots: FieldValidity.Slots                       = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self               = new Input(p.copy(validity = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): Input      = this.value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): Input     = this.onBlur(f)

    /** Fires with the text the field holds when the reader commits it (focus leaving after an edit, or Enter). It hears the value
      * too, so a one-way binding with only an `onChange` says nothing.
      */
    def onChange(f: String => UI.Action)(using Frame): Input =
        new Input(p.copy(onChange = Present(Callback.of(f)), value = p.value.heardToo))

    /** One instance per place where a constant gives the text its start: it holds the reader's text from there, and a host region
      * that runs again hands it the run's props instead of starting it over. Unbound or bound to a signal the field keeps nothing
      * of its own (the browser keeps an unbound text), so it is drawn directly (see [[Input.drawn]]).
      */
    private[uic] def render(using Frame): UI =
        if p.value.start.isDefined then UI.mountedWith(p)(Input.drawn) else renderInPlace

    /** The field with no instance of its own, for a host that states its text itself and draws it in its own region (InputOtp's
      * cells, Paginator's jump field, DataTable's filter fields): every prop is read as it is, a constant as the text to show.
      */
    private[uic] def renderInPlace(using Frame): UI = Input.drawn(new UI.Live(p, Absent))
end Input

object Input:
    /** What a text field holds. Each field is a setter of its name (`uic.Input().size(Size.Small)`), which takes a constant or a
      * signal where the field is a `Prop`, the value itself where it is an `Opt` and the function itself for a callback; `value`
      * binds and `onInput` (`@eventName`) hears it; `onChange` is written by hand, and the fields marked `@noSetter` by hosts and
      * the shared traits.
      */
    final private[uic] case class Props(
        /** The value. A `SignalRef` binds TWO-WAY: an edit writes it, and its changes update the field. Any other signal binds
          * ONE-WAY: the field shows it, and an edit writes nothing and only reports through `onInput` and `onChange`, so the app
          * writes its own state and the field follows; with neither, a card says the reader cannot change it. A constant is where
          * the text starts: the reader edits it freely, and the field keeps the edit when a region around it runs again.
          * `ref.readOnly` opts a ref out. Its event `onInput` fires with the text after every keystroke.
          */
        @eventName("onInput") value: Controlled[String] = Controlled(constantStarts = true),
        @noSetter placeholder: Maybe[Prop[String]] = Absent,
        /** Native `disabled`. A signal toggles it in place (no re-render, so caret and focus survive), e.g. disabled until another
          * field is valid.
          */
        disabled: Prop[Boolean] = false,
        /** Native `readonly`: focusable and readable, not editable. A signal toggles it in place. */
        readonly: Prop[Boolean] = false,
        /** Marks the field required (the native constraint and `aria-required`). A signal toggles both in place. */
        required: Prop[Boolean] = false,
        /** Native `name` for HTML form participation. A signal draws the field again when it changes. */
        name: Opt[Prop[String]] = Opt.unset,
        /** Native `maxlength`: caps the number of characters (a negative cap is none at all). A signal draws the field again
          * when it changes.
          */
        maxLength: Opt[Prop[Int]] = Opt.unset,
        /** The native input type, [[InputType.Text]] by default. A signal that moves between the free-text types switches the type
          * on the field the page has, in place (kyo-ui's `textType` channel), so the text the reader typed, the caret and focus
          * stay. A change to or from [[InputType.Number]] draws the field again, as a number field: `number` reads its value
          * differently, so it is another element.
          */
        inputType: Prop[InputType] = InputType.Text,
        /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg`, a signal swapping the class in place. */
        size: Prop[Size] = Size.Normal,
        /** Fill variant: `Filled` renders `.p-variant-filled`, `Outlined` is the default; a signal swaps the class in place. */
        variant: Prop[FieldVariant] = FieldVariant.Outlined,
        /** Spans the full width of its container (`.p-inputtext-fluid`), a signal toggling the class in place. */
        fluid: Prop[Boolean] = false,
        /** Seeds focus onto this field when a re-render inserts it into the DOM for the first time, kyo-ui's `focusAuto`. A field
          * that appears in place of something else, a cell that swaps its text for an editor being the case this exists for, is
          * focusable but not focused: the element is new, so nothing carried focus into it, and a handler that fired before the
          * insert cannot reach it either. The flag is declarative for that reason, and it is why an imperative `focusId` is the
          * wrong tool here. At most one focus-auto element is seeded per patch, so several fields appearing together (a row of
          * editors) hand focus to the first.
          */
        focusAuto: Boolean = false,
        /** Client-local keystroke filter: `"int"` keeps only digits, `"decimal"` also allows one separator, any other string is a
          * whitelist charset. The client rejects disallowed `beforeinput` keystrokes before they reach the field, so an invalid
          * character never becomes visible, and the filtered value is what reaches `onInput` and the ref. The string stays this
          * library's surface; kyo takes a typed [[kyo.UI.InputFilter]], which `Input.asInputFilter` maps onto. Not on a `Number`
          * field, which constrains its content through its type. A signal draws the field again when it changes.
          */
        inputFilter: Opt[Prop[String]] = Opt.unset,
        /** Client-local input mask, e.g. `"(999) 999-9999"`: `9` = digit, `a` = letter, `*` = alphanumeric; every other character
          * is a literal. The client formats the field as the reader types and the MASKED value is what reaches `onInput`,
          * `onChange` and the ref. Prefer the dedicated [[InputMask]] component for phone, date and ssn fields. Not on a `Number`
          * field. A signal draws the field again when it changes.
          */
        inputMask: Opt[Prop[String]] = Opt.unset,
        onChange: Maybe[String => UI.Action] = Absent,
        /** Fires on focus loss (native `blur`) with the text the field holds as focus leaves it, which kyo-ui's blur carries: what
          * the reader typed, also when nothing binds the field or it started from a constant. Unlike `onChange` it fires even when
          * the value was not edited. The form-validation layer wires its `Blur` trigger here.
          */
        onBlur: Maybe[String => UI.Action] = Absent,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        @noSetter description: Maybe[Prop[String]] = Absent,
        @noSetter extraClasses: List[String] = Nil,
        @noSetter unheardAs: Controlled.Name = Input.valueName("Input", "onInput or onChange"),
        @noSetter filledUp: Maybe[(String, String)] = Absent,
        @noSetter revealedAs: Maybe[Signal[UI.TextType]] = Absent
    ) derives CanEqual

    def apply(): Input = new Input(Props())

    private[uic] def valueName(component: String, events: String) = Controlled.Name(component, "value", "a change", events)

    /** What of the props builds the field and has no channel of its own, read as one value: no hearer is in it, so a run that
      * only brings new hearers leaves it equal and draws nothing again.
      *
      * @param text
      *   what the field's `value` binds: the caller's binding, the instance's own ref, or a constant start drawn in place; absent
      *   where nothing is bound and the browser keeps the text
      * @param unheard
      *   a signal bound one way that nobody hears
      */
    final private[uic] case class Base(
        text: Maybe[Prop[String]],
        hearsInput: Boolean,
        hearsChange: Boolean,
        blurs: Boolean,
        unheard: Boolean,
        placeholder: Maybe[Prop[String]],
        focusAuto: Boolean,
        validity: FieldValidity.Slots,
        access: Accessibility,
        description: Maybe[Prop[String]],
        extraClasses: List[String],
        unheardAs: Controlled.Name,
        filledUp: Maybe[(String, String)],
        revealedAs: Maybe[Signal[UI.TextType]]
    ) derives CanEqual

    private object Base:
        /** The base of `q`, as the engine hands it: a value that starts from a constant is bound to the instance's own ref. */
        def apply(q: Props): Base =
            Base(
                q.value.bound.orElse(q.value.start),
                q.value.hearer.isDefined,
                q.onChange.isDefined,
                q.onBlur.isDefined,
                q.value.deaf,
                q.placeholder,
                q.focusAuto,
                q.validity,
                q.access,
                q.description,
                q.extraClasses,
                q.unheardAs,
                q.filledUp,
                q.revealedAs
            )
    end Base

    /** The field, drawn once over `props`, which hand it `value` bound: to the instance's own ref where a constant starts it, as
      * the caller bound it otherwise, and not at all where nothing is bound and the browser keeps the text.
      *
      * Each place reads the field it shows: the size, the variant, the width, `disabled`, `readonly` and `required` reach the
      * element through its class, attribute and `aria-*` channels, so a run that changes one of them patches it alone. What builds
      * the element (the [[Base]], and whether it is a number field) is one `reactive` block, drawn again only when a run
      * changes it. An input, a commit and a blur read the run's hearers when they come. The card for a signal nobody hears is a
      * block of its own over `disabled` and `readonly`, so it shows only while the reader could edit.
      */
    private def drawn(props: UI.Live[Props])(using Frame): UI =
        val base     = props(Base(_))
        val disabled = props(_.disabled)
        val readonly = props(_.readonly)
        val kind     = props(_.inputType)
        // What builds the element is only whether it is a number field; between the free-text types the type is a channel.
        val number   = Prop.map(kind)(_ == InputType.Number)
        val freeText = Prop.map(kind)(_.textType)
        // The native attributes have no channel: a signal among them is read where the field is built, so it draws the field again.
        val name        = props(_.name)
        val maxLength   = props(_.maxLength)
        val inputFilter = props(_.inputFilter)
        val inputMask   = props(_.inputMask)
        val blur        = props.hearing(_.onBlur)
        val heard       = Heard(props.hearing(_.value.hearer), props.hearing(_.onChange), (_, s) => blur(s))
        UI.reactive {
            val b         = base.read
            val validity  = FieldValidity(b.validity)
            val native    = Native(name.read, maxLength.read)
            val field: UI =
                if number.read then shared(numberInput, b, native, props, validity, heard)
                else
                    val typed = constrained(input, inputFilter.read, inputMask.read).textType(b.revealedAs.getOrElse(freeText))
                    shared(typed, b, native, props, validity, heard)
            val followed = validity.follow(field)
            if b.unheard then fragment(followed, unheardCard(b.unheardAs, disabled, readonly)) else followed
        }
    end drawn

    /** The native attributes as this run of the block reads them. */
    final private case class Native(name: Maybe[String], maxLength: Maybe[Int])

    /** The run's hearers, read when their event comes. */
    final private case class Heard(input: String => UI.Action, change: String => UI.Action, blur: (UI.MouseEvent, String) => UI.Action)

    /** What every factory's field carries: the classes, the id, focus seeding, the validity marks, the filled class, the value, the
      * placeholder, the flags, the native attributes, the accessible name and description, and the hearers.
      */
    private def shared[E <: (UI.Ast.TextInput & UI.Ast.Interactive) { type Self = E }](
        el: E,
        b: Base,
        native: Native,
        props: UI.Live[Props],
        validity: FieldValidity,
        heard: Heard
    )(using Frame): E =
        val required = props(_.required)
        validity.mark(
            b.extraClasses.foldLeft(el.cssClass("p-inputtext").cssClass("p-component"))(_.cssClass(_))
                .when(b.access.id)(_.id(_))
                .focusAuto(b.focusAuto)
                .cssClass.matching(props(_.size)) {
                    case Size.Small  => "p-inputtext-sm"
                    case Size.Normal => Absent
                    case Size.Large  => "p-inputtext-lg"
                }
                .cssClass.matching(props(_.variant)) {
                    case FieldVariant.Outlined => Absent
                    case FieldVariant.Filled   => "p-variant-filled"
                }
        )
            .when(props(_.fluid))(_.cssClass("p-inputtext-fluid"))
            // Prime's `p-filled` follows the text the field holds, which the client knows and a render does not (an unbound
            // field's text, the reader's edit): kyo-ui toggles it there, the static render from the start.
            .filledClass("p-filled")
            .when(b.filledUp)((e, up) => e.filledClass(up._1, up._2))
            .when(b.text)(_.value(_))
            .placeholder(b.placeholder)
            .disabled(props(_.disabled))
            .readOnly(props(_.readonly))
            .required(required)
            .aria(Aria.required -> required)
            .when(native.name)(_.jsProp("name", _))
            .when(native.maxLength)((e, n) => e.jsProp("maxLength", math.max(0, n).toString))
            .aria(
                Aria.label       -> b.access.name,
                Aria.labelledby  -> b.access.nameRef,
                Aria.description -> b.description
            )
            .when(b.hearsInput)(_.onInput(heard.input))
            .when(b.hearsChange)(_.onChange(heard.change))
            .when(b.blurs)(_.onBlur(heard.blur))
    end shared

    /** The typing constraints, which kyo puts on `ConstrainedInput` rather than `TextInput`: `NumberInput` does not mix the trait
      * in, because a `number` field reads its value as the empty string while the content is not a valid number, so a
      * character-level filter cannot see what it is filtering. It constrains its content through `type`, `min`, `max` and `step`.
      */
    private def constrained[E <: UI.Ast.ConstrainedInput { type Self = E }](
        el: E,
        inputFilter: Maybe[String],
        inputMask: Maybe[String]
    ): E =
        el.when(inputFilter)((e, f) => e.inputFilter(asInputFilter(f))).when(inputMask)(_.inputMask(_))

    /** Only a signal nobody hears is a mistake worth a card, and only while the reader could edit. Its own block, so the flags it
      * reads stay channels of the field.
      */
    private def unheardCard(name: Controlled.Name, disabled: Prop[Boolean], readonly: Prop[Boolean])(using Frame): UI =
        UI.reactive(fragment(Diagnostics.cards(name.component)(Diagnostic(!readonly.read && !disabled.read, name.advice))*))

    /** Maps this library's filter string onto kyo's typed [[kyo.UI.InputFilter]]. `"int"` is `Digits` (digits only; kyo's set
      * carries no sign character), `"decimal"` is `Decimal`, and anything else is taken as an explicit whitelist charset.
      */
    private[uic] def asInputFilter(pattern: String): UI.InputFilter =
        pattern match
            case "int"     => UI.InputFilter.Digits
            case "decimal" => UI.InputFilter.Decimal
            case chars     => UI.InputFilter.Allowed(chars)
end Input
