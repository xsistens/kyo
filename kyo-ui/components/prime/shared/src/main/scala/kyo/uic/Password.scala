package kyo.uic

import kyo.*
import kyo.UI.*

/** Password: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Password anatomy: `div.p-password.p-component.p-inputwrapper` >
  * `input.p-password-input.p-inputtext[type=password]` plus, with
  * `toggleMask(true)`, the eye toggle (`.p-password-toggle-mask-icon` with
  * Prime's `-unmask-icon`/`-mask-icon` state classes) that switches the field
  * between `type=password` and `type=text`), so the extracted `@primeuix`
  * password CSS applies verbatim. The reveal state is component-internal
  * (a `SignalRef[Boolean]` of the instance); the field follows it through
  * kyo-ui's `type` channel, so a reveal switches the type on the field the page
  * has and the text, the caret and focus stay.
  *
  * `feedback(true)` renders the strength meter (`.p-password-meter` +
  * `.p-password-meter-label` with Prime's `-weak`/`-medium`/`-strong` classes
  * inside a `.p-password-content` block). Deliberate deviation from Prime: the
  * meter renders INLINE below the field, not in a while-typing overlay: a
  * floated panel adds focus/dismiss machinery for zero information gain on a
  * server-driven page. Strength is computed on the server from the field's
  * text with Prime's OWN default rules: medium = two of [lowercase, uppercase,
  * digit] and at least 6 chars; strong = all three and at least 8 chars;
  * anything else weak; empty shows `promptLabel`.
  */
final class Password private[uic] (private[uic] val p: Password.Props) extends Component(Setters[Password.Props, Password]), Node,
      FormControl[String], HasPlaceholder, HasAccessibleNameRef, HasAccessibleDescription:

    type Self = Password

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): Password           = new Password(p.copy(placeholder = v))
    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): Password = new Password(p.copy(description = v))
    private[uic] def accessibility: Accessibility                                = p.access
    private[uic] def withAccessibility(v: Accessibility): Self                   = new Password(p.copy(access = v))
    private[uic] def validitySlots: FieldValidity.Slots                          = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self                  = new Password(p.copy(validity = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): Password      = this.value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): Password     = this.onBlur(f)

    /** Fires with the text the field holds when the reader commits it (focus leaving after an edit, or Enter). It hears the value
      * too, so a one-way binding with only an `onChange` says nothing.
      */
    def onChange(f: String => UI.Action)(using Frame): Password =
        new Password(p.copy(onChange = Present(Callback.of(f)), value = p.value.heardToo))

    /** One instance per place, drawn once: it holds the reader's text (unbound in the instance's own ref from an empty start,
      * from a constant in that ref from the constant) and the reveal, and a host region that runs again hands it the run's
      * props instead of starting it over (see [[Password.drawnWith]]).
      */
    private[uic] def render(using Frame): UI =
        UI.mountedWith(p)(Password.drawn)

    /** The instance with its reveal handed in (golden-test seam): the same draw, over a reveal the test holds. */
    private[uic] def wired(revealed: SignalRef[Boolean])(using Frame): UI =
        UI.mountedWith(p)(Password.drawnWith(_, revealed))
end Password

object Password:
    /** What a password field holds. Each field is a setter of its name (`uic.Password().size(Size.Small)`), which takes a constant
      * or a signal where the field is a `Prop`, the value itself where it is an `Opt` and the function itself for a callback;
      * `value` binds and `onInput` (`@eventName`) hears it; `onChange` is written by hand, and the fields marked `@noSetter` by
      * the shared traits.
      */
    final private[uic] case class Props(
        /** The value. A `SignalRef` binds TWO-WAY: an edit writes it, and its changes update the field and the meter. Any other
          * signal binds ONE-WAY: the field and the meter show it, and an edit writes nothing and only reports through `onInput`
          * and `onChange`, so the app writes its own state and the field follows; with neither, a card says the reader cannot
          * change it. A constant is where the text starts. Unbound or from a constant, the instance keeps the text in a ref of
          * its own: the meter judges what the reader typed, and a reveal or a host region that runs again keeps it.
          * `ref.readOnly` opts a ref out. Its event `onInput` fires with the text after every keystroke.
          */
        @eventName("onInput") value: Controlled[String] = Controlled(start = Present(""), constantStarts = true),
        /** Adds the eye toggle that reveals and masks the typed text (Prime's `toggleMask`). */
        toggleMask: Boolean = false,
        /** Shows the strength meter below the field (inline; see the class doc). */
        feedback: Boolean = false,
        /** Meter label while the text is empty (Prime default "Enter a password"). A signal changes it in place. */
        promptLabel: Prop[String] = "Enter a password",
        /** Meter label for a weak text (Prime default "Weak"). A signal changes it in place. */
        weakLabel: Prop[String] = "Weak",
        /** Meter label for a medium text (Prime default "Medium"). A signal changes it in place. */
        mediumLabel: Prop[String] = "Medium",
        /** Meter label for a strong text (Prime default "Strong"). A signal changes it in place. */
        strongLabel: Prop[String] = "Strong",
        @noSetter placeholder: Maybe[Prop[String]] = Absent,
        /** Native `disabled`: the field and the eye take no input. A signal toggles both in place. */
        disabled: Prop[Boolean] = false,
        /** Native `readonly`: focusable and readable, not editable; the eye still reveals. A signal toggles it in place. */
        readonly: Prop[Boolean] = false,
        /** Marks the field required (the native constraint and `aria-required`). A signal toggles both in place. */
        required: Prop[Boolean] = false,
        /** Native `name` for HTML form participation. A signal draws the field again when it changes. */
        name: Opt[Prop[String]] = Opt.unset,
        /** Native `maxlength`: caps the number of characters (a negative cap is none at all). A signal draws the field again
          * when it changes.
          */
        maxLength: Opt[Prop[Int]] = Opt.unset,
        /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field, a signal swapping the class in place. */
        size: Prop[Size] = Size.Normal,
        /** Fill variant: `Filled` renders `.p-variant-filled`, `Outlined` is the default; a signal swaps the class in place. */
        variant: Prop[FieldVariant] = FieldVariant.Outlined,
        /** Spans the full width of its container (`.p-password-fluid`), a signal toggling the class in place. */
        fluid: Prop[Boolean] = false,
        onChange: Maybe[String => UI.Action] = Absent,
        /** Fires on focus loss (native `blur`) with the text the field holds as focus leaves it: what the reader typed, also
          * unbound or from a constant, with the meter and the reveal on or off. Unlike `onChange` it fires even when the text
          * was not edited. The form-validation layer wires its `Blur` trigger here.
          */
        onBlur: Maybe[String => UI.Action] = Absent,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        @noSetter description: Maybe[Prop[String]] = Absent
    ) derives CanEqual

    def apply(): Password = new Password(Props())

    /** What of the props builds the field and has no channel of its own, read as one value: no hearer is in it, so a run that
      * only brings new hearers or new labels leaves it equal and draws nothing again.
      *
      * @param eye
      *   the eye toggle is drawn
      * @param meter
      *   the strength meter is drawn
      * @param text
      *   what the field's value binds: the caller's binding, or the instance's own ref, which the engine binds where the run
      *   binds nothing
      * @param placeholder
      *   whether the field has a placeholder, and the prop it shows, whose signal patches in place
      * @param description
      *   whether the field has an accessible description, and the prop it says, whose signal patches in place
      */
    final private[uic] case class Base(
        eye: Boolean,
        meter: Boolean,
        text: Prop[String],
        hearsInput: Boolean,
        hearsChange: Boolean,
        blurs: Boolean,
        placeholder: Maybe[Prop[String]],
        validity: FieldValidity.Slots,
        access: Accessibility,
        description: Maybe[Prop[String]]
    ) derives CanEqual

    private object Base:
        /** The base of `q`, as the engine hands it: an unbound `value` is bound to the instance's own ref. */
        def apply(q: Props): Base =
            Base(
                q.toggleMask,
                q.feedback,
                q.value.value(""),
                q.value.hearer.isDefined,
                q.onChange.isDefined,
                q.onBlur.isDefined,
                q.placeholder,
                q.validity,
                q.access,
                q.description
            )
    end Base

    /** The field over `props` with a reveal of its own, which starts masked. */
    private def drawn(props: UI.Live[Props])(using Frame): UI < Sync =
        Signal.initRef(false).map(drawnWith(props, _))

    /** The field, drawn once over `props`, which hand it `value` bound: to the instance's own ref where a run binds nothing, as
      * the caller bound it otherwise. `revealed` is the reveal the eye flips.
      *
      * Each place reads the field it shows: the size, the variant, the width, `disabled`, `readonly` and `required` reach the field and the
      * wrapper through their class and attribute channels, and the reveal reaches the field's type through kyo-ui's `textType`
      * channel and the eye's classes, label and glyph through theirs, so a run that changes one of them, or a reveal, patches it
      * alone. What builds the field (the [[Base]], and the native `name` and `maxLength`, which have no channel) is one
      * `reactive` block, drawn again only when a run changes it. The meter is the one region, over the text, a sibling of the
      * field, so a keystroke draws only the meter. An input, a commit and a blur read the run's hearers when they come.
      */
    private def drawnWith(props: UI.Live[Props], revealed: SignalRef[Boolean])(using Frame): UI =
        val base            = props(Base(_))
        val disabled        = props(_.disabled)
        val readonly        = props(_.readonly)
        val required        = props(_.required)
        val size            = props(_.size)
        val variant         = props(_.variant)
        val fluid           = props(_.fluid)
        val name            = props(_.name)
        val maxLength       = props(_.maxLength)
        val input           = props.hearing(_.value.hearer)
        val change          = props.hearing(_.onChange)
        val blur            = props.hearing(_.onBlur)
        val typed           = revealed.map(on => if on then UI.TextType.Text else UI.TextType.Password)
        val flip: UI.Action = revealed.getAndUpdate(!_).unit
        // The eye offers what a press does: to reveal while masked, to mask while revealed. Disabled, it is silent.
        val eye = span
            .cssClass("p-password-toggle-mask-icon")
            .cssClass("p-password-mask-icon", revealed)
            .cssClass("p-password-unmask-icon", revealed.map(!_))
            .role("button")
            .tabIndex(0)
            .aria("label", revealed.map(on => if on then "Hide password" else "Show password"))
            .silent(disabled)
            .onClick(flip)
            .onKeyDown { e =>
                e.key match
                    case Keyboard.Enter | Keyboard.Space => flip
                    case _                               => Kyo.unit: UI.Action
            }(GlyphSvg(revealed.map(on => if on then Icons.eyeSlash else Icons.eye), "p-icon"))

        UI.reactive {
            val b        = base.read
            val validity = FieldValidity(b.validity)
            // Input carries the shared text-field contract, drawn in place: this instance binds its text.
            val field = Input()
                .extraClass("p-password-input")
                .filledOn("p-inputwrapper-filled", ".p-password")
                .unheardAs("Password", "onInput or onChange")
                .inputType(InputType.Password)
                .when(b.eye)(_.revealing(typed))
                .value(b.text)
                .when(b.hearsInput)(_.onInput(input))
                .when(b.hearsChange)(_.onChange(change))
                .when(b.blurs)(_.onBlur(blur))
                .withPlaceholder(b.placeholder)
                .disabled(disabled)
                .readonly(readonly)
                .required(required)
                .when(name.read)(_.name(_))
                .when(maxLength.read)(_.maxLength(_))
                .size(size)
                .variant(variant)
                .fluid(fluid)
                .invalid(validity.red)
                .withAccessibility(b.access)
                .withAccessibleDescription(b.description)
            // The wrapper's `p-inputwrapper-filled` follows the text: kyo-ui's client toggles it from the field as the reader
            // types (`filledOn`), and this channel stamps the starting class, which the static render cannot reach from the field.
            val root = div
                .cssClass("p-password")
                .cssClass("p-component")
                .cssClass("p-inputwrapper")
                .cssClass("p-inputwrapper-filled", UI.reactive(b.text.read.nonEmpty))
                .cssClass("p-password-fluid", fluid)(UI.children(field.renderInPlace, Maybe.when(b.eye)(eye))*)
            // The inline strength meter (see the class doc for the deviation): the one region, over the text.
            val body = if b.meter then fragment(root, UI.reactive(meter(props, b.text.read))) else root
            validity.follow(body)
        }
    end drawnWith

    /** The meter over the text `v`: the bar and the label of its strength, the label a prop of the latest run. */
    private def meter(live: UI.Live[Props], v: String)(using Frame): UI =
        val (cls, width, label) = strength(v) match
            case 0 => ("p-password-meter-weak", 0.0, live(_.promptLabel))
            case 1 => ("p-password-meter-weak", 100.0 / 3, live(_.weakLabel))
            case 2 => ("p-password-meter-medium", 200.0 / 3, live(_.mediumLabel))
            case _ => ("p-password-meter-strong", 100.0, live(_.strongLabel))
        div.cssClass("p-password-content").cssClass("p-uic-password-feedback")(
            div.cssClass("p-password-meter")(
                div.cssClass("p-password-meter-label").cssClass(cls).style(_.width(width.pct))
            ),
            div.cssClass("p-password-meter-text")(label)
        )
    end meter

    /** Prime's default strength rules as pure predicates: 0 = empty (prompt),
      * 1 = weak, 2 = medium (two of [a-z, A-Z, 0-9] and length at least 6),
      * 3 = strong (all three and length at least 8).
      */
    private[uic] def strength(v: String): Int =
        if v.isEmpty then 0
        else
            val lower   = v.exists(_.isLower)
            val upper   = v.exists(_.isUpper)
            val digit   = v.exists(_.isDigit)
            val classes = List(lower, upper, digit).count(identity)
            if classes == 3 && v.length >= 8 then 3
            else if classes >= 2 && v.length >= 6 then 2
            else 1
end Password
