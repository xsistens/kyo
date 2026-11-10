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
final class Password private[uic] (private[uic] val p: Password.Props) extends Node, FormControl[String], HasPlaceholder, HasAccessibleName,
      HasProps:
    import p.*

    type Self = Password

    /** The value. A `SignalRef` binds TWO-WAY: edits write back, ref changes update the field (and the strength meter
      * when `feedback(true)`). Any other signal binds ONE-WAY: the field and the meter show it, and an edit writes
      * nothing and only reports through [[onInput]] and [[onChange]]; with neither, a card says the reader cannot change
      * it. A constant is the starting text. Unbound or from a constant, the field keeps the text itself, in the instance's
      * own ref: the meter judges what the reader typed, and a reveal or a host region that runs again keeps it.
      * `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): Password = new Password(p.copy(valueSlot = valueSlot.bind(v)))

    /** Adds the eye toggle that reveals/masks the typed value (Prime's `toggleMask`). */
    def toggleMask(v: Boolean): Password = new Password(p.copy(toggleMaskFlag = v))

    /** Shows the strength meter below the field (inline; see the class doc). */
    def feedback(v: Boolean): Password = new Password(p.copy(feedbackFlag = v))

    /** Meter label while the value is empty (Prime default "Enter a password"). A `Signal[String]` changes it in place. */
    def promptLabel(v: Prop[String]): Password = new Password(p.copy(promptLabelV = v))

    /** Meter label for weak values (Prime default "Weak"). A `Signal[String]` changes it in place. */
    def weakLabel(v: Prop[String]): Password = new Password(p.copy(weakLabelV = v))

    /** Meter label for medium values (Prime default "Medium"). A `Signal[String]` changes it in place. */
    def mediumLabel(v: Prop[String]): Password = new Password(p.copy(mediumLabelV = v))

    /** Meter label for strong values (Prime default "Strong"). A `Signal[String]` changes it in place. */
    def strongLabel(v: Prop[String]): Password = new Password(p.copy(strongLabelV = v))

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): Password = new Password(p.copy(placeholderText = v))

    def disabled(v: Boolean): Password = new Password(p.copy(disabledFlag = v))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles it IN
      * PLACE via kyo-ui's boolean attribute channel (no re-render).
      */
    def readonly(v: Prop[Boolean]): Password = new Password(p.copy(readonlyFlag = Present(v)))

    /** Marks the field required (native constraint + `aria-required`). */
    def required(v: Boolean): Password = new Password(p.copy(requiredFlag = v))

    /** Native `name` for HTML form participation. */
    def name(v: String): Password = new Password(p.copy(nameV = Present(v)))

    /** Native `maxlength`: caps the number of characters. */
    def maxLength(n: Int): Password = new Password(p.copy(maxLengthV = Present(math.max(0, n))))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field. */
    def size(v: Size): Password = new Password(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is default. */
    def variant(v: FieldVariant): Password = new Password(p.copy(variantV = v))

    /** Spans the full container width (`.p-password-fluid`). */
    def fluid(v: Boolean): Password = new Password(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                      = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self              = new Password(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): Password  = value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): Password = onBlur(f)

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Password(p.copy(access = v))

    def onChange(f: String => UI.Action)(using Frame): Password =
        new Password(p.copy(onChangeSF = Present(Callback.of(f)), valueSlot = valueSlot.heardToo))
    def onInput(f: String => UI.Action)(using Frame): Password = new Password(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fires on focus loss (native `blur`) with the text the field holds as focus
      * leaves it: what the reader typed, also unbound or from a constant, with the
      * meter and the reveal on or off. Unlike `onChange` it fires even when the value
      * was not edited. The form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => UI.Action)(using Frame): Password = new Password(p.copy(onBlurSF = Present(Callback.of(f))))

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
    final private[uic] case class Props(
        valueSlot: Controlled[String] = Controlled(start = Present(""), constantStarts = true),
        toggleMaskFlag: Boolean = false,
        feedbackFlag: Boolean = false,
        promptLabelV: Prop[String] = "Enter a password",
        weakLabelV: Prop[String] = "Weak",
        mediumLabelV: Prop[String] = "Medium",
        strongLabelV: Prop[String] = "Strong",
        placeholderText: Maybe[Prop[String]] = Absent,
        disabledFlag: Boolean = false,
        readonlyFlag: Maybe[Prop[Boolean]] = Absent,
        requiredFlag: Boolean = false,
        nameV: Maybe[String] = Absent,
        maxLengthV: Maybe[Int] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onChangeSF: Maybe[String => UI.Action] = Absent,
        onBlurSF: Maybe[String => UI.Action] = Absent
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
      */
    final private[uic] case class Base(
        eye: Boolean,
        meter: Boolean,
        text: Prop[String],
        hearsInput: Boolean,
        hearsChange: Boolean,
        blurs: Boolean,
        readonly: Maybe[Prop[Boolean]],
        placeholder: Maybe[Prop[String]],
        validity: FieldValidity.Slots,
        access: Accessibility
    ) derives CanEqual

    private object Base:
        /** The base of `q`, as the engine hands it: an unbound `value` is bound to the instance's own ref. */
        def apply(q: Props): Base =
            Base(
                q.toggleMaskFlag,
                q.feedbackFlag,
                q.valueSlot.value(""),
                q.valueSlot.hearer.isDefined,
                q.onChangeSF.isDefined,
                q.onBlurSF.isDefined,
                q.readonlyFlag,
                q.placeholderText,
                q.validityV,
                q.access
            )
    end Base

    /** The field over `props` with a reveal of its own, which starts masked. */
    private def drawn(props: UI.Live[Props])(using Frame): UI < Sync =
        Signal.initRef(false).map(drawnWith(props, _))

    /** The field, drawn once over `props`, which hand it `value` bound: to the instance's own ref where a run binds nothing, as
      * the caller bound it otherwise. `revealed` is the reveal the eye flips.
      *
      * Each place reads the field it shows: the size, the variant, the width, `disabled` and `required` reach the field and the
      * wrapper through their class and attribute channels, and the reveal reaches the field's type through kyo-ui's `textType`
      * channel and the eye's classes, label and glyph through theirs, so a run that changes one of them, or a reveal, patches it
      * alone. What builds the field (the [[Base]], and the native `name` and `maxLength`, which have no channel) is one
      * `reactive` block, drawn again only when a run changes it. The meter is the one region, over the text, a sibling of the
      * field, so a keystroke draws only the meter. An input, a commit and a blur read the run's hearers when they come.
      */
    private def drawnWith(props: UI.Live[Props], revealed: SignalRef[Boolean])(using Frame): UI =
        val base            = props(Base(_))
        val disabled        = props(_.disabledFlag)
        val required        = props(_.requiredFlag)
        val size            = props(_.sizeV)
        val variant         = props(_.variantV)
        val fluid           = props(_.fluidFlag)
        val name            = props(_.nameV)
        val maxLength       = props(_.maxLengthV)
        val input           = props.hearing(_.valueSlot.hearer)
        val change          = props.hearing(_.onChangeSF)
        val blur            = props.hearing(_.onBlurSF)
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
                .when(b.readonly)(_.readonly(_))
                .required(required)
                .when(name.read)(_.name(_))
                .when(maxLength.read)(_.maxLength(_))
                .size(size)
                .variant(variant)
                .fluid(fluid)
                .invalid(validity.red)
                .withAccessibility(b.access)
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
            case 0 => ("p-password-meter-weak", 0.0, live(_.promptLabelV))
            case 1 => ("p-password-meter-weak", 100.0 / 3, live(_.weakLabelV))
            case 2 => ("p-password-meter-medium", 200.0 / 3, live(_.mediumLabelV))
            case _ => ("p-password-meter-strong", 100.0, live(_.strongLabelV))
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
