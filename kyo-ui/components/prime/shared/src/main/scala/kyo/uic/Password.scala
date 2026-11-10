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
      * it. A constant is the starting text. With the meter or the mask toggle on, the field keeps the text itself, from a
      * constant and, with the meter, unbound too: the meter judges what the reader typed, and a reveal keeps it.
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

    /** The meter is drawn from the text and the eye keeps a reveal, so with either on the field is an instance, one per place,
      * drawn once: it holds the text (from a constant, and with the meter unbound too) and the reveal, and a host region that runs
      * again hands it the run's props instead of starting it over. The field is built once over a small value of the structural
      * props (what binds the text, the switches, the attributes, never a hearer), its text always a channel, its type a channel
      * over the reveal, and only the meter is a region, over the text. Unbound without the meter or the eye, the field keeps
      * nothing, draws no value and leaves the text to the browser, so it has no instance.
      */
    private[uic] def render(using Frame): UI =
        if !Password.keepsOwnText(p) && !toggleMaskFlag then Password.drawn(new UI.Live(p, Absent), Absent, Absent)
        else UI.mountedWith(p)(Password.instance(_, Absent))

    /** The instance with its reveal handed in (golden-test seam): the internal reveal state driving the password/text type
      * switch.
      */
    private[uic] def wired(revealed: SignalRef[Boolean])(using Frame): UI =
        UI.mountedWith(p)(Password.instance(_, Present(revealed)))
end Password

object Password:
    final private[uic] case class Props(
        valueSlot: Controlled[String] = Controlled(),
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

    /** What builds the field and has no channel, read as one value: a run that only brings new hearers or new labels leaves it
      * equal, so the field is not built again. `text` is what the field shows: the caller's signal, the instance's own text, a
      * constant to start from, or nothing.
      */
    final private case class Base(
        toggleMask: Boolean,
        feedback: Boolean,
        disabled: Boolean,
        readonly: Maybe[Prop[Boolean]],
        required: Boolean,
        name: Maybe[String],
        maxLength: Maybe[Int],
        size: Size,
        variant: FieldVariant,
        fluid: Boolean,
        validity: FieldValidity.Slots,
        access: Accessibility,
        placeholder: Maybe[Prop[String]],
        text: Maybe[Prop[String]],
        hearsInput: Boolean,
        hearsChange: Boolean,
        hearsBlur: Boolean
    ) derives CanEqual

    private object Base:
        def of(q: Props, own: Maybe[SignalRef[String]]): Base =
            val text = (own, q.valueSlot.bound) match
                case (Present(ref), bound) if keepsOwnText(q) && bound.forall(constant) => Present(ref)
                case (_, bound)                                                         => bound
            Base(
                q.toggleMaskFlag,
                q.feedbackFlag,
                q.disabledFlag,
                q.readonlyFlag,
                q.requiredFlag,
                q.nameV,
                q.maxLengthV,
                q.sizeV,
                q.variantV,
                q.fluidFlag,
                q.validityV,
                q.access,
                q.placeholderText,
                text,
                q.valueSlot.hearer.isDefined,
                q.onChangeSF.isDefined,
                q.onBlurSF.isDefined
            )
        end of
    end Base

    /** Whether the field draws something from its text after the page is shown (the meter, or the reveal) that the reader can
      * change.
      */
    private def keepsText(q: Props): Boolean = (q.feedbackFlag || q.toggleMaskFlag) && !q.disabledFlag && !q.readonlyFlag.exists {
        case on: Boolean => on
        case _           => false
    }

    /** Whether the text the reader types is the field's to keep: with the meter even unbound, and from a constant. */
    private def keepsOwnText(q: Props): Boolean = keepsText(q) && (q.feedbackFlag || q.valueSlot.bound.isDefined)

    /** Whether a bound text is a constant, which is only where the field's own text starts. */
    private def constant(v: Prop[String]): Boolean = v match
        case _: Signal[?] => false
        case _            => true

    /** Where the field's own text starts: a constant the caller gave, else empty. */
    private def startText(q: Props): String =
        q.valueSlot.bound match
            case Present(a: String) => a
            case _                  => ""

    /** The instance's state, minted once: its own text and its reveal (the one handed in, for the golden seam). */
    private def instance(live: UI.Live[Props], revealed: Maybe[SignalRef[Boolean]])(using Frame): UI < Sync =
        for
            start  <- live.call(startText)
            own    <- Signal.initRef(start)
            reveal <- revealed match
                case Present(r) => Kyo.lift(r)
                case Absent     => Signal.initRef(false)
        yield drawn(live, Present(own), Present(reveal))

    private def drawn(live: UI.Live[Props], own: Maybe[SignalRef[String]], reveal: Maybe[SignalRef[Boolean]])(using Frame): UI =
        Prop.render(live(Base.of(_, own)))(built(live, _, reveal))

    private def built(live: UI.Live[Props], b: Base, reveal: Maybe[SignalRef[Boolean]])(using Frame): UI =
        val validity            = FieldValidity(b.validity)
        val shown: Prop[String] = b.text.getOrElse("")
        val eye                 = if b.toggleMask then reveal else Absent

        // === the field (Input carries the shared text-field contract) ===========
        // The wrapper's `p-inputwrapper-filled` follows the text the field holds: kyo-ui's client toggles it as the reader
        // types (the static render cannot reach the wrapper, so the class below stamps the starting text).
        var in = Input()
            .extraClass("p-password-input")
            .filledOn("p-inputwrapper-filled", ".p-password")
            .size(b.size)
            .variant(b.variant)
            .fluid(b.fluid)
            .invalid(validity.red)
            .unheardAs("Password", "onInput or onChange")
        in = eye match
            case Present(r) => in.revealing(r.map(on => if on then UI.TextType.Text else UI.TextType.Password))
            case Absent     => in.inputType(InputType.Password)
        b.text.foreach(t => in = in.value(t))
        // Each hearer is read when its event comes, so it is the latest run's.
        if b.hearsInput then in = in.onInput(s => live.call(_.valueSlot.hearer).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        if b.hearsChange then in = in.onChange(s => live.call(_.onChangeSF).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        if b.hearsBlur then in = in.onBlur(s => live.call(_.onBlurSF).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        in = in.withPlaceholder(b.placeholder)
        if b.disabled then in = in.disabled(true)
        b.readonly.foreach(r => in = in.readonly(r))
        if b.required then in = in.required(true)
        b.name.foreach(v => in = in.name(v))
        b.maxLength.foreach(n => in = in.maxLength(n))
        b.access.name.foreach(v => in = in.accessibleName(v))
        b.access.id.foreach(v => in = in.id(v))

        // === the eye toggle, its state classes and label channels over the reveal ==
        val toggle: List[UI] = eye.toList.map { r =>
            var t = span
                .cssClass("p-password-toggle-mask-icon")
                .cssClass("p-password-mask-icon", r)
                .cssClass("p-password-unmask-icon", r.map(!_))
                .role("button")
                .tabIndex(0)
                .aria("label", r.map(on => if on then "Hide password" else "Show password"))
            if !b.disabled then
                val flip: UI.Action = r.getAndUpdate(!_).unit
                t = t.onClick(flip).onKeyDown { e =>
                    e.key match
                        case Keyboard.Enter | Keyboard.Space => flip
                        case _                               => ()
                }
            end if
            t(r.render(on => GlyphSvg(if on then Icons.eyeSlash else Icons.eye, "p-icon")))
        }

        // === wrapper root =======================================================
        var el = div
            .cssClass("p-password")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
            .cssClass("p-inputwrapper-filled", Prop.map[String, Boolean](shown)(_.nonEmpty))
        if b.fluid then el = el.cssClass("p-password-fluid")
        val root = el(UI.children((in: UI) :: toggle)*)

        // === inline strength meter (see the class doc for the deviation), the one region, over the text ==
        val body = if b.feedback then fragment(root, Prop.render(shown)(meter(live, _))) else root
        validity.follow(body)
    end built

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
