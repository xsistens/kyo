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
  * (mounted `SignalRef[Boolean]`); kyo's caret-preserving morph keeps focus
  * and caret across the type switch.
  *
  * `feedback(true)` renders the strength meter (`.p-password-meter` +
  * `.p-password-meter-label` with Prime's `-weak`/`-medium`/`-strong` classes
  * inside a `.p-password-content` block). Deliberate deviation from Prime: the
  * meter renders INLINE below the field, not in a while-typing overlay: a
  * floated panel adds focus/dismiss machinery for zero information gain on a
  * server-driven page. Strength is computed on the server from the bound value
  * with Prime's OWN default rules: medium = two of [lowercase, uppercase,
  * digit] and â¥ 6 chars; strong = all three and â¥ 8 chars; anything else weak;
  * empty shows `promptLabel`.
  */
final class Password private[uic] (private[uic] val p: Password.Props) extends Node, TextFormControl, HasPlaceholder, HasAccessibleName,
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

    /** Meter label while the value is empty (Prime default "Enter a password"). A `Signal[String]` re-
      * renders it in place on emission.
      */
    def promptLabel(v: String | Signal[String]): Password = new Password(p.copy(promptLabelV = ReactiveValue(v)))

    /** Meter label for weak values (Prime default "Weak"). A `Signal[String]` re-renders it in place on
      * emission.
      */
    def weakLabel(v: String | Signal[String]): Password = new Password(p.copy(weakLabelV = ReactiveValue(v)))

    /** Meter label for medium values (Prime default "Medium"). A `Signal[String]` re-renders it in place on
      * emission.
      */
    def mediumLabel(v: String | Signal[String]): Password = new Password(p.copy(mediumLabelV = ReactiveValue(v)))

    /** Meter label for strong values (Prime default "Strong"). A `Signal[String]` re-renders it in place on
      * emission.
      */
    def strongLabel(v: String | Signal[String]): Password           = new Password(p.copy(strongLabelV = ReactiveValue(v)))
    private[uic] def withPlaceholder(v: Maybe[TextValue]): Password = new Password(p.copy(placeholderText = v))

    def disabled(v: Boolean): Password = new Password(p.copy(disabledFlag = v))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles it IN
      * PLACE via kyo-ui's boolean attribute channel (no re-render).
      */
    def readonly(v: Boolean | Signal[Boolean]): Password = new Password(p.copy(readonlyFlag = Present(ReactiveValue(v))))

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

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new Password(p.copy(validityV = v))

    private[uic] def accessibility: Accessibility              = accessV
    private[uic] def withAccessibility(v: Accessibility): Self = new Password(p.copy(accessV = v))

    def onChange(f: String => UI.Action)(using Frame): Password =
        new Password(p.copy(onChangeSF = Present(Callback.of(f)), valueSlot = valueSlot.heardToo))
    def onInput(f: String => UI.Action)(using Frame): Password = new Password(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fires on focus loss (native `blur`) with the text the field holds as focus
      * leaves it: what the reader typed, also unbound or from a constant, with the
      * meter and the reveal on or off. Unlike `onChange` it fires even when the value
      * was not edited. The form-validation layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => UI.Action)(using Frame): Password = new Password(p.copy(onBlurSF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        // The meter is drawn from the text and the eye's reveal draws the field again, so with either on the text the reader
        // types is the field's to keep: from a constant (which a reveal would otherwise give back), and with the meter unbound
        // too. Unbound without the meter, the field draws no value and the text stays the browser's. With a mask toggle the one mount
        // keeps the reveal beside the text.
        val keepsOwnText = keepsText && (feedbackFlag || valueSlot.binding.isDefined)
        if !toggleMaskFlag then
            Slot.owningFrom(valueSlot, unbound = "", on = ReactiveValue.Const(keepsOwnText)) { slot =>
                val field = new Password(p.copy(valueSlot = slot))
                field.validated(field.withValue(Absent, _))
            }
        else
            UI.mounted {
                for
                    value  <- if keepsOwnText then valueSlot.ownedFrom("") else Kyo.lift(valueSlot)
                    reveal <- Signal.initRef(false)
                yield new Password(p.copy(valueSlot = value)).wired(reveal)
            }
        end if
    end render

    /** Whether the field draws something from its text after the page is shown (the meter, or the reveal) that the
      * reader can change.
      */
    private def keepsText: Boolean = (feedbackFlag || toggleMaskFlag) && !readonlyFlag.constTrue && !disabledFlag

    /** The subscription tree the mount publishes (golden-test seam): the internal
      * reveal state driving the password/text type switch.
      */
    private[uic] def wired(revealed: SignalRef[Boolean])(using Frame): UI =
        validated(validity => revealed.render(r => withValue(Present((revealed, r)), validity)))

    /** The field built once with its validity handed to `build`, followed by the message row. */
    private def validated(build: FieldValidity => UI)(using Frame): UI =
        val validity = FieldValidity(validityV)
        validity.follow(build(validity))

    /** Resolves the bound value: the meter needs the CURRENT text, so with
      * `feedback(true)` the body renders under the signal; without feedback a
      * two-way ref binds directly and a one-way signal is shown in place (no
      * re-render per keystroke), and kyo-ui puts back an edit its owner did not take.
      */
    private def withValue(rev: Maybe[(SignalRef[Boolean], Boolean)], validity: FieldValidity)(using Frame): UI =
        valueSlot.binding match
            case Present(ReactiveValue.Dyn(_)) if !feedbackFlag => body("", rev, validity)
            case Present(ReactiveValue.Dyn(sig))                => sig.render(v => body(v, rev, validity))
            case Present(ReactiveValue.Const(v))                => body(v, rev, validity)
            case _                                              => body("", rev, validity)

    private def body(current: String, rev: Maybe[(SignalRef[Boolean], Boolean)], validity: FieldValidity)(using Frame): UI =
        val revealed = rev.exists(_._2)

        // === the field (Input carries the shared text-field contract) ===========
        // The wrapper's `p-inputwrapper-filled` follows the text the field holds: kyo-ui's client toggles it as the reader
        // types (the static render cannot reach the wrapper, so the class below stamps the starting text).
        var in = Input()
            .extraClass("p-password-input")
            .filledOn("p-inputwrapper-filled", ".p-password")
            .inputType(if revealed then InputType.Text else InputType.Password)
            .size(sizeV)
            .variant(variantV)
            .fluid(fluidFlag)
            .invalid(validity.red)
            .unheardAs("Password", "onInput or onChange")
        valueSlot.binding match
            case Present(ReactiveVariable(r)) if !feedbackFlag => in = in.value(r)
            case Present(ReactiveVariable(_))                  => in = in.value(current)
            case Present(bound)                                => in = in.value(bound.bound)
            case _                                             => ()
        end match
        // Under feedback the field re-renders per keystroke (the meter observes the
        // ref), so the ref writes back through the input handler instead of Bound.Ref.
        if feedbackFlag && valueSlot.writable.isDefined then in = in.onInput(s => valueSlot.change(s))
        else valueSlot.hearer.foreach(f => in = in.onInput(f))
        onChangeSF.foreach(f => in = in.onChange(f))
        onBlurSF.foreach(f => in = in.onBlur(f))
        placeholderText match
            case Present(TextValue.Const(v)) => in = in.placeholder(v)
            case Present(TextValue.Dyn(sig)) => in = in.placeholder(sig)
            case Absent                      => ()
        end match
        if disabledFlag then in = in.disabled(true)
        in = readonlyFlag.foldFlag(in)(in.readonly(_))
        if requiredFlag then in = in.required(true)
        nameV.foreach(v => in = in.name(v))
        maxLengthV.foreach(n => in = in.maxLength(n))
        accessV.nameV match
            case Present(TextValue.Const(v)) => in = in.accessibleName(v)
            case Present(TextValue.Dyn(s))   => in = in.accessibleName(s)
            case Absent                      => ()
        end match
        accessV.idV.foreach(v => in = in.id(v))

        // === the eye toggle =====================================================
        val toggle: List[UI] = rev.toList.map { (ref, isRevealed) =>
            var t = span
                .cssClass("p-password-toggle-mask-icon")
                .cssClass(if isRevealed then "p-password-mask-icon" else "p-password-unmask-icon")
                .role("button")
                .tabIndex(0)
                .aria("label", if isRevealed then "Hide password" else "Show password")
            if !disabledFlag then
                val flip: UI.Action = ref.set(!isRevealed)
                t = t.onClick(flip).onKeyDown { e =>
                    e.key match
                        case Keyboard.Enter | Keyboard.Space => flip
                        case _                               => ()
                }
            end if
            t(GlyphSvg(if isRevealed then Icons.eyeSlash else Icons.eye, "p-icon"))
        }

        // === wrapper root =======================================================
        var el = div
            .cssClass("p-password")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
        // Without the meter a ref binds the field directly and nothing draws again per keystroke, so the wrapper
        // follows the ref through a class channel; every other path draws from the text it renders.
        el = valueSlot.binding match
            case Present(ReactiveValue.Dyn(sig)) if !feedbackFlag => el.cssClass("p-inputwrapper-filled", sig.map(_.nonEmpty))
            case _                                                => el.cssClass("p-inputwrapper-filled", current.nonEmpty)
        if fluidFlag then el = el.cssClass("p-password-fluid")
        val root = el(UI.children((in: UI) :: toggle)*)

        // === inline strength meter (see the class doc for the deviation) ========
        val meter: List[UI] =
            if !feedbackFlag then Nil
            else
                val (cls, width, label) = Password.strength(current) match
                    case 0 => ("p-password-meter-weak", 0.0, promptLabelV)
                    case 1 => ("p-password-meter-weak", 100.0 / 3, weakLabelV)
                    case 2 => ("p-password-meter-medium", 200.0 / 3, mediumLabelV)
                    case _ => ("p-password-meter-strong", 100.0, strongLabelV)
                val labelText: UI = label match
                    case TextValue.Const(t) => div.cssClass("p-password-meter-text")(t)
                    case TextValue.Dyn(s)   => div.cssClass("p-password-meter-text")(s)
                List(
                    div.cssClass("p-password-content").cssClass("p-uic-password-feedback")(
                        div.cssClass("p-password-meter")(
                            div.cssClass("p-password-meter-label").cssClass(cls).style(_.width(width.pct))
                        ),
                        labelText
                    )
                )

        if meter.isEmpty then root else fragment((root :: meter)*)
    end body
end Password

object Password:
    final private[uic] case class Props(
        valueSlot: Slot[String] = Slot(),
        toggleMaskFlag: Boolean = false,
        feedbackFlag: Boolean = false,
        promptLabelV: TextValue = TextValue.Const("Enter a password"),
        weakLabelV: TextValue = TextValue.Const("Weak"),
        mediumLabelV: TextValue = TextValue.Const("Medium"),
        strongLabelV: TextValue = TextValue.Const("Strong"),
        placeholderText: Maybe[TextValue] = Absent,
        disabledFlag: Boolean = false,
        readonlyFlag: Maybe[BoolValue] = Absent,
        requiredFlag: Boolean = false,
        nameV: Maybe[String] = Absent,
        maxLengthV: Maybe[Int] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        accessV: Accessibility = Accessibility.none,
        onChangeSF: Maybe[String => UI.Action] = Absent,
        onBlurSF: Maybe[String => UI.Action] = Absent
    )

    def apply(): Password = new Password(Props())

    /** Prime's default strength rules as pure predicates: 0 = empty (prompt),
      * 1 = weak, 2 = medium (two of [a-z, A-Z, 0-9] and length â¥ 6),
      * 3 = strong (all three and length â¥ 8).
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
