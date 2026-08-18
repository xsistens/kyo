package kyo.uic

import kyo.*
import kyo.UI.*

/** Password — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * meter renders INLINE below the field, not in a while-typing overlay — a
  * floated panel adds focus/dismiss machinery for zero information gain on a
  * server-driven page. Strength is computed on the server from the bound value
  * with Prime's OWN default rules: medium = two of [lowercase, uppercase,
  * digit] and ≥ 6 chars; strong = all three and ≥ 8 chars; anything else weak;
  * empty shows `promptLabel`.
  */
final case class Password private (
    valueBinding: Maybe[Input.Value] = Absent,
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
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    idV: Maybe[String] = Absent,
    onChangeSF: Maybe[String => Any < Async] = Absent,
    onInputSF: Maybe[String => Any < Async] = Absent,
    onBlurSF: Maybe[String => Any < Async] = Absent
) extends Node, TextFormControl:
    type Self = Password

    /** Sets a constant value. */
    def value(v: String): Password = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref`: edits write back, ref changes update the field
      * (and the strength meter when `feedback(true)`).
      */
    def value(ref: SignalRef[String]): Password = copy(valueBinding = Present(Input.Value.Ref(ref)))

    /** Adds the eye toggle that reveals/masks the typed value (Prime's `toggleMask`). */
    def toggleMask(v: Boolean): Password = copy(toggleMaskFlag = v)

    /** Shows the strength meter below the field (inline — see the class doc). */
    def feedback(v: Boolean): Password = copy(feedbackFlag = v)

    /** Meter label while the value is empty (Prime default "Enter a password"). */
    def promptLabel(v: String): Password = copy(promptLabelV = TextValue.Const(v))

    /** Reactive empty-value meter label tracking `sig` — re-renders in place on emission. */
    def promptLabel(sig: Signal[String]): Password = copy(promptLabelV = TextValue.Dyn(sig))

    /** Meter label for weak values (Prime default "Weak"). */
    def weakLabel(v: String): Password = copy(weakLabelV = TextValue.Const(v))

    /** Reactive weak-value meter label tracking `sig` — re-renders in place on emission. */
    def weakLabel(sig: Signal[String]): Password = copy(weakLabelV = TextValue.Dyn(sig))

    /** Meter label for medium values (Prime default "Medium"). */
    def mediumLabel(v: String): Password = copy(mediumLabelV = TextValue.Const(v))

    /** Reactive medium-value meter label tracking `sig` — re-renders in place on emission. */
    def mediumLabel(sig: Signal[String]): Password = copy(mediumLabelV = TextValue.Dyn(sig))

    /** Meter label for strong values (Prime default "Strong"). */
    def strongLabel(v: String): Password = copy(strongLabelV = TextValue.Const(v))

    /** Reactive strong-value meter label tracking `sig` — re-renders in place on emission. */
    def strongLabel(sig: Signal[String]): Password = copy(strongLabelV = TextValue.Dyn(sig))

    def placeholder(v: String): Password = copy(placeholderText = Present(TextValue.Const(v)))

    /** Reactive placeholder that tracks `sig` — patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render of the field) on each emission.
      */
    def placeholder(sig: Signal[String]): Password = copy(placeholderText = Present(TextValue.Dyn(sig)))
    def disabled(v: Boolean): Password             = copy(disabledFlag = v)

    /** Native `readonly` — focusable but not editable. */
    def readonly(v: Boolean): Password = copy(readonlyFlag = Present(BoolValue.Const(v)))

    /** Reactive readonly — toggled IN PLACE via kyo-ui's boolean attribute channel (no re-render). */
    def readonly(sig: Signal[Boolean]): Password = copy(readonlyFlag = Present(BoolValue.Dyn(sig)))

    /** Marks the field required (native constraint + `aria-required`). */
    def required(v: Boolean): Password = copy(requiredFlag = v)

    /** Native `name` for HTML form participation. */
    def name(v: String): Password = copy(nameV = Present(v))

    /** Native `maxlength` — caps the number of characters. */
    def maxLength(n: Int): Password = copy(maxLengthV = Present(math.max(0, n)))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field. */
    def size(v: Size): Password = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is default. */
    def variant(v: FieldVariant): Password = copy(variantV = v)

    /** Spans the full container width (`.p-password-fluid`). */
    def fluid(v: Boolean): Password = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Password = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)`. */
    def invalidMessage(v: String): Password = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): Password = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Password = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label` on the input. */
    def accessibleName(v: String): Password = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Password = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Native element `id` — pair with `Label.forId` / `FloatLabel.forId`. */
    def id(v: String): Password = copy(idV = Present(v))

    def onChange(f: String => Any < Async): Password = copy(onChangeSF = Present(f))
    def onInput(f: String => Any < Async): Password  = copy(onInputSF = Present(f))

    /** Fires on focus loss (native `blur`) with the field's current value — unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): Password = copy(onBlurSF = Present(f))

    private[uic] def render(using Frame): UI =
        if !toggleMaskFlag then withValue(Absent)
        else
            UI.mounted {
                Signal.initRef(false).map(r => wired(r))
            }.placeholder(withValue(Absent))

    /** The subscription tree the mount publishes (golden-test seam): the internal
      * reveal state driving the password/text type switch.
      */
    private[uic] def wired(revealed: SignalRef[Boolean])(using Frame): UI =
        revealed.render(r => withValue(Present((revealed, r))))

    /** Resolves the bound value: the meter needs the CURRENT text, so with
      * `feedback(true)` the body renders under `ref.render`; without feedback the
      * ref binds directly (no re-render per keystroke).
      */
    private def withValue(rev: Maybe[(SignalRef[Boolean], Boolean)])(using Frame): UI =
        valueBinding match
            case Present(Input.Value.Ref(ref)) if feedbackFlag =>
                ref.render(v => body(v, rev))
            case Present(Input.Value.Const(v)) => body(v, rev)
            case _                             => body("", rev)

    private def body(current: String, rev: Maybe[(SignalRef[Boolean], Boolean)])(using Frame): UI =
        // Reactive-invalid gate (INSIDE the mount subscription — never around the
        // UI.mounted node): with a reactive slot set, re-render field + meter + message
        // through the shared helper; otherwise the untouched static form.
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => bodyStatic(current, rev)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).bodyStatic(current, rev)
                )

    private def bodyStatic(current: String, rev: Maybe[(SignalRef[Boolean], Boolean)])(using Frame): UI =
        val revealed = rev.exists(_._2)

        // === the field (Input carries the shared text-field contract) ===========
        var in = Input()
            .extraClass("p-password-input")
            .inputType(if revealed then InputType.Text else InputType.Password)
            .size(sizeV)
            .variant(variantV)
            .fluid(fluidFlag)
            .invalid(invalidV.constTrue)
        valueBinding match
            case Present(Input.Value.Ref(r)) if !feedbackFlag => in = in.value(r)
            case Present(_)                                   => in = in.value(current)
            case Absent                                       => ()
        end match
        // Under feedback the field re-renders per keystroke (the meter observes the
        // ref), so the ref writes back through the input handler instead of Bound.Ref.
        valueBinding match
            case Present(Input.Value.Ref(r)) if feedbackFlag =>
                in = in.onInput { s =>
                    val user: Any < Async = onInputSF match
                        case Present(f) => f(s)
                        case Absent     => ()
                    for
                        _ <- r.set(s)
                        x <- user
                    yield x
                    end for
                }
            case _ =>
                onInputSF.foreach(f => in = in.onInput(f))
        end match
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
        accNameV match
            case Present(TextValue.Const(v)) => in = in.accessibleName(v)
            case Present(TextValue.Dyn(s))   => in = in.accessibleName(s)
            case Absent                      => ()
        end match
        idV.foreach(v => in = in.id(v))

        // === the eye toggle =====================================================
        val toggle: List[UI] = rev.toList.map { (ref, isRevealed) =>
            var t = span
                .cssClass("p-password-toggle-mask-icon")
                .cssClass(if isRevealed then "p-password-mask-icon" else "p-password-unmask-icon")
                .role("button")
                .tabIndex(0)
                .aria("label", if isRevealed then "Hide password" else "Show password")
            if !disabledFlag then
                val flip: Any < Async = ref.set(!isRevealed)
                t = t.onClick(flip).onKeyDown { e =>
                    e.key match
                        case Keyboard.Enter | Keyboard.Space => flip
                        case _                               => ()
                }
            end if
            t(toChild(GlyphSvg(if isRevealed then Icons.eyeSlash else Icons.eye, "p-icon")))
        }

        // === wrapper root =======================================================
        var el = div
            .cssClass("p-password")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
        if current.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if fluidFlag then el = el.cssClass("p-password-fluid")
        val root = el(((in: UI) :: toggle).map(toChild)*)

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
                    case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-password-meter-text")(t))
                List(
                    div.cssClass("p-password-content").cssClass("p-uic-password-feedback")(
                        div.cssClass("p-password-meter")(
                            div.cssClass("p-password-meter-label").cssClass(cls).style(_.width(width.pct))
                        ),
                        labelText
                    )
                )

        val withMeter: UI =
            if meter.isEmpty then root else fragment((root :: meter)*)
        FieldInvalid.withMessage(withMeter, invalidV.constTrue, invalidMsgV)
    end bodyStatic
end Password

object Password:
    def apply(): Password = new Password()

    /** Prime's default strength rules as pure predicates: 0 = empty (prompt),
      * 1 = weak, 2 = medium (two of [a-z, A-Z, 0-9] and length ≥ 6),
      * 3 = strong (all three and length ≥ 8).
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
