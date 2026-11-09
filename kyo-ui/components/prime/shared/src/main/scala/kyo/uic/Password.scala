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
      TextValued, HasPlaceholder, HasAccessibleName:

    type Self = Password

    private[uic] def withPlaceholder(v: Maybe[TextValue]): Password = new Password(p.copy(placeholder = v))

    /** Native `maxlength`: caps the number of characters; a negative cap is 0. */
    def maxLength(n: Int): Password = new Password(p.copy(maxLength = math.max(0, n)))

    private[uic] def validitySlots: FieldValidity.Slots         = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new Password(p.copy(validity = v))

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new Password(p.copy(access = v))

    /** Fires on a commit (native `change`) with the field's text. It hears the value too, so a signal bound one way and heard
      * here is no mistake.
      */
    def onChange(f: String => UI.Action)(using Frame): Password =
        new Password(p.copy(onChange = Present(Callback.of(f)), value = p.value.heardToo))

    /** The meter is drawn from the text and the eye keeps a reveal, so with either on the field is an instance, one per place,
      * drawn once: it holds the text (from a constant, and with the meter unbound too) and the reveal, and a host region that runs
      * again hands it the run's props instead of starting it over. The field is built once over a small value of the structural
      * props (what binds the text, the switches, the attributes, never a hearer), its text always a channel, its type a channel
      * over the reveal, and only the meter is a region, over the text. Unbound without the meter or the eye, the field keeps
      * nothing, draws no value and leaves the text to the browser, so it has no instance.
      */
    private[uic] def render(using Frame): UI =
        if !Password.keepsOwnText(p) && !p.toggleMask then Password.drawn(new UI.Live(p, Absent), Absent, Absent)
        else UI.mountedWith(p)(Password.instance(_, Absent))

    /** The instance with its reveal handed in (golden-test seam): the internal reveal state driving the password/text type
      * switch.
      */
    private[uic] def wired(revealed: SignalRef[Boolean])(using Frame): UI =
        UI.mountedWith(p)(Password.instance(_, Present(revealed)))
end Password

object Password:
    /** What a password field holds. Each field is a setter of its name (`uic.Password().size(Size.Small)`), which takes the
      * field's own type, the value itself where it is an `Opt` and the function itself for a callback; `value` binds and `onInput`
      * (`@eventName`) hears it; `maxLength` and `onChange` are written by hand, and the fields marked `@noSetter` by the shared
      * traits.
      */
    final private[uic] case class Props(
        /** The value. A `SignalRef` binds TWO-WAY: edits write back, ref changes update the field (and the strength meter when
          * `feedback(true)`). Any other signal binds ONE-WAY: the field and the meter show it, and an edit writes nothing and only
          * reports through `onInput` and `onChange`; with neither, a card says the reader cannot change it. A constant is the
          * starting text. With the meter or the mask toggle on, the field keeps the text itself, from a constant and, with the
          * meter, unbound too: the meter judges what the reader typed, and a reveal keeps it. `ref.readOnly` opts a ref out. Its
          * event `onInput` fires with the text on every keystroke.
          */
        @eventName("onInput") value: Controlled[String] = Controlled(),
        /** Adds the eye toggle that reveals/masks the typed value (Prime's `toggleMask`). */
        toggleMask: Boolean = false,
        /** Shows the strength meter below the field (inline; see the class doc). */
        feedback: Boolean = false,
        /** Meter label while the value is empty (Prime default "Enter a password"). A signal changes it in place. */
        promptLabel: Prop[String] = "Enter a password",
        /** Meter label for weak values (Prime default "Weak"). A signal changes it in place. */
        weakLabel: Prop[String] = "Weak",
        /** Meter label for medium values (Prime default "Medium"). A signal changes it in place. */
        mediumLabel: Prop[String] = "Medium",
        /** Meter label for strong values (Prime default "Strong"). A signal changes it in place. */
        strongLabel: Prop[String] = "Strong",
        @noSetter placeholder: Maybe[TextValue] = Absent,
        /** Native `disabled` on the field, which then takes no focus and no input; the eye answers no press. */
        disabled: Boolean = false,
        /** Native `readonly`, so the field is focusable but not editable. A signal toggles it in place through kyo-ui's boolean
          * attribute channel (no re-render).
          */
        readonly: Prop[Boolean] = false,
        /** Marks the field required (native constraint + `aria-required`). */
        required: Boolean = false,
        /** Native `name` for HTML form participation. */
        name: Opt[String] = Opt.unset,
        maxLength: Opt[Int] = Opt.unset,
        /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the field. */
        size: Size = Size.Normal,
        /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is default. */
        variant: FieldVariant = FieldVariant.Outlined,
        /** Spans the full container width (`.p-password-fluid`). */
        fluid: Boolean = false,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        onChange: Maybe[String => UI.Action] = Absent,
        /** Fires on focus loss (native `blur`) with the text the field holds as focus leaves it: what the reader typed, also
          * unbound or from a constant, with the meter and the reveal on or off. Unlike `onChange` it fires even when the value was
          * not edited. The form-validation layer wires its `Blur` trigger here.
          */
        onBlur: Maybe[String => UI.Action] = Absent
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
        readonly: Prop[Boolean],
        required: Boolean,
        name: Maybe[String],
        maxLength: Maybe[Int],
        size: Size,
        variant: FieldVariant,
        fluid: Boolean,
        validity: FieldValidity.Slots,
        access: Accessibility,
        placeholder: Maybe[TextValue],
        text: Maybe[Prop[String]],
        hearsInput: Boolean,
        hearsChange: Boolean,
        hearsBlur: Boolean
    ) derives CanEqual

    private object Base:
        def of(q: Props, own: Maybe[SignalRef[String]]): Base =
            // The instance's own text where the text is the field's to keep and the caller bound nothing or only where it starts.
            val text: Maybe[Prop[String]] =
                own.filter(_ => keepsOwnText(q) && q.value.binding.forall(_.const.isDefined)).orElse(q.value.bound)
            Base(
                q.toggleMask,
                q.feedback,
                q.disabled,
                q.readonly,
                q.required,
                q.name,
                q.maxLength,
                q.size,
                q.variant,
                q.fluid,
                q.validity,
                q.access,
                q.placeholder,
                text,
                q.value.hearer.isDefined,
                q.onChange.isDefined,
                q.onBlur.isDefined
            )
        end of
    end Base

    /** Whether the field draws something from its text after the page is shown (the meter, or the reveal) that the reader can
      * change.
      */
    private def keepsText(q: Props): Boolean =
        (q.feedback || q.toggleMask) && !q.disabled && !ReactiveValue[Boolean](q.readonly).const.contains(true)

    /** Whether the text the reader types is the field's to keep: with the meter even unbound, and from a constant. */
    private def keepsOwnText(q: Props): Boolean = keepsText(q) && (q.feedback || q.value.bound.isDefined)

    /** Where the field's own text starts: a constant the caller gave, else empty. */
    private def startText(q: Props): String = q.value.binding.flatMap(_.const).getOrElse("")

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
        val base = live(Base.of(_, own))
        UI.reactive(built(live, base.read, reveal))

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
            case Present(r) => in.inputType(r.map(on => if on then UI.TextType.Text else UI.TextType.Password))
            case Absent     => in.inputType(InputType.Password)
        b.text.foreach(t => in = in.value(t))
        // Each hearer is read when its event comes, so it is the latest run's.
        if b.hearsInput then in = in.onInput(s => live.call(_.value.hearer).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        if b.hearsChange then in = in.onChange(s => live.call(_.onChange).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        if b.hearsBlur then in = in.onBlur(s => live.call(_.onBlur).map(_.fold(Kyo.unit: UI.Action)(_(s))))
        in = in.withPlaceholder(b.placeholder)
        if b.disabled then in = in.disabled(true)
        in = in.readonly(b.readonly)
        if b.required then in = in.required(true)
        b.name.foreach(v => in = in.name(v))
        b.maxLength.foreach(n => in = in.maxLength(n))
        b.access.name.foreach(v => in = in.accessibleName(v))
        b.access.id.foreach(v => in = in.id(v))

        // === the eye toggle, its state classes and label channels over the reveal ==
        val toggle: Maybe[UI] = eye.map { r =>
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
            t(GlyphSvg(r.map(on => if on then Icons.eyeSlash else Icons.eye), "p-icon"))
        }

        // === wrapper root =======================================================
        var el = div
            .cssClass("p-password")
            .cssClass("p-component")
            .cssClass("p-inputwrapper")
            .cssClass("p-inputwrapper-filled", UI.reactive(shown.read.nonEmpty))
        if b.fluid then el = el.cssClass("p-password-fluid")
        val root = el(UI.children(in: UI, toggle)*)

        // === inline strength meter (see the class doc for the deviation), the one region, over the text ==
        val body = if b.feedback then fragment(root, UI.reactive(meter(live, shown.read))) else root
        validity.follow(body)
    end built

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
