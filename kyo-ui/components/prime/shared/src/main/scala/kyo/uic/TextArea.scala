package kyo.uic

import kyo.*
import kyo.UI.*

/** Multi-line text input: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Textarea: the component IS the bare
  * `<textarea class="p-textarea">`, no wrapper element). Composes the kyo `<textarea>` element, so two-way binding
  * (`value(SignalRef)` to `Bound.Ref`), `onInput`/`onChange`, and the caret-preserving morph path are all inherited from kyo-ui,
  * not rebuilt.
  *
  * The text is the reader's, as in [[Input]]: unbound, the browser keeps what was typed, and a constant is where it starts,
  * which the instance holds from there, so a host region that runs again keeps the reader's text. Any other `Signal[String]`
  * binds one way: the field shows it, and an edit only reports through `onInput`/`onChange`. Validation is Prime's
  * `.p-invalid` model: `invalid` plus the optional `invalidMessage` row below the field.
  */
final class TextArea private[uic] (private[uic] val p: TextArea.Props) extends Component(Setters[TextArea.Props, TextArea]),
      Node, FormControl[String], HasPlaceholder, HasAccessibleNameRef, HasAccessibleDescription:

    type Self = TextArea

    /** Package-internal class hook: wrappers (FloatLabel/IconField) stamp Prime's state classes (`p-filled`, iconfield padding)
      * onto the field they wrap.
      */
    private[uic] def extraClass(cls: String): TextArea = new TextArea(p.copy(extraClasses = p.extraClasses :+ cls))

    private[uic] def accessibility: Accessibility                                = p.access
    private[uic] def withAccessibility(v: Accessibility): Self                   = new TextArea(p.copy(access = v))
    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): TextArea           = new TextArea(p.copy(placeholder = v))
    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): TextArea = new TextArea(p.copy(description = v))
    private[uic] def validitySlots: FieldValidity.Slots                          = p.validity
    private[uic] def withValidity(v: FieldValidity.Slots): Self                  = new TextArea(p.copy(validity = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): TextArea      = this.value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): TextArea     = this.onBlur(f)

    /** Fires with the text the field holds when the reader commits it (focus leaving after an edit). It hears the value too, so a
      * one-way binding with only an `onChange` says nothing.
      */
    def onChange(f: String => UI.Action)(using Frame): TextArea =
        new TextArea(p.copy(onChange = Present(Callback.of(f)), value = p.value.heardToo))

    /** One instance per place where a constant gives the text its start: it holds the reader's text from there, and a host region
      * that runs again hands it the run's props instead of starting it over. Unbound or bound to a signal the field keeps nothing
      * of its own (the browser keeps an unbound text), so it is drawn directly (see [[TextArea.drawn]]).
      */
    private[uic] def render(using Frame): UI =
        if p.value.start.isDefined then UI.mountedWith(p)(TextArea.drawn) else TextArea.drawn(new UI.Live(p, Absent))
end TextArea

object TextArea:
    /** What a text area holds. Each field is a setter of its name (`uic.TextArea().rows(4)`), which takes a constant or a signal
      * where the field is a `Prop`, the value itself where it is an `Opt` and the function itself for a callback; `value` binds
      * and `onInput` (`@eventName`) hears it; `onChange` is written by hand, and the fields marked `@noSetter` by hosts and the
      * shared traits.
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
        /** The number of rows the field shows at least (one at the fewest): kyo's `<textarea>` has no native `rows`, so it is
          * written as `data-rows` and drawn as a minimum height of 22px per row. A signal patches both in place.
          */
        rows: Opt[Prop[Int]] = Opt.unset,
        /** Native `disabled`. A signal toggles it in place (no re-render, so caret and focus survive). */
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
        /** Prime `autoResize`: the field grows with its content (`.p-textarea-resizable` and the CSS `field-sizing: content`
          * remainder class `.p-uic-autoresize`; no JS measuring). A signal toggles both classes in place.
          */
        autoResize: Prop[Boolean] = false,
        /** Size: `.p-textarea-sm` / default / `.p-textarea-lg`, a signal swapping the class in place. */
        size: Prop[Size] = Size.Normal,
        /** Fill variant: `Filled` renders `.p-variant-filled`, `Outlined` is the default; a signal swaps the class in place. */
        variant: Prop[FieldVariant] = FieldVariant.Outlined,
        /** Spans the full width of its container (`.p-textarea-fluid`), a signal toggling the class in place. */
        fluid: Prop[Boolean] = false,
        onChange: Maybe[String => UI.Action] = Absent,
        /** Fires on focus loss (native `blur`) with the text the field holds as focus leaves it, which kyo-ui's blur carries: what
          * the reader typed, also when nothing binds the field or it started from a constant. Unlike `onChange` it fires even when
          * the value was not edited. The form-validation layer wires its `Blur` trigger here.
          */
        onBlur: Maybe[String => UI.Action] = Absent,
        @noSetter validity: FieldValidity.Slots = FieldValidity.Slots.empty,
        @noSetter access: Accessibility = Accessibility.none,
        @noSetter description: Maybe[Prop[String]] = Absent,
        @noSetter extraClasses: List[String] = Nil
    ) derives CanEqual

    def apply(): TextArea = new TextArea(Props())

    private val valueName = Controlled.Name("TextArea", "value", "a change", "onInput or onChange")

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
        validity: FieldValidity.Slots,
        access: Accessibility,
        description: Maybe[Prop[String]],
        extraClasses: List[String]
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
                q.validity,
                q.access,
                q.description,
                q.extraClasses
            )
    end Base

    /** The field, drawn once over `props`, which hand it `value` bound: to the instance's own ref where a constant starts it, as
      * the caller bound it otherwise, and not at all where nothing is bound and the browser keeps the text.
      *
      * Each place reads the field it shows: the size, the variant, the width, `autoResize`, the rows, `disabled`, `readonly` and
      * `required` reach the element through its class, attribute, inline-style and `aria-*` channels, so a run that changes one
      * of them patches it alone. What builds the element (the [[Base]], `name` and `maxLength`, which have no channel) is one
      * `reactive` block, drawn again only when a run changes it. An input, a commit and a blur read the run's hearers when they
      * come. The card for a signal nobody hears is a block of its own over `disabled` and `readonly`, so it shows only while the
      * reader could edit.
      */
    private def drawn(props: UI.Live[Props])(using Frame): UI =
        val base     = props(Base(_))
        val disabled = props(_.disabled)
        val readonly = props(_.readonly)
        val required = props(_.required)
        // aria-required says only that the field is required: unset, the attribute is left out, as the native one is.
        val requiredAria = Prop.map[Boolean, Maybe[Boolean]](required)(r => if r then Present(true) else Absent)
        val rows         = Prop.map[Maybe[Int], Maybe[Int]](props(_.rows))(_.map(math.max(1, _)))
        // The native attributes have no channel: a signal among them is read where the field is built, so it draws the field again.
        val name      = props(_.name)
        val maxLength = props(_.maxLength)
        val input     = props.hearing(_.value.hearer)
        val change    = props.hearing(_.onChange)
        val blur      = props.hearing(_.onBlur)
        UI.reactive {
            val b        = base.read
            val validity = FieldValidity(b.validity)
            val field    = validity.mark(
                b.extraClasses.foldLeft(textarea.cssClass("p-textarea").cssClass("p-component"))(_.cssClass(_))
                    .when(b.access.id)(_.id(_))
                    .cssClass.matching(props(_.size)) {
                        case Size.Small  => "p-textarea-sm"
                        case Size.Normal => Absent
                        case Size.Large  => "p-textarea-lg"
                    }
                    .cssClass.matching(props(_.variant)) {
                        case FieldVariant.Outlined => Absent
                        case FieldVariant.Filled   => "p-variant-filled"
                    }
            )
                .when(props(_.fluid))(_.cssClass("p-textarea-fluid"))
                .when(props(_.autoResize))(_.cssClass("p-textarea-resizable", "p-uic-autoresize"))
                // Prime's `p-filled` follows the text the field holds, which the client knows and a render does not (an unbound
                // field's text, the reader's edit): kyo-ui toggles it there, the static render from the start.
                .filledClass("p-filled")
                .when(b.text)(_.value(_))
                .placeholder(b.placeholder)
                .data(data"data-rows" -> rows)
                .inlineStyle(Prop.map[Maybe[Int], Style](rows)(_.fold(Style.empty)(n => Style.minHeight((n * 22).px))))
                .disabled(disabled)
                .readOnly(readonly)
                .required(required)
                .aria(Aria.required -> requiredAria)
                .when(name.read)(_.jsProp("name", _))
                .when(maxLength.read)((e, n) => e.jsProp("maxLength", math.max(0, n).toString))
                .aria(
                    Aria.label       -> b.access.name,
                    Aria.labelledby  -> b.access.nameRef,
                    Aria.description -> b.description
                )
                .when(b.hearsInput)(_.onInput(input))
                .when(b.hearsChange)(_.onChange(change))
                // The blur carries the text the field holds as focus leaves it: what the reader typed, whatever binds the field.
                .when(b.blurs)(_.onBlur((_, text) => blur(text)))
            val followed = validity.follow(field)
            if b.unheard then fragment(followed, unheardCard(disabled, readonly)) else followed
        }
    end drawn

    /** Only a signal nobody hears is a mistake worth a card, and only while the reader could edit. Its own block, so the flags it
      * reads stay channels of the field.
      */
    private def unheardCard(disabled: Prop[Boolean], readonly: Prop[Boolean])(using Frame): UI =
        UI.reactive(fragment(Diagnostics.cards(valueName.component)(Diagnostic(!readonly.read && !disabled.read, valueName.advice))*))
end TextArea
