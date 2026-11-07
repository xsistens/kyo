package kyo.uic

import kyo.*
import kyo.UI.*

/** Multi-line text input: native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's Textarea: the component IS the bare
  * `<textarea class="p-textarea">`). Closely mirrors [[Input]] but composes the
  * kyo `<textarea>` factory, so two-way binding (`value(SignalRef)` →
  * `Bound.Ref`), `onInput`/`onChange`, and the caret-preserving morph path are
  * all inherited. The `value` binding is [[Input]]'s: a `SignalRef` two-way, any other signal one way.
  */
final class TextArea private[uic] (private[uic] val p: TextArea.Props) extends Node, TextFormControl, HasPlaceholder,
      HasAccessibleNameRef,
      HasProps:
    import p.*

    type Self = TextArea

    /** Package-internal class hook: wrappers (FloatLabel/IconField) stamp Prime's
      * state classes (`p-filled`, iconfield padding) onto the field they wrap.
      */
    private[uic] def extraClass(cls: String): TextArea = new TextArea(p.copy(extraClassesV = extraClassesV :+ cls))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new TextArea(p.copy(access = v))

    /** The value. A `SignalRef` binds TWO-WAY: an edit writes it, and its changes update the textarea. Any other signal
      * binds ONE-WAY: the textarea shows it, and an edit writes nothing and only reports through [[onInput]] and
      * [[onChange]]; with neither, a card says the reader cannot change it. A constant is the starting text, which the
      * reader edits freely. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): TextArea = new TextArea(p.copy(valueSlot = valueSlot.bind(v)))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): TextArea = new TextArea(p.copy(placeholderText = v))

    def rows(n: Int): TextArea = new TextArea(p.copy(rowsV = Present(math.max(1, n))))

    /** Disables the field. A `Signal[Boolean]` toggles the native `disabled` attribute IN PLACE via kyo-
      * ui's boolean attribute channel, so there is no re-render and caret/focus survive.
      */
    def disabled(v: Boolean | Signal[Boolean]): TextArea = new TextArea(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles the
      * attribute IN PLACE via kyo-ui's boolean attribute channel, so there is no re-render and caret/focus
      * survive.
      */
    def readonly(v: Boolean | Signal[Boolean]): TextArea = new TextArea(p.copy(readonlyFlag = Present(ReactiveValue(v))))

    /** Marks the field required (native constraint + `aria-required`). */
    def required(v: Boolean): TextArea = new TextArea(p.copy(requiredFlag = Present(v)))

    /** Native `name` for HTML form participation. */
    def name(v: String): TextArea = new TextArea(p.copy(nameV = Present(v)))

    /** Native `maxlength`: caps the number of characters. */
    def maxLength(n: Int): TextArea = new TextArea(p.copy(maxLengthV = Present(math.max(0, n))))

    /** Prime `autoResize`: the textarea grows with its content
      * (`.p-textarea-resizable` + the CSS `field-sizing: content` remainder class
      * `.p-uic-autoresize`; no JS measuring).
      */
    def autoResize(v: Boolean): TextArea = new TextArea(p.copy(autoResizeFlag = v))

    /** Size: `.p-textarea-sm` / default / `.p-textarea-lg`. */
    def size(v: Size): TextArea = new TextArea(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TextArea = new TextArea(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-textarea-fluid`). */
    def fluid(v: Boolean): TextArea = new TextArea(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new TextArea(p.copy(validityV = v))

    def onInput(f: String => UI.Action)(using Frame): TextArea  = new TextArea(p.copy(valueSlot = valueSlot.hear(f)))
    def onChange(f: String => UI.Action)(using Frame): TextArea =
        new TextArea(p.copy(onChangeF = Present(Callback.of(f)), valueSlot = valueSlot.heardToo))

    /** Fires on focus loss with the text the field holds as focus leaves it (what
      * the reader typed, also unbound or from a constant); unlike onChange it fires
      * even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => UI.Action)(using Frame): TextArea = new TextArea(p.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(validityV)
        Diagnostics.after(validity.follow(body(validity)))("TextArea")(
            Diagnostic.unheard(valueSlot, TextArea.valueName, on = !readonlyFlag.constTrue && !disabledFlag.constTrue)
        )
    end render

    private def body(validity: FieldValidity)(using Frame): UI =
        var b = textarea.cssClass("p-textarea").cssClass("p-component")
        extraClassesV.foreach(c => b = b.cssClass(c))
        access.id.foreach(v => b = b.id(v))
        sizeV match
            case Size.Small  => b = b.cssClass("p-textarea-sm")
            case Size.Large  => b = b.cssClass("p-textarea-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then b = b.cssClass("p-variant-filled")
        b = validity.mark(b)
        if fluidFlag then b = b.cssClass("p-textarea-fluid")
        if autoResizeFlag then b = b.cssClass("p-textarea-resizable").cssClass("p-uic-autoresize")
        // Prime's `p-filled` follows the text the field holds, which kyo-ui toggles in the client (see Input).
        b = b.filledClass("p-filled")
        // A two-way ref is kyo-ui's own binding. A one-way signal is shown in place, with no region around the field, so a keystroke renders
        // nothing: kyo-ui puts back an edit its owner did not take. A constant is the starting text.
        valueSlot.bound.foreach(v => b = b.value(v))
        b = placeholderText match
            case Present(TextValue.Const(v)) => b.placeholder(v)
            case Present(TextValue.Dyn(sig)) => b.placeholder(sig)
            case Absent                      => b
        // `<textarea>` has no native `rows` attribute in the kyo AST, so the row count
        // is exposed as `data-rows` (wire/discoverability hook) and reflected visually
        // as a minimum height (~1.4rem per row).
        b = rowsV.map(n => b.data("rows", n.toString).style(_.minHeight((n * 22).px))).getOrElse(b)
        b = disabledFlag.foldFlag(b)(b.disabled(_))
        b = readonlyFlag.foldFlag(b)(b.readOnly(_))
        if requiredFlag.getOrElse(false) then b = b.aria("required", "true").jsProp("required", "true")
        b = nameV.map(v => b.jsProp("name", v)).getOrElse(b)
        b = maxLengthV.map(n => b.jsProp("maxLength", n.toString)).getOrElse(b)
        b = access.name match
            case Present(v) => b.aria("label", v)
            case _          => b
        b = access.nameRef.map(v => b.aria("labelledby", v)).getOrElse(b)
        b = valueSlot.hearer.map(f => b.onInput(f)).getOrElse(b)
        b = onChangeF.map(f => b.onChange(f)).getOrElse(b)
        // The blur carries the text the field holds as focus leaves it: what the reader typed, whatever binds the field.
        b = onBlurF.map(f => b.onBlur((_, text) => f(text))).getOrElse(b)
        b
    end body
end TextArea

object TextArea:
    final private[uic] case class Props(
        valueSlot: Slot[String] = Slot(),
        placeholderText: Maybe[TextValue] = Absent,
        rowsV: Maybe[Int] = Absent,
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Maybe[BoolValue] = Absent,
        requiredFlag: Maybe[Boolean] = Absent,
        nameV: Maybe[String] = Absent,
        maxLengthV: Maybe[Int] = Absent,
        autoResizeFlag: Boolean = false,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onChangeF: Maybe[String => UI.Action] = Absent,
        onBlurF: Maybe[String => UI.Action] = Absent,
        extraClassesV: List[String] = Nil
    )

    def apply(): TextArea = new TextArea(Props())

    private val valueName = Slot.Name("TextArea", "value", "a change", "onInput or onChange")
end TextArea
