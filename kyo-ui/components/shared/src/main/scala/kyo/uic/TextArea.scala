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
final case class TextArea private (
    valueBinding: Maybe[TextValue] = Absent,
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
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onInputF: Maybe[String => UI.Action] = Absent,
    onChangeF: Maybe[String => UI.Action] = Absent,
    onBlurF: Maybe[String => UI.Action] = Absent,
    extraClassesV: List[String] = Nil,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl, HasPlaceholder, HasAccessibleNameRef:
    type Self = TextArea

    /** Package-internal class hook: wrappers (FloatLabel/IconField) stamp Prime's
      * state classes (`p-filled`, iconfield padding) onto the field they wrap.
      */
    private[uic] def extraClass(cls: String): TextArea = copy(extraClassesV = extraClassesV :+ cls)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): TextArea = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** The value. A `SignalRef` binds TWO-WAY: an edit writes it, and its changes update the textarea. Any other signal
      * binds ONE-WAY: the textarea shows it, and an edit writes nothing and only reports through [[onInput]] and
      * [[onChange]]; with neither, a card says the reader cannot change it. A constant is the starting text, which the
      * reader edits freely. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): TextArea = copy(valueBinding = Present(ReactiveValue(v)))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): TextArea = copy(placeholderText = v)

    def rows(n: Int): TextArea = copy(rowsV = Present(math.max(1, n)))

    /** Disables the field. A `Signal[Boolean]` toggles the native `disabled` attribute IN PLACE via kyo-
      * ui's boolean attribute channel, so there is no re-render and caret/focus survive.
      */
    def disabled(v: Boolean | Signal[Boolean]): TextArea = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles the
      * attribute IN PLACE via kyo-ui's boolean attribute channel, so there is no re-render and caret/focus
      * survive.
      */
    def readonly(v: Boolean | Signal[Boolean]): TextArea = copy(readonlyFlag = Present(ReactiveValue(v)))

    /** Marks the field required (native constraint + `aria-required`). */
    def required(v: Boolean): TextArea = copy(requiredFlag = Present(v))

    /** Native `name` for HTML form participation. */
    def name(v: String): TextArea = copy(nameV = Present(v))

    /** Native `maxlength`: caps the number of characters. */
    def maxLength(n: Int): TextArea = copy(maxLengthV = Present(math.max(0, n)))

    /** Prime `autoResize`: the textarea grows with its content
      * (`.p-textarea-resizable` + the CSS `field-sizing: content` remainder class
      * `.p-uic-autoresize`; no JS measuring).
      */
    def autoResize(v: Boolean): TextArea = copy(autoResizeFlag = v)

    /** Size: `.p-textarea-sm` / default / `.p-textarea-lg`. */
    def size(v: Size): TextArea = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TextArea = copy(variantV = v)

    /** Spans the full width of its container (`.p-textarea-fluid`). */
    def fluid(v: Boolean): TextArea = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): TextArea                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): TextArea                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): TextArea = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): TextArea = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): TextArea = copy(accNameRefV = v)

    def onInput(f: String => UI.Action)(using Frame): TextArea  = copy(onInputF = Present(Callback.of(f)))
    def onChange(f: String => UI.Action)(using Frame): TextArea = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss with the text the field holds as focus leaves it (what
      * the reader typed, also unbound or from a constant); unlike onChange it fires
      * even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => UI.Action)(using Frame): TextArea = copy(onBlurF = Present(Callback.of(f)))

    private[uic] def render(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        // A two-way ref is kyo-ui's own binding. A one-way signal draws the field from what its owner holds and paints it
        // again after each report (ReactiveValue.repainting), so an edit the owner did not take is put back.
        val built = valueBinding match
            case Present(ReactiveVariable(_))    => body(Absent, (), validity)
            case Present(ReactiveValue.Dyn(sig)) => ReactiveValue.repainting(sig)((v, repaint) => body(Present(v), repaint, validity))
            case Present(ReactiveValue.Const(v)) => body(Present(v), (), validity)
            case Absent                          => body(Absent, (), validity)
        val unheard = valueBinding.exists(_.dyn.isDefined) && !ReactiveValue.heard(valueBinding, onInputF.orElse(onChangeF)) &&
            !readonlyFlag.constTrue && !disabledFlag.constTrue
        if unheard then fragment(validity.follow(built), KeyDiagnostics.unheard("TextArea", "value", "onInput or onChange"))
        else validity.follow(built)
    end render

    private def body(shown: Maybe[String], repaint: UI.Action, validity: FieldValidity)(using Frame): UI =
        var b = textarea.cssClass("p-textarea").cssClass("p-component")
        extraClassesV.foreach(c => b = b.cssClass(c))
        idV.foreach(v => b = b.id(v))
        sizeV match
            case Size.Small  => b = b.cssClass("p-textarea-sm")
            case Size.Large  => b = b.cssClass("p-textarea-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then b = b.cssClass("p-variant-filled")
        b = validity.mark(b)
        if fluidFlag then b = b.cssClass("p-textarea-fluid")
        if autoResizeFlag then b = b.cssClass("p-textarea-resizable").cssClass("p-uic-autoresize")
        valueBinding match
            case Present(ReactiveVariable(r)) => b = b.value(r)
            case _                            => shown.foreach(v => b = b.value(v))
        end match
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
        b = accNameV match
            case Present(TextValue.Const(v)) => b.aria("label", v)
            case Present(TextValue.Dyn(s))   => b.aria("label", s)
            case Absent                      => b
        b = accNameRefV.map(v => b.aria("labelledby", v)).getOrElse(b)
        b = onInputF.map(f => b.onInput(v => f(v).andThen(repaint))).getOrElse(b)
        b = onChangeF.map(f => b.onChange(v => f(v).andThen(repaint))).getOrElse(b)
        // The blur carries the text the field holds as focus leaves it: what the reader typed, whatever binds the field.
        b = onBlurF.map(f => b.onBlur((_, text) => f(text))).getOrElse(b)
        b
    end body
end TextArea

object TextArea:
    def apply(): TextArea = new TextArea()
