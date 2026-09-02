package kyo.uic

import kyo.*
import kyo.UI.*

/** Multi-line text input — native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's Textarea: the component IS the bare
  * `<textarea class="p-textarea">`). Closely mirrors [[Input]] but composes the
  * kyo `<textarea>` factory, so two-way binding (`value(SignalRef)` →
  * `Bound.Ref`), `onInput`/`onChange`, and the caret-preserving morph path are
  * all inherited. Reuses [[Input.Value]] as the const-or-ref carrier.
  */
final case class TextArea private (
    valueBinding: Maybe[Input.Value] = Absent,
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
    onInputF: Maybe[String => Any < Async] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
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

    /** Sets a constant value. */
    def value(v: String): TextArea = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref`: edits write back into the ref, ref changes update the textarea. */
    def value(ref: SignalRef[String]): TextArea = copy(valueBinding = Present(Input.Value.Ref(ref)))

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

    /** Native `maxlength` — caps the number of characters. */
    def maxLength(n: Int): TextArea = copy(maxLengthV = Present(math.max(0, n)))

    /** Prime `autoResize` — the textarea grows with its content
      * (`.p-textarea-resizable` + the CSS `field-sizing: content` remainder class
      * `.p-uic-autoresize`; no JS measuring).
      */
    def autoResize(v: Boolean): TextArea = copy(autoResizeFlag = v)

    /** Size: `.p-textarea-sm` / default / `.p-textarea-lg`. */
    def size(v: Size): TextArea = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TextArea = copy(variantV = v)

    /** Spans the full width of its container (`.p-textarea-fluid`). */
    def fluid(v: Boolean): TextArea = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): TextArea                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): TextArea                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): TextArea = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): TextArea = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): TextArea = copy(accNameRefV = v)

    def onInput(f: String => Any < Async): TextArea  = copy(onInputF = Present(f))
    def onChange(f: String => Any < Async): TextArea = copy(onChangeF = Present(f))

    /** Fires on focus loss with the field's current value — unlike onChange it
      * fires even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => Any < Async): TextArea = copy(onBlurF = Present(f))

    private[uic] def render(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
        var b = textarea.cssClass("p-textarea").cssClass("p-component")
        extraClassesV.foreach(c => b = b.cssClass(c))
        idV.foreach(v => b = b.id(v))
        sizeV match
            case Size.Small  => b = b.cssClass("p-textarea-sm")
            case Size.Large  => b = b.cssClass("p-textarea-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then b = b.cssClass("p-variant-filled")
        if invalidV.constTrue then b = b.cssClass("p-invalid").aria("invalid", "true")
        if fluidFlag then b = b.cssClass("p-textarea-fluid")
        if autoResizeFlag then b = b.cssClass("p-textarea-resizable").cssClass("p-uic-autoresize")
        valueBinding match
            case Present(Input.Value.Const(v)) => b = b.value(v)
            case Present(Input.Value.Ref(r))   => b = b.value(r)
            case Absent                        => ()
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
        b = onInputF.map(f => b.onInput(f)).getOrElse(b)
        b = onChangeF.map(f => b.onChange(f)).getOrElse(b)
        b = onBlurF match
            case Present(f) =>
                b.onBlur(valueBinding match
                    case Present(Input.Value.Ref(r))   => r.use(f)
                    case Present(Input.Value.Const(v)) => f(v)
                    case Absent                        => f(""))
            case Absent => b
        FieldInvalid.withMessage(b, invalidV.constTrue, invalidMsgV)
    end renderStatic
end TextArea

object TextArea:
    def apply(): TextArea = new TextArea()
