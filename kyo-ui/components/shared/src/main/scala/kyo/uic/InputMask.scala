package kyo.uic

import kyo.*

/** InputMask: native kyo-ui, PrimeOne design. A thin, typed wrapper over
  * [[Input]] that applies the fork's client-local `inputMask`, so the component
  * IS the same bare `<input class="p-inputtext">` and the extracted `@primeuix`
  * inputtext CSS applies verbatim (Prime's InputMask has no sheet of its own;
  * it reuses InputText styling).
  *
  * The mask vocabulary is the fork's: `9` = digit, `a` = letter, `*` =
  * alphanumeric; every other character is a literal that the client inserts as
  * you type. Masking is CLIENT-LOCAL (on `beforeinput`), so the already-masked
  * value is what reaches `onChange`/the bound ref, so the round-trip carries the
  * formatted string, not the raw keystrokes.
  *
  * Honest deferrals vs PrimeReact's InputMask: the fork formats INCREMENTALLY
  * (it inserts literals ahead of the caret as slots fill) but shows no
  * `slotChar` placeholder for the not-yet-typed tail, and there is no
  * `autoClear` (clear-on-blur-if-incomplete). Both are client-runtime
  * behaviors the fork's mask engine does not model, so they are documented
  * rather than faked. `onComplete` IS honest: it fires when the masked value
  * fills every mask slot (its length reaches the full template length).
  */
final case class InputMask private (
    maskV: String,
    valueBinding: Maybe[TextValue] = Absent,
    placeholderText: Maybe[TextValue] = Absent,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Maybe[BoolValue] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    idV: Maybe[String] = Absent,
    onChangeF: Maybe[String => UI.Action] = Absent,
    onBlurF: Maybe[String => UI.Action] = Absent,
    onCompleteF: Maybe[String => UI.Action] = Absent
) extends Node, TextFormControl, HasPlaceholder, HasAccessibleName:
    type Self = InputMask

    /** The mask template (`9` digit, `a` letter, `*` alphanumeric, else literal). */
    def mask(v: String): InputMask = copy(maskV = v)

    /** The value. A `SignalRef` binds TWO-WAY: an edit writes the MASKED value, and ref writes update the field. Any other
      * signal binds ONE-WAY: the field shows it, and an edit only reports through [[onChange]] and [[onComplete]]; with
      * neither, a card says the reader cannot change it. A constant (already formatted, or raw; the client re-masks on
      * edit) is the starting text. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): InputMask = copy(valueBinding = Present(ReactiveValue(v)))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): InputMask = copy(placeholderText = v)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): InputMask = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Disables the field. A `Signal[Boolean]` toggles it IN PLACE via kyo-ui's boolean attribute channel
      * (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): InputMask = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles it IN
      * PLACE via kyo-ui's boolean attribute channel (no re-render).
      */
    def readonly(v: Boolean | Signal[Boolean]): InputMask = copy(readonlyFlag = Present(ReactiveValue(v)))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): InputMask = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled`. */
    def variant(v: FieldVariant): InputMask = copy(variantV = v)

    /** Spans the full width of its container (`.p-inputtext-fluid`). */
    def fluid(v: Boolean): InputMask = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): InputMask                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): InputMask                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): InputMask = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): InputMask = copy(accNameV = v)

    /** Fired with the MASKED value on every change (native change commit). */
    def onChange(f: String => UI.Action)(using Frame): InputMask = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss with the field's current value. Unlike onChange it
      * fires even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => UI.Action)(using Frame): InputMask = copy(onBlurF = Present(Callback.of(f)))

    /** Fired with the MASKED value when it fills every mask slot (complete). */
    def onComplete(f: String => UI.Action)(using Frame): InputMask = copy(onCompleteF = Present(Callback.of(f)))

    /** The number of literal-template characters when every slot is filled: a
      * masked value of this length is complete.
      */
    private def fullLength: Int = maskV.length

    private[uic] def render(using Frame): UI =
        // The validity slots go to the Input as they are, so it stamps them on the field and follows it
        // with the message row exactly as a plain Input does.
        var in = Input()
            .extraClass("p-inputmask") // Prime tags the masked field with this alongside p-inputtext
            .unheardAs("InputMask", "onChange or onComplete")
            .inputMask(maskV)
            .size(sizeV)
            .variant(variantV)
            .withInvalid(invalidV)
            .withInvalidMessage(invalidMsgV)
            .withInvalidMessageDyn(invalidMsgDynV)
            .fluid(fluidFlag)
        in = disabledFlag.foldFlag(in)(in.disabled(_))
        in = readonlyFlag.foldFlag(in)(in.readonly(_))
        idV.foreach(v => in = in.id(v))
        accNameV match
            case Present(TextValue.Const(v)) => in = in.accessibleName(v)
            case Present(TextValue.Dyn(s))   => in = in.accessibleName(s)
            case Absent                      => ()
        end match
        placeholderText match
            case Present(TextValue.Const(v)) => in = in.placeholder(v)
            case Present(TextValue.Dyn(sig)) => in = in.placeholder(sig)
            case Absent                      => ()
        end match
        valueBinding.foreach(v => in = in.value(v.bound))
        if onChangeF.isDefined || onCompleteF.isDefined then
            in = in.onChange { masked =>
                val fireChange: UI.Action = onChangeF match
                    case Present(f) => f(masked)
                    case Absent     => ()
                val fireComplete: UI.Action = onCompleteF match
                    case Present(f) if masked.length >= fullLength => f(masked)
                    case _                                         => ()
                for
                    _ <- fireChange
                    r <- fireComplete
                yield r
                end for
            }
        end if
        // The masked field IS an inner Input, whose own onBlur reads the shared value
        // binding (passed through above), so the blur trigger delegates straight to it.
        onBlurF.foreach(f => in = in.onBlur(f))
        in.render
    end render
end InputMask

object InputMask:
    /** An input mask with the given template (`9`/`a`/`*` slots, else literal). */
    def apply(mask: String): InputMask = new InputMask(maskV = mask)

    /** An unbound input mask; set the template via [[InputMask.mask]]. */
    def apply(): InputMask = new InputMask(maskV = "")
end InputMask
