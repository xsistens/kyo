package kyo.uic

import kyo.*

/** InputMask — native kyo-ui, PrimeOne design. A thin, typed wrapper over
  * [[Input]] that applies the fork's client-local `inputMask`, so the component
  * IS the same bare `<input class="p-inputtext">` and the extracted `@primeuix`
  * inputtext CSS applies verbatim (Prime's InputMask has no sheet of its own —
  * it reuses InputText styling).
  *
  * The mask vocabulary is the fork's: `9` = digit, `a` = letter, `*` =
  * alphanumeric; every other character is a literal that the client inserts as
  * you type. Masking is CLIENT-LOCAL (on `beforeinput`), so the already-masked
  * value is what reaches `onChange`/the bound ref — the round-trip carries the
  * formatted string, not the raw keystrokes.
  *
  * Honest deferrals vs PrimeReact's InputMask: the fork formats INCREMENTALLY
  * (it inserts literals ahead of the caret as slots fill) but shows no
  * `slotChar` placeholder for the not-yet-typed tail, and there is no
  * `autoClear` (clear-on-blur-if-incomplete) — both are client-runtime
  * behaviors the fork's mask engine does not model, so they are documented
  * rather than faked. `onComplete` IS honest: it fires when the masked value
  * fills every mask slot (its length reaches the full template length).
  */
final case class InputMask private (
    maskV: String,
    valueBinding: Maybe[Input.Value] = Absent,
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
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    onCompleteF: Maybe[String => Any < Async] = Absent
) extends Node, TextFormControl, HasPlaceholder, HasAccessibleName:
    type Self = InputMask

    /** The mask template (`9` digit, `a` letter, `*` alphanumeric, else literal). */
    def mask(v: String): InputMask = copy(maskV = v)

    /** Sets a constant value (already formatted, or raw — the client re-masks on edit). */
    def value(v: String): InputMask = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds two-way to `ref`: edits write the MASKED value back, ref writes update the field. */
    def value(ref: SignalRef[String]): InputMask = copy(valueBinding = Present(Input.Value.Ref(ref)))

    private[uic] def withPlaceholder(v: Maybe[TextValue]): InputMask = copy(placeholderText = v)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): InputMask = copy(idV = v)

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

    /** Fill variant — `Filled` renders `.p-variant-filled`. */
    def variant(v: FieldVariant): InputMask = copy(variantV = v)

    /** Spans the full width of its container (`.p-inputtext-fluid`). */
    def fluid(v: Boolean): InputMask = copy(fluidFlag = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): InputMask                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): InputMask                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): InputMask = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): InputMask = copy(accNameV = v)

    /** Fired with the MASKED value on every change (native change commit). */
    def onChange(f: String => Any < Async): InputMask = copy(onChangeF = Present(f))

    /** Fires on focus loss with the field's current value — unlike onChange it
      * fires even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => Any < Async): InputMask = copy(onBlurF = Present(f))

    /** Fired with the MASKED value when it fills every mask slot (complete). */
    def onComplete(f: String => Any < Async): InputMask = copy(onCompleteF = Present(f))

    /** The number of literal-template characters when every slot is filled — a
      * masked value of this length is complete.
      */
    private def fullLength: Int = maskV.length

    private[uic] def render(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderStatic
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderStatic
                )

    private def renderStatic(using Frame): UI =
        var in = Input()
            .extraClass("p-inputmask") // Prime tags the masked field with this alongside p-inputtext
            .inputMask(maskV)
            .size(sizeV)
            .variant(variantV)
            .invalid(invalidV.constTrue)
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
        valueBinding match
            case Present(Input.Value.Const(v)) => in = in.value(v)
            case Present(Input.Value.Ref(r))   => in = in.value(r)
            case Absent                        => ()
        end match
        invalidMsgV.foreach(m => in = in.invalidMessage(m))
        if onChangeF.isDefined || onCompleteF.isDefined then
            in = in.onChange { masked =>
                val fireChange: Any < Async = onChangeF match
                    case Present(f) => f(masked)
                    case Absent     => ()
                val fireComplete: Any < Async = onCompleteF match
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
    end renderStatic
end InputMask

object InputMask:
    /** An input mask with the given template (`9`/`a`/`*` slots, else literal). */
    def apply(mask: String): InputMask = new InputMask(maskV = mask)

    /** An unbound input mask — set the template via [[InputMask.mask]]. */
    def apply(): InputMask = new InputMask(maskV = "")
end InputMask
