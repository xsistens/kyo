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
final class InputMask private[uic] (private[uic] val p: InputMask.Props) extends Node, FormControl[String], HasPlaceholder,
      HasAccessibleName,
      HasProps:
    import p.*

    type Self = InputMask

    /** The mask template (`9` digit, `a` letter, `*` alphanumeric, else literal). */
    def mask(v: String): InputMask = new InputMask(p.copy(maskV = v))

    /** The value. A `SignalRef` binds TWO-WAY: an edit writes the MASKED value, and ref writes update the field. Any other
      * signal binds ONE-WAY: the field shows it, and an edit only reports through [[onChange]] and [[onComplete]]; with
      * neither, a card says the reader cannot change it. A constant (already formatted, or raw; the client re-masks on
      * edit) is the starting text. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): InputMask = new InputMask(p.copy(valueSlot = valueSlot.bind(v)))

    private[uic] def withPlaceholder(v: Maybe[UI.Prop[String]]): InputMask = new InputMask(p.copy(placeholderText = v))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new InputMask(p.copy(access = v))

    /** Disables the field. A `Signal[Boolean]` toggles it IN PLACE via kyo-ui's boolean attribute channel
      * (no re-render).
      */
    def disabled(v: Boolean | Signal[Boolean]): InputMask = new InputMask(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** Native `readonly`, so the field is focusable but not editable. A `Signal[Boolean]` toggles it IN
      * PLACE via kyo-ui's boolean attribute channel (no re-render).
      */
    def readonly(v: Boolean | Signal[Boolean]): InputMask = new InputMask(p.copy(readonlyFlag = Present(ReactiveValue(v))))

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): InputMask = new InputMask(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`. */
    def variant(v: FieldVariant): InputMask = new InputMask(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-inputtext-fluid`). */
    def fluid(v: Boolean): InputMask = new InputMask(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                       = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self               = new InputMask(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): InputMask  = value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): InputMask = onBlur(f)

    /** Fired with the MASKED value on every change (native change commit). */
    def onChange(f: String => UI.Action)(using Frame): InputMask = new InputMask(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fires on focus loss with the MASKED text the field holds as focus leaves it
      * (what the reader typed, also unbound or from a constant). Unlike onChange it
      * fires even when the value was not edited. The validation layer's Blur trigger.
      */
    def onBlur(f: String => UI.Action)(using Frame): InputMask = new InputMask(p.copy(onBlurF = Present(Callback.of(f))))

    /** Fired with the MASKED value when it fills every mask slot (complete). */
    def onComplete(f: String => UI.Action)(using Frame): InputMask = new InputMask(p.copy(onCompleteF = Present(Callback.of(f))))

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
            .withValidity(validityV)
            .fluid(fluidFlag)
        in = disabledFlag.foldFlag(in)(in.disabled(_))
        in = readonlyFlag.foldFlag(in)(in.readonly(_))
        access.id.foreach(v => in = in.id(v))
        access.name.foreach(v => in = in.accessibleName(v))
        in = in.withPlaceholder(placeholderText)
        valueSlot.bound.foreach(v => in = in.value(v))
        if valueSlot.hearer.isDefined || onCompleteF.isDefined then
            in = in.onChange { masked =>
                val fireChange: UI.Action = valueSlot.hearer match
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
    final private[uic] case class Props(
        maskV: String,
        valueSlot: Controlled[String] = Controlled(),
        placeholderText: Maybe[UI.Prop[String]] = Absent,
        disabledFlag: Maybe[BoolValue] = Absent,
        readonlyFlag: Maybe[BoolValue] = Absent,
        sizeV: Size = Size.Normal,
        variantV: FieldVariant = FieldVariant.Outlined,
        fluidFlag: Boolean = false,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onBlurF: Maybe[String => UI.Action] = Absent,
        onCompleteF: Maybe[String => UI.Action] = Absent
    )

    /** An input mask with the given template (`9`/`a`/`*` slots, else literal). */
    def apply(mask: String): InputMask = new InputMask(Props(maskV = mask))

    /** An unbound input mask; set the template via [[InputMask.mask]]. */
    def apply(): InputMask = new InputMask(Props(maskV = ""))
end InputMask
