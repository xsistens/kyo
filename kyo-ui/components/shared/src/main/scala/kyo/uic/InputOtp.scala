package kyo.uic

import kyo.*
import kyo.UI.*

/** InputOtp: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputOtp anatomy: `div.p-inputotp.p-component` > N single-character
  * `input.p-inputotp-input.p-inputtext` cells), so the extracted `@primeuix`
  * inputotp CSS applies verbatim.
  *
  * The whole code binds to ONE `value(SignalRef[String])`: cell i shows the
  * i-th character, and typing into a cell writes that character into position
  * i of the bound string (gaps between filled cells are held as spaces,
  * trailing gaps trimmed). `length(n)` sets the cell count (Prime default 4),
  * `integerOnly(true)` drops non-digit input, `mask(true)` renders
  * password-type cells.
  *
  * Auto-advance (self-addressing): each cell mints a session-unique id
  * (in the `UI.mounted` region that also grabs the session `Commands`) and
  * stamps it on itself; after a digit lands in cell N, the input handler drives
  * `focusId(cellIds(N+1))`, moving focus to the next cell exactly as Prime's
  * client JS does; over server-push the focus command rides the same transport
  * as the render op. In the static projection (SSG / the placeholder before the
  * transport attaches) the cells carry no ids and focus stays put; Tab /
  * Shift-Tab and the pointer always work.
  */
final case class InputOtp private (
    valueBinding: Maybe[TextValue] = Absent,
    lengthV: Int = 4,
    integerOnlyFlag: Boolean = false,
    maskFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    readonlyFlag: Boolean = false,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[String => UI.Action] = Absent,
    onBlurF: Maybe[String => UI.Action] = Absent,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl, HasAccessibleName:
    type Self = InputOtp

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): InputOtp = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** The whole code. A `SignalRef` binds TWO-WAY: a cell edit writes its character into the right position, and ref
      * changes re-render every cell. Any other signal binds ONE-WAY: the cells show it, and an edit writes nothing and
      * only reports the code it asks for through [[onChange]]; without it, a card says the reader cannot change it. A
      * constant is the starting code. Unbound or from a constant, the field keeps the code itself, so a cell edit writes
      * there and [[onChange]] hears the whole code the cells hold. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): InputOtp = copy(valueBinding = Present(ReactiveValue(v)))

    /** Number of cells (Prime default 4). */
    def length(n: Int): InputOtp = copy(lengthV = math.max(1, n))

    /** Accepts only digits (Prime's `integerOnly`): other characters never reach
      * the BOUND value. Each cell carries the fork's client-local `inputFilter("int")`,
      * so a non-digit keystroke is rejected on `beforeinput` BEFORE it ever paints,
      * matching Prime's client-side block. (A server-side `filter` still guards
      * the stored code as defense in depth.)
      */
    def integerOnly(v: Boolean): InputOtp = copy(integerOnlyFlag = v)

    /** Renders password-type cells (Prime's `mask`). */
    def mask(v: Boolean): InputOtp = copy(maskFlag = v)

    /** Disables all cells; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): InputOtp = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `readonly` on every cell. */
    def readonly(v: Boolean): InputOtp = copy(readonlyFlag = v)

    /** Size: `.p-inputtext-sm` / default / `.p-inputtext-lg` on the cells (the
      * sheet widens the cells per size).
      */
    def size(v: Size): InputOtp = copy(sizeV = v)

    /** Fill variant: `Filled` renders `.p-variant-filled` cells. */
    def variant(v: FieldVariant): InputOtp = copy(variantV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): InputOtp                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): InputOtp                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): InputOtp = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): InputOtp = copy(accNameV = v)

    /** Fired with the FULL code after a cell write-back. */
    def onChange(f: String => UI.Action)(using Frame): InputOtp = copy(onChangeF = Present(Callback.of(f)))

    /** Fires on focus loss (native `blur`) with the field's current value; unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => UI.Action)(using Frame): InputOtp = copy(onBlurF = Present(Callback.of(f)))

    private def interactive: Boolean = !disabledFlag.constTrue && !readonlyFlag

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        val field    = valueBinding match
            case Present(ReactiveValue.Dyn(sig)) if interactive =>
                // Auto-advance needs the session Commands and stable per-cell ids: mint
                // them ONCE in a mounted effect, then the reactive body reads the same
                // ids across re-renders. Golden/SSG projection = the un-advancing
                // placeholder (no ids); the wired seam exercises the advancing form. Bound one way,
                // each cell follows its position of the signal, so kyo-ui puts back a character the owner does not take.
                UI.mounted {
                    for
                        cmds <- UI.commands
                        ids  <- Kyo.foreach(0 until lengthV)(_ => cmds.freshId)
                    yield wired(sig, ids.toList, id => cmds.focusId(id))
                }.placeholder(validity.follow(sig.render(v => body(v, Absent, _ => (), validity))))
            case Present(ReactiveValue.Dyn(sig)) => validity.follow(sig.render(v => body(v, Absent, _ => (), validity)))
            // Unbound or started from a constant, the code is the field's own: the cells build it together, so a code
            // the reader asks for is the one they hold, which a ref the field keeps for as long as it is shown remembers.
            case Present(ReactiveValue.Const(v)) if interactive => owned(v, validity)
            case Absent if interactive                          => owned("", validity)
            case Present(ReactiveValue.Const(v))                => validity.follow(body(v, Absent, _ => (), validity))
            case Absent                                         => validity.follow(body("", Absent, _ => (), validity))
        val unheard = interactive && valueBinding.exists(_.dyn.isDefined) && !ReactiveValue.heard(valueBinding, onChangeF)
        if unheard then fragment(field, KeyDiagnostics.unheard("InputOtp", "value", "onChange")) else field
    end renderResolved

    /** The field over a code it keeps itself, starting at `initial`, drawn as a two-way binding of it. */
    private def owned(initial: String, validity: FieldValidity)(using Frame): UI =
        ReactiveValue.owned(initial)(own => copy(valueBinding = Present(own)).renderResolved)(
            validity.follow(body(initial, Absent, _ => (), validity))
        )

    /** The advancing subscription the mount publishes: the golden seam. `focus`
      * moves DOM focus to the given cell id (production: `Commands.focusId`).
      */
    private[uic] def wired(code: Signal[String], cellIds: List[String], focus: String => UI.Action)(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        validity.follow(code.render(v => body(v, Present(cellIds), focus, validity)))
    end wired

    /** Writes cell i's new content into the bound code (or, one-way, reports the code it asks for): the LAST accepted
      * character wins (a second keystroke in a full cell replaces it), an empty edit clears the position; gaps stay as
      * spaces, trailing gaps trimmed. The code it changes is the one the binding holds when the edit arrives, not the one
      * the cells were drawn from: a keystroke can land before the render of the one before it.
      */
    private def writeCell(i: Int, raw: String, drawn: String)(using Frame): UI.Action =
        val accepted = (if integerOnlyFlag then raw.filter(_.isDigit) else raw).takeRight(1)
        valueBinding.fold[String < Sync](drawn)(_.current).map { current =>
            val padded = current.padTo(lengthV, ' ').take(lengthV)
            val next   = padded.updated(i, if accepted.isEmpty then ' ' else accepted.head).mkString.reverse.dropWhile(_ == ' ').reverse
            ReactiveValue.change(valueBinding, next, onChangeF)
        }
    end writeCell

    private def body(
        current: String,
        cellIds: Maybe[List[String]],
        focus: String => UI.Action,
        validity: FieldValidity
    )(using Frame): UI =
        val cells: List[UI] = (0 until lengthV).toList.map { i =>
            def at(code: String): String = code.lift(i).filterNot(_ == ' ').map(_.toString).getOrElse("")
            // Bound one way the cell follows its position of the code, in place; otherwise it shows what this render read.
            val ch: String | Signal[String] = valueBinding match
                case Present(ReactiveValue.Dyn(sig)) if valueBinding.exists(_.writable.isEmpty) => sig.map(at)
                case _                                                                          => at(current)
            var cell = Input()
                .extraClass("p-inputotp-input")
                .inputType(if maskFlag then InputType.Password else InputType.Text)
                .size(sizeV)
                .variant(variantV)
                .invalid(validity.red)
                .maxLength(1)
                .value(ch)
                .accessibleName(s"Please enter OTP character ${i + 1}")
            cellIds.foreach(ids => cell = cell.id(ids(i)))
            cell = if i == 0 then idV.map(v => cell.id(v)).getOrElse(cell) else cell
            if integerOnlyFlag then cell = cell.inputFilter("int")
            if disabledFlag.constTrue then cell = cell.disabled(true)
            if readonlyFlag then cell = cell.readonly(true)
            if interactive then
                cell = cell.onInput { s =>
                    // A digit landed → advance to the next cell via focusId; the
                    // last cell and an empty edit stay put.
                    val accepted           = (if integerOnlyFlag then s.filter(_.isDigit) else s).takeRight(1)
                    val advance: UI.Action = cellIds match
                        case Present(ids) if accepted.nonEmpty && i + 1 < lengthV => focus(ids(i + 1))
                        case _                                                    => ()
                    writeCell(i, s, current).andThen(advance)
                }
                onBlurF.foreach { f =>
                    // A cell's native blur carries only its OWN character, so the full
                    // code is read from the binding instead (the fallback covers the
                    // unbound projection).
                    cell = cell.onBlur(_ => valueBinding.fold(f(current))(_.current.map(f)))
                }
            end if
            cell.render
        }

        var el = div.cssClass("p-inputotp").cssClass("p-component")
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        el(UI.children(cells)*)
    end body
end InputOtp

object InputOtp:
    def apply(): InputOtp = new InputOtp()
