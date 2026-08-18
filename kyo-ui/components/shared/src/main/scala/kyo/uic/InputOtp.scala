package kyo.uic

import kyo.*
import kyo.UI.*

/** InputOtp — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * client JS does — over server-push the focus command rides the same transport
  * as the render op. In the static projection (SSG / the placeholder before the
  * transport attaches) the cells carry no ids and focus stays put; Tab /
  * Shift-Tab and the pointer always work.
  */
final case class InputOtp private (
    valueBinding: Maybe[Input.Value] = Absent,
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
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl:
    type Self = InputOtp

    /** Native `id` on the FIRST OTP cell input — pair with `Label.forId`; the form layer
      * stamps the bound field's id here so focus-first-invalid targets the first cell.
      */
    def id(v: String): InputOtp = copy(idV = Present(v))

    /** Sets a constant code. */
    def value(v: String): InputOtp = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds the whole code two-way to `ref`: cell edits write their character
      * into the right position, ref changes re-render every cell.
      */
    def value(ref: SignalRef[String]): InputOtp = copy(valueBinding = Present(Input.Value.Ref(ref)))

    /** Number of cells (Prime default 4). */
    def length(n: Int): InputOtp = copy(lengthV = math.max(1, n))

    /** Accepts only digits (Prime's `integerOnly`) — other characters never reach
      * the BOUND value. Each cell carries the fork's client-local `inputFilter("int")`,
      * so a non-digit keystroke is rejected on `beforeinput` BEFORE it ever paints
      * — matching Prime's client-side block. (A server-side `filter` still guards
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

    /** Fill variant — `Filled` renders `.p-variant-filled` cells. */
    def variant(v: FieldVariant): InputOtp = copy(variantV = v)

    /** Marks the cells invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): InputOtp = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the cells while `invalid(true)`. */
    def invalidMessage(v: String): InputOtp = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` on
      * the cells in place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): InputOtp = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * cells red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): InputOtp = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label` on the root; cells carry per-position labels. */
    def accessibleName(v: String): InputOtp = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): InputOtp = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Fired with the FULL code after a cell write-back. */
    def onChange(f: String => Any < Async): InputOtp = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the field's current value — unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): InputOtp = copy(onBlurF = Present(f))

    private def interactive: Boolean = !disabledFlag.constTrue && !readonlyFlag

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        valueBinding match
            case Present(Input.Value.Ref(ref)) if interactive =>
                // Auto-advance needs the session Commands and stable per-cell ids: mint
                // them ONCE in a mounted effect, then the reactive body reads the same
                // ids across re-renders. Golden/SSG projection = the un-advancing
                // placeholder (no ids); the wired seam exercises the advancing form.
                UI.mounted {
                    for
                        cmds <- UI.commands
                        ids  <- Kyo.foreach(0 until lengthV)(_ => cmds.freshId)
                    yield wired(ref, ids.toList, id => cmds.focusId(id))
                }.placeholder(ref.render(v => body(v, Present(ref), Absent, _ => ())))
            case Present(Input.Value.Ref(ref)) => ref.render(v => body(v, Present(ref), Absent, _ => ()))
            case Present(Input.Value.Const(v)) => body(v, Absent, Absent, _ => ())
            case Absent                        => body("", Absent, Absent, _ => ())

    /** The advancing subscription the mount publishes — the golden seam. `focus`
      * moves DOM focus to the given cell id (production: `Commands.focusId`).
      */
    private[uic] def wired(ref: SignalRef[String], cellIds: List[String], focus: String => Any < Async)(using Frame): UI =
        ref.render(v => body(v, Present(ref), Present(cellIds), focus))

    /** Writes cell i's new content into the bound code: the LAST accepted
      * character wins (a second keystroke in a full cell replaces it), an empty
      * edit clears the position; gaps stay as spaces, trailing gaps trimmed.
      */
    private def writeCell(i: Int, raw: String, current: String, ref: Maybe[SignalRef[String]])(using Frame): Any < Async =
        val accepted = (if integerOnlyFlag then raw.filter(_.isDigit) else raw).takeRight(1)
        val padded   = current.padTo(lengthV, ' ').take(lengthV)
        val next     = padded.updated(i, if accepted.isEmpty then ' ' else accepted.head).mkString.reverse.dropWhile(_ == ' ').reverse
        val write: Any < Async = ref match
            case Present(r) => r.set(next)
            case Absent     => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end writeCell

    private def body(
        current: String,
        ref: Maybe[SignalRef[String]],
        cellIds: Maybe[List[String]],
        focus: String => Any < Async
    )(using Frame): UI =
        // Reactive-invalid gate (INSIDE the mount subscription — never around the
        // UI.mounted node): with a reactive slot set, re-render the cells + message
        // through the shared helper; otherwise the untouched static form.
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => bodyStatic(current, ref, cellIds, focus)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                        .bodyStatic(current, ref, cellIds, focus)
                )

    private def bodyStatic(
        current: String,
        ref: Maybe[SignalRef[String]],
        cellIds: Maybe[List[String]],
        focus: String => Any < Async
    )(using Frame): UI =
        val cells: List[UI] = (0 until lengthV).toList.map { i =>
            val ch = current.lift(i).filterNot(_ == ' ').map(_.toString).getOrElse("")
            var cell = Input()
                .extraClass("p-inputotp-input")
                .inputType(if maskFlag then InputType.Password else InputType.Text)
                .size(sizeV)
                .variant(variantV)
                .invalid(invalidV.constTrue)
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
                    val accepted = (if integerOnlyFlag then s.filter(_.isDigit) else s).takeRight(1)
                    val advance: Any < Async = cellIds match
                        case Present(ids) if accepted.nonEmpty && i + 1 < lengthV => focus(ids(i + 1))
                        case _                                                    => ()
                    writeCell(i, s, current, ref).andThen(advance)
                }
                onBlurF.foreach { f =>
                    // A cell's native blur carries only its OWN character, so the full
                    // code is read from the bound ref instead (the fallback covers the
                    // unbound/const projection).
                    cell = cell.onBlur { _ =>
                        ref match
                            case Present(r) => r.use(f)
                            case Absent     => f(current)
                    }
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
        val root = el(cells.map(toChild)*)
        FieldInvalid.withMessage(root, invalidV.constTrue, invalidMsgV)
    end bodyStatic
end InputOtp

object InputOtp:
    def apply(): InputOtp = new InputOtp()
