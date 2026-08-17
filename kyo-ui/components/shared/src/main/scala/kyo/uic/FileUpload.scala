package kyo.uic

import kyo.*
import kyo.UI.*

/** FileUpload (BASIC mode) — native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's basic FileUpload anatomy:
  * `div.p-fileupload.p-fileupload-basic.p-component` >
  * `div.p-fileupload-basic-content` with the choose Button (plus glyph +
  * "Choose"), the `span.p-fileupload-filelabel` ("No file chosen"), and the
  * sheet-hidden native `input[type=file]`), so the extracted `@primeuix`
  * fileupload CSS applies verbatim.
  *
  * Server-honest DEVIATIONS (documented):
  *   - Prime's choose Button forwards its click to the hidden input from JS —
  *     here the choose affordance is a real `<label for>` wearing Prime's
  *     Button classes, so the native file dialog opens with ZERO JS. Pass a
  *     stable `inputId` (it wires the `for`/`id` pair).
  *   - kyo's file-input `onFileSelect` (mounted5) delivers the full
  *     [[kyo.UI.FilePayload]] metadata — name, size, mimeType, content — so the
  *     basic file label now shows the chosen name(s) ON ITS OWN, from an
  *     internal signal the picker writes: no app state needed. `multiple(true)`
  *     accepts several files (the label joins their names). Prime's
  *     `auto`/upload-URL machinery does not apply — the content already IS
  *     server-side in the handler.
  *   - The advanced mode (drag & drop, file list, progress) is deferred.
  *
  * A [[FileFormControl]] over `Seq[kyo.UI.FilePayload]`, so
  * `uic.FileUpload().bind(field)` wires value, validity, message and the blur
  * trigger — and a validator can read the picked files' sizes and extensions
  * because the payload metadata IS the bound value.
  */
final case class FileUpload private (
    inputIdV: String = "p-uic-fileupload",
    chooseLabelV: TextValue = TextValue.Const("Choose"),
    fileLabelV: Maybe[TextValue] = Absent,
    acceptV: List[FileAccept] = Nil,
    multipleFlag: Boolean = false,
    disabledFlag: Boolean = false,
    filesRef: Maybe[SignalRef[Seq[UI.FilePayload]]] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    onSelectF: Maybe[Seq[UI.FilePayload] => Any < Async] = Absent,
    onBlurF: Maybe[Seq[UI.FilePayload] => Any < Async] = Absent
) extends Node, FileFormControl:
    type Self = FileUpload

    /** DOM id of the native file input (the label's `for` target). Give every
      * FileUpload on a page its own id.
      */
    def inputId(v: String): FileUpload = copy(inputIdV = v)

    /** [[inputId]] under the name the form layer uses: `bind` stamps the field's minted id
      * here so focus-first-invalid lands on the native input, and it is the same `for`
      * target the choose label points at.
      */
    def id(v: String): FileUpload = inputId(v)

    /** Binds the picked files two-way: a pick writes the [[kyo.UI.FilePayload]] metadata
      * (name, size, MIME type, content) into `ref`, and a ref write of an empty Seq clears
      * the label back to the empty state. This is what makes a FileUpload validatable — a
      * rule can read the sizes and extensions it was handed.
      */
    def value(ref: SignalRef[Seq[UI.FilePayload]]): FileUpload = copy(filesRef = Present(ref))

    /** Label of the choose button (default "Choose", Prime's locale default). */
    def chooseLabel(v: String): FileUpload = copy(chooseLabelV = TextValue.Const(v))

    /** Reactive choose-button label tracking `sig` — re-renders in place on emission. */
    def chooseLabel(sig: Signal[String]): FileUpload = copy(chooseLabelV = TextValue.Dyn(sig))

    /** Empty-state text of the `span.p-fileupload-filelabel` (default "No file
      * chosen"). Once a file is picked, the label shows the chosen name(s).
      */
    def fileLabel(v: String): FileUpload = copy(fileLabelV = Present(TextValue.Const(v)))

    /** Reactive empty-state file label tracking `sig` — re-renders in place on emission. */
    def fileLabel(sig: Signal[String]): FileUpload = copy(fileLabelV = Present(TextValue.Dyn(sig)))

    /** Native `accept` filter (kyo's typed [[kyo.UI.FileAccept]] vocabulary). */
    def accept(vs: FileAccept*): FileUpload = copy(acceptV = acceptV ++ vs.toList)

    /** Allow selecting several files (native `multiple`); the label joins names. */
    def multiple(v: Boolean): FileUpload = copy(multipleFlag = v)

    /** Disables the control: the button dims and the input locks. */
    def disabled(v: Boolean): FileUpload = copy(disabledFlag = v)

    /** Marks the control invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): FileUpload = copy(invalidV = Present(BoolValue.Const(v)))

    /** Reactive validity: the bound signal toggles the invalid state on emission. */
    def invalid(sig: Signal[Boolean]): FileUpload = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Message rendered below the control while it is invalid (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): FileUpload = copy(invalidMsgV = Present(v))

    /** Reactive invalid message — `Present` shows the row and (by default) marks the
      * control invalid; `Absent` clears both.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): FileUpload = copy(invalidMsgDynV = Present(sig))

    /** Fired with the picked files' [[kyo.UI.FilePayload]] metadata (name, size,
      * mimeType, content) once a file is chosen — after the [[value]] write-back.
      */
    def onSelect(f: Seq[UI.FilePayload] => Any < Async): FileUpload = copy(onSelectF = Present(f))

    /** Fires on focus loss with the currently picked files — the validation layer's Blur
      * trigger. Reports an empty Seq when nothing is bound or picked.
      */
    def onBlur(f: Seq[UI.FilePayload] => Any < Async): FileUpload = copy(onBlurF = Present(f))

    private[uic] def render(using Frame): UI =
        // The chosen-name label is reactive: a pick writes the picked name(s) into the
        // BOUND value ref when there is one, else into a signal owned by this mount; the
        // static projection shows the empty-state label until the transport attaches.
        // The validity boundary is resolved inside `body`, never around `UI.mounted`.
        filesRef match
            case Present(_) => body(Absent)
            case Absent =>
                UI.mounted {
                    for shown <- Signal.initRef[Maybe[String]](Absent)
                    yield body(Present(shown))
                }.placeholder(body(Absent))

    /** The chosen-file label: the joined name(s) once picked (adding Prime's
      * `.p-fileupload-filename`), else the empty-state text.
      */
    private def labelSpan(chosen: Maybe[String])(using Frame): UI =
        chosen match
            case Present(names) => span.cssClass("p-fileupload-filelabel").cssClass("p-fileupload-filename")(names)
            case Absent =>
                fileLabelV match
                    case Present(TextValue.Const(t)) => span.cssClass("p-fileupload-filelabel")(t)
                    case Present(TextValue.Dyn(s))   => s.render(t => span.cssClass("p-fileupload-filelabel")(t))
                    case Absent                      => span.cssClass("p-fileupload-filelabel")("No file chosen")

    /** Resolves the reactive validity slots, then builds. The boundary sits HERE, inside
      * the mount, rather than around `UI.mounted`: wrapping the mount would re-mount on
      * every emission and drop the label signal (see [[FieldInvalid.reactive]]).
      */
    private def body(shown: Maybe[SignalRef[Maybe[String]]])(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => bodyStatic(shown, invalidV.constTrue, invalidMsgV)
            case _ =>
                FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) => bodyStatic(shown, red, msg))

    private def bodyStatic(shown: Maybe[SignalRef[Maybe[String]]], red: Boolean, redMsg: Maybe[String])(using Frame): UI =
        // The choose affordance: a real <label for> wearing Prime's Button classes,
        // so the native file dialog opens without any JS lifecycle.
        var choose = label
            .forId(inputIdV)
            .cssClass("p-fileupload-choose-button")
            .cssClass("p-button")
            .cssClass("p-component")
        if disabledFlag then choose = choose.cssClass("p-disabled")
        val chooseLabelEl: UI = chooseLabelV match
            case TextValue.Const(t) => span.cssClass("p-button-label")(t)
            case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-button-label")(t))
        val chooseEl: UI = choose(
            toChild(GlyphSvg(Icons.plus, "p-button-icon")),
            toChild(chooseLabelEl)
        )

        // A bound value ref is the label's source of truth; without one the mount's own
        // signal is (and the static projection has neither).
        val fileLabelEl: UI = (filesRef, shown) match
            case (Present(ref), _)      => ref.render(fs => labelSpan(FileUpload.joinedNames(fs)))
            case (Absent, Present(ref)) => ref.render(labelSpan)
            case _                      => labelSpan(Absent)

        var file = fileInput.id(inputIdV)
        if acceptV.nonEmpty then file = file.accept(acceptV*)
        if multipleFlag then file = file.multiple(true)
        if disabledFlag then file = file.disabled(true)
        // The pick writes through even without an onSelect handler: a bound value ref is
        // the form's value and must track the picker on its own.
        if onSelectF.isDefined || filesRef.isDefined then
            file = file.onFileSelect { payloads =>
                val setLabel: Any < Async = (filesRef, shown) match
                    case (Present(ref), _) => ref.set(payloads)
                    case (Absent, Present(ref)) =>
                        ref.set(FileUpload.joinedNames(payloads))
                    case _ => ()
                val fire: Any < Async = onSelectF match
                    case Present(handler) => handler(payloads)
                    case Absent           => ()
                setLabel.andThen(fire)
            }
        end if
        onBlurF.foreach { f =>
            file = file.onBlur(filesRef match
                case Present(ref) => ref.use(f)
                case Absent       => f(Seq.empty))
        }

        var root = div
            .cssClass("p-fileupload")
            .cssClass("p-fileupload-basic")
            .cssClass("p-component")
        if red then root = root.cssClass("p-invalid").aria("invalid", "true")
        FieldInvalid.withMessage(
            root(
                div.cssClass("p-fileupload-basic-content")(
                    toChild(chooseEl),
                    toChild(fileLabelEl),
                    toChild(file)
                )
            ),
            red,
            redMsg
        )
    end bodyStatic
end FileUpload

object FileUpload:
    def apply(): FileUpload = new FileUpload()

    /** The label text for a set of picked files: their names joined, or `Absent` for the
      * empty state.
      */
    private def joinedNames(files: Seq[UI.FilePayload]): Maybe[String] =
        if files.isEmpty then Absent else Present(files.map(_.name).mkString(", "))
end FileUpload
