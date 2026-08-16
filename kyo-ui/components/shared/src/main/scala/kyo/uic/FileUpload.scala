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
  */
final case class FileUpload private (
    inputIdV: String = "p-uic-fileupload",
    chooseLabelV: TextValue = TextValue.Const("Choose"),
    fileLabelV: Maybe[TextValue] = Absent,
    acceptV: List[FileAccept] = Nil,
    multipleFlag: Boolean = false,
    disabledFlag: Boolean = false,
    onSelectF: Maybe[Seq[UI.FilePayload] => Any < Async] = Absent
) extends Node:
    type Self = FileUpload

    /** DOM id of the native file input (the label's `for` target). Give every
      * FileUpload on a page its own id.
      */
    def inputId(v: String): FileUpload = copy(inputIdV = v)

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

    /** Fired with the picked files' [[kyo.UI.FilePayload]] metadata (name, size,
      * mimeType, content) once a file is chosen.
      */
    def onSelect(f: Seq[UI.FilePayload] => Any < Async): FileUpload = copy(onSelectF = Present(f))

    private[uic] def render(using Frame): UI =
        // The chosen-name label is reactive: onFileSelect writes the picked name(s)
        // into an internal signal owned by this mount; the static projection shows
        // the empty-state label until the transport attaches.
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

    private def body(shown: Maybe[SignalRef[Maybe[String]]])(using Frame): UI =
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

        val fileLabelEl: UI = shown match
            case Present(ref) => ref.render(labelSpan)
            case Absent       => labelSpan(Absent)

        var file = fileInput.id(inputIdV)
        if acceptV.nonEmpty then file = file.accept(acceptV*)
        if multipleFlag then file = file.multiple(true)
        if disabledFlag then file = file.disabled(true)
        onSelectF.foreach { handler =>
            file = file.onFileSelect { payloads =>
                val names = payloads.map(_.name).mkString(", ")
                val setLabel: Any < Async = shown match
                    case Present(ref) => ref.set(if payloads.isEmpty then Absent else Present(names))
                    case Absent       => ()
                setLabel.andThen(handler(payloads))
            }
        }

        div
            .cssClass("p-fileupload")
            .cssClass("p-fileupload-basic")
            .cssClass("p-component")(
                div.cssClass("p-fileupload-basic-content")(
                    toChild(chooseEl),
                    toChild(fileLabelEl),
                    toChild(file)
                )
            )
    end body
end FileUpload

object FileUpload:
    def apply(): FileUpload = new FileUpload()
