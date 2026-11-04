package kyo.uic

import kyo.*
import kyo.UI.*

/** FileUpload (BASIC mode): native kyo-ui, PrimeOne design (mirrors
  * PrimeVue/PrimeReact's basic FileUpload anatomy:
  * `div.p-fileupload.p-fileupload-basic.p-component` >
  * `div.p-fileupload-basic-content` with the choose Button (plus glyph +
  * "Choose"), the `span.p-fileupload-filelabel` ("No file chosen"), and the
  * sheet-hidden native `input[type=file]`), so the extracted `@primeuix`
  * fileupload CSS applies verbatim.
  *
  * Server-honest DEVIATIONS (documented):
  *   - Prime's choose Button forwards its click to the hidden input from JS;
  *     here the choose affordance is a real `<label for>` wearing Prime's
  *     Button classes, so the native file dialog opens with ZERO JS. Pass a
  *     stable `inputId` (it wires the `for`/`id` pair).
  *   - kyo's file-input `onFileSelect` delivers the full
  *     [[kyo.UI.FilePayload]] metadata (name, size, mimeType, content), so the
  *     basic file label now shows the chosen name(s) ON ITS OWN, from an
  *     internal signal the picker writes: no app state needed. `multiple(true)`
  *     accepts several files (the label joins their names). Prime's
  *     `auto`/upload-URL machinery does not apply: the content already IS
  *     server-side in the handler.
  *   - The advanced mode (drag & drop, file list, progress) is deferred.
  *
  * A [[FileFormControl]] over `Seq[kyo.UI.FilePayload]`, so
  * `uic.FileUpload().bind(field)` wires value, validity, message and the blur
  * trigger, and a validator can read the picked files' sizes and extensions
  * because the payload metadata IS the bound value.
  *
  * The native file input carries the id ([[inputId]]). It is visually hidden (`.p-hidden-accessible`) rather than
  * `display:none`, so it keeps its tab stop: Space or Enter on it opens the file dialog, and the choose button
  * shows its focus ring.
  */
final class FileUpload private[uic] (private[uic] val s: FileUpload.State) extends Node, FileFormControl, Stated:
    import s.*

    type Self = FileUpload

    /** DOM id of the native file input (the label's `for` target). Give every
      * FileUpload on a page its own id.
      */
    def inputId(v: String): FileUpload = new FileUpload(s.copy(inputIdV = v))

    /** The one implementor of [[HasElementId]] whose storage is not `Maybe[String]`:
      * `inputIdV` is a plain `String` with a working default, because the native input needs
      * a `for` target whether or not a caller named one. So an `Absent` here means "keep the
      * default", not "clear it", which is why this writer is spelled out rather than being
      * the `copy(idV = v)` every other implementor supplies.
      *
      * `id` and [[inputId]] are the same slot: `bind` stamps the field's minted id here so
      * focus-first-invalid lands on the native input, and it is the same `for` target the
      * choose label points at.
      */
    private[uic] def withElementId(v: Maybe[String]): FileUpload =
        v.map(inputId).getOrElse(this)

    /** Reads it back; `Absent` while the id is still the untouched default.
      *
      * The family's odd one out, because `inputIdV` is a plain `String` that always has a
      * value ([[inputId]]'s working default, so the native input is addressable unasked).
      * A reader answering `Present` for that default would tell `form.bind` that every
      * FileUpload it ever meets already carries a caller's id. What the reader is FOR is
      * "did someone choose this", and nobody chose the default.
      */
    private[uic] def elementId: Maybe[String] =
        if inputIdV == FileUpload.defaultInputId then Absent else Present(inputIdV)

    /** Binds the picked files, as [[kyo.UI.FilePayload]] metadata (name, size, MIME type, content). A `SignalRef` binds
      * two way: a pick writes it, then [[onChange]] hears the files, and a write of an empty Seq clears the label back to
      * the empty state. This is what makes a FileUpload validatable: a rule can read the sizes and extensions it was
      * handed. A plain `Signal` binds one way: the label follows it and a pick is only reported. A constant nobody hears
      * is fixed: the control stays reachable and opens no dialog. Unbound, the control keeps the pick itself. A signal
      * nothing hears is reported in a card.
      */
    def value(v: Seq[UI.FilePayload] | Signal[Seq[UI.FilePayload]]): FileUpload = new FileUpload(s.copy(filesSlot = filesSlot.bind(v)))

    /** A constant nobody hears is fixed: the control stays reachable and opens no dialog. */
    private def fixed: Boolean = filesSlot.fixed

    /** Label of the choose button (default "Choose", Prime's locale default). A `Signal[String]` re-renders
      * it in place on emission.
      */
    def chooseLabel(v: String | Signal[String]): FileUpload = new FileUpload(s.copy(chooseLabelV = ReactiveValue(v)))

    /** Empty-state text of the `span.p-fileupload-filelabel` (default "No file chosen"). Once a file is
      * picked, the label shows the chosen name(s). A `Signal[String]` re-renders the empty-state text in
      * place on emission.
      */
    def fileLabel(v: String | Signal[String]): FileUpload = new FileUpload(s.copy(fileLabelV = Present(ReactiveValue(v))))

    /** Native `accept` filter (kyo's typed [[kyo.UI.FileAccept]] vocabulary). */
    def accept(vs: FileAccept*): FileUpload = new FileUpload(s.copy(acceptV = acceptV ++ vs.toList))

    /** Allow selecting several files (native `multiple`); the label joins names. */
    def multiple(v: Boolean): FileUpload = new FileUpload(s.copy(multipleFlag = v))

    /** Disables the control: the button dims and the input locks. */
    def disabled(v: Boolean): FileUpload = new FileUpload(s.copy(disabledFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new FileUpload(s.copy(validityV = v))

    /** Fired with the picked files' [[kyo.UI.FilePayload]] metadata (name, size, mimeType, content) once a file is
      * chosen, after a two-way [[value]] is written.
      */
    def onChange(f: Seq[UI.FilePayload] => UI.Action)(using Frame): FileUpload = new FileUpload(s.copy(filesSlot = filesSlot.hear(f)))

    /** Fires on focus loss with the files the control holds: the validation layer's Blur trigger. Unbound that is what the
      * reader picked, an empty Seq before any pick.
      */
    def onBlur(f: Seq[UI.FilePayload] => UI.Action)(using Frame): FileUpload = new FileUpload(s.copy(onBlurF = Present(Callback.of(f))))

    private[uic] def render(using Frame): UI =
        // The chosen-name label is reactive over the files. Unbound, the mount holds what the reader picks; the page as first
        // rendered shows the empty-state label until the transport attaches.
        Diagnostics.before(Slot.owning(
            filesSlot,
            start = Seq.empty
        )(s => new FileUpload(this.s.copy(filesSlot = s)).body)(body))("FileUpload")(
            Diagnostic.unheard(filesSlot, FileUpload.filesName)
        )
    end render

    /** The chosen-file label: the joined name(s) once picked (adding Prime's
      * `.p-fileupload-filename`), else the empty-state text.
      */
    private def labelSpan(chosen: Maybe[String])(using Frame): UI =
        chosen match
            case Present(names) => span.cssClass("p-fileupload-filelabel").cssClass("p-fileupload-filename")(names)
            case Absent         =>
                fileLabelV match
                    case Present(TextValue.Const(t)) => span.cssClass("p-fileupload-filelabel")(t)
                    case Present(TextValue.Dyn(s))   => span.cssClass("p-fileupload-filelabel")(s)
                    case Absent                      => span.cssClass("p-fileupload-filelabel")("No file chosen")

    private def body(using Frame): UI =
        val validity = FieldValidity(validityV)
        // The choose affordance: a real <label for> wearing Prime's Button classes,
        // so the native file dialog opens without any JS lifecycle.
        var choose = label
            .forId(inputIdV)
            .cssClass("p-fileupload-choose-button")
            .cssClass("p-button")
            .cssClass("p-component")
        if disabledFlag then choose = choose.cssClass("p-disabled")
        if fixed then choose = choose.aria("disabled", "true")
        val chooseLabelEl: UI = chooseLabelV match
            case TextValue.Const(t) => span.cssClass("p-button-label")(t)
            case TextValue.Dyn(s)   => span.cssClass("p-button-label")(s)
        val chooseEl: UI = choose(
            GlyphSvg(Icons.plus, "p-button-icon"),
            chooseLabelEl
        )

        val fileLabelEl: UI = filesSlot.reading(Seq.empty).render(fs => labelSpan(FileUpload.joinedNames(fs)))

        // Visually hidden rather than display:none (see the class doc), so the input keeps its tab stop.
        var file = fileInput.id(inputIdV).cssClass("p-hidden-accessible")
        if acceptV.nonEmpty then file = file.accept(acceptV*)
        if multipleFlag then file = file.multiple(true)
        if disabledFlag then file = file.disabled(true)
        // The pick goes through even without an onChange handler: a bound value ref is the form's value and must track the
        // picker on its own, and an unbound control keeps the pick in its own state. A fixed control takes no pick.
        if !fixed && (filesSlot.heard || filesSlot.binding.isDefined) then file = file.onFileSelect(filesSlot.change(_))
        onBlurF.foreach(f => file = file.onBlur(filesSlot.current(Seq.empty).map(f)))

        var root = div
            .cssClass("p-fileupload")
            .cssClass("p-fileupload-basic")
            .cssClass("p-component")
        // The browser opens the dialog on its own; an inert root declines that default for the click and the keys.
        if fixed then root = root.preventActivation
        validity.follow(
            validity.mark(root)(
                div.cssClass("p-fileupload-basic-content")(
                    chooseEl,
                    fileLabelEl,
                    file
                )
            )
        )
    end body
end FileUpload

object FileUpload:
    final private[uic] case class State(
        inputIdV: String = FileUpload.defaultInputId,
        chooseLabelV: TextValue = TextValue.Const("Choose"),
        fileLabelV: Maybe[TextValue] = Absent,
        acceptV: List[FileAccept] = Nil,
        multipleFlag: Boolean = false,
        disabledFlag: Boolean = false,
        filesSlot: Slot[Seq[UI.FilePayload]] = Slot(),
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        onBlurF: Maybe[Seq[UI.FilePayload] => UI.Action] = Absent
    )

    def apply(): FileUpload = new FileUpload(State())

    /** The native input's id when nobody has chosen one. Named rather than inlined because
      * [[FileUpload.elementId]] has to tell the default apart from a caller's choice.
      */
    private[uic] val defaultInputId = "p-uic-fileupload"

    private val filesName = Slot.Name("FileUpload", "the files", "a pick", "onChange", consequence = "the reader's choice reaches nobody")

    /** The label text for a set of picked files: their names joined, or `Absent` for the
      * empty state.
      */
    private def joinedNames(files: Seq[UI.FilePayload]): Maybe[String] =
        if files.isEmpty then Absent else Present(files.map(_.name).mkString(", "))
end FileUpload
