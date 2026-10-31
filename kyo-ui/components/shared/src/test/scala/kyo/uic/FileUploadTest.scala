package kyo.uic

import kyo.*
import kyo.UI.*

/** [[FileUpload]]'s picked files under every way they can be bound, held to rule A ([[ReactiveValueContract]]): unbound
  * the control keeps the pick, a constant nobody hears is fixed, a signal is shown and a pick only reported, and a ref is
  * written before the report.
  */
class FileUploadTest extends ReactiveValueContract:

    private val report = UI.FilePayload("report.pdf", 1024, "application/pdf", "")

    private def inputOf(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-hidden-accessible")

    /** The files the label names, read back as the payloads of this suite: the label is all a render shows of them. */
    private def filesShown(t: UI)(using Frame): Seq[UI.FilePayload] < Sync =
        elementWithClass(t, "p-fileupload-filelabel").map(l =>
            textOf(l) match
                case "No file chosen" => Seq.empty
                case "report.pdf"     => Seq(report)
                case other            => throw new AssertionError(s"the label names $other")
        )

    /** Whether a pick goes nowhere and the control says so: the choose button in its role's words, and the root by declining
      * the browser's own dialog. The input the pick arrives on is hidden, so it is the button that speaks.
      */
    private def pickFixed(t: UI)(using Frame): Boolean < Sync =
        for
            in     <- inputOf(t)
            root   <- elementWithClass(t, "p-fileupload")
            choose <- elementWithClass(t, "p-fileupload-choose-button")
        yield in.attrs.onFileSelect.isEmpty && root.attrs.dataAttrs.contains("kyo-inert") &&
            choose.attrs.ariaAttrs.get("disabled").contains("true")

    holds(new Slot[uic.FileUpload, Seq[UI.FilePayload]](
        name = "value",
        bare = Present(uic.FileUpload()),
        bind = uic.FileUpload().value(_),
        hear = (u, f) => u.onChange(f),
        event = "onChange",
        render = _.render,
        start = Seq.empty,
        next = Seq(report),
        control = inputOf(_),
        shows = filesShown(_),
        gesture = (c: UI.Ast.Element) =>
            c.attrs.onFileSelect match
                case Present(f) => handled(f(Seq(report)))
                case Absent     => throw new AssertionError("the input takes no pick"),
        answers = _.attrs.onFileSelect.nonEmpty,
        isFixed = Present(pickFixed(_)),
        // Unbound, the label the pick names lives in the mount; the page as first rendered takes no pick.
        ready = Present(elementsWithClass(_, "p-hidden-accessible").map(_.exists(_.attrs.onFileSelect.nonEmpty)))
    ))

end FileUploadTest
