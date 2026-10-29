package kyo.uic

import kyo.*
import kyo.UI.*

/** [[FileUpload]]'s picked files under every way they can be bound: a constant nobody hears is fixed, a signal is shown
  * and a pick only reported, and a ref is written before the report.
  */
class FileUploadTest extends UicTest:

    private val report = UI.FilePayload("report.pdf", 1024, "application/pdf", "")

    private def inputOf(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-hidden-accessible")

    private def pick(t: UI, files: Seq[UI.FilePayload])(using Frame): Any < Async =
        inputOf(t).map(_.attrs.onFileSelect match
            case Present(f) => handled(f(files))
            case Absent     => throw new AssertionError("the input takes no pick"))

    "a constant nobody hears is fixed: the control stays reachable, opens no dialog, and says so" in {
        val fixed = uic.FileUpload().value(Seq(report))
        for
            in     <- inputOf(fixed.render)
            root   <- elementWithClass(fixed.render, "p-fileupload")
            choose <- elementWithClass(fixed.render, "p-fileupload-choose-button")
            card   <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(in.attrs.onFileSelect.isEmpty, "a pick goes nowhere")
            assert(root.attrs.dataAttrs.contains("kyo-inert"), "the browser opens no dialog")
            assert(choose.attrs.ariaAttrs.get("disabled").contains("true"), "and the button says so")
            assert(card.isEmpty, s"a fixed value is not a mistake: ${card.map(textOf)}")
        end for
    }

    "files bound one way are shown, and a pick is reported without being written" in {
        for
            held <- Signal.initRef(Seq.empty[UI.FilePayload])
            seen <- Signal.initRef(Seq.empty[UI.FilePayload])
            ui = uic.FileUpload().value(held.readOnly).onChange(seen.set).render
            _    <- pick(ui, Seq(report))
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(uic.FileUpload().value(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h.isEmpty && s == Seq(report), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the control reports it" in {
        for
            held <- Signal.initRef(Seq.empty[UI.FilePayload])
            seen <- Signal.initRef((Seq.empty[UI.FilePayload], Seq.empty[UI.FilePayload]))
            ui = uic.FileUpload().value(held).onChange(fs => held.get.map(h => seen.set((fs, h)))).render
            _ <- pick(ui, Seq(report))
            s <- seen.get
        yield assert(s == (Seq(report), Seq(report)), s"reported, held: $s")
    }

end FileUploadTest
