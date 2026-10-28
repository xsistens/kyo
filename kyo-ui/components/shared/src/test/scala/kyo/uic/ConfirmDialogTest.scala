package kyo.uic

import kyo.*
import kyo.UI.*

/** [[ConfirmDialog]]'s visibility under every way it can be bound. The two choices are heard apart: [[onAccept]] hears
  * an accept, [[onReject]] a reject and every dismissal, and [[onOpenChange]] both.
  */
class ConfirmDialogTest extends UicTest:

    private def confirm(using Frame) = uic.ConfirmDialog().header("Delete?").message("This cannot be undone.")

    private def accepts(t: UI)(using Frame) = elementsWithClass(t, "p-confirmdialog-accept-button")
    private def rejects(t: UI)(using Frame) = elementsWithClass(t, "p-confirmdialog-reject-button")
    private def closers(t: UI)(using Frame) = elementsWithClass(t, "p-dialog-close-button")
    private def dialogs(t: UI)(using Frame) = elementsWithClass(t, "p-confirmdialog")

    "an unbound confirm dialog starts open and keeps the state itself, so a choice closes it" in {
        for
            first <- dialogs(confirm.render)
            after <- publishedAfterReady(
                confirm.render,
                t => accepts(t).map(_.exists(declaresClick)),
                until = t => dialogs(t).map(_.isEmpty)
            )(t => accepts(t).map(b => click(b.head)))
            card <- elementsWithClass(confirm.render, "p-uic-key-error")
        yield
            assert(first.size == 1, "the page as first rendered already shows it")
            assert(after.nonEmpty, "the confirm dialog never closed")
            assert(card.isEmpty, s"state the dialog keeps itself: ${card.map(textOf)}")
        end for
    }

    "a constant nobody hears is fixed: neither choice nor the close button does anything" in {
        val fixed = confirm.open(true)
        for
            a    <- accepts(fixed.render)
            r    <- rejects(fixed.render)
            c    <- closers(fixed.render)
            card <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(a.size == 1 && r.size == 1 && c.size == 1, "it shows both choices and the close button")
            assert((a ++ r ++ c).forall(b => !declaresClick(b)), "nothing closes it")
            assert((a ++ c).forall(_.attrs.ariaAttrs.get("disabled").contains("true")), "and the buttons say so")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
        end for
    }

    "onAccept hears an accept only: the reject and the close button stay inert" in {
        for
            seen <- Signal.initRef(List.empty[String])
            ui = confirm.open(true).onAccept(seen.updateAndGet(_ :+ "accept")).render
            a <- elementWithClass(ui, "p-confirmdialog-accept-button")
            _ <- click(a)
            r <- elementWithClass(ui, "p-confirmdialog-reject-button")
            c <- elementWithClass(ui, "p-dialog-close-button")
            s <- seen.get
        yield
            assert(s == List("accept"), s"heard $s")
            assert(!declaresClick(c) && c.attrs.ariaAttrs.get("disabled").contains("true"), "nobody hears a dismissal")
            assert(r.attrs.ariaAttrs.get("disabled").contains("true"), "nor a reject")
        end for
    }

    "a signal bound one way is followed, and a choice is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(List.empty[String])
            ui = confirm.open(held.readOnly).onOpenChange(v => seen.updateAndGet(_ :+ s"open $v"))
                .onReject(seen.updateAndGet(_ :+ "reject")).render
            r        <- elementWithClass(ui, "p-confirmdialog-reject-button")
            _        <- click(r)
            h        <- held.get
            s        <- seen.get
            deaf     <- elementsWithClass(confirm.open(held.readOnly).render, "p-uic-key-error")
            halfDeaf <- elementsWithClass(confirm.open(held.readOnly).onAccept(()).render, "p-uic-key-error")
        yield
            assert(h && s == List("open false", "reject"), s"held $h, heard $s")
            assert(deaf.nonEmpty && deaf.map(textOf).mkString.contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(halfDeaf.nonEmpty, s"nobody hears a reject: ${halfDeaf.map(textOf)}")
        end for
    }

    "a ref is written before the choice is reported" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(List.empty[String])
            ui = confirm.open(held).onAccept(held.get.map(h => seen.updateAndGet(_ :+ s"accept held $h"))).render
            a <- elementWithClass(ui, "p-confirmdialog-accept-button")
            _ <- click(a)
            s <- seen.get
        yield assert(s == List("accept held false"), s"heard $s")
    }

end ConfirmDialogTest
