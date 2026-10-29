package kyo.uic

import kyo.*
import kyo.UI.*

/** [[ConfirmDialog]]'s visibility under every way it can be bound. The two choices are heard apart: [[onAccept]] hears
  * an accept, [[onReject]] a reject and every dismissal, and [[onOpenChange]] both, which is the slot held to rule A
  * ([[ReactiveValueContract]]).
  */
class ConfirmDialogTest extends ReactiveValueContract:

    private def confirm(using Frame) = uic.ConfirmDialog().header("Delete?").message("This cannot be undone.")

    private def rejects(t: UI)(using Frame) = elementsWithClass(t, "p-confirmdialog-reject-button")
    private def closers(t: UI)(using Frame) = elementsWithClass(t, "p-dialog-close-button")
    private def dialogs(t: UI)(using Frame) = elementsWithClass(t, "p-confirmdialog")

    holds(new Slot[ConfirmDialog, Boolean](
        name = "open",
        bare = Present(confirm),
        bind = confirm.open(_),
        hear = (d, f) => d.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = elementWithClass(_, "p-confirmdialog-accept-button"),
        shows = dialogs(_).map(_.nonEmpty)
    ))

    "a fixed dialog's other ways out are inert too: the reject and the close button do nothing, and the close says so" in {
        val fixed = confirm.open(true)
        for
            r <- rejects(fixed.render)
            c <- closers(fixed.render)
        yield
            assert(r.size == 1 && c.size == 1, "it shows the reject and the close button")
            assert((r ++ c).forall(b => !declaresClick(b)), "nothing closes it")
            assert(c.forall(_.attrs.ariaAttrs.get("disabled").contains("true")), "and the close button says so")
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
