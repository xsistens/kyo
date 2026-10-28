package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Dialog]]'s visibility under every way it can be bound: unbound it is the dialog's own and starts open, a constant
  * nobody hears is fixed, a signal is followed and a close only reported, and a ref is written.
  */
class DialogTest extends UicTest:

    private def dialog(using Frame) = uic.Dialog().header("Details")(p("body"))

    private def closers(t: UI)(using Frame) = elementsWithClass(t, "p-dialog-close-button")

    "an unbound dialog starts open and keeps the state itself, so the close button closes it" in {
        for
            first <- elementsWithClass(dialog.render, "p-dialog")
            after <- publishedAfterReady(
                dialog.render,
                t => closers(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-dialog").map(_.isEmpty)
            )(t => closers(t).map(b => click(b.head)))
            card <- elementsWithClass(dialog.render, "p-uic-key-error")
        yield
            assert(first.size == 1, "the page as first rendered already shows it")
            assert(after.nonEmpty, "the dialog never closed")
            assert(card.isEmpty, s"state the dialog keeps itself: ${card.map(textOf)}")
        end for
    }

    "the page as first rendered does not call an unbound close button fixed" in {
        for b <- elementWithClass(dialog.render, "p-dialog-close-button")
        yield assert(!b.attrs.ariaAttrs.contains("disabled"), "the button the reader will press is not disabled")
    }

    "a constant nobody hears is fixed: the dialog stays, and nothing closes it" in {
        val fixed = dialog.open(true)
        for
            b    <- elementWithClass(fixed.render, "p-dialog-close-button")
            box  <- elementWithClass(fixed.render, "p-dialog")
            mask <- elementWithClass(fixed.render, "p-dialog-mask")
            card <- elementsWithClass(fixed.render, "p-uic-key-error")
            shut <- elementsWithClass(dialog.open(false).render, "p-dialog")
        yield
            assert(!declaresClick(b) && b.attrs.ariaAttrs.get("disabled").contains("true"), "the close button is inert and says so")
            assert(box.attrs.onKeyDown.isEmpty, "Escape closes nothing")
            assert(mask.attrs.onClick.isEmpty, "nor does the backdrop")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
            assert(shut.isEmpty, "a constant false shows nothing")
        end for
    }

    "a constant somebody hears reports the close it asks for" in {
        for
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            b    <- elementWithClass(dialog.open(true).onOpenChange(v => seen.set(Present(v))).render, "p-dialog-close-button")
            _    <- click(b)
            s    <- seen.get
        yield assert(s == Present(false), s"reported $s")
    }

    "a signal bound one way is followed, and a close is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            b    <- elementWithClass(dialog.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render, "p-dialog-close-button")
            _    <- click(b)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(dialog.open(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the dialog reports it" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef((true, true))
            b    <- elementWithClass(
                dialog.open(held).onOpenChange(v => held.get.map(h => seen.set((v, h)))).render,
                "p-dialog-close-button"
            )
            _ <- click(b)
            s <- seen.get
        yield assert(s == (false, false), s"reported, held: $s")
    }

end DialogTest
