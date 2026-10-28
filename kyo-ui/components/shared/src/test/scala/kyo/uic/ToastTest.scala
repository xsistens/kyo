package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Toast]]'s visibility under every way it can be bound: unbound it is the toast's own and starts open, a constant
  * nobody hears is fixed, a signal is followed and a close only reported, and a ref is written.
  */
class ToastTest extends UicTest:

    private def toast(using Frame) = uic.Toast().summary("Saved").closable(true)

    private def closers(t: UI)(using Frame) = elementsWithClass(t, "p-toast-close-button")

    "an unbound toast starts open and keeps the state itself, so the close button closes it" in {
        for
            first <- elementsWithClass(toast.render, "p-toast")
            after <- publishedAfterReady(
                toast.render,
                t => closers(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-toast").map(_.isEmpty)
            )(t => closers(t).map(b => click(b.head)))
            card <- elementsWithClass(toast.render, "p-uic-key-error")
        yield
            assert(first.size == 1, "the page as first rendered already shows it")
            assert(after.nonEmpty, "the toast never closed")
            assert(card.isEmpty, s"state the toast keeps itself: ${card.map(textOf)}")
        end for
    }

    "the page as first rendered does not call an unbound close button fixed" in {
        for b <- elementWithClass(toast.render, "p-toast-close-button")
        yield assert(!b.attrs.ariaAttrs.contains("disabled"), "the button the reader will press is not disabled")
    }

    "a constant nobody hears is fixed: the toast stays, and its close button closes nothing" in {
        for
            b    <- elementWithClass(toast.open(true).render, "p-toast-close-button")
            card <- elementsWithClass(toast.open(true).render, "p-uic-key-error")
            shut <- elementsWithClass(toast.open(false).render, "p-toast")
        yield
            assert(!declaresClick(b) && b.attrs.ariaAttrs.get("disabled").contains("true"), "the close button is inert and says so")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
            assert(shut.isEmpty, "a constant false shows nothing")
    }

    "a signal bound one way is followed, and a close is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            b    <- elementWithClass(toast.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render, "p-toast-close-button")
            _    <- click(b)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(toast.open(held.readOnly).render, "p-uic-key-error")
            mute <- elementsWithClass(toast.closable(false).open(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(mute.isEmpty, s"a toast the reader cannot close has nothing to hear: ${mute.map(textOf)}")
        end for
    }

    "a ref is written before the toast reports it" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef((true, true))
            b    <- elementWithClass(
                toast.open(held).onOpenChange(v => held.get.map(h => seen.set((v, h)))).render,
                "p-toast-close-button"
            )
            _ <- click(b)
            s <- seen.get
        yield assert(s == (false, false), s"reported, held: $s")
    }

end ToastTest
