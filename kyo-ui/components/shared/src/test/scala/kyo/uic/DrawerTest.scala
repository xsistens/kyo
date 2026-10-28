package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Drawer]]'s visibility under every way it can be bound: unbound it is the drawer's own and starts open, a constant
  * nobody hears is fixed, a signal is followed and a close only reported, and a ref is written.
  */
class DrawerTest extends UicTest:

    private def drawer(using Frame) = uic.Drawer().header("Details")(p("body"))

    private def closers(t: UI)(using Frame) = elementsWithClass(t, "p-drawer-close-button")

    "an unbound drawer starts open and keeps the state itself, so the close button closes it" in {
        for
            first <- elementsWithClass(drawer.render, "p-drawer")
            after <- publishedAfterReady(
                drawer.render,
                t => closers(t).map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-drawer").map(_.isEmpty)
            )(t => closers(t).map(b => click(b.head)))
            card <- elementsWithClass(drawer.render, "p-uic-key-error")
        yield
            assert(first.size == 1, "the page as first rendered already shows it")
            assert(after.nonEmpty, "the drawer never closed")
            assert(card.isEmpty, s"state the drawer keeps itself: ${card.map(textOf)}")
        end for
    }

    "the page as first rendered does not call an unbound close button fixed" in {
        for b <- elementWithClass(drawer.render, "p-drawer-close-button")
        yield assert(!b.attrs.ariaAttrs.contains("disabled"), "the button the reader will press is not disabled")
    }

    "a constant nobody hears is fixed: the drawer stays, and nothing closes it" in {
        val fixed = drawer.open(true)
        for
            b    <- elementWithClass(fixed.render, "p-drawer-close-button")
            box  <- elementWithClass(fixed.render, "p-drawer")
            mask <- elementWithClass(fixed.render, "p-drawer-mask")
            card <- elementsWithClass(fixed.render, "p-uic-key-error")
            shut <- elementsWithClass(drawer.open(false).render, "p-drawer")
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
            b    <- elementWithClass(drawer.open(true).onOpenChange(v => seen.set(Present(v))).render, "p-drawer-close-button")
            _    <- click(b)
            s    <- seen.get
        yield assert(s == Present(false), s"reported $s")
    }

    "a signal bound one way is followed, and a close is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            b    <- elementWithClass(drawer.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render, "p-drawer-close-button")
            _    <- click(b)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(drawer.open(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the drawer reports it" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef((true, true))
            b    <- elementWithClass(
                drawer.open(held).onOpenChange(v => held.get.map(h => seen.set((v, h)))).render,
                "p-drawer-close-button"
            )
            _ <- click(b)
            s <- seen.get
        yield assert(s == (false, false), s"reported, held: $s")
    }

end DrawerTest
