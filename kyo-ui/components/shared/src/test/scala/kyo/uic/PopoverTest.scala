package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Popover]]'s visibility under every way it can be bound: unbound it is the popover's own (closed with a trigger to
  * open it, open without one), a constant nobody hears is fixed, and a signal is followed and only reported.
  */
class PopoverTest extends UicTest:

    private def withTrigger(using Frame) = uic.Popover().trigger(span("More"))(p("inside"))

    private def triggers(t: UI)(using Frame) = elementsWithClass(t, "p-uic-popover-trigger")
    private def panels(t: UI)(using Frame)   = elementsWithClass(t, "p-popover")

    "an unbound popover with a trigger starts closed and keeps the state itself, so the trigger opens it" in {
        for
            first <- panels(withTrigger.render)
            after <- publishedAfterReady(
                withTrigger.render,
                t => triggers(t).map(_.exists(declaresClick)),
                until = t => panels(t).map(_.nonEmpty)
            )(t => triggers(t).map(b => click(b.head)))
            card <- elementsWithClass(withTrigger.render, "p-uic-key-error")
        yield
            assert(first.isEmpty, "it starts closed")
            assert(after.nonEmpty, "the trigger never opened it")
            assert(card.isEmpty, s"state the popover keeps itself: ${card.map(textOf)}")
        end for
    }

    "an unbound popover without a trigger starts open" in {
        for shown <- panels(uic.Popover()(p("inside")).render)
        yield assert(shown.size == 1, "nothing could open it, so it shows until dismissed")
    }

    "a constant nobody hears is fixed: the trigger toggles nothing" in {
        for
            t    <- elementWithClass(withTrigger.open(false).render, "p-uic-popover-trigger")
            card <- elementsWithClass(withTrigger.open(false).render, "p-uic-key-error")
        yield
            assert(!declaresClick(t), "the trigger is inert")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
    }

    "a signal bound one way is followed, and a toggle is reported without being written" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            t <- elementWithClass(withTrigger.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render, "p-uic-popover-trigger")
            _ <- click(t)
            h <- held.get
            s <- seen.get
            deaf <- elementsWithClass(withTrigger.open(held.readOnly).render, "p-uic-key-error")
        yield
            assert(!h && s == Present(true), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("Popover"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

end PopoverTest
