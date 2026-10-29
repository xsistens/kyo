package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Dialog]]'s visibility under rule A ([[ReactiveValueContract]]): unbound it is the dialog's own and starts open, and
  * the close button asks to close it. What only a dialog has is below: the other ways out a fixed one must close too.
  */
class DialogTest extends ReactiveValueContract:

    private def bare(using Frame) = uic.Dialog().header("Details")(p("body"))

    holds(new Slot[Dialog, Boolean](
        name = "open",
        bare = Present(bare),
        bind = bare.open(_),
        hear = (d, f) => d.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = elementWithClass(_, "p-dialog-close-button"),
        shows = elementsWithClass(_, "p-dialog").map(_.nonEmpty)
    ))

    "a dialog fixed open closes on neither Escape nor the backdrop, and one fixed shut shows nothing" in {
        val fixed = bare.open(true)
        for
            box  <- elementWithClass(fixed.render, "p-dialog")
            mask <- elementWithClass(fixed.render, "p-dialog-mask")
            shut <- elementsWithClass(bare.open(false).render, "p-dialog")
        yield
            assert(box.attrs.onKeyDown.isEmpty, "Escape closes nothing")
            assert(mask.attrs.onClick.isEmpty, "nor does the backdrop")
            assert(shut.isEmpty, "a constant false shows nothing")
        end for
    }

end DialogTest
