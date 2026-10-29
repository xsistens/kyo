package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Drawer]]'s visibility under rule A ([[ReactiveValueContract]]): unbound it is the drawer's own and starts open, and
  * the close button asks to close it. What only a drawer has is below: the other ways out a fixed one must close too.
  */
class DrawerTest extends ReactiveValueContract:

    private def bare(using Frame) = uic.Drawer().header("Details")(p("body"))

    holds(new Slot[Drawer, Boolean](
        name = "open",
        bare = Present(bare),
        bind = bare.open(_),
        hear = (d, f) => d.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = elementWithClass(_, "p-drawer-close-button"),
        shows = elementsWithClass(_, "p-drawer").map(_.nonEmpty)
    ))

    "a drawer fixed open closes on neither Escape nor the backdrop, and one fixed shut shows nothing" in {
        val fixed = bare.open(true)
        for
            box  <- elementWithClass(fixed.render, "p-drawer")
            mask <- elementWithClass(fixed.render, "p-drawer-mask")
            shut <- elementsWithClass(bare.open(false).render, "p-drawer")
        yield
            assert(box.attrs.onKeyDown.isEmpty, "Escape closes nothing")
            assert(mask.attrs.onClick.isEmpty, "nor does the backdrop")
            assert(shut.isEmpty, "a constant false shows nothing")
        end for
    }

end DrawerTest
