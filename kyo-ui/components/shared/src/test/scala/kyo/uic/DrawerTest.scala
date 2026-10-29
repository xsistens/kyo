package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Drawer]]'s visibility under rule A ([[ReactiveValueContract]]): unbound it is the drawer's own and starts open, and
  * the close button asks to close it. What only a drawer has is below: the other ways out a fixed one must close too.
  */
class DrawerTest extends ReactiveValueContract[Drawer, Boolean]:

    def bare(using Frame)                                     = uic.Drawer().header("Details")(p("body"))
    def bind(d: Drawer, v: Boolean | Signal[Boolean])         = d.open(v)
    def hear(d: Drawer, f: Boolean => UI.Action)(using Frame) = d.onOpenChange(f)
    def event                                                 = "onOpenChange"
    def render(d: Drawer)(using Frame)                        = d.render
    def start                                                 = true
    def next                                                  = false
    def control(t: UI)(using Frame)                           = elementWithClass(t, "p-drawer-close-button")
    def shows(t: UI)(using Frame)                             = elementsWithClass(t, "p-drawer").map(_.nonEmpty)

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
