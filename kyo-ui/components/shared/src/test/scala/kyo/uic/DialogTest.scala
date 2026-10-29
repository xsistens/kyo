package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Dialog]]'s visibility under rule A ([[ReactiveValueContract]]): unbound it is the dialog's own and starts open, and
  * the close button asks to close it. What only a dialog has is below: the other ways out a fixed one must close too.
  */
class DialogTest extends ReactiveValueContract[Dialog, Boolean]:

    def bare(using Frame)                                     = uic.Dialog().header("Details")(p("body"))
    def bind(d: Dialog, v: Boolean | Signal[Boolean])         = d.open(v)
    def hear(d: Dialog, f: Boolean => UI.Action)(using Frame) = d.onOpenChange(f)
    def event                                                 = "onOpenChange"
    def render(d: Dialog)(using Frame)                        = d.render
    def start                                                 = true
    def next                                                  = false
    def control(t: UI)(using Frame)                           = elementWithClass(t, "p-dialog-close-button")
    def shows(t: UI)(using Frame)                             = elementsWithClass(t, "p-dialog").map(_.nonEmpty)

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
