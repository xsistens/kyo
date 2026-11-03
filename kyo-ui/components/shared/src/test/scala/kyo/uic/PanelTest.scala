package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Panel]]'s collapse state under rule A ([[ReactiveValueContract]]): a toggleable panel nobody binds keeps it itself,
  * starting expanded, and the toggle asks for the other state.
  */
class PanelTest extends ReactiveValueContract:

    private def panel(using Frame) = uic.Panel().header("Details").toggleable(true)(p("body"))

    holds(new SlotCase[Panel, Boolean](
        name = "collapsed",
        bare = Present(panel),
        bind = panel.collapsed(_),
        hear = (c, f) => c.onCollapsedChange(f),
        event = "onCollapsedChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-panel-toggle-button"),
        shows = elementsWithClass(_, "p-uic-collapsed").map(_.nonEmpty)
    ))

end PanelTest
