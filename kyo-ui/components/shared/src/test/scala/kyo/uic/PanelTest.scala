package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Panel]]'s collapse state under rule A ([[ReactiveValueContract]]): a toggleable panel nobody binds keeps it itself,
  * starting expanded, and the toggle asks for the other state.
  */
class PanelTest extends ReactiveValueContract[Panel, Boolean]:

    def bare(using Frame)                                    = uic.Panel().header("Details").toggleable(true)(p("body"))
    def bind(c: Panel, v: Boolean | Signal[Boolean])         = c.collapsed(v)
    def hear(c: Panel, f: Boolean => UI.Action)(using Frame) = c.onCollapsedChange(f)
    def event                                                = "onCollapsedChange"
    def render(c: Panel)(using Frame)                        = c.render
    def start                                                = false
    def next                                                 = true
    def control(t: UI)(using Frame)                          = elementWithClass(t, "p-panel-toggle-button")
    def shows(t: UI)(using Frame)                            = elementsWithClass(t, "p-uic-collapsed").map(_.nonEmpty)

end PanelTest
