package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Fieldset]]'s collapse state under rule A ([[ReactiveValueContract]]): a toggleable fieldset nobody binds keeps it
  * itself, starting expanded, and the toggle asks for the other state.
  */
class FieldsetTest extends ReactiveValueContract[Fieldset, Boolean]:

    def bare(using Frame)                                       = uic.Fieldset().legend("Details").toggleable(true)(p("body"))
    def bind(c: Fieldset, v: Boolean | Signal[Boolean])         = c.collapsed(v)
    def hear(c: Fieldset, f: Boolean => UI.Action)(using Frame) = c.onCollapsedChange(f)
    def event                                                   = "onCollapsedChange"
    def render(c: Fieldset)(using Frame)                        = c.render
    def start                                                   = false
    def next                                                    = true
    def control(t: UI)(using Frame)                             = elementWithClass(t, "p-fieldset-toggle-button")
    def shows(t: UI)(using Frame)                               = elementsWithClass(t, "p-uic-collapsed").map(_.nonEmpty)

end FieldsetTest
