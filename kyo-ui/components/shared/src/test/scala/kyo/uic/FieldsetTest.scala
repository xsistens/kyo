package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Fieldset]]'s collapse state under rule A ([[ReactiveValueContract]]): a toggleable fieldset nobody binds keeps it
  * itself, starting expanded, and the toggle asks for the other state.
  */
class FieldsetTest extends ReactiveValueContract:

    holds(new Slot[Fieldset, Boolean](
        name = "collapsed",
        bare = uic.Fieldset().legend("Details").toggleable(true)(p("body")),
        bind = _.collapsed(_),
        hear = (c, f) => c.onCollapsedChange(f),
        event = "onCollapsedChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-fieldset-toggle-button"),
        shows = elementsWithClass(_, "p-uic-collapsed").map(_.nonEmpty)
    ))

end FieldsetTest
