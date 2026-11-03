package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Toast]]'s visibility under rule A ([[ReactiveValueContract]]): a closable toast nobody binds is its own and starts
  * open, and the close button asks to close it. What only a toast has is below: a toast the reader cannot close has no
  * slot to hear.
  */
class ToastTest extends ReactiveValueContract:

    private def toast(using Frame) = uic.Toast().summary("Saved").closable(true)

    holds(new SlotCase[Toast, Boolean](
        name = "open",
        bare = Present(toast),
        bind = toast.open(_),
        hear = (t, f) => t.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = elementWithClass(_, "p-toast-close-button"),
        shows = elementsWithClass(_, "p-toast").map(_.nonEmpty)
    ))

    "a constant false shows nothing" in {
        for shut <- elementsWithClass(toast.open(false).render, "p-toast")
        yield assert(shut.isEmpty, "a constant false shows nothing")
    }

    "a toast the reader cannot close has nothing to hear, so a signal nobody hears is no card" in {
        for
            held <- Signal.initRef(true)
            mute <- elementsWithClass(toast.closable(false).open(held.readOnly).render, "p-uic-key-error")
        yield assert(mute.isEmpty, s"nothing the reader does can be lost: ${mute.map(textOf)}")
    }

end ToastTest
