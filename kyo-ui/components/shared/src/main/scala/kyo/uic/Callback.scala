package kyo.uic

import kyo.*
import kyo.internal.Failures

/** A callback an app hands a component, wrapped so that its failure cannot cut the component's own work short.
  *
  * A component runs an app's callback in the middle of its own handler: a select fires `onChange` and then closes its
  * panel, a table commits a cell, reports it, and then closes the editor. Were the callback's failure to travel up that
  * chain, the panel would stay open and the editor with it. So the failure goes to the `onFailure` rules around the
  * handler right where the callback ran, exactly as it would at the end of the handler, and the component finishes.
  */
private[uic] object Callback:

    def apply(action: => UI.Action)(using Frame): Unit < Async =
        Failures.recover(())(Sync.defer(action).unit)

    def of[A](f: A => UI.Action)(using Frame): A => UI.Action =
        a => apply(f(a))

    def of2[A, B](f: (A, B) => UI.Action)(using Frame): (A, B) => UI.Action =
        (a, b) => apply(f(a, b))
end Callback
