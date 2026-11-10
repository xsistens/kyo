package kyo.uic

import kyo.*

/** A callback an app hands a component, wrapped so that its failure cannot cut the component's own work short.
  *
  * A component runs an app's callback in the middle of its own handler: a select fires `onChange` and then closes its
  * panel, a table commits a cell, reports it, and then closes the editor. Were the callback's failure to travel up that
  * chain, the panel would stay open and the editor with it. So the callback runs under [[kyo.UI.attempt]]: its failure
  * goes to the `onFailure` rules around the handler right where it ran, exactly as it would at the end of the handler,
  * and the component finishes.
  */
private[uic] object Callback:

    def apply(action: => UI.Action)(using Frame): Unit < Async =
        UI.attempt(action)

    def of[A](f: A => UI.Action)(using Frame): A => UI.Action =
        a => apply(f(a))

    def of2[A, B](f: (A, B) => UI.Action)(using Frame): (A, B) => UI.Action =
        (a, b) => apply(f(a, b))
end Callback

extension [P](props: UI.Live[P])
    /** The hearer `field` picks, read when its event comes: the latest run's, so a run that brings a new hearer draws nothing again
      * and the event still reaches it, and nothing where that run sets none.
      */
    private[uic] def hearing[A](field: P => Maybe[A => UI.Action])(using Frame): A => UI.Action =
        a => props.call(field).map(_.fold(Kyo.unit: UI.Action)(_(a)))
end extension
