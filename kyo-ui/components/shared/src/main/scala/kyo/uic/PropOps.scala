package kyo.uic

import kyo.*
import kyo.UI.*

/** What uic's components read off a [[kyo.UI.Prop]] beyond kyo-ui's own operations, as package-level extensions every
  * component reaches without an import.
  *
  * An extension on a prop lies on every value, since any value is a prop, so each one here is package-internal: no app sees
  * it, and a name a value already carries is never taken.
  */
extension [A](v: Maybe[Prop[A]])
    /** The optional prop as one prop of an option: `Absent` where none is set, a constant or a signal of `Present` otherwise,
      * so a reader of the prop sees an option either way (`live(_.onLabel.present)`).
      */
    private[uic] def present(using CanEqual[A, A], Frame): Prop[Maybe[A]] = v match
        case Present(a) => Prop.map(a)(Present(_))
        case _          => Absent
end extension
