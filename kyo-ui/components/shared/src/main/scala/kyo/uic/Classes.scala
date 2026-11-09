package kyo.uic

import kyo.*
import kyo.UI.*

extension [E <: UI.Ast.Element { type Self = E }](el: E)

    /** The class of the case `choice` names, for a prop over a closed set of cases (a severity, a variant, a size).
      *
      * A constant adds its one class, or none for a case `cls` gives none. A signal is one class-name channel
      * (`cssClass(Signal[Maybe[String]])`): a change takes the old case's class off and puts the new one's on, in place, and
      * draws nothing again; a static render writes the class of the case the signal holds.
      *
      * {{{
      * el.classFor(p.size)(_.cls("p-button"))
      * }}}
      */
    private[uic] def classFor[A](choice: Prop[A])(cls: A => Maybe[String])(using Frame): E =
        choice match
            case sig: Signal[A] @unchecked => el.cssClass(sig.map(cls))
            case a                         => cls(a.asInstanceOf[A]).fold(el)(el.cssClass(_))
end extension
