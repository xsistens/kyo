package kyo

/** kyo-ui-components: native PrimeOne component library for kyo-ui.
  *
  * Downstream usage (no name prefix; the namespace carries the origin):
  * {{{
  *   import kyo.*
  *   import kyo.UI.*
  *   import kyo.uic
  *   import scala.language.implicitConversions
  *
  *   uic.Button("Save").design(uic.ButtonDesign.Emphasized).icon("save").onClick(save)
  *   uic.Input().placeholder("Email").value(emailRef)   // two-way signal bind
  * }}}
  */
package object uic:
    /** Moves focus to the element with DOM id `id`, for a handler: it runs in a session, whichever page drew the id. */
    private[uic] def focusById(id: String)(using Frame): UI.Action = UI.commands.map(_.focusId(id))
end uic
