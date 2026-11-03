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
end uic
