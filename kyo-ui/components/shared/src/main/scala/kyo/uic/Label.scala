package kyo.uic

import kyo.*
import kyo.UI.*

/** Form-field label — native kyo-ui, PrimeOne design. Prime styles plain
  * `<label>` elements through the form layer, so the component renders a bare
  * `<label class="p-uic-label">` (kyo class — Prime ships no label CSS of its
  * own): `required` adds the asterisk marker class, `showColon` the trailing
  * colon, and `forId` forwards to the native `for` binding.
  *
  * This is a STANDALONE element, not a wrapper: it takes no field and renders
  * beside one, linked only by the `for`/`id` pair. Three different mechanisms sit
  * near each other in this module and it is worth keeping them apart:
  *   - standalone, linked by id — `Label` (here);
  *   - single-host wrappers that take the field and keep its concrete type so
  *     they can stamp classes on it — [[FloatLabel]], [[IftaLabel]], [[IconField]];
  *   - a container of already-built children in visual order — [[InputGroup]].
  */
final case class Label private (
    text: TextValue,
    forIdV: Maybe[String] = Absent,
    requiredFlag: Boolean = false,
    showColon: Boolean = false
) extends Node:
    type Self = Label

    def forId(id: String): Label     = copy(forIdV = Present(id))
    def required(v: Boolean): Label  = copy(requiredFlag = v)
    def showColon(v: Boolean): Label = copy(showColon = v)

    private[uic] def render(using Frame): UI =
        val base = label.cssClass("p-uic-label")
        val b1   = if requiredFlag then base.cssClass("p-uic-label-required") else base
        val b2   = if showColon then b1.cssClass("p-uic-label-colon") else b1
        val b3   = forIdV.map(b2.forId).getOrElse(b2)
        text match
            case TextValue.Const(t) => b3(t)
            case TextValue.Dyn(s)   => s.render(t => b3(t))
    end render
end Label

object Label:
    def apply(text: String): Label = new Label(TextValue.Const(text))

    /** A label whose text tracks `text` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(text: Signal[String]): Label = new Label(TextValue.Dyn(text))
end Label
