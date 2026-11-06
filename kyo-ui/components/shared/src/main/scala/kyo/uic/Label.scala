package kyo.uic

import kyo.*
import kyo.UI.*

/** Form-field label: native kyo-ui, PrimeOne design. Prime styles plain
  * `<label>` elements through the form layer, so the component renders a bare
  * `<label class="p-uic-label">` (kyo class; Prime ships no label CSS of its
  * own): `required` adds the asterisk marker class, `showColon` the trailing
  * colon, and `forId` forwards to the native `for` binding.
  *
  * This is a STANDALONE element, not a wrapper: it takes no field and renders
  * beside one, linked only by the `for`/`id` pair. Three different mechanisms sit
  * near each other in this module and it is worth keeping them apart:
  *   - standalone, linked by id: `Label` (here);
  *   - single-host wrappers that take the field and keep its concrete type so
  *     they can stamp classes on it: [[FloatLabel]], [[IftaLabel]], [[IconField]];
  *   - a container of already-built children in visual order: [[InputGroup]].
  */
final class Label private[uic] (private[uic] val p: Label.Props) extends Node, HasProps:
    import p.*

    type Self = Label

    def forId(id: String): Label     = new Label(p.copy(forIdV = Present(id)))
    def required(v: Boolean): Label  = new Label(p.copy(requiredFlag = v))
    def showColon(v: Boolean): Label = new Label(p.copy(showColon = v))

    private[uic] def render(using Frame): UI =
        val base = label.cssClass("p-uic-label")
        val b1   = if requiredFlag then base.cssClass("p-uic-label-required") else base
        val b2   = if p.showColon then b1.cssClass("p-uic-label-colon") else b1
        val b3   = forIdV.map(b2.forId).getOrElse(b2)
        text match
            case TextValue.Const(t) => b3(t)
            case TextValue.Dyn(s)   => b3(s)
    end render
end Label

object Label:
    final private[uic] case class Props(
        text: TextValue,
        forIdV: Maybe[String] = Absent,
        requiredFlag: Boolean = false,
        showColon: Boolean = false
    )

    /** A label showing `text`. A `Signal[String]` re-renders it in place on emission (e.g. a locale-driven
      * `I18n.t` leaf).
      */
    def apply(v: String | Signal[String]): Label = new Label(Props(ReactiveValue(v)))
end Label
