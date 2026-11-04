package kyo.uic

import kyo.*
import kyo.UI.*

/** Chip: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Chip
  * anatomy: `div.p-chip.p-component` > [`img.p-chip-image` | `.p-chip-icon`] +
  * `div.p-chip-label` + optional `.p-chip-remove-icon`), so the extracted
  * `@primeuix` chip CSS applies verbatim.
  *
  * The leading visual follows Prime's precedence: `image` wins over `icon`.
  * The remove affordance only renders with `removable(true)` (Prime
  * semantics); `onRemove` runs when it is activated by click, Enter, Space, or
  * Backspace (Prime's key set); visibility stays app-owned, exactly like
  * [[Message.onDismissed]]. DEVIATION: the remove icon renders as a real
  * `<button class="p-chip-remove-icon">` around the glyph (Prime uses a bare
  * `<svg>` with handlers): same class vocabulary, native focus/keyboard
  * semantics; the kyo remainder strips the button chrome.
  */
final class Chip private[uic] (private[uic] val s: Chip.State) extends Node, Stated:
    import s.*

    type Self = Chip

    /** Package-internal class hook: hosts (MultiSelect) stamp Prime's contextual
      * class (`p-multiselect-chip`) onto the chip root.
      */
    private[uic] def extraClass(cls: String): Chip = new Chip(s.copy(extraClassesV = extraClassesV :+ cls))

    /** Package-internal: ends the remove button's own events at the button (kyo's `stopPropagation`).
      *
      * For a host that renders the chip inside an element listening for the same events: MultiSelect's
      * field toggles its panel on a click and opens it on Enter or Space, and a remove is neither. The
      * stop covers both event types the button declares, so a key pressed on the focused remove button
      * stays the button's too. Off by default, so a chip an app places in a clickable container of its
      * own keeps bubbling as before.
      */
    private[uic] def isolateRemove(v: Boolean): Chip = new Chip(s.copy(isolateRemoveFlag = v))

    /** Leading icon (`.p-chip-icon`); an `image` wins over it (Prime precedence). */
    def icon(glyph: IconGlyph): Chip = new Chip(s.copy(iconV = Present(glyph)))

    /** Leading image URL, rendered as the circular `img.p-chip-image`. */
    def image(url: String): Chip = new Chip(s.copy(imageV = Present(url)))

    /** Renders the remove affordance (`.p-chip-remove-icon`, hidden by default). */
    def removable(v: Boolean): Chip = new Chip(s.copy(removableFlag = v))

    /** Glyph override for the remove affordance (Prime's `removeIcon`; default:
      * the times-circle glyph).
      */
    def removeIcon(glyph: IconGlyph): Chip = new Chip(s.copy(removeIconV = Present(glyph)))

    /** Runs `action` when the remove affordance is activated (implies nothing
      * about visibility; hide the chip from your own state if you want it gone).
      */
    def onRemove(action: => UI.Action)(using Frame): Chip =
        new Chip(s.copy(onRemoveEff = Present(Callback(action))))

    /** Adds default-slot children after the label. */
    def apply(cs: UI*): Chip = new Chip(s.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        var el = div.cssClass("p-chip").cssClass("p-component")
        extraClassesV.foreach(c => el = el.cssClass(c))
        // A reactive label carries no static aria string (the visible p-chip-label
        // conveys it); only a constant label mirrors onto aria-label.
        labelV.foreach {
            case TextValue.Const(l) => el = el.aria("label", l)
            case TextValue.Dyn(_)   => ()
        }

        // Prime precedence: image > icon.
        val lead: List[UI] = (imageV, iconV) match
            case (Present(url), _)    => List(img(ImgSrc.Path(url), labelV.map(_.constOrEmpty).getOrElse("")).cssClass("p-chip-image"))
            case (Absent, Present(g)) => List(GlyphSvg(g, "p-chip-icon"))
            case _                    => Nil
        val labelChild: List[UI] = labelV.toList.map {
            case TextValue.Const(l) => div.cssClass("p-chip-label")(l): UI
            case TextValue.Dyn(s)   => div.cssClass("p-chip-label")(s)
        }
        val removeChild: List[UI] =
            if !removableFlag then Nil
            else
                var btn = button
                    .cssClass("p-chip-remove-icon")
                    .jsProp("type", "button")
                    .aria("label", "Remove")
                onRemoveEff.foreach { e =>
                    btn = btn.onClick(e).onKeyDown { evt =>
                        evt.key match
                            case Keyboard.Backspace => e
                            case _                  => () // Enter/Space activate the native button already
                    }
                }
                if isolateRemoveFlag then btn = btn.stopPropagation(true)
                List(btn(GlyphSvg(removeIconV.getOrElse(Icons.timesCircle))))

        el(UI.children(lead ++ labelChild ++ kids ++ removeChild)*)
    end render
end Chip

object Chip:
    final private[uic] case class State(
        labelV: Maybe[TextValue] = Absent,
        iconV: Maybe[IconGlyph] = Absent,
        imageV: Maybe[String] = Absent,
        removableFlag: Boolean = false,
        removeIconV: Maybe[IconGlyph] = Absent,
        onRemoveEff: Maybe[UI.Action] = Absent,
        kids: List[UI] = Nil,
        extraClassesV: List[String] = Nil,
        isolateRemoveFlag: Boolean = false
    )

    /** A chip labelled `label`. A `Signal[String]` re-renders the label in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: String | Signal[String]): Chip = new Chip(State(labelV = Present(ReactiveValue(label))))

    /** An empty chip; fill the default slot via `apply(cs*)`. */
    def apply(): Chip = new Chip(State())
end Chip
