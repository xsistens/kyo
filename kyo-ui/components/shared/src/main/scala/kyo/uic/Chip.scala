package kyo.uic

import kyo.*
import kyo.UI.*

/** Chip — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Chip
  * anatomy: `div.p-chip.p-component` > [`img.p-chip-image` | `.p-chip-icon`] +
  * `div.p-chip-label` + optional `.p-chip-remove-icon`), so the extracted
  * `@primeuix` chip CSS applies verbatim.
  *
  * The leading visual follows Prime's precedence: `image` wins over `icon`.
  * The remove affordance only renders with `removable(true)` (Prime
  * semantics); `onRemove` runs when it is activated by click, Enter, Space, or
  * Backspace (Prime's key set) — visibility stays app-owned, exactly like
  * [[Message.onDismissed]]. DEVIATION: the remove icon renders as a real
  * `<button class="p-chip-remove-icon">` around the glyph (Prime uses a bare
  * `<svg>` with handlers) — same class vocabulary, native focus/keyboard
  * semantics; the kyo remainder strips the button chrome.
  */
final case class Chip private (
    labelV: Maybe[TextValue] = Absent,
    iconV: Maybe[IconGlyph] = Absent,
    imageV: Maybe[String] = Absent,
    removableFlag: Boolean = false,
    removeIconV: Maybe[IconGlyph] = Absent,
    onRemoveEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil,
    extraClassesV: List[String] = Nil
) extends Node:
    type Self = Chip

    /** Package-internal class hook: hosts (MultiSelect) stamp Prime's contextual
      * class (`p-multiselect-chip`) onto the chip root.
      */
    private[uic] def extraClass(cls: String): Chip = copy(extraClassesV = extraClassesV :+ cls)

    /** Leading icon (`.p-chip-icon`); an `image` wins over it (Prime precedence). */
    def icon(glyph: IconGlyph): Chip = copy(iconV = Present(glyph))

    /** Leading image URL, rendered as the circular `img.p-chip-image`. */
    def image(url: String): Chip = copy(imageV = Present(url))

    /** Renders the remove affordance (`.p-chip-remove-icon`, hidden by default). */
    def removable(v: Boolean): Chip = copy(removableFlag = v)

    /** Glyph override for the remove affordance (Prime's `removeIcon`; default:
      * the times-circle glyph).
      */
    def removeIcon(glyph: IconGlyph): Chip = copy(removeIconV = Present(glyph))

    /** Runs `action` when the remove affordance is activated (implies nothing
      * about visibility — hide the chip from your own state if you want it gone).
      */
    def onRemove(action: => Any < Async)(using Frame): Chip =
        copy(onRemoveEff = Present(Sync.defer(action)))

    /** Adds default-slot children after the label. */
    def apply(cs: UI*): Chip = copy(kids = kids ++ cs)

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
            case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-chip-label")(t))
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
                List(btn(toChild(GlyphSvg(removeIconV.getOrElse(Icons.timesCircle)))))

        el((lead ++ labelChild ++ kids ++ removeChild).map(toChild)*)
    end render
end Chip

object Chip:
    /** A chip labelled `label` (rendered as `div.p-chip-label`). */
    def apply(label: String): Chip = new Chip(labelV = Present(TextValue.Const(label)))

    /** A chip whose label tracks `label` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): Chip = new Chip(labelV = Present(TextValue.Dyn(label)))

    /** An empty chip — fill the default slot via `apply(cs*)`. */
    def apply(): Chip = new Chip()
end Chip
