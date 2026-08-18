package kyo.uic

import kyo.*
import kyo.UI.*

/** Tag — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Tag
  * anatomy: `span.p-tag[.p-tag-<severity>][.p-tag-rounded]` > optional
  * `.p-tag-icon` + `span.p-tag-label`), so the extracted `@primeuix` tag CSS
  * applies verbatim.
  *
  * Without a `severity`, the base `.p-tag` skin renders the primary chip.
  * Prime's tag vocabulary is success/info/warn/danger/secondary/contrast —
  * `Primary` (and `Help`) keep the unsuffixed base skin. `onClick` is a kyo
  * extension (Prime's Tag is inert); it adds the pointer cursor.
  */
final case class Tag private (
    labelV: Maybe[TextValue] = Absent,
    severityV: Maybe[SeverityValue] = Absent,
    roundedFlag: Boolean = false,
    iconV: Maybe[IconGlyph] = Absent,
    onClickEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Tag

    /** Semantic accent (`.p-tag-<token>`); unset keeps the primary base skin. */
    def severity(v: Severity): Tag = copy(severityV = Present(SeverityValue.Const(v)))

    /** Reactive accent — the `.p-tag-<token>` class is swapped IN PLACE via kyo-ui's class
      * channel on emission (no re-render).
      */
    def severity(sig: Signal[Severity]): Tag = copy(severityV = Present(SeverityValue.Dyn(sig)))

    /** Fully rounded pill corners (`.p-tag-rounded`). */
    def rounded(v: Boolean): Tag = copy(roundedFlag = v)

    /** Leading icon (`.p-tag-icon`), inheriting the chip's text color. */
    def icon(glyph: IconGlyph): Tag = copy(iconV = Present(glyph))

    /** Runs `action` when the tag is clicked (kyo extension; adds the pointer cursor). */
    def onClick(action: => Any < Async)(using Frame): Tag = copy(onClickEff = Present(Sync.defer(action)))

    /** Adds default-slot children after the label. */
    def apply(cs: UI*): Tag = copy(kids = kids ++ cs)

    /** The `.p-tag-<token>` suffix for a severity; `Primary`/`Help` render the unsuffixed base skin. */
    private def tagToken(s: Severity): Maybe[String] = s match
        case Severity.Success   => Present("success")
        case Severity.Info      => Present("info")
        case Severity.Warn      => Present("warn")
        case Severity.Danger    => Present("danger")
        case Severity.Secondary => Present("secondary")
        case Severity.Contrast  => Present("contrast")
        case _                  => Absent

    private[uic] def render(using Frame): UI =
        var el = span.cssClass("p-tag").cssClass("p-component")
        severityV match
            case Present(SeverityValue.Const(s)) => tagToken(s).foreach(t => el = el.cssClass(s"p-tag-$t"))
            case Present(SeverityValue.Dyn(sig)) =>
                Severity.values.foreach(s => tagToken(s).foreach(t => el = el.cssClass(s"p-tag-$t", sig.map(_ == s))))
            case Absent => ()
        end match
        if roundedFlag then el = el.cssClass("p-tag-rounded")
        onClickEff.foreach(e => el = el.cssClass("p-uic-clickable").onClick(e))
        val iconChild: List[UI] = iconV.toList.map(g => GlyphSvg(g, "p-tag-icon"))
        val labelChild: List[UI] = labelV.toList.map {
            case TextValue.Const(t) => span.cssClass("p-tag-label")(t)
            case TextValue.Dyn(s)   => s.render(t => span.cssClass("p-tag-label")(t))
        }
        el((iconChild ++ labelChild ++ kids).map(toChild)*)
    end render
end Tag

object Tag:
    /** A tag labelled `label` (rendered as `span.p-tag-label`). */
    def apply(label: String): Tag = new Tag(labelV = Present(TextValue.Const(label)))

    /** A tag whose label tracks `label` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def apply(label: Signal[String]): Tag = new Tag(labelV = Present(TextValue.Dyn(label)))

    /** An empty tag — fill the default slot via `apply(cs*)`. */
    def apply(): Tag = new Tag()
end Tag
