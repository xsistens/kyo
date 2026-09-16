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
  * extension (Prime's Tag is inert): it adds the pointer cursor and makes the
  * chip a control, `role="button"` and one tab stop answering Enter and Space,
  * since an action only the mouse can reach is not an action for everyone.
  */
final case class Tag private (
    labelV: Maybe[TextValue] = Absent,
    severityV: Maybe[SeverityValue] = Absent,
    roundedFlag: Boolean = false,
    iconV: Maybe[IconGlyph] = Absent,
    onClickEff: Maybe[Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    kids: List[UI] = Nil
) extends Node, HasElementId:
    type Self = Tag

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Tag = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Semantic accent (`.p-tag-<token>`); unset keeps the primary base skin. A `Signal[Severity]` swaps
      * the class IN PLACE via kyo-ui's class channel on emission (no re-render).
      */
    def severity(v: Severity | Signal[Severity]): Tag = copy(severityV = Present(ReactiveValue(v)))

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
        idV.foreach(v => el = el.id(v))
        severityV match
            case Present(SeverityValue.Const(s)) => tagToken(s).foreach(t => el = el.cssClass(s"p-tag-$t"))
            case Present(SeverityValue.Dyn(sig)) =>
                Severity.values.foreach(s => tagToken(s).foreach(t => el = el.cssClass(s"p-tag-$t", sig.map(_ == s))))
            case Absent => ()
        end match
        if roundedFlag then el = el.cssClass("p-tag-rounded")
        // A tag that acts on a click is a control, so it is reachable and operable as one: the
        // pointer cursor alone left the action to the mouse, which is the one reader a chip like
        // this is easiest to overlook. Card, Avatar and Icon carry the same three lines.
        onClickEff.foreach { e =>
            el = el
                .cssClass("p-uic-clickable")
                .role("button")
                .tabIndex(0)
                .onClick(e)
                .onKeyDown { evt =>
                    evt.key match
                        case Keyboard.Enter | Keyboard.Space => e
                        case _                               => ()
                }
        }
        val iconChild: List[UI] = iconV.toList.map(g => GlyphSvg(g, "p-tag-icon"))
        val labelChild: List[UI] = labelV.toList.map {
            case TextValue.Const(t) => span.cssClass("p-tag-label")(t)
            // The signal goes in as the label's CHILD, not as a region around the span: a
            // `Signal[String]` child lifts to a `Reactive` carrying its string signal, which
            // `ReactiveUI.bindTextRegion` binds straight to the text write. A region around
            // the span would rebuild the span on every emission to change one character.
            case TextValue.Dyn(s) => span.cssClass("p-tag-label")(s)
        }
        el((iconChild ++ labelChild ++ kids).map(toChild)*)
    end render
end Tag

object Tag:
    /** A tag labelled `label`. A `Signal[String]` patches the label text IN PLACE on emission (kyo-ui's text
      * channel — no region, no re-render of the label span), e.g. a locale-driven `I18n.t` leaf or a counter.
      */
    def apply(label: String | Signal[String]): Tag = new Tag(labelV = Present(ReactiveValue(label)))

    /** An empty tag — fill the default slot via `apply(cs*)`. */
    def apply(): Tag = new Tag()
end Tag
