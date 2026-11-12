package kyo.uic

import kyo.*
import kyo.UI.*

/** Tag: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Tag
  * anatomy: `span.p-tag[.p-tag-<severity>][.p-tag-rounded]` > optional
  * `.p-tag-icon` + `span.p-tag-label`), so the extracted `@primeuix` tag CSS
  * applies verbatim.
  *
  * The default `Primary` severity renders the base `.p-tag` skin, the primary chip.
  * Prime's tag vocabulary is success/info/warn/danger/secondary/contrast;
  * `Primary` and `Help` keep the unsuffixed base skin. `onClick` is a kyo
  * extension (Prime's Tag is inert): it adds the pointer cursor and makes the
  * chip a control, `role="button"` and one tab stop answering Enter and Space,
  * since an action only the mouse can reach is not an action for everyone.
  * It keeps no state, so it has no instance: a host region that runs again has
  * nothing of it to lose.
  */
final class Tag private[uic] (private[uic] val p: Tag.Props) extends Component(Setters[Tag.Props, Tag]), Node, HasElementId:

    type Self = Tag

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new Tag(p.copy(access = v))

    /** Adds default-slot children after the label. */
    def apply(cs: UI*): Tag = new Tag(p.copy(extraChildren = p.extraChildren ++ cs))

    /** Draws the tag. Nothing of it decides the structure but what is fixed when the tag is built (a click, an icon, a label, the
      * children): the severity and `rounded` reach the root through its class channels, the label through its text channel, and an
      * icon signal swaps the glyph in its own region below the tag, so constants draw no region and a signal patches in place. A tag
      * that acts on a click is a control, reachable and operable as one: the pointer cursor alone would leave the action to the
      * mouse. Card, Avatar and Icon carry the same three settings.
      */
    private[uic] def render(using Frame): UI =
        span.cssClass("p-tag", "p-component")
            .when(p.access.id)(_.id(_))
            .cssClass.matching(p.severity) {
                case Severity.Primary   => Absent
                case Severity.Secondary => "p-tag-secondary"
                case Severity.Success   => "p-tag-success"
                case Severity.Info      => "p-tag-info"
                case Severity.Warn      => "p-tag-warn"
                case Severity.Danger    => "p-tag-danger"
                case Severity.Help      => Absent
                case Severity.Contrast  => "p-tag-contrast"
            }
            .when(p.rounded)(_.cssClass("p-tag-rounded"))
            .when(p.onClick) { (el, act) =>
                el.cssClass("p-uic-clickable").role("button").tabIndex(0).onClick(act).onKeyDown { evt =>
                    evt.key match
                        case Keyboard.Enter | Keyboard.Space => act
                        case _                               => ()
                }
            }(UI.children(
                p.icon.map(GlyphSvg(_, "p-tag-icon")),
                p.label.map(text => span.cssClass("p-tag-label")(text)),
                p.extraChildren
            )*)
    end render
end Tag

object Tag:
    /** What a tag holds. Each field is a setter of its name (`uic.Tag("Done").severity(uic.Severity.Success)`), which takes a
      * constant or a signal where the field is a `Prop`, the value itself where it is an `Opt`, and `onClick` its action by name;
      * the children are added through `apply(cs*)`, and `access` is written by the shared trait (`id`).
      */
    final private[uic] case class Props(
        /** The label (`span.p-tag-label`). A signal patches the label text in place (kyo-ui's text channel: no region, no new
          * label span), e.g. a locale-driven `I18n.t` leaf or a counter.
          */
        label: Opt[Prop[String]] = Opt.unset,
        /** Semantic accent (`.p-tag-<severity>`); `Primary` (the default) and `Help` keep the primary base skin. A signal swaps
          * the class in place via kyo-ui's class channel, with no region.
          */
        severity: Prop[Severity] = Severity.Primary,
        /** Fully rounded pill corners (`.p-tag-rounded`), a signal toggling the class in place. */
        rounded: Prop[Boolean] = false,
        /** Leading icon (`.p-tag-icon`), inheriting the chip's text color. A signal swaps the glyph in its own region, below the
          * tag.
          */
        icon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** What a click (or Enter and Space) runs, a kyo extension: it adds the pointer cursor, `role="button"` and one tab stop.
          * Its setter takes the action by name, built each time the event comes, and a failure goes to the `onFailure` rules where
          * it ran.
          */
        onClick: Maybe[UI.Action] = Absent,
        @noSetter access: Accessibility = Accessibility.none,
        @noSetter extraChildren: List[UI] = Nil
    ) derives CanEqual

    /** A tag labelled `label`. A signal patches the label text in place on emission. */
    def apply(label: Prop[String]): Tag = new Tag(Props(label = label))

    /** An empty tag; fill the default slot via `apply(cs*)`. */
    def apply(): Tag = new Tag(Props())
end Tag
