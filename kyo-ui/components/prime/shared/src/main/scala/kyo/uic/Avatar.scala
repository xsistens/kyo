package kyo.uic

import kyo.*
import kyo.UI.*

/** Avatar shapes (`.p-avatar-circle` vs the default rounded square). */
enum AvatarShape derives CanEqual:
    case Circle, Square

/** Avatar: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Avatar
  * anatomy: `div.p-avatar[.p-avatar-circle][.p-avatar-lg|.p-avatar-xl][.p-avatar-image]`
  * containing initials (`span.p-avatar-text`), an icon, or a native `<img>`), so
  * the extracted `@primeuix` avatar CSS applies verbatim.
  *
  * Content precedence follows Prime: initials win over `icon`, `icon` wins over
  * `image`. kyo extensions beyond Prime's surface: `onClick` (the avatar becomes a
  * control: focusable `role="button"`, Enter and Space run the click), `disabled`
  * (`.p-disabled` + `aria-disabled`; a control is then silent and out of the tab
  * order), and the `badge` overlay rendered as `span.p-uic-avatar-badge`. Every
  * value takes a constant or a signal, and a signal patches its place. It keeps no
  * state, so it has no instance: a host region that runs again has nothing of it to
  * lose.
  */
final class Avatar private[uic] (private[uic] val p: Avatar.Props) extends Component(Setters[Avatar.Props, Avatar]), Node,
      HasAccessibleName, HasElementId:

    type Self = Avatar

    private[uic] def accessibility: Accessibility              = p.access
    private[uic] def withAccessibility(v: Accessibility): Self = new Avatar(p.copy(access = v))

    /** Draws the avatar. What decides the structure is fixed when the avatar is built: which content is set (the initials, else the
      * icon, else the image), whether it acts on a click, whether a badge is set. Everything else reaches its place through a
      * channel: the shape and the size through the root's class channels, `disabled` through its class, `aria-disabled`,
      * silence and, on a control, tab stop channels, the initials through their text channel, the accessible name (the initials
      * where none is set) through `aria-label` and the image's `alt`, the image through its `src` channel, and an icon signal
      * swaps the glyph in its own region below the avatar. So constants draw no region, and a signal patches in place.
      */
    private[uic] def render(using Frame): UI =
        val control            = p.onClick.isDefined
        val name: Prop[String] = p.access.name.getOrElse(p.initials.getOrElse("Avatar"))
        val content: UI        = p.initials.map(text => span.cssClass("p-avatar-text")(text): UI)
            .orElse(p.icon.map(GlyphSvg(_, "p-avatar-icon")))
            .orElse(p.image.map(url => img(Prop.map[String, ImgSrc](url)(ImgSrc.Path(_)), name): UI))
            .getOrElse(span.cssClass("p-avatar-text")(""))
        // A control is a tab stop while it is not disabled, an avatar that is no control never one.
        val tabStop: Prop[Boolean] = if control then Prop.map[Boolean, Boolean](p.disabled)(!_) else false
        div.cssClass("p-avatar", "p-component")
            .when(p.access.id)(_.id(_))
            .cssClass.matching(p.shape) {
                case AvatarShape.Square => Absent
                case AvatarShape.Circle => "p-avatar-circle"
            }
            .cssClass.matching(p.size) {
                case Size.Small  => Absent
                case Size.Normal => Absent
                case Size.Large  => "p-avatar-lg"
                case Size.XLarge => "p-avatar-xl"
            }
            .when(p.initials.isEmpty && p.icon.isEmpty && p.image.isDefined)(_.cssClass("p-avatar-image"))
            .when(p.disabled)(_.cssClass("p-disabled").aria(Aria.disabled -> true))
            .when(control)(_.cssClass("p-uic-clickable"))
            .role(if control then "button" else "img")
            .aria(Aria.label -> name)
            .when(tabStop)(_.tabIndex(0))
            .silent(p.disabled)
            .when(p.onClick)(_.onClick(_))(UI.children(content, p.badge.map(b => span.cssClass("p-uic-avatar-badge")(b)))*)
    end render
end Avatar

object Avatar:
    /** What an avatar holds. Each field is a setter of its name (`uic.Avatar().initials("AL").shape(uic.AvatarShape.Circle)`),
      * which takes a constant or a signal where the field is a `Prop`, the value itself where it is an `Opt`, and `onClick` its action
      * by name; `access` is written by the shared traits (`accessibleName`, `id`).
      */
    final private[uic] case class Props(
        /** The initials (Prime's `label`, `span.p-avatar-text`); they win over `icon` and `image` and name the avatar where no
          * accessible name is set. A signal patches the text and the name in place.
          */
        initials: Opt[Prop[String]] = Opt.unset,
        /** The icon (`.p-avatar-icon`), drawn where no initials are set; it wins over `image`. A signal swaps the glyph in its own
          * region, below the avatar.
          */
        icon: Opt[Prop[IconGlyph]] = Opt.unset,
        /** Image URL rendered as a native `<img>` (`.p-avatar-image` on the root), drawn where neither initials nor an icon are
          * set. A signal patches the image's `src` in place.
          */
        image: Opt[Prop[String]] = Opt.unset,
        /** `Circle` renders `.p-avatar-circle`; `Square` (default) keeps the rounded box. A signal swaps the class in place. */
        shape: Prop[AvatarShape] = AvatarShape.Square,
        /** Size: `Normal` (default), `Large` (`.p-avatar-lg`), `XLarge` (`.p-avatar-xl`); `Small` draws as `Normal` (Prime has no
          * small avatar). A signal swaps the class in place.
          */
        size: Prop[ExtendedSize] = Size.Normal,
        /** Greys the avatar out (`.p-disabled` + `aria-disabled`); a control is then silent and leaves the tab order. A signal
          * patches the class, the attribute, the silence and the tab stop in place.
          */
        disabled: Prop[Boolean] = false,
        /** Overlay badge (e.g. a status dot or counter) rendered as `span.p-uic-avatar-badge`. */
        badge: Opt[UI] = Opt.unset,
        /** What a click (or Enter and Space) runs, a kyo extension: it makes the avatar a control, `role="button"` with the pointer
          * cursor and one tab stop. Its setter takes the action by name, built each time the event comes, and a failure goes to the
          * `onFailure` rules where it ran.
          */
        onClick: Maybe[UI.Action] = Absent,
        @noSetter access: Accessibility = Accessibility.none
    ) derives CanEqual

    def apply(): Avatar = new Avatar(Props())
end Avatar
