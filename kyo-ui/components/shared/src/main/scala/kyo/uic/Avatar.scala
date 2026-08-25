package kyo.uic

import kyo.*
import kyo.UI.*

/** Avatar shapes (`.p-avatar-circle` vs the default rounded square). */
enum AvatarShape derives CanEqual:
    case Circle, Square

/** Avatar — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Avatar
  * anatomy: `div.p-avatar[.p-avatar-circle][.p-avatar-lg|.p-avatar-xl][.p-avatar-image]`
  * containing initials (`span.p-avatar-text`), an icon, or a native `<img>`), so
  * the extracted `@primeuix` avatar CSS applies verbatim.
  *
  * Content precedence follows Prime: initials win over `icon`, `icon` wins over
  * `image`. kyo extensions beyond Prime's surface: `interactive`/`onClick`
  * (focusable `role="button"`, Enter/Space activation), `disabled`
  * (`.p-disabled` + `aria-disabled`), and the `badge(UI)` overlay rendered as
  * `span.p-uic-avatar-badge` (Prime's OverlayBadge arrives later).
  */
final case class Avatar private (
    initialsV: Maybe[String] = Absent,
    iconV: Maybe[IconGlyph] = Absent,
    imageV: Maybe[String] = Absent,
    shapeV: AvatarShape = AvatarShape.Square,
    sizeV: ExtendedSize = Size.Normal,
    disabledFlag: Boolean = false,
    interactiveFlag: Boolean = false,
    accessibleNameV: Maybe[String] = Absent,
    badgeV: Maybe[UI] = Absent,
    onClickEff: Maybe[Any < Async] = Absent
) extends Node:
    type Self = Avatar

    /** Sets the initials text (Prime's `label`; wins over `icon` and `image`). */
    def initials(v: String): Avatar = copy(initialsV = Present(v))

    /** Alias for [[initials]] — Prime's property name. */
    def label(v: String): Avatar = copy(initialsV = Present(v))

    def icon(glyph: IconGlyph): Avatar = copy(iconV = Present(glyph))

    /** Image URL rendered as a native `<img>` (`.p-avatar-image` on the root). */
    def image(url: String): Avatar = copy(imageV = Present(url))

    /** `Circle` renders `.p-avatar-circle`; `Square` (default) keeps the rounded box. */
    def shape(v: AvatarShape): Avatar = copy(shapeV = v)

    /** Size: `Normal` (default) / `Large` (`.p-avatar-lg`) / `XLarge`
      * (`.p-avatar-xl`); `Small` falls back to `Normal` (Prime has no small avatar).
      */
    def size(v: ExtendedSize): Avatar = copy(sizeV = v)

    /** Greys the avatar out (`.p-disabled` + `aria-disabled`) and suppresses interaction. */
    def disabled(v: Boolean): Avatar = copy(disabledFlag = v)

    /** Makes the avatar a focusable button activated by click/Enter/Space (kyo extension). */
    def interactive(v: Boolean): Avatar = copy(interactiveFlag = v)

    /** Accessible name announced for the avatar (overrides the initials-derived default). */
    def accessibleName(v: String): Avatar = copy(accessibleNameV = Present(v))

    /** Overlay badge (e.g. a status dot or counter) rendered as `span.p-uic-avatar-badge`. */
    def badge(v: UI): Avatar = copy(badgeV = Present(v))

    /** Runs `action` when the avatar is activated. Implies [[interactive]]. */
    def onClick(action: => Any < Async)(using Frame): Avatar =
        copy(onClickEff = Present(Sync.defer(action)), interactiveFlag = true)

    private[uic] def render(using Frame): UI =
        val accName = accessibleNameV.getOrElse(initialsV.getOrElse("Avatar"))

        var el = div.cssClass("p-avatar").cssClass("p-component")
        if shapeV == AvatarShape.Circle then el = el.cssClass("p-avatar-circle")
        sizeV match
            case Size.Large               => el = el.cssClass("p-avatar-lg")
            case Size.XLarge              => el = el.cssClass("p-avatar-xl")
            case Size.Small | Size.Normal => ()
        end match
        val showsImage = initialsV.isEmpty && iconV.isEmpty && imageV.isDefined
        if showsImage then el = el.cssClass("p-avatar-image")
        if disabledFlag then el = el.cssClass("p-disabled").aria("disabled", "true")

        // Prime content precedence: label > icon > image.
        val content: UI =
            (initialsV, iconV, imageV) match
                case (Present(t), _, _)           => span.cssClass("p-avatar-text")(t)
                case (Absent, Present(g), _)      => GlyphSvg(g, "p-avatar-icon")
                case (Absent, Absent, Present(u)) => img(ImgSrc.Path(u), accName)
                case _                            => span.cssClass("p-avatar-text")("")
        val badgeChild: List[UI] = badgeV.toList.map(b => span.cssClass("p-uic-avatar-badge")(toChild(b)))

        val withA11y =
            if interactiveFlag then el.cssClass("p-uic-clickable").role("button").aria("label", accName)
            else el.role("img").aria("label", accName)
        // Enter/Space keyboard activation mirrors Button's native behaviour.
        val withEvt =
            if interactiveFlag && !disabledFlag then
                val activated = withA11y.tabIndex(0)
                onClickEff match
                    case Present(e) =>
                        activated
                            .onClick(e)
                            .onKeyDown { evt =>
                                evt.key match
                                    case Keyboard.Enter | Keyboard.Space => e
                                    case _                               => ()
                            }
                    case Absent => activated
                end match
            else withA11y
        withEvt((content :: badgeChild).map(toChild)*)
    end render
end Avatar

object Avatar:
    def apply(): Avatar = new Avatar()
