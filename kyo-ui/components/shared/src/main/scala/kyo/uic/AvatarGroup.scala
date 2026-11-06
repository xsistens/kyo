package kyo.uic

import kyo.*
import kyo.UI.*

/** AvatarGroup: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * AvatarGroup anatomy: `div.p-avatar-group.p-component` wrapping [[Avatar]]
  * children). The `.p-avatar-group` rules live in the extracted avatar sheet
  * (Prime ships them there): adjacent avatars overlap by the group offset and
  * gain the group border, per size.
  *
  * Construct from avatars (`AvatarGroup(a, b, c)`) or push arbitrary children
  * through `apply(cs*)`, e.g. a trailing `+N` counter Avatar.
  */
final class AvatarGroup private[uic] (private[uic] val p: AvatarGroup.Props) extends Node, HasProps:
    import p.*

    type Self = AvatarGroup

    /** Appends one avatar to the stack. */
    def avatar(a: Avatar): AvatarGroup = new AvatarGroup(p.copy(avatarsV = avatarsV :+ a))

    /** Adds arbitrary children after the avatars. */
    def apply(cs: UI*): AvatarGroup = new AvatarGroup(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        div.cssClass("p-avatar-group").cssClass("p-component")(
            UI.children(avatarsV.map(_.render) ++ kids)*
        )
end AvatarGroup

object AvatarGroup:
    final private[uic] case class Props(
        avatarsV: List[Avatar] = Nil,
        kids: List[UI] = Nil
    )

    def apply(): AvatarGroup = new AvatarGroup(Props())

    /** A group of the given avatars, stacked with the Prime overlap. */
    def apply(avatars: Avatar*): AvatarGroup = new AvatarGroup(Props(avatarsV = avatars.toList))
end AvatarGroup
