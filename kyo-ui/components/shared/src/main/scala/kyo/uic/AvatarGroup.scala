package kyo.uic

import kyo.*
import kyo.UI.*

/** AvatarGroup — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * AvatarGroup anatomy: `div.p-avatar-group.p-component` wrapping [[Avatar]]
  * children). The `.p-avatar-group` rules live in the extracted avatar sheet
  * (Prime ships them there): adjacent avatars overlap by the group offset and
  * gain the group border, per size.
  *
  * Construct from avatars (`AvatarGroup(a, b, c)`) or push arbitrary children
  * through `apply(cs*)` — e.g. a trailing `+N` counter Avatar.
  */
final case class AvatarGroup private (
    avatarsV: List[Avatar] = Nil,
    kids: List[UI] = Nil
) extends Node:
    type Self = AvatarGroup

    /** Appends one avatar to the stack. */
    def avatar(a: Avatar): AvatarGroup = copy(avatarsV = avatarsV :+ a)

    /** Adds arbitrary children after the avatars. */
    def apply(cs: UI*): AvatarGroup = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        div.cssClass("p-avatar-group").cssClass("p-component")(
            (avatarsV.map(_.render) ++ kids).map(toChild)*
        )
end AvatarGroup

object AvatarGroup:
    def apply(): AvatarGroup = new AvatarGroup()

    /** A group of the given avatars, stacked with the Prime overlap. */
    def apply(avatars: Avatar*): AvatarGroup = new AvatarGroup(avatarsV = avatars.toList)
end AvatarGroup
