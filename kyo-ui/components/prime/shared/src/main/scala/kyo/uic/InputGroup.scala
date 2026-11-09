package kyo.uic

import kyo.*
import kyo.UI.*

/** InputGroup: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputGroup anatomy: `div.p-inputgroup.p-component` whose children are form
  * fields plus `span.p-inputgroupaddon` addons), so the extracted `@primeuix`
  * inputgroup CSS applies verbatim: first/last children get the outer border
  * radius, inner edges fuse, and addons carry the muted surface.
  *
  * This is a CONTAINER, not a wrapper. [[FloatLabel]], [[IftaLabel]] and
  * [[IconField]] each take ONE field and keep its concrete type, because they
  * stamp classes onto it (`.p-filled`, the icon paddings). InputGroup stamps
  * nothing: Prime's CSS keys on child POSITION (`:first-child`, `:last-child`)
  * and on `.p-component`, so the group needs no handle on any child and accepts
  * finished `UI` values in visual order: components, addons and raw kyo elements
  * alike. Build each child fully before placing it here; there is no group-level
  * setter that reaches back into one. [[Label]] is the third mechanism again:
  * standalone, linked to a field by `forId` rather than containing it.
  *
  * Compose with the existing components; the CSS keys on `.p-component`
  * children (Input, Button, Select all qualify):
  * {{{
  * uic.InputGroup()(
  *   uic.InputGroup.addon(uic.Icon(uic.Icons.user)),
  *   uic.Input().placeholder("Username"),
  *   uic.InputGroup.addon(span(".com"))
  * )
  * }}}
  *
  * `id(...)` lands on the wrapping `div`. Because that wrapper is not itself
  * focusable, a `Form`-level error can target it (`form.check(code, focusId =
  * Present(id))`) and the focus-jump lands on the group's FIRST focusable field.
  */
final class InputGroup private[uic] (private[uic] val p: InputGroup.Props) extends Node, HasElementId, HasProps:
    import p.*

    type Self = InputGroup

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new InputGroup(p.copy(access = v))

    /** Appends children (fields and [[InputGroup.addon]]s, in visual order). */
    def apply(cs: UI*): InputGroup = new InputGroup(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        var el = div.cssClass("p-inputgroup").cssClass("p-component")
        el = access.id.map(v => el.id(v)).getOrElse(el)
        el(UI.children(kids)*)
    end render
end InputGroup

object InputGroup:
    final private[uic] case class Props(
        kids: List[UI] = Nil,
        access: Accessibility = Accessibility.none
    )

    def apply(): InputGroup = new InputGroup(Props())

    /** One addon cell (`span.p-inputgroupaddon`): text, an icon, or a Button
      * (the CSS strips the padding around a nested `.p-button`).
      */
    def addon(cs: UI*)(using Frame): UI =
        span.cssClass("p-inputgroupaddon")(UI.children(cs)*)
end InputGroup
