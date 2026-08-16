package kyo.uic

import kyo.*
import kyo.UI.*

/** InputGroup — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * InputGroup anatomy: `div.p-inputgroup.p-component` whose children are form
  * fields plus `span.p-inputgroupaddon` addons), so the extracted `@primeuix`
  * inputgroup CSS applies verbatim: first/last children get the outer border
  * radius, inner edges fuse, and addons carry the muted surface.
  *
  * Compose with the existing components — the CSS keys on `.p-component`
  * children (Input, Button, Select all qualify):
  * {{{
  * uic.InputGroup()(
  *   uic.InputGroup.addon(uic.Icon(uic.Icons.user)),
  *   uic.Input().placeholder("Username"),
  *   uic.InputGroup.addon(span(".com"))
  * )
  * }}}
  */
final case class InputGroup private (
    kids: List[UI] = Nil,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = InputGroup

    /** Native `id` on the wrapping `div`. Because the wrapper is not itself focusable, a
      * `Form`-level error can target this id (`form.check(code, focusId = Present(id))`) and
      * the focus-jump lands on the group's FIRST focusable field.
      */
    def id(v: String): InputGroup = copy(idV = Present(v))

    /** Appends children (fields and [[InputGroup.addon]]s, in visual order). */
    def apply(cs: UI*): InputGroup = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var el = div.cssClass("p-inputgroup").cssClass("p-component")
        el = idV.map(v => el.id(v)).getOrElse(el)
        el(kids.map(toChild)*)
    end render
end InputGroup

object InputGroup:
    def apply(): InputGroup = new InputGroup()

    /** One addon cell (`span.p-inputgroupaddon`) — text, an icon, or a Button
      * (the CSS strips the padding around a nested `.p-button`).
      */
    def addon(cs: UI*)(using Frame): UI =
        span.cssClass("p-inputgroupaddon")(cs.map(toChild)*)
end InputGroup
