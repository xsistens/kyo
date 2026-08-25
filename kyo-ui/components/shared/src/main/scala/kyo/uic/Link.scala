package kyo.uic

import kyo.*
import kyo.UI.*

/** Link accessible roles (`role` attribute override). */
enum LinkAccessibleRole derives CanEqual:
    case Link, Button

    private[uic] def token: String = this.toString
end LinkAccessibleRole

/** Link — native kyo-ui, PrimeOne design. Prime ships no Link component, so the
  * vocabulary is the kyo extension `a.p-uic-link`, styled like Prime Button's
  * Link variant (primary color, underline on hover). Composes the native kyo
  * `<a>` element so the real `href`, focus, and click behaviour are inherited.
  * A disabled link drops its `href` and click handler, leaves the tab order,
  * and exposes `aria-disabled` plus the `.p-disabled` skin (there is no native
  * `disabled` attribute on an anchor).
  */
final case class Link private (
    hrefV: Maybe[String] = Absent,
    disabledFlag: Boolean = false,
    wrapFlag: Boolean = false,
    iconV: Maybe[IconGlyph] = Absent,
    endIconV: Maybe[IconGlyph] = Absent,
    targetV: Maybe[String] = Absent,
    tooltipV: Maybe[TextValue] = Absent,
    accessibleRoleV: LinkAccessibleRole = LinkAccessibleRole.Link,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    accessibleDescriptionV: Maybe[TextValue] = Absent,
    onClickEff: Maybe[Any < Async] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Link

    def href(v: String): Link      = copy(hrefV = Present(v))
    def disabled(v: Boolean): Link = copy(disabledFlag = v)

    /** Whether the link text wraps onto multiple lines (default false — stays on
      * one line, `.p-uic-link-wrap`).
      */
    def wrap(v: Boolean): Link = copy(wrapFlag = v)

    /** Icon rendered before the link text. */
    def icon(glyph: IconGlyph): Link = copy(iconV = Present(glyph))

    /** Icon rendered after the link text. */
    def endIcon(glyph: IconGlyph): Link = copy(endIconV = Present(glyph))

    /** Anchor `target` (e.g. `_blank`, `_self`). `_blank` automatically also gets
      * `rel="noopener"` so the opened page cannot script this one.
      */
    def target(v: String): Link = copy(targetV = Present(v))

    /** Native tooltip (the `title` attribute). */
    def tooltip(v: String): Link = copy(tooltipV = Present(TextValue.Const(v)))

    /** Reactive tooltip — native `title` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def tooltip(sig: Signal[String]): Link = copy(tooltipV = Present(TextValue.Dyn(sig)))

    /** ARIA role override — `Button` renders `role="button"` on the anchor. */
    def accessibleRole(v: LinkAccessibleRole): Link = copy(accessibleRoleV = v)

    /** Accessible name announced instead of the visible text (`aria-label`). */
    def accessibleName(v: String): Link = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Link = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** ID reference(s) of the element(s) that label the link (`aria-labelledby`). */
    def accessibleNameRef(v: String): Link = copy(accessibleNameRefV = Present(v))

    /** Additional accessible description (`aria-description`). */
    def accessibleDescription(v: String): Link = copy(accessibleDescriptionV = Present(TextValue.Const(v)))

    /** Reactive accessible description — `aria-description` patched IN PLACE via kyo-ui's
      * attribute channel (`setAttribute`, no re-render).
      */
    def accessibleDescription(sig: Signal[String]): Link = copy(accessibleDescriptionV = Present(TextValue.Dyn(sig)))

    /** Runs `action` when the link is activated. Ignored while disabled. */
    def onClick(action: => Any < Async)(using Frame): Link =
        copy(onClickEff = Present(Sync.defer(action)))

    /** Adds default-slot children (typically the link text). */
    def apply(cs: UI*): Link = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        var el = a.cssClass("p-uic-link")
        if wrapFlag then el = el.cssClass("p-uic-link-wrap")
        accessibleNameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accessibleNameRefV.foreach(r => el = el.aria("labelledby", r))
        accessibleDescriptionV match
            case Present(TextValue.Const(d)) => el = el.aria("description", d)
            case Present(TextValue.Dyn(s))   => el = el.aria("description", s)
            case Absent                      => ()
        end match
        tooltipV match
            case Present(TextValue.Const(t)) => el = el.jsProp("title", t)
            case Present(TextValue.Dyn(s))   => el = el.title(s)
            case Absent                      => ()
        end match
        if accessibleRoleV == LinkAccessibleRole.Button then el = el.role("button")
        if disabledFlag then
            val disabledRole = if accessibleRoleV == LinkAccessibleRole.Button then "button" else "link"
            el = el.cssClass("p-disabled").role(disabledRole).aria("disabled", "true").tabIndex(-1)
        else
            hrefV.foreach(h => el = el.href(Href.Path(h)))
            targetV.foreach(t => el = applyTarget(el, t))
            onClickEff.foreach(e => el = el.onClick(e))
        end if
        el(contentChildren.map(toChild)*)
    end render

    /** Maps the string `target` onto kyo's typed `Target`; frame names (not
      * expressible in the kyo enum) go through the DOM `target` property instead.
      * `_blank` additionally sets `rel="noopener"`.
      */
    private def applyTarget(el: Ast.Anchor, t: String): Ast.Anchor =
        val withTgt = t match
            case "_self"   => el.target(Target.Self)
            case "_blank"  => el.target(Target.Blank)
            case "_parent" => el.target(Target.Parent)
            case "_top"    => el.target(Target.Top)
            case other     => el.jsProp("target", other)
        if t == "_blank" then withTgt.jsProp("rel", "noopener") else withTgt
    end applyTarget

    private def contentChildren(using Frame): List[UI] =
        val iconChild: List[UI]    = iconV.toList.map(g => GlyphSvg(g, "p-uic-link-icon"))
        val endIconChild: List[UI] = endIconV.toList.map(g => GlyphSvg(g, "p-uic-link-icon"))
        iconChild ++ kids ++ endIconChild
    end contentChildren
end Link

object Link:
    def apply(): Link = new Link()

    /** A link labelled `text` (placed in the default slot). */
    def apply(text: String)(using Frame): Link = new Link(kids = List(stringToUI(text)))
end Link
