package kyo.uic

import kyo.*
import kyo.UI.*

/** Link accessible roles (`role` attribute override). */
enum LinkAccessibleRole derives CanEqual:
    case Link, Button

    private[uic] def token: String = this.toString
end LinkAccessibleRole

/** Link: native kyo-ui, PrimeOne design. Prime ships no Link component, so the
  * vocabulary is the kyo extension `a.p-uic-link`, styled like Prime Button's
  * Link variant (primary color, underline on hover). Composes the native kyo
  * `<a>` element so the real `href`, focus, and click behaviour are inherited.
  * A disabled link drops its `href` and click handler, leaves the tab order,
  * and exposes `aria-disabled` plus the `.p-disabled` skin (there is no native
  * `disabled` attribute on an anchor).
  */
final class Link private[uic] (private[uic] val p: Link.Props) extends Node, HasTooltip, HasAccessibleNameRef, HasAccessibleDescription,
      PopupTrigger, HasProps:
    import p.*

    type Self = Link

    def href(v: String): Link      = new Link(p.copy(hrefV = Present(v)))
    def disabled(v: Boolean): Link = new Link(p.copy(disabledFlag = v))

    /** Whether the link text wraps onto multiple lines (default false: stays on
      * one line, `.p-uic-link-wrap`).
      */
    def wrap(v: Boolean): Link = new Link(p.copy(wrapFlag = v))

    /** Icon rendered before the link text. */
    def icon(glyph: IconGlyph): Link = new Link(p.copy(iconV = Present(glyph)))

    /** Icon rendered after the link text. */
    def endIcon(glyph: IconGlyph): Link = new Link(p.copy(endIconV = Present(glyph)))

    /** Anchor `target` (e.g. `_blank`, `_self`). `_blank` automatically also gets
      * `rel="noopener"` so the opened page cannot script this one.
      */
    def target(v: String): Link = new Link(p.copy(targetV = Present(v)))

    private[uic] def withTooltip(v: Maybe[Prop[String]]): Link = new Link(p.copy(tooltipV = v))

    /** ARIA role override: `Button` renders `role="button"` on the anchor. */
    def accessibleRole(v: LinkAccessibleRole): Link = new Link(p.copy(accessibleRoleV = v))

    private[uic] def accessibility: Accessibility                            = access
    private[uic] def withAccessibility(v: Accessibility): Self               = new Link(p.copy(access = v))
    private[uic] def withAccessibleDescription(v: Maybe[Prop[String]]): Link = new Link(p.copy(accessibleDescriptionV = v))

    /** Runs `action` when the link is activated. Ignored while disabled. */
    def onClick(action: => UI.Action)(using Frame): Link =
        new Link(p.copy(onClickEff = Present(Callback(action))))

    /** Adds default-slot children (typically the link text). */
    def apply(cs: UI*): Link = new Link(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI = anchor

    // The link's one element is its control. One with no `href` is no native control, so the host's keys go on it.
    private[uic] def asPopupTrigger(
        popup: String,
        expanded: String | Signal[String],
        fixed: Boolean,
        activate: Maybe[UI.Action]
    )(using Frame): UI =
        PopupTrigger.element(anchor, popup, expanded, fixed, activate)

    private def anchor(using Frame): Ast.Anchor =
        var el = a.cssClass("p-uic-link")
        if wrapFlag then el = el.cssClass("p-uic-link-wrap")
        access.name.foreach(n => el = el.aria("label", n))
        access.nameRef.foreach(r => el = el.aria("labelledby", r))
        el = el.aria("description", accessibleDescriptionV).title(tooltipV)
        if accessibleRoleV == LinkAccessibleRole.Button then el = el.role("button")
        if disabledFlag then
            val disabledRole = if accessibleRoleV == LinkAccessibleRole.Button then "button" else "link"
            el = el.cssClass("p-disabled").role(disabledRole).aria("disabled", "true").tabIndex(-1)
        else
            hrefV.foreach(h => el = el.href(Href.Path(h)))
            targetV.foreach(t => el = applyTarget(el, t))
            onClickEff.foreach(e => el = el.onClick(e))
        end if
        el(UI.children(contentChildren)*)
    end anchor

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
    final private[uic] case class Props(
        hrefV: Maybe[String] = Absent,
        disabledFlag: Boolean = false,
        wrapFlag: Boolean = false,
        iconV: Maybe[IconGlyph] = Absent,
        endIconV: Maybe[IconGlyph] = Absent,
        targetV: Maybe[String] = Absent,
        tooltipV: Maybe[Prop[String]] = Absent,
        accessibleRoleV: LinkAccessibleRole = LinkAccessibleRole.Link,
        access: Accessibility = Accessibility.none,
        accessibleDescriptionV: Maybe[Prop[String]] = Absent,
        onClickEff: Maybe[UI.Action] = Absent,
        kids: List[UI] = Nil
    )

    def apply(): Link = new Link(Props())

    /** A link labelled `text` (placed in the default slot). */
    def apply(text: String)(using Frame): Link = new Link(Props(kids = List(stringToUI(text))))
end Link
