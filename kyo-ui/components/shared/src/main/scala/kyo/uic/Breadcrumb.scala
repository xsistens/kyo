package kyo.uic

import kyo.*
import kyo.UI.*

/** A single entry in a [[Breadcrumb]] trail — a `text` label plus an optional
  * `href` and an optional leading `icon`. A missing href renders the crumb as
  * plain text rather than a link, which the last crumb (the current page)
  * conventionally is, and which an intermediate step with no page behind it may
  * also be. `target` maps onto the anchor's `target` attribute when it is one of
  * the standard `_self` / `_blank` / `_parent` / `_top` values.
  */
final case class BreadcrumbItem private[uic] (
    text: TextValue,
    href: Maybe[String],
    target: Maybe[String],
    icon: Maybe[IconGlyph]
)

object BreadcrumbItem:
    /** Construct a crumb. A `Signal[String]` label re-renders in place on emission; the rest of
      * the crumb (href, target, icon) is static either way.
      */
    def apply(
        text: String | Signal[String],
        href: Maybe[String] = Absent,
        target: Maybe[String] = Absent,
        icon: Maybe[IconGlyph] = Absent
    ): BreadcrumbItem = new BreadcrumbItem(ReactiveValue(text), href, target, icon)
end BreadcrumbItem

/** Breadcrumb — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Breadcrumb anatomy: `nav.p-breadcrumb.p-component` > `ol.p-breadcrumb-list`
  * with interleaved `li.p-breadcrumb-item` entries — each an
  * `a.p-breadcrumb-item-link` wrapping an optional `.p-breadcrumb-item-icon`
  * glyph plus the `span.p-breadcrumb-item-label` — and `li.p-breadcrumb-separator`
  * chevron glyphs between them), so the extracted `@primeuix` breadcrumb CSS
  * applies verbatim.
  *
  * `home(icon, href)` prepends Prime's home item (`li.p-breadcrumb-home-item`,
  * icon-only). `onItemClick` fires with the item's href (or text when hrefless)
  * in addition to — not instead of — native navigation.
  */
final case class Breadcrumb private (
    homeV: Maybe[BreadcrumbItem] = Absent,
    itemsV: List[BreadcrumbItem] = Nil,
    separatorV: Maybe[IconGlyph] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent,
    onItemClickF: Maybe[String => Any < Async] = Absent
) extends Node, HasAccessibleName:
    type Self = Breadcrumb

    /** Appends the given crumbs to the trail. */
    def items(is: BreadcrumbItem*): Breadcrumb = copy(itemsV = itemsV ++ is.toList)

    /** Appends a linked crumb. A `Signal[String]` label re-renders in place on emission. */
    def item(text: String | Signal[String], href: String): Breadcrumb =
        copy(itemsV = itemsV :+ BreadcrumbItem(text, Present(href)))

    /** Appends a crumb with nothing to open (the current page, or a step in the trail that has
      * no page of its own). A `Signal[String]` label re-renders in place on emission. Only the
      * last crumb of the trail carries `aria-current="page"`.
      */
    def item(text: String | Signal[String]): Breadcrumb = copy(itemsV = itemsV :+ BreadcrumbItem(text))

    /** Prime's home item: an icon-only crumb rendered first (`li.p-breadcrumb-home-item`). */
    def home(icon: IconGlyph, href: String): Breadcrumb =
        copy(homeV = Present(BreadcrumbItem("", Present(href), icon = Present(icon))))

    /** Overrides the separator glyph (default the Prime chevron, [[Icons.chevronRight]]). */
    def separator(v: IconGlyph): Breadcrumb = copy(separatorV = Present(v))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Breadcrumb = copy(accessibleNameV = v)

    /** Fired with the item's href (or its text when hrefless) on click, in addition
      * to the anchor's native navigation.
      */
    def onItemClick(f: String => Any < Async): Breadcrumb = copy(onItemClickF = Present(f))

    private[uic] def render(using Frame): UI =
        val sepGlyph = separatorV.getOrElse(Icons.chevronRight)
        val allItems: List[(BreadcrumbItem, Boolean)] =
            homeV.toList.map(h => (h, true)) ++ itemsV.map(i => (i, false))
        val lastIdx = allItems.length - 1
        val entries: List[UI] = allItems.zipWithIndex.flatMap { case ((it, isHome), i) =>
            val itemCls = if isHome then "p-breadcrumb-home-item" else "p-breadcrumb-item"
            val row: UI = li.cssClass(itemCls)(toChild(renderLink(it, i == lastIdx)))
            val sep: List[UI] =
                if i < lastIdx then
                    List(
                        li.cssClass("p-breadcrumb-separator").aria("hidden", "true")(
                            toChild(GlyphSvg(sepGlyph, "p-breadcrumb-separator-icon"))
                        )
                    )
                else Nil
            row :: sep
        }
        val navBase = UI.nav.cssClass("p-breadcrumb").cssClass("p-component")
        val labelled = accessibleNameV match
            case Present(TextValue.Const(v)) => navBase.aria("label", v)
            case Present(TextValue.Dyn(s))   => navBase.aria("label", s)
            case Absent                      => navBase.aria("label", "Breadcrumb")
        labelled(
            UI.ol.cssClass("p-breadcrumb-list")(entries.map(toChild)*)
        )
    end render

    /** One crumb's `a.p-breadcrumb-item-link` (icon + label); hrefless crumbs
      * render the same anchor without an href, and the LAST crumb is the one
      * marked `aria-current="page"`.
      *
      * Last rather than every hrefless crumb: a trail can pass through something
      * that is not a page of its own, a section heading with no index behind it,
      * and such a crumb is neither a link nor where the reader is. Marking it
      * current told a screen reader the trail ends in two places at once.
      */
    private def renderLink(it: BreadcrumbItem, isCurrent: Boolean)(using Frame): UI =
        val payload = it.href.getOrElse(it.text.constOrEmpty)
        var anchor  = a.cssClass("p-breadcrumb-item-link")
        it.href match
            case Present(h) =>
                anchor = anchor.href(Href.Path(h))
                Breadcrumb.parseTarget(it.target).foreach(t => anchor = anchor.target(t))
            case Absent =>
                if isCurrent then anchor = anchor.aria("current", "page")
        end match
        val iconSlot: List[UI] = it.icon.toList.map(g => GlyphSvg(g, "p-breadcrumb-item-icon"))
        val labelSlot: List[UI] = it.text match
            case TextValue.Dyn(s)   => List(span.cssClass("p-breadcrumb-item-label")(s))
            case TextValue.Const(t) => if t.isEmpty then Nil else List(span.cssClass("p-breadcrumb-item-label")(t))
        val linked = anchor((iconSlot ++ labelSlot).map(toChild)*)
        // The click handler sits on a display-contents wrapper so kyo does not
        // suppress the anchor's native navigation; the handler fires in addition.
        onItemClickF match
            case Present(f) => span.cssClass("p-uic-link-wrapper").onClick(f(payload))(toChild(linked))
            case Absent     => linked
    end renderLink
end Breadcrumb

object Breadcrumb:
    def apply(): Breadcrumb = new Breadcrumb()

    /** Maps a `target` string onto kyo's typed [[Target]] (standard values only). */
    private def parseTarget(t: Maybe[String]): Maybe[Target] =
        t match
            case Present("_self")   => Present(Target.Self)
            case Present("_blank")  => Present(Target.Blank)
            case Present("_parent") => Present(Target.Parent)
            case Present("_top")    => Present(Target.Top)
            case _                  => Absent
end Breadcrumb
