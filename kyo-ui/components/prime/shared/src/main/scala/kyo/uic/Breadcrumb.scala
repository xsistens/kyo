package kyo.uic

import kyo.*
import kyo.UI.*

/** A single entry in a [[Breadcrumb]] trail: a `text` label plus an optional
  * `href` and an optional leading `icon`. A missing href renders the crumb as
  * plain text rather than a link, which the last crumb (the current page)
  * conventionally is, and which an intermediate step with no page behind it may
  * also be. `target` maps onto the anchor's `target` attribute when it is one of
  * the standard `_self` / `_blank` / `_parent` / `_top` values.
  *
  * [[Breadcrumb.onItemClick]] hands the crumb over, and [[href]] is what a handler reads from it.
  */
final class BreadcrumbItem private[uic] (private[uic] val p: BreadcrumbItem.Props) extends HasProps derives CanEqual:
    /** Where this crumb links to, `Absent` for one that is plain text. */
    def href: Maybe[String] = p.href
end BreadcrumbItem

object BreadcrumbItem:
    final private[uic] case class Props(
        text: TextValue,
        href: Maybe[String],
        target: Maybe[String],
        icon: Maybe[IconGlyph]
    )

    /** Construct a crumb. A `Signal[String]` label re-renders in place on emission; the rest of
      * the crumb (href, target, icon) is static either way.
      */
    def apply(
        text: String | Signal[String],
        href: Maybe[String] = Absent,
        target: Maybe[String] = Absent,
        icon: Maybe[IconGlyph] = Absent
    ): BreadcrumbItem = new BreadcrumbItem(Props(ReactiveValue(text), href, target, icon))
end BreadcrumbItem

/** A crumb, built directly or as `_.item(...)` (see [[BreadcrumbItemScope]]). */
type BreadcrumbItemEntry = Entry[BreadcrumbItem, BreadcrumbItemScope]

/** A [[BreadcrumbItemEntry]] or a signal of crumbs, for a trail that changes while it is shown (see [[LiveEntry]]). */
type BreadcrumbItemLiveEntry = LiveEntry[BreadcrumbItem, BreadcrumbItemScope]

/** What `_` stands for in [[Breadcrumb.items]]: `_.item("Docs", href = Present("/docs"))`, as [[BreadcrumbItem.apply]]. */
final class BreadcrumbItemScope private ():
    /** A crumb, as [[BreadcrumbItem.apply]]. */
    def item(
        text: String | Signal[String],
        href: Maybe[String] = Absent,
        target: Maybe[String] = Absent,
        icon: Maybe[IconGlyph] = Absent
    ): BreadcrumbItem = BreadcrumbItem(text, href, target, icon)
end BreadcrumbItemScope

object BreadcrumbItemScope:
    private val scope = new BreadcrumbItemScope

    private[uic] def resolve(entries: Seq[BreadcrumbItemEntry]): List[BreadcrumbItem] =
        Entry.resolve(scope, entries)

    private[uic] def reading(entries: Seq[BreadcrumbItemLiveEntry])(using Frame): ReactiveValue[List[BreadcrumbItem]] =
        Entry.reading(scope, entries)
end BreadcrumbItemScope

/** Breadcrumb: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Breadcrumb anatomy: `nav.p-breadcrumb.p-component` > `ol.p-breadcrumb-list`
  * with interleaved `li.p-breadcrumb-item` entries (each an
  * `a.p-breadcrumb-item-link` wrapping an optional `.p-breadcrumb-item-icon`
  * glyph plus the `span.p-breadcrumb-item-label`) and `li.p-breadcrumb-separator`
  * chevron glyphs between them), so the extracted `@primeuix` breadcrumb CSS
  * applies verbatim.
  *
  * `home(icon, href)` prepends Prime's home item (`li.p-breadcrumb-home-item`,
  * icon-only). `onItemClick` fires with the item's href (or text when hrefless)
  * in addition to, not instead of, native navigation.
  */
final class Breadcrumb private[uic] (private[uic] val p: Breadcrumb.Props) extends Node, HasAccessibleName, HasProps:
    import p.*

    type Self = Breadcrumb

    /** Appends the given crumbs to the trail, each built directly or from `_` (see [[BreadcrumbItemScope]]), or a signal of
      * crumbs that change while the trail is shown (see [[LiveEntry]]).
      */
    def items(is: BreadcrumbItemLiveEntry*): Breadcrumb = new Breadcrumb(p.copy(itemSources = itemSources ++ is))

    /** Appends a linked crumb. A `Signal[String]` label re-renders in place on emission. */
    def item(text: String | Signal[String], href: String): Breadcrumb =
        new Breadcrumb(p.copy(itemSources = itemSources :+ BreadcrumbItem(text, Present(href))))

    /** Appends a crumb with nothing to open (the current page, or a step in the trail that has
      * no page of its own). A `Signal[String]` label re-renders in place on emission. Only the
      * last crumb of the trail carries `aria-current="page"`.
      */
    def item(text: String | Signal[String]): Breadcrumb = new Breadcrumb(p.copy(itemSources = itemSources :+ BreadcrumbItem(text)))

    /** Prime's home item: an icon-only crumb rendered first (`li.p-breadcrumb-home-item`). */
    def home(icon: IconGlyph, href: String): Breadcrumb =
        new Breadcrumb(p.copy(homeV = Present(BreadcrumbItem("", Present(href), icon = Present(icon)))))

    /** Overrides the separator glyph (default the Prime chevron, [[Icons.chevronRight]]). */
    def separator(v: IconGlyph): Breadcrumb = new Breadcrumb(p.copy(separatorV = Present(v)))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Breadcrumb(p.copy(access = v))

    /** Fired with the item clicked, in addition to the anchor's native navigation. */
    def onItemClick(f: BreadcrumbItem => UI.Action)(using Frame): Breadcrumb =
        new Breadcrumb(p.copy(onItemClickF = Present(Callback.of(f))))

    /** The crumbs as ONE reading: fixed unless one of the setters was handed a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[BreadcrumbItem]] = BreadcrumbItemScope.reading(itemSources)

    private[uic] def render(using Frame): UI =
        val navBase  = UI.nav.cssClass("p-breadcrumb").cssClass("p-component")
        val labelled = access.name match
            case Present(v) => navBase.aria(AriaKey.label -> v)
            case _          => navBase.aria(AriaKey.label -> "Breadcrumb")
        // Only the list follows a live trail: which crumb is last, and so current, moves with it.
        labelled(entries.render(items => UI.ol.cssClass("p-breadcrumb-list")(UI.children(trail(items))*)))
    end render

    /** The rows of the list: the home item and the crumbs, separated by the chevron. */
    private def trail(items: List[BreadcrumbItem])(using Frame): List[UI] =
        val sepGlyph                                  = separatorV.getOrElse(Icons.chevronRight)
        val allItems: List[(BreadcrumbItem, Boolean)] =
            homeV.toList.map(h => (h, true)) ++ items.map(i => (i, false))
        val lastIdx = allItems.length - 1
        allItems.zipWithIndex.flatMap { case ((it, isHome), i) =>
            val itemCls       = if isHome then "p-breadcrumb-home-item" else "p-breadcrumb-item"
            val row: UI       = li.cssClass(itemCls)(renderLink(it, i == lastIdx))
            val sep: List[UI] =
                if i < lastIdx then
                    List(
                        li.cssClass("p-breadcrumb-separator").aria(AriaKey.hidden -> "true")(
                            GlyphSvg(sepGlyph, "p-breadcrumb-separator-icon")
                        )
                    )
                else Nil
            row :: sep
        }
    end trail

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
        var anchor = a.cssClass("p-breadcrumb-item-link")
        it.href match
            case Present(h) =>
                anchor = anchor.href(Href.Path(h))
                Breadcrumb.parseTarget(it.p.target).foreach(t => anchor = anchor.target(t))
            case Absent =>
                if isCurrent then anchor = anchor.aria(AriaKey.current -> "page")
        end match
        val iconSlot: List[UI]  = it.p.icon.toList.map(g => GlyphSvg(g, "p-breadcrumb-item-icon"))
        val labelSlot: List[UI] = it.p.text match
            case TextValue.Dyn(s)   => List(span.cssClass("p-breadcrumb-item-label")(s))
            case TextValue.Const(t) => if t.isEmpty then Nil else List(span.cssClass("p-breadcrumb-item-label")(t))
        val linked = anchor(UI.children(iconSlot ++ labelSlot)*)
        // The click handler sits on a display-contents wrapper so kyo does not
        // suppress the anchor's native navigation; the handler fires in addition.
        onItemClickF match
            case Present(f) => span.cssClass("p-uic-link-wrapper").onClick(f(it))(linked)
            case Absent     => linked
    end renderLink
end Breadcrumb

object Breadcrumb:
    final private[uic] case class Props(
        homeV: Maybe[BreadcrumbItem] = Absent,
        itemSources: List[BreadcrumbItemLiveEntry] = Nil,
        separatorV: Maybe[IconGlyph] = Absent,
        access: Accessibility = Accessibility.none,
        onItemClickF: Maybe[BreadcrumbItem => UI.Action] = Absent
    )

    def apply(): Breadcrumb = new Breadcrumb(Props())

    /** Maps a `target` string onto kyo's typed [[Target]] (standard values only). */
    private def parseTarget(t: Maybe[String]): Maybe[Target] =
        t match
            case Present("_self")   => Present(Target.Self)
            case Present("_blank")  => Present(Target.Blank)
            case Present("_parent") => Present(Target.Parent)
            case Present("_top")    => Present(Target.Top)
            case _                  => Absent
end Breadcrumb
