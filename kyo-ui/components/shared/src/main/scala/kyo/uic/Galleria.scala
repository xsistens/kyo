package kyo.uic

import kyo.*
import kyo.UI.*

/** One image of a [[Galleria]], a hand-authored carrier: the main `src`, its
  * `alt` text, an optional `thumbnailSrc` (falls back to `src`), and the
  * optional caption `title`/`subtitle` pair.
  *
  * The caption also accepts reactive text: set `titleDyn`/`subtitleDyn` to a
  * `Signal[String]` and the caption re-renders that line in place on emission
  * (a locale-driven `I18n.t` leaf, say). A `*Dyn` slot takes precedence over its
  * constant counterpart; leaving both `Dyn` slots `Absent` renders exactly the
  * constant caption.
  */
final case class GalleriaItem(
    src: String,
    alt: String = "",
    thumbnailSrc: Maybe[String] = Absent,
    title: Maybe[String] = Absent,
    subtitle: Maybe[String] = Absent,
    titleDyn: Maybe[Signal[String]] = Absent,
    subtitleDyn: Maybe[Signal[String]] = Absent
) derives CanEqual

/** A Galleria image, built directly or as `_.item(...)` (see [[GalleriaItemScope]]). */
type GalleriaItemEntry = Entry[GalleriaItem, GalleriaItemScope]

/** A [[GalleriaItemEntry]] or a signal of images, for a gallery whose images change while it is shown (see [[LiveEntry]]). */
type GalleriaItemLiveEntry = LiveEntry[GalleriaItem, GalleriaItemScope]

/** What `_` stands for in [[Galleria.items]]: `_.item("/img/1.jpg", alt = "Harbour")`, as [[GalleriaItem.apply]]. */
final class GalleriaItemScope private ():
    /** An image, as [[GalleriaItem.apply]]. */
    def item(
        src: String,
        alt: String = "",
        thumbnailSrc: Maybe[String] = Absent,
        title: Maybe[String] = Absent,
        subtitle: Maybe[String] = Absent,
        titleDyn: Maybe[Signal[String]] = Absent,
        subtitleDyn: Maybe[Signal[String]] = Absent
    ): GalleriaItem = GalleriaItem(src, alt, thumbnailSrc, title, subtitle, titleDyn, subtitleDyn)
end GalleriaItemScope

object GalleriaItemScope:
    private val scope = new GalleriaItemScope

    private[uic] def resolve(entries: Seq[GalleriaItemEntry]): List[GalleriaItem] = Entry.resolve(scope, entries)

    private[uic] def reading(entries: Seq[GalleriaItemLiveEntry])(using Frame): ReactiveValue[List[GalleriaItem]] =
        Entry.reading(scope, entries)
end GalleriaItemScope

/** Galleria: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Galleria anatomy: `div.p-galleria.p-component` > `div.p-galleria-content` >
  * `div.p-galleria-items-container` > `div.p-galleria-items` with the plain
  * `button.p-galleria-prev-button.p-galleria-nav-button` /
  * `.p-galleria-next-button` navigators (chevron glyphs, absolutely positioned
  * by the sheet) around `div.p-galleria-item` (the preview image + the optional
  * `div.p-galleria-caption`), then `div.p-galleria-thumbnails` >
  * `div.p-galleria-thumbnails-content` with the thumbnail navigators flanking
  * `div.p-galleria-thumbnails-viewport` > `div.p-galleria-thumbnail-items` of
  * `div.p-galleria-thumbnail-item[.p-galleria-thumbnail-item-current]` entries,
  * plus the optional `ul.p-galleria-indicator-list`), so the extracted
  * `@primeuix` galleria CSS applies verbatim.
  *
  * `activeIndex` binds two-way to a `SignalRef[Int]` (clamped at render):
  * thumbnail/indicator clicks and the navigators write it; external writes
  * re-render the preview. Server-honest DEVIATION (documented): Prime windows
  * the thumbnail strip (`numVisible`) and slides it via a JS translate; here
  * ALL thumbnails render in the strip and the thumbnail navigators step the
  * active index by one. `circular(true)` wraps the navigators; otherwise they
  * disable at the ends. The fullscreen mode is deferred (portal + JS
  * lifecycle).
  *
  * Galleria and [[Carousel]] share the item-navigator and indicator features under
  * the same names (`showItemNavigators`, `showIndicators`) and disagree only on
  * their DEFAULTS: thumbnails on, indicators off, item navigators off here;
  * navigators and indicators both on in Carousel. That is Prime's own split
  * between the two components, inherited deliberately so the out-of-the-box look
  * matches the design system rather than a local convention.
  */
final case class Galleria private (
    // The images `body` draws, resolved from `itemSources` by the render.
    itemList: List[GalleriaItem] = Nil,
    itemSources: List[GalleriaItemLiveEntry] = Nil,
    activeSlot: Slot[Int] = Slot(),
    itemTemplateF: Maybe[GalleriaItem => UI] = Absent,
    thumbnailTemplateF: Maybe[GalleriaItem => UI] = Absent,
    circularFlag: Boolean = false,
    showThumbnailsFlag: Boolean = true,
    showIndicatorsFlag: Boolean = false,
    showItemNavigatorsFlag: Boolean = false,
    showThumbnailNavigatorsFlag: Boolean = true
) extends Node:
    type Self = Galleria

    /** Appends the given images, each built directly or from `_` (see [[GalleriaItemScope]]), or a signal of images that
      * change while the gallery is shown (see [[LiveEntry]]).
      */
    def items(is: GalleriaItemLiveEntry*): Galleria = copy(itemSources = itemSources ++ is)

    /** The images as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): ReactiveValue[List[GalleriaItem]] = GalleriaItemScope.reading(itemSources)

    /** Binds the active image index (0-based, clamped at render). A `SignalRef` binds two way: a navigator, a thumbnail
      * or a dot writes it, then [[onActiveIndexChange]] hears the new index. A plain `Signal` binds one way: the gallery
      * shows it and only reports. A constant nobody hears is fixed: the gallery stays on that image, its navigators,
      * thumbnails and dots reachable and inert. Unbound, the gallery keeps the index itself, starting on the first image.
      * A signal nothing hears is reported in a card.
      */
    def activeIndex(v: Int | Signal[Int]): Galleria = copy(activeSlot = activeSlot.bind(v))

    /** Custom preview content, replacing the default full-width `img`. */
    def itemTemplate(f: GalleriaItem => UI): Galleria = copy(itemTemplateF = Present(f))

    /** Custom thumbnail content, replacing the default thumbnail `img`. */
    def thumbnailTemplate(f: GalleriaItem => UI): Galleria = copy(thumbnailTemplateF = Present(f))

    /** Wraps the navigators past the ends (default false: they disable). */
    def circular(v: Boolean): Galleria = copy(circularFlag = v)

    /** Renders the thumbnail strip (default true, like Prime). */
    def showThumbnails(v: Boolean): Galleria = copy(showThumbnailsFlag = v)

    /** Renders the indicator dots below the preview. Default false, which is
      * PrimeVue/PrimeReact's own Galleria default; [[Carousel.showIndicators]] is
      * the same feature defaulted to true because Prime's Carousel does that.
      */
    def showIndicators(v: Boolean): Galleria = copy(showIndicatorsFlag = v)

    /** Renders the prev/next navigators on the preview. Default false, Prime's own
      * Galleria default; [[Carousel.showItemNavigators]] is the same feature under
      * the same name, defaulted to true after Prime's Carousel.
      */
    def showItemNavigators(v: Boolean): Galleria = copy(showItemNavigatorsFlag = v)

    /** Renders the thumbnail-strip navigators (default true, like Prime). */
    def showThumbnailNavigators(v: Boolean): Galleria = copy(showThumbnailNavigatorsFlag = v)

    /** Fired with the index the reader asks for after any navigation, after a two-way [[activeIndex]] is written. */
    def onActiveIndexChange(f: Int => UI.Action)(using Frame): Galleria = copy(activeSlot = activeSlot.hear(f))

    /** A constant nobody hears is fixed: the gallery stays on that image, and nothing navigates. */
    private def fixed: Boolean = activeSlot.fixed

    private[uic] def render(using Frame): UI =
        val gallery = UI.mounted {
            for
                cmds <- UI.commands
                base <- cmds.freshId
                // Unbound, the gallery keeps the active image itself, starting on the first.
                self <- activeSlot.owned(0).map(s => copy(activeSlot = s))
            yield self.shown(base, id => cmds.focusId(id))
        }.placeholder(shown("", _ => ()))
        // An index bound one way that nothing hears: every navigation is painted away again.
        Diagnostics.before(gallery)("Galleria")(Diagnostic.unheard(activeSlot, Galleria.activeName))
    end render

    /** The rendered gallery. An empty `base` is the placeholder a golden render and an SSG page
      * show: no mount has run, so there is nothing to move focus with and the strips stay as they
      * were, which is a gallery whose dots and thumbnails are reachable by clicking.
      */
    private def shown(base: String, focus: String => UI.Action)(using Frame): UI =
        // The images and the active index are ONE region (see [[ReactiveValue.and]]): a list that shrinks moves the clamp too.
        entries.and(activeSlot.reading(0)).render((items, i) => copy(itemList = items).body(i, base, focus))

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI = shown(base, focus)

    /** One id per indicator dot and per thumbnail, derived from the one minted id. */
    private def dotId(base: String, i: Int): String   = s"$base-d$i"
    private def thumbId(base: String, i: Int): String = s"$base-t$i"

    /** Stamps a strip button with its id and the arrows that reach its neighbours.
      *
      * An empty `base` is the placeholder render, which has no mount to move focus from, so the
      * button is returned untouched and the strip stays what it was.
      */
    private def roving(
        el: Ast.Button,
        base: String,
        idAt: (String, Int) => String,
        i: Int,
        focus: String => UI.Action
    )(using Frame): Ast.Button =
        if base.isEmpty then el
        else el.id(idAt(base, i)).onKeyDown(stripMove(base, idAt, i, focus))

    /** Moves focus along a strip of `itemList.size` buttons, and does nothing else.
      *
      * A dot and a thumbnail are both real `<button>`s, so Enter and Space are already the
      * browser's and the dispatcher's; this handler only supplies what the roving tabindex took
      * away, which is any way at all of reaching the other buttons. Both strips lie horizontally,
      * so both take the horizontal arrows.
      */
    private def stripMove(base: String, idAt: (String, Int) => String, self: Int, focus: String => UI.Action)(using
        Frame
    ): KeyboardEvent => UI.Action = e =>
        val navigable = itemList.indices.toList
        ListNav.onKey(navigable, self, e.key, wrap = circularFlag, ListNav.Orientation.Horizontal) match
            case Present(step) if step.focus != self && navigable.contains(step.focus) =>
                focus(idAt(base, step.focus))
            case _ => ()
        end match
    end stripMove

    private def body(rawIndex: Int, base: String, focus: String => UI.Action)(using Frame): UI =
        if itemList.isEmpty then div.cssClass("p-galleria").cssClass("p-component")()
        else
            val active  = math.min(math.max(rawIndex, 0), itemList.size - 1)
            val current = itemList(active)
            val atStart = active == 0
            val atEnd   = active == itemList.size - 1

            def navButton(cls: String, navCls: String, iconCls: String, glyph: IconGlyph, name: String, off: Boolean, target: Int): UI =
                var b = button
                    .cssClass(cls)
                    .cssClass(navCls)
                    .jsProp("type", "button")
                    .aria("label", name)
                if off then b = b.cssClass("p-disabled").jsProp("disabled", "true")
                else if fixed then b = b.aria("disabled", "true")
                else b = b.onClick(setActive(target))
                b(GlyphSvg(glyph, iconCls))
            end navButton

            val itemNav: (List[UI], List[UI]) =
                if showItemNavigatorsFlag then
                    (
                        List(navButton(
                            "p-galleria-prev-button",
                            "p-galleria-nav-button",
                            "p-galleria-prev-icon",
                            Icons.chevronLeft,
                            "Previous Item",
                            !circularFlag && atStart,
                            active - 1
                        )),
                        List(navButton(
                            "p-galleria-next-button",
                            "p-galleria-nav-button",
                            "p-galleria-next-icon",
                            Icons.chevronRight,
                            "Next Item",
                            !circularFlag && atEnd,
                            active + 1
                        ))
                    )
                else (Nil, Nil)

            val preview: UI = itemTemplateF match
                case Present(f) => f(current)
                case Absent     =>
                    img(ImgSrc.Path(current.src), current.alt).cssClass("p-uic-galleria-image")

            val titleSlot: List[UI] = current.titleDyn match
                case Present(sig) => List(div.cssClass("p-uic-galleria-title")(sig))
                case Absent       => current.title.toList.map(t => div.cssClass("p-uic-galleria-title")(t): UI)
            val subtitleSlot: List[UI] = current.subtitleDyn match
                case Present(sig) => List(p.cssClass("p-uic-galleria-subtitle")(sig))
                case Absent       => current.subtitle.toList.map(s => p.cssClass("p-uic-galleria-subtitle")(s): UI)
            val caption: List[UI] =
                if titleSlot.nonEmpty || subtitleSlot.nonEmpty then
                    List(
                        div.cssClass("p-galleria-caption")(UI.children(titleSlot ++ subtitleSlot)*)
                    )
                else Nil

            val itemsEl: UI = div.cssClass("p-galleria-items")(
                UI.children(itemNav._1 ++ List(div.cssClass("p-galleria-item")(preview): UI) ++ itemNav._2 ++ caption)*
            )

            val indicators: List[UI] =
                if showIndicatorsFlag then
                    List(
                        ul.cssClass("p-galleria-indicator-list")(
                            itemList.indices.toList.map { i =>
                                var item = li.cssClass("p-galleria-indicator")
                                if i == active then item = item.cssClass("p-galleria-indicator-active")
                                if !fixed then item = item.onClick(setActive(i))
                                item(
                                    roving(
                                        button
                                            .cssClass("p-galleria-indicator-button")
                                            .jsProp("type", "button")
                                            .aria("label", s"Item ${i + 1}")
                                            .tabIndex(if i == active then 0 else -1),
                                        base,
                                        dotId,
                                        i,
                                        focus
                                    )
                                )
                            }*
                        )
                    )
                else Nil

            val itemsContainer: UI = div.cssClass("p-galleria-items-container")(
                UI.children((itemsEl :: indicators))*
            )

            val thumbnails: List[UI] =
                if showThumbnailsFlag then
                    val thumbNav: (List[UI], List[UI]) =
                        if showThumbnailNavigatorsFlag then
                            (
                                List(
                                    navButton(
                                        "p-galleria-thumbnail-prev-button",
                                        "p-galleria-thumbnail-nav-button",
                                        "p-galleria-thumbnail-prev-icon",
                                        Icons.chevronLeft,
                                        "Previous Thumbnail",
                                        !circularFlag && atStart,
                                        active - 1
                                    )
                                ),
                                List(
                                    navButton(
                                        "p-galleria-thumbnail-next-button",
                                        "p-galleria-thumbnail-nav-button",
                                        "p-galleria-thumbnail-next-icon",
                                        Icons.chevronRight,
                                        "Next Thumbnail",
                                        !circularFlag && atEnd,
                                        active + 1
                                    )
                                )
                            )
                        else (Nil, Nil)

                    val thumbItems: List[UI] = itemList.zipWithIndex.map { (it, i) =>
                        var itemEl = div.cssClass("p-galleria-thumbnail-item")
                        if i == active then
                            itemEl = itemEl.cssClass("p-galleria-thumbnail-item-current").cssClass("p-galleria-thumbnail-item-active")
                        val content: UI = thumbnailTemplateF match
                            case Present(f) => f(it)
                            case Absent     =>
                                img(ImgSrc.Path(it.thumbnailSrc.getOrElse(it.src)), it.alt)
                                    .cssClass("p-uic-galleria-thumbnail-image")
                        val thumb = button
                            .cssClass("p-galleria-thumbnail")
                            .jsProp("type", "button")
                            .tabIndex(if i == active then 0 else -1)
                            .aria("label", s"Item ${i + 1}")
                            .aria("current", if i == active then "page" else "false")
                        itemEl(
                            roving(
                                if fixed then thumb.aria("disabled", "true") else thumb.onClick(setActive(i)),
                                base,
                                thumbId,
                                i,
                                focus
                            )(content)
                        )
                    }

                    List(
                        div.cssClass("p-galleria-thumbnails")(
                            div.cssClass("p-galleria-thumbnails-content")(
                                UI.children(thumbNav._1 ++ List(
                                    div.cssClass("p-galleria-thumbnails-viewport")(
                                        div.cssClass("p-galleria-thumbnail-items")(UI.children(thumbItems)*)
                                    ): UI
                                ) ++ thumbNav._2)*
                            )
                        )
                    )
                else Nil

            div.cssClass("p-galleria").cssClass("p-component")(
                div.cssClass("p-galleria-content")(
                    UI.children((itemsContainer :: thumbnails))*
                )
            )

    /** A navigator/thumbnail/indicator click asks for the (wrapped, clamped) index: written into a two-way
      * [[activeIndex]], then reported to `onActiveIndexChange`.
      */
    private def setActive(target: Int)(using Frame): UI.Action =
        val n    = itemList.size
        val next =
            if n == 0 then 0
            else if circularFlag then ((target % n) + n) % n
            else math.min(math.max(target, 0), n - 1)
        activeSlot.change(next)
    end setActive
end Galleria

object Galleria:
    private val activeName = Slot.Name("Galleria", "the active image", "a navigation", "onActiveIndexChange")
    def apply(): Galleria  = new Galleria()
