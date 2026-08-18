package kyo.uic

import kyo.*
import kyo.UI.*

/** One image of a [[Galleria]] — a hand-authored carrier: the main `src`, its
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
)

/** Galleria — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * the thumbnail strip (`numVisible`) and slides it via a JS translate — here
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
    itemList: List[GalleriaItem] = Nil,
    activeRef: Maybe[SignalRef[Int]] = Absent,
    itemTemplateF: Maybe[GalleriaItem => UI] = Absent,
    thumbnailTemplateF: Maybe[GalleriaItem => UI] = Absent,
    circularFlag: Boolean = false,
    showThumbnailsFlag: Boolean = true,
    showIndicatorsFlag: Boolean = false,
    showItemNavigatorsFlag: Boolean = false,
    showThumbnailNavigatorsFlag: Boolean = true,
    onItemChangeF: Maybe[Int => Any < Async] = Absent
) extends Node:
    type Self = Galleria

    /** Appends the given images. */
    def items(is: GalleriaItem*): Galleria = copy(itemList = itemList ++ is.toList)

    /** Binds the active image index two-way (0-based, clamped at render). */
    def activeIndex(ref: SignalRef[Int]): Galleria = copy(activeRef = Present(ref))

    /** Custom preview content, replacing the default full-width `img`. */
    def itemTemplate(f: GalleriaItem => UI): Galleria = copy(itemTemplateF = Present(f))

    /** Custom thumbnail content, replacing the default thumbnail `img`. */
    def thumbnailTemplate(f: GalleriaItem => UI): Galleria = copy(thumbnailTemplateF = Present(f))

    /** Wraps the navigators past the ends (default false — they disable). */
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

    /** Fired with the new index after any navigation write. */
    def onItemChange(f: Int => Any < Async): Galleria = copy(onItemChangeF = Present(f))

    private[uic] def render(using Frame): UI =
        activeRef match
            case Present(r) => r.render(body)
            case Absent     => body(0)

    private def body(rawIndex: Int)(using Frame): UI =
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
                else b = b.onClick(setActive(target))
                b(toChild(GlyphSvg(glyph, iconCls)))
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
                case Absent =>
                    img(ImgSrc.Path(current.src), current.alt).cssClass("p-uic-galleria-image")

            val titleSlot: List[UI] = current.titleDyn match
                case Present(sig) => List(sig.render(t => div.cssClass("p-uic-galleria-title")(t)))
                case Absent       => current.title.toList.map(t => div.cssClass("p-uic-galleria-title")(t): UI)
            val subtitleSlot: List[UI] = current.subtitleDyn match
                case Present(sig) => List(sig.render(t => p.cssClass("p-uic-galleria-subtitle")(t)))
                case Absent       => current.subtitle.toList.map(s => p.cssClass("p-uic-galleria-subtitle")(s): UI)
            val caption: List[UI] =
                if titleSlot.nonEmpty || subtitleSlot.nonEmpty then
                    List(
                        div.cssClass("p-galleria-caption")((titleSlot ++ subtitleSlot).map(toChild)*)
                    )
                else Nil

            val itemsEl: UI = div.cssClass("p-galleria-items")(
                (itemNav._1 ++ List(div.cssClass("p-galleria-item")(toChild(preview)): UI) ++ itemNav._2 ++ caption).map(toChild)*
            )

            val indicators: List[UI] =
                if showIndicatorsFlag then
                    List(
                        ul.cssClass("p-galleria-indicator-list")(
                            itemList.indices.toList.map { i =>
                                var item = li.cssClass("p-galleria-indicator")
                                if i == active then item = item.cssClass("p-galleria-indicator-active")
                                item = item.onClick(setActive(i))
                                toChild(
                                    item(
                                        toChild(
                                            button
                                                .cssClass("p-galleria-indicator-button")
                                                .jsProp("type", "button")
                                                .aria("label", s"Item ${i + 1}")
                                                .tabIndex(if i == active then 0 else -1)
                                        )
                                    )
                                )
                            }*
                        )
                    )
                else Nil

            val itemsContainer: UI = div.cssClass("p-galleria-items-container")(
                ((itemsEl :: indicators)).map(toChild)*
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
                            case Absent =>
                                img(ImgSrc.Path(it.thumbnailSrc.getOrElse(it.src)), it.alt)
                                    .cssClass("p-uic-galleria-thumbnail-image")
                        itemEl(
                            toChild(
                                button
                                    .cssClass("p-galleria-thumbnail")
                                    .jsProp("type", "button")
                                    .tabIndex(if i == active then 0 else -1)
                                    .aria("label", s"Item ${i + 1}")
                                    .aria("current", if i == active then "page" else "false")
                                    .onClick(setActive(i))(toChild(content))
                            )
                        )
                    }

                    List(
                        div.cssClass("p-galleria-thumbnails")(
                            toChild(
                                div.cssClass("p-galleria-thumbnails-content")(
                                    (thumbNav._1 ++ List(
                                        div.cssClass("p-galleria-thumbnails-viewport")(
                                            div.cssClass("p-galleria-thumbnail-items")(thumbItems.map(toChild)*)
                                        ): UI
                                    ) ++ thumbNav._2).map(toChild)*
                                )
                            )
                        )
                    )
                else Nil

            div.cssClass("p-galleria").cssClass("p-component")(
                toChild(
                    div.cssClass("p-galleria-content")(
                        ((itemsContainer :: thumbnails)).map(toChild)*
                    )
                )
            )

    /** A navigator/thumbnail/indicator click writes the (wrapped, clamped) index,
      * then fires `onItemChange`.
      */
    private def setActive(target: Int)(using Frame): Any < Async =
        val n = itemList.size
        val next =
            if n == 0 then 0
            else if circularFlag then ((target % n) + n) % n
            else math.min(math.max(target, 0), n - 1)
        val write: Any < Async = activeRef match
            case Present(ref) => ref.set(next)
            case Absent       => ()
        val fire: Any < Async = onItemChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end setActive
end Galleria

object Galleria:
    def apply(): Galleria = new Galleria()
