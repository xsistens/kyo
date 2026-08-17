package kyo.uic

import kyo.*
import kyo.UI.*

/** Carousel — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Carousel anatomy: `div.p-carousel.p-component.p-carousel-horizontal|-vertical`
  * > `div.p-carousel-content-container` > `div.p-carousel-content` with the
  * secondary/text/rounded prev/next Buttons (`.p-carousel-prev-button` /
  * `.p-carousel-next-button`, chevron glyphs — up/down when vertical) flanking
  * `div.p-carousel-viewport` > `div.p-carousel-item-list` of
  * `div.p-carousel-item` entries, then the `ul.p-carousel-indicator-list` of
  * `li.p-carousel-indicator[.p-carousel-indicator-active]` dot buttons), so the
  * extracted `@primeuix` carousel CSS applies verbatim.
  *
  * The `page` binds two-way to a `SignalRef[Int]` (0-based, clamped at render).
  *
  * Slide (mounted8): the interactive path renders EVERY item once in a STABLE
  * strip (`.p-carousel-item-list`, id-stamped) sized `itemCount/numVisible`
  * viewports wide (each item `flex: 0 0 (100/itemCount)%`, so `numVisible` fill
  * the clipping `.p-carousel-viewport`), and slides the strip by patching its
  * `transform` IN PLACE through `bindStyleById` — `translateX(-(clampedStart *
  * 100/itemCount)%)` (or `translateY` when vertical), where `clampedStart` is the
  * window's lead index. The strip element is NEVER re-rendered (that is the hard
  * invariant — a replaced strip drops the CSS `transition` mid-flight), so the
  * nav buttons (disabled at the ends), the indicator dots (active dot), and each
  * item's `p-carousel-item-active` class — all of which DO change per page — live
  * OUTSIDE the strip: the nav row and indicator list are small `pageRef.render`
  * regions that are SIBLINGS of the strip, and the per-item active class is
  * toggled IN PLACE via `bindClassById` per item (its id minted alongside the
  * strip's). A page write therefore only re-styles the strip and re-renders the
  * sibling regions; the strip's transition animates the turn.
  *
  * Placeholder / golden (server-honest, no runtime): the SSR/no-runtime
  * `.placeholder` keeps the ORIGINAL window render — only the `numVisible` visible
  * items (each `flex: 1 0 (100/numVisible)%`, Prime's inline window sizing), no
  * transform — so the interactive slide is a pure runtime enhancement layered on a
  * server-correct first paint.
  *
  * Autoplay ([[autoplay]], default off): a built-in scope-bound fiber spawned
  * inside the interactive `UI.mounted` — `Fiber.init(Loop.forever { Async.sleep;
  * advance })`. This is the component half of [[Node]]'s fiber-ownership rule:
  * the fiber is spawned inside the mount, so its Scope is the carousel's lifetime
  * and mounted4 supervision cancels it on unmount (no leak); nothing is scheduled
  * by `render` itself. Autoplay always wraps to page 0 at the end (Prime's
  * autoplay-implies-circular behavior) even when [[circular]] is off, so it keeps
  * cycling.
  *
  * `circular(true)` wraps prev/next past the ends (the wrap-around CLONE strip is
  * part of Prime's translate machinery and stays DEFERRED — circular only wraps
  * the page index); otherwise the nav buttons disable at the ends, exactly like
  * Prime.
  *
  * Touch/pointer swipe (mounted6): when bound to a `page` ref, the STABLE strip
  * carries `onPointerDown`/`onPointerUp` — a horizontal (or vertical) drag past a
  * small threshold turns the page prev/next on release. The strip is never
  * re-rendered by the gesture (the mounted8 in-place transform means the write
  * only re-styles it), so the release SLIDES via the transition and the drag
  * surface is not detached mid-flight.
  */
final case class Carousel[A] private (
    itemList: List[A] = Nil,
    templateF: Maybe[A => UI] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    numVisibleV: Int = 1,
    numScrollV: Int = 1,
    circularFlag: Boolean = false,
    verticalFlag: Boolean = false,
    verticalHeightV: Maybe[String] = Absent,
    showNavigatorsFlag: Boolean = true,
    showIndicatorsFlag: Boolean = true,
    autoplayMsV: Int = 0,
    onPageChangeF: Maybe[Int => Any < Async] = Absent
) extends Node:
    type Self = Carousel[A]

    /** Appends items; `template` renders one item's slide content. */
    def items(is: Seq[A])(template: A => UI): Carousel[A] =
        copy(itemList = itemList ++ is.toList, templateF = Present(template))

    /** Binds the 0-based page two-way (clamped into range at render). */
    def page(ref: SignalRef[Int]): Carousel[A] = copy(pageRef = Present(ref))

    /** Items shown per page (default 1). */
    def numVisible(v: Int): Carousel[A] = copy(numVisibleV = math.max(1, v))

    /** Items stepped per page turn (default 1). */
    def numScroll(v: Int): Carousel[A] = copy(numScrollV = math.max(1, v))

    /** Wraps prev/next past the ends (default false — the buttons disable). */
    def circular(v: Boolean): Carousel[A] = copy(circularFlag = v)

    /** Vertical orientation (`.p-carousel-vertical`, chevron-up/-down navigators);
      * pair with [[verticalViewHeight]] (default 300px, Prime's default).
      */
    def vertical(v: Boolean): Carousel[A] = copy(verticalFlag = v)

    /** Viewport height in vertical orientation (CSS length, Prime's
      * `verticalViewPortHeight` — an inline style there too).
      */
    def verticalViewHeight(v: String): Carousel[A] = copy(verticalHeightV = Present(v))

    /** Renders the prev/next buttons (default true). */
    def showNavigators(v: Boolean): Carousel[A] = copy(showNavigatorsFlag = v)

    /** Renders the indicator dots (default true). */
    def showIndicators(v: Boolean): Carousel[A] = copy(showIndicatorsFlag = v)

    /** Advances the page automatically every `intervalMs` (0 or negative = off).
      * A built-in scope-bound fiber inside the interactive mount does the stepping
      * (see the class doc); it wraps to page 0 at the end even without [[circular]],
      * so the carousel keeps cycling. Requires a bound [[page]] ref.
      */
    def autoplay(intervalMs: Int): Carousel[A] = copy(autoplayMsV = math.max(0, intervalMs))

    /** Fired with the new page after a nav/indicator/swipe/autoplay write. */
    def onPageChange(f: Int => Any < Async): Carousel[A] = copy(onPageChangeF = Present(f))

    /** Prime's page count: one page per `numScroll` step over the overhang. */
    private def totalPages: Int = Carousel.pageCount(numScrollV, numVisibleV, itemList.size)

    private[uic] def render(using Frame): UI =
        pageRef match
            case Present(r) =>
                // The stable strip needs a session id (bindStyleById target), one id per
                // item (bindClassById targets for the active class), and a swipe start-x
                // ref — all minted once in the mount. The autoplay fiber is spawned here
                // too, so its Scope is this mount (mounted4 cancels it on unmount).
                UI.mounted {
                    for
                        cmds     <- UI.commands
                        stripId  <- cmds.freshId
                        itemIds  <- Kyo.foreach(itemList.indices.toList)(_ => cmds.freshId)
                        startPos <- Signal.initRef(0.0)
                        _        <- autoplayLoop(r)
                    yield fragment(
                        slideTrigger(cmds, stripId, itemIds, r),
                        interactive(r, stripId, itemIds, startPos)
                    )
                }.placeholder(r.render(p => body(p)))
            case Absent => body(0)

    /** The autoplay fiber: scope-bound to the enclosing mount (mounted4 supervision
      * cancels it on unmount, so it never leaks and never busy-blocks — it sleeps
      * between steps). No-op when autoplay is off or there is nothing to cycle.
      */
    private def autoplayLoop(r: SignalRef[Int])(using Frame): Unit < (Async & Scope) =
        val pages = totalPages
        if autoplayMsV <= 0 || pages <= 1 then ()
        else
            Fiber.init {
                Loop.forever {
                    for
                        _   <- Async.sleep(autoplayMsV.millis)
                        cur <- r.get
                        _   <- advance((cur + 1) % pages) // always wrap, even when not circular
                    yield Loop.continue(())
                }
            }.unit
        end if
    end autoplayLoop

    /** An invisible sibling that wires the in-place patches AFTER the strip is in
      * the DOM (a nested `UI.mounted` whose effect runs once the enclosing content
      * is published, so `getElementById` resolves the freshly inserted strip): it
      * binds the strip's `transform` to the page (the translate that slides) and
      * each item's `p-carousel-item-active` to the current window. Both detach when
      * the mount's Scope closes. Captures `cmds` so the inner effect needs no Env.
      */
    private def slideTrigger(cmds: UI.Commands, stripId: String, itemIds: Seq[String], r: SignalRef[Int])(using Frame): UI =
        UI.mounted {
            val bindTransform: Unit < (Async & Scope) =
                cmds.bindStyleById(stripId, r.map(p => stripTransform(p)))
            val bindActive: Unit < (Async & Scope) =
                Kyo.foreach(itemList.indices.toList) { i =>
                    cmds.bindClassById(itemIds(i), "p-carousel-item-active", r.map(p => inWindow(i, p)))
                }.unit
            bindTransform.andThen(bindActive).andThen(UI.empty)
        }.placeholder(UI.empty)

    /** The strip's translate for a page: the fraction is negative of the window's
      * lead index over the item count (the strip spans `itemCount/numVisible`
      * viewports, so this % of the strip's own width is exactly `clampedStart`
      * item-widths). Vertical orientation slides on Y instead.
      */
    private def stripTransform(rawPage: Int): Style =
        val frac = Carousel.translateFraction(rawPage, numScrollV, numVisibleV, itemList.size)
        if verticalFlag then Style.translate(0.pct, frac.pct) else Style.translate(frac.pct, 0.pct)

    /** Whether item `i` sits in the visible window at `rawPage`. */
    private def inWindow(i: Int, rawPage: Int): Boolean =
        val cs = Carousel.clampedStart(rawPage, numScrollV, numVisibleV, itemList.size)
        i >= cs && i < cs + numVisibleV

    /** The interactive (stable-strip + reactive siblings) subtree the mount
      * publishes — the golden seam. The transform/active-class patches (Commands)
      * are applied separately by [[slideTrigger]], so this stays a pure render.
      */
    private[uic] def wired(ref: SignalRef[Int], stripId: String, startPos: SignalRef[Double])(using Frame): UI =
        interactive(ref, stripId, itemList.indices.map(i => s"$stripId-i$i"), startPos)

    /** The runtime render: reactive nav/indicator siblings around the STABLE strip
      * of ALL items. Only the siblings re-render on a page turn; the strip is built
      * once so `bindStyleById` can slide it in place.
      */
    private def interactive(r: SignalRef[Int], stripId: String, itemIds: Seq[String], startPos: SignalRef[Double])(using Frame): UI =
        val pages     = totalPages
        val prevGlyph = if verticalFlag then Icons.chevronUp else Icons.chevronLeft
        val nextGlyph = if verticalFlag then Icons.chevronDown else Icons.chevronRight

        val prev: List[UI] =
            if showNavigatorsFlag then
                List(r.render { p =>
                    val page = Carousel.clampPage(p, pages)
                    navButton("p-carousel-prev-button", prevGlyph, "Previous Page", !circularFlag && page == 0, page - 1, pages)
                })
            else Nil
        val next: List[UI] =
            if showNavigatorsFlag then
                List(r.render { p =>
                    val page = Carousel.clampPage(p, pages)
                    navButton(
                        "p-carousel-next-button",
                        nextGlyph,
                        "Next Page",
                        !circularFlag && (pages == 0 || page == pages - 1),
                        page + 1,
                        pages
                    )
                })
            else Nil

        // The stable strip: ALL items, each 1/itemCount of the strip so numVisible
        // fill the viewport; the strip spans itemCount/numVisible viewports wide.
        val itemEls: List[UI] = itemList.zipWithIndex.map { (a, i) =>
            div
                .cssClass("p-carousel-item")
                .id(itemIds(i))
                .role("group")
                .aria("roledescription", "slide")
                .style(_.flexGrow(0).flexShrink(0).flexBasis(basis))(
                    toChild(templateF.map(_(a)).getOrElse(stringToUI(a.toString)))
                )
        }

        var strip = div
            .cssClass("p-carousel-item-list")
            .cssClass("p-uic-carousel-track")
            .id(stripId)
        strip =
            if verticalFlag then strip.style(_.height(stripSpan))
            else strip.style(_.width(stripSpan))
        // Swipe (mounted6): the stable strip carries the pointer handlers; the turn
        // reads the LIVE page (r.get) and writes on release, so the write patches the
        // transform in place and the strip slides without being re-rendered.
        strip = strip
            .onPointerDown((e: PointerEvent) => startPos.set(if verticalFlag then e.y else e.x))
            .onPointerUp { (e: PointerEvent) =>
                startPos.get.map { s0 =>
                    val delta = (if verticalFlag then e.y else e.x) - s0
                    if delta <= -Carousel.SwipeThreshold then r.get.map(p => setPage(Carousel.clampPage(p, pages) + 1, pages))
                    else if delta >= Carousel.SwipeThreshold then r.get.map(p => setPage(Carousel.clampPage(p, pages) - 1, pages))
                    else ()
                }
            }
        val stripEl: UI = strip(itemEls.map(toChild)*)

        var viewport = div.cssClass("p-carousel-viewport")
        if verticalFlag then
            viewport = viewport.style(_.height(CssValue.length(verticalHeightV.getOrElse("300px"))))
        val viewportEl: UI = viewport(stripEl)

        val content: UI = div.cssClass("p-carousel-content")((prev ++ (viewportEl :: next)).map(toChild)*)

        val indicators: List[UI] =
            if showIndicatorsFlag && pages > 0 then List(r.render(p => indicatorList(Carousel.clampPage(p, pages), pages)))
            else Nil

        shell(content, indicators)
    end interactive

    /** The placeholder (SSR/no-runtime/golden) render: the ORIGINAL window — only
      * the `numVisible` visible items (Prime's inline window sizing), no transform.
      * Byte-identical to the pre-slide render, so the golden assertions stay green.
      */
    private def body(rawPage: Int)(using Frame): UI =
        val pages   = totalPages
        val page    = Carousel.clampPage(rawPage, pages)
        val start   = math.min(page * numScrollV, math.max(0, itemList.size - numVisibleV))
        val visible = itemList.slice(start, start + numVisibleV)

        val atStart = page == 0
        val atEnd   = pages == 0 || page == pages - 1

        val prevGlyph = if verticalFlag then Icons.chevronUp else Icons.chevronLeft
        val nextGlyph = if verticalFlag then Icons.chevronDown else Icons.chevronRight
        val prev: List[UI] =
            if showNavigatorsFlag then
                List(navButton("p-carousel-prev-button", prevGlyph, "Previous Page", !circularFlag && atStart, page - 1, pages))
            else Nil
        val next: List[UI] =
            if showNavigatorsFlag then
                List(navButton("p-carousel-next-button", nextGlyph, "Next Page", !circularFlag && atEnd, page + 1, pages))
            else Nil

        val itemEls: List[UI] = visible.zipWithIndex.map { (a, i) =>
            var el = div
                .cssClass("p-carousel-item")
                .cssClass("p-carousel-item-active")
                .role("group")
                .aria("roledescription", "slide")
                .style(_.flexGrow(1).flexShrink(0).flexBasis((100.0 / numVisibleV).pct))
            if i == 0 then el = el.cssClass("p-carousel-item-start")
            if i == visible.size - 1 then el = el.cssClass("p-carousel-item-end")
            el(toChild(templateF.map(_(a)).getOrElse(stringToUI(a.toString))))
        }

        val track: UI = div.cssClass("p-carousel-item-list")(itemEls.map(toChild)*)

        var viewport = div.cssClass("p-carousel-viewport")
        if verticalFlag then
            viewport = viewport.style(_.height(CssValue.length(verticalHeightV.getOrElse("300px"))))
        val viewportEl: UI = viewport(track)

        val content: UI = div.cssClass("p-carousel-content")((prev ++ (viewportEl :: next)).map(toChild)*)

        val indicators: List[UI] =
            if showIndicatorsFlag && pages > 0 then List(indicatorList(page, pages))
            else Nil

        shell(content, indicators)
    end body

    /** The root region wrapping the content row and the indicator list. */
    private def shell(content: UI, indicators: List[UI])(using Frame): UI =
        var root = div.cssClass("p-carousel").cssClass("p-component").role("region")
        root = root.cssClass(if verticalFlag then "p-carousel-vertical" else "p-carousel-horizontal")
        root(toChild(div.cssClass("p-carousel-content-container")((content :: indicators).map(toChild)*)))
    end shell

    /** A prev/next navigator (Prime's secondary text rounded Button). */
    private def navButton(cls: String, glyph: IconGlyph, name: String, off: Boolean, target: Int, pages: Int)(using Frame): UI =
        Button()
            .icon(glyph)
            .severity(Severity.Secondary)
            .variant(ButtonVariant.Text)
            .rounded(true)
            .accessibleName(name)
            .disabled(off)
            .onClick(setPage(target, pages))
            .extraClass(cls)
            .render

    /** The indicator dot list for a (clamped) page. */
    private def indicatorList(page: Int, pages: Int)(using Frame): UI =
        ul.cssClass("p-carousel-indicator-list")(
            (0 until pages).toList.map { i =>
                var item = li.cssClass("p-carousel-indicator")
                if i == page then item = item.cssClass("p-carousel-indicator-active")
                var btn = button
                    .cssClass("p-carousel-indicator-button")
                    .jsProp("type", "button")
                    .aria("label", s"Page ${i + 1}")
                    .tabIndex(if i == page then 0 else -1)
                if i == page then btn = btn.aria("current", "page")
                btn = btn.onClick(setPage(i, pages))
                toChild(item(toChild(btn)))
            }*
        )

    /** The strip's main-size span (the strip is `itemCount/numVisible` viewports). */
    private def stripSpan: Length = (math.max(itemList.size, 1) * 100.0 / numVisibleV).pct

    /** A runtime strip item's flex-basis: 1/itemCount of the strip. */
    private def basis: Length = (100.0 / math.max(itemList.size, 1)).pct

    /** A nav/indicator/swipe turn writes the (wrapped, clamped) page and fires
      * `onPageChange`. The translating strip keeps the active window in view, so
      * there is no scroll-into-view step.
      */
    private def setPage(target: Int, pages: Int)(using Frame): Any < Async =
        val next =
            if pages <= 0 then 0
            else if circularFlag then ((target % pages) + pages) % pages
            else math.min(math.max(target, 0), pages - 1)
        advance(next)
    end setPage

    /** Writes the page ref and fires `onPageChange` (the shared write). */
    private def advance(next: Int)(using Frame): Any < Async =
        val write: Any < Async = pageRef match
            case Present(ref) => ref.set(next)
            case Absent       => ()
        val fire: Any < Async = onPageChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        write.andThen(fire)
    end advance
end Carousel

object Carousel:
    def apply[A](): Carousel[A] = new Carousel[A]()

    /** Drag distance (px) past which a swipe turns the page. */
    private[uic] val SwipeThreshold = 40.0

    /** Prime's page count: one page per `numScroll` step over the overhang. */
    private[uic] def pageCount(numScroll: Int, numVisible: Int, itemCount: Int): Int =
        if itemCount <= 0 then 0
        else math.max(1, math.ceil((itemCount - numVisible).toDouble / numScroll).toInt + 1)

    /** Clamps a raw page into `[0, pages-1]`. */
    private[uic] def clampPage(rawPage: Int, pages: Int): Int =
        if pages <= 0 then 0 else math.min(math.max(rawPage, 0), pages - 1)

    /** The lead item index of the visible window at `page` — `page * numScroll`,
      * clamped so the last window sits flush against the end (`itemCount -
      * numVisible`). Matches the placeholder's `start`.
      */
    private[uic] def clampedStart(page: Int, numScroll: Int, numVisible: Int, itemCount: Int): Int =
        val cp = clampPage(page, pageCount(numScroll, numVisible, itemCount))
        math.min(cp * numScroll, math.max(0, itemCount - numVisible))

    /** The strip's translate fraction (a percentage, negative) for a page:
      * `-(clampedStart * 100 / itemCount)`. The strip spans `itemCount/numVisible`
      * viewports, so this % of the strip's own size is exactly `clampedStart`
      * item-widths — e.g. page 1 of a 1-visible/1-scroll strip over 5 items is
      * `-20.0`, clamped to `-80.0` at the last window.
      */
    private[uic] def translateFraction(page: Int, numScroll: Int, numVisible: Int, itemCount: Int): Double =
        if itemCount <= 0 then 0.0
        else -(clampedStart(page, numScroll, numVisible, itemCount) * 100.0 / itemCount)
end Carousel
