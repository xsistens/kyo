package kyo.uic

import kyo.*
import kyo.UI.*

/** Carousel: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Carousel anatomy: `div.p-carousel.p-component.p-carousel-horizontal|-vertical`
  * > `div.p-carousel-content-container` > `div.p-carousel-content` with the
  * secondary/text/rounded prev/next Buttons (`.p-carousel-prev-button` /
  * `.p-carousel-next-button`, chevron glyphs, up/down when vertical) flanking
  * `div.p-carousel-viewport` > `div.p-carousel-item-list` of
  * `div.p-carousel-item` entries, then the `ul.p-carousel-indicator-list` of
  * `li.p-carousel-indicator[.p-carousel-indicator-active]` dot buttons), so the
  * extracted `@primeuix` carousel CSS applies verbatim.
  *
  * The `page` is 0-based and clamped at render. A `SignalRef` binds two way: a turn
  * writes it, then `onPageChange` hears the page. A plain `Signal` or a constant
  * binds one way: the carousel shows that page and only reports a turn. Without a
  * page the carousel is a static strip.
  *
  * Slide: the interactive path renders EVERY item once in a STABLE
  * strip (`.p-carousel-item-list`, id-stamped) sized `itemCount/numVisible`
  * viewports wide (each item `flex: 0 0 (100/itemCount)%`, so `numVisible` fill
  * the clipping `.p-carousel-viewport`), and slides the strip by patching its
  * `transform` IN PLACE through `bindStyleById`: `translateX(-(clampedStart *
  * 100/itemCount)%)` (or `translateY` when vertical), where `clampedStart` is the
  * window's lead index. The strip element is NEVER re-rendered (that is the hard
  * invariant: a replaced strip drops the CSS `transition` mid-flight), so the
  * nav buttons (disabled at the ends), the indicator dots (active dot), and each
  * item's `p-carousel-item-active` class (all of which DO change per page) live
  * OUTSIDE the strip: the nav row and indicator list are small regions over the
  * items and the page that are SIBLINGS of the strip, and the per-item active
  * class is a reactive class toggled IN PLACE on the item. A page write therefore
  * only re-styles the strip and re-renders the sibling regions; the strip's
  * transition animates the turn.
  *
  * Items from a signal ([[items]] takes one) render inside the strip as a list
  * region, so the strip itself survives a change to them: its width, its
  * translate, the page count and the dots all follow the items as they are.
  *
  * Placeholder / golden (server-honest, no runtime): the SSR/no-runtime
  * `.placeholder` keeps the ORIGINAL window render (only the `numVisible` visible
  * items, each `flex: 1 0 (100/numVisible)%`, Prime's inline window sizing, no
  * transform), so the interactive slide is a pure runtime enhancement layered on a
  * server-correct first paint.
  *
  * Autoplay ([[autoplay]], default off): a built-in scope-bound fiber spawned
  * inside the interactive `UI.mounted`: `Fiber.init(Loop.forever { Async.sleep;
  * advance })`. This is the component half of [[Node]]'s fiber-ownership rule:
  * the fiber is spawned inside the mount, so its Scope is the carousel's lifetime
  * and its scope cancels it on unmount (no leak); nothing is scheduled
  * by `render` itself. Autoplay always wraps to page 0 at the end (Prime's
  * autoplay-implies-circular behavior) even when [[circular]] is off, so it keeps
  * cycling.
  *
  * `circular(true)` wraps prev/next past the ends (the wrap-around CLONE strip is
  * part of Prime's translate machinery and stays DEFERRED; circular only wraps
  * the page index); otherwise the nav buttons disable at the ends, exactly like
  * Prime.
  *
  * Touch/pointer swipe: when bound to a `page` ref, the STABLE strip
  * carries `onPointerDown`/`onPointerUp`: a horizontal (or vertical) drag past a
  * small threshold turns the page prev/next on release. The strip is never
  * re-rendered by the gesture (the in-place transform means the write
  * only re-styles it), so the release SLIDES via the transition and the drag
  * surface is not detached mid-flight.
  */
final case class Carousel[A] private (
    // The items the placeholder draws, resolved from `itemSources` by the render.
    itemList: List[A] = Nil,
    itemSources: List[List[A] | Signal[Seq[A]]] = Nil,
    templateF: Maybe[A => UI] = Absent,
    pageSlot: Maybe[ReactiveValue[Int]] = Absent,
    numVisibleV: Int = 1,
    numScrollV: Int = 1,
    circularFlag: Boolean = false,
    verticalFlag: Boolean = false,
    verticalHeightV: Maybe[String] = Absent,
    showItemNavigatorsFlag: Boolean = true,
    showIndicatorsFlag: Boolean = true,
    autoplayMsV: Int = 0,
    onPageChangeF: Maybe[Int => UI.Action] = Absent,
    fixedFlag: Boolean = false
) extends Node:
    type Self = Carousel[A]

    // Items compare by `equals`, which is how a list of them read from a signal tells a change from a repeat.
    private given CanEqual[A, A] = CanEqual.derived

    /** Appends items, or a signal of items that change while the carousel is shown; `template` renders one item's slide
      * content. The items keep the order the calls gave them.
      */
    def items(is: Iterable[A] | Signal[Seq[A]])(template: A => UI): Carousel[A] =
        val source: List[A] | Signal[Seq[A]] = is match
            case sig: Signal[?] => sig.asInstanceOf[Signal[Seq[A]]]
            case as             => as.asInstanceOf[Iterable[A]].toList
        copy(itemSources = itemSources :+ source, templateF = Present(template))
    end items

    /** The items as ONE reading: fixed unless one of them was handed over as a signal. */
    private[uic] def entries(using Frame): Reading[List[A]] = Reading.concat(itemSources)

    /** The items and the page, read as ONE region (see [[Reading]]): the page count and so the clamp follow the items. */
    private def shown(r: Signal[Int])(using Frame): Reading[(List[A], Int)] = entries.and(Reading.Live(r))

    /** The items as they are now, read when an autoplay step or a swipe needs their count. */
    private def current(using Frame): List[A] < Sync =
        entries match
            case Reading.Fixed(as) => as
            case Reading.Live(sig) => sig.current

    /** The 0-based page (clamped into range at render). A `SignalRef` binds two way: a turn (a nav button, a dot, a
      * swipe, an autoplay step) writes it, then [[onPageChange]] hears the page. A plain `Signal` or a constant binds one
      * way: the carousel shows that page and only reports a turn. A one-way page with no [[onPageChange]] is reported in a
      * card, and a constant nobody hears is fixed: its navigators and dots stay reachable, turn nothing and say so.
      * Unbound, the carousel keeps the page itself, starting on the first.
      */
    def page(v: Int | Signal[Int]): Carousel[A] = copy(pageSlot = Present(ReactiveValue[Int](v)))

    /** Items shown per page (default 1). */
    def numVisible(v: Int): Carousel[A] = copy(numVisibleV = math.max(1, v))

    /** Items stepped per page turn (default 1). */
    def numScroll(v: Int): Carousel[A] = copy(numScrollV = math.max(1, v))

    /** Wraps prev/next past the ends (default false: the buttons disable). */
    def circular(v: Boolean): Carousel[A] = copy(circularFlag = v)

    /** Vertical orientation (`.p-carousel-vertical`, chevron-up/-down navigators);
      * pair with [[verticalViewHeight]] (default 300px, Prime's default).
      */
    def vertical(v: Boolean): Carousel[A] = copy(verticalFlag = v)

    /** Viewport height in vertical orientation (CSS length, Prime's
      * `verticalViewPortHeight`, an inline style there too).
      */
    def verticalViewHeight(v: String): Carousel[A] = copy(verticalHeightV = Present(v))

    /** Renders the prev/next buttons flanking the viewport. Default true, which is
      * PrimeVue/PrimeReact's own Carousel default. [[Galleria]] spells the same
      * feature `showItemNavigators` and defaults it to false because Prime's
      * Galleria does, not because the two components disagree about the concept.
      */
    def showItemNavigators(v: Boolean): Carousel[A] = copy(showItemNavigatorsFlag = v)

    /** Renders the indicator dots. Default true, Prime's own Carousel default;
      * [[Galleria.showIndicators]] is the same feature defaulted to false, again
      * following Prime.
      */
    def showIndicators(v: Boolean): Carousel[A] = copy(showIndicatorsFlag = v)

    /** Advances the page automatically every `intervalMs` (0 or negative = off).
      * A built-in scope-bound fiber inside the interactive mount does the stepping
      * (see the class doc); it wraps to page 0 at the end even without [[circular]],
      * so the carousel keeps cycling. Requires a bound [[page]] ref.
      */
    def autoplay(intervalMs: Int): Carousel[A] = copy(autoplayMsV = math.max(0, intervalMs))

    /** Fired with the page a turn (nav, indicator, swipe, autoplay) asks for, after a two-way [[page]] is written. */
    def onPageChange(f: Int => UI.Action)(using Frame): Carousel[A] = copy(onPageChangeF = Present(Callback.of(f)))

    /** Prime's page count over `n` items: one page per `numScroll` step over the overhang. */
    private def pagesOf(n: Int): Int = Carousel.pageCount(numScrollV, numVisibleV, n)

    private[uic] def render(using Frame): UI =
        pageSlot match
            // Unbound, the carousel keeps the page the reader turns to itself, starting on the first.
            case Absent =>
                ReactiveValue.owned(0)(own => copy(pageSlot = Present(own)).render)(entries.render(as => copy(itemList = as).body(0)))
            // A constant nobody hears is fixed: drawn on its page, turned by nothing.
            case Present(slot) if ReactiveValue.fixed(pageSlot, onPageChangeF) =>
                entries.render(as => copy(itemList = as, fixedFlag = true).body(slot.const.getOrElse(0)))
            case Present(slot) =>
                val r: Signal[Int] = slot.dyn.getOrElse(Signal.initConst(slot.const.getOrElse(0)))
                // The stable strip needs a session id (bindStyleById target, and the base every item and dot id
                // derives from) and a swipe start-x ref, both minted once in the mount. The autoplay fiber is spawned
                // here too, so its Scope is this mount, which cancels it on unmount.
                val carousel = UI.mounted {
                    for
                        cmds     <- UI.commands
                        stripId  <- cmds.freshId
                        startPos <- Signal.initRef(0.0)
                        _        <- autoplayLoop(r)
                    yield fragment(
                        slideTrigger(cmds, stripId, r),
                        interactive(r, stripId, startPos, id => cmds.focusId(id))
                    )
                }.placeholder(shown(r).render((as, p) => copy(itemList = as).body(p)))
                // A signal nobody hears: every turn the reader takes is painted away again.
                if !ReactiveValue.deaf(pageSlot, onPageChangeF) then carousel
                else
                    fragment(
                        Diagnostics.card(
                            "Carousel",
                            "the page is bound one way and nothing hears a turn, so the reader cannot change it; add " +
                                "onPageChange, or bind a SignalRef",
                            Nil
                        ),
                        carousel
                    )
                end if

    /** The autoplay fiber: scope-bound to the enclosing mount (the scope
      * cancels it on unmount, so it never leaks and never busy-blocks: it sleeps
      * between steps). No-op when autoplay is off or there is nothing to cycle.
      * `onPageChange` is a [[Callback]], so a step whose callback fails or throws is
      * reported to the `onFailure` rules around the carousel, as a click's would be,
      * and autoplay goes on.
      */
    private def autoplayLoop(r: Signal[Int])(using Frame): Unit < (Async & Scope) =
        // Fixed items that make one page have nothing to cycle; a live list may grow into more, so it always gets the
        // fiber, and each step counts the pages it holds then.
        val idle = entries match
            case Reading.Fixed(as) => pagesOf(as.size) <= 1
            case Reading.Live(_)   => false
        if autoplayMsV <= 0 || idle then ()
        else
            Fiber.init {
                Loop.forever {
                    for
                        _   <- Async.sleep(autoplayMsV.millis)
                        as  <- current
                        cur <- r.current
                        pages = pagesOf(as.size)
                        // always wrap, even when not circular
                        _ <- (if pages > 1 then advance((cur + 1) % pages) else (): UI.Action)
                    yield Loop.continue(())
                }
            }.unit
        end if
    end autoplayLoop

    /** An invisible sibling that wires the in-place patches AFTER the strip is in
      * the DOM (a nested `UI.mounted` whose effect runs once the enclosing content
      * is published, so `getElementById` resolves the freshly inserted strip): it
      * binds the strip's size and `transform` to the items and the page (the translate
      * that slides). Each item's `p-carousel-item-active` is a reactive class on the
      * item itself. The binding detaches when the mount's Scope closes. Captures `cmds`
      * so the inner effect needs no Env.
      */
    private def slideTrigger(cmds: UI.Commands, stripId: String, r: Signal[Int])(using Frame): UI =
        UI.mounted {
            cmds.bindStyleById(stripId, shown(r).signal.map((as, p) => stripStyle(p, as.size))).andThen(UI.empty)
        }.placeholder(UI.empty)

    /** The strip's size and translate for a page over `n` items. The size spans `n/numVisible` viewports; the translate's
      * fraction is negative of the window's lead index over `n`, so this % of the strip's own size is exactly
      * `clampedStart` item-sizes. Vertical orientation sizes and slides on Y instead.
      */
    private def stripStyle(rawPage: Int, n: Int): Style =
        val frac = Carousel.translateFraction(rawPage, numScrollV, numVisibleV, n)
        if verticalFlag then Style.height(stripSpan(n)).translate(0.pct, frac.pct)
        else Style.width(stripSpan(n)).translate(frac.pct, 0.pct)
    end stripStyle

    /** Whether item `i` of `n` sits in the visible window at `rawPage`. */
    private def inWindow(i: Int, rawPage: Int, n: Int): Boolean =
        val cs = Carousel.clampedStart(rawPage, numScrollV, numVisibleV, n)
        i >= cs && i < cs + numVisibleV

    /** The interactive (stable-strip + reactive siblings) subtree the mount
      * publishes: the golden seam. The transform/active-class patches (Commands)
      * are applied separately by [[slideTrigger]], so this stays a pure render.
      */
    private[uic] def wired(
        ref: Signal[Int],
        stripId: String,
        startPos: SignalRef[Double],
        focus: String => UI.Action = _ => ()
    )(using Frame): UI =
        interactive(ref, stripId, startPos, focus)

    /** The runtime render: reactive nav/indicator siblings around the STABLE strip
      * of ALL items. Only the siblings re-render on a page turn; the strip is built
      * once so `bindStyleById` can slide it in place. Its items are a list region
      * when they come from a signal, which a change to them repaints without
      * replacing the strip.
      */
    private def interactive(
        r: Signal[Int],
        stripId: String,
        startPos: SignalRef[Double],
        focus: String => UI.Action
    )(using Frame): UI =
        val state     = shown(r)
        val prevGlyph = if verticalFlag then Icons.chevronUp else Icons.chevronLeft
        val nextGlyph = if verticalFlag then Icons.chevronDown else Icons.chevronRight

        val prev: List[UI] =
            if showItemNavigatorsFlag then
                List(state.render { (as, p) =>
                    val pages = pagesOf(as.size)
                    val page  = Carousel.clampPage(p, pages)
                    navButton("p-carousel-prev-button", prevGlyph, "Previous Page", !circularFlag && page == 0, page - 1, pages)
                })
            else Nil
        val next: List[UI] =
            if showItemNavigatorsFlag then
                List(state.render { (as, p) =>
                    val pages = pagesOf(as.size)
                    val page  = Carousel.clampPage(p, pages)
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
        // Each item carries the count it was sized for, so a list that grows sizes
        // every item again.
        def itemEl(a: A, i: Int, n: Int): UI =
            div
                .cssClass("p-carousel-item")
                .id(s"$stripId-i$i")
                .role("group")
                .aria("roledescription", "slide")
                .cssClass("p-carousel-item-active", r.map(p => inWindow(i, p, n)))
                .style(_.flexGrow(0).flexShrink(0).flexBasis(basis(n)))(
                    templateF.map(_(a)).getOrElse(stringToUI(a.toString))
                )
        val itemEls: List[UI] = entries match
            case Reading.Fixed(as) => as.zipWithIndex.map((a, i) => itemEl(a, i, as.size))
            case Reading.Live(sig) =>
                given CanEqual[Chunk[(A, Int)], Chunk[(A, Int)]] = CanEqual.derived
                List(sig.map(as => Chunk.from(as.map(a => (a, as.size)))).foreachIndexed((i, an) => itemEl(an._1, i, an._2)))
        val initial = entries match
            case Reading.Fixed(as) => as.size
            case Reading.Live(_)   => 0

        var strip = div
            .cssClass("p-carousel-item-list")
            .cssClass("p-uic-carousel-track")
            .id(stripId)
        // A live strip is sized by the bound style alone, which carries the size beside the translate.
        strip =
            if verticalFlag then strip.style(_.height(stripSpan(initial)))
            else strip.style(_.width(stripSpan(initial)))
        // Swipe: the stable strip carries the pointer handlers; the turn
        // reads the LIVE page (r.current) and items on release, so the write patches the
        // transform in place and the strip slides without being re-rendered.
        strip = strip
            .onPointerDown((e: PointerEvent) => startPos.set(if verticalFlag then e.y else e.x))
            .onPointerUp { (e: PointerEvent) =>
                for
                    s0 <- startPos.get
                    as <- current
                    p  <- r.current
                yield
                    val pages           = pagesOf(as.size)
                    val delta           = (if verticalFlag then e.y else e.x) - s0
                    val turn: UI.Action =
                        if delta <= -Carousel.SwipeThreshold then setPage(Carousel.clampPage(p, pages) + 1, pages)
                        else if delta >= Carousel.SwipeThreshold then setPage(Carousel.clampPage(p, pages) - 1, pages)
                        else ()
                    turn
            }
        val stripEl: UI = strip(UI.children(itemEls)*)

        var viewport = div.cssClass("p-carousel-viewport")
        if verticalFlag then
            viewport = viewport.style(_.height(Length.parse(verticalHeightV.getOrElse("300px"))))
        val viewportEl: UI = viewport(stripEl)

        val content: UI = div.cssClass("p-carousel-content")(UI.children(prev ++ (viewportEl :: next))*)

        val indicators: List[UI] =
            if showIndicatorsFlag then
                List(state.render { (as, p) =>
                    val pages = pagesOf(as.size)
                    if pages > 0 then indicatorList(Carousel.clampPage(p, pages), pages, dotIds(stripId, pages), focus)
                    else UI.empty
                })
            else Nil

        shell(content, indicators)
    end interactive

    /** The placeholder (SSR/no-runtime/golden) render: the ORIGINAL window, only
      * the `numVisible` visible items (Prime's inline window sizing), no transform.
      * Byte-identical to the pre-slide render, so the golden assertions stay green.
      */
    private def body(rawPage: Int)(using Frame): UI =
        val pages   = pagesOf(itemList.size)
        val page    = Carousel.clampPage(rawPage, pages)
        val start   = math.min(page * numScrollV, math.max(0, itemList.size - numVisibleV))
        val visible = itemList.slice(start, start + numVisibleV)

        val atStart = page == 0
        val atEnd   = pages == 0 || page == pages - 1

        val prevGlyph      = if verticalFlag then Icons.chevronUp else Icons.chevronLeft
        val nextGlyph      = if verticalFlag then Icons.chevronDown else Icons.chevronRight
        val prev: List[UI] =
            if showItemNavigatorsFlag then
                List(navButton("p-carousel-prev-button", prevGlyph, "Previous Page", !circularFlag && atStart, page - 1, pages))
            else Nil
        val next: List[UI] =
            if showItemNavigatorsFlag then
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
            el(templateF.map(_(a)).getOrElse(stringToUI(a.toString)))
        }

        val track: UI = div.cssClass("p-carousel-item-list")(UI.children(itemEls)*)

        var viewport = div.cssClass("p-carousel-viewport")
        if verticalFlag then
            viewport = viewport.style(_.height(Length.parse(verticalHeightV.getOrElse("300px"))))
        val viewportEl: UI = viewport(track)

        val content: UI = div.cssClass("p-carousel-content")(UI.children(prev ++ (viewportEl :: next))*)

        val indicators: List[UI] =
            if showIndicatorsFlag && pages > 0 then List(indicatorList(page, pages, Nil, _ => ()))
            else Nil

        shell(content, indicators)
    end body

    /** The root region wrapping the content row and the indicator list. */
    private def shell(content: UI, indicators: List[UI])(using Frame): UI =
        var root = div.cssClass("p-carousel").cssClass("p-component").role("region")
        root = root.cssClass(if verticalFlag then "p-carousel-vertical" else "p-carousel-horizontal")
        root(div.cssClass("p-carousel-content-container")(UI.children(content :: indicators)*))
    end shell

    /** A prev/next navigator (Prime's secondary text rounded Button). */
    private def navButton(cls: String, glyph: IconGlyph, name: String, off: Boolean, target: Int, pages: Int)(using Frame): UI =
        val b = Button()
            .icon(glyph)
            .severity(Severity.Secondary)
            .variant(ButtonVariant.Text)
            .rounded(true)
            .accessibleName(name)
            .disabled(off)
            .extraClass(cls)
        // A fixed carousel shows its navigators, turns nothing with them, and says so: a tab stop with aria-disabled.
        (if fixedFlag then b.ariaDisabled(true) else b.onClick(setPage(target, pages))).render
    end navButton

    /** One id per indicator dot, derived from the strip's own minted id. */
    private def dotIds(stripId: String, pages: Int): Seq[String] = (0 until pages).map(i => s"$stripId-d$i")

    /** The indicator dot list for a (clamped) page.
      *
      * The dots rove their tabindex, so every inactive one is OUT of the Tab order and the arrows
      * are the only way to reach it. `dotIds` empty is the placeholder render, which has no mount
      * to move focus from and so keeps the dots as they were.
      */
    private def indicatorList(page: Int, pages: Int, ids: Seq[String], focus: String => UI.Action)(using Frame): UI =
        val navigable = (0 until pages).toList
        val axis      = if verticalFlag then ListNav.Orientation.Vertical else ListNav.Orientation.Horizontal
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
                // A fixed carousel's dots turn nothing, and the ones off the page say so; the current one names the page.
                if !fixedFlag then btn = btn.onClick(setPage(i, pages))
                else if i != page then btn = btn.aria("disabled", "true")
                if ids.isDefinedAt(i) then
                    btn = btn.id(ids(i)).onKeyDown { e =>
                        // A dot IS a button, so Enter and Space are the browser's; this moves
                        // focus and nothing else, the way Tabs and Accordion do. The wrap follows
                        // the carousel's own: on a carousel that does not come round, the last
                        // dot has no next page, so the arrow has nowhere to take the reader.
                        ListNav.onKey(navigable, i, e.key, wrap = circularFlag, axis) match
                            case Present(step) if step.focus != i && ids.isDefinedAt(step.focus) => focus(ids(step.focus))
                            case _                                                               => ()
                    }
                end if
                item(btn)
            }*
        )
    end indicatorList

    /** The strip's main-size span over `n` items (the strip is `n/numVisible` viewports). */
    private def stripSpan(n: Int): Length = (math.max(n, 1) * 100.0 / numVisibleV).pct

    /** A runtime strip item's flex-basis: 1/n of the strip. */
    private def basis(n: Int): Length = (100.0 / math.max(n, 1)).pct

    /** A nav/indicator/swipe turn writes the (wrapped, clamped) page and fires
      * `onPageChange`. The translating strip keeps the active window in view, so
      * there is no scroll-into-view step.
      */
    private def setPage(target: Int, pages: Int)(using Frame): UI.Action =
        val next =
            if pages <= 0 then 0
            else if circularFlag then ((target % pages) + pages) % pages
            else math.min(math.max(target, 0), pages - 1)
        advance(next)
    end setPage

    /** Writes a two-way page and fires `onPageChange` (the shared write). */
    private def advance(next: Int)(using Frame): UI.Action = ReactiveValue.change(pageSlot, next, onPageChangeF)
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

    /** The lead item index of the visible window at `page`: `page * numScroll`,
      * clamped so the last window sits flush against the end (`itemCount -
      * numVisible`). Matches the placeholder's `start`.
      */
    private[uic] def clampedStart(page: Int, numScroll: Int, numVisible: Int, itemCount: Int): Int =
        val cp = clampPage(page, pageCount(numScroll, numVisible, itemCount))
        math.min(cp * numScroll, math.max(0, itemCount - numVisible))

    /** The strip's translate fraction (a percentage, negative) for a page:
      * `-(clampedStart * 100 / itemCount)`. The strip spans `itemCount/numVisible`
      * viewports, so this % of the strip's own size is exactly `clampedStart`
      * item-widths. For example, page 1 of a 1-visible/1-scroll strip over 5 items
      * is `-20.0`, clamped to `-80.0` at the last window.
      */
    private[uic] def translateFraction(page: Int, numScroll: Int, numVisible: Int, itemCount: Int): Double =
        if itemCount <= 0 then 0.0
        else -(clampedStart(page, numScroll, numVisible, itemCount) * 100.0 / itemCount)
end Carousel
